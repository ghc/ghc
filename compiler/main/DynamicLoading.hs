-- | Dynamically lookup up values from modules and loading them.
module DynamicLoading (
#ifdef GHCI
        -- * Force loading information
        forceLoadModuleInterfaces,
        forceLoadNameModuleInterface,
        forceLoadTyCon,
        
        -- * Finding names
        lookupRdrNameInModule,
        
        -- * Loading values
        getValueSafely,
        getHValueSafely,
        lessUnsafeCoerce
#endif
    ) where

#ifdef GHCI
import Linker           ( linkModule, getHValue )
import SrcLoc           ( noSrcSpan )
import Finder           ( findImportedModule, cannotFindModule )
import DriverPhases     ( HscSource(HsSrcFile) )
import TcRnMonad        ( initTc, initIfaceTcRn )
import LoadIface        ( loadPluginInterface )
import RdrName          ( RdrName, Provenance(..), ImportSpec(..), ImpDeclSpec(..)
                        , ImpItemSpec(..), mkGlobalRdrEnv, lookupGRE_RdrName, gre_name )
import RnNames          ( gresFromAvails )
import PrelNames        ( iNTERACTIVE )
import DynFlags

import HscTypes         ( HscEnv(..), FindResult(..), ModIface(..), lookupTypeHscEnv )
import BasicTypes       ( HValue )
import TypeRep          ( TyThing(..), pprTyThingCategory )
import Type             ( Type, eqType )
import TyCon            ( TyCon )
import Name             ( Name, nameModule_maybe )
import Id               ( idType )
import Module           ( Module, ModuleName )
import Panic
import FastString
import ErrUtils
import Outputable
import Exception
import Hooks

import Data.Maybe        ( mapMaybe )
import GHC.Exts          ( unsafeCoerce# )


-- | Force the interfaces for the given modules to be loaded. The 'SDoc' parameter is used
-- for debugging (@-ddump-if-trace@) only: it is shown as the reason why the module is being loaded.
forceLoadModuleInterfaces :: HscEnv -> SDoc -> [Module] -> IO ()
forceLoadModuleInterfaces hsc_env doc modules
    = (initTc hsc_env HsSrcFile False iNTERACTIVE $ initIfaceTcRn $ mapM_ (loadPluginInterface doc) modules) >> return ()

-- | Force the interface for the module containing the name to be loaded. The 'SDoc' parameter is used
-- for debugging (@-ddump-if-trace@) only: it is shown as the reason why the module is being loaded.
forceLoadNameModuleInterface :: HscEnv -> SDoc -> Name -> IO ()
forceLoadNameModuleInterface hsc_env reason name = do
    let name_modules = mapMaybe nameModule_maybe [name]
    forceLoadModuleInterfaces hsc_env reason name_modules

-- | Load the 'TyCon' associated with the given name, come hell or high water. Fails if:
--
-- * The interface could not be loaded
-- * The name is not that of a 'TyCon'
-- * The name did not exist in the loaded module
forceLoadTyCon :: HscEnv -> Name -> IO TyCon
forceLoadTyCon hsc_env con_name = do
    forceLoadNameModuleInterface hsc_env (ptext (sLit "contains a name used in an invocation of loadTyConTy")) con_name
    
    mb_con_thing <- lookupTypeHscEnv hsc_env con_name
    case mb_con_thing of
        Nothing -> throwCmdLineErrorS dflags $ missingTyThingError con_name
        Just (ATyCon tycon) -> return tycon
        Just con_thing -> throwCmdLineErrorS dflags $ wrongTyThingError con_name con_thing
  where dflags = hsc_dflags hsc_env

-- | Loads the value corresponding to a 'Name' if that value has the given 'Type'. This only provides limited safety
-- in that it is up to the user to ensure that that type corresponds to the type you try to use the return value at!
--
-- If the value found was not of the correct type, returns @Nothing@. Any other condition results in an exception:
--
-- * If we could not load the names module
-- * If the thing being loaded is not a value
-- * If the Name does not exist in the module
-- * If the link failed

getValueSafely :: HscEnv -> Name -> Type -> IO (Maybe a)
getValueSafely hsc_env val_name expected_type = do
  mb_hval <- lookupHook getValueSafelyHook getHValueSafely dflags hsc_env val_name expected_type
  case mb_hval of
    Nothing   -> return Nothing
    Just hval -> do
      value <- lessUnsafeCoerce dflags "getValueSafely" hval
      return (Just value)
  where
    dflags = hsc_dflags hsc_env

getHValueSafely :: HscEnv -> Name -> Type -> IO (Maybe HValue)
getHValueSafely hsc_env val_name expected_type = do
    forceLoadNameModuleInterface hsc_env (ptext (sLit "contains a name used in an invocation of getHValueSafely")) val_name
    -- Now look up the names for the value and type constructor in the type environment
    mb_val_thing <- lookupTypeHscEnv hsc_env val_name
    case mb_val_thing of
        Nothing -> throwCmdLineErrorS dflags $ missingTyThingError val_name
        Just (AnId id) -> do
            -- Check the value type in the interface against the type recovered from the type constructor
            -- before finally casting the value to the type we assume corresponds to that constructor
            if expected_type `eqType` idType id
             then do
                -- Link in the module that contains the value, if it has such a module
                case nameModule_maybe val_name of
                    Just mod -> do linkModule hsc_env mod
                                   return ()
                    Nothing ->  return ()
                -- Find the value that we just linked in and cast it given that we have proved it's type
                hval <- getHValue hsc_env val_name
                return (Just hval)
             else return Nothing
        Just val_thing -> throwCmdLineErrorS dflags $ wrongTyThingError val_name val_thing
   where dflags = hsc_dflags hsc_env

-- | Coerce a value as usual, but:
--
-- 1) Evaluate it immediately to get a segfault early if the coercion was wrong
--
-- 2) Wrap it in some debug messages at verbosity 3 or higher so we can see what happened
--    if it /does/ segfault
lessUnsafeCoerce :: DynFlags -> String -> a -> IO b
lessUnsafeCoerce dflags context what = do
    debugTraceMsg dflags 3 $ (ptext $ sLit "Coercing a value in") <+> (text context) <> (ptext $ sLit "...")
    output <- evaluate (unsafeCoerce# what)
    debugTraceMsg dflags 3 $ ptext $ sLit "Successfully evaluated coercion"
    return output


-- | Finds the 'Name' corresponding to the given 'RdrName' in the context of the 'ModuleName'. Returns @Nothing@ if no
-- such 'Name' could be found. Any other condition results in an exception:
--
-- * If the module could not be found
-- * If we could not determine the imports of the module
lookupRdrNameInModule :: HscEnv -> ModuleName -> RdrName -> IO (Maybe Name)
lookupRdrNameInModule hsc_env mod_name rdr_name = do
    -- First find the package the module resides in by searching exposed packages and home modules
    found_module <- findImportedModule hsc_env mod_name Nothing
    case found_module of
        Found _ mod -> do
            -- Find the exports of the module
            (_, mb_iface) <- initTc hsc_env HsSrcFile False iNTERACTIVE $ initIfaceTcRn $ loadPluginInterface (ptext (sLit "contains a name used in an invocation of lookupRdrNameInModule")) mod
            case mb_iface of
                Just iface -> do
                    -- Try and find the required name in the exports
                    let decl_spec = ImpDeclSpec { is_mod = mod_name, is_as = mod_name
                                                , is_qual = False, is_dloc = noSrcSpan }
                        provenance = Imported [ImpSpec decl_spec ImpAll]
                        env = mkGlobalRdrEnv (gresFromAvails provenance (mi_exports iface))
                    case lookupGRE_RdrName rdr_name env of
                        [gre] -> return (Just (gre_name gre))
                        []    -> return Nothing
                        _     -> panic "lookupRdrNameInModule"

                Nothing -> throwCmdLineErrorS dflags $ hsep [ptext (sLit "Could not determine the exports of the module"), ppr mod_name]
        err -> throwCmdLineErrorS dflags $ cannotFindModule dflags mod_name err
  where dflags = hsc_dflags hsc_env


wrongTyThingError :: Name -> TyThing -> SDoc
wrongTyThingError name got_thing = hsep [ptext (sLit "The name"), ppr name, ptext (sLit "is not that of a value but rather a"), pprTyThingCategory got_thing]

missingTyThingError :: Name -> SDoc
missingTyThingError name = hsep [ptext (sLit "The name"), ppr name, ptext (sLit "is not in the type environment: are you sure it exists?")]

throwCmdLineErrorS :: DynFlags -> SDoc -> IO a
throwCmdLineErrorS dflags = throwCmdLineError . showSDoc dflags

throwCmdLineError :: String -> IO a
throwCmdLineError = throwGhcExceptionIO . CmdLineError
#endif
