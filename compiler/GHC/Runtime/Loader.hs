{-# LANGUAGE CPP #-}

-- | Dynamically lookup up values from modules and loading them.
module GHC.Runtime.Loader (
        initializePlugins,
        -- * Loading plugins
        loadFrontendPlugin,

        -- * Force loading information
        forceLoadModuleInterfaces,
        forceLoadNameModuleInterface,
        forceLoadTyCon,

        -- * Finding names
        lookupRdrNameInModuleForPlugins,

        -- * Loading values
        getValueSafely,
        getHValueSafely,
        lessUnsafeCoerce
    ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Hooks
import GHC.Driver.Plugins

import GHC.Linker.Loader       ( loadModule, loadName )
import GHC.Runtime.Interpreter ( wormhole, withInterp )
import GHC.Runtime.Interpreter.Types

import GHC.Tc.Utils.Monad      ( initTcInteractive, initIfaceTcRn )
import GHC.Iface.Load          ( loadPluginInterface, cannotFindModule )
import GHC.Rename.Names ( gresFromAvails )
import GHC.Builtin.Names ( pluginTyConName, frontendPluginTyConName )

import GHC.Driver.Env
import GHCi.RemoteTypes  ( HValue )
import GHC.Core.Type     ( Type, eqType, mkTyConTy )
import GHC.Core.TyCon    ( TyCon )

import GHC.Types.SrcLoc        ( noSrcSpan )
import GHC.Types.Name    ( Name, nameModule_maybe )
import GHC.Types.Id      ( idType )
import GHC.Types.TyThing
import GHC.Types.Name.Occurrence ( OccName, mkVarOcc )
import GHC.Types.Name.Reader   ( RdrName, ImportSpec(..), ImpDeclSpec(..)
                               , ImpItemSpec(..), mkGlobalRdrEnv, lookupGRE_RdrName
                               , greMangledName, mkRdrQual )

import GHC.Unit.Finder         ( findPluginModule, FindResult(..) )
import GHC.Unit.Module   ( Module, ModuleName )
import GHC.Unit.Module.ModIface

import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Exception

import GHC.Data.FastString

import Control.Monad     ( unless )
import Data.Maybe        ( mapMaybe )
import Unsafe.Coerce     ( unsafeCoerce )

-- | Loads the plugins specified in the pluginModNames field of the dynamic
-- flags. Should be called after command line arguments are parsed, but before
-- actual compilation starts. Idempotent operation. Should be re-called if
-- pluginModNames or pluginModNameOpts changes.
initializePlugins :: HscEnv -> IO HscEnv
initializePlugins hsc_env
    -- plugins not changed
  | map lpModuleName (hsc_plugins hsc_env) == pluginModNames dflags
   -- arguments not changed
  , all same_args (hsc_plugins hsc_env)
  = return hsc_env -- no need to reload plugins
  | otherwise
  = do loaded_plugins <- loadPlugins hsc_env
       let hsc_env' = hsc_env { hsc_plugins = loaded_plugins }
       withPlugins hsc_env' driverPlugin hsc_env'
  where
    plugin_args = pluginModNameOpts dflags
    same_args p = paArguments (lpPlugin p) == argumentsForPlugin p plugin_args
    argumentsForPlugin p = map snd . filter ((== lpModuleName p) . fst)
    dflags = hsc_dflags hsc_env

loadPlugins :: HscEnv -> IO [LoadedPlugin]
loadPlugins hsc_env
  = do { unless (null to_load) $
           checkExternalInterpreter hsc_env
       ; plugins <- mapM loadPlugin to_load
       ; return $ zipWith attachOptions to_load plugins }
  where
    dflags  = hsc_dflags hsc_env
    to_load = pluginModNames dflags

    attachOptions mod_nm (plug, mod) =
        LoadedPlugin (PluginWithArgs plug (reverse options)) mod
      where
        options = [ option | (opt_mod_nm, option) <- pluginModNameOpts dflags
                            , opt_mod_nm == mod_nm ]
    loadPlugin = loadPlugin' (mkVarOcc "plugin") pluginTyConName hsc_env


loadFrontendPlugin :: HscEnv -> ModuleName -> IO FrontendPlugin
loadFrontendPlugin hsc_env mod_name = do
    checkExternalInterpreter hsc_env
    fst <$> loadPlugin' (mkVarOcc "frontendPlugin") frontendPluginTyConName
                hsc_env mod_name

-- #14335
checkExternalInterpreter :: HscEnv -> IO ()
checkExternalInterpreter hsc_env
  | Just (ExternalInterp {}) <- hsc_interp hsc_env
  = throwIO (InstallationError "Plugins require -fno-external-interpreter")
  | otherwise
  = pure ()

loadPlugin' :: OccName -> Name -> HscEnv -> ModuleName -> IO (a, ModIface)
loadPlugin' occ_name plugin_name hsc_env mod_name
  = do { let plugin_rdr_name = mkRdrQual mod_name occ_name
             dflags = hsc_dflags hsc_env
       ; mb_name <- lookupRdrNameInModuleForPlugins hsc_env mod_name
                        plugin_rdr_name
       ; case mb_name of {
            Nothing ->
                throwGhcExceptionIO (CmdLineError $ showSDoc dflags $ hsep
                          [ text "The module", ppr mod_name
                          , text "did not export the plugin name"
                          , ppr plugin_rdr_name ]) ;
            Just (name, mod_iface) ->

     do { plugin_tycon <- forceLoadTyCon hsc_env plugin_name
        ; mb_plugin <- getValueSafely hsc_env name (mkTyConTy plugin_tycon)
        ; case mb_plugin of
            Nothing ->
                throwGhcExceptionIO (CmdLineError $ showSDoc dflags $ hsep
                          [ text "The value", ppr name
                          , text "did not have the type"
                          , ppr pluginTyConName, text "as required"])
            Just plugin -> return (plugin, mod_iface) } } }


-- | Force the interfaces for the given modules to be loaded. The 'SDoc' parameter is used
-- for debugging (@-ddump-if-trace@) only: it is shown as the reason why the module is being loaded.
forceLoadModuleInterfaces :: HscEnv -> SDoc -> [Module] -> IO ()
forceLoadModuleInterfaces hsc_env doc modules
    = (initTcInteractive hsc_env $
       initIfaceTcRn $
       mapM_ (loadPluginInterface doc) modules)
      >> return ()

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
    forceLoadNameModuleInterface hsc_env (text "contains a name used in an invocation of loadTyConTy") con_name

    mb_con_thing <- lookupType hsc_env con_name
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
    forceLoadNameModuleInterface hsc_env (text "contains a name used in an invocation of getHValueSafely") val_name
    -- Now look up the names for the value and type constructor in the type environment
    mb_val_thing <- lookupType hsc_env val_name
    case mb_val_thing of
        Nothing -> throwCmdLineErrorS dflags $ missingTyThingError val_name
        Just (AnId id) -> do
            -- Check the value type in the interface against the type recovered from the type constructor
            -- before finally casting the value to the type we assume corresponds to that constructor
            if expected_type `eqType` idType id
             then do
                -- Link in the module that contains the value, if it has such a module
                case nameModule_maybe val_name of
                    Just mod -> do loadModule hsc_env mod
                                   return ()
                    Nothing ->  return ()
                -- Find the value that we just linked in and cast it given that we have proved it's type
                hval <- withInterp hsc_env $ \interp -> loadName hsc_env val_name >>= wormhole interp
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
    debugTraceMsg dflags 3 $ (text "Coercing a value in") <+> (text context) <>
                             (text "...")
    output <- evaluate (unsafeCoerce what)
    debugTraceMsg dflags 3 (text "Successfully evaluated coercion")
    return output


-- | Finds the 'Name' corresponding to the given 'RdrName' in the
-- context of the 'ModuleName'. Returns @Nothing@ if no such 'Name'
-- could be found. Any other condition results in an exception:
--
-- * If the module could not be found
-- * If we could not determine the imports of the module
--
-- Can only be used for looking up names while loading plugins (and is
-- *not* suitable for use within plugins).  The interface file is
-- loaded very partially: just enough that it can be used, without its
-- rules and instances affecting (and being linked from!) the module
-- being compiled.  This was introduced by 57d6798.
--
-- Need the module as well to record information in the interface file
lookupRdrNameInModuleForPlugins :: HscEnv -> ModuleName -> RdrName
                                -> IO (Maybe (Name, ModIface))
lookupRdrNameInModuleForPlugins hsc_env mod_name rdr_name = do
    -- First find the unit the module resides in by searching exposed units and home modules
    found_module <- findPluginModule hsc_env mod_name
    case found_module of
        Found _ mod -> do
            -- Find the exports of the module
            (_, mb_iface) <- initTcInteractive hsc_env $
                             initIfaceTcRn $
                             loadPluginInterface doc mod
            case mb_iface of
                Just iface -> do
                    -- Try and find the required name in the exports
                    let decl_spec = ImpDeclSpec { is_mod = mod_name, is_as = mod_name
                                                , is_qual = False, is_dloc = noSrcSpan }
                        imp_spec = ImpSpec decl_spec ImpAll
                        env = mkGlobalRdrEnv (gresFromAvails (Just imp_spec) (mi_exports iface))
                    case lookupGRE_RdrName rdr_name env of
                        [gre] -> return (Just (greMangledName gre, iface))
                        []    -> return Nothing
                        _     -> panic "lookupRdrNameInModule"

                Nothing -> throwCmdLineErrorS dflags $ hsep [text "Could not determine the exports of the module", ppr mod_name]
        err -> throwCmdLineErrorS dflags $ cannotFindModule hsc_env mod_name err
  where
    dflags = hsc_dflags hsc_env
    doc = text "contains a name used in an invocation of lookupRdrNameInModule"

wrongTyThingError :: Name -> TyThing -> SDoc
wrongTyThingError name got_thing = hsep [text "The name", ppr name, ptext (sLit "is not that of a value but rather a"), pprTyThingCategory got_thing]

missingTyThingError :: Name -> SDoc
missingTyThingError name = hsep [text "The name", ppr name, ptext (sLit "is not in the type environment: are you sure it exists?")]

throwCmdLineErrorS :: DynFlags -> SDoc -> IO a
throwCmdLineErrorS dflags = throwCmdLineError . showSDoc dflags

throwCmdLineError :: String -> IO a
throwCmdLineError = throwGhcExceptionIO . CmdLineError
