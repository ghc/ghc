

-- | Dynamically lookup up values from modules and loading them.
module GHC.Runtime.Loader (
        initializePlugins, initializeSessionPlugins,
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
import GHC.Data.FastString

import GHC.Driver.DynFlags
import GHC.Driver.Ppr
import GHC.Driver.Hooks
import GHC.Driver.Plugins
import GHC.Driver.Plugins.External

import GHC.Linker.Loader       ( loadModule, loadName )
import GHC.Runtime.Interpreter ( wormhole )
import GHC.Runtime.Interpreter.Types

import GHC.Rename.Names ( gresFromAvails )

import GHC.Tc.Utils.Monad      ( initTcInteractive, initIfaceTcRn )
import GHC.Iface.Load          ( loadPluginInterface, cannotFindModule )
import GHC.Builtin.Names ( pluginTyConName, frontendPluginTyConName )

import GHC.Driver.Env
import GHCi.RemoteTypes     ( HValue )
import GHC.Core.Type        ( Type, mkTyConTy )
import GHC.Core.TyCo.Compare( eqType )
import GHC.Core.TyCon       ( TyCon(tyConName) )


import GHC.Types.SrcLoc        ( noSrcSpan )
import GHC.Types.Name    ( Name, nameModule, nameModule_maybe )
import GHC.Types.Id      ( idType )
import GHC.Types.PkgQual
import GHC.Types.TyThing
import GHC.Types.Name.Occurrence ( OccName, mkVarOccFS )
import GHC.Types.Name.Reader
import GHC.Types.Unique.DFM

import GHC.Unit.Finder         ( findPluginModule, FindResult(..) )
import GHC.Driver.Config.Finder ( initFinderOpts )
import GHC.Driver.Config.Diagnostic ( initIfaceMessageOpts )
import GHC.Unit.Module   ( Module, ModuleName, thisGhcUnit, GenModule(moduleUnit), IsBootInterface(NotBoot) )
import GHC.Unit.Module.ModIface
import GHC.Unit.Env

import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Utils.Misc ( HasDebugCallStack )
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Exception

import Control.Monad     ( unless )
import Data.Maybe        ( mapMaybe )
import Unsafe.Coerce     ( unsafeCoerce )
import GHC.Linker.Types
import Data.List (unzip4)
import GHC.Iface.Errors.Ppr
import GHC.Driver.Monad

{- Note [Timing of plugin initialization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Plugins needs to be initialised as soon as possible in the pipeline. This is because
driver plugins are executed immediately after being loaded, which can modify anything
in the HscEnv, including the logger and DynFlags (for example #21279). For example,
in ghc/Main.hs the logger is used almost immediately after the session has been initialised
and so if a user overwrites the logger expecting all output to go there then unless
the plugins are initialised before that point then unexpected things will happen.

We initialise plugins in ghc/Main.hs for the main ghc executable.

When people are using the GHC API, they also need to initialise plugins
at the highest level possible for things to work as expected. We keep
some defensive calls to plugin initialisation in functions like `load'` and `oneshot`
to catch cases where API users have not initialised their own plugins.

In addition to this, there needs to be an initialisation call for each module
just in case the user has enabled a plugin just for that module using OPTIONS_GHC
pragma.

-}

-- | Initialise plugins specified by the current DynFlags and update the session.
initializeSessionPlugins :: GhcMonad m => m ()
initializeSessionPlugins = getSession >>= liftIO . initializePlugins >>= setSession

-- | Loads the plugins specified in the pluginModNames field of the dynamic
-- flags. Should be called after command line arguments are parsed, but before
-- actual compilation starts. Idempotent operation. Should be re-called if
-- pluginModNames or pluginModNameOpts changes.
initializePlugins :: HscEnv -> IO HscEnv
initializePlugins hsc_env
    -- check that plugin specifications didn't change

    -- dynamic plugins
  | loaded_plugins <- loadedPlugins (hsc_plugins hsc_env)
  , map lpModuleName loaded_plugins == reverse (pluginModNames dflags)
  , all same_args loaded_plugins

    -- external plugins
  , external_plugins <- externalPlugins (hsc_plugins hsc_env)
  , check_external_plugins external_plugins (externalPluginSpecs dflags)

    -- ensure we have initialised static plugins
  , all spInitialised (staticPlugins (hsc_plugins hsc_env))

  = return hsc_env -- no change, no need to reload plugins

  | otherwise
  = do (loaded_plugins, links, pkgs) <- loadPlugins hsc_env
       external_plugins <- loadExternalPlugins (externalPluginSpecs dflags)
       let plugins' = (hsc_plugins hsc_env) { staticPlugins    = map (\sp -> sp{ spInitialised = True }) $ staticPlugins (hsc_plugins hsc_env)
                                            , externalPlugins  = external_plugins
                                            , loadedPlugins    = loaded_plugins
                                            , loadedPluginDeps = (links, pkgs)
                                            }
       let hsc_env' = hsc_env { hsc_plugins = plugins' }
       withPlugins (hsc_plugins hsc_env') driverPlugin hsc_env'
  where
    dflags = hsc_dflags hsc_env
    -- dynamic plugins
    plugin_args = pluginModNameOpts dflags
    same_args p = paArguments (lpPlugin p) == argumentsForPlugin p plugin_args
    argumentsForPlugin p = map snd . filter ((== lpModuleName p) . fst)
    -- external plugins
    check_external_plugin p spec = and
      [ epUnit                p  == esp_unit_id spec
      , epModule              p  == esp_module spec
      , paArguments (epPlugin p) == esp_args spec
      ]
    check_external_plugins eps specs = case (eps,specs) of
      ([]  , [])  -> True
      (_   , [])  -> False -- some external plugin removed
      ([]  , _ )  -> False -- some external plugin added
      (p:ps,s:ss) -> check_external_plugin p s && check_external_plugins ps ss

loadPlugins :: HscEnv -> IO ([LoadedPlugin], [Linkable], PkgsLoaded)
loadPlugins hsc_env
  = do { unless (null to_load) $
           checkExternalInterpreter hsc_env
       ; plugins_with_deps <- mapM loadPlugin to_load
       ; let (plugins, ifaces, links, pkgs) = unzip4 plugins_with_deps
       ; return (zipWith attachOptions to_load (zip plugins ifaces), concat links, foldl' plusUDFM emptyUDFM pkgs)
       }
  where
    dflags  = hsc_dflags hsc_env
    to_load = reverse $ pluginModNames dflags

    attachOptions mod_nm (plug, mod) =
        LoadedPlugin (PluginWithArgs plug (reverse options)) mod
      where
        options = [ option | (opt_mod_nm, option) <- pluginModNameOpts dflags
                            , opt_mod_nm == mod_nm ]
    loadPlugin = loadPlugin' (mkVarOccFS (fsLit "plugin")) pluginTyConName hsc_env


loadFrontendPlugin :: HscEnv -> ModuleName -> IO (FrontendPlugin, [Linkable], PkgsLoaded)
loadFrontendPlugin hsc_env mod_name = do
    checkExternalInterpreter hsc_env
    (plugin, _iface, links, pkgs)
      <- loadPlugin' (mkVarOccFS (fsLit "frontendPlugin")) frontendPluginTyConName
           hsc_env mod_name
    return (plugin, links, pkgs)

-- #14335
checkExternalInterpreter :: HscEnv -> IO ()
checkExternalInterpreter hsc_env = case interpInstance <$> hsc_interp hsc_env of
  Just (ExternalInterp {})
    -> throwIO (InstallationError "Plugins require -fno-external-interpreter")
  _ -> pure ()

loadPlugin' :: OccName -> Name -> HscEnv -> ModuleName -> IO (a, ModIface, [Linkable], PkgsLoaded)
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
        ; case thisGhcUnit == (moduleUnit . nameModule . tyConName) plugin_tycon of {
            False ->
                throwGhcExceptionIO (CmdLineError $ showSDoc dflags $ hsep
                          [ text "The plugin module", ppr mod_name
                          , text "was built with a compiler that is incompatible with the one loading it"
                          ]) ;
            True ->
     do { eith_plugin <- getValueSafely hsc_env name (mkTyConTy plugin_tycon)
        ; case eith_plugin of
            Left actual_type ->
                throwGhcExceptionIO (CmdLineError $
                    showSDocForUser dflags (ue_units (hsc_unit_env hsc_env))
                      alwaysQualify $ hsep
                          [ text "The value", ppr name
                          , text "with type", ppr actual_type
                          , text "did not have the type"
                          , text "GHC.Plugins.Plugin"
                          , text "as required"])
            Right (plugin, links, pkgs) -> return (plugin, mod_iface, links, pkgs) } } } } }


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
-- If the value found was not of the correct type, returns @Left <actual_type>@. Any other condition results in an exception:
--
-- * If we could not load the names module
-- * If the thing being loaded is not a value
-- * If the Name does not exist in the module
-- * If the link failed

getValueSafely :: HscEnv -> Name -> Type -> IO (Either Type (a, [Linkable], PkgsLoaded))
getValueSafely hsc_env val_name expected_type = do
  eith_hval <- case getValueSafelyHook hooks of
    Nothing -> getHValueSafely interp hsc_env val_name expected_type
    Just h  -> h                      hsc_env val_name expected_type
  case eith_hval of
    Left actual_type -> return (Left actual_type)
    Right (hval, links, pkgs) -> do
      value <- lessUnsafeCoerce logger "getValueSafely" hval
      return (Right (value, links, pkgs))
  where
    interp = hscInterp hsc_env
    logger = hsc_logger hsc_env
    hooks  = hsc_hooks hsc_env

getHValueSafely :: Interp -> HscEnv -> Name -> Type -> IO (Either Type (HValue, [Linkable], PkgsLoaded))
getHValueSafely interp hsc_env val_name expected_type = do
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
                    Just mod -> do loadModule interp hsc_env mod
                                   return ()
                    Nothing ->  return ()
                -- Find the value that we just linked in and cast it given that we have proved it's type
                hval <- do
                  (v, links, pkgs) <- loadName interp hsc_env val_name
                  hv <- wormhole interp v
                  return (hv, links, pkgs)
                return (Right hval)
             else return (Left (idType id))
        Just val_thing -> throwCmdLineErrorS dflags $ wrongTyThingError val_name val_thing
   where dflags = hsc_dflags hsc_env

-- | Coerce a value as usual, but:
--
-- 1) Evaluate it immediately to get a segfault early if the coercion was wrong
--
-- 2) Wrap it in some debug messages at verbosity 3 or higher so we can see what happened
--    if it /does/ segfault
lessUnsafeCoerce :: Logger -> String -> a -> IO b
lessUnsafeCoerce logger context what = do
    debugTraceMsg logger 3 $
        (text "Coercing a value in") <+> (text context) <> (text "...")
    output <- evaluate (unsafeCoerce what)
    debugTraceMsg logger 3 (text "Successfully evaluated coercion")
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
lookupRdrNameInModuleForPlugins :: HasDebugCallStack
                                => HscEnv -> ModuleName -> RdrName
                                -> IO (Maybe (Name, ModIface))
lookupRdrNameInModuleForPlugins hsc_env mod_name rdr_name = do
    let dflags     = hsc_dflags hsc_env
    let fopts      = initFinderOpts dflags
    let fc         = hsc_FC hsc_env
    let unit_env   = hsc_unit_env hsc_env
    let unit_state = ue_units unit_env
    let mhome_unit = hsc_home_unit_maybe hsc_env
    -- First find the unit the module resides in by searching exposed units and home modules
    found_module <- findPluginModule fc fopts unit_state mhome_unit mod_name
    case found_module of
        Found _ mod -> do
            -- Find the exports of the module
            (_, mb_iface) <- initTcInteractive hsc_env $
                             initIfaceTcRn $
                             loadPluginInterface doc mod
            case mb_iface of
                Just iface -> do
                    -- Try and find the required name in the exports
                    let decl_spec = ImpDeclSpec { is_mod = mod, is_as = mod_name, is_pkg_qual = NoPkgQual
                                                , is_qual = False, is_dloc = noSrcSpan, is_isboot = NotBoot }
                        imp_spec = ImpSpec decl_spec ImpAll
                        env = mkGlobalRdrEnv
                            $ gresFromAvails hsc_env (Just imp_spec) (mi_exports iface)
                    case lookupGRE env (LookupRdrName rdr_name (RelevantGREsFOS WantNormal)) of
                        [gre] -> return (Just (greName gre, iface))
                        []    -> return Nothing
                        _     -> panic "lookupRdrNameInModule"

                Nothing -> throwCmdLineErrorS dflags $ hsep [text "Could not determine the exports of the module", ppr mod_name]
        err ->
          let opts   = initIfaceMessageOpts dflags
              err_txt = missingInterfaceErrorDiagnostic opts
                      $ cannotFindModule hsc_env mod_name err
          in throwCmdLineErrorS dflags err_txt
  where
    doc = text "contains a name used in an invocation of lookupRdrNameInModule"

wrongTyThingError :: Name -> TyThing -> SDoc
wrongTyThingError name got_thing = hsep [text "The name", ppr name, text "is not that of a value but rather a", pprTyThingCategory got_thing]

missingTyThingError :: Name -> SDoc
missingTyThingError name = hsep [text "The name", ppr name, text "is not in the type environment: are you sure it exists?"]

throwCmdLineErrorS :: DynFlags -> SDoc -> IO a
throwCmdLineErrorS dflags = throwCmdLineError . showSDoc dflags

throwCmdLineError :: String -> IO a
throwCmdLineError = throwGhcExceptionIO . CmdLineError
