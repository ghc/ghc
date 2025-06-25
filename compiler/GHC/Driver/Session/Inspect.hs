{-# LANGUAGE LambdaCase #-}

-- | GHC API utilities for inspecting the GHC session
module GHC.Driver.Session.Inspect where

import GHC.Prelude
import GHC.Data.Maybe
import Control.Monad

import GHC.ByteCode.Types
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv
import GHC.Driver.Env
import GHC.Driver.Main
import GHC.Driver.Monad
import GHC.Driver.Session
import GHC.Rename.Names
import GHC.Runtime.Context
import GHC.Runtime.Interpreter
import GHC.HsToCore.Breakpoints (ModBreaks)
import GHC.Types.Avail
import GHC.Types.Name
import GHC.Types.Name.Ppr
import GHC.Types.Name.Reader
import GHC.Types.Name.Set
import GHC.Types.PkgQual
import GHC.Types.SafeHaskell
import GHC.Types.SrcLoc
import GHC.Types.TyThing
import GHC.Types.TypeEnv
import GHC.Unit.External
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface
import GHC.Utils.Misc
import GHC.Utils.Outputable
import qualified GHC.Unit.Home.Graph as HUG

-- %************************************************************************
-- %*                                                                      *
--             Inspecting the session
-- %*                                                                      *
-- %************************************************************************

-- | Get the module dependency graph.
getModuleGraph :: GhcMonad m => m ModuleGraph -- ToDo: DiGraph ModSummary
getModuleGraph = liftM hsc_mod_graph getSession

{-# DEPRECATED isLoaded "Prefer 'isLoadedModule' and 'isLoadedHomeModule'" #-}
-- | Return @True@ \<==> module is loaded.
isLoaded :: GhcMonad m => ModuleName -> m Bool
isLoaded m = withSession $ \hsc_env -> liftIO $ do
  hmis <- HUG.lookupAllHug (hsc_HUG hsc_env) m
  return $! not (null hmis)

-- | Check whether a 'ModuleName' is found in the 'HomePackageTable'
-- for the given 'UnitId'.
isLoadedModule :: GhcMonad m => UnitId -> ModuleName -> m Bool
isLoadedModule uid m = withSession $ \hsc_env -> liftIO $ do
  hmi <- HUG.lookupHug (hsc_HUG hsc_env) uid m
  return $! isJust hmi

-- | Check whether 'Module' is part of the 'HomeUnitGraph'.
--
-- Similar to 'isLoadedModule', but for 'Module's.
isLoadedHomeModule :: GhcMonad m => Module -> m Bool
isLoadedHomeModule m = withSession $ \hsc_env -> liftIO $ do
  hmi <- HUG.lookupHugByModule m (hsc_HUG hsc_env)
  return $! isJust hmi

-- | Return the bindings for the current interactive session.
getBindings :: GhcMonad m => m [TyThing]
getBindings = withSession $ \hsc_env ->
    return $ icInScopeTTs $ hsc_IC hsc_env

-- | Return the instances for the current interactive session.
getInsts :: GhcMonad m => m ([ClsInst], [FamInst])
getInsts = withSession $ \hsc_env ->
    let (inst_env, fam_env) = ic_instances (hsc_IC hsc_env)
    in return (instEnvElts inst_env, fam_env)

getNamePprCtx :: GhcMonad m => m NamePprCtx
getNamePprCtx = withSession $ \hsc_env -> do
  return $ icNamePprCtx (hsc_unit_env hsc_env) (hsc_IC hsc_env)

-- | Container for information about a 'Module'.
data ModuleInfo = ModuleInfo {
        minf_type_env  :: TypeEnv,
        minf_exports   :: [AvailInfo],
        minf_instances :: [ClsInst],
        minf_iface     :: Maybe ModIface,
        minf_safe      :: SafeHaskellMode,
        minf_modBreaks :: Maybe InternalModBreaks
  }
        -- We don't want HomeModInfo here, because a ModuleInfo applies
        -- to package modules too.

-- | Request information about a loaded 'Module'
getModuleInfo :: GhcMonad m => Module -> m (Maybe ModuleInfo)  -- XXX: Maybe X
getModuleInfo mdl = withSession $ \hsc_env -> do
  if HUG.memberHugUnit (moduleUnit mdl) (hsc_HUG hsc_env)
        then liftIO $ getHomeModuleInfo hsc_env mdl
        else liftIO $ getPackageModuleInfo hsc_env mdl

getPackageModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
getPackageModuleInfo hsc_env mdl
  = do  eps <- hscEPS hsc_env
        iface <- hscGetModuleInterface hsc_env mdl
        let
            avails = mi_exports iface
            pte    = eps_PTE eps
            tys    = [ ty | name <- concatMap availNames avails,
                            Just ty <- [lookupTypeEnv pte name] ]

        return (Just (ModuleInfo {
                        minf_type_env  = mkTypeEnv tys,
                        minf_exports   = avails,
                        minf_instances = error "getModuleInfo: instances for package module unimplemented",
                        minf_iface     = Just iface,
                        minf_safe      = getSafeMode $ mi_trust iface,
                        minf_modBreaks = Nothing
                }))

availsToGlobalRdrEnv :: HasDebugCallStack => HscEnv -> Module -> [AvailInfo] -> IfGlobalRdrEnv
availsToGlobalRdrEnv hsc_env mod avails
  = forceGlobalRdrEnv rdr_env
    -- See Note [Forcing GREInfo] in GHC.Types.GREInfo.
  where
    rdr_env = mkGlobalRdrEnv (gresFromAvails hsc_env (Just imp_spec) avails)
      -- We're building a GlobalRdrEnv as if the user imported
      -- all the specified modules into the global interactive module
    imp_spec = ImpSpec { is_decl = decl, is_item = ImpAll}
    decl = ImpDeclSpec { is_mod = mod, is_as = moduleName mod,
                         is_qual = False, is_isboot = NotBoot, is_pkg_qual = NoPkgQual,
                         is_dloc = srcLocSpan interactiveSrcLoc,
                         is_level = NormalLevel }

getHomeModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
getHomeModuleInfo hsc_env mdl =
  HUG.lookupHugByModule mdl (hsc_HUG hsc_env) >>= \case
    Nothing  -> return Nothing
    Just hmi -> do
      let details  = hm_details hmi
          iface    = hm_iface hmi
      return (Just (ModuleInfo {
                        minf_type_env  = md_types details,
                        minf_exports   = md_exports details,
                         -- NB: already forced. See Note [Forcing GREInfo] in GHC.Types.GREInfo.
                        minf_instances = instEnvElts $ md_insts details,
                        minf_iface     = Just iface,
                        minf_safe      = getSafeMode $ mi_trust iface,
                        minf_modBreaks = getModBreaks hmi
                        }))

-- | The list of top-level entities defined in a module
modInfoTyThings :: ModuleInfo -> [TyThing]
modInfoTyThings minf = typeEnvElts (minf_type_env minf)

modInfoExports :: ModuleInfo -> [Name]
modInfoExports minf = concatMap availNames $! minf_exports minf

modInfoExportsWithSelectors :: ModuleInfo -> [Name]
modInfoExportsWithSelectors minf = concatMap availNames $! minf_exports minf

-- | Returns the instances defined by the specified module.
-- Warning: currently unimplemented for package modules.
modInfoInstances :: ModuleInfo -> [ClsInst]
modInfoInstances = minf_instances

modInfoIsExportedName :: ModuleInfo -> Name -> Bool
modInfoIsExportedName minf name = elemNameSet name (availsToNameSet (minf_exports minf))

mkNamePprCtxForModule ::
  GhcMonad m =>
  Module     ->
  ModuleInfo ->
  m NamePprCtx
mkNamePprCtxForModule mod minf = withSession $ \hsc_env -> do
  let name_ppr_ctx = mkNamePprCtx ptc (hsc_unit_env hsc_env) (availsToGlobalRdrEnv hsc_env mod (minf_exports minf))
      ptc = initPromotionTickContext (hsc_dflags hsc_env)
  return name_ppr_ctx

modInfoLookupName :: GhcMonad m =>
                     ModuleInfo -> Name
                  -> m (Maybe TyThing) -- XXX: returns a Maybe X
modInfoLookupName minf name = withSession $ \hsc_env -> do
   case lookupTypeEnv (minf_type_env minf) name of
     Just tyThing -> return (Just tyThing)
     Nothing      -> liftIO (lookupType hsc_env name)

modInfoIface :: ModuleInfo -> Maybe ModIface
modInfoIface = minf_iface

-- | Retrieve module safe haskell mode
modInfoSafe :: ModuleInfo -> SafeHaskellMode
modInfoSafe = minf_safe

modInfoModBreaks :: ModuleInfo -> Maybe InternalModBreaks
modInfoModBreaks = minf_modBreaks

