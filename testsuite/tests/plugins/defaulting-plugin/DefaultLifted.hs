{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances, DataKinds, PatternSynonyms, StandaloneDeriving, GeneralizedNewtypeDeriving, PolyKinds #-}
{-# OPTIONS -Wno-orphans #-}
module DefaultLifted(DefaultType,plugin) where
import GHC.Plugins
import GHC.Tc.Types.Constraint
import GHC.Tc.Plugin
import GHC.Core.InstEnv
import GHC.Tc.Solver (approximateWC)
import GHC.Unit.Finder (findPluginModule)
import GHC.Driver.Config.Finder (initFinderOpts)
import Data.List
import GHC.Tc.Types
import qualified Data.Map as M
import Control.Monad (liftM2)
import GHC.Tc.Utils.TcType

class DefaultType x (y :: x)

instance Eq Type where
  (==) = eqType
instance Ord Type where
  compare = nonDetCmpType
instance Semigroup (TcPluginM [a]) where
  (<>) = liftM2 (++)
instance Monoid (TcPluginM [a]) where
  mempty = pure mempty

plugin :: Plugin
plugin = defaultPlugin {
  defaultingPlugin = install,
  pluginRecompile = purePlugin
  }

install :: p -> Maybe GHC.Tc.Types.DefaultingPlugin
install _ = Just $ DefaultingPlugin { dePluginInit = initialize
                                    , dePluginRun  = run
                                    , dePluginStop = stop
                                    }

pattern FoundModule :: Module -> FindResult
pattern FoundModule a <- Found _ a
fr_mod :: a -> a
fr_mod = id

lookupModule :: ModuleName -- ^ Name of the module
             -> TcPluginM Module
lookupModule mod_nm = do
  hsc_env <- getTopEnv
  let dflags    = hsc_dflags hsc_env
  let fopts     = initFinderOpts dflags
  let fc        = hsc_FC hsc_env
  let units     = hsc_units hsc_env
  let home_unit = hsc_home_unit hsc_env
  -- found_module <- findPluginModule fc fopts units home_unit mod_name
  found_module <- tcPluginIO $ findPluginModule fc fopts units (Just home_unit) mod_nm
  case found_module of
    FoundModule h -> return (fr_mod h)
    _          -> do
      found_module' <- findImportedModule mod_nm (ThisPkg (homeUnitAsUnit home_unit))
      case found_module' of
        FoundModule h -> return (fr_mod h)
        _          -> panicDoc "Unable to resolve module looked up by plugin: "
                               (ppr mod_nm)

data PluginState = PluginState { defaultClassName :: Name }

-- | Find a 'Name' in a 'Module' given an 'OccName'
lookupName :: Module -> OccName -> TcPluginM Name
lookupName md occ = lookupOrig md occ

solveDefaultType :: PluginState -> [Ct] -> TcPluginM [DefaultingProposal]
solveDefaultType _     []      = return []
solveDefaultType state wanteds = do
  envs <- getInstEnvs
  insts <- classInstances envs <$> tcLookupClass (defaultClassName state)
  let defaults =
        foldl' (\m inst ->
                 case is_tys inst of
                   [matchty, replacety] -> M.insertWith (++) matchty [replacety] m
                   _ -> error "Unsupported defaulting type")
               M.empty insts
  let groups =
        foldl' (\m wanted ->
                  foldl' (\m' var -> M.insertWith (++) var [wanted] m')
                         m
                         (filter (isVariableDefaultable defaults) $ tyCoVarsOfCtList wanted))
               M.empty wanteds
  M.foldMapWithKey (\var cts ->
                    case M.lookup (tyVarKind var) defaults of
                      Nothing -> error "Bug, we already checked that this variable has a default"
                      Just deftys -> do
                        pure [DefaultingProposal [[(var, defty)] | defty <- deftys] cts])
    groups
  where isVariableDefaultable defaults v = isAmbiguousTyVar v && M.member (tyVarKind v) defaults

lookupDefaultTypes :: TcPluginM PluginState
lookupDefaultTypes = do
    md   <- lookupModule (mkModuleName "DefaultLifted")
    name <- lookupName md (mkTcOcc "DefaultType")
    pure $ PluginState { defaultClassName = name }

initialize :: TcPluginM PluginState
initialize = do
  lookupDefaultTypes

run :: PluginState -> WantedConstraints -> TcPluginM [DefaultingProposal]
run s ws = do
  solveDefaultType s (ctsElts $ approximateWC False ws)

stop :: Monad m => p -> m ()
stop _ = do
  return ()
