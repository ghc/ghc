
module Vectorise.Monad.Global (
	readGEnv,
	setGEnv,
	updGEnv,
	
	-- * Vars
	defGlobalVar,
	
	-- * Scalars
	globalScalars,
	
	-- * TyCons
	lookupTyCon,
	lookupBoxedTyCon,
	defTyCon,
	
	-- * Datacons
	lookupDataCon,
	defDataCon,
	
	-- * PA Dictionaries
	lookupTyConPA,
	defTyConPA,
	defTyConPAs,
	
	-- * PR Dictionaries
	lookupTyConPR
) where
import Vectorise.Monad.Base
import Vectorise.Env
import TyCon
import DataCon
import NameEnv
import Var
import VarEnv
import VarSet


-- Global Environment ---------------------------------------------------------
-- | Project something from the global environment.
readGEnv :: (GlobalEnv -> a) -> VM a
readGEnv f	= VM $ \_ genv lenv -> return (Yes genv lenv (f genv))


-- | Set the value of the global environment.
setGEnv :: GlobalEnv -> VM ()
setGEnv genv	= VM $ \_ _ lenv -> return (Yes genv lenv ())


-- | Update the global environment using the provided function.
updGEnv :: (GlobalEnv -> GlobalEnv) -> VM ()
updGEnv f	= VM $ \_ genv lenv -> return (Yes (f genv) lenv ())


-- Vars -----------------------------------------------------------------------
-- | Add a mapping between a global var and its vectorised version to the state.
defGlobalVar :: Var -> Var -> VM ()
defGlobalVar v v' = updGEnv $ \env ->
  env { global_vars = extendVarEnv (global_vars env) v v'
      , global_exported_vars = upd (global_exported_vars env)
      }
  where
    upd env | isExportedId v = extendVarEnv env v (v, v')
            | otherwise      = env


-- Scalars --------------------------------------------------------------------
-- | Get the set of global scalar variables.
globalScalars :: VM VarSet
globalScalars 
	= readGEnv global_scalars


-- TyCons ---------------------------------------------------------------------
-- | Lookup the vectorised version of a `TyCon` from the global environment.
lookupTyCon :: TyCon -> VM (Maybe TyCon)
lookupTyCon tc
  | isUnLiftedTyCon tc || isTupleTyCon tc
  = return (Just tc)

  | otherwise 
  = readGEnv $ \env -> lookupNameEnv (global_tycons env) (tyConName tc)


-- | Lookup the vectorised version of a boxed `TyCon` from the global environment.
lookupBoxedTyCon :: TyCon -> VM (Maybe TyCon)
lookupBoxedTyCon tc 
	= readGEnv $ \env -> lookupNameEnv (global_boxed_tycons env)
                                           (tyConName tc)


-- | Add a mapping between plain and vectorised `TyCon`s to the global environment.
defTyCon :: TyCon -> TyCon -> VM ()
defTyCon tc tc' = updGEnv $ \env ->
  env { global_tycons = extendNameEnv (global_tycons env) (tyConName tc) tc' }


-- DataCons -------------------------------------------------------------------
-- | Lookup the vectorised version of a `DataCon` from the global environment.
lookupDataCon :: DataCon -> VM (Maybe DataCon)
lookupDataCon dc
  | isTupleTyCon (dataConTyCon dc) 
  = return (Just dc)

  | otherwise 
  = readGEnv $ \env -> lookupNameEnv (global_datacons env) (dataConName dc)


-- | Add the mapping between plain and vectorised `DataCon`s to the global environment.
defDataCon :: DataCon -> DataCon -> VM ()
defDataCon dc dc' = updGEnv $ \env ->
  env { global_datacons = extendNameEnv (global_datacons env) (dataConName dc) dc' }


-- PA dictionaries ------------------------------------------------------------
-- | Lookup a PA `TyCon` from the global environment.
lookupTyConPA :: TyCon -> VM (Maybe Var)
lookupTyConPA tc
	= readGEnv $ \env -> lookupNameEnv (global_pa_funs env) (tyConName tc)


-- | Add a mapping between a PA TyCon and is vectorised version to the global environment.
defTyConPA :: TyCon -> Var -> VM ()
defTyConPA tc pa = updGEnv $ \env ->
  env { global_pa_funs = extendNameEnv (global_pa_funs env) (tyConName tc) pa }


-- | Add several mapping between PA TyCons and their vectorised versions to the global environment.
defTyConPAs :: [(TyCon, Var)] -> VM ()
defTyConPAs ps = updGEnv $ \env ->
  env { global_pa_funs = extendNameEnvList (global_pa_funs env)
                                           [(tyConName tc, pa) | (tc, pa) <- ps] }


-- PR Dictionaries ------------------------------------------------------------
lookupTyConPR :: TyCon -> VM (Maybe Var)
lookupTyConPR tc = readGEnv $ \env -> lookupNameEnv (global_pr_funs env) (tyConName tc)


