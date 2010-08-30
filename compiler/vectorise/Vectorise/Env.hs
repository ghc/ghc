
module Vectorise.Env (
	Scope(..),

	-- * Local Environments
	LocalEnv(..),
	emptyLocalEnv,
	
	-- * Global Environments
	GlobalEnv(..),
	initGlobalEnv,
	extendImportedVarsEnv,
	extendScalars,
	setFamInstEnv,
	extendTyConsEnv,
	extendDataConsEnv,
	extendPAFunsEnv,
	setPRFunsEnv,
	setBoxedTyConsEnv,
	updVectInfo
) where
import HscTypes
import InstEnv
import FamInstEnv
import CoreSyn
import TyCon
import DataCon
import VarEnv
import VarSet
import Var
import Name
import NameEnv
import FastString


-- | Indicates what scope something (a variable) is in.
data Scope a b 
	= Global a 
	| Local  b


-- LocalEnv -------------------------------------------------------------------
-- | The local environment.
data LocalEnv
	= LocalEnv {
        -- Mapping from local variables to their vectorised and lifted versions.
            local_vars		:: VarEnv (Var, Var)

        -- In-scope type variables.
        , local_tyvars		:: [TyVar]

        -- Mapping from tyvars to their PA dictionaries.
        , local_tyvar_pa	:: VarEnv CoreExpr

        -- Local binding name.
        , local_bind_name	:: FastString
        }


-- | Create an empty local environment.
emptyLocalEnv :: LocalEnv
emptyLocalEnv = LocalEnv {
                   local_vars     = emptyVarEnv
                 , local_tyvars   = []
                 , local_tyvar_pa = emptyVarEnv
                 , local_bind_name  = fsLit "fn"
                 }


-- GlobalEnv ------------------------------------------------------------------
-- | The global environment.
--	These are things the exist at top-level.
data GlobalEnv 
	= GlobalEnv {
        -- | Mapping from global variables to their vectorised versions.
          global_vars		:: VarEnv Var

        -- | Purely scalar variables. Code which mentions only these
        --   variables doesn't have to be lifted.
        , global_scalars	:: VarSet

        -- | Exported variables which have a vectorised version.
        , global_exported_vars	:: VarEnv (Var, Var)

        -- | Mapping from TyCons to their vectorised versions.
        --   TyCons which do not have to be vectorised are mapped to themselves.
        , global_tycons		:: NameEnv TyCon

        -- | Mapping from DataCons to their vectorised versions.
        , global_datacons	:: NameEnv DataCon

        -- | Mapping from TyCons to their PA dfuns.
	, global_pa_funs	:: NameEnv Var

        -- | Mapping from TyCons to their PR dfuns.
        , global_pr_funs	:: NameEnv Var

        -- | Mapping from unboxed TyCons to their boxed versions.
        , global_boxed_tycons	:: NameEnv TyCon

        -- | External package inst-env & home-package inst-env for class instances.
        , global_inst_env	:: (InstEnv, InstEnv)

        -- | External package inst-env & home-package inst-env for family instances.
        , global_fam_inst_env	:: FamInstEnvs

        -- | Hoisted bindings.
        , global_bindings	:: [(Var, CoreExpr)]
        }


-- | Create an initial global environment
initGlobalEnv :: VectInfo -> (InstEnv, InstEnv) -> FamInstEnvs -> GlobalEnv
initGlobalEnv info instEnvs famInstEnvs
	= GlobalEnv 
	{ global_vars          = mapVarEnv snd $ vectInfoVar info
	, global_scalars       = emptyVarSet
	, global_exported_vars = emptyVarEnv
	, global_tycons        = mapNameEnv snd $ vectInfoTyCon info
	, global_datacons      = mapNameEnv snd $ vectInfoDataCon info
	, global_pa_funs       = mapNameEnv snd $ vectInfoPADFun info
	, global_pr_funs       = emptyNameEnv
	, global_boxed_tycons  = emptyNameEnv
	, global_inst_env      = instEnvs
	, global_fam_inst_env  = famInstEnvs
	, global_bindings      = []
	}



-- Operators on Global Environments -------------------------------------------
-- | Extend the list of global variables in an environment.
extendImportedVarsEnv :: [(Var, Var)] -> GlobalEnv -> GlobalEnv
extendImportedVarsEnv ps genv
  = genv { global_vars	 = extendVarEnvList (global_vars genv) ps }


-- | Extend the set of scalar variables in an environment.
extendScalars :: [Var] -> GlobalEnv -> GlobalEnv
extendScalars vs genv
  = genv { global_scalars = extendVarSetList (global_scalars genv) vs }


-- | Set the list of type family instances in an environment.
setFamInstEnv :: FamInstEnv -> GlobalEnv -> GlobalEnv
setFamInstEnv l_fam_inst genv
  = genv { global_fam_inst_env = (g_fam_inst, l_fam_inst) }
  where (g_fam_inst, _) = global_fam_inst_env genv


-- | Extend the list of type constructors in an environment.
extendTyConsEnv :: [(Name, TyCon)] -> GlobalEnv -> GlobalEnv
extendTyConsEnv ps genv
  = genv { global_tycons = extendNameEnvList (global_tycons genv) ps }


-- | Extend the list of data constructors in an environment.
extendDataConsEnv :: [(Name, DataCon)] -> GlobalEnv -> GlobalEnv
extendDataConsEnv ps genv
  = genv { global_datacons = extendNameEnvList (global_datacons genv) ps }


-- | Extend the list of PA functions in an environment.
extendPAFunsEnv :: [(Name, Var)] -> GlobalEnv -> GlobalEnv
extendPAFunsEnv ps genv
  = genv { global_pa_funs = extendNameEnvList (global_pa_funs genv) ps }


-- | Set the list of PR functions in an environment.
setPRFunsEnv :: [(Name, Var)] -> GlobalEnv -> GlobalEnv
setPRFunsEnv ps genv
  = genv { global_pr_funs = mkNameEnv ps }


-- | Set the list of boxed type constructor in an environment.
setBoxedTyConsEnv :: [(Name, TyCon)] -> GlobalEnv -> GlobalEnv
setBoxedTyConsEnv ps genv
  = genv { global_boxed_tycons = mkNameEnv ps }


-- | TODO: What is this for?
updVectInfo :: GlobalEnv -> TypeEnv -> VectInfo -> VectInfo
updVectInfo env tyenv info
  = info 
    { vectInfoVar     = global_exported_vars env
    , vectInfoTyCon   = mk_env typeEnvTyCons global_tycons
    , vectInfoDataCon = mk_env typeEnvDataCons global_datacons
    , vectInfoPADFun  = mk_env typeEnvTyCons global_pa_funs
    }
  where
    mk_env from_tyenv from_env 
	= mkNameEnv [(name, (from,to))
                        | from     <- from_tyenv tyenv
                        , let name =  getName from
                        , Just to  <- [lookupNameEnv (from_env env) name]]

