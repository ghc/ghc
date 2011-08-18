module Vectorise.Env (
  Scope(..),

  -- * Local Environments
  LocalEnv(..),
  emptyLocalEnv,

  -- * Global Environments
  GlobalEnv(..),
  initGlobalEnv,
  extendImportedVarsEnv,
  setFamEnv,
  extendFamEnv,
  extendTyConsEnv,
  extendDataConsEnv,
  extendPAFunsEnv,
  setPRFunsEnv,
  modVectInfo
) where

import HscTypes
import InstEnv
import FamInstEnv
import CoreSyn
import Type
import TyCon
import DataCon
import VarEnv
import VarSet
import Var
import NameSet
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
            local_vars    :: VarEnv (Var, Var)

        -- In-scope type variables.
        , local_tyvars    :: [TyVar]

        -- Mapping from tyvars to their PA dictionaries.
        , local_tyvar_pa  :: VarEnv CoreExpr

        -- Local binding name.
        , local_bind_name :: FastString
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

-- |The global environment: entities that exist at top-level.
--
data GlobalEnv 
        = GlobalEnv
        { global_vars                 :: VarEnv Var
          -- ^Mapping from global variables to their vectorised versions — aka the /vectorisation
          -- map/.

        , global_vect_decls           :: VarEnv (Type, CoreExpr)
          -- ^Mapping from global variables that have a vectorisation declaration to the right-hand
          -- side of that declaration and its type.  This mapping only applies to non-scalar
          -- vectorisation declarations.  All variables with a scalar vectorisation declaration are
          -- mentioned in 'global_scalars_vars'.

        , global_scalar_vars          :: VarSet
          -- ^Purely scalar variables. Code which mentions only these variables doesn't have to be
          -- lifted.  This includes variables from the current module that have a scalar
          -- vectorisation declaration and those that the vectoriser determines to be scalar.

        , global_scalar_tycons       :: NameSet
          -- ^Type constructors whose values can only contain scalar data and that appear in a
          -- 'VECTORISE SCALAR type' pragma in the current or an imported module.  Scalar code may
          -- only operate on such data.
        
        , global_novect_vars          :: VarSet
          -- ^Variables that are not vectorised.  (They may be referenced in the right-hand sides
          -- of vectorisation declarations, though.)

        , global_exported_vars        :: VarEnv (Var, Var)
          -- ^Exported variables which have a vectorised version.

        , global_tycons               :: NameEnv TyCon
          -- ^Mapping from TyCons to their vectorised versions.
          -- TyCons which do not have to be vectorised are mapped to themselves.

        , global_datacons             :: NameEnv DataCon
          -- ^Mapping from DataCons to their vectorised versions.

        , global_pa_funs              :: NameEnv Var
          -- ^Mapping from TyCons to their PA dfuns.

        , global_pr_funs              :: NameEnv Var
          -- ^Mapping from TyCons to their PR dfuns.

        , global_inst_env             :: (InstEnv, InstEnv)
          -- ^External package inst-env & home-package inst-env for class instances.

        , global_fam_inst_env         :: FamInstEnvs
          -- ^External package inst-env & home-package inst-env for family instances.

        , global_bindings             :: [(Var, CoreExpr)]
          -- ^Hoisted bindings.
        }

-- |Create an initial global environment.
--
initGlobalEnv :: VectInfo -> [CoreVect] -> (InstEnv, InstEnv) -> FamInstEnvs -> GlobalEnv
initGlobalEnv info vectDecls instEnvs famInstEnvs
  = GlobalEnv 
  { global_vars                 = mapVarEnv snd $ vectInfoVar info
  , global_vect_decls           = mkVarEnv vects
  , global_scalar_vars          = vectInfoScalarVars info   `extendVarSetList` scalar_vars
  , global_scalar_tycons        = vectInfoScalarTyCons info `addListToNameSet` scalar_tycons
  , global_novect_vars          = mkVarSet novects
  , global_exported_vars        = emptyVarEnv
  , global_tycons               = mapNameEnv snd $ vectInfoTyCon info
  , global_datacons             = mapNameEnv snd $ vectInfoDataCon info
  , global_pa_funs              = mapNameEnv snd $ vectInfoPADFun info
  , global_pr_funs              = emptyNameEnv
  , global_inst_env             = instEnvs
  , global_fam_inst_env         = famInstEnvs
  , global_bindings             = []
  }
  where
    vects         = [(var, (varType var, exp)) | Vect     var   (Just exp) <- vectDecls]
    scalar_vars   = [var                       | Vect     var   Nothing    <- vectDecls]
    novects       = [var                       | NoVect   var              <- vectDecls]
    scalar_tycons = [tyConName tycon           | VectType tycon Nothing    <- vectDecls]


-- Operators on Global Environments -------------------------------------------

-- |Extend the list of global variables in an environment.
--
extendImportedVarsEnv :: [(Var, Var)] -> GlobalEnv -> GlobalEnv
extendImportedVarsEnv ps genv
  = genv { global_vars = extendVarEnvList (global_vars genv) ps }

-- |Set the list of type family instances in an environment.
--
setFamEnv :: FamInstEnv -> GlobalEnv -> GlobalEnv
setFamEnv l_fam_inst genv
  = genv { global_fam_inst_env = (g_fam_inst, l_fam_inst) }
  where (g_fam_inst, _) = global_fam_inst_env genv

-- |Extend the list of type family instances.
--
extendFamEnv :: [FamInst] -> GlobalEnv -> GlobalEnv
extendFamEnv new genv
  = genv { global_fam_inst_env = (g_fam_inst, extendFamInstEnvList l_fam_inst new) }
  where (g_fam_inst, l_fam_inst) = global_fam_inst_env genv

-- |Extend the list of type constructors in an environment.
--
extendTyConsEnv :: [(Name, TyCon)] -> GlobalEnv -> GlobalEnv
extendTyConsEnv ps genv
  = genv { global_tycons = extendNameEnvList (global_tycons genv) ps }

-- |Extend the list of data constructors in an environment.
--
extendDataConsEnv :: [(Name, DataCon)] -> GlobalEnv -> GlobalEnv
extendDataConsEnv ps genv
  = genv { global_datacons = extendNameEnvList (global_datacons genv) ps }

-- |Extend the list of PA functions in an environment.
--
extendPAFunsEnv :: [(Name, Var)] -> GlobalEnv -> GlobalEnv
extendPAFunsEnv ps genv
  = genv { global_pa_funs = extendNameEnvList (global_pa_funs genv) ps }

-- |Set the list of PR functions in an environment.
--
setPRFunsEnv :: [(Name, Var)] -> GlobalEnv -> GlobalEnv
setPRFunsEnv ps genv
  = genv { global_pr_funs = mkNameEnv ps }

-- |Compute vectorisation information that goes into 'ModGuts' (and is stored in interface files).
-- The incoming 'vectInfo' is that from the 'HscEnv' and 'EPS'.  The outgoing one contains only the
-- definitions for the currently compiled module; this includes variables, type constructors, and
-- data constructors referenced in VECTORISE pragmas.
--
modVectInfo :: GlobalEnv -> TypeEnv -> [CoreVect]-> VectInfo -> VectInfo
modVectInfo env tyenv vectDecls info
  = info 
    { vectInfoVar          = global_exported_vars env
    , vectInfoTyCon        = mk_env tyCons   (global_tycons   env)
    , vectInfoDataCon      = mk_env dataCons (global_datacons env)
    , vectInfoPADFun       = mk_env tyCons   (global_pa_funs  env)
    , vectInfoScalarVars   = global_scalar_vars   env `minusVarSet`  vectInfoScalarVars   info
    , vectInfoScalarTyCons = global_scalar_tycons env `minusNameSet` vectInfoScalarTyCons info
    }
  where
    vectTypeTyCons = [tycon | VectType tycon _ <- vectDecls]
    tyCons         = typeEnvTyCons   tyenv ++ vectTypeTyCons
    dataCons       = typeEnvDataCons tyenv ++ concatMap tyConDataCons vectTypeTyCons
    
    -- Produce an entry for every declaration that is mentioned in the domain of the 'inspectedEnv'
    mk_env decls inspectedEnv
      = mkNameEnv [(name, (decl, to))
                  | decl     <- decls
                  , let name = getName decl
                  , Just to  <- [lookupNameEnv inspectedEnv name]]
