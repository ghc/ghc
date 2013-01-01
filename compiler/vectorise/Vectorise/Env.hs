module Vectorise.Env (
  Scope(..),

  -- * Local Environments
  LocalEnv(..),
  emptyLocalEnv,

  -- * Global Environments
  GlobalEnv(..),
  initGlobalEnv,
  extendImportedVarsEnv,
  extendFamEnv,
  setPAFunsEnv,
  setPRFunsEnv,
  modVectInfo
) where

import HscTypes
import InstEnv
import FamInstEnv
import CoreSyn
import Type
import Class
import TyCon
import DataCon
import VarEnv
import VarSet
import Var
import NameSet
import Name
import NameEnv
import FastString
import TysPrim
import TysWiredIn


import Data.Maybe


-- |Indicates what scope something (a variable) is in.
--
data Scope a b 
        = Global a 
        | Local  b


-- LocalEnv -------------------------------------------------------------------

-- |The local environment.
--
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

-- |Create an empty local environment.
--
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
          -- ^Type constructors whose values can only contain scalar data.  This includes type
          -- constructors that appear in a 'VECTORISE SCALAR type' pragma or 'VECTORISE type' pragma
          -- *without* a right-hand side in the current or an imported module as well as type
          -- constructors that are automatically identified as scalar by the vectoriser (in
          -- 'Vectorise.Type.Env').  Scalar code may only operate on such data.
          --
          -- NB: Not all type constructors in that set are members of the 'Scalar' type class
          --     (which can be trivially marshalled across scalar code boundaries).
        
        , global_novect_vars          :: VarSet
          -- ^Variables that are not vectorised.  (They may be referenced in the right-hand sides
          -- of vectorisation declarations, though.)

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
-- We add scalar variables and type constructors identified by vectorisation pragmas already here
-- to the global table, so that we can query scalarness during vectorisation, and especially, when
-- vectorising the scalar entities' definitions themselves.
--
initGlobalEnv :: VectInfo -> [CoreVect] -> (InstEnv, InstEnv) -> FamInstEnvs -> GlobalEnv
initGlobalEnv info vectDecls instEnvs famInstEnvs
  = GlobalEnv 
  { global_vars                 = mapVarEnv snd $ vectInfoVar info
  , global_vect_decls           = mkVarEnv vects
  , global_scalar_vars          = vectInfoScalarVars info   `extendVarSetList` scalar_vars
  , global_scalar_tycons        = vectInfoScalarTyCons info `addListToNameSet` scalar_tycons
  , global_novect_vars          = mkVarSet novects
  , global_tycons               = mapNameEnv snd $ vectInfoTyCon info
  , global_datacons             = mapNameEnv snd $ vectInfoDataCon info
  , global_pa_funs              = emptyNameEnv
  , global_pr_funs              = emptyNameEnv
  , global_inst_env             = instEnvs
  , global_fam_inst_env         = famInstEnvs
  , global_bindings             = []
  }
  where
    vects         = [(var, (ty, exp)) | Vect     var   (Just exp@(Var rhs_var)) <- vectDecls
                                      , let ty = varType rhs_var]
                                        -- FIXME: we currently only allow RHSes consisting of a
                                        --   single variable to be able to obtain the type without
                                        --   inference — see also 'TcBinds.tcVect'
    scalar_vars   = [var              | Vect     var   Nothing                   <- vectDecls] ++
                    [var              | VectInst var                             <- vectDecls] ++ 
                    [dataConWrapId doubleDataCon, dataConWrapId floatDataCon, dataConWrapId intDataCon] -- TODO: fix this hack
    novects       = [var              | NoVect   var                             <- vectDecls]
    scalar_tycons = [tyConName tycon  | VectType True tycon Nothing              <- vectDecls] ++
                    [tyConName tycon  | VectType _    tycon (Just tycon')        <- vectDecls
                                      , tycon == tycon']  ++ 
                                      map tyConName [doublePrimTyCon, intPrimTyCon, floatPrimTyCon]  -- TODO: fix this hack
                      -- - for 'VectType True tycon Nothing', we checked that the type does not
                      --   contain arrays (or type variables that could be instatiated to arrays)
                      -- - for 'VectType _ tycon (Just tycon')', where the two tycons are the same,
                      --   we also know that there can be no embedded arrays


-- Operators on Global Environments -------------------------------------------

-- |Extend the list of global variables in an environment.
--
extendImportedVarsEnv :: [(Var, Var)] -> GlobalEnv -> GlobalEnv
extendImportedVarsEnv ps genv
  = genv { global_vars = extendVarEnvList (global_vars genv) ps }

-- |Extend the list of type family instances.
--
extendFamEnv :: [FamInst Unbranched] -> GlobalEnv -> GlobalEnv
extendFamEnv new genv
  = genv { global_fam_inst_env = (g_fam_inst, extendFamInstEnvList l_fam_inst new) }
  where (g_fam_inst, l_fam_inst) = global_fam_inst_env genv

-- |Set the list of PA functions in an environment.
--
setPAFunsEnv :: [(Name, Var)] -> GlobalEnv -> GlobalEnv
setPAFunsEnv ps genv = genv { global_pa_funs = mkNameEnv ps }

-- |Set the list of PR functions in an environment.
--
setPRFunsEnv :: [(Name, Var)] -> GlobalEnv -> GlobalEnv
setPRFunsEnv ps genv = genv { global_pr_funs = mkNameEnv ps }

-- |Compute vectorisation information that goes into 'ModGuts' (and is stored in interface files).
-- The incoming 'vectInfo' is that from the 'HscEnv' and 'EPS'.  The outgoing one contains only the
-- declarations for the currently compiled module; this includes variables, type constructors, and
-- data constructors referenced in VECTORISE pragmas, even if they are defined in an imported
-- module.
--
-- The variables explicitly include class selectors and dfuns.
--
modVectInfo :: GlobalEnv -> [Id] -> [TyCon] -> [CoreVect]-> VectInfo -> VectInfo
modVectInfo env mg_ids mg_tyCons vectDecls info
  = info 
    { vectInfoVar          = mk_env ids      (global_vars     env)
    , vectInfoTyCon        = mk_env tyCons   (global_tycons   env)
    , vectInfoDataCon      = mk_env dataCons (global_datacons env)
    , vectInfoScalarVars   = global_scalar_vars   env `minusVarSet`  vectInfoScalarVars   info
    , vectInfoScalarTyCons = global_scalar_tycons env `minusNameSet` vectInfoScalarTyCons info
    }
  where
    vectIds         = [id    | Vect     id    _   <- vectDecls] ++
                      [id    | VectInst id        <- vectDecls]
    vectTypeTyCons  = [tycon | VectType _ tycon _ <- vectDecls] ++
                      [tycon | VectClass tycon    <- vectDecls]
    vectDataCons    = concatMap tyConDataCons vectTypeTyCons
    ids             = mg_ids ++ vectIds ++ dataConIds ++ selIds
    tyCons          = mg_tyCons ++ vectTypeTyCons
    dataCons        = concatMap tyConDataCons mg_tyCons ++ vectDataCons
    dataConIds      = map dataConWorkId dataCons
    selIds          = concat [ classAllSelIds cls 
                             | tycon <- tyCons
                             , cls <- maybeToList . tyConClass_maybe $ tycon]
    
    -- Produce an entry for every declaration that is mentioned in the domain of the 'inspectedEnv'
    mk_env decls inspectedEnv
      = mkNameEnv [(name, (decl, to))
                  | decl     <- decls
                  , let name = getName decl
                  , Just to  <- [lookupNameEnv inspectedEnv name]]
