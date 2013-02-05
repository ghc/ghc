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
        = LocalEnv
        { local_vars      :: VarEnv (Var, Var)
          -- ^Mapping from local variables to their vectorised and lifted versions.

        , local_tyvars    :: [TyVar]
          -- ^In-scope type variables.

        , local_tyvar_pa  :: VarEnv CoreExpr
          -- ^Mapping from tyvars to their PA dictionaries.

        , local_bind_name :: FastString
          -- ^Local binding name. This is only used to generate better names for hoisted
          -- expressions.
        }

-- |Create an empty local environment.
--
emptyLocalEnv :: LocalEnv
emptyLocalEnv = LocalEnv
                { local_vars      = emptyVarEnv
                , local_tyvars    = []
                , local_tyvar_pa  = emptyVarEnv
                , local_bind_name = fsLit "fn"
                }


-- GlobalEnv ------------------------------------------------------------------

-- |The global environment: entities that exist at top-level.
--
data GlobalEnv 
        = GlobalEnv
        { global_vect_avoid           :: Bool
          -- ^'True' implies to avoid vectorisation as far as possible.

        , global_vars                 :: VarEnv Var
          -- ^Mapping from global variables to their vectorised versions — aka the /vectorisation
          -- map/.

        , global_parallel_vars        :: VarSet
          -- ^The domain of 'global_vars'.
          --
          -- This information is not redundant as it is impossible to extract the domain from a
          -- 'VarEnv' (which is keyed on uniques alone). Moreover, we have mapped variables that
          -- do not involve parallelism — e.g., the workers of vectorised, but scalar data types.
          -- In addition, workers of parallel data types that we could not vectorise also need to
          -- be tracked.

        , global_vect_decls           :: VarEnv (Maybe (Type, CoreExpr))
          -- ^Mapping from global variables that have a vectorisation declaration to the right-hand
          -- side of that declaration and its type and mapping variables that have NOVECTORISE
          -- declarations to 'Nothing'.

        , global_tycons               :: NameEnv TyCon
          -- ^Mapping from TyCons to their vectorised versions. The vectorised version will be
          -- identical to the original version if it is not changed by vectorisation. In any case,
          -- if a tycon appears in the domain of this mapping, it was successfully vectorised.

        , global_parallel_tycons      :: NameSet
          -- ^Type constructors whose definition directly or indirectly includes a parallel type,
          -- such as '[::]'.
          --
          -- NB: This information is not redundant as some types have got a mapping in
          --     'global_tycons' (to a type other than themselves) and are still not parallel. An
          --     example is '(->)'. Moreover, some types have *not* got a mapping in 'global_tycons'
          --     (because they couldn't be vectorised), but still contain parallel types.
        
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
          -- ^Hoisted bindings — temporary storage for toplevel bindings during code gen.
        }

-- |Create an initial global environment.
--
-- We add scalar variables and type constructors identified by vectorisation pragmas already here
-- to the global table, so that we can query scalarness during vectorisation, and especially, when
-- vectorising the scalar entities' definitions themselves.
--
initGlobalEnv :: Bool -> VectInfo -> [CoreVect] -> (InstEnv, InstEnv) -> FamInstEnvs -> GlobalEnv
initGlobalEnv vectAvoid info vectDecls instEnvs famInstEnvs
  = GlobalEnv 
  { global_vect_avoid           = vectAvoid
  , global_vars                 = mapVarEnv snd $ vectInfoVar info
  , global_vect_decls           = mkVarEnv vects
  , global_parallel_vars        = vectInfoParallelVars info
  , global_parallel_tycons      = vectInfoParallelTyCons info
  , global_tycons               = mapNameEnv snd $ vectInfoTyCon info
  , global_datacons             = mapNameEnv snd $ vectInfoDataCon info
  , global_pa_funs              = emptyNameEnv
  , global_pr_funs              = emptyNameEnv
  , global_inst_env             = instEnvs
  , global_fam_inst_env         = famInstEnvs
  , global_bindings             = []
  }
  where
    vects         = [(var, Just (ty, exp)) | Vect   var   exp@(Var rhs_var) <- vectDecls
                                           , let ty = varType rhs_var] ++
                                        -- FIXME: we currently only allow RHSes consisting of a
                                        --   single variable to be able to obtain the type without
                                        --   inference — see also 'TcBinds.tcVect'
                    [(var, Nothing)        | NoVect var                     <- vectDecls]


-- Operators on Global Environments -------------------------------------------

-- |Extend the list of global variables in an environment.
--
extendImportedVarsEnv :: [(Var, Var)] -> GlobalEnv -> GlobalEnv
extendImportedVarsEnv ps genv
  = genv { global_vars = extendVarEnvList (global_vars genv) ps }

-- |Extend the list of type family instances.
--
extendFamEnv :: [FamInst] -> GlobalEnv -> GlobalEnv
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
    { vectInfoVar            = mk_env ids      (global_vars     env)
    , vectInfoTyCon          = mk_env tyCons   (global_tycons   env)
    , vectInfoDataCon        = mk_env dataCons (global_datacons env)
    , vectInfoParallelVars   = (global_parallel_vars   env `minusVarSet`  vectInfoParallelVars   info)
                               `intersectVarSet` (mkVarSet ids)
    , vectInfoParallelTyCons =  global_parallel_tycons env `minusNameSet` vectInfoParallelTyCons info
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
