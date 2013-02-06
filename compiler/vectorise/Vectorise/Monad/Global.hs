-- Operations on the global state of the vectorisation monad.

module Vectorise.Monad.Global (
  readGEnv,
  setGEnv,
  updGEnv,
  
  -- * Configuration
  isVectAvoidanceAggressive,
  
  -- * Vars
  defGlobalVar, undefGlobalVar,
  
  -- * Vectorisation declarations
  lookupVectDecl, 
  
  -- * Scalars
  globalParallelVars, globalParallelTyCons,
  
  -- * TyCons
  lookupTyCon,
  defTyConName, defTyCon, globalVectTyCons,
  
  -- * Datacons
  lookupDataCon,
  defDataCon,
  
  -- * PA Dictionaries
  lookupTyConPA,
  defTyConPAs,
  
  -- * PR Dictionaries
  lookupTyConPR
) where

import Vectorise.Monad.Base
import Vectorise.Env

import CoreSyn
import Type
import TyCon
import DataCon
import DynFlags
import NameEnv
import NameSet
import Name
import VarEnv
import VarSet
import Var as Var
import FastString
import Outputable


-- Global Environment ---------------------------------------------------------

-- |Project something from the global environment.
--
readGEnv :: (GlobalEnv -> a) -> VM a
readGEnv f  = VM $ \_ genv lenv -> return (Yes genv lenv (f genv))

-- |Set the value of the global environment.
--
setGEnv :: GlobalEnv -> VM ()
setGEnv genv  = VM $ \_ _ lenv -> return (Yes genv lenv ())

-- |Update the global environment using the provided function.
--
updGEnv :: (GlobalEnv -> GlobalEnv) -> VM ()
updGEnv f = VM $ \_ genv lenv -> return (Yes (f genv) lenv ())


-- Configuration --------------------------------------------------------------

-- |Should we avoid as much vectorisation as possible?
--
-- Set by '-f[no]-vectorisation-avoidance'
--
isVectAvoidanceAggressive :: VM Bool
isVectAvoidanceAggressive = readGEnv global_vect_avoid


-- Vars -----------------------------------------------------------------------

-- |Add a mapping between a global var and its vectorised version to the state.
--
defGlobalVar :: Var -> Var -> VM ()
defGlobalVar v v'
  = do { traceVt "add global var mapping:" (ppr v <+> text "-->" <+> ppr v') 

           -- check for duplicate vectorisation
       ; currentDef <- readGEnv $ \env -> lookupVarEnv (global_vars env) v
       ; case currentDef of
           Just old_v' ->
               do dflags <- getDynFlags
                  cantVectorise dflags "Variable is already vectorised:" $
                            ppr v <+> moduleOf v old_v'
           Nothing     -> return ()

       ; updGEnv  $ \env -> env { global_vars = extendVarEnv (global_vars env) v v' }
       }
  where
    moduleOf var var' | var == var'
                      = ptext (sLit "vectorises to itself")
                      | Just mod <- nameModule_maybe (Var.varName var') 
                      = ptext (sLit "in module") <+> ppr mod
                      | otherwise
                      = ptext (sLit "in the current module")

-- |Remove the mapping of a variable in the vectorisation map.
--
undefGlobalVar :: Var -> VM ()
undefGlobalVar v
  = do 
    { traceVt "REMOVING global var mapping:" (ppr v)
    ; updGEnv  $ \env -> env { global_vars = delVarEnv (global_vars env) v }
    }


-- Vectorisation declarations -------------------------------------------------

-- |Check whether a variable has a vectorisation declaration.
--
-- The first component of the result indicates whether the variable has a 'NOVECTORISE' declaration.
-- The second component contains the given type and expression in case of a 'VECTORISE' declaration.
--
lookupVectDecl :: Var -> VM (Bool, Maybe (Type, CoreExpr))
lookupVectDecl var 
  = readGEnv $ \env -> 
      case lookupVarEnv (global_vect_decls env) var of
        Nothing -> (False, Nothing)
        Just Nothing  -> (True, Nothing)
        Just vectDecl -> (False, vectDecl)


-- Parallel entities -----------------------------------------------------------

-- |Get the set of global parallel variables.
--
globalParallelVars :: VM VarSet
globalParallelVars = readGEnv global_parallel_vars

-- |Get the set of all parallel type constructors (those that may embed parallelism) including both
-- both those parallel type constructors declared in an imported module and those declared in the
-- current module.
--
globalParallelTyCons :: VM NameSet
globalParallelTyCons = readGEnv global_parallel_tycons


-- TyCons ---------------------------------------------------------------------

-- |Determine the vectorised version of a `TyCon`. The vectorisation map in the global environment
-- contains a vectorised version if the original `TyCon` embeds any parallel arrays.
--
lookupTyCon :: TyCon -> VM (Maybe TyCon)
lookupTyCon tc
  = readGEnv $ \env -> lookupNameEnv (global_tycons env) (tyConName tc)

-- |Add a mapping between plain and vectorised `TyCon`s to the global environment.
--
-- The second argument is only to enable tracing for (mutually) recursively defined type
-- constructors, where we /must not/ pull at the vectorised type constructors (because that would
-- pull too early at the recursive knot).
--
defTyConName :: TyCon -> Name -> TyCon -> VM ()
defTyConName tc nameOfTc' tc'
  = do { traceVt "add global tycon mapping:" (ppr tc <+> text "-->" <+> ppr nameOfTc') 

           -- check for duplicate vectorisation
       ; currentDef <- readGEnv $ \env -> lookupNameEnv (global_tycons env) (tyConName tc)
       ; case currentDef of
           Just old_tc' ->
               do dflags <- getDynFlags
                  cantVectorise dflags "Type constructor or class is already vectorised:" $
                            ppr tc <+> moduleOf tc old_tc'
           Nothing     -> return ()

       ; updGEnv $ \env -> 
           env { global_tycons = extendNameEnv (global_tycons env) (tyConName tc) tc' }
       }
  where
    moduleOf tc tc' | tc == tc'
                    = ptext (sLit "vectorises to itself")
                    | Just mod <- nameModule_maybe (tyConName tc') 
                    = ptext (sLit "in module") <+> ppr mod
                    | otherwise
                    = ptext (sLit "in the current module")

-- |Add a mapping between plain and vectorised `TyCon`s to the global environment.
--
defTyCon :: TyCon -> TyCon -> VM ()
defTyCon tc tc' = defTyConName tc (tyConName tc') tc'

-- |Get the set of all vectorised type constructors.
--
globalVectTyCons :: VM (NameEnv TyCon)
globalVectTyCons = readGEnv global_tycons


-- DataCons -------------------------------------------------------------------

-- |Lookup the vectorised version of a `DataCon` from the global environment.
--
lookupDataCon :: DataCon -> VM (Maybe DataCon)
lookupDataCon dc
  | isTupleTyCon (dataConTyCon dc) 
  = return (Just dc)
  | otherwise 
  = readGEnv $ \env -> lookupNameEnv (global_datacons env) (dataConName dc)

-- |Add the mapping between plain and vectorised `DataCon`s to the global environment.
--
defDataCon :: DataCon -> DataCon -> VM ()
defDataCon dc dc' = updGEnv $ \env ->
  env { global_datacons = extendNameEnv (global_datacons env) (dataConName dc) dc' }


-- 'PA' dictionaries ------------------------------------------------------------

-- |Lookup the 'PA' dfun of a vectorised type constructor in the global environment.
--
lookupTyConPA :: TyCon -> VM (Maybe Var)
lookupTyConPA tc
  = readGEnv $ \env -> lookupNameEnv (global_pa_funs env) (tyConName tc)

-- |Associate vectorised type constructors with the dfun of their 'PA' instances in the global
-- environment.
--
defTyConPAs :: [(TyCon, Var)] -> VM ()
defTyConPAs ps = updGEnv $ \env ->
  env { global_pa_funs = extendNameEnvList (global_pa_funs env)
                                           [(tyConName tc, pa) | (tc, pa) <- ps] }


-- PR Dictionaries ------------------------------------------------------------

lookupTyConPR :: TyCon -> VM (Maybe Var)
lookupTyConPR tc = readGEnv $ \env -> lookupNameEnv (global_pr_funs env) (tyConName tc)
