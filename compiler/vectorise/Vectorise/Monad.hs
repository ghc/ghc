
module Vectorise.Monad (
	module Vectorise.Monad.Base,
	module Vectorise.Monad.Naming,
	module Vectorise.Monad.Local,
	module Vectorise.Monad.Global,
	module Vectorise.Monad.InstEnv,
	initV,

	-- * Builtins
	liftBuiltinDs,
	builtin,
	builtins,
	
	-- * Variables
	lookupVar,
	maybeCantVectoriseVarM,
	dumpVar,
	addGlobalScalar, 
    deleteGlobalScalar,
    
	-- * Primitives
	lookupPrimPArray,
	lookupPrimMethod
) where

import Vectorise.Monad.Base
import Vectorise.Monad.Naming
import Vectorise.Monad.Local
import Vectorise.Monad.Global
import Vectorise.Monad.InstEnv
import Vectorise.Builtins
import Vectorise.Env

import HscTypes hiding ( MonadThings(..) )
import DynFlags
import MonadUtils (liftIO)
import TyCon
import Var
import VarEnv
import Id
import DsMonad
import Outputable
import FastString

import Control.Monad
import VarSet

-- | Run a vectorisation computation.
--
initV :: HscEnv
      -> ModGuts
      -> VectInfo
      -> VM a
      -> IO (Maybe (VectInfo, a))
initV hsc_env guts info thing_inside
  = do { (_, Just r) <- initDs hsc_env (mg_module guts) (mg_rdr_env guts) (mg_types guts) go
       ; return r
       }
  where
    go 
      = do {   -- pick a DPH backend
           ; dflags <- getDOptsDs
           ; case dphPackageMaybe dflags of
               Nothing  -> failWithDs $ ptext selectBackendErr
               Just pkg -> do {

               -- set up tables of builtin entities
           ; let compilingDPH = dphBackend dflags == DPHThis  -- FIXME: temporary kludge support
           ; builtins        <- initBuiltins pkg
           ; builtin_vars    <- initBuiltinVars compilingDPH builtins
           ; builtin_tycons  <- initBuiltinTyCons builtins
           ; let builtin_datacons = initBuiltinDataCons builtins
           ; builtin_boxed   <- initBuiltinBoxedTyCons builtins
           ; builtin_scalars <- initBuiltinScalars compilingDPH builtins

               -- set up class and type family envrionments
           ; eps <- liftIO $ hscEPS hsc_env
           ; let famInstEnvs = (eps_fam_inst_env eps, mg_fam_inst_env guts)
                 instEnvs    = (eps_inst_env     eps, mg_inst_env     guts)
           ; builtin_prs <- initBuiltinPRs builtins instEnvs
           ; builtin_pas <- initBuiltinPAs builtins instEnvs

               -- construct the initial global environment
           ; let genv = extendImportedVarsEnv builtin_vars
                        . extendScalars       builtin_scalars
                        . extendTyConsEnv     builtin_tycons
                        . extendDataConsEnv   builtin_datacons
                        . extendPAFunsEnv     builtin_pas
                        . setPRFunsEnv        builtin_prs
                        . setBoxedTyConsEnv   builtin_boxed
                        $ initGlobalEnv info (mg_vect_decls guts) instEnvs famInstEnvs
 
               -- perform vectorisation
           ; r <- runVM thing_inside builtins genv emptyLocalEnv
           ; case r of
               Yes genv _ x -> return $ Just (new_info genv, x)
               No           -> return Nothing
           } }

    new_info genv = updVectInfo genv (mg_types guts) info

    selectBackendErr = sLit "To use -fvectorise select a DPH backend with -fdph-par or -fdph-seq"

-- Builtins -------------------------------------------------------------------
-- | Lift a desugaring computation using the `Builtins` into the vectorisation monad.
liftBuiltinDs :: (Builtins -> DsM a) -> VM a
liftBuiltinDs p = VM $ \bi genv lenv -> do { x <- p bi; return (Yes genv lenv x)}


-- | Project something from the set of builtins.
builtin :: (Builtins -> a) -> VM a
builtin f = VM $ \bi genv lenv -> return (Yes genv lenv (f bi))


-- | Lift a function using the `Builtins` into the vectorisation monad.
builtins :: (a -> Builtins -> b) -> VM (a -> b)
builtins f = VM $ \bi genv lenv -> return (Yes genv lenv (`f` bi))


-- Var ------------------------------------------------------------------------
-- | Lookup the vectorised and\/or lifted versions of this variable.
--	If it's in the global environment we get the vectorised version.
--      If it's in the local environment we get both the vectorised and lifted version.
lookupVar :: Var -> VM (Scope Var (Var, Var))
lookupVar v
 = do r <- readLEnv $ \env -> lookupVarEnv (local_vars env) v
      case r of
        Just e  -> return (Local e)
        Nothing -> liftM Global
                . maybeCantVectoriseVarM v
                . readGEnv $ \env -> lookupVarEnv (global_vars env) v

maybeCantVectoriseVarM :: Monad m => Var -> m (Maybe Var) -> m Var
maybeCantVectoriseVarM v p
 = do r <- p
      case r of
        Just x  -> return x
        Nothing -> dumpVar v

dumpVar :: Var -> a
dumpVar var
	| Just _		<- isClassOpId_maybe var
	= cantVectorise "ClassOpId not vectorised:" (ppr var)

	| otherwise
	= cantVectorise "Variable not vectorised:" (ppr var)


-- local scalars --------------------------------------------------------------

addGlobalScalar :: Var -> VM ()
addGlobalScalar var 
  = do { traceVt "addGlobalScalar" (ppr var)
       ; updGEnv $ \env -> env{global_scalars = extendVarSet (global_scalars env) var}
     }
     
deleteGlobalScalar :: Var -> VM ()
deleteGlobalScalar var 
  = do { traceVt "deleteGlobalScalar" (ppr var)
       ; updGEnv $ \env -> env{global_scalars = delVarSet (global_scalars env) var}
     }
     
     
-- Primitives -----------------------------------------------------------------
lookupPrimPArray :: TyCon -> VM (Maybe TyCon)
lookupPrimPArray = liftBuiltinDs . primPArray

lookupPrimMethod :: TyCon -> String -> VM (Maybe Var)
lookupPrimMethod tycon = liftBuiltinDs . primMethod tycon

