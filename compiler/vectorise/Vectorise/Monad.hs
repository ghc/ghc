
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

	-- * Primitives
	lookupPrimPArray,
	lookupPrimMethod
)
where
import Vectorise.Monad.Base
import Vectorise.Monad.Naming
import Vectorise.Monad.Local
import Vectorise.Monad.Global
import Vectorise.Monad.InstEnv
import Vectorise.Builtins
import Vectorise.Env

import HscTypes hiding  ( MonadThings(..) )
import MonadUtils (liftIO)
import Module
import TyCon
import Var
import VarEnv
import Id
import DsMonad
import Outputable
import Control.Monad


-- | Run a vectorisation computation.
initV	:: PackageId
	-> HscEnv
	-> ModGuts
	-> VectInfo
	-> VM a
	-> IO (Maybe (VectInfo, a))

initV pkg hsc_env guts info p
  = do
         -- XXX: ignores error messages and warnings, check that this is
         -- indeed ok (the use of "Just r" suggests so)
      (_,Just r) <- initDs hsc_env (mg_module guts)
                               (mg_rdr_env guts)
                               (mg_types guts)
                               go
      return r
  where
    go 
     = do
        builtins	<- initBuiltins pkg
        builtin_vars	<- initBuiltinVars builtins
        builtin_tycons	<- initBuiltinTyCons builtins
        let builtin_datacons = initBuiltinDataCons builtins
        builtin_boxed	<- initBuiltinBoxedTyCons builtins
        builtin_scalars	<- initBuiltinScalars builtins

        eps <- liftIO $ hscEPS hsc_env
        let famInstEnvs = (eps_fam_inst_env eps, mg_fam_inst_env guts)
            instEnvs    = (eps_inst_env     eps, mg_inst_env     guts)

        builtin_prs	<- initBuiltinPRs builtins instEnvs
        builtin_pas	<- initBuiltinPAs builtins instEnvs

        let genv = extendImportedVarsEnv builtin_vars
                 . extendScalars builtin_scalars
                 . extendTyConsEnv builtin_tycons
                 . extendDataConsEnv builtin_datacons
                 . extendPAFunsEnv builtin_pas
                 . setPRFunsEnv    builtin_prs
                 . setBoxedTyConsEnv builtin_boxed
                 $ initGlobalEnv info instEnvs famInstEnvs

        r <- runVM p builtins genv emptyLocalEnv
        case r of
          Yes genv _ x -> return $ Just (new_info genv, x)
          No           -> return Nothing

    new_info genv = updVectInfo genv (mg_types guts) info


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


-- Primitives -----------------------------------------------------------------
lookupPrimPArray :: TyCon -> VM (Maybe TyCon)
lookupPrimPArray = liftBuiltinDs . primPArray

lookupPrimMethod :: TyCon -> String -> VM (Maybe Var)
lookupPrimMethod tycon = liftBuiltinDs . primMethod tycon

