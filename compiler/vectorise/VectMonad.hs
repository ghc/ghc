module VectMonad (
  VM,

  noV, tryV, maybeV, orElseV, localV, closedV, initV,
  newLocalVar, newTyVar,
  
  Builtins(..), paDictTyCon,
  builtin,

  GlobalEnv(..),
  readGEnv, setGEnv, updGEnv,

  LocalEnv(..),
  readLEnv, setLEnv, updLEnv,

  lookupTyCon,
  lookupTyVarPA, extendTyVarPA, deleteTyVarPA,

  lookupInst, lookupFamInst
) where

#include "HsVersions.h"

import HscTypes
import CoreSyn
import Class
import TyCon
import Type
import Var
import VarEnv
import Id
import Name
import NameEnv

import DsMonad
import PrelNames

import InstEnv
import FamInstEnv

import Panic
import Outputable
import FastString

-- ----------------------------------------------------------------------------
-- Vectorisation monad

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paClass          :: Class
                , closureTyCon     :: TyCon
                , mkClosureVar     :: Var
                , applyClosureVar  :: Var
                , mkClosurePVar    :: Var
                , applyClosurePVar :: Var
                , lengthPAVar      :: Var
                , replicatePAVar   :: Var
                }

paDictTyCon :: Builtins -> TyCon
paDictTyCon = classTyCon . paClass

initBuiltins :: DsM Builtins
initBuiltins
  = do
      parrayTyCon  <- dsLookupTyCon parrayTyConName
      paClass      <- dsLookupClass paClassName
      closureTyCon <- dsLookupTyCon closureTyConName

      mkClosureVar     <- dsLookupGlobalId mkClosureName
      applyClosureVar  <- dsLookupGlobalId applyClosureName
      mkClosurePVar    <- dsLookupGlobalId mkClosurePName
      applyClosurePVar <- dsLookupGlobalId applyClosurePName
      lengthPAVar      <- dsLookupGlobalId lengthPAName
      replicatePAVar   <- dsLookupGlobalId replicatePAName

      return $ Builtins {
                 parrayTyCon      = parrayTyCon
               , paClass          = paClass
               , closureTyCon     = closureTyCon
               , mkClosureVar     = mkClosureVar
               , applyClosureVar  = applyClosureVar
               , mkClosurePVar    = mkClosurePVar
               , applyClosurePVar = applyClosurePVar
               , lengthPAVar      = lengthPAVar
               , replicatePAVar   = replicatePAVar
               }

data GlobalEnv = GlobalEnv {
                  -- Mapping from global variables to their vectorised versions.
                  -- 
                  global_vars :: VarEnv CoreExpr

                  -- Exported variables which have a vectorised version
                  --
                , global_exported_vars :: VarEnv (Var, Var)

                  -- Mapping from TyCons to their vectorised versions.
                  -- TyCons which do not have to be vectorised are mapped to
                  -- themselves.
                  --
                , global_tycons :: NameEnv TyCon

                  -- Mapping from TyCons to their PA dictionaries
                  --
                , global_tycon_pa :: NameEnv CoreExpr

                -- External package inst-env & home-package inst-env for class
                -- instances
                --
                , global_inst_env :: (InstEnv, InstEnv)

                -- External package inst-env & home-package inst-env for family
                -- instances
                --
                , global_fam_inst_env :: FamInstEnvs
                }

data LocalEnv = LocalEnv {
                 -- Mapping from local variables to their vectorised and
                 -- lifted versions
                 --
                 local_vars :: VarEnv (CoreExpr, CoreExpr)

                 -- Mapping from tyvars to their PA dictionaries
               , local_tyvar_pa :: VarEnv CoreExpr

                 -- Hoisted bindings
               , local_bindings :: [(Var, CoreExpr)]
               }
              

initGlobalEnv :: VectInfo -> (InstEnv, InstEnv) -> FamInstEnvs -> GlobalEnv
initGlobalEnv info instEnvs famInstEnvs
  = GlobalEnv {
      global_vars          = mapVarEnv  (Var . snd) $ vectInfoCCVar   info
    , global_exported_vars = emptyVarEnv
    , global_tycons        = mapNameEnv snd $ vectInfoCCTyCon info
    , global_tycon_pa      = emptyNameEnv
    , global_inst_env      = instEnvs
    , global_fam_inst_env  = famInstEnvs
    }

emptyLocalEnv = LocalEnv {
                   local_vars     = emptyVarEnv
                 , local_tyvar_pa = emptyVarEnv
                 , local_bindings = []
                 }

-- FIXME
updVectInfo :: GlobalEnv -> TypeEnv -> VectInfo -> VectInfo
updVectInfo env tyenv info
  = info {
      vectInfoCCVar   = global_exported_vars env
    , vectInfoCCTyCon = tc_env
    }
  where
    tc_env = mkNameEnv [(tc_name, (tc,tc'))
               | tc <- typeEnvTyCons tyenv
               , let tc_name = tyConName tc
               , Just tc' <- [lookupNameEnv (global_tycons env) tc_name]]

data VResult a = Yes GlobalEnv LocalEnv a | No

newtype VM a = VM { runVM :: Builtins -> GlobalEnv -> LocalEnv -> DsM (VResult a) }

instance Monad VM where
  return x   = VM $ \bi genv lenv -> return (Yes genv lenv x)
  VM p >>= f = VM $ \bi genv lenv -> do
                                      r <- p bi genv lenv
                                      case r of
                                        Yes genv' lenv' x -> runVM (f x) bi genv' lenv'
                                        No                -> return No

noV :: VM a
noV = VM $ \_ _ _ -> return No

tryV :: VM a -> VM (Maybe a)
tryV (VM p) = VM $ \bi genv lenv ->
  do
    r <- p bi genv lenv
    case r of
      Yes genv' lenv' x -> return (Yes genv' lenv' (Just x))
      No                -> return (Yes genv  lenv  Nothing)

maybeV :: VM (Maybe a) -> VM a
maybeV p = maybe noV return =<< p

orElseV :: VM a -> VM a -> VM a
orElseV p q = maybe q return =<< tryV p

localV :: VM a -> VM a
localV p = do
             env <- readLEnv id
             x <- p
             setLEnv env
             return x

closedV :: VM a -> VM a
closedV p = do
              env <- readLEnv id
              setLEnv emptyLocalEnv
              x <- p
              setLEnv env
              return x

liftDs :: DsM a -> VM a
liftDs p = VM $ \bi genv lenv -> do { x <- p; return (Yes genv lenv x) }

builtin :: (Builtins -> a) -> VM a
builtin f = VM $ \bi genv lenv -> return (Yes genv lenv (f bi))

readGEnv :: (GlobalEnv -> a) -> VM a
readGEnv f = VM $ \bi genv lenv -> return (Yes genv lenv (f genv))

setGEnv :: GlobalEnv -> VM ()
setGEnv genv = VM $ \_ _ lenv -> return (Yes genv lenv ())

updGEnv :: (GlobalEnv -> GlobalEnv) -> VM ()
updGEnv f = VM $ \_ genv lenv -> return (Yes (f genv) lenv ())

readLEnv :: (LocalEnv -> a) -> VM a
readLEnv f = VM $ \bi genv lenv -> return (Yes genv lenv (f lenv))

setLEnv :: LocalEnv -> VM ()
setLEnv lenv = VM $ \_ genv _ -> return (Yes genv lenv ())

updLEnv :: (LocalEnv -> LocalEnv) -> VM ()
updLEnv f = VM $ \_ genv lenv -> return (Yes genv (f lenv) ())

getInstEnv :: VM (InstEnv, InstEnv)
getInstEnv = readGEnv global_inst_env

getFamInstEnv :: VM FamInstEnvs
getFamInstEnv = readGEnv global_fam_inst_env

newLocalVar :: FastString -> Type -> VM Var
newLocalVar fs ty
  = do
      u <- liftDs newUnique
      return $ mkSysLocal fs u ty

newTyVar :: FastString -> Kind -> VM Var
newTyVar fs k
  = do
      u <- liftDs newUnique
      return $ mkTyVar (mkSysTvName u fs) k

lookupTyCon :: TyCon -> VM (Maybe TyCon)
lookupTyCon tc = readGEnv $ \env -> lookupNameEnv (global_tycons env) (tyConName tc)

lookupTyVarPA :: Var -> VM (Maybe CoreExpr)
lookupTyVarPA tv = readLEnv $ \env -> lookupVarEnv (local_tyvar_pa env) tv 

extendTyVarPA :: Var -> CoreExpr -> VM ()
extendTyVarPA tv pa = updLEnv $ \env -> env { local_tyvar_pa = extendVarEnv (local_tyvar_pa env) tv pa }

deleteTyVarPA :: Var -> VM ()
deleteTyVarPA tv = updLEnv $ \env -> env { local_tyvar_pa = delVarEnv (local_tyvar_pa env) tv }

-- Look up the dfun of a class instance.
--
-- The match must be unique - ie, match exactly one instance - but the 
-- type arguments used for matching may be more specific than those of 
-- the class instance declaration.  The found class instances must not have
-- any type variables in the instance context that do not appear in the
-- instances head (i.e., no flexi vars); for details for what this means,
-- see the docs at InstEnv.lookupInstEnv.
--
lookupInst :: Class -> [Type] -> VM (DFunId, [Type])
lookupInst cls tys
  = do { instEnv <- getInstEnv
       ; case lookupInstEnv instEnv cls tys of
	   ([(inst, inst_tys)], _) 
             | noFlexiVar -> return (instanceDFunId inst, inst_tys')
             | otherwise  -> pprPanic "VectMonad.lookupInst: flexi var: " 
                                      (ppr $ mkTyConApp (classTyCon cls) tys)
             where
               inst_tys'  = [ty | Right ty <- inst_tys]
               noFlexiVar = all isRight inst_tys
	   _other         -> noV
       }
  where
    isRight (Left  _) = False
    isRight (Right _) = True

-- Look up the representation tycon of a family instance.
--
-- The match must be unique - ie, match exactly one instance - but the 
-- type arguments used for matching may be more specific than those of 
-- the family instance declaration.
--
-- Return the instance tycon and its type instance.  For example, if we have
--
--  lookupFamInst 'T' '[Int]' yields (':R42T', 'Int')
--
-- then we have a coercion (ie, type instance of family instance coercion)
--
--  :Co:R42T Int :: T [Int] ~ :R42T Int
--
-- which implies that :R42T was declared as 'data instance T [a]'.
--
lookupFamInst :: TyCon -> [Type] -> VM (TyCon, [Type])
lookupFamInst tycon tys
  = ASSERT( isOpenTyCon tycon )
    do { instEnv <- getFamInstEnv
       ; case lookupFamInstEnv instEnv tycon tys of
	   [(fam_inst, rep_tys)] -> return (famInstTyCon fam_inst, rep_tys)
	   _other                -> 
             pprPanic "VectMonad.lookupFamInst: not found: " 
                      (ppr $ mkTyConApp tycon tys)
       }

initV :: HscEnv -> ModGuts -> VectInfo -> VM a -> IO (Maybe (VectInfo, a))
initV hsc_env guts info p
  = do
      eps <- hscEPS hsc_env
      let famInstEnvs = (eps_fam_inst_env eps, mg_fam_inst_env guts)
      let instEnvs    = (eps_inst_env     eps, mg_inst_env     guts)

      Just r <- initDs hsc_env (mg_module guts)
                               (mg_rdr_env guts)
                               (mg_types guts)
                               (go instEnvs famInstEnvs)
      return r
  where

    go instEnvs famInstEnvs = 
      do
        builtins <- initBuiltins
        r <- runVM p builtins (initGlobalEnv info instEnvs famInstEnvs) 
                   emptyLocalEnv
        case r of
          Yes genv _ x -> return $ Just (new_info genv, x)
          No           -> return Nothing

    new_info genv = updVectInfo genv (mg_types guts) info

