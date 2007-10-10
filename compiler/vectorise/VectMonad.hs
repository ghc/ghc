{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module VectMonad (
  Scope(..),
  VM,

  noV, tryV, maybeV, traceMaybeV, orElseV, fixV, localV, closedV, initV,
  liftDs,
  cloneName, cloneId,
  newExportedVar, newLocalVar, newDummyVar, newTyVar,
  
  Builtins(..), sumTyCon, prodTyCon,
  builtin, builtins,

  GlobalEnv(..),
  setFamInstEnv,
  readGEnv, setGEnv, updGEnv,

  LocalEnv(..),
  readLEnv, setLEnv, updLEnv,

  getBindName, inBind,

  lookupVar, defGlobalVar,
  lookupTyCon, defTyCon,
  lookupDataCon, defDataCon,
  lookupTyConPA, defTyConPA, defTyConPAs,
  lookupTyConPR,
  lookupPrimMethod, lookupPrimPArray,
  lookupTyVarPA, defLocalTyVar, defLocalTyVarWithPA, localTyVars,

  {-lookupInst,-} lookupFamInst
) where

#include "HsVersions.h"

import VectBuiltIn

import HscTypes
import CoreSyn
import TyCon
import DataCon
import Type
import Class
import Var
import VarEnv
import Id
import OccName
import Name
import NameEnv
import TysPrim       ( intPrimTy )
import Module
import IfaceEnv
import IOEnv         ( ioToIOEnv )

import DsMonad
import PrelNames

import InstEnv
import FamInstEnv

import Panic
import Outputable
import FastString
import SrcLoc        ( noSrcSpan )

import Control.Monad ( liftM, zipWithM )

data Scope a b = Global a | Local b

-- ----------------------------------------------------------------------------
-- Vectorisation monad

data GlobalEnv = GlobalEnv {
                  -- Mapping from global variables to their vectorised versions.
                  -- 
                  global_vars :: VarEnv Var

                  -- Exported variables which have a vectorised version
                  --
                , global_exported_vars :: VarEnv (Var, Var)

                  -- Mapping from TyCons to their vectorised versions.
                  -- TyCons which do not have to be vectorised are mapped to
                  -- themselves.
                  --
                , global_tycons :: NameEnv TyCon

                  -- Mapping from DataCons to their vectorised versions
                  --
                , global_datacons :: NameEnv DataCon

                  -- Mapping from TyCons to their PA dfuns
                  --
                , global_pa_funs :: NameEnv Var

                  -- Mapping from TyCons to their PR dfuns
                , global_pr_funs :: NameEnv Var

                -- External package inst-env & home-package inst-env for class
                -- instances
                --
                , global_inst_env :: (InstEnv, InstEnv)

                -- External package inst-env & home-package inst-env for family
                -- instances
                --
                , global_fam_inst_env :: FamInstEnvs

                -- Hoisted bindings
                , global_bindings :: [(Var, CoreExpr)]
                }

data LocalEnv = LocalEnv {
                 -- Mapping from local variables to their vectorised and
                 -- lifted versions
                 --
                 local_vars :: VarEnv (Var, Var)

                 -- In-scope type variables
                 --
               , local_tyvars :: [TyVar]

                 -- Mapping from tyvars to their PA dictionaries
               , local_tyvar_pa :: VarEnv CoreExpr

                 -- Local binding name
               , local_bind_name :: FastString
               }

initGlobalEnv :: VectInfo -> (InstEnv, InstEnv) -> FamInstEnvs -> GlobalEnv
initGlobalEnv info instEnvs famInstEnvs
  = GlobalEnv {
      global_vars          = mapVarEnv snd $ vectInfoVar info
    , global_exported_vars = emptyVarEnv
    , global_tycons        = mapNameEnv snd $ vectInfoTyCon info
    , global_datacons      = mapNameEnv snd $ vectInfoDataCon info
    , global_pa_funs       = mapNameEnv snd $ vectInfoPADFun info
    , global_pr_funs       = emptyNameEnv
    , global_inst_env      = instEnvs
    , global_fam_inst_env  = famInstEnvs
    , global_bindings      = []
    }

setFamInstEnv :: FamInstEnv -> GlobalEnv -> GlobalEnv
setFamInstEnv l_fam_inst genv
  = genv { global_fam_inst_env = (g_fam_inst, l_fam_inst) }
  where
    (g_fam_inst, _) = global_fam_inst_env genv

extendTyConsEnv :: [(Name, TyCon)] -> GlobalEnv -> GlobalEnv
extendTyConsEnv ps genv
  = genv { global_tycons = extendNameEnvList (global_tycons genv) ps }

extendPAFunsEnv :: [(Name, Var)] -> GlobalEnv -> GlobalEnv
extendPAFunsEnv ps genv
  = genv { global_pa_funs = extendNameEnvList (global_pa_funs genv) ps }

setPRFunsEnv :: [(Name, Var)] -> GlobalEnv -> GlobalEnv
setPRFunsEnv ps genv
  = genv { global_pr_funs = mkNameEnv ps }

emptyLocalEnv = LocalEnv {
                   local_vars     = emptyVarEnv
                 , local_tyvars   = []
                 , local_tyvar_pa = emptyVarEnv
                 , local_bind_name  = FSLIT("fn")
                 }

-- FIXME
updVectInfo :: GlobalEnv -> TypeEnv -> VectInfo -> VectInfo
updVectInfo env tyenv info
  = info {
      vectInfoVar     = global_exported_vars env
    , vectInfoTyCon   = mk_env typeEnvTyCons global_tycons
    , vectInfoDataCon = mk_env typeEnvDataCons global_datacons
    , vectInfoPADFun  = mk_env typeEnvTyCons global_pa_funs
    }
  where
    mk_env from_tyenv from_env = mkNameEnv [(name, (from,to))
                                   | from <- from_tyenv tyenv
                                   , let name = getName from
                                   , Just to <- [lookupNameEnv (from_env env) name]]

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

traceNoV :: String -> SDoc -> VM a
traceNoV s d = pprTrace s d noV

tryV :: VM a -> VM (Maybe a)
tryV (VM p) = VM $ \bi genv lenv ->
  do
    r <- p bi genv lenv
    case r of
      Yes genv' lenv' x -> return (Yes genv' lenv' (Just x))
      No                -> return (Yes genv  lenv  Nothing)

maybeV :: VM (Maybe a) -> VM a
maybeV p = maybe noV return =<< p

traceMaybeV :: String -> SDoc -> VM (Maybe a) -> VM a
traceMaybeV s d p = maybe (traceNoV s d) return =<< p

orElseV :: VM a -> VM a -> VM a
orElseV p q = maybe q return =<< tryV p

fixV :: (a -> VM a) -> VM a
fixV f = VM (\bi genv lenv -> fixDs $ \r -> runVM (f (unYes r)) bi genv lenv )
  where
    unYes (Yes _ _ x) = x

localV :: VM a -> VM a
localV p = do
             env <- readLEnv id
             x <- p
             setLEnv env
             return x

closedV :: VM a -> VM a
closedV p = do
              env <- readLEnv id
              setLEnv (emptyLocalEnv { local_bind_name = local_bind_name env })
              x <- p
              setLEnv env
              return x

liftDs :: DsM a -> VM a
liftDs p = VM $ \bi genv lenv -> do { x <- p; return (Yes genv lenv x) }

builtin :: (Builtins -> a) -> VM a
builtin f = VM $ \bi genv lenv -> return (Yes genv lenv (f bi))

builtins :: (a -> Builtins -> b) -> VM (a -> b)
builtins f = VM $ \bi genv lenv -> return (Yes genv lenv (`f` bi))

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

getBindName :: VM FastString
getBindName = readLEnv local_bind_name

inBind :: Id -> VM a -> VM a
inBind id p
  = do updLEnv $ \env -> env { local_bind_name = occNameFS (getOccName id) }
       p

cloneName :: (OccName -> OccName) -> Name -> VM Name
cloneName mk_occ name = liftM make (liftDs newUnique)
  where
    occ_name = mk_occ (nameOccName name)

    make u | isExternalName name = mkExternalName u (nameModule name)
                                                    occ_name
                                                    (nameSrcSpan name)
           | otherwise           = mkSystemName u occ_name

cloneId :: (OccName -> OccName) -> Id -> Type -> VM Id
cloneId mk_occ id ty
  = do
      name <- cloneName mk_occ (getName id)
      let id' | isExportedId id = Id.mkExportedLocalId name ty
              | otherwise       = Id.mkLocalId         name ty
      return id'

newExportedVar :: OccName -> Type -> VM Var
newExportedVar occ_name ty 
  = do
      mod <- liftDs getModuleDs
      u   <- liftDs newUnique

      let name = mkExternalName u mod occ_name noSrcSpan
      
      return $ Id.mkExportedLocalId name ty

newLocalVar :: FastString -> Type -> VM Var
newLocalVar fs ty
  = do
      u <- liftDs newUnique
      return $ mkSysLocal fs u ty

newDummyVar :: Type -> VM Var
newDummyVar = newLocalVar FSLIT("ds")

newTyVar :: FastString -> Kind -> VM Var
newTyVar fs k
  = do
      u <- liftDs newUnique
      return $ mkTyVar (mkSysTvName u fs) k

defGlobalVar :: Var -> Var -> VM ()
defGlobalVar v v' = updGEnv $ \env ->
  env { global_vars = extendVarEnv (global_vars env) v v'
      , global_exported_vars = upd (global_exported_vars env)
      }
  where
    upd env | isExportedId v = extendVarEnv env v (v, v')
            | otherwise      = env

lookupVar :: Var -> VM (Scope Var (Var, Var))
lookupVar v
  = do
      r <- readLEnv $ \env -> lookupVarEnv (local_vars env) v
      case r of
        Just e  -> return (Local e)
        Nothing -> liftM Global
                 $  traceMaybeV "lookupVar" (ppr v)
                                (readGEnv $ \env -> lookupVarEnv (global_vars env) v)

lookupTyCon :: TyCon -> VM (Maybe TyCon)
lookupTyCon tc
  | isUnLiftedTyCon tc || isTupleTyCon tc = return (Just tc)

  | otherwise = readGEnv $ \env -> lookupNameEnv (global_tycons env) (tyConName tc)

defTyCon :: TyCon -> TyCon -> VM ()
defTyCon tc tc' = updGEnv $ \env ->
  env { global_tycons = extendNameEnv (global_tycons env) (tyConName tc) tc' }

lookupDataCon :: DataCon -> VM (Maybe DataCon)
lookupDataCon dc = readGEnv $ \env -> lookupNameEnv (global_datacons env) (dataConName dc)

defDataCon :: DataCon -> DataCon -> VM ()
defDataCon dc dc' = updGEnv $ \env ->
  env { global_datacons = extendNameEnv (global_datacons env) (dataConName dc) dc' }

lookupPrimPArray :: TyCon -> VM (Maybe TyCon)
lookupPrimPArray = liftDs . primPArray

lookupPrimMethod :: TyCon -> String -> VM (Maybe Var)
lookupPrimMethod tycon = liftDs . primMethod tycon

lookupTyConPA :: TyCon -> VM (Maybe Var)
lookupTyConPA tc = readGEnv $ \env -> lookupNameEnv (global_pa_funs env) (tyConName tc)

defTyConPA :: TyCon -> Var -> VM ()
defTyConPA tc pa = updGEnv $ \env ->
  env { global_pa_funs = extendNameEnv (global_pa_funs env) (tyConName tc) pa }

defTyConPAs :: [(TyCon, Var)] -> VM ()
defTyConPAs ps = updGEnv $ \env ->
  env { global_pa_funs = extendNameEnvList (global_pa_funs env)
                                           [(tyConName tc, pa) | (tc, pa) <- ps] }

lookupTyVarPA :: Var -> VM (Maybe CoreExpr)
lookupTyVarPA tv = readLEnv $ \env -> lookupVarEnv (local_tyvar_pa env) tv

lookupTyConPR :: TyCon -> VM (Maybe Var)
lookupTyConPR tc = readGEnv $ \env -> lookupNameEnv (global_pr_funs env) (tyConName tc)

defLocalTyVar :: TyVar -> VM ()
defLocalTyVar tv = updLEnv $ \env ->
  env { local_tyvars   = tv : local_tyvars env
      , local_tyvar_pa = local_tyvar_pa env `delVarEnv` tv
      }

defLocalTyVarWithPA :: TyVar -> CoreExpr -> VM ()
defLocalTyVarWithPA tv pa = updLEnv $ \env ->
  env { local_tyvars   = tv : local_tyvars env
      , local_tyvar_pa = extendVarEnv (local_tyvar_pa env) tv pa
      }

localTyVars :: VM [TyVar]
localTyVars = readLEnv (reverse . local_tyvars)

-- Look up the dfun of a class instance.
--
-- The match must be unique - ie, match exactly one instance - but the 
-- type arguments used for matching may be more specific than those of 
-- the class instance declaration.  The found class instances must not have
-- any type variables in the instance context that do not appear in the
-- instances head (i.e., no flexi vars); for details for what this means,
-- see the docs at InstEnv.lookupInstEnv.
--
{-
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
	   _other         -> traceNoV "lookupInst" (ppr cls <+> ppr tys)
       }
  where
    isRight (Left  _) = False
    isRight (Right _) = True
-}

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
      Just r <- initDs hsc_env (mg_module guts)
                               (mg_rdr_env guts)
                               (mg_types guts)
                               go
      return r
  where

    go =
      do
        builtins       <- initBuiltins
        let builtin_tycons = initBuiltinTyCons builtins
        builtin_pas    <- initBuiltinPAs builtins
        builtin_prs    <- initBuiltinPRs builtins

        eps <- ioToIOEnv $ hscEPS hsc_env
        let famInstEnvs = (eps_fam_inst_env eps, mg_fam_inst_env guts)
            instEnvs    = (eps_inst_env     eps, mg_inst_env     guts)

        let genv = extendTyConsEnv builtin_tycons
                 . extendPAFunsEnv builtin_pas
                 . setPRFunsEnv    builtin_prs
                 $ initGlobalEnv info instEnvs famInstEnvs

        r <- runVM p builtins genv emptyLocalEnv
        case r of
          Yes genv _ x -> return $ Just (new_info genv, x)
          No           -> return Nothing

    new_info genv = updVectInfo genv (mg_types guts) info

