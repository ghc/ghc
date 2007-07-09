module Vectorise( vectorise )
where

#include "HsVersions.h"

import DynFlags
import HscTypes

import CoreLint             ( showPass, endPass )
import CoreSyn
import TyCon
import Type
import TypeRep
import Var
import VarEnv
import Name                 ( mkSysTvName )
import NameEnv

import DsMonad

import PrelNames

import Outputable
import FastString
import Control.Monad        ( liftM2 )

vectorise :: HscEnv -> ModGuts -> IO ModGuts
vectorise hsc_env guts
  | not (Opt_Vectorise `dopt` dflags) = return guts
  | otherwise
  = do
      showPass dflags "Vectorisation"
      eps <- hscEPS hsc_env
      let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps
      Just guts' <- initDs hsc_env (mg_module guts)
                                   (mg_rdr_env guts)
                                   (mg_types guts)
                                   (vectoriseModule info guts)
      endPass dflags "Vectorisation" Opt_D_dump_vect (mg_binds guts')
      return guts'
  where
    dflags = hsc_dflags hsc_env

-- ----------------------------------------------------------------------------
-- Vectorisation monad

data Builtins = Builtins {
                  parrayTyCon      :: TyCon
                , paTyCon          :: TyCon
                , closureTyCon     :: TyCon
                , mkClosureVar     :: Var
                , applyClosureVar  :: Var
                , mkClosurePVar    :: Var
                , applyClosurePVar :: Var
                , closurePAVar     :: Var
                , lengthPAVar      :: Var
                , replicatePAVar   :: Var
                }

initBuiltins :: DsM Builtins
initBuiltins
  = do
      parrayTyCon  <- dsLookupTyCon parrayTyConName
      paTyCon      <- dsLookupTyCon paTyConName
      closureTyCon <- dsLookupTyCon closureTyConName

      mkClosureVar     <- dsLookupGlobalId mkClosureName
      applyClosureVar  <- dsLookupGlobalId applyClosureName
      mkClosurePVar    <- dsLookupGlobalId mkClosurePName
      applyClosurePVar <- dsLookupGlobalId applyClosurePName
      closurePAVar     <- dsLookupGlobalId closurePAName
      lengthPAVar      <- dsLookupGlobalId lengthPAName
      replicatePAVar   <- dsLookupGlobalId replicatePAName

      return $ Builtins {
                 parrayTyCon      = parrayTyCon
               , paTyCon          = paTyCon
               , closureTyCon     = closureTyCon
               , mkClosureVar     = mkClosureVar
               , applyClosureVar  = applyClosureVar
               , mkClosurePVar    = mkClosurePVar
               , applyClosurePVar = applyClosurePVar
               , closurePAVar     = closurePAVar
               , lengthPAVar      = lengthPAVar
               , replicatePAVar   = replicatePAVar
               }

data VEnv = VEnv {
              -- Mapping from global variables to their vectorised versions.
              -- 
              vect_global_vars :: VarEnv CoreExpr

              -- Mapping from local variables to their vectorised and lifted
              -- versions.
              --
            , vect_local_vars :: VarEnv (CoreExpr, CoreExpr)

              -- Exported variables which have a vectorised version
              --
            , vect_exported_vars :: VarEnv (Var, Var)

              -- Mapping from TyCons to their vectorised versions.
              -- TyCons which do not have to be vectorised are mapped to
              -- themselves.
              --
            , vect_tycons :: NameEnv TyCon

              -- Mapping from TyCons to their PA dictionaries
              --
            , vect_tycon_pa :: NameEnv CoreExpr

              -- Mapping from tyvars to their PA dictionaries
              --
            , vect_tyvar_pa :: VarEnv CoreExpr
            }

initVEnv :: VectInfo -> DsM VEnv
initVEnv info
  = return $ VEnv {
               vect_global_vars   = mapVarEnv  (Var . snd) $ vectInfoCCVar   info
             , vect_local_vars    = emptyVarEnv
             , vect_exported_vars = emptyVarEnv
             , vect_tycons        = mapNameEnv snd $ vectInfoCCTyCon info
             , vect_tycon_pa      = emptyNameEnv
             , vect_tyvar_pa      = emptyVarEnv
             }

-- FIXME
updVectInfo :: VEnv -> ModGuts -> ModGuts
updVectInfo env guts = guts { mg_vect_info = info' }
  where
    info' = info {
              vectInfoCCVar   = vect_exported_vars env
            , vectInfoCCTyCon = tc_env
            }

    info  = mg_vect_info guts
    tyenv = mg_types guts

    tc_env = mkNameEnv [(tc_name, (tc,tc')) | tc <- typeEnvTyCons tyenv
                                            , let tc_name = tyConName tc
                                            , Just tc' <- [lookupNameEnv (vect_tycons env) tc_name]]

data VResult a = Yes VEnv a | No

newtype VM a = VM { runVM :: Builtins -> VEnv -> DsM (VResult a) }

instance Monad VM where
  return x   = VM $ \bi env -> return (Yes env x)
  VM p >>= f = VM $ \bi env -> do
                                 r <- p bi env
                                 case r of
                                   Yes env' x -> runVM (f x) bi env'
                                   No         -> return No

noV :: VM a
noV = VM $ \bi env -> return No

tryV :: VM a -> VM (Maybe a)
tryV (VM p) = VM $ \bi env -> do
                                r <- p bi env
                                case r of
                                  Yes env' x -> return (Yes env' (Just x))
                                  No         -> return (Yes env Nothing)

maybeV :: VM (Maybe a) -> VM a
maybeV p = maybe noV return =<< p

orElseV :: VM a -> VM a -> VM a
orElseV p q = maybe q return =<< tryV p

liftDs :: DsM a -> VM a
liftDs p = VM $ \bi env -> do { x <- p; return (Yes env x) }

builtin :: (Builtins -> a) -> VM a
builtin f = VM $ \bi env -> return (Yes env (f bi))

readEnv :: (VEnv -> a) -> VM a
readEnv f = VM $ \bi env -> return (Yes env (f env))

setEnv :: VEnv -> VM ()
setEnv env = VM $ \_ _ -> return (Yes env ())

updEnv :: (VEnv -> VEnv) -> VM ()
updEnv f = VM $ \_ env -> return (Yes (f env) ())

newTyVar :: FastString -> Kind -> VM Var
newTyVar fs k
  = do
      u <- liftDs newUnique
      return $ mkTyVar (mkSysTvName u fs) k

lookupVar :: Var -> VM CoreExpr
lookupVar v = maybeV . readEnv $ \env -> lookupVarEnv (vect_vars env) v

lookupTyCon :: TyCon -> VM (Maybe TyCon)
lookupTyCon tc = readEnv $ \env -> lookupNameEnv (vect_tycons env) (tyConName tc)

-- ----------------------------------------------------------------------------
-- Bindings

vectoriseModule :: VectInfo -> ModGuts -> DsM ModGuts
vectoriseModule info guts
  = do
      builtins <- initBuiltins
      env <- initVEnv info
      r <- runVM (vectModule guts) builtins env
      case r of
        Yes env' guts' -> return $ updVectInfo env' guts'
        No             -> return guts

vectModule :: ModGuts -> VM ModGuts
vectModule guts = return guts

-- ----------------------------------------------------------------------------
-- Types

paArgType :: Type -> Kind -> VM (Maybe Type)
paArgType ty k
  | Just k' <- kindView k = paArgType ty k'

-- Here, we assume that for a kind (k1 -> k2) to be valid, k1 and k2 can only
-- be made up of * and (->), i.e., they can't be coercion kinds or #.
paArgType ty (FunTy k1 k2)
  = do
      tv  <- newTyVar FSLIT("a") k1
      ty1 <- paArgType' (TyVarTy tv) k1
      ty2 <- paArgType' (AppTy ty (TyVarTy tv)) k2
      return . Just $ ForAllTy tv (FunTy ty1 ty2)

paArgType ty k
  | isLiftedTypeKind k
  = do
      tc <- builtin paTyCon
      return . Just $ TyConApp tc [ty]

  | otherwise
  = return Nothing 

paArgType' :: Type -> Kind -> VM Type
paArgType' ty k
  = do
      r <- paArgType ty k
      case r of
        Just ty' -> return ty'
        Nothing  -> pprPanic "paArgType'" (ppr ty)

vectTyCon :: TyCon -> VM TyCon
vectTyCon tc
  | isFunTyCon tc        = builtin closureTyCon
  | isBoxedTupleTyCon tc = return tc
  | isUnLiftedTyCon tc   = return tc
  | otherwise = do
                  r <- lookupTyCon tc
                  case r of
                    Just tc' -> return tc'

                    -- FIXME: just for now
                    Nothing  -> pprTrace "ccTyCon:" (ppr tc) $ return tc

vectType :: Type -> VM Type
vectType ty | Just ty' <- coreView ty = vectType ty
vectType (TyVarTy tv) = return $ TyVarTy tv
vectType (AppTy ty1 ty2) = liftM2 AppTy (vectType ty1) (vectType ty2)
vectType (TyConApp tc tys) = liftM2 TyConApp (vectTyCon tc) (mapM vectType tys)
vectType (FunTy ty1 ty2)   = liftM2 TyConApp (builtin closureTyCon)
                                             (mapM vectType [ty1,ty2])
vectType (ForAllTy tv ty)
  = do
      r   <- paArgType (TyVarTy tv) (tyVarKind tv)
      ty' <- vectType ty
      return . ForAllTy tv $ case r of { Just paty -> FunTy paty ty'; Nothing -> ty' }

vectType ty = pprPanic "vectType:" (ppr ty)

isClosureTyCon :: TyCon -> Bool
isClosureTyCon tc = tyConUnique tc == closureTyConKey

splitClosureTy :: Type -> (Type, Type)
splitClosureTy ty
  | Just (tc, [arg_ty, res_ty]) <- splitTyConApp_maybe ty
  , isClosureTyCon tc
  = (arg_ty, res_ty)

  | otherwise = pprPanic "splitClosureTy" (ppr ty)

