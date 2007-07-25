module VectUtils (
  collectAnnTypeBinders, collectAnnTypeArgs, isAnnTypeArg,
  splitClosureTy,
  mkPADictType, mkPArrayType,
  paDictArgType, paDictOfType,
  paMethod, lengthPA, replicatePA,
  lookupPArrayFamInst,
  hoistExpr, takeHoisted
) where

#include "HsVersions.h"

import VectMonad

import CoreSyn
import CoreUtils
import Type
import TypeRep
import TyCon
import Var
import PrelNames

import Outputable
import FastString

import Control.Monad         ( liftM )

collectAnnTypeArgs :: AnnExpr b ann -> (AnnExpr b ann, [Type])
collectAnnTypeArgs expr = go expr []
  where
    go (_, AnnApp f (_, AnnType ty)) tys = go f (ty : tys)
    go e                             tys = (e, tys)

collectAnnTypeBinders :: AnnExpr Var ann -> ([Var], AnnExpr Var ann)
collectAnnTypeBinders expr = go [] expr
  where
    go bs (_, AnnLam b e) | isTyVar b = go (b:bs) e
    go bs e                           = (reverse bs, e)

isAnnTypeArg :: AnnExpr b ann -> Bool
isAnnTypeArg (_, AnnType t) = True
isAnnTypeArg _              = False

isClosureTyCon :: TyCon -> Bool
isClosureTyCon tc = tyConUnique tc == closureTyConKey

splitClosureTy :: Type -> (Type, Type)
splitClosureTy ty
  | Just (tc, [arg_ty, res_ty]) <- splitTyConApp_maybe ty
  , isClosureTyCon tc
  = (arg_ty, res_ty)

  | otherwise = pprPanic "splitClosureTy" (ppr ty)

mkPADictType :: Type -> VM Type
mkPADictType ty
  = do
      tc <- builtin paDictTyCon
      return $ TyConApp tc [ty]

mkPArrayType :: Type -> VM Type
mkPArrayType ty
  = do
      tc <- builtin parrayTyCon
      return $ TyConApp tc [ty]

paDictArgType :: TyVar -> VM (Maybe Type)
paDictArgType tv = go (TyVarTy tv) (tyVarKind tv)
  where
    go ty k | Just k' <- kindView k = go ty k'
    go ty (FunTy k1 k2)
      = do
          tv   <- newTyVar FSLIT("a") k1
          mty1 <- go (TyVarTy tv) k1
          case mty1 of
            Just ty1 -> do
                          mty2 <- go (AppTy ty (TyVarTy tv)) k2
                          return $ fmap (ForAllTy tv . FunTy ty1) mty2
            Nothing  -> go ty k2

    go ty k
      | isLiftedTypeKind k
      = liftM Just (mkPADictType ty)

    go ty k = return Nothing

paDictOfType :: Type -> VM CoreExpr
paDictOfType ty = paDictOfTyApp ty_fn ty_args
  where
    (ty_fn, ty_args) = splitAppTys ty

paDictOfTyApp :: Type -> [Type] -> VM CoreExpr
paDictOfTyApp ty_fn ty_args
  | Just ty_fn' <- coreView ty_fn = paDictOfTyApp ty_fn' ty_args
paDictOfTyApp (TyVarTy tv) ty_args
  = do
      dfun <- maybeV (lookupTyVarPA tv)
      paDFunApply dfun ty_args
paDictOfTyApp (TyConApp tc _) ty_args
  = do
      pa_class <- builtin paClass
      (dfun, ty_args') <- lookupInst pa_class [TyConApp tc ty_args]
      paDFunApply (Var dfun) ty_args'
paDictOfTyApp ty ty_args = pprPanic "paDictOfTyApp" (ppr ty)

paDFunApply :: CoreExpr -> [Type] -> VM CoreExpr
paDFunApply dfun tys
  = do
      dicts <- mapM paDictOfType tys
      return $ mkApps (mkTyApps dfun tys) dicts

paMethod :: (Builtins -> Var) -> Type -> VM CoreExpr
paMethod method ty
  = do
      fn   <- builtin method
      dict <- paDictOfType ty
      return $ mkApps (Var fn) [Type ty, dict]

lengthPA :: CoreExpr -> VM CoreExpr
lengthPA x = liftM (`App` x) (paMethod lengthPAVar (exprType x))

replicatePA :: CoreExpr -> CoreExpr -> VM CoreExpr
replicatePA len x = liftM (`mkApps` [len,x])
                          (paMethod replicatePAVar (exprType x))

lookupPArrayFamInst :: Type -> VM (TyCon, [Type])
lookupPArrayFamInst ty = builtin parrayTyCon >>= (`lookupFamInst` [ty])

hoistExpr :: FastString -> CoreExpr -> VM Var
hoistExpr fs expr
  = do
      var <- newLocalVar fs (exprType expr)
      updGEnv $ \env ->
        env { global_bindings = (var, expr) : global_bindings env }
      return var

takeHoisted :: VM [(Var, CoreExpr)]
takeHoisted
  = do
      env <- readGEnv id
      setGEnv $ env { global_bindings = [] }
      return $ global_bindings env

