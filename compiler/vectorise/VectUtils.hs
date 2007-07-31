module VectUtils (
  collectAnnTypeBinders, collectAnnTypeArgs, isAnnTypeArg,
  splitClosureTy,
  mkPADictType, mkPArrayType,
  paDictArgType, paDictOfType,
  paMethod, lengthPA, replicatePA, emptyPA,
  polyAbstract, polyApply,
  lookupPArrayFamInst,
  hoistExpr, takeHoisted
) where

#include "HsVersions.h"

import VectMonad

import DsUtils
import CoreSyn
import CoreUtils
import Type
import TypeRep
import TyCon
import DataCon            ( dataConWrapId )
import Var
import Id                 ( mkWildId )
import MkId               ( unwrapFamInstScrut )
import PrelNames
import TysWiredIn
import BasicTypes         ( Boxity(..) )

import Outputable
import FastString

import Control.Monad         ( liftM, zipWithM_ )

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
isClosureTyCon tc = tyConName tc == closureTyConName

splitClosureTy :: Type -> (Type, Type)
splitClosureTy ty
  | Just (tc, [arg_ty, res_ty]) <- splitTyConApp_maybe ty
  , isClosureTyCon tc
  = (arg_ty, res_ty)

  | otherwise = pprPanic "splitClosureTy" (ppr ty)

isPArrayTyCon :: TyCon -> Bool
isPArrayTyCon tc = tyConName tc == parrayTyConName

splitPArrayTy :: Type -> Type
splitPArrayTy ty
  | Just (tc, [arg_ty]) <- splitTyConApp_maybe ty
  , isPArrayTyCon tc
  = arg_ty

  | otherwise = pprPanic "splitPArrayTy" (ppr ty)

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
lengthPA x = liftM (`App` x) (paMethod lengthPAVar ty)
  where
    ty = splitPArrayTy (exprType x)

replicatePA :: CoreExpr -> CoreExpr -> VM CoreExpr
replicatePA len x = liftM (`mkApps` [len,x])
                          (paMethod replicatePAVar (exprType x))

emptyPA :: Type -> VM CoreExpr
emptyPA = paMethod emptyPAVar

polyAbstract :: [TyVar] -> ((CoreExpr -> CoreExpr) -> VM a) -> VM a
polyAbstract tvs p
  = localV
  $ do
      mdicts <- mapM mk_dict_var tvs
      zipWithM_ (\tv -> maybe (defLocalTyVar tv) (defLocalTyVarWithPA tv . Var)) tvs mdicts
      p (mk_lams mdicts)
  where
    mk_dict_var tv = do
                       r <- paDictArgType tv
                       case r of
                         Just ty -> liftM Just (newLocalVar FSLIT("dPA") ty)
                         Nothing -> return Nothing

    mk_lams mdicts = mkLams (tvs ++ [dict | Just dict <- mdicts])

polyApply :: CoreExpr -> [Type] -> VM CoreExpr
polyApply expr tys
  = do
      dicts <- mapM paDictOfType tys
      return $ expr `mkTyApps` tys `mkApps` dicts

lookupPArrayFamInst :: Type -> VM (TyCon, [Type])
lookupPArrayFamInst ty = builtin parrayTyCon >>= (`lookupFamInst` [ty])

hoistExpr :: FastString -> CoreExpr -> VM Var
hoistExpr fs expr
  = do
      var <- newLocalVar fs (exprType expr)
      updGEnv $ \env ->
        env { global_bindings = (var, expr) : global_bindings env }
      return var

hoistPolyExpr :: FastString -> [TyVar] -> CoreExpr -> VM CoreExpr
hoistPolyExpr fs tvs expr
  = do
      poly_expr <- closedV . polyAbstract tvs $ \abstract -> return (abstract expr)
      fn        <- hoistExpr fs poly_expr
      polyApply (Var fn) (mkTyVarTys tvs)

takeHoisted :: VM [(Var, CoreExpr)]
takeHoisted
  = do
      env <- readGEnv id
      setGEnv $ env { global_bindings = [] }
      return $ global_bindings env


mkClosure :: Type -> Type -> Type -> CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr -> VM CoreExpr
mkClosure arg_ty res_ty env_ty pa_dict vfn lfn env
  = do
      mk <- builtin mkClosureVar
      return $ Var mk `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [pa_dict, vfn, lfn, env]

mkClosureP :: Type -> Type -> Type -> CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr -> VM CoreExpr
mkClosureP arg_ty res_ty env_ty pa_dict vfn lfn env
  = do
      mk <- builtin mkClosurePVar
      return $ Var mk `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [pa_dict, vfn, lfn, env]

-- (clo <x1,...,xn> <f,f^>, aclo (Arr lc xs1 ... xsn) <f,f^>)
--   where
--     f  = \env v -> case env of <x1,...,xn> -> e x1 ... xn v
--     f^ = \env v -> case env of Arr l xs1 ... xsn -> e^ l x1 ... xn v

buildClosure :: [TyVar] -> [(Var,Var)] -> (Var,Var) -> Var -> CoreExpr -> CoreExpr -> VM (CoreExpr, CoreExpr)
buildClosure tvs env (varg, larg) lv vbody lbody
  = do
      let (venv_ty, venv, bind_venv) = mkVectEnv tys vs
      (lenv, bind_lenv) <- mkLiftEnv (Var lv) tys ls
      lenv_ty <- mkPArrayType venv_ty

      venv_bndr <- newLocalVar FSLIT("env") venv_ty
      lenv_bndr <- newLocalVar FSLIT("env") lenv_ty

      let mono_vfn = mkLams [venv_bndr, varg]
                   . bind_venv (Var venv_bndr)
                   $ vbody `mkVarApps` vs `mkVarApps` [varg]
          mono_lfn = mkLams [lenv_bndr, larg]
                   . bind_lenv (Var lenv_bndr) lv
                   $ lbody `mkVarApps` (lv:ls) `mkVarApps` [larg]

      vfn <- hoistPolyExpr FSLIT("vfn") tvs mono_vfn
      lfn <- hoistPolyExpr FSLIT("lfn") tvs mono_lfn

      pa_dict <- paDictOfType venv_ty

      vclo <- mkClosure  arg_ty res_ty venv_ty pa_dict vfn lfn venv
      lclo <- mkClosureP arg_ty res_ty venv_ty pa_dict vfn lfn lenv

      return (vclo, lclo)

  where
    vs  = map fst env
    ls  = map snd env
    tys = map idType vs

    arg_ty = idType varg
    res_ty = exprType vbody
    
mkVectEnv :: [Type] -> [Var] -> (Type, CoreExpr, CoreExpr -> CoreExpr -> CoreExpr)
mkVectEnv []   []  = (unitTy, Var unitDataConId, \env body -> body)
mkVectEnv [ty] [v] = (ty, Var v, \env body -> Let (NonRec v env) body)
mkVectEnv tys  vs  = (ty, mkCoreTup (map Var vs),
                        \env body -> Case env (mkWildId ty) (exprType body)
                                       [(DataAlt (tupleCon Boxed (length vs)), vs, body)])
  where
    ty = mkCoreTupTy tys

mkLiftEnv :: CoreExpr -> [Type] -> [Var]
          -> VM (CoreExpr, CoreExpr -> Var -> CoreExpr -> CoreExpr)
mkLiftEnv lc [ty] [v]
  = do
      len <- lengthPA (Var v)
      return (Var v, \env lv body -> Let (NonRec v env)
                                   $ Case len lv (exprType body) [(DEFAULT, [], body)])

-- NOTE: this transparently deals with empty environments
mkLiftEnv lc tys vs
  = do
      (env_tc, env_tyargs) <- lookupPArrayFamInst vty
      let [env_con] = tyConDataCons env_tc
          
          env = Var (dataConWrapId env_con)
                `mkTyApps` env_tyargs
                `mkApps`   (lc : map Var vs)

          bind env lv body = let scrut = unwrapFamInstScrut env_tc env_tyargs env
                             in
                             Case scrut (mkWildId (exprType scrut)) (exprType body)
                               [(DataAlt env_con, lv : vs, body)]
      return (env, bind)
  where
    vty = mkCoreTupTy tys

