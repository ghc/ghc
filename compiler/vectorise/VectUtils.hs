module VectUtils (
  collectAnnTypeBinders, collectAnnTypeArgs, isAnnTypeArg,
  collectAnnValBinders,
  mkDataConTag,
  splitClosureTy,
  mkPRepr, mkToPRepr, mkFromPRepr,
  mkPADictType, mkPArrayType, mkPReprType,
  parrayReprTyCon, parrayReprDataCon, mkVScrut,
  prDictOfType, prCoerce,
  paDictArgType, paDictOfType, paDFunType,
  paMethod, lengthPA, replicatePA, emptyPA, liftPA,
  polyAbstract, polyApply, polyVApply,
  hoistBinding, hoistExpr, hoistPolyVExpr, takeHoisted,
  buildClosure, buildClosures,
  mkClosureApp
) where

#include "HsVersions.h"

import VectCore
import VectMonad

import DsUtils
import CoreSyn
import CoreUtils
import Coercion
import Type
import TypeRep
import TyCon
import DataCon            ( DataCon, dataConWrapId, dataConTag )
import Var
import Id                 ( mkWildId )
import MkId               ( unwrapFamInstScrut )
import Name               ( Name )
import PrelNames
import TysWiredIn
import BasicTypes         ( Boxity(..) )

import Outputable
import FastString

import Control.Monad         ( liftM, liftM2, zipWithM_ )

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

collectAnnValBinders :: AnnExpr Var ann -> ([Var], AnnExpr Var ann)
collectAnnValBinders expr = go [] expr
  where
    go bs (_, AnnLam b e) | isId b = go (b:bs) e
    go bs e                        = (reverse bs, e)

isAnnTypeArg :: AnnExpr b ann -> Bool
isAnnTypeArg (_, AnnType t) = True
isAnnTypeArg _              = False

mkDataConTag :: DataCon -> CoreExpr
mkDataConTag dc = mkConApp intDataCon [mkIntLitInt $ dataConTag dc]

splitUnTy :: String -> Name -> Type -> Type
splitUnTy s name ty
  | Just (tc, [ty']) <- splitTyConApp_maybe ty
  , tyConName tc == name
  = ty'

  | otherwise = pprPanic s (ppr ty)

splitBinTy :: String -> Name -> Type -> (Type, Type)
splitBinTy s name ty
  | Just (tc, [ty1, ty2]) <- splitTyConApp_maybe ty
  , tyConName tc == name
  = (ty1, ty2)

  | otherwise = pprPanic s (ppr ty)

splitCrossTy :: Type -> (Type, Type)
splitCrossTy = splitBinTy "splitCrossTy" ndpCrossTyConName

splitPlusTy :: Type -> (Type, Type)
splitPlusTy = splitBinTy "splitSumTy" ndpPlusTyConName

splitEmbedTy :: Type -> Type
splitEmbedTy = splitUnTy "splitEmbedTy" embedTyConName

splitClosureTy :: Type -> (Type, Type)
splitClosureTy = splitBinTy "splitClosureTy" closureTyConName

splitPArrayTy :: Type -> Type
splitPArrayTy = splitUnTy "splitPArrayTy" parrayTyConName

mkBuiltinTyConApp :: (Builtins -> TyCon) -> [Type] -> VM Type
mkBuiltinTyConApp get_tc tys
  = do
      tc <- builtin get_tc
      return $ mkTyConApp tc tys

mkBuiltinTyConApps :: (Builtins -> TyCon) -> [Type] -> Type -> VM Type
mkBuiltinTyConApps get_tc tys ty
  = do
      tc <- builtin get_tc
      return $ foldr (mk tc) ty tys
  where
    mk tc ty1 ty2 = mkTyConApp tc [ty1,ty2]

mkBuiltinTyConApps1 :: (Builtins -> TyCon) -> Type -> [Type] -> VM Type
mkBuiltinTyConApps1 get_tc dft [] = return dft
mkBuiltinTyConApps1 get_tc dft tys
  = do
      tc <- builtin get_tc
      case tys of
        [] -> pprPanic "mkBuiltinTyConApps1" (ppr tc)
        _  -> return $ foldr1 (mk tc) tys
  where
    mk tc ty1 ty2 = mkTyConApp tc [ty1,ty2]

mkPRepr :: [[Type]] -> VM Type
mkPRepr [] = return unitTy
mkPRepr tys
  = do
      embed <- builtin embedTyCon
      cross <- builtin crossTyCon
      plus  <- builtin plusTyCon

      let mk_embed ty      = mkTyConApp embed [ty]
          mk_cross ty1 ty2 = mkTyConApp cross [ty1, ty2]
          mk_plus  ty1 ty2 = mkTyConApp plus  [ty1, ty2]

          mk_tup   []      = unitTy
          mk_tup   tys     = foldr1 mk_cross tys

          mk_sum   []      = unitTy
          mk_sum   tys     = foldr1 mk_plus  tys

      return . mk_sum
             . map (mk_tup . map mk_embed)
             $ tys

mkToPRepr :: [[CoreExpr]] -> VM ([CoreExpr], Type)
mkToPRepr ess
  = do
      embed_tc <- builtin embedTyCon
      embed_dc <- builtin embedDataCon
      cross_tc <- builtin crossTyCon
      cross_dc <- builtin crossDataCon
      plus_tc  <- builtin plusTyCon
      left_dc  <- builtin leftDataCon
      right_dc <- builtin rightDataCon

      let mk_embed expr
            = (mkConApp   embed_dc [Type ty, expr],
               mkTyConApp embed_tc [ty])
            where ty = exprType expr

          mk_cross (expr1, ty1) (expr2, ty2)
            = (mkConApp   cross_dc [Type ty1, Type ty2, expr1, expr2],
               mkTyConApp cross_tc [ty1, ty2])

          mk_tup [] = (Var unitDataConId, unitTy)
          mk_tup es = foldr1 mk_cross es

          mk_sum []           = ([Var unitDataConId], unitTy)
          mk_sum [(expr, ty)] = ([expr], ty)
          mk_sum ((expr, lty) : es)
            = let (alts, rty) = mk_sum es
              in
              (mkConApp left_dc [Type lty, Type rty, expr]
                 : [mkConApp right_dc [Type lty, Type rty, alt] | alt <- alts],
               mkTyConApp plus_tc [lty, rty])

      return . mk_sum $ map (mk_tup . map mk_embed) ess

mkFromPRepr :: CoreExpr -> Type -> [([Var], CoreExpr)] -> VM CoreExpr
mkFromPRepr scrut res_ty alts
  = do
      embed_dc <- builtin embedDataCon
      cross_dc <- builtin crossDataCon
      left_dc  <- builtin leftDataCon
      right_dc <- builtin rightDataCon
      pa_tc    <- builtin paTyCon

      let un_embed expr ty var res
            = Case expr (mkWildId ty) res_ty
                   [(DataAlt embed_dc, [var], res)]

          un_cross expr ty var1 var2 res
            = Case expr (mkWildId ty) res_ty
                [(DataAlt cross_dc, [var1, var2], res)]

          un_tup expr ty []    res = return res
          un_tup expr ty [var] res = return $ un_embed expr ty var res
          un_tup expr ty (var : vars) res
            = do
                lv <- newLocalVar FSLIT("x") lty
                rv <- newLocalVar FSLIT("y") rty
                liftM (un_cross expr ty lv rv
                      . un_embed (Var lv) lty var)
                      (un_tup (Var rv) rty vars res)
            where
              (lty, rty) = splitCrossTy ty

          un_plus expr ty var1 var2 res1 res2
            = Case expr (mkWildId ty) res_ty
                [(DataAlt left_dc,  [var1], res1),
                 (DataAlt right_dc, [var2], res2)]

          un_sum expr ty [(vars, res)] = un_tup expr ty vars res
          un_sum expr ty ((vars, res) : alts)
            = do
                lv <- newLocalVar FSLIT("l") lty
                rv <- newLocalVar FSLIT("r") rty
                liftM2 (un_plus expr ty lv rv)
                         (un_tup (Var lv) lty vars res)
                         (un_sum (Var rv) rty alts)
            where
              (lty, rty) = splitPlusTy ty

      un_sum scrut (exprType scrut) alts

mkClosureType :: Type -> Type -> VM Type
mkClosureType arg_ty res_ty = mkBuiltinTyConApp closureTyCon [arg_ty, res_ty]

mkClosureTypes :: [Type] -> Type -> VM Type
mkClosureTypes = mkBuiltinTyConApps closureTyCon

mkPReprType :: Type -> VM Type
mkPReprType ty = mkBuiltinTyConApp preprTyCon [ty]

mkPADictType :: Type -> VM Type
mkPADictType ty = mkBuiltinTyConApp paTyCon [ty]

mkPArrayType :: Type -> VM Type
mkPArrayType ty = mkBuiltinTyConApp parrayTyCon [ty]

parrayReprTyCon :: Type -> VM (TyCon, [Type])
parrayReprTyCon ty = builtin parrayTyCon >>= (`lookupFamInst` [ty])

parrayReprDataCon :: Type -> VM (DataCon, [Type])
parrayReprDataCon ty
  = do
      (tc, arg_tys) <- parrayReprTyCon ty
      let [dc] = tyConDataCons tc
      return (dc, arg_tys)

mkVScrut :: VExpr -> VM (VExpr, TyCon, [Type])
mkVScrut (ve, le)
  = do
      (tc, arg_tys) <- parrayReprTyCon (exprType ve)
      return ((ve, unwrapFamInstScrut tc arg_tys le), tc, arg_tys)

prDictOfType :: Type -> VM CoreExpr
prDictOfType orig_ty
  | Just (tycon, ty_args) <- splitTyConApp_maybe orig_ty
  = do
      dfun <- traceMaybeV "prDictOfType" (ppr tycon) (lookupTyConPR tycon)
      prDFunApply (Var dfun) ty_args

prDFunApply :: CoreExpr -> [Type] -> VM CoreExpr
prDFunApply dfun tys
  = do
      args <- mapM mkDFunArg arg_tys
      return $ mkApps mono_dfun args
  where
    mono_dfun    = mkTyApps dfun tys
    (arg_tys, _) = splitFunTys (exprType mono_dfun)

mkDFunArg :: Type -> VM CoreExpr
mkDFunArg ty
  | Just (tycon, [arg]) <- splitTyConApp_maybe ty

  = let name = tyConName tycon

        get_dict | name == paTyConName = paDictOfType
                 | name == prTyConName = prDictOfType
                 | otherwise           = pprPanic "mkDFunArg" (ppr ty)

    in get_dict arg

mkDFunArg ty = pprPanic "mkDFunArg" (ppr ty)

prCoerce :: TyCon -> [Type] -> CoreExpr -> VM CoreExpr
prCoerce repr_tc args expr
  | Just arg_co <- tyConFamilyCoercion_maybe repr_tc
  = do
      pr_tc <- builtin prTyCon

      let co = mkAppCoercion (mkTyConApp pr_tc [])
                             (mkSymCoercion (mkTyConApp arg_co args))

      return $ mkCoerce co expr

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
      dfun <- traceMaybeV "paDictOfTyApp" (ppr tc) (lookupTyConPA tc)
      paDFunApply (Var dfun) ty_args
paDictOfTyApp ty ty_args = pprPanic "paDictOfTyApp" (ppr ty)

paDFunType :: TyCon -> VM Type
paDFunType tc
  = do
      margs <- mapM paDictArgType tvs
      res   <- mkPADictType (mkTyConApp tc arg_tys)
      return . mkForAllTys tvs
             $ mkFunTys [arg | Just arg <- margs] res
  where
    tvs = tyConTyVars tc
    arg_tys = mkTyVarTys tvs

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

liftPA :: CoreExpr -> VM CoreExpr
liftPA x
  = do
      lc <- builtin liftingContext
      replicatePA (Var lc) x

newLocalVVar :: FastString -> Type -> VM VVar
newLocalVVar fs vty
  = do
      lty <- mkPArrayType vty
      vv  <- newLocalVar fs vty
      lv  <- newLocalVar fs lty
      return (vv,lv)

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

polyVApply :: VExpr -> [Type] -> VM VExpr
polyVApply expr tys
  = do
      dicts <- mapM paDictOfType tys
      return $ mapVect (\e -> e `mkTyApps` tys `mkApps` dicts) expr

hoistBinding :: Var -> CoreExpr -> VM ()
hoistBinding v e = updGEnv $ \env ->
  env { global_bindings = (v,e) : global_bindings env }

hoistExpr :: FastString -> CoreExpr -> VM Var
hoistExpr fs expr
  = do
      var <- newLocalVar fs (exprType expr)
      hoistBinding var expr
      return var

hoistVExpr :: VExpr -> VM VVar
hoistVExpr (ve, le)
  = do
      fs <- getBindName
      vv <- hoistExpr ('v' `consFS` fs) ve
      lv <- hoistExpr ('l' `consFS` fs) le
      return (vv, lv)

hoistPolyVExpr :: [TyVar] -> VM VExpr -> VM VExpr
hoistPolyVExpr tvs p
  = do
      expr <- closedV . polyAbstract tvs $ \abstract ->
              liftM (mapVect abstract) p
      fn   <- hoistVExpr expr
      polyVApply (vVar fn) (mkTyVarTys tvs)

takeHoisted :: VM [(Var, CoreExpr)]
takeHoisted
  = do
      env <- readGEnv id
      setGEnv $ env { global_bindings = [] }
      return $ global_bindings env

mkClosure :: Type -> Type -> Type -> VExpr -> VExpr -> VM VExpr
mkClosure arg_ty res_ty env_ty (vfn,lfn) (venv,lenv)
  = do
      dict <- paDictOfType env_ty
      mkv  <- builtin mkClosureVar
      mkl  <- builtin mkClosurePVar
      return (Var mkv `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, venv],
              Var mkl `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, lenv])

mkClosureApp :: VExpr -> VExpr -> VM VExpr
mkClosureApp (vclo, lclo) (varg, larg)
  = do
      vapply <- builtin applyClosureVar
      lapply <- builtin applyClosurePVar
      return (Var vapply `mkTyApps` [arg_ty, res_ty] `mkApps` [vclo, varg],
              Var lapply `mkTyApps` [arg_ty, res_ty] `mkApps` [lclo, larg])
  where
    (arg_ty, res_ty) = splitClosureTy (exprType vclo)

buildClosures :: [TyVar] -> [VVar] -> [Type] -> Type -> VM VExpr -> VM VExpr
buildClosures tvs vars [] res_ty mk_body
  = mk_body
buildClosures tvs vars [arg_ty] res_ty mk_body
  = buildClosure tvs vars arg_ty res_ty mk_body
buildClosures tvs vars (arg_ty : arg_tys) res_ty mk_body
  = do
      res_ty' <- mkClosureTypes arg_tys res_ty
      arg <- newLocalVVar FSLIT("x") arg_ty
      buildClosure tvs vars arg_ty res_ty'
        . hoistPolyVExpr tvs
        $ do
            lc <- builtin liftingContext
            clo <- buildClosures tvs (vars ++ [arg]) arg_tys res_ty mk_body
            return $ vLams lc (vars ++ [arg]) clo

-- (clo <x1,...,xn> <f,f^>, aclo (Arr lc xs1 ... xsn) <f,f^>)
--   where
--     f  = \env v -> case env of <x1,...,xn> -> e x1 ... xn v
--     f^ = \env v -> case env of Arr l xs1 ... xsn -> e^ l x1 ... xn v
--
buildClosure :: [TyVar] -> [VVar] -> Type -> Type -> VM VExpr -> VM VExpr
buildClosure tvs vars arg_ty res_ty mk_body
  = do
      (env_ty, env, bind) <- buildEnv vars
      env_bndr <- newLocalVVar FSLIT("env") env_ty
      arg_bndr <- newLocalVVar FSLIT("arg") arg_ty

      fn <- hoistPolyVExpr tvs
          $ do
              lc    <- builtin liftingContext
              body  <- mk_body
              body' <- bind (vVar env_bndr)
                            (vVarApps lc body (vars ++ [arg_bndr]))
              return (vLamsWithoutLC [env_bndr, arg_bndr] body')

      mkClosure arg_ty res_ty env_ty fn env

buildEnv :: [VVar] -> VM (Type, VExpr, VExpr -> VExpr -> VM VExpr)
buildEnv vvs
  = do
      lc <- builtin liftingContext
      let (ty, venv, vbind) = mkVectEnv tys vs
      (lenv, lbind) <- mkLiftEnv lc tys ls
      return (ty, (venv, lenv),
              \(venv,lenv) (vbody,lbody) ->
              do
                let vbody' = vbind venv vbody
                lbody' <- lbind lenv lbody
                return (vbody', lbody'))
  where
    (vs,ls) = unzip vvs
    tys     = map idType vs

mkVectEnv :: [Type] -> [Var] -> (Type, CoreExpr, CoreExpr -> CoreExpr -> CoreExpr)
mkVectEnv []   []  = (unitTy, Var unitDataConId, \env body -> body)
mkVectEnv [ty] [v] = (ty, Var v, \env body -> Let (NonRec v env) body)
mkVectEnv tys  vs  = (ty, mkCoreTup (map Var vs),
                        \env body -> Case env (mkWildId ty) (exprType body)
                                       [(DataAlt (tupleCon Boxed (length vs)), vs, body)])
  where
    ty = mkCoreTupTy tys

mkLiftEnv :: Var -> [Type] -> [Var] -> VM (CoreExpr, CoreExpr -> CoreExpr -> VM CoreExpr)
mkLiftEnv lc [ty] [v]
  = return (Var v, \env body ->
                   do
                     len <- lengthPA (Var v)
                     return . Let (NonRec v env)
                            $ Case len lc (exprType body) [(DEFAULT, [], body)])

-- NOTE: this transparently deals with empty environments
mkLiftEnv lc tys vs
  = do
      (env_tc, env_tyargs) <- parrayReprTyCon vty
      let [env_con] = tyConDataCons env_tc
          
          env = Var (dataConWrapId env_con)
                `mkTyApps`  env_tyargs
                `mkVarApps` (lc : vs)

          bind env body = let scrut = unwrapFamInstScrut env_tc env_tyargs env
                          in
                          return $ Case scrut (mkWildId (exprType scrut))
                                        (exprType body)
                                        [(DataAlt env_con, lc : bndrs, body)]
      return (env, bind)
  where
    vty = mkCoreTupTy tys

    bndrs | null vs   = [mkWildId unitTy]
          | otherwise = vs

