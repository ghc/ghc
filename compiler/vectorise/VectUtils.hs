module VectUtils (
  collectAnnTypeBinders, collectAnnTypeArgs, isAnnTypeArg,
  collectAnnValBinders,
  dataConTagZ, mkDataConTag, mkDataConTagLit,

  newLocalVVar,

  mkBuiltinCo, voidType,
  mkPADictType, mkPArrayType, mkPDataType, mkPReprType, mkPArray,

  pdataReprTyCon, pdataReprDataCon, mkVScrut,
  prDFunOfTyCon,
  paDictArgType, paDictOfType, paDFunType,
  paMethod, mkPR, replicatePD, emptyPD, packPD,
  combinePD,
  liftPD,
  zipScalars, scalarClosure,
  polyAbstract, polyApply, polyVApply,
  hoistBinding, hoistExpr, hoistPolyVExpr, takeHoisted,
  buildClosure, buildClosures,
  mkClosureApp
) where

import VectCore
import VectMonad

import MkCore ( mkCoreTup, mkCoreTupTy, mkWildCase )
import CoreSyn
import CoreUtils
import Coercion
import Type
import TypeRep
import TyCon
import DataCon
import Var
import MkId               ( unwrapFamInstScrut )
import TysWiredIn
import BasicTypes         ( Boxity(..) )
import Literal            ( Literal, mkMachInt )

import Outputable
import FastString

import Control.Monad


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
isAnnTypeArg (_, AnnType _) = True
isAnnTypeArg _              = False

dataConTagZ :: DataCon -> Int
dataConTagZ con = dataConTag con - fIRST_TAG

mkDataConTagLit :: DataCon -> Literal
mkDataConTagLit = mkMachInt . toInteger . dataConTagZ

mkDataConTag :: DataCon -> CoreExpr
mkDataConTag = mkIntLitInt . dataConTagZ

splitPrimTyCon :: Type -> Maybe TyCon
splitPrimTyCon ty
  | Just (tycon, []) <- splitTyConApp_maybe ty
  , isPrimTyCon tycon
  = Just tycon

  | otherwise = Nothing

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

voidType :: VM Type
voidType = mkBuiltinTyConApp voidTyCon []

mkClosureTypes :: [Type] -> Type -> VM Type
mkClosureTypes = mkBuiltinTyConApps closureTyCon

mkPReprType :: Type -> VM Type
mkPReprType ty = mkBuiltinTyConApp preprTyCon [ty]

mkPADictType :: Type -> VM Type
mkPADictType ty = mkBuiltinTyConApp paTyCon [ty]

mkPArrayType :: Type -> VM Type
mkPArrayType ty
  | Just tycon <- splitPrimTyCon ty
  = do
      r <- lookupPrimPArray tycon
      case r of
        Just arr -> return $ mkTyConApp arr []
        Nothing  -> cantVectorise "Primitive tycon not vectorised" (ppr tycon)
mkPArrayType ty = mkBuiltinTyConApp parrayTyCon [ty]

mkPDataType :: Type -> VM Type
mkPDataType ty = mkBuiltinTyConApp pdataTyCon [ty]

mkPArray :: Type -> CoreExpr -> CoreExpr -> VM CoreExpr
mkPArray ty len dat = do
                        tc <- builtin parrayTyCon
                        let [dc] = tyConDataCons tc
                        return $ mkConApp dc [Type ty, len, dat]

mkBuiltinCo :: (Builtins -> TyCon) -> VM Coercion
mkBuiltinCo get_tc
  = do
      tc <- builtin get_tc
      return $ mkTyConApp tc []

pdataReprTyCon :: Type -> VM (TyCon, [Type])
pdataReprTyCon ty = builtin pdataTyCon >>= (`lookupFamInst` [ty])

pdataReprDataCon :: Type -> VM (DataCon, [Type])
pdataReprDataCon ty
  = do
      (tc, arg_tys) <- pdataReprTyCon ty
      let [dc] = tyConDataCons tc
      return (dc, arg_tys)

mkVScrut :: VExpr -> VM (CoreExpr, CoreExpr, TyCon, [Type])
mkVScrut (ve, le)
  = do
      (tc, arg_tys) <- pdataReprTyCon ty
      return (ve, unwrapFamInstScrut tc arg_tys le, tc, arg_tys)
  where
    ty = exprType ve

prDFunOfTyCon :: TyCon -> VM CoreExpr
prDFunOfTyCon tycon
  = liftM Var
  . maybeCantVectoriseM "No PR dictionary for tycon" (ppr tycon)
  $ lookupTyConPR tycon

paDictArgType :: TyVar -> VM (Maybe Type)
paDictArgType tv = go (TyVarTy tv) (tyVarKind tv)
  where
    go ty k | Just k' <- kindView k = go ty k'
    go ty (FunTy k1 k2)
      = do
          tv   <- newTyVar (fsLit "a") k1
          mty1 <- go (TyVarTy tv) k1
          case mty1 of
            Just ty1 -> do
                          mty2 <- go (AppTy ty (TyVarTy tv)) k2
                          return $ fmap (ForAllTy tv . FunTy ty1) mty2
            Nothing  -> go ty k2

    go ty k
      | isLiftedTypeKind k
      = liftM Just (mkPADictType ty)

    go _ _ = return Nothing

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
      dfun <- maybeCantVectoriseM "No PA dictionary for tycon" (ppr tc)
            $ lookupTyConPA tc
      paDFunApply (Var dfun) ty_args
paDictOfTyApp ty _
  = cantVectorise "Can't construct PA dictionary for type" (ppr ty)

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

paMethod :: (Builtins -> Var) -> String -> Type -> VM CoreExpr
paMethod _ name ty
  | Just tycon <- splitPrimTyCon ty
  = liftM Var
  . maybeCantVectoriseM "No PA method" (text name <+> text "for" <+> ppr tycon)
  $ lookupPrimMethod tycon name

paMethod method _ ty
  = do
      fn   <- builtin method
      dict <- paDictOfType ty
      return $ mkApps (Var fn) [Type ty, dict]

mkPR :: Type -> VM CoreExpr
mkPR ty
  = do
      fn   <- builtin mkPRVar
      dict <- paDictOfType ty
      return $ mkApps (Var fn) [Type ty, dict]

replicatePD :: CoreExpr -> CoreExpr -> VM CoreExpr
replicatePD len x = liftM (`mkApps` [len,x])
                          (paMethod replicatePDVar "replicatePD" (exprType x))

emptyPD :: Type -> VM CoreExpr
emptyPD = paMethod emptyPDVar "emptyPD"

packPD :: Type -> CoreExpr -> CoreExpr -> CoreExpr -> VM CoreExpr
packPD ty xs len sel = liftM (`mkApps` [xs, len, sel])
                             (paMethod packPDVar "packPD" ty)

combinePD :: Type -> CoreExpr -> CoreExpr -> [CoreExpr]
          -> VM CoreExpr
combinePD ty len sel xs
  = liftM (`mkApps` (len : sel : xs))
          (paMethod (combinePDVar n) ("combine" ++ show n ++ "PD") ty)
  where
    n = length xs

liftPD :: CoreExpr -> VM CoreExpr
liftPD x
  = do
      lc <- builtin liftingContext
      replicatePD (Var lc) x

zipScalars :: [Type] -> Type -> VM CoreExpr
zipScalars arg_tys res_ty
  = do
      scalar <- builtin scalarClass
      (dfuns, _) <- mapAndUnzipM (\ty -> lookupInst scalar [ty]) ty_args
      zipf <- builtin (scalarZip $ length arg_tys)
      return $ Var zipf `mkTyApps` ty_args `mkApps` map Var dfuns
    where
      ty_args = arg_tys ++ [res_ty]

scalarClosure :: [Type] -> Type -> CoreExpr -> CoreExpr -> VM CoreExpr
scalarClosure arg_tys res_ty scalar_fun array_fun
  = do
      ctr <- builtin (closureCtrFun $ length arg_tys)
      pas <- mapM paDictOfType (init arg_tys)
      return $ Var ctr `mkTyApps` (arg_tys ++ [res_ty])
                       `mkApps`   (pas ++ [scalar_fun, array_fun])

newLocalVVar :: FastString -> Type -> VM VVar
newLocalVVar fs vty
  = do
      lty <- mkPDataType vty
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
                         Just ty -> liftM Just (newLocalVar (fsLit "dPA") ty)
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

{-
boxExpr :: Type -> VExpr -> VM VExpr
boxExpr ty (vexpr, lexpr)
  | Just (tycon, []) <- splitTyConApp_maybe ty
  , isUnLiftedTyCon tycon
  = do
      r <- lookupBoxedTyCon tycon
      case r of
        Just tycon' -> let [dc] = tyConDataCons tycon'
                       in
                       return (mkConApp dc [vexpr], lexpr)
        Nothing     -> return (vexpr, lexpr)
-}

mkClosure :: Type -> Type -> Type -> VExpr -> VExpr -> VM VExpr
mkClosure arg_ty res_ty env_ty (vfn,lfn) (venv,lenv)
  = do
      dict <- paDictOfType env_ty
      mkv  <- builtin closureVar
      mkl  <- builtin liftedClosureVar
      return (Var mkv `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, venv],
              Var mkl `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, lenv])

mkClosureApp :: Type -> Type -> VExpr -> VExpr -> VM VExpr
mkClosureApp arg_ty res_ty (vclo, lclo) (varg, larg)
  = do
      vapply <- builtin applyVar
      lapply <- builtin liftedApplyVar
      lc     <- builtin liftingContext
      return (Var vapply `mkTyApps` [arg_ty, res_ty] `mkApps` [vclo, varg],
              Var lapply `mkTyApps` [arg_ty, res_ty] `mkApps` [Var lc, lclo, larg])

buildClosures :: [TyVar] -> [VVar] -> [Type] -> Type -> VM VExpr -> VM VExpr
buildClosures _   _    [] _ mk_body
  = mk_body
buildClosures tvs vars [arg_ty] res_ty mk_body
  = liftM vInlineMe (buildClosure tvs vars arg_ty res_ty mk_body)
buildClosures tvs vars (arg_ty : arg_tys) res_ty mk_body
  = do
      res_ty' <- mkClosureTypes arg_tys res_ty
      arg <- newLocalVVar (fsLit "x") arg_ty
      liftM vInlineMe
        . buildClosure tvs vars arg_ty res_ty'
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
      env_bndr <- newLocalVVar (fsLit "env") env_ty
      arg_bndr <- newLocalVVar (fsLit "arg") arg_ty

      fn <- hoistPolyVExpr tvs
          $ do
              lc    <- builtin liftingContext
              body  <- mk_body
              return . vInlineMe
                     . vLams lc [env_bndr, arg_bndr]
                     $ bind (vVar env_bndr)
                            (vVarApps lc body (vars ++ [arg_bndr]))

      mkClosure arg_ty res_ty env_ty fn env

buildEnv :: [VVar] -> VM (Type, VExpr, VExpr -> VExpr -> VExpr)
buildEnv [] = do
             ty    <- voidType
             void  <- builtin voidVar
             pvoid <- builtin pvoidVar
             return (ty, vVar (void, pvoid), \_ body -> body)

buildEnv [v] = return (vVarType v, vVar v,
                    \env body -> vLet (vNonRec v env) body)

buildEnv vs
  = do
      
      (lenv_tc, lenv_tyargs) <- pdataReprTyCon ty

      let venv_con   = tupleCon Boxed (length vs) 
          [lenv_con] = tyConDataCons lenv_tc

          venv       = mkCoreTup (map Var vvs)
          lenv       = Var (dataConWrapId lenv_con)
                       `mkTyApps` lenv_tyargs
                       `mkApps`   map Var lvs

          vbind env body = mkWildCase env ty (exprType body)
                           [(DataAlt venv_con, vvs, body)]

          lbind env body =
            let scrut = unwrapFamInstScrut lenv_tc lenv_tyargs env
            in
            mkWildCase scrut (exprType scrut) (exprType body)
              [(DataAlt lenv_con, lvs, body)]

          bind (venv, lenv) (vbody, lbody) = (vbind venv vbody,
                                              lbind lenv lbody)

      return (ty, (venv, lenv), bind)
  where
    (vvs, lvs) = unzip vs
    tys        = map vVarType vs
    ty         = mkCoreTupTy tys

