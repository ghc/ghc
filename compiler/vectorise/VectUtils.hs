module VectUtils (
  collectAnnTypeBinders, collectAnnTypeArgs, isAnnTypeArg,
  collectAnnValBinders,
  dataConTagZ, mkDataConTag, mkDataConTagLit,

  newLocalVVar,

  mkBuiltinCo,
  mkPADictType, mkPArrayType, mkPReprType,

  parrayReprTyCon, parrayReprDataCon, mkVScrut,
  prDFunOfTyCon,
  paDictArgType, paDictOfType, paDFunType,
  paMethod, mkPR, lengthPA, replicatePA, emptyPA, packPA, combinePA, liftPA,
  polyAbstract, polyApply, polyVApply,
  hoistBinding, hoistExpr, hoistPolyVExpr, takeHoisted,
  buildClosure, buildClosures,
  mkClosureApp
) where

import VectCore
import VectMonad

import MkCore
import CoreSyn
import CoreUtils
import Coercion
import Type
import TypeRep
import TyCon
import DataCon
import Var
import Id                 ( mkWildId )
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
    go bs (_, AnnLam b e) | isIdVar b = go (b:bs) e
    go bs e                           = (reverse bs, e)

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

{-
mkBuiltinTyConApps1 :: (Builtins -> TyCon) -> Type -> [Type] -> VM Type
mkBuiltinTyConApps1 _      dft [] = return dft
mkBuiltinTyConApps1 get_tc _   tys
  = do
      tc <- builtin get_tc
      case tys of
        [] -> pprPanic "mkBuiltinTyConApps1" (ppr tc)
        _  -> return $ foldr1 (mk tc) tys
  where
    mk tc ty1 ty2 = mkTyConApp tc [ty1,ty2]

mkClosureType :: Type -> Type -> VM Type
mkClosureType arg_ty res_ty = mkBuiltinTyConApp closureTyCon [arg_ty, res_ty]
-}

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

mkBuiltinCo :: (Builtins -> TyCon) -> VM Coercion
mkBuiltinCo get_tc
  = do
      tc <- builtin get_tc
      return $ mkTyConApp tc []

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

type PAMethod = (Builtins -> Var, String)

pa_length, pa_replicate, pa_empty, pa_pack :: (Builtins -> Var, String)
pa_length    = (lengthPAVar,    "lengthPA")
pa_replicate = (replicatePAVar, "replicatePA")
pa_empty     = (emptyPAVar,     "emptyPA")
pa_pack      = (packPAVar,      "packPA")

paMethod :: PAMethod -> Type -> VM CoreExpr
paMethod (_method, name) ty
  | Just tycon <- splitPrimTyCon ty
  = liftM Var
  . maybeCantVectoriseM "No PA method" (text name <+> text "for" <+> ppr tycon)
  $ lookupPrimMethod tycon name

paMethod (method, _name) ty
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

lengthPA :: Type -> CoreExpr -> VM CoreExpr
lengthPA ty x = liftM (`App` x) (paMethod pa_length ty)

replicatePA :: CoreExpr -> CoreExpr -> VM CoreExpr
replicatePA len x = liftM (`mkApps` [len,x])
                          (paMethod pa_replicate (exprType x))

emptyPA :: Type -> VM CoreExpr
emptyPA = paMethod pa_empty

packPA :: Type -> CoreExpr -> CoreExpr -> CoreExpr -> VM CoreExpr
packPA ty xs len sel = liftM (`mkApps` [xs, len, sel])
                             (paMethod pa_pack ty)

combinePA :: Type -> CoreExpr -> CoreExpr -> CoreExpr -> [CoreExpr]
          -> VM CoreExpr
combinePA ty len sel is xs
  = liftM (`mkApps` (len : sel : is : xs))
          (paMethod (combinePAVar n, "combine" ++ show n ++ "PA") ty)
  where
    n = length xs

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
      mkv  <- builtin mkClosureVar
      mkl  <- builtin mkClosurePVar
      return (Var mkv `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, venv],
              Var mkl `mkTyApps` [arg_ty, res_ty, env_ty] `mkApps` [dict, vfn, lfn, lenv])

mkClosureApp :: Type -> Type -> VExpr -> VExpr -> VM VExpr
mkClosureApp arg_ty res_ty (vclo, lclo) (varg, larg)
  = do
      vapply <- builtin applyClosureVar
      lapply <- builtin applyClosurePVar
      return (Var vapply `mkTyApps` [arg_ty, res_ty] `mkApps` [vclo, varg],
              Var lapply `mkTyApps` [arg_ty, res_ty] `mkApps` [lclo, larg])

buildClosures :: [TyVar] -> [VVar] -> [Type] -> Type -> VM VExpr -> VM VExpr
buildClosures _   _    [] _ mk_body
  = mk_body
buildClosures tvs vars [arg_ty] res_ty mk_body
  = buildClosure tvs vars arg_ty res_ty mk_body
buildClosures tvs vars (arg_ty : arg_tys) res_ty mk_body
  = do
      res_ty' <- mkClosureTypes arg_tys res_ty
      arg <- newLocalVVar (fsLit "x") arg_ty
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
      env_bndr <- newLocalVVar (fsLit "env") env_ty
      arg_bndr <- newLocalVVar (fsLit "arg") arg_ty

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
    tys     = map varType vs

mkVectEnv :: [Type] -> [Var] -> (Type, CoreExpr, CoreExpr -> CoreExpr -> CoreExpr)
mkVectEnv []   []  = (unitTy, Var unitDataConId, \_ body -> body)
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
                     len <- lengthPA ty (Var v)
                     return . Let (NonRec v env)
                            $ Case len lc (exprType body) [(DEFAULT, [], body)])

-- NOTE: this transparently deals with empty environments
mkLiftEnv lc tys vs
  = do
      (env_tc, env_tyargs) <- parrayReprTyCon vty

      bndrs <- if null vs then do
                                 v <- newDummyVar unitTy
                                 return [v]
                          else return vs
      let [env_con] = tyConDataCons env_tc
          
          env = Var (dataConWrapId env_con)
                `mkTyApps`  env_tyargs
                `mkApps`    (Var lc : args)

          bind env body = let scrut = unwrapFamInstScrut env_tc env_tyargs env
                          in
                          return $ Case scrut (mkWildId (exprType scrut))
                                        (exprType body)
                                        [(DataAlt env_con, lc : bndrs, body)]
      return (env, bind)
  where
    vty = mkCoreTupTy tys

    args  | null vs   = [Var unitDataConId]
          | otherwise = map Var vs

