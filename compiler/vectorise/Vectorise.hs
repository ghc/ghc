
module Vectorise( vectorise )
where

import VectMonad
import VectUtils
import VectType
import VectCore

import HscTypes hiding      ( MonadThings(..) )

import Module               ( PackageId )
import CoreSyn
import CoreUtils
import MkCore               ( mkWildCase )
import CoreFVs
import CoreMonad            ( CoreM, getHscEnv )
import DataCon
import TyCon
import Type
import FamInstEnv           ( extendFamInstEnvList )
import Var
import VarEnv
import VarSet
import Id
import OccName

import Literal              ( Literal, mkMachInt )
import TysWiredIn

import Outputable
import FastString
import Control.Monad        ( liftM, liftM2, zipWithM )
import Data.List            ( sortBy, unzip4 )

vectorise :: PackageId -> ModGuts -> CoreM ModGuts
vectorise backend guts = do
    hsc_env <- getHscEnv
    liftIO $ vectoriseIO backend hsc_env guts

vectoriseIO :: PackageId -> HscEnv -> ModGuts -> IO ModGuts
vectoriseIO backend hsc_env guts
  = do
      eps <- hscEPS hsc_env
      let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps
      Just (info', guts') <- initV backend hsc_env guts info (vectModule guts)
      return (guts' { mg_vect_info = info' })

vectModule :: ModGuts -> VM ModGuts
vectModule guts
  = do
      (types', fam_insts, tc_binds) <- vectTypeEnv (mg_types guts)

      let fam_inst_env' = extendFamInstEnvList (mg_fam_inst_env guts) fam_insts
      updGEnv (setFamInstEnv fam_inst_env')

      -- dicts   <- mapM buildPADict pa_insts
      -- workers <- mapM vectDataConWorkers pa_insts
      binds'  <- mapM vectTopBind (mg_binds guts)
      return $ guts { mg_types        = types'
                    , mg_binds        = Rec tc_binds : binds'
                    , mg_fam_inst_env = fam_inst_env'
                    , mg_fam_insts    = mg_fam_insts guts ++ fam_insts
                    }

vectTopBind :: CoreBind -> VM CoreBind
vectTopBind b@(NonRec var expr)
  = do
      var'  <- vectTopBinder var
      expr' <- vectTopRhs var expr
      hs    <- takeHoisted
      cexpr <- tryConvert var var' expr
      return . Rec $ (var, cexpr) : (var', expr') : hs
  `orElseV`
    return b

vectTopBind b@(Rec bs)
  = do
      vars'  <- mapM vectTopBinder vars
      exprs' <- zipWithM vectTopRhs vars exprs
      hs     <- takeHoisted
      cexprs <- sequence $ zipWith3 tryConvert vars vars' exprs
      return . Rec $ zip vars cexprs ++ zip vars' exprs' ++ hs
  `orElseV`
    return b
  where
    (vars, exprs) = unzip bs

vectTopBinder :: Var -> VM Var
vectTopBinder var
  = do
      vty  <- vectType (idType var)
      var' <- cloneId mkVectOcc var vty
      defGlobalVar var var'
      return var'

vectTopRhs :: Var -> CoreExpr -> VM CoreExpr
vectTopRhs var expr
  = do
      closedV . liftM vectorised
              . inBind var
              $ vectPolyExpr (freeVars expr)

tryConvert :: Var -> Var -> CoreExpr -> VM CoreExpr
tryConvert var vect_var rhs
  = fromVect (idType var) (Var vect_var) `orElseV` return rhs

-- ----------------------------------------------------------------------------
-- Bindings

vectBndr :: Var -> VM VVar
vectBndr v
  = do
      (vty, lty) <- vectAndLiftType (idType v)
      let vv = v `Id.setIdType` vty
          lv = v `Id.setIdType` lty
      updLEnv (mapTo vv lv)
      return (vv, lv)
  where
    mapTo vv lv env = env { local_vars = extendVarEnv (local_vars env) v (vv, lv) }

vectBndrNew :: Var -> FastString -> VM VVar
vectBndrNew v fs
  = do
      vty <- vectType (idType v)
      vv  <- newLocalVVar fs vty
      updLEnv (upd vv)
      return vv
  where
    upd vv env = env { local_vars = extendVarEnv (local_vars env) v vv }

vectBndrIn :: Var -> VM a -> VM (VVar, a)
vectBndrIn v p
  = localV
  $ do
      vv <- vectBndr v
      x <- p
      return (vv, x)

vectBndrNewIn :: Var -> FastString -> VM a -> VM (VVar, a)
vectBndrNewIn v fs p
  = localV
  $ do
      vv <- vectBndrNew v fs
      x  <- p
      return (vv, x)

vectBndrsIn :: [Var] -> VM a -> VM ([VVar], a)
vectBndrsIn vs p
  = localV
  $ do
      vvs <- mapM vectBndr vs
      x <- p
      return (vvs, x)

-- ----------------------------------------------------------------------------
-- Expressions

vectVar :: Var -> VM VExpr
vectVar v
  = do
      r <- lookupVar v
      case r of
        Local (vv,lv) -> return (Var vv, Var lv)
        Global vv     -> do
                           let vexpr = Var vv
                           lexpr <- liftPD vexpr
                           return (vexpr, lexpr)

vectPolyVar :: Var -> [Type] -> VM VExpr
vectPolyVar v tys
  = do
      vtys <- mapM vectType tys
      r <- lookupVar v
      case r of
        Local (vv, lv) -> liftM2 (,) (polyApply (Var vv) vtys)
                                     (polyApply (Var lv) vtys)
        Global poly    -> do
                            vexpr <- polyApply (Var poly) vtys
                            lexpr <- liftPD vexpr
                            return (vexpr, lexpr)

vectLiteral :: Literal -> VM VExpr
vectLiteral lit
  = do
      lexpr <- liftPD (Lit lit)
      return (Lit lit, lexpr)

vectPolyExpr :: CoreExprWithFVs -> VM VExpr
vectPolyExpr (_, AnnNote note expr)
  = liftM (vNote note) $ vectPolyExpr expr
vectPolyExpr expr
  = polyAbstract tvs $ \abstract ->
    do
      mono' <- vectFnExpr False mono
      return $ mapVect abstract mono'
  where
    (tvs, mono) = collectAnnTypeBinders expr

vectExpr :: CoreExprWithFVs -> VM VExpr
vectExpr (_, AnnType ty)
  = liftM vType (vectType ty)

vectExpr (_, AnnVar v) = vectVar v

vectExpr (_, AnnLit lit) = vectLiteral lit

vectExpr (_, AnnNote note expr)
  = liftM (vNote note) (vectExpr expr)

vectExpr e@(_, AnnApp _ arg)
  | isAnnTypeArg arg
  = vectTyAppExpr fn tys
  where
    (fn, tys) = collectAnnTypeArgs e

vectExpr (_, AnnApp (_, AnnVar v) (_, AnnLit lit))
  | Just con <- isDataConId_maybe v
  , is_special_con con
  = do
      let vexpr = App (Var v) (Lit lit)
      lexpr <- liftPD vexpr
      return (vexpr, lexpr)
  where
    is_special_con con = con `elem` [intDataCon, floatDataCon, doubleDataCon]


vectExpr (_, AnnApp fn arg)
  = do
      arg_ty' <- vectType arg_ty
      res_ty' <- vectType res_ty
      fn'     <- vectExpr fn
      arg'    <- vectExpr arg
      mkClosureApp arg_ty' res_ty' fn' arg'
  where
    (arg_ty, res_ty) = splitFunTy . exprType $ deAnnotate fn

vectExpr (_, AnnCase scrut bndr ty alts)
  | Just (tycon, ty_args) <- splitTyConApp_maybe scrut_ty
  , isAlgTyCon tycon
  = vectAlgCase tycon ty_args scrut bndr ty alts
  where
    scrut_ty = exprType (deAnnotate scrut)

vectExpr (_, AnnLet (AnnNonRec bndr rhs) body)
  = do
      vrhs <- localV . inBind bndr $ vectPolyExpr rhs
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vLet (vNonRec vbndr vrhs) vbody

vectExpr (_, AnnLet (AnnRec bs) body)
  = do
      (vbndrs, (vrhss, vbody)) <- vectBndrsIn bndrs
                                $ liftM2 (,)
                                  (zipWithM vect_rhs bndrs rhss)
                                  (vectPolyExpr body)
      return $ vLet (vRec vbndrs vrhss) vbody
  where
    (bndrs, rhss) = unzip bs

    vect_rhs bndr rhs = localV
                      . inBind bndr
                      $ vectExpr rhs

vectExpr e@(_, AnnLam bndr _)
  | isId bndr = vectFnExpr True e
{-
onlyIfV (isEmptyVarSet fvs) (vectScalarLam bs $ deAnnotate body)
                `orElseV` vectLam True fvs bs body
  where
    (bs,body) = collectAnnValBinders e
-}

vectExpr e = cantVectorise "Can't vectorise expression" (ppr $ deAnnotate e)

vectFnExpr :: Bool -> CoreExprWithFVs -> VM VExpr
vectFnExpr inline e@(fvs, AnnLam bndr _)
  | isId bndr = onlyIfV (isEmptyVarSet fvs) (vectScalarLam bs $ deAnnotate body)
                `orElseV` vectLam inline fvs bs body
  where
    (bs,body) = collectAnnValBinders e
vectFnExpr _ e = vectExpr e


vectScalarLam :: [Var] -> CoreExpr -> VM VExpr
vectScalarLam args body
  = do
      scalars <- globalScalars
      onlyIfV (all is_scalar_ty arg_tys
               && is_scalar_ty res_ty
               && is_scalar (extendVarSetList scalars args) body)
        $ do
            fn_var <- hoistExpr (fsLit "fn") (mkLams args body)
            zipf <- zipScalars arg_tys res_ty
            clo <- scalarClosure arg_tys res_ty (Var fn_var)
                                                (zipf `App` Var fn_var)
            clo_var <- hoistExpr (fsLit "clo") clo
            lclo <- liftPD (Var clo_var)
            return (Var clo_var, lclo)
  where
    arg_tys = map idType args
    res_ty  = exprType body

    is_scalar_ty ty | Just (tycon, []) <- splitTyConApp_maybe ty
                    = tycon == intTyCon
                      || tycon == floatTyCon
                      || tycon == doubleTyCon

                    | otherwise = False

    is_scalar vs (Var v)     = v `elemVarSet` vs
    is_scalar _ e@(Lit _)    = is_scalar_ty $ exprType e
    is_scalar vs (App e1 e2) = is_scalar vs e1 && is_scalar vs e2
    is_scalar _ _            = False

vectLam :: Bool -> VarSet -> [Var] -> CoreExprWithFVs -> VM VExpr
vectLam inline fvs bs body
  = do
      tyvars <- localTyVars
      (vs, vvs) <- readLEnv $ \env ->
                   unzip [(var, vv) | var <- varSetElems fvs
                                    , Just vv <- [lookupVarEnv (local_vars env) var]]

      arg_tys <- mapM (vectType . idType) bs
      res_ty  <- vectType (exprType $ deAnnotate body)

      buildClosures tyvars vvs arg_tys res_ty
        . hoistPolyVExpr tyvars
        $ do
            lc <- builtin liftingContext
            (vbndrs, vbody) <- vectBndrsIn (vs ++ bs)
                                           (vectExpr body)
            return . maybe_inline $ vLams lc vbndrs vbody
  where
    maybe_inline = if inline then vInlineMe else id

vectTyAppExpr :: CoreExprWithFVs -> [Type] -> VM VExpr
vectTyAppExpr (_, AnnVar v) tys = vectPolyVar v tys
vectTyAppExpr e tys = cantVectorise "Can't vectorise expression"
                        (ppr $ deAnnotate e `mkTyApps` tys)

-- We convert
--
--   case e :: t of v { ... }
--
-- to
--
--   V:    let v' = e in case v' of _ { ... }
--   L:    let v' = e in case v' `cast` ... of _ { ... }
--
-- When lifting, we have to do it this way because v must have the type
-- [:V(T):] but the scrutinee must be cast to the representation type. We also
-- have to handle the case where v is a wild var correctly.
--

-- FIXME: this is too lazy
vectAlgCase :: TyCon -> [Type] -> CoreExprWithFVs -> Var -> Type
            -> [(AltCon, [Var], CoreExprWithFVs)]
            -> VM VExpr
vectAlgCase _tycon _ty_args scrut bndr ty [(DEFAULT, [], body)]
  = do
      vscrut         <- vectExpr scrut
      (vty, lty)     <- vectAndLiftType ty
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vCaseDEFAULT vscrut vbndr vty lty vbody

vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt _, [], body)]
  = do
      vscrut         <- vectExpr scrut
      (vty, lty)     <- vectAndLiftType ty
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vCaseDEFAULT vscrut vbndr vty lty vbody

vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt dc, bndrs, body)]
  = do
      (vty, lty) <- vectAndLiftType ty
      vexpr      <- vectExpr scrut
      (vbndr, (vbndrs, (vect_body, lift_body)))
         <- vect_scrut_bndr
          . vectBndrsIn bndrs
          $ vectExpr body
      let (vect_bndrs, lift_bndrs) = unzip vbndrs
      (vscrut, lscrut, pdata_tc, _arg_tys) <- mkVScrut (vVar vbndr)
      vect_dc <- maybeV (lookupDataCon dc)
      let [pdata_dc] = tyConDataCons pdata_tc

      let vcase = mk_wild_case vscrut vty vect_dc  vect_bndrs vect_body
          lcase = mk_wild_case lscrut lty pdata_dc lift_bndrs lift_body

      return $ vLet (vNonRec vbndr vexpr) (vcase, lcase)
  where
    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

    mk_wild_case expr ty dc bndrs body
      = mkWildCase expr (exprType expr) ty [(DataAlt dc, bndrs, body)]

vectAlgCase tycon _ty_args scrut bndr ty alts
  = do
      vect_tc     <- maybeV (lookupTyCon tycon)
      (vty, lty)  <- vectAndLiftType ty

      let arity = length (tyConDataCons vect_tc)
      sel_ty <- builtin (selTy arity)
      sel_bndr <- newLocalVar (fsLit "sel") sel_ty
      let sel = Var sel_bndr

      (vbndr, valts) <- vect_scrut_bndr
                      $ mapM (proc_alt arity sel vty lty) alts'
      let (vect_dcs, vect_bndrss, lift_bndrss, vbodies) = unzip4 valts

      vexpr <- vectExpr scrut
      (vect_scrut, lift_scrut, pdata_tc, _arg_tys) <- mkVScrut (vVar vbndr)
      let [pdata_dc] = tyConDataCons pdata_tc

      let (vect_bodies, lift_bodies) = unzip vbodies

      vdummy <- newDummyVar (exprType vect_scrut)
      ldummy <- newDummyVar (exprType lift_scrut)
      let vect_case = Case vect_scrut vdummy vty
                           (zipWith3 mk_vect_alt vect_dcs vect_bndrss vect_bodies)

      lc <- builtin liftingContext
      lbody <- combinePD vty (Var lc) sel lift_bodies
      let lift_case = Case lift_scrut ldummy lty
                           [(DataAlt pdata_dc, sel_bndr : concat lift_bndrss,
                             lbody)]

      return . vLet (vNonRec vbndr vexpr)
             $ (vect_case, lift_case)
  where
    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

    alts' = sortBy (\(alt1, _, _) (alt2, _, _) -> cmp alt1 alt2) alts

    cmp (DataAlt dc1) (DataAlt dc2) = dataConTag dc1 `compare` dataConTag dc2
    cmp DEFAULT       DEFAULT       = EQ
    cmp DEFAULT       _             = LT
    cmp _             DEFAULT       = GT
    cmp _             _             = panic "vectAlgCase/cmp"

    proc_alt arity sel vty lty (DataAlt dc, bndrs, body)
      = do
          vect_dc <- maybeV (lookupDataCon dc)
          let ntag = dataConTagZ vect_dc
              tag  = mkDataConTag vect_dc
              fvs  = freeVarsOf body `delVarSetList` bndrs

          pick <- builtin (selPick arity)
          let flags_expr = mkApps pick [sel, tag]
          flags_var <- newLocalVar (fsLit "flags") (exprType flags_expr)
          lc        <- builtin liftingContext
          elems     <- builtin (selElements arity ntag)

          (vbndrs, vbody)
            <- vectBndrsIn bndrs
             . localV
             $ do
                 binds    <- mapM (pack_var (Var lc) (Var flags_var))
                           . filter isLocalId
                           $ varSetElems fvs
                 (ve, le) <- vectExpr body
                 empty    <- emptyPD vty
                 return (ve, Case (elems `App` sel) lc lty
                               [(DEFAULT, [], Let (NonRec flags_var flags_expr)
                                              $ mkLets (concat binds) le),
                                (LitAlt (mkMachInt 0), [], empty)])
          let (vect_bndrs, lift_bndrs) = unzip vbndrs
          return (vect_dc, vect_bndrs, lift_bndrs, vbody)

    proc_alt _ _ _ _ _ = panic "vectAlgCase/proc_alt"

    mk_vect_alt vect_dc bndrs body = (DataAlt vect_dc, bndrs, body)

    pack_var len flags v
      = do
          r <- lookupVar v
          case r of
            Local (vv, lv) ->
              do
                lv'  <- cloneVar lv
                expr <- packPD (idType vv) (Var lv) len flags
                updLEnv (\env -> env { local_vars = extendVarEnv
                                                (local_vars env) v (vv, lv') })
                return [(NonRec lv' expr)]

            _ -> return []

