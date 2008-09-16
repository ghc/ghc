
module Vectorise( vectorise )
where

import VectMonad
import VectUtils
import VectType
import VectCore

import DynFlags
import HscTypes hiding      ( MonadThings(..) )

import Module               ( PackageId )
import CoreLint             ( showPass, endPass )
import CoreSyn
import CoreUtils
import CoreFVs
import SimplMonad           ( SimplCount, zeroSimplCount )
import Rules                ( RuleBase )
import DataCon
import TyCon
import Type
import FamInstEnv           ( extendFamInstEnvList )
import Var
import VarEnv
import VarSet
import Id
import OccName

import DsMonad

import Literal              ( Literal, mkMachInt )
import TysWiredIn

import Outputable
import FastString
import Control.Monad        ( liftM, liftM2, zipWithM )
import Data.List            ( sortBy, unzip4 )

vectorise :: PackageId -> HscEnv -> UniqSupply -> RuleBase -> ModGuts
          -> IO (SimplCount, ModGuts)
vectorise backend hsc_env _ _ guts
  = do
      showPass dflags "Vectorisation"
      eps <- hscEPS hsc_env
      let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps
      Just (info', guts') <- initV backend hsc_env guts info (vectModule guts)
      endPass dflags "Vectorisation" Opt_D_dump_vect (mg_binds guts')
      return (zeroSimplCount dflags, guts' { mg_vect_info = info' })
  where
    dflags = hsc_dflags hsc_env

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
                           lexpr <- liftPA vexpr
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
                            lexpr <- liftPA vexpr
                            return (vexpr, lexpr)

vectLiteral :: Literal -> VM VExpr
vectLiteral lit
  = do
      lexpr <- liftPA (Lit lit)
      return (Lit lit, lexpr)

vectPolyExpr :: CoreExprWithFVs -> VM VExpr
vectPolyExpr (_, AnnNote note expr)
  = liftM (vNote note) $ vectPolyExpr expr
vectPolyExpr expr
  = polyAbstract tvs $ \abstract ->
    do
      mono' <- vectExpr mono
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
      lexpr <- liftPA vexpr
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

vectExpr e@(fvs, AnnLam bndr _)
  | isId bndr = vectLam fvs bs body
  where
    (bs,body) = collectAnnValBinders e

vectExpr e = cantVectorise "Can't vectorise expression" (ppr $ deAnnotate e)

vectLam :: VarSet -> [Var] -> CoreExprWithFVs -> VM VExpr
vectLam fvs bs body
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
            return $ vLams lc vbndrs vbody

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

vectAlgCase tycon _ty_args scrut bndr ty [(DataAlt dc, bndrs, body)]
  = do
      vect_tc    <- maybeV (lookupTyCon tycon)
      (vty, lty) <- vectAndLiftType ty
      vexpr      <- vectExpr scrut
      (vbndr, (vbndrs, vbody)) <- vect_scrut_bndr
                                . vectBndrsIn bndrs
                                $ vectExpr body

      (vscrut, arr_tc, _arg_tys) <- mkVScrut (vVar vbndr)
      vect_dc <- maybeV (lookupDataCon dc)
      let [arr_dc] = tyConDataCons arr_tc
      repr <- mkRepr vect_tc
      shape_bndrs <- arrShapeVars repr
      return . vLet (vNonRec vbndr vexpr)
             $ vCaseProd vscrut vty lty vect_dc arr_dc shape_bndrs vbndrs vbody
  where
    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

vectAlgCase tycon _ty_args scrut bndr ty alts
  = do
      vect_tc     <- maybeV (lookupTyCon tycon)
      (vty, lty)  <- vectAndLiftType ty
      repr        <- mkRepr vect_tc
      shape_bndrs <- arrShapeVars repr
      (len, sel, indices) <- arrSelector repr (map Var shape_bndrs)

      (vbndr, valts) <- vect_scrut_bndr $ mapM (proc_alt sel vty lty) alts'
      let (vect_dcs, vect_bndrss, lift_bndrss, vbodies) = unzip4 valts

      vexpr <- vectExpr scrut
      (vscrut, arr_tc, _arg_tys) <- mkVScrut (vVar vbndr)
      let [arr_dc] = tyConDataCons arr_tc

      let (vect_scrut,  lift_scrut)  = vscrut
          (vect_bodies, lift_bodies) = unzip vbodies

      vdummy <- newDummyVar (exprType vect_scrut)
      ldummy <- newDummyVar (exprType lift_scrut)
      let vect_case = Case vect_scrut vdummy vty
                           (zipWith3 mk_vect_alt vect_dcs vect_bndrss vect_bodies)

      lbody <- combinePA vty len sel indices lift_bodies
      let lift_case = Case lift_scrut ldummy lty
                           [(DataAlt arr_dc, shape_bndrs ++ concat lift_bndrss,
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

    proc_alt sel vty lty (DataAlt dc, bndrs, body)
      = do
          vect_dc <- maybeV (lookupDataCon dc)
          let tag = mkDataConTag vect_dc
              fvs = freeVarsOf body `delVarSetList` bndrs
          (vect_bndrs, lift_bndrs, vbody)
            <- vect_alt_bndrs bndrs
             $ \len -> packLiftingContext len sel tag fvs vty lty
             $ vectExpr body

          return (vect_dc, vect_bndrs, lift_bndrs, vbody)
    proc_alt _ _ _ _ = panic "vectAlgCase/proc_alt"

    vect_alt_bndrs [] p
      = do
          void_tc <- builtin voidTyCon
          let void_ty = mkTyConApp void_tc []
          arr_ty <- mkPArrayType void_ty
          bndr   <- newLocalVar (fsLit "voids") arr_ty
          len    <- lengthPA void_ty (Var bndr)
          e      <- p len
          return ([], [bndr], e)

    vect_alt_bndrs bndrs p
       = localV
       $ do
           vbndrs <- mapM vectBndr bndrs
           let (vect_bndrs, lift_bndrs) = unzip vbndrs
               vv : _ = vect_bndrs
               lv : _ = lift_bndrs
           len <- lengthPA (idType vv) (Var lv)
           e   <- p len
           return (vect_bndrs, lift_bndrs, e)

    mk_vect_alt vect_dc bndrs body = (DataAlt vect_dc, bndrs, body)

packLiftingContext :: CoreExpr -> CoreExpr -> CoreExpr -> VarSet
                   -> Type -> Type -> VM VExpr -> VM VExpr
packLiftingContext len shape tag fvs vty lty p
  = do
      select <- builtin selectPAIntPrimVar
      let sel_expr = mkApps (Var select) [shape, tag]
      sel_var <- newLocalVar (fsLit "sel#") (exprType sel_expr)
      lc_var <- builtin liftingContext
      localV $
        do
          bnds <- mapM (packFreeVar (Var lc_var) (Var sel_var))
                . filter isLocalId
                $ varSetElems fvs
          (vexpr, lexpr) <- p
          empty <- emptyPA vty
          return (vexpr, Let (NonRec sel_var sel_expr)
                         $ Case len lc_var lty
                             [(DEFAULT, [], mkLets (concat bnds) lexpr),
                              (LitAlt (mkMachInt 0), [], empty)])

packFreeVar :: CoreExpr -> CoreExpr -> Var -> VM [CoreBind]
packFreeVar len sel v
  = do
      r <- lookupVar v
      case r of
        Local (vv,lv) ->
          do
            lv' <- cloneVar lv
            expr <- packPA (idType vv) (Var lv) len sel
            updLEnv (upd vv lv')
            return [(NonRec lv' expr)]

        _  -> return []
  where
    upd vv lv' env = env { local_vars = extendVarEnv (local_vars env) v (vv, lv') }

