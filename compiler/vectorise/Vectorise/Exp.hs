
-- | Vectorisation of expressions.
module Vectorise.Exp
	(vectPolyExpr)
where
import VectUtils
import VectType
import Vectorise.Var
import Vectorise.Vect
import Vectorise.Env
import Vectorise.Monad
import Vectorise.Builtins

import CoreSyn
import CoreUtils
import MkCore
import CoreFVs
import DataCon
import TyCon
import Type
import Var
import VarEnv
import VarSet
import Id
import BasicTypes
import Literal
import TysWiredIn
import TysPrim
import Outputable
import FastString
import Control.Monad
import Data.List


-- | Vectorise a polymorphic expression.
vectPolyExpr 
	:: Bool 		-- ^ When vectorising the RHS of a binding, whether that
				--   binding is a loop breaker.
	-> CoreExprWithFVs
	-> VM (Inline, VExpr)

vectPolyExpr loop_breaker (_, AnnNote note expr)
 = do (inline, expr') <- vectPolyExpr loop_breaker expr
      return (inline, vNote note expr')

vectPolyExpr loop_breaker expr
 = do
      arity <- polyArity tvs
      polyAbstract tvs $ \args ->
        do
          (inline, mono') <- vectFnExpr False loop_breaker mono
          return (addInlineArity inline arity,
                  mapVect (mkLams $ tvs ++ args) mono')
  where
    (tvs, mono) = collectAnnTypeBinders expr


-- | Vectorise an expression.
vectExpr :: CoreExprWithFVs -> VM VExpr
vectExpr (_, AnnType ty)
  = liftM vType (vectType ty)

vectExpr (_, AnnVar v) 
  = vectVar v

vectExpr (_, AnnLit lit) 
  = vectLiteral lit

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


-- TODO: Avoid using closure application for dictionaries.
-- vectExpr (_, AnnApp fn arg)
--  | if is application of dictionary 
--    just use regular app instead of closure app.

-- for lifted version. 
--      do liftPD (sub a dNumber)
--      lift the result of the selection, not sub and dNumber seprately. 

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
      vrhs <- localV . inBind bndr . liftM snd $ vectPolyExpr False rhs
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vLet (vNonRec vbndr vrhs) vbody

vectExpr (_, AnnLet (AnnRec bs) body)
  = do
      (vbndrs, (vrhss, vbody)) <- vectBndrsIn bndrs
                                $ liftM2 (,)
                                  (zipWithM vect_rhs bndrs rhss)
                                  (vectExpr body)
      return $ vLet (vRec vbndrs vrhss) vbody
  where
    (bndrs, rhss) = unzip bs

    vect_rhs bndr rhs = localV
                      . inBind bndr
                      . liftM snd
                      $ vectPolyExpr (isLoopBreaker $ idOccInfo bndr) rhs

vectExpr e@(_, AnnLam bndr _)
  | isId bndr = liftM snd $ vectFnExpr True False e
{-
onlyIfV (isEmptyVarSet fvs) (vectScalarLam bs $ deAnnotate body)
                `orElseV` vectLam True fvs bs body
  where
    (bs,body) = collectAnnValBinders e
-}

vectExpr e = cantVectorise "Can't vectorise expression" (ppr $ deAnnotate e)


-- | Vectorise an expression with an outer lambda abstraction.
vectFnExpr 
	:: Bool 		-- ^ When the RHS of a binding, whether that binding should be inlined.
	-> Bool 		-- ^ Whether the binding is a loop breaker.
	-> CoreExprWithFVs 	-- ^ Expression to vectorise. Must have an outer `AnnLam`.
	-> VM (Inline, VExpr)

vectFnExpr inline loop_breaker e@(fvs, AnnLam bndr _)
  | isId bndr = onlyIfV (isEmptyVarSet fvs)
                        (mark DontInline . vectScalarLam bs $ deAnnotate body)
                `orElseV` mark inlineMe (vectLam inline loop_breaker fvs bs body)
  where
    (bs,body) = collectAnnValBinders e

vectFnExpr _ _ e = mark DontInline $ vectExpr e

mark :: Inline -> VM a -> VM (Inline, a)
mark b p = do { x <- p; return (b,x) }


-- | Vectorise a function where are the args have scalar type,
--   that is Int, Float, Double etc.
vectScalarLam 
	:: [Var]	-- ^ Bound variables of function.
	-> CoreExpr	-- ^ Function body.
	-> VM VExpr
	
vectScalarLam args body
 = do scalars <- globalScalars
      onlyIfV (all is_scalar_ty arg_tys
               && is_scalar_ty res_ty
               && is_scalar (extendVarSetList scalars args) body
               && uses scalars body)
        $ do
            fn_var  <- hoistExpr (fsLit "fn") (mkLams args body) DontInline
            zipf    <- zipScalars arg_tys res_ty
            clo     <- scalarClosure arg_tys res_ty (Var fn_var)
                                                (zipf `App` Var fn_var)
            clo_var <- hoistExpr (fsLit "clo") clo DontInline
            lclo    <- liftPD (Var clo_var)
            return (Var clo_var, lclo)
  where
    arg_tys = map idType args
    res_ty  = exprType body

    is_scalar_ty ty 
        | Just (tycon, [])   <- splitTyConApp_maybe ty
        =    tycon == intTyCon
          || tycon == floatTyCon
          || tycon == doubleTyCon

        | otherwise = False

    is_scalar vs (Var v)     = v `elemVarSet` vs
    is_scalar _ e@(Lit _)    = is_scalar_ty $ exprType e
    is_scalar vs (App e1 e2) = is_scalar vs e1 && is_scalar vs e2
    is_scalar _ _            = False

    -- A scalar function has to actually compute something. Without the check,
    -- we would treat (\(x :: Int) -> x) as a scalar function and lift it to
    -- (map (\x -> x)) which is very bad. Normal lifting transforms it to
    -- (\n# x -> x) which is what we want.
    uses funs (Var v)     = v `elemVarSet` funs 
    uses funs (App e1 e2) = uses funs e1 || uses funs e2
    uses _ _              = False


-- | Vectorise a lambda abstraction.
vectLam 
	:: Bool			-- ^ When the RHS of a binding, whether that binding should be inlined.
	-> Bool			-- ^ Whether the binding is a loop breaker.
	-> VarSet		-- ^ The free variables in the body.
	-> [Var]		-- ^ Binding variables.
	-> CoreExprWithFVs	-- ^ Body of abstraction.
	-> VM VExpr

vectLam inline loop_breaker fvs bs body
 = do tyvars    <- localTyVars
      (vs, vvs) <- readLEnv $ \env ->
                   unzip [(var, vv) | var <- varSetElems fvs
                                    , Just vv <- [lookupVarEnv (local_vars env) var]]

      arg_tys   <- mapM (vectType . idType) bs
      res_ty    <- vectType (exprType $ deAnnotate body)

      buildClosures tyvars vvs arg_tys res_ty
        . hoistPolyVExpr tyvars (maybe_inline (length vs + length bs))
        $ do
            lc              <- builtin liftingContext
            (vbndrs, vbody) <- vectBndrsIn (vs ++ bs) (vectExpr body)

            vbody' <- break_loop lc res_ty vbody
            return $ vLams lc vbndrs vbody'
  where
    maybe_inline n | inline    = Inline n
                   | otherwise = DontInline

    break_loop lc ty (ve, le)
      | loop_breaker
      = do
          empty <- emptyPD ty
          lty <- mkPDataType ty
          return (ve, mkWildCase (Var lc) intPrimTy lty
                        [(DEFAULT, [], le),
                         (LitAlt (mkMachInt 0), [], empty)])

      | otherwise = return (ve, le)
 

vectTyAppExpr :: CoreExprWithFVs -> [Type] -> VM VExpr
vectTyAppExpr (_, AnnVar v) tys = vectPolyVar v tys
vectTyAppExpr e tys = cantVectorise "Can't vectorise expression"
                        (ppr $ deAnnotate e `mkTyApps` tys)


-- | Vectorise an algebraic case expression.
--   We convert
--
--   case e :: t of v { ... }
--
-- to
--
--   V:    let v' = e in case v' of _ { ... }
--   L:    let v' = e in case v' `cast` ... of _ { ... }
--
--   When lifting, we have to do it this way because v must have the type
--   [:V(T):] but the scrutinee must be cast to the representation type. We also
--   have to handle the case where v is a wild var correctly.
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

    proc_alt arity sel _ lty (DataAlt dc, bndrs, body)
      = do
          vect_dc <- maybeV (lookupDataCon dc)
          let ntag = dataConTagZ vect_dc
              tag  = mkDataConTag vect_dc
              fvs  = freeVarsOf body `delVarSetList` bndrs

          sel_tags  <- liftM (`App` sel) (builtin (selTags arity))
          lc        <- builtin liftingContext
          elems     <- builtin (selElements arity ntag)

          (vbndrs, vbody)
            <- vectBndrsIn bndrs
             . localV
             $ do
                 binds    <- mapM (pack_var (Var lc) sel_tags tag)
                           . filter isLocalId
                           $ varSetElems fvs
                 (ve, le) <- vectExpr body
                 return (ve, Case (elems `App` sel) lc lty
                             [(DEFAULT, [], (mkLets (concat binds) le))])
                 -- empty    <- emptyPD vty
                 -- return (ve, Case (elems `App` sel) lc lty
                 --             [(DEFAULT, [], Let (NonRec flags_var flags_expr)
                 --                             $ mkLets (concat binds) le),
                 --               (LitAlt (mkMachInt 0), [], empty)])
          let (vect_bndrs, lift_bndrs) = unzip vbndrs
          return (vect_dc, vect_bndrs, lift_bndrs, vbody)

    proc_alt _ _ _ _ _ = panic "vectAlgCase/proc_alt"

    mk_vect_alt vect_dc bndrs body = (DataAlt vect_dc, bndrs, body)

    pack_var len tags t v
      = do
          r <- lookupVar v
          case r of
            Local (vv, lv) ->
              do
                lv'  <- cloneVar lv
                expr <- packByTagPD (idType vv) (Var lv) len tags t
                updLEnv (\env -> env { local_vars = extendVarEnv
                                                (local_vars env) v (vv, lv') })
                return [(NonRec lv' expr)]

            _ -> return []

