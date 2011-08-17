
-- | Vectorisation of expressions.
module Vectorise.Exp (

  -- Vectorise a polymorphic expression
  vectPolyExpr, 
  
  -- Vectorise a scalar expression of functional type
  vectScalarFun
) where

#include "HsVersions.h"

import Vectorise.Type.Type
import Vectorise.Var
import Vectorise.Vect
import Vectorise.Env
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Utils

import CoreSyn
import CoreUtils
import MkCore
import CoreFVs
import DataCon
import TyCon
import Type
import NameSet
import Var
import VarEnv
import VarSet
import Id
import BasicTypes( isStrongLoopBreaker )
import Literal
import TysWiredIn
import TysPrim
import Outputable
import FastString
import Control.Monad
import Data.List


-- | Vectorise a polymorphic expression.
--
vectPolyExpr :: Bool            -- ^ When vectorising the RHS of a binding, whether that
                                --   binding is a loop breaker.
             -> [Var]                     
             -> CoreExprWithFVs
             -> VM (Inline, Bool, VExpr)
vectPolyExpr loop_breaker recFns (_, AnnNote note expr)
 = do (inline, isScalarFn, expr') <- vectPolyExpr loop_breaker recFns expr
      return (inline, isScalarFn, vNote note expr')
vectPolyExpr loop_breaker recFns expr
 = do
      arity <- polyArity tvs
      polyAbstract tvs $ \args ->
        do
          (inline, isScalarFn, mono') <- vectFnExpr False loop_breaker recFns mono
          return (addInlineArity inline arity, isScalarFn, 
                  mapVect (mkLams $ tvs ++ args) mono')
  where
    (tvs, mono) = collectAnnTypeBinders expr


-- |Vectorise an expression.
--
vectExpr :: CoreExprWithFVs -> VM VExpr
vectExpr (_, AnnType ty)
  = liftM vType (vectType ty)

vectExpr (_, AnnVar v) 
  = vectVar v

vectExpr (_, AnnLit lit) 
  = vectLiteral lit

vectExpr (_, AnnNote note expr)
  = liftM (vNote note) (vectExpr expr)

-- SPECIAL CASE: Vectorise/lift 'patError @ ty err' by only vectorising/lifting the type 'ty';
--   its only purpose is to abort the program, but we need to adjust the type to keep CoreLint
--   happy.
vectExpr (_, AnnApp (_, AnnApp (_, AnnVar v) (_, AnnType ty)) err)
  | v == pAT_ERROR_ID
  = do { (vty, lty) <- vectAndLiftType ty
       ; return (mkCoreApps (Var v) [Type vty, err'], mkCoreApps (Var v) [Type lty, err'])
       }
  where
    err' = deAnnotate err

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
  | otherwise = cantVectorise "Can't vectorise expression" (ppr scrut_ty) 
  where
    scrut_ty = exprType (deAnnotate scrut)

vectExpr (_, AnnLet (AnnNonRec bndr rhs) body)
  = do
      vrhs <- localV . inBind bndr . liftM (\(_,_,z)->z) $ vectPolyExpr False [] rhs
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
                      . liftM (\(_,_,z)->z)
                      $ vectPolyExpr (isStrongLoopBreaker $ idOccInfo bndr) [] rhs

vectExpr e@(_, AnnLam bndr _)
  | isId bndr = liftM (\(_,_,z) ->z) $ vectFnExpr True False [] e
{-
onlyIfV (isEmptyVarSet fvs) (vectScalarLam bs $ deAnnotate body)
                `orElseV` vectLam True fvs bs body
  where
    (bs,body) = collectAnnValBinders e
-}

vectExpr e = cantVectorise "Can't vectorise expression (vectExpr)" (ppr $ deAnnotate e)

-- | Vectorise an expression with an outer lambda abstraction.
--
vectFnExpr :: Bool             -- ^ If we process the RHS of a binding, whether that binding should
                               --   be inlined
           -> Bool             -- ^ Whether the binding is a loop breaker
           -> [Var]            -- ^ Names of function in same recursive binding group
           -> CoreExprWithFVs  -- ^ Expression to vectorise; must have an outer `AnnLam`
           -> VM (Inline, Bool, VExpr)
vectFnExpr inline loop_breaker recFns expr@(_fvs, AnnLam bndr _)
  | isId bndr = mark DontInline True (vectScalarFun False recFns (deAnnotate expr))
                `orElseV` 
                mark inlineMe False (vectLam inline loop_breaker expr)
vectFnExpr _ _ _  e = mark DontInline False $ vectExpr e

mark :: Inline -> Bool -> VM a -> VM (Inline, Bool, a)
mark b isScalarFn p = do { x <- p; return (b, isScalarFn, x) }

-- |Vectorise an expression of functional type, where all arguments and the result are of scalar
-- type (i.e., 'Int', 'Float', 'Double' etc.) and which does not contain any subcomputations that
-- involve parallel arrays.  Such functionals do not requires the full blown vectorisation
-- transformation; instead, they can be lifted by application of a member of the zipWith family
-- (i.e., 'map', 'zipWith', zipWith3', etc.)
--
vectScalarFun :: Bool       -- ^ Was the function marked as scalar by the user?
              -> [Var]      -- ^ Functions names in same recursive binding group
              -> CoreExpr   -- ^ Expression to be vectorised
              -> VM VExpr
vectScalarFun forceScalar recFns expr
 = do { gscalarVars  <- globalScalarVars
      ; scalarTyCons <- globalScalarTyCons
      ; let scalarVars = gscalarVars `extendVarSetList` recFns
            (arg_tys, res_ty) = splitFunTys (exprType expr)
      ; MASSERT( not $ null arg_tys )
      ; onlyIfV (forceScalar                              -- user asserts the functions is scalar
                 ||
                 all (is_scalar_ty scalarTyCons) arg_tys  -- check whether the function is scalar
                  && is_scalar_ty scalarTyCons res_ty
                  && is_scalar scalarVars (is_scalar_ty scalarTyCons) expr
                  && uses scalarVars expr)
        $ mkScalarFun arg_tys res_ty expr
      }
  where
    is_scalar_ty scalarTyCons ty 
      | Just (tycon, _) <- splitTyConApp_maybe ty
      = tyConName tycon `elemNameSet` scalarTyCons
      | otherwise = False

    -- Checks whether an expression contain a non-scalar subexpression. 
    --
    -- Precodition: The variables in the first argument are scalar.
    --
    -- In case of a recursive binding group, we /assume/ that all bindings are scalar (by adding
    -- them to the list of scalar variables) and then check them.  If one of them turns out not to
    -- be scalar, the entire group is regarded as not being scalar.
    --
    -- The second argument is a predicate that checks whether a type is scalar.
    --
    is_scalar :: VarSet -> (Type -> Bool) -> CoreExpr -> Bool
    is_scalar scalars  _isScalarTC (Var v)         = v `elemVarSet` scalars
    is_scalar _scalars _isScalarTC (Lit _)         = True
    is_scalar scalars  isScalarTC  e@(App e1 e2) 
      | maybe_parr_ty (exprType e)                  = False
      | otherwise                                   = is_scalar scalars isScalarTC e1 && 
                                                      is_scalar scalars isScalarTC e2
    is_scalar scalars  isScalarTC  (Lam var body)  
      | maybe_parr_ty (varType var)                 = False
      | otherwise                                   = is_scalar (scalars `extendVarSet` var)
                                                               isScalarTC body
    is_scalar scalars  isScalarTC  (Let bind body) = bindsAreScalar && 
                                                     is_scalar scalars' isScalarTC body
      where
        (bindsAreScalar, scalars') = is_scalar_bind scalars isScalarTC bind
    is_scalar scalars  isScalarTC  (Case e var ty alts)
      | isScalarTC ty                  = is_scalar scalars' isScalarTC e && 
                                         all (is_scalar_alt scalars' isScalarTC) alts
      | otherwise                      = False
      where
        scalars' = scalars `extendVarSet` var
    is_scalar scalars  isScalarTC  (Cast e _coe)   = is_scalar scalars isScalarTC e
    is_scalar scalars  isScalarTC  (Note _ e   )   = is_scalar scalars isScalarTC e
    is_scalar _scalars _isScalarTC (Type {})       = True
    is_scalar _scalars _isScalarTC (Coercion {})   = True

    -- Result: (<is this binding group scalar>, scalars ++ variables bound in this group)
    is_scalar_bind scalars isScalarTCs (NonRec var e) = (is_scalar scalars isScalarTCs e, 
                                                         scalars `extendVarSet` var)
    is_scalar_bind scalars isScalarTCs (Rec bnds)     = (all (is_scalar scalars' isScalarTCs) es,
                                                         scalars')
      where
        (vars, es) = unzip bnds
        scalars'   = scalars `extendVarSetList` vars

    is_scalar_alt scalars isScalarTCs (_, vars, e) = is_scalar (scalars `extendVarSetList ` vars)
                                                               isScalarTCs e

    -- Checks whether the type might be a parallel array type.  In particular, if the outermost
    -- constructor is a type family, we conservatively assume that it may be a parallel array type.
    maybe_parr_ty :: Type -> Bool
    maybe_parr_ty ty 
      | Just ty'        <- coreView ty            = maybe_parr_ty ty'
      | Just (tyCon, _) <- splitTyConApp_maybe ty = isPArrTyCon tyCon || isSynFamilyTyCon tyCon 
    maybe_parr_ty _                               = False

    -- FIXME: I'm not convinced that this reasoning is (always) sound.  If the identify functions
    --        is called by some other function that is otherwise scalar, it would be very bad
    --        that just this call to the identity makes it not be scalar.
    -- A scalar function has to actually compute something. Without the check,
    -- we would treat (\(x :: Int) -> x) as a scalar function and lift it to
    -- (map (\x -> x)) which is very bad. Normal lifting transforms it to
    -- (\n# x -> x) which is what we want.
    uses funs (Var v)       = v `elemVarSet` funs 
    uses funs (App e1 e2)   = uses funs e1 || uses funs e2
    uses funs (Lam b body)  = uses (funs `extendVarSet` b) body
    uses funs (Let (NonRec _b letExpr) body) 
                            = uses funs letExpr || uses funs  body
    uses funs (Case e _eId _ty alts) 
                            = uses funs e || any (uses_alt funs) alts
    uses _ _                = False

    uses_alt funs (_, _bs, e) = uses funs e 

mkScalarFun :: [Type] -> Type -> CoreExpr -> VM VExpr
mkScalarFun arg_tys res_ty expr
  = do { fn_var  <- hoistExpr (fsLit "fn") expr DontInline
       ; zipf    <- zipScalars arg_tys res_ty
       ; clo     <- scalarClosure arg_tys res_ty (Var fn_var) (zipf `App` Var fn_var)
       ; clo_var <- hoistExpr (fsLit "clo") clo DontInline
       ; lclo    <- liftPD (Var clo_var)
       ; return (Var clo_var, lclo)
       }

-- | Vectorise a lambda abstraction.
--
vectLam :: Bool             -- ^ When the RHS of a binding, whether that binding should be inlined.
        -> Bool             -- ^ Whether the binding is a loop breaker.
        -> CoreExprWithFVs  -- ^ Body of abstraction.
        -> VM VExpr
vectLam inline loop_breaker expr@(fvs, AnnLam _ _)
 = do let (bs, body) = collectAnnValBinders expr
 
      tyvars    <- localTyVars
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
vectLam _ _ _ = panic "vectLam"
 

vectTyAppExpr :: CoreExprWithFVs -> [Type] -> VM VExpr
vectTyAppExpr (_, AnnVar v) tys = vectPolyVar v tys
vectTyAppExpr e tys = cantVectorise "Can't vectorise expression (vectTyExpr)"
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

