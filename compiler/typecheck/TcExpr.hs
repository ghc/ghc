{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcExpr]{Typecheck an expression}
-}

{-# LANGUAGE CPP, TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TcExpr ( tcPolyExpr, tcMonoExpr, tcMonoExprNC,
                tcInferSigma, tcInferSigmaNC, tcInferRho, tcInferRhoNC,
                tcSyntaxOp, tcSyntaxOpGen, SyntaxOpType(..), synKnownType,
                tcCheckId,
                addExprErrCtxt,
                getFixedTyVars ) where

#include "HsVersions.h"

import {-# SOURCE #-}   TcSplice( tcSpliceExpr, tcTypedBracket, tcUntypedBracket )
import THNames( liftStringName, liftName )

import HsSyn
import TcHsSyn
import TcRnMonad
import TcUnify
import BasicTypes
import Inst
import TcBinds          ( chooseInferredQuantifiers, tcLocalBinds )
import TcSigs           ( tcUserTypeSig, tcInstSig )
import TcSimplify       ( simplifyInfer, InferMode(..) )
import FamInst          ( tcGetFamInstEnvs, tcLookupDataFamInst )
import FamInstEnv       ( FamInstEnvs )
import RnEnv            ( addUsedGRE )
import RnUtils          ( addNameClashErrRn, unknownSubordinateErr )
import TcEnv
import TcArrows
import TcMatches
import TcHsType
import TcPatSyn( tcPatSynBuilderOcc, nonBidirectionalErr )
import TcPat
import TcMType
import TcType
import DsMonad
import Id
import IdInfo
import ConLike
import DataCon
import PatSyn
import Name
import NameEnv
import NameSet
import RdrName
import TyCon
import TyCoRep
import Type
import TcEvidence
import VarSet
import TysWiredIn
import TysPrim( intPrimTy )
import PrimOp( tagToEnumKey )
import PrelNames
import DynFlags
import SrcLoc
import Util
import VarEnv  ( emptyTidyEnv )
import ListSetOps
import Maybes
import Outputable
import FastString
import Control.Monad
import Class(classTyCon)
import UniqSet ( nonDetEltsUniqSet )
import qualified GHC.LanguageExtensions as LangExt

import Data.Function
import Data.List
import Data.Either
import qualified Data.Set as Set

{-
************************************************************************
*                                                                      *
\subsection{Main wrappers}
*                                                                      *
************************************************************************
-}

tcPolyExpr, tcPolyExprNC
  :: LHsExpr GhcRn         -- Expression to type check
  -> TcSigmaType           -- Expected type (could be a polytype)
  -> TcM (LHsExpr GhcTcId) -- Generalised expr with expected type

-- tcPolyExpr is a convenient place (frequent but not too frequent)
-- place to add context information.
-- The NC version does not do so, usually because the caller wants
-- to do so himself.

tcPolyExpr   expr res_ty = tc_poly_expr expr (mkCheckExpType res_ty)
tcPolyExprNC expr res_ty = tc_poly_expr_nc expr (mkCheckExpType res_ty)

-- these versions take an ExpType
tc_poly_expr, tc_poly_expr_nc :: LHsExpr GhcRn -> ExpSigmaType
                              -> TcM (LHsExpr GhcTcId)
tc_poly_expr expr res_ty
  = addExprErrCtxt expr $
    do { traceTc "tcPolyExpr" (ppr res_ty); tc_poly_expr_nc expr res_ty }

tc_poly_expr_nc (L loc expr) res_ty
  = do { traceTc "tcPolyExprNC" (ppr res_ty)
       ; (wrap, expr')
           <- tcSkolemiseET GenSigCtxt res_ty $ \ res_ty ->
              setSrcSpan loc $
                -- NB: setSrcSpan *after* skolemising, so we get better
                -- skolem locations
              tcExpr expr res_ty
       ; return $ L loc (mkHsWrap wrap expr') }

---------------
tcMonoExpr, tcMonoExprNC
    :: LHsExpr GhcRn     -- Expression to type check
    -> ExpRhoType        -- Expected type
                         -- Definitely no foralls at the top
    -> TcM (LHsExpr GhcTcId)

tcMonoExpr expr res_ty
  = addErrCtxt (exprCtxt expr) $
    tcMonoExprNC expr res_ty

tcMonoExprNC (L loc expr) res_ty
  = setSrcSpan loc $
    do  { expr' <- tcExpr expr res_ty
        ; return (L loc expr') }

---------------
tcInferSigma, tcInferSigmaNC :: LHsExpr GhcRn -> TcM ( LHsExpr GhcTcId
                                                    , TcSigmaType )
-- Infer a *sigma*-type.
tcInferSigma expr = addErrCtxt (exprCtxt expr) (tcInferSigmaNC expr)

tcInferSigmaNC (L loc expr)
  = setSrcSpan loc $
    do { (expr', sigma) <- tcInferNoInst (tcExpr expr)
       ; return (L loc expr', sigma) }

tcInferRho, tcInferRhoNC :: LHsExpr GhcRn -> TcM (LHsExpr GhcTcId, TcRhoType)
-- Infer a *rho*-type. The return type is always (shallowly) instantiated.
tcInferRho expr = addErrCtxt (exprCtxt expr) (tcInferRhoNC expr)

tcInferRhoNC expr
  = do { (expr', sigma) <- tcInferSigmaNC expr
       ; (wrap, rho) <- topInstantiate (lexprCtOrigin expr) sigma
       ; return (mkLHsWrap wrap expr', rho) }


{-
************************************************************************
*                                                                      *
        tcExpr: the main expression typechecker
*                                                                      *
************************************************************************

NB: The res_ty is always deeply skolemised.
-}

tcExpr :: HsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTcId)
tcExpr (HsVar (L _ name))   res_ty = tcCheckId name res_ty
tcExpr e@(HsUnboundVar uv)  res_ty = tcUnboundId e uv res_ty

tcExpr e@(HsApp {})     res_ty = tcApp1 e res_ty
tcExpr e@(HsAppType {}) res_ty = tcApp1 e res_ty

tcExpr e@(HsLit lit) res_ty
  = do { let lit_ty = hsLitType lit
       ; tcWrapResult e (HsLit (convertLit lit)) lit_ty res_ty }

tcExpr (HsPar expr)   res_ty = do { expr' <- tcMonoExprNC expr res_ty
                                  ; return (HsPar expr') }

tcExpr (HsSCC src lbl expr) res_ty
  = do { expr' <- tcMonoExpr expr res_ty
       ; return (HsSCC src lbl expr') }

tcExpr (HsTickPragma src info srcInfo expr) res_ty
  = do { expr' <- tcMonoExpr expr res_ty
       ; return (HsTickPragma src info srcInfo expr') }

tcExpr (HsCoreAnn src lbl expr) res_ty
  = do  { expr' <- tcMonoExpr expr res_ty
        ; return (HsCoreAnn src lbl expr') }

tcExpr (HsOverLit lit) res_ty
  = do  { lit' <- newOverloadedLit lit res_ty
        ; return (HsOverLit lit') }

tcExpr (NegApp expr neg_expr) res_ty
  = do  { (expr', neg_expr')
            <- tcSyntaxOp NegateOrigin neg_expr [SynAny] res_ty $
               \[arg_ty] ->
               tcMonoExpr expr (mkCheckExpType arg_ty)
        ; return (NegApp expr' neg_expr') }

tcExpr e@(HsIPVar x) res_ty
  = do {   {- Implicit parameters must have a *tau-type* not a
              type scheme.  We enforce this by creating a fresh
              type variable as its type.  (Because res_ty may not
              be a tau-type.) -}
         ip_ty <- newOpenFlexiTyVarTy
       ; let ip_name = mkStrLitTy (hsIPNameFS x)
       ; ipClass <- tcLookupClass ipClassName
       ; ip_var <- emitWantedEvVar origin (mkClassPred ipClass [ip_name, ip_ty])
       ; tcWrapResult e (fromDict ipClass ip_name ip_ty (HsVar (noLoc ip_var)))
                      ip_ty res_ty }
  where
  -- Coerces a dictionary for `IP "x" t` into `t`.
  fromDict ipClass x ty = mkHsWrap $ mkWpCastR $
                          unwrapIP $ mkClassPred ipClass [x,ty]
  origin = IPOccOrigin x

tcExpr e@(HsOverLabel mb_fromLabel l) res_ty
  = do { -- See Note [Type-checking overloaded labels]
         loc <- getSrcSpanM
       ; case mb_fromLabel of
           Just fromLabel -> tcExpr (applyFromLabel loc fromLabel) res_ty
           Nothing -> do { isLabelClass <- tcLookupClass isLabelClassName
                         ; alpha <- newFlexiTyVarTy liftedTypeKind
                         ; let pred = mkClassPred isLabelClass [lbl, alpha]
                         ; loc <- getSrcSpanM
                         ; var <- emitWantedEvVar origin pred
                         ; tcWrapResult e (fromDict pred (HsVar (L loc var)))
                                        alpha res_ty } }
  where
  -- Coerces a dictionary for `IsLabel "x" t` into `t`,
  -- or `HasField "x" r a into `r -> a`.
  fromDict pred = mkHsWrap $ mkWpCastR $ unwrapIP pred
  origin = OverLabelOrigin l
  lbl = mkStrLitTy l

  applyFromLabel loc fromLabel =
    L loc (HsVar (L loc fromLabel)) `HsAppType`
      mkEmptyWildCardBndrs (L loc (HsTyLit (HsStrTy NoSourceText l)))

tcExpr (HsLam match) res_ty
  = do  { (match', wrap) <- tcMatchLambda herald match_ctxt match res_ty
        ; return (mkHsWrap wrap (HsLam match')) }
  where
    match_ctxt = MC { mc_what = LambdaExpr, mc_body = tcBody }
    herald = sep [ text "The lambda expression" <+>
                   quotes (pprSetDepth (PartWay 1) $
                           pprMatches match),
                        -- The pprSetDepth makes the abstraction print briefly
                   text "has"]

tcExpr e@(HsLamCase matches) res_ty
  = do { (matches', wrap)
           <- tcMatchLambda msg match_ctxt matches res_ty
           -- The laziness annotation is because we don't want to fail here
           -- if there are multiple arguments
       ; return (mkHsWrap wrap $ HsLamCase matches') }
  where
    msg = sep [ text "The function" <+> quotes (ppr e)
              , text "requires"]
    match_ctxt = MC { mc_what = CaseAlt, mc_body = tcBody }

tcExpr e@(ExprWithTySig expr sig_ty) res_ty
  = do { let loc = getLoc (hsSigWcType sig_ty)
       ; sig_info <- checkNoErrs $  -- Avoid error cascade
                     tcUserTypeSig loc sig_ty Nothing
       ; (expr', poly_ty) <- tcExprSig expr sig_info
       ; let expr'' = ExprWithTySigOut expr' sig_ty
       ; tcWrapResult e expr'' poly_ty res_ty }

{-
Note [Type-checking overloaded labels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recall that we have

  module GHC.OverloadedLabels where
    class IsLabel (x :: Symbol) a where
      fromLabel :: a

We translate `#foo` to `fromLabel @"foo"`, where we use

 * the in-scope `fromLabel` if `RebindableSyntax` is enabled; or if not
 * `GHC.OverloadedLabels.fromLabel`.

In the `RebindableSyntax` case, the renamer will have filled in the
first field of `HsOverLabel` with the `fromLabel` function to use, and
we simply apply it to the appropriate visible type argument.

In the `OverloadedLabels` case, when we see an overloaded label like
`#foo`, we generate a fresh variable `alpha` for the type and emit an
`IsLabel "foo" alpha` constraint.  Because the `IsLabel` class has a
single method, it is represented by a newtype, so we can coerce
`IsLabel "foo" alpha` to `alpha` (just like for implicit parameters).

-}


{-
************************************************************************
*                                                                      *
                Infix operators and sections
*                                                                      *
************************************************************************

Note [Left sections]
~~~~~~~~~~~~~~~~~~~~
Left sections, like (4 *), are equivalent to
        \ x -> (*) 4 x,
or, if PostfixOperators is enabled, just
        (*) 4
With PostfixOperators we don't actually require the function to take
two arguments at all.  For example, (x `not`) means (not x); you get
postfix operators!  Not Haskell 98, but it's less work and kind of
useful.

Note [Typing rule for ($)]
~~~~~~~~~~~~~~~~~~~~~~~~~~
People write
   runST $ blah
so much, where
   runST :: (forall s. ST s a) -> a
that I have finally given in and written a special type-checking
rule just for saturated applications of ($).
  * Infer the type of the first argument
  * Decompose it; should be of form (arg2_ty -> res_ty),
       where arg2_ty might be a polytype
  * Use arg2_ty to typecheck arg2

Note [Typing rule for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to allow
       x `seq` (# p,q #)
which suggests this type for seq:
   seq :: forall (a:*) (b:Open). a -> b -> b,
with (b:Open) meaning that be can be instantiated with an unboxed
tuple.  The trouble is that this might accept a partially-applied
'seq', and I'm just not certain that would work.  I'm only sure it's
only going to work when it's fully applied, so it turns into
    case x of _ -> (# p,q #)

So it seems more uniform to treat 'seq' as if it was a language
construct.

See also Note [seqId magic] in MkId
-}

tcExpr expr@(OpApp arg1 op fix arg2) res_ty
  | (L loc (HsVar (L lv op_name))) <- op
  , op_name `hasKey` seqIdKey           -- Note [Typing rule for seq]
  = do { arg1_ty <- newFlexiTyVarTy liftedTypeKind
       ; let arg2_exp_ty = res_ty
       ; arg1' <- tcArg op arg1 arg1_ty 1
       ; arg2' <- addErrCtxt (funAppCtxt op arg2 2) $
                  tc_poly_expr_nc arg2 arg2_exp_ty
       ; arg2_ty <- readExpType arg2_exp_ty
       ; op_id <- tcLookupId op_name
       ; let op' = L loc (mkHsWrap (mkWpTyApps [arg1_ty, arg2_ty])
                                   (HsVar (L lv op_id)))
       ; return $ OpApp arg1' op' fix arg2' }

  | (L loc (HsVar (L lv op_name))) <- op
  , op_name `hasKey` dollarIdKey        -- Note [Typing rule for ($)]
  = do { traceTc "Application rule" (ppr op)
       ; (arg1', arg1_ty) <- tcInferSigma arg1

       ; let doc   = text "The first argument of ($) takes"
             orig1 = lexprCtOrigin arg1
       ; (wrap_arg1, [arg2_sigma], op_res_ty) <-
           matchActualFunTys doc orig1 (Just (unLoc arg1)) 1 arg1_ty

         -- We have (arg1 $ arg2)
         -- So: arg1_ty = arg2_ty -> op_res_ty
         -- where arg2_sigma maybe polymorphic; that's the point

       ; arg2'  <- tcArg op arg2 arg2_sigma 2

       -- Make sure that the argument type has kind '*'
       --   ($) :: forall (r:RuntimeRep) (a:*) (b:TYPE r). (a->b) -> a -> b
       -- Eg we do not want to allow  (D#  $  4.0#)   Trac #5570
       --    (which gives a seg fault)
       --
       -- The *result* type can have any kind (Trac #8739),
       -- so we don't need to check anything for that
       ; _ <- unifyKind (Just (HsCoreTy arg2_sigma)) (typeKind arg2_sigma) liftedTypeKind
           -- ignore the evidence. arg2_sigma must have type * or #,
           -- because we know arg2_sigma -> or_res_ty is well-kinded
           -- (because otherwise matchActualFunTys would fail)
           -- There's no possibility here of, say, a kind family reducing to *.

       ; wrap_res <- tcSubTypeHR orig1 (Just expr) op_res_ty res_ty
                       -- op_res -> res

       ; op_id  <- tcLookupId op_name
       ; res_ty <- readExpType res_ty
       ; let op' = L loc (mkHsWrap (mkWpTyApps [ getRuntimeRep "tcExpr ($)" res_ty
                                               , arg2_sigma
                                               , res_ty])
                                   (HsVar (L lv op_id)))
             -- arg1' :: arg1_ty
             -- wrap_arg1 :: arg1_ty "->" (arg2_sigma -> op_res_ty)
             -- wrap_res :: op_res_ty "->" res_ty
             -- op' :: (a2_ty -> res_ty) -> a2_ty -> res_ty

             -- wrap1 :: arg1_ty "->" (arg2_sigma -> res_ty)
             wrap1 = mkWpFun idHsWrapper wrap_res arg2_sigma res_ty doc
                     <.> wrap_arg1
             doc = text "When looking at the argument to ($)"

       ; return (OpApp (mkLHsWrap wrap1 arg1') op' fix arg2') }

  | (L loc (HsRecFld (Ambiguous lbl _))) <- op
  , Just sig_ty <- obviousSig (unLoc arg1)
    -- See Note [Disambiguating record fields]
  = do { sig_tc_ty <- tcHsSigWcType ExprSigCtxt sig_ty
       ; sel_name <- disambiguateSelector lbl sig_tc_ty
       ; let op' = L loc (HsRecFld (Unambiguous lbl sel_name))
       ; tcExpr (OpApp arg1 op' fix arg2) res_ty
       }

  | otherwise
  = do { traceTc "Non Application rule" (ppr op)
       ; (wrap, op', [Left arg1', Left arg2'])
           <- tcApp (Just $ mk_op_msg op)
                     op [Left arg1, Left arg2] res_ty
       ; return (mkHsWrap wrap $ OpApp arg1' op' fix arg2') }

-- Right sections, equivalent to \ x -> x `op` expr, or
--      \ x -> op x expr

tcExpr expr@(SectionR op arg2) res_ty
  = do { (op', op_ty) <- tcInferFun op
       ; (wrap_fun, [arg1_ty, arg2_ty], op_res_ty)
                  <- matchActualFunTys (mk_op_msg op) fn_orig (Just (unLoc op)) 2 op_ty
       ; wrap_res <- tcSubTypeHR SectionOrigin (Just expr)
                                 (mkFunTy arg1_ty op_res_ty) res_ty
       ; arg2' <- tcArg op arg2 arg2_ty 2
       ; return ( mkHsWrap wrap_res $
                  SectionR (mkLHsWrap wrap_fun op') arg2' ) }
  where
    fn_orig = lexprCtOrigin op
    -- It's important to use the origin of 'op', so that call-stacks
    -- come out right; they are driven by the OccurrenceOf CtOrigin
    -- See Trac #13285

tcExpr expr@(SectionL arg1 op) res_ty
  = do { (op', op_ty) <- tcInferFun op
       ; dflags <- getDynFlags      -- Note [Left sections]
       ; let n_reqd_args | xopt LangExt.PostfixOperators dflags = 1
                         | otherwise                            = 2

       ; (wrap_fn, (arg1_ty:arg_tys), op_res_ty)
           <- matchActualFunTys (mk_op_msg op) fn_orig (Just (unLoc op))
                                n_reqd_args op_ty
       ; wrap_res <- tcSubTypeHR SectionOrigin (Just expr)
                                 (mkFunTys arg_tys op_res_ty) res_ty
       ; arg1' <- tcArg op arg1 arg1_ty 1
       ; return ( mkHsWrap wrap_res $
                  SectionL arg1' (mkLHsWrap wrap_fn op') ) }
  where
    fn_orig = lexprCtOrigin op
    -- It's important to use the origin of 'op', so that call-stacks
    -- come out right; they are driven by the OccurrenceOf CtOrigin
    -- See Trac #13285

tcExpr expr@(ExplicitTuple tup_args boxity) res_ty
  | all tupArgPresent tup_args
  = do { let arity  = length tup_args
             tup_tc = tupleTyCon boxity arity
       ; res_ty <- expTypeToType res_ty
       ; (coi, arg_tys) <- matchExpectedTyConApp tup_tc res_ty
                           -- Unboxed tuples have RuntimeRep vars, which we
                           -- don't care about here
                           -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
       ; let arg_tys' = case boxity of Unboxed -> drop arity arg_tys
                                       Boxed   -> arg_tys
       ; tup_args1 <- tcTupArgs tup_args arg_tys'
       ; return $ mkHsWrapCo coi (ExplicitTuple tup_args1 boxity) }

  | otherwise
  = -- The tup_args are a mixture of Present and Missing (for tuple sections)
    do { let arity = length tup_args

       ; arg_tys <- case boxity of
           { Boxed   -> newFlexiTyVarTys arity liftedTypeKind
           ; Unboxed -> replicateM arity newOpenFlexiTyVarTy }
       ; let actual_res_ty
                 = mkFunTys [ty | (ty, (L _ (Missing _))) <- arg_tys `zip` tup_args]
                            (mkTupleTy boxity arg_tys)

       ; wrap <- tcSubTypeHR (Shouldn'tHappenOrigin "ExpTuple")
                             (Just expr)
                             actual_res_ty res_ty

       -- Handle tuple sections where
       ; tup_args1 <- tcTupArgs tup_args arg_tys

       ; return $ mkHsWrap wrap (ExplicitTuple tup_args1 boxity) }

tcExpr (ExplicitSum alt arity expr _) res_ty
  = do { let sum_tc = sumTyCon arity
       ; res_ty <- expTypeToType res_ty
       ; (coi, arg_tys) <- matchExpectedTyConApp sum_tc res_ty
       ; -- Drop levity vars, we don't care about them here
         let arg_tys' = drop arity arg_tys
       ; expr' <- tcPolyExpr expr (arg_tys' `getNth` (alt - 1))
       ; return $ mkHsWrapCo coi (ExplicitSum alt arity expr' arg_tys') }

tcExpr (ExplicitList _ witness exprs) res_ty
  = case witness of
      Nothing   -> do  { res_ty <- expTypeToType res_ty
                       ; (coi, elt_ty) <- matchExpectedListTy res_ty
                       ; exprs' <- mapM (tc_elt elt_ty) exprs
                       ; return $
                         mkHsWrapCo coi $ ExplicitList elt_ty Nothing exprs' }

      Just fln -> do { ((exprs', elt_ty), fln')
                         <- tcSyntaxOp ListOrigin fln
                                       [synKnownType intTy, SynList] res_ty $
                            \ [elt_ty] ->
                            do { exprs' <-
                                    mapM (tc_elt elt_ty) exprs
                               ; return (exprs', elt_ty) }

                     ; return $ ExplicitList elt_ty (Just fln') exprs' }
     where tc_elt elt_ty expr = tcPolyExpr expr elt_ty

tcExpr (ExplicitPArr _ exprs) res_ty    -- maybe empty
  = do  { res_ty <- expTypeToType res_ty
        ; (coi, elt_ty) <- matchExpectedPArrTy res_ty
        ; exprs' <- mapM (tc_elt elt_ty) exprs
        ; return $
          mkHsWrapCo coi $ ExplicitPArr elt_ty exprs' }
  where
    tc_elt elt_ty expr = tcPolyExpr expr elt_ty

{-
************************************************************************
*                                                                      *
                Let, case, if, do
*                                                                      *
************************************************************************
-}

tcExpr (HsLet (L l binds) expr) res_ty
  = do  { (binds', expr') <- tcLocalBinds binds $
                             tcMonoExpr expr res_ty
        ; return (HsLet (L l binds') expr') }

tcExpr (HsCase scrut matches) res_ty
  = do  {  -- We used to typecheck the case alternatives first.
           -- The case patterns tend to give good type info to use
           -- when typechecking the scrutinee.  For example
           --   case (map f) of
           --     (x:xs) -> ...
           -- will report that map is applied to too few arguments
           --
           -- But now, in the GADT world, we need to typecheck the scrutinee
           -- first, to get type info that may be refined in the case alternatives
          (scrut', scrut_ty) <- tcInferRho scrut

        ; traceTc "HsCase" (ppr scrut_ty)
        ; matches' <- tcMatchesCase match_ctxt scrut_ty matches res_ty
        ; return (HsCase scrut' matches') }
 where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = tcBody }

tcExpr (HsIf Nothing pred b1 b2) res_ty    -- Ordinary 'if'
  = do { pred' <- tcMonoExpr pred (mkCheckExpType boolTy)
       ; res_ty <- tauifyExpType res_ty
           -- Just like Note [Case branches must never infer a non-tau type]
           -- in TcMatches (See #10619)

       ; b1' <- tcMonoExpr b1 res_ty
       ; b2' <- tcMonoExpr b2 res_ty
       ; return (HsIf Nothing pred' b1' b2') }

tcExpr (HsIf (Just fun) pred b1 b2) res_ty
  = do { ((pred', b1', b2'), fun')
           <- tcSyntaxOp IfOrigin fun [SynAny, SynAny, SynAny] res_ty $
              \ [pred_ty, b1_ty, b2_ty] ->
              do { pred' <- tcPolyExpr pred pred_ty
                 ; b1'   <- tcPolyExpr b1   b1_ty
                 ; b2'   <- tcPolyExpr b2   b2_ty
                 ; return (pred', b1', b2') }
       ; return (HsIf (Just fun') pred' b1' b2') }

tcExpr (HsMultiIf _ alts) res_ty
  = do { res_ty <- if isSingleton alts
                   then return res_ty
                   else tauifyExpType res_ty
             -- Just like TcMatches
             -- Note [Case branches must never infer a non-tau type]

       ; alts' <- mapM (wrapLocM $ tcGRHS match_ctxt res_ty) alts
       ; res_ty <- readExpType res_ty
       ; return (HsMultiIf res_ty alts') }
  where match_ctxt = MC { mc_what = IfAlt, mc_body = tcBody }

tcExpr (HsDo do_or_lc stmts _) res_ty
  = do { expr' <- tcDoStmts do_or_lc stmts res_ty
       ; return expr' }

tcExpr (HsProc pat cmd) res_ty
  = do  { (pat', cmd', coi) <- tcProc pat cmd res_ty
        ; return $ mkHsWrapCo coi (HsProc pat' cmd') }

-- Typechecks the static form and wraps it with a call to 'fromStaticPtr'.
-- See Note [Grand plan for static forms] in StaticPtrTable for an overview.
-- To type check
--      (static e) :: p a
-- we want to check (e :: a),
-- and wrap (static e) in a call to
--    fromStaticPtr :: IsStatic p => StaticPtr a -> p a

tcExpr (HsStatic fvs expr) res_ty
  = do  { res_ty          <- expTypeToType res_ty
        ; (co, (p_ty, expr_ty)) <- matchExpectedAppTy res_ty
        ; (expr', lie)    <- captureConstraints $
            addErrCtxt (hang (text "In the body of a static form:")
                             2 (ppr expr)
                       ) $
            tcPolyExprNC expr expr_ty

        -- Check that the free variables of the static form are closed.
        -- It's OK to use nonDetEltsUniqSet here as the only side effects of
        -- checkClosedInStaticForm are error messages.
        ; mapM_ checkClosedInStaticForm $ nonDetEltsUniqSet fvs

        -- Require the type of the argument to be Typeable.
        -- The evidence is not used, but asking the constraint ensures that
        -- the current implementation is as restrictive as future versions
        -- of the StaticPointers extension.
        ; typeableClass <- tcLookupClass typeableClassName
        ; _ <- emitWantedEvVar StaticOrigin $
                  mkTyConApp (classTyCon typeableClass)
                             [liftedTypeKind, expr_ty]

        -- Insert the constraints of the static form in a global list for later
        -- validation.
        ; emitStaticConstraints lie

        -- Wrap the static form with the 'fromStaticPtr' call.
        ; fromStaticPtr <- newMethodFromName StaticOrigin fromStaticPtrName p_ty
        ; let wrap = mkWpTyApps [expr_ty]
        ; loc <- getSrcSpanM
        ; return $ mkHsWrapCo co $ HsApp (L loc $ mkHsWrap wrap fromStaticPtr)
                                         (L loc (HsStatic fvs expr'))
        }

{-
************************************************************************
*                                                                      *
                Record construction and update
*                                                                      *
************************************************************************
-}

tcExpr expr@(RecordCon { rcon_con_name = L loc con_name
                       , rcon_flds = rbinds }) res_ty
  = do  { con_like <- tcLookupConLike con_name

        -- Check for missing fields
        ; checkMissingFields con_like rbinds

        ; (con_expr, con_sigma) <- tcInferId con_name
        ; (con_wrap, con_tau) <-
            topInstantiate (OccurrenceOf con_name) con_sigma
              -- a shallow instantiation should really be enough for
              -- a data constructor.
        ; let arity = conLikeArity con_like
              Right (arg_tys, actual_res_ty) = tcSplitFunTysN arity con_tau
        ; case conLikeWrapId_maybe con_like of
               Nothing -> nonBidirectionalErr (conLikeName con_like)
               Just con_id -> do {
                  res_wrap <- tcSubTypeHR (Shouldn'tHappenOrigin "RecordCon")
                                          (Just expr) actual_res_ty res_ty
                ; rbinds' <- tcRecordBinds con_like arg_tys rbinds
                ; return $
                  mkHsWrap res_wrap $
                  RecordCon { rcon_con_name = L loc con_id
                            , rcon_con_expr = mkHsWrap con_wrap con_expr
                            , rcon_con_like = con_like
                            , rcon_flds = rbinds' } } }

{-
Note [Type of a record update]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The main complication with RecordUpd is that we need to explicitly
handle the *non-updated* fields.  Consider:

        data T a b c = MkT1 { fa :: a, fb :: (b,c) }
                     | MkT2 { fa :: a, fb :: (b,c), fc :: c -> c }
                     | MkT3 { fd :: a }

        upd :: T a b c -> (b',c) -> T a b' c
        upd t x = t { fb = x}

The result type should be (T a b' c)
not (T a b c),   because 'b' *is not* mentioned in a non-updated field
not (T a b' c'), because 'c' *is*     mentioned in a non-updated field
NB that it's not good enough to look at just one constructor; we must
look at them all; cf Trac #3219

After all, upd should be equivalent to:
        upd t x = case t of
                        MkT1 p q -> MkT1 p x
                        MkT2 a b -> MkT2 p b
                        MkT3 d   -> error ...

So we need to give a completely fresh type to the result record,
and then constrain it by the fields that are *not* updated ("p" above).
We call these the "fixed" type variables, and compute them in getFixedTyVars.

Note that because MkT3 doesn't contain all the fields being updated,
its RHS is simply an error, so it doesn't impose any type constraints.
Hence the use of 'relevant_cont'.

Note [Implicit type sharing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We also take into account any "implicit" non-update fields.  For example
        data T a b where { MkT { f::a } :: T a a; ... }
So the "real" type of MkT is: forall ab. (a~b) => a -> T a b

Then consider
        upd t x = t { f=x }
We infer the type
        upd :: T a b -> a -> T a b
        upd (t::T a b) (x::a)
           = case t of { MkT (co:a~b) (_:a) -> MkT co x }
We can't give it the more general type
        upd :: T a b -> c -> T c b

Note [Criteria for update]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to allow update for existentials etc, provided the updated
field isn't part of the existential. For example, this should be ok.
  data T a where { MkT { f1::a, f2::b->b } :: T a }
  f :: T a -> b -> T b
  f t b = t { f1=b }

The criterion we use is this:

  The types of the updated fields
  mention only the universally-quantified type variables
  of the data constructor

NB: this is not (quite) the same as being a "naughty" record selector
(See Note [Naughty record selectors]) in TcTyClsDecls), at least
in the case of GADTs. Consider
   data T a where { MkT :: { f :: a } :: T [a] }
Then f is not "naughty" because it has a well-typed record selector.
But we don't allow updates for 'f'.  (One could consider trying to
allow this, but it makes my head hurt.  Badly.  And no one has asked
for it.)

In principle one could go further, and allow
  g :: T a -> T a
  g t = t { f2 = \x -> x }
because the expression is polymorphic...but that seems a bridge too far.

Note [Data family example]
~~~~~~~~~~~~~~~~~~~~~~~~~~
    data instance T (a,b) = MkT { x::a, y::b }
  --->
    data :TP a b = MkT { a::a, y::b }
    coTP a b :: T (a,b) ~ :TP a b

Suppose r :: T (t1,t2), e :: t3
Then  r { x=e } :: T (t3,t1)
  --->
      case r |> co1 of
        MkT x y -> MkT e y |> co2
      where co1 :: T (t1,t2) ~ :TP t1 t2
            co2 :: :TP t3 t2 ~ T (t3,t2)
The wrapping with co2 is done by the constructor wrapper for MkT

Outgoing invariants
~~~~~~~~~~~~~~~~~~~
In the outgoing (HsRecordUpd scrut binds cons in_inst_tys out_inst_tys):

  * cons are the data constructors to be updated

  * in_inst_tys, out_inst_tys have same length, and instantiate the
        *representation* tycon of the data cons.  In Note [Data
        family example], in_inst_tys = [t1,t2], out_inst_tys = [t3,t2]

Note [Mixed Record Field Updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following pattern synonym.

  data MyRec = MyRec { foo :: Int, qux :: String }

  pattern HisRec{f1, f2} = MyRec{foo = f1, qux=f2}

This allows updates such as the following

  updater :: MyRec -> MyRec
  updater a = a {f1 = 1 }

It would also make sense to allow the following update (which we reject).

  updater a = a {f1 = 1, qux = "two" } ==? MyRec 1 "two"

This leads to confusing behaviour when the selectors in fact refer the same
field.

  updater a = a {f1 = 1, foo = 2} ==? ???

For this reason, we reject a mixture of pattern synonym and normal record
selectors in the same update block. Although of course we still allow the
following.

  updater a = (a {f1 = 1}) {foo = 2}

  > updater (MyRec 0 "str")
  MyRec 2 "str"

-}

tcExpr expr@(RecordUpd { rupd_expr = record_expr, rupd_flds = rbnds }) res_ty
  = ASSERT( notNull rbnds )
    do  { -- STEP -2: typecheck the record_expr, the record to be updated
          (record_expr', record_rho) <- tcInferRho record_expr

        -- STEP -1  See Note [Disambiguating record fields]
        -- After this we know that rbinds is unambiguous
        ; rbinds <- disambiguateRecordBinds record_expr record_rho rbnds res_ty
        ; let upd_flds = map (unLoc . hsRecFieldLbl . unLoc) rbinds
              upd_fld_occs = map (occNameFS . rdrNameOcc . rdrNameAmbiguousFieldOcc) upd_flds
              sel_ids      = map selectorAmbiguousFieldOcc upd_flds
        -- STEP 0
        -- Check that the field names are really field names
        -- and they are all field names for proper records or
        -- all field names for pattern synonyms.
        ; let bad_guys = [ setSrcSpan loc $ addErrTc (notSelector fld_name)
                         | fld <- rbinds,
                           -- Excludes class ops
                           let L loc sel_id = hsRecUpdFieldId (unLoc fld),
                           not (isRecordSelector sel_id),
                           let fld_name = idName sel_id ]
        ; unless (null bad_guys) (sequence bad_guys >> failM)
        -- See note [Mixed Record Selectors]
        ; let (data_sels, pat_syn_sels) =
                partition isDataConRecordSelector sel_ids
        ; MASSERT( all isPatSynRecordSelector pat_syn_sels )
        ; checkTc ( null data_sels || null pat_syn_sels )
                  ( mixedSelectors data_sels pat_syn_sels )

        -- STEP 1
        -- Figure out the tycon and data cons from the first field name
        ; let   -- It's OK to use the non-tc splitters here (for a selector)
              sel_id : _  = sel_ids

              mtycon :: Maybe TyCon
              mtycon = case idDetails sel_id of
                          RecSelId (RecSelData tycon) _ -> Just tycon
                          _ -> Nothing

              con_likes :: [ConLike]
              con_likes = case idDetails sel_id of
                             RecSelId (RecSelData tc) _
                                -> map RealDataCon (tyConDataCons tc)
                             RecSelId (RecSelPatSyn ps) _
                                -> [PatSynCon ps]
                             _  -> panic "tcRecordUpd"
                -- NB: for a data type family, the tycon is the instance tycon

              relevant_cons = conLikesWithFields con_likes upd_fld_occs
                -- A constructor is only relevant to this process if
                -- it contains *all* the fields that are being updated
                -- Other ones will cause a runtime error if they occur

        -- Step 2
        -- Check that at least one constructor has all the named fields
        -- i.e. has an empty set of bad fields returned by badFields
        ; checkTc (not (null relevant_cons)) (badFieldsUpd rbinds con_likes)

        -- Take apart a representative constructor
        ; let con1 = ASSERT( not (null relevant_cons) ) head relevant_cons
              (con1_tvs, _, _, _prov_theta, req_theta, con1_arg_tys, _)
                 = conLikeFullSig con1
              con1_flds   = map flLabel $ conLikeFieldLabels con1
              con1_tv_tys = mkTyVarTys con1_tvs
              con1_res_ty = case mtycon of
                              Just tc -> mkFamilyTyConApp tc con1_tv_tys
                              Nothing -> conLikeResTy con1 con1_tv_tys

        -- Check that we're not dealing with a unidirectional pattern
        -- synonym
        ; unless (isJust $ conLikeWrapId_maybe con1)
                  (nonBidirectionalErr (conLikeName con1))

        -- STEP 3    Note [Criteria for update]
        -- Check that each updated field is polymorphic; that is, its type
        -- mentions only the universally-quantified variables of the data con
        ; let flds1_w_tys  = zipEqual "tcExpr:RecConUpd" con1_flds con1_arg_tys
              bad_upd_flds = filter bad_fld flds1_w_tys
              con1_tv_set  = mkVarSet con1_tvs
              bad_fld (fld, ty) = fld `elem` upd_fld_occs &&
                                      not (tyCoVarsOfType ty `subVarSet` con1_tv_set)
        ; checkTc (null bad_upd_flds) (badFieldTypes bad_upd_flds)

        -- STEP 4  Note [Type of a record update]
        -- Figure out types for the scrutinee and result
        -- Both are of form (T a b c), with fresh type variables, but with
        -- common variables where the scrutinee and result must have the same type
        -- These are variables that appear in *any* arg of *any* of the
        -- relevant constructors *except* in the updated fields
        --
        ; let fixed_tvs = getFixedTyVars upd_fld_occs con1_tvs relevant_cons
              is_fixed_tv tv = tv `elemVarSet` fixed_tvs

              mk_inst_ty :: TCvSubst -> (TyVar, TcType) -> TcM (TCvSubst, TcType)
              -- Deals with instantiation of kind variables
              --   c.f. TcMType.newMetaTyVars
              mk_inst_ty subst (tv, result_inst_ty)
                | is_fixed_tv tv   -- Same as result type
                = return (extendTvSubst subst tv result_inst_ty, result_inst_ty)
                | otherwise        -- Fresh type, of correct kind
                = do { (subst', new_tv) <- newMetaTyVarX subst tv
                     ; return (subst', mkTyVarTy new_tv) }

        ; (result_subst, con1_tvs') <- newMetaTyVars con1_tvs
        ; let result_inst_tys = mkTyVarTys con1_tvs'
              init_subst = mkEmptyTCvSubst (getTCvInScope result_subst)

        ; (scrut_subst, scrut_inst_tys) <- mapAccumLM mk_inst_ty init_subst
                                                      (con1_tvs `zip` result_inst_tys)

        ; let rec_res_ty    = TcType.substTy result_subst con1_res_ty
              scrut_ty      = TcType.substTy scrut_subst  con1_res_ty
              con1_arg_tys' = map (TcType.substTy result_subst) con1_arg_tys

        ; wrap_res <- tcSubTypeHR (exprCtOrigin expr)
                                  (Just expr) rec_res_ty res_ty
        ; co_scrut <- unifyType (Just (unLoc record_expr)) record_rho scrut_ty
                -- NB: normal unification is OK here (as opposed to subsumption),
                -- because for this to work out, both record_rho and scrut_ty have
                -- to be normal datatypes -- no contravariant stuff can go on

        -- STEP 5
        -- Typecheck the bindings
        ; rbinds'      <- tcRecordUpd con1 con1_arg_tys' rbinds

        -- STEP 6: Deal with the stupid theta
        ; let theta' = substThetaUnchecked scrut_subst (conLikeStupidTheta con1)
        ; instStupidTheta RecordUpdOrigin theta'

        -- Step 7: make a cast for the scrutinee, in the
        --         case that it's from a data family
        ; let fam_co :: HsWrapper   -- RepT t1 .. tn ~R scrut_ty
              fam_co | Just tycon <- mtycon
                     , Just co_con <- tyConFamilyCoercion_maybe tycon
                     = mkWpCastR (mkTcUnbranchedAxInstCo co_con scrut_inst_tys [])
                     | otherwise
                     = idHsWrapper

        -- Step 8: Check that the req constraints are satisfied
        -- For normal data constructors req_theta is empty but we must do
        -- this check for pattern synonyms.
        ; let req_theta' = substThetaUnchecked scrut_subst req_theta
        ; req_wrap <- instCallConstraints RecordUpdOrigin req_theta'

        -- Phew!
        ; return $
          mkHsWrap wrap_res $
          RecordUpd { rupd_expr = mkLHsWrap fam_co (mkLHsWrapCo co_scrut record_expr')
                    , rupd_flds = rbinds'
                    , rupd_cons = relevant_cons, rupd_in_tys = scrut_inst_tys
                    , rupd_out_tys = result_inst_tys, rupd_wrap = req_wrap } }

tcExpr e@(HsRecFld f) res_ty
    = tcCheckRecSelId e f res_ty

{-
************************************************************************
*                                                                      *
        Arithmetic sequences                    e.g. [a,b..]
        and their parallel-array counterparts   e.g. [: a,b.. :]

*                                                                      *
************************************************************************
-}

tcExpr (ArithSeq _ witness seq) res_ty
  = tcArithSeq witness seq res_ty

tcExpr (PArrSeq _ seq@(FromTo expr1 expr2)) res_ty
  = do  { res_ty <- expTypeToType res_ty
        ; (coi, elt_ty) <- matchExpectedPArrTy res_ty
        ; expr1' <- tcPolyExpr expr1 elt_ty
        ; expr2' <- tcPolyExpr expr2 elt_ty
        ; enumFromToP <- initDsTc $ dsDPHBuiltin enumFromToPVar
        ; enum_from_to <- newMethodFromName (PArrSeqOrigin seq)
                                 (idName enumFromToP) elt_ty
        ; return $
          mkHsWrapCo coi $ PArrSeq enum_from_to (FromTo expr1' expr2') }

tcExpr (PArrSeq _ seq@(FromThenTo expr1 expr2 expr3)) res_ty
  = do  { res_ty <- expTypeToType res_ty
        ; (coi, elt_ty) <- matchExpectedPArrTy res_ty
        ; expr1' <- tcPolyExpr expr1 elt_ty
        ; expr2' <- tcPolyExpr expr2 elt_ty
        ; expr3' <- tcPolyExpr expr3 elt_ty
        ; enumFromThenToP <- initDsTc $ dsDPHBuiltin enumFromThenToPVar
        ; eft <- newMethodFromName (PArrSeqOrigin seq)
                      (idName enumFromThenToP) elt_ty        -- !!!FIXME: chak
        ; return $
          mkHsWrapCo coi $
          PArrSeq eft (FromThenTo expr1' expr2' expr3') }

tcExpr (PArrSeq _ _) _
  = panic "TcExpr.tcExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer shouldn't have
    -- let it through

{-
************************************************************************
*                                                                      *
                Template Haskell
*                                                                      *
************************************************************************
-}

-- HsSpliced is an annotation produced by 'RnSplice.rnSpliceExpr'.
-- Here we get rid of it and add the finalizers to the global environment.
--
-- See Note [Delaying modFinalizers in untyped splices] in RnSplice.
tcExpr (HsSpliceE (HsSpliced mod_finalizers (HsSplicedExpr expr)))
       res_ty
  = do addModFinalizersWithLclEnv mod_finalizers
       tcExpr expr res_ty
tcExpr (HsSpliceE splice)        res_ty
  = tcSpliceExpr splice res_ty
tcExpr e@(HsBracket brack)         res_ty
  = tcTypedBracket e brack res_ty
tcExpr e@(HsRnBracketOut brack ps) res_ty
  = tcUntypedBracket e brack ps res_ty

{-
************************************************************************
*                                                                      *
                Catch-all
*                                                                      *
************************************************************************
-}

tcExpr other _ = pprPanic "tcMonoExpr" (ppr other)
  -- Include ArrForm, ArrApp, which shouldn't appear at all
  -- Also HsTcBracketOut, HsQuasiQuoteE

{-
************************************************************************
*                                                                      *
                Arithmetic sequences [a..b] etc
*                                                                      *
************************************************************************
-}

tcArithSeq :: Maybe (SyntaxExpr GhcRn) -> ArithSeqInfo GhcRn -> ExpRhoType
           -> TcM (HsExpr GhcTcId)

tcArithSeq witness seq@(From expr) res_ty
  = do { (wrap, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr' <- tcPolyExpr expr elt_ty
       ; enum_from <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromName elt_ty
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from wit' (From expr') }

tcArithSeq witness seq@(FromThen expr1 expr2) res_ty
  = do { (wrap, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr1' <- tcPolyExpr expr1 elt_ty
       ; expr2' <- tcPolyExpr expr2 elt_ty
       ; enum_from_then <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromThenName elt_ty
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from_then wit' (FromThen expr1' expr2') }

tcArithSeq witness seq@(FromTo expr1 expr2) res_ty
  = do { (wrap, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr1' <- tcPolyExpr expr1 elt_ty
       ; expr2' <- tcPolyExpr expr2 elt_ty
       ; enum_from_to <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromToName elt_ty
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from_to wit' (FromTo expr1' expr2') }

tcArithSeq witness seq@(FromThenTo expr1 expr2 expr3) res_ty
  = do { (wrap, elt_ty, wit') <- arithSeqEltType witness res_ty
        ; expr1' <- tcPolyExpr expr1 elt_ty
        ; expr2' <- tcPolyExpr expr2 elt_ty
        ; expr3' <- tcPolyExpr expr3 elt_ty
        ; eft <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromThenToName elt_ty
        ; return $ mkHsWrap wrap $
          ArithSeq eft wit' (FromThenTo expr1' expr2' expr3') }

-----------------
arithSeqEltType :: Maybe (SyntaxExpr GhcRn) -> ExpRhoType
                -> TcM (HsWrapper, TcType, Maybe (SyntaxExpr GhcTc))
arithSeqEltType Nothing res_ty
  = do { res_ty <- expTypeToType res_ty
       ; (coi, elt_ty) <- matchExpectedListTy res_ty
       ; return (mkWpCastN coi, elt_ty, Nothing) }
arithSeqEltType (Just fl) res_ty
  = do { (elt_ty, fl')
           <- tcSyntaxOp ListOrigin fl [SynList] res_ty $
              \ [elt_ty] -> return elt_ty
       ; return (idHsWrapper, elt_ty, Just fl') }

{-
************************************************************************
*                                                                      *
                Applications
*                                                                      *
************************************************************************
-}

type LHsExprArgIn  = Either (LHsExpr GhcRn) (LHsWcType GhcRn)
type LHsExprArgOut = Either (LHsExpr GhcTcId) (LHsWcType GhcRn)
   -- Left e   => argument expression
   -- Right ty => visible type application

tcApp1 :: HsExpr GhcRn  -- either HsApp or HsAppType
       -> ExpRhoType -> TcM (HsExpr GhcTcId)
tcApp1 e res_ty
  = do { (wrap, fun, args) <- tcApp Nothing (noLoc e) [] res_ty
       ; return (mkHsWrap wrap $ unLoc $ foldl mk_hs_app fun args) }
  where
    mk_hs_app f (Left a)  = mkHsApp f a
    mk_hs_app f (Right a) = mkHsAppTypeOut f a

tcApp :: Maybe SDoc  -- like "The function `f' is applied to"
                     -- or leave out to get exactly that message
      -> LHsExpr GhcRn -> [LHsExprArgIn] -- Function and args
      -> ExpRhoType -> TcM (HsWrapper, LHsExpr GhcTcId, [LHsExprArgOut])
           -- (wrap, fun, args). For an ordinary function application,
           -- these should be assembled as (wrap (fun args)).
           -- But OpApp is slightly different, so that's why the caller
           -- must assemble

tcApp m_herald orig_fun orig_args res_ty
  = go orig_fun orig_args
  where
    go :: LHsExpr GhcRn -> [LHsExprArgIn]
       -> TcM (HsWrapper, LHsExpr GhcTcId, [LHsExprArgOut])
    go (L _ (HsPar e))       args = go e  args
    go (L _ (HsApp e1 e2))   args = go e1 (Left e2:args)
    go (L _ (HsAppType e t)) args = go e  (Right t:args)

    go (L loc (HsVar (L _ fun))) args
      | fun `hasKey` tagToEnumKey
      , count isLeft args == 1
      = do { (wrap, expr, args) <- tcTagToEnum loc fun args res_ty
           ; return (wrap, expr, args) }

      | fun `hasKey` seqIdKey
      , count isLeft args == 2
      = do { (wrap, expr, args) <- tcSeq loc fun args res_ty
           ; return (wrap, expr, args) }

    go (L loc (HsRecFld (Ambiguous lbl _))) args@(Left (L _ arg) : _)
      | Just sig_ty <- obviousSig arg
      = do { sig_tc_ty <- tcHsSigWcType ExprSigCtxt sig_ty
           ; sel_name  <- disambiguateSelector lbl sig_tc_ty
           ; go (L loc (HsRecFld (Unambiguous lbl sel_name))) args }

    -- See Note [Visible type application for the empty list constructor]
    go (L loc (ExplicitList _ Nothing [])) [Right ty_arg]
      = do { ty_arg' <- tcHsTypeApp ty_arg liftedTypeKind
           ; let list_ty = TyConApp listTyCon [ty_arg']
           ; _ <- tcSubTypeDS (OccurrenceOf nilDataConName) GenSigCtxt
                              list_ty res_ty
           ; let expr :: LHsExpr GhcTcId
                 expr = L loc $ ExplicitList ty_arg' Nothing []
           ; return (idHsWrapper, expr, []) }

    go fun args
      = do {   -- Type-check the function
           ; (fun1, fun_sigma) <- tcInferFun fun
           ; let orig = lexprCtOrigin fun

           ; (wrap_fun, args1, actual_res_ty)
               <- tcArgs fun fun_sigma orig args
                         (m_herald `orElse` mk_app_msg fun)

                -- this is just like tcWrapResult, but the types don't line
                -- up to call that function
           ; wrap_res <- addFunResCtxt True (unLoc fun) actual_res_ty res_ty $
                         tcSubTypeDS_NC_O orig GenSigCtxt
                           (Just $ unLoc $ foldl mk_hs_app fun args)
                           actual_res_ty res_ty

           ; return (wrap_res, mkLHsWrap wrap_fun fun1, args1) }

    mk_hs_app f (Left a)  = mkHsApp f a
    mk_hs_app f (Right a) = mkHsAppType f a

mk_app_msg :: LHsExpr GhcRn -> SDoc
mk_app_msg fun = sep [ text "The function" <+> quotes (ppr fun)
                     , text "is applied to"]

mk_op_msg :: LHsExpr GhcRn -> SDoc
mk_op_msg op = text "The operator" <+> quotes (ppr op) <+> text "takes"

{-
Note [Visible type application for the empty list constructor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Getting the expression [] @Int to typecheck is slightly tricky since [] isn't
an ordinary data constructor. By default, when tcExpr typechecks a list
expression, it wraps the expression in a coercion, which gives it a type to the
effect of p[a]. It isn't until later zonking that the type becomes
forall a. [a], but that's too late for visible type application.

The workaround is to check for empty list expressions that have a visible type
argument in tcApp, and if so, directly typecheck [] @ty data constructor name.
This avoids the intermediate coercion and produces an expression of type [ty],
as one would intuitively expect.

Unfortunately, this workaround isn't terribly robust, since more involved
expressions such as (let in []) @Int won't work. Until a more elegant fix comes
along, however, this at least allows direct type application on [] to work,
which is better than before.
-}

----------------
tcInferFun :: LHsExpr GhcRn -> TcM (LHsExpr GhcTcId, TcSigmaType)
-- Infer type of a function
tcInferFun (L loc (HsVar (L _ name)))
  = do { (fun, ty) <- setSrcSpan loc (tcInferId name)
               -- Don't wrap a context around a plain Id
       ; return (L loc fun, ty) }

tcInferFun (L loc (HsRecFld f))
  = do { (fun, ty) <- setSrcSpan loc (tcInferRecSelId f)
               -- Don't wrap a context around a plain Id
       ; return (L loc fun, ty) }

tcInferFun fun
  = tcInferSigma fun
      -- NB: tcInferSigma; see TcUnify
      -- Note [Deep instantiation of InferResult]


----------------
-- | Type-check the arguments to a function, possibly including visible type
-- applications
tcArgs :: LHsExpr GhcRn   -- ^ The function itself (for err msgs only)
       -> TcSigmaType    -- ^ the (uninstantiated) type of the function
       -> CtOrigin       -- ^ the origin for the function's type
       -> [LHsExprArgIn] -- ^ the args
       -> SDoc           -- ^ the herald for matchActualFunTys
       -> TcM (HsWrapper, [LHsExprArgOut], TcSigmaType)
          -- ^ (a wrapper for the function, the tc'd args, result type)
tcArgs fun orig_fun_ty fun_orig orig_args herald
  = go [] 1 orig_fun_ty orig_args
  where
    orig_arity = length orig_args

    go _ _ fun_ty [] = return (idHsWrapper, [], fun_ty)

    go acc_args n fun_ty (Right hs_ty_arg:args)
      = do { (wrap1, upsilon_ty) <- topInstantiateInferred fun_orig fun_ty
               -- wrap1 :: fun_ty "->" upsilon_ty
           ; case tcSplitForAllTy_maybe upsilon_ty of
               Just (tvb, inner_ty) ->
                 do { let tv   = binderVar tvb
                          vis  = binderArgFlag tvb
                          kind = tyVarKind tv
                    ; MASSERT2( vis == Specified
                        , (vcat [ ppr fun_ty, ppr upsilon_ty, ppr tvb
                                , ppr inner_ty, pprTyVar tv
                                , ppr vis ]) )
                    ; ty_arg <- tcHsTypeApp hs_ty_arg kind
                    ; let insted_ty = substTyWithUnchecked [tv] [ty_arg] inner_ty
                    ; (inner_wrap, args', res_ty)
                        <- go acc_args (n+1) insted_ty args
                   -- inner_wrap :: insted_ty "->" (map typeOf args') -> res_ty
                    ; let inst_wrap = mkWpTyApps [ty_arg]
                    ; return ( inner_wrap <.> inst_wrap <.> wrap1
                             , Right hs_ty_arg : args'
                             , res_ty ) }
               _ -> ty_app_err upsilon_ty hs_ty_arg }

    go acc_args n fun_ty (Left arg : args)
      = do { (wrap, [arg_ty], res_ty)
               <- matchActualFunTysPart herald fun_orig (Just (unLoc fun)) 1 fun_ty
                                        acc_args orig_arity
               -- wrap :: fun_ty "->" arg_ty -> res_ty
           ; arg' <- tcArg fun arg arg_ty n
           ; (inner_wrap, args', inner_res_ty)
               <- go (arg_ty : acc_args) (n+1) res_ty args
               -- inner_wrap :: res_ty "->" (map typeOf args') -> inner_res_ty
           ; return ( mkWpFun idHsWrapper inner_wrap arg_ty res_ty doc <.> wrap
                    , Left arg' : args'
                    , inner_res_ty ) }
      where
        doc = text "When checking the" <+> speakNth n <+>
              text "argument to" <+> quotes (ppr fun)

    ty_app_err ty arg
      = do { (_, ty) <- zonkTidyTcType emptyTidyEnv ty
           ; failWith $
               text "Cannot apply expression of type" <+> quotes (ppr ty) $$
               text "to a visible type argument" <+> quotes (ppr arg) }

----------------
tcArg :: LHsExpr GhcRn                   -- The function (for error messages)
      -> LHsExpr GhcRn                   -- Actual arguments
      -> TcRhoType                       -- expected arg type
      -> Int                             -- # of argument
      -> TcM (LHsExpr GhcTcId)           -- Resulting argument
tcArg fun arg ty arg_no = addErrCtxt (funAppCtxt fun arg arg_no) $
                          tcPolyExprNC arg ty

----------------
tcTupArgs :: [LHsTupArg GhcRn] -> [TcSigmaType] -> TcM [LHsTupArg GhcTcId]
tcTupArgs args tys
  = ASSERT( equalLength args tys ) mapM go (args `zip` tys)
  where
    go (L l (Missing {}),   arg_ty) = return (L l (Missing arg_ty))
    go (L l (Present expr), arg_ty) = do { expr' <- tcPolyExpr expr arg_ty
                                         ; return (L l (Present expr')) }

---------------------------
-- See TcType.SyntaxOpType also for commentary
tcSyntaxOp :: CtOrigin
           -> SyntaxExpr GhcRn
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpRhoType               -- ^ overall result type
           -> ([TcSigmaType] -> TcM a) -- ^ Type check any arguments
           -> TcM (a, SyntaxExpr GhcTcId)
-- ^ Typecheck a syntax operator
-- The operator is always a variable at this stage (i.e. renamer output)
tcSyntaxOp orig expr arg_tys res_ty
  = tcSyntaxOpGen orig expr arg_tys (SynType res_ty)

-- | Slightly more general version of 'tcSyntaxOp' that allows the caller
-- to specify the shape of the result of the syntax operator
tcSyntaxOpGen :: CtOrigin
              -> SyntaxExpr GhcRn
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaType] -> TcM a)
              -> TcM (a, SyntaxExpr GhcTcId)
tcSyntaxOpGen orig (SyntaxExpr { syn_expr = HsVar (L _ op) })
              arg_tys res_ty thing_inside
  = do { (expr, sigma) <- tcInferId op
       ; (result, expr_wrap, arg_wraps, res_wrap)
           <- tcSynArgA orig sigma arg_tys res_ty $
              thing_inside
       ; return (result, SyntaxExpr { syn_expr      = mkHsWrap expr_wrap expr
                                    , syn_arg_wraps = arg_wraps
                                    , syn_res_wrap  = res_wrap }) }

tcSyntaxOpGen _ other _ _ _ = pprPanic "tcSyntaxOp" (ppr other)

{-
Note [tcSynArg]
~~~~~~~~~~~~~~~
Because of the rich structure of SyntaxOpType, we must do the
contra-/covariant thing when working down arrows, to get the
instantiation vs. skolemisation decisions correct (and, more
obviously, the orientation of the HsWrappers). We thus have
two tcSynArgs.
-}

-- works on "expected" types, skolemising where necessary
-- See Note [tcSynArg]
tcSynArgE :: CtOrigin
          -> TcSigmaType
          -> SyntaxOpType                -- ^ shape it is expected to have
          -> ([TcSigmaType] -> TcM a)    -- ^ check the arguments
          -> TcM (a, HsWrapper)
           -- ^ returns a wrapper :: (type of right shape) "->" (type passed in)
tcSynArgE orig sigma_ty syn_ty thing_inside
  = do { (skol_wrap, (result, ty_wrapper))
           <- tcSkolemise GenSigCtxt sigma_ty $ \ _ rho_ty ->
              go rho_ty syn_ty
       ; return (result, skol_wrap <.> ty_wrapper) }
    where
    go rho_ty SynAny
      = do { result <- thing_inside [rho_ty]
           ; return (result, idHsWrapper) }

    go rho_ty SynRho   -- same as SynAny, because we skolemise eagerly
      = do { result <- thing_inside [rho_ty]
           ; return (result, idHsWrapper) }

    go rho_ty SynList
      = do { (list_co, elt_ty) <- matchExpectedListTy rho_ty
           ; result <- thing_inside [elt_ty]
           ; return (result, mkWpCastN list_co) }

    go rho_ty (SynFun arg_shape res_shape)
      = do { ( ( ( (result, arg_ty, res_ty)
                 , res_wrapper )                   -- :: res_ty_out "->" res_ty
               , arg_wrapper1, [], arg_wrapper2 )  -- :: arg_ty "->" arg_ty_out
             , match_wrapper )         -- :: (arg_ty -> res_ty) "->" rho_ty
               <- matchExpectedFunTys herald 1 (mkCheckExpType rho_ty) $
                  \ [arg_ty] res_ty ->
                  do { arg_tc_ty <- expTypeToType arg_ty
                     ; res_tc_ty <- expTypeToType res_ty

                         -- another nested arrow is too much for now,
                         -- but I bet we'll never need this
                     ; MASSERT2( case arg_shape of
                                   SynFun {} -> False;
                                   _         -> True
                               , text "Too many nested arrows in SyntaxOpType" $$
                                 pprCtOrigin orig )

                     ; tcSynArgA orig arg_tc_ty [] arg_shape $
                       \ arg_results ->
                       tcSynArgE orig res_tc_ty res_shape $
                       \ res_results ->
                       do { result <- thing_inside (arg_results ++ res_results)
                          ; return (result, arg_tc_ty, res_tc_ty) }}

           ; return ( result
                    , match_wrapper <.>
                      mkWpFun (arg_wrapper2 <.> arg_wrapper1) res_wrapper
                              arg_ty res_ty doc ) }
      where
        herald = text "This rebindable syntax expects a function with"
        doc = text "When checking a rebindable syntax operator arising from" <+> ppr orig

    go rho_ty (SynType the_ty)
      = do { wrap   <- tcSubTypeET orig GenSigCtxt the_ty rho_ty
           ; result <- thing_inside []
           ; return (result, wrap) }

-- works on "actual" types, instantiating where necessary
-- See Note [tcSynArg]
tcSynArgA :: CtOrigin
          -> TcSigmaType
          -> [SyntaxOpType]              -- ^ argument shapes
          -> SyntaxOpType                -- ^ result shape
          -> ([TcSigmaType] -> TcM a)    -- ^ check the arguments
          -> TcM (a, HsWrapper, [HsWrapper], HsWrapper)
            -- ^ returns a wrapper to be applied to the original function,
            -- wrappers to be applied to arguments
            -- and a wrapper to be applied to the overall expression
tcSynArgA orig sigma_ty arg_shapes res_shape thing_inside
  = do { (match_wrapper, arg_tys, res_ty)
           <- matchActualFunTys herald orig Nothing (length arg_shapes) sigma_ty
              -- match_wrapper :: sigma_ty "->" (arg_tys -> res_ty)
       ; ((result, res_wrapper), arg_wrappers)
           <- tc_syn_args_e arg_tys arg_shapes $ \ arg_results ->
              tc_syn_arg    res_ty  res_shape  $ \ res_results ->
              thing_inside (arg_results ++ res_results)
       ; return (result, match_wrapper, arg_wrappers, res_wrapper) }
  where
    herald = text "This rebindable syntax expects a function with"

    tc_syn_args_e :: [TcSigmaType] -> [SyntaxOpType]
                  -> ([TcSigmaType] -> TcM a)
                  -> TcM (a, [HsWrapper])
                    -- the wrappers are for arguments
    tc_syn_args_e (arg_ty : arg_tys) (arg_shape : arg_shapes) thing_inside
      = do { ((result, arg_wraps), arg_wrap)
               <- tcSynArgE     orig arg_ty  arg_shape  $ \ arg1_results ->
                  tc_syn_args_e      arg_tys arg_shapes $ \ args_results ->
                  thing_inside (arg1_results ++ args_results)
           ; return (result, arg_wrap : arg_wraps) }
    tc_syn_args_e _ _ thing_inside = (, []) <$> thing_inside []

    tc_syn_arg :: TcSigmaType -> SyntaxOpType
               -> ([TcSigmaType] -> TcM a)
               -> TcM (a, HsWrapper)
                  -- the wrapper applies to the overall result
    tc_syn_arg res_ty SynAny thing_inside
      = do { result <- thing_inside [res_ty]
           ; return (result, idHsWrapper) }
    tc_syn_arg res_ty SynRho thing_inside
      = do { (inst_wrap, rho_ty) <- deeplyInstantiate orig res_ty
               -- inst_wrap :: res_ty "->" rho_ty
           ; result <- thing_inside [rho_ty]
           ; return (result, inst_wrap) }
    tc_syn_arg res_ty SynList thing_inside
      = do { (inst_wrap, rho_ty) <- topInstantiate orig res_ty
               -- inst_wrap :: res_ty "->" rho_ty
           ; (list_co, elt_ty)   <- matchExpectedListTy rho_ty
               -- list_co :: [elt_ty] ~N rho_ty
           ; result <- thing_inside [elt_ty]
           ; return (result, mkWpCastN (mkTcSymCo list_co) <.> inst_wrap) }
    tc_syn_arg _ (SynFun {}) _
      = pprPanic "tcSynArgA hits a SynFun" (ppr orig)
    tc_syn_arg res_ty (SynType the_ty) thing_inside
      = do { wrap   <- tcSubTypeO orig GenSigCtxt res_ty the_ty
           ; result <- thing_inside []
           ; return (result, wrap) }

{-
Note [Push result type in]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Unify with expected result before type-checking the args so that the
info from res_ty percolates to args.  This is when we might detect a
too-few args situation.  (One can think of cases when the opposite
order would give a better error message.)
experimenting with putting this first.

Here's an example where it actually makes a real difference

   class C t a b | t a -> b
   instance C Char a Bool

   data P t a = forall b. (C t a b) => MkP b
   data Q t   = MkQ (forall a. P t a)

   f1, f2 :: Q Char;
   f1 = MkQ (MkP True)
   f2 = MkQ (MkP True :: forall a. P Char a)

With the change, f1 will type-check, because the 'Char' info from
the signature is propagated into MkQ's argument. With the check
in the other order, the extra signature in f2 is reqd.

************************************************************************
*                                                                      *
                Expressions with a type signature
                        expr :: type
*                                                                      *
********************************************************************* -}

tcExprSig :: LHsExpr GhcRn -> TcIdSigInfo -> TcM (LHsExpr GhcTcId, TcType)
tcExprSig expr (CompleteSig { sig_bndr = poly_id, sig_loc = loc })
  = setSrcSpan loc $   -- Sets the location for the implication constraint
    do { (tv_prs, theta, tau) <- tcInstType tcInstSkolTyVars poly_id
       ; given <- newEvVars theta
       ; let skol_info = SigSkol ExprSigCtxt (idType poly_id) tv_prs
             skol_tvs  = map snd tv_prs
       ; (ev_binds, expr') <- checkConstraints skol_info skol_tvs given $
                              tcExtendTyVarEnv2 tv_prs $
                              tcPolyExprNC expr tau

       ; let poly_wrap = mkWpTyLams   skol_tvs
                         <.> mkWpLams given
                         <.> mkWpLet  ev_binds
       ; return (mkLHsWrap poly_wrap expr', idType poly_id) }

tcExprSig expr sig@(PartialSig { psig_name = name, sig_loc = loc })
  = setSrcSpan loc $   -- Sets the location for the implication constraint
    do { (tclvl, wanted, (expr', sig_inst))
             <- pushLevelAndCaptureConstraints  $
                do { sig_inst <- tcInstSig sig
                   ; expr' <- tcExtendTyVarEnv2 (sig_inst_skols sig_inst) $
                              tcExtendTyVarEnv2 (sig_inst_wcs   sig_inst) $
                              tcPolyExprNC expr (sig_inst_tau sig_inst)
                   ; return (expr', sig_inst) }
       -- See Note [Partial expression signatures]
       ; let tau = sig_inst_tau sig_inst
             infer_mode | null (sig_inst_theta sig_inst)
                        , isNothing (sig_inst_wcx sig_inst)
                        = ApplyMR
                        | otherwise
                        = NoRestrictions
       ; (qtvs, givens, ev_binds)
                 <- simplifyInfer tclvl infer_mode [sig_inst] [(name, tau)] wanted
       ; tau <- zonkTcType tau
       ; let inferred_theta = map evVarPred givens
             tau_tvs        = tyCoVarsOfType tau
       ; (binders, my_theta) <- chooseInferredQuantifiers inferred_theta
                                   tau_tvs qtvs (Just sig_inst)
       ; let inferred_sigma = mkInfSigmaTy qtvs inferred_theta tau
             my_sigma       = mkForAllTys binders (mkPhiTy  my_theta tau)
       ; wrap <- if inferred_sigma `eqType` my_sigma -- NB: eqType ignores vis.
                 then return idHsWrapper  -- Fast path; also avoids complaint when we infer
                                          -- an ambiguous type and have AllowAmbiguousType
                                          -- e..g infer  x :: forall a. F a -> Int
                 else tcSubType_NC ExprSigCtxt inferred_sigma my_sigma

       ; traceTc "tcExpSig" (ppr qtvs $$ ppr givens $$ ppr inferred_sigma $$ ppr my_sigma)
       ; let poly_wrap = wrap
                         <.> mkWpTyLams qtvs
                         <.> mkWpLams givens
                         <.> mkWpLet  ev_binds
       ; return (mkLHsWrap poly_wrap expr', my_sigma) }


{- Note [Partial expression signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Partial type signatures on expressions are easy to get wrong.  But
here is a guiding principile
    e :: ty
should behave like
    let x :: ty
        x = e
    in x

So for partial signatures we apply the MR if no context is given.  So
   e :: IO _          apply the MR
   e :: _ => IO _     do not apply the MR
just like in TcBinds.decideGeneralisationPlan

This makes a difference (Trac #11670):
   peek :: Ptr a -> IO CLong
   peek ptr = peekElemOff undefined 0 :: _
from (peekElemOff undefined 0) we get
          type: IO w
   constraints: Storable w

We must NOT try to generalise over 'w' because the signature specifies
no constraints so we'll complain about not being able to solve
Storable w.  Instead, don't generalise; then _ gets instantiated to
CLong, as it should.
-}

{- *********************************************************************
*                                                                      *
                 tcInferId
*                                                                      *
********************************************************************* -}

tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr GhcTcId)
tcCheckId name res_ty
  = do { (expr, actual_res_ty) <- tcInferId name
       ; traceTc "tcCheckId" (vcat [ppr name, ppr actual_res_ty, ppr res_ty])
       ; addFunResCtxt False (HsVar (noLoc name)) actual_res_ty res_ty $
         tcWrapResultO (OccurrenceOf name) (HsVar (noLoc name)) expr actual_res_ty res_ty }

tcCheckRecSelId :: HsExpr GhcRn -> AmbiguousFieldOcc GhcRn -> ExpRhoType -> TcM (HsExpr GhcTcId)
tcCheckRecSelId rn_expr f@(Unambiguous (L _ lbl) _) res_ty
  = do { (expr, actual_res_ty) <- tcInferRecSelId f
       ; addFunResCtxt False (HsRecFld f) actual_res_ty res_ty $
         tcWrapResultO (OccurrenceOfRecSel lbl) rn_expr expr actual_res_ty res_ty }
tcCheckRecSelId rn_expr (Ambiguous lbl _) res_ty
  = case tcSplitFunTy_maybe =<< checkingExpType_maybe res_ty of
      Nothing       -> ambiguousSelector lbl
      Just (arg, _) -> do { sel_name <- disambiguateSelector lbl arg
                          ; tcCheckRecSelId rn_expr (Unambiguous lbl sel_name) res_ty }

------------------------
tcInferRecSelId :: AmbiguousFieldOcc GhcRn -> TcM (HsExpr GhcTcId, TcRhoType)
tcInferRecSelId (Unambiguous (L _ lbl) sel)
  = do { (expr', ty) <- tc_infer_id lbl sel
       ; return (expr', ty) }
tcInferRecSelId (Ambiguous lbl _)
  = ambiguousSelector lbl

------------------------
tcInferId :: Name -> TcM (HsExpr GhcTcId, TcSigmaType)
-- Look up an occurrence of an Id
-- Do not instantiate its type
tcInferId id_name
  | id_name `hasKey` tagToEnumKey
  = failWithTc (text "tagToEnum# must appear applied to one argument")
        -- tcApp catches the case (tagToEnum# arg)

  | id_name `hasKey` assertIdKey
  = do { dflags <- getDynFlags
       ; if gopt Opt_IgnoreAsserts dflags
         then tc_infer_id (nameRdrName id_name) id_name
         else tc_infer_assert id_name }

  | otherwise
  = do { (expr, ty) <- tc_infer_id (nameRdrName id_name) id_name
       ; traceTc "tcInferId" (ppr id_name <+> dcolon <+> ppr ty)
       ; return (expr, ty) }

tc_infer_assert :: Name -> TcM (HsExpr GhcTcId, TcSigmaType)
-- Deal with an occurrence of 'assert'
-- See Note [Adding the implicit parameter to 'assert']
tc_infer_assert assert_name
  = do { assert_error_id <- tcLookupId assertErrorName
       ; (wrap, id_rho) <- topInstantiate (OccurrenceOf assert_name)
                                          (idType assert_error_id)
       ; return (mkHsWrap wrap (HsVar (noLoc assert_error_id)), id_rho)
       }

tc_infer_id :: RdrName -> Name -> TcM (HsExpr GhcTcId, TcSigmaType)
tc_infer_id lbl id_name
 = do { thing <- tcLookup id_name
      ; case thing of
             ATcId { tct_id = id }
               -> do { check_naughty id        -- Note [Local record selectors]
                     ; checkThLocalId id
                     ; return_id id }

             AGlobal (AnId id)
               -> do { check_naughty id
                     ; return_id id }
                    -- A global cannot possibly be ill-staged
                    -- nor does it need the 'lifting' treatment
                    -- hence no checkTh stuff here

             AGlobal (AConLike cl) -> case cl of
                 RealDataCon con -> return_data_con con
                 PatSynCon ps    -> tcPatSynBuilderOcc ps

             _ -> failWithTc $
                  ppr thing <+> text "used where a value identifier was expected" }
  where
    return_id id = return (HsVar (noLoc id), idType id)

    return_data_con con
       -- For data constructors, must perform the stupid-theta check
      | null stupid_theta
      = return (HsConLikeOut (RealDataCon con), con_ty)

      | otherwise
       -- See Note [Instantiating stupid theta]
      = do { let (tvs, theta, rho) = tcSplitSigmaTy con_ty
           ; (subst, tvs') <- newMetaTyVars tvs
           ; let tys'   = mkTyVarTys tvs'
                 theta' = substTheta subst theta
                 rho'   = substTy subst rho
           ; wrap <- instCall (OccurrenceOf id_name) tys' theta'
           ; addDataConStupidTheta con tys'
           ; return (mkHsWrap wrap (HsConLikeOut (RealDataCon con)), rho') }

      where
        con_ty         = dataConUserType con
        stupid_theta   = dataConStupidTheta con

    check_naughty id
      | isNaughtyRecordSelector id = failWithTc (naughtyRecordSel lbl)
      | otherwise                  = return ()


tcUnboundId :: HsExpr GhcRn -> UnboundVar -> ExpRhoType -> TcM (HsExpr GhcTcId)
-- Typecheck an occurrence of an unbound Id
--
-- Some of these started life as a true expression hole "_".
-- Others might simply be variables that accidentally have no binding site
--
-- We turn all of them into HsVar, since HsUnboundVar can't contain an
-- Id; and indeed the evidence for the CHoleCan does bind it, so it's
-- not unbound any more!
tcUnboundId rn_expr unbound res_ty
 = do { ty <- newOpenFlexiTyVarTy  -- Allow Int# etc (Trac #12531)
      ; let occ = unboundVarOcc unbound
      ; name <- newSysName occ
      ; let ev = mkLocalId name ty
      ; loc <- getCtLocM HoleOrigin Nothing
      ; let can = CHoleCan { cc_ev = CtWanted { ctev_pred = ty
                                              , ctev_dest = EvVarDest ev
                                              , ctev_nosh = WDeriv
                                              , ctev_loc  = loc}
                           , cc_hole = ExprHole unbound }
      ; emitInsoluble can
      ; tcWrapResultO (UnboundOccurrenceOf occ) rn_expr (HsVar (noLoc ev)) ty res_ty }


{-
Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker transforms (assert e1 e2) to (assertError e1 e2).
This isn't really the Right Thing because there's no way to "undo"
if you want to see the original source code in the typechecker
output.  We'll have fix this in due course, when we care more about
being able to reconstruct the exact original program.

Note [tagToEnum#]
~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude, because it relies on our
knowing *now* that the type is ok, which in turn relies on the
eager-unification part of the type checker pushing enough information
here.  In theory the Right Thing to do is to have a new form of
constraint but I definitely cannot face that!  And it works ok as-is.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

When data type families are involved it's a bit more complicated.
     data family F a
     data instance F [Int] = A | B | C
Then we want to generate something like
     tagToEnum# R:FListInt 3# |> co :: R:FListInt ~ F [Int]
Usually that coercion is hidden inside the wrappers for
constructors of F [Int] but here we have to do it explicitly.

It's all grotesquely complicated.

Note [Instantiating stupid theta]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, when we infer the type of an Id, we don't instantiate,
because we wish to allow for visible type application later on.
But if a datacon has a stupid theta, we're a bit stuck. We need
to emit the stupid theta constraints with instantiated types. It's
difficult to defer this to the lazy instantiation, because a stupid
theta has no spot to put it in a type. So we just instantiate eagerly
in this case. Thus, users cannot use visible type application with
a data constructor sporting a stupid theta. I won't feel so bad for
the users that complain.

-}

tcSeq :: SrcSpan -> Name -> [LHsExprArgIn]
      -> ExpRhoType -> TcM (HsWrapper, LHsExpr GhcTcId, [LHsExprArgOut])
-- (seq e1 e2) :: res_ty
-- We need a special typing rule because res_ty can be unboxed
-- See Note [Typing rule for seq]
tcSeq loc fun_name args res_ty
  = do  { fun <- tcLookupId fun_name
        ; (arg1_ty, args1) <- case args of
            (Right hs_ty_arg1 : args1)
              -> do { ty_arg1 <- tcHsTypeApp hs_ty_arg1 liftedTypeKind
                    ; return (ty_arg1, args1) }

            _ -> do { arg_ty1 <- newFlexiTyVarTy liftedTypeKind
                    ; return (arg_ty1, args) }

        ; (arg1, arg2, arg2_exp_ty) <- case args1 of
            [Right hs_ty_arg2, Left term_arg1, Left term_arg2]
              -> do { arg2_kind <- newOpenTypeKind
                    ; ty_arg2 <- tcHsTypeApp hs_ty_arg2 arg2_kind
                                   -- see Note [Typing rule for seq]
                    ; _ <- tcSubTypeDS (OccurrenceOf fun_name) GenSigCtxt ty_arg2 res_ty
                    ; return (term_arg1, term_arg2, mkCheckExpType ty_arg2) }
            [Left term_arg1, Left term_arg2]
              -> return (term_arg1, term_arg2, res_ty)
            _ -> too_many_args "seq" args

        ; arg1' <- tcMonoExpr arg1 (mkCheckExpType arg1_ty)
        ; arg2' <- tcMonoExpr arg2 arg2_exp_ty
        ; res_ty <- readExpType res_ty  -- by now, it's surely filled in
        ; let fun'    = L loc (mkHsWrap ty_args (HsVar (L loc fun)))
              ty_args = WpTyApp res_ty <.> WpTyApp arg1_ty
        ; return (idHsWrapper, fun', [Left arg1', Left arg2']) }

tcTagToEnum :: SrcSpan -> Name -> [LHsExprArgIn] -> ExpRhoType
            -> TcM (HsWrapper, LHsExpr GhcTcId, [LHsExprArgOut])
-- tagToEnum# :: forall a. Int# -> a
-- See Note [tagToEnum#]   Urgh!
tcTagToEnum loc fun_name args res_ty
  = do { fun <- tcLookupId fun_name

       ; arg <- case args of
           [Right hs_ty_arg, Left term_arg]
             -> do { ty_arg <- tcHsTypeApp hs_ty_arg liftedTypeKind
                   ; _ <- tcSubTypeDS (OccurrenceOf fun_name) GenSigCtxt ty_arg res_ty
                     -- other than influencing res_ty, we just
                     -- don't care about a type arg passed in.
                     -- So drop the evidence.
                   ; return term_arg }
           [Left term_arg] -> do { _ <- expTypeToType res_ty
                                 ; return term_arg }
           _          -> too_many_args "tagToEnum#" args

       ; res_ty <- readExpType res_ty
       ; ty'    <- zonkTcType res_ty

       -- Check that the type is algebraic
       ; let mb_tc_app = tcSplitTyConApp_maybe ty'
             Just (tc, tc_args) = mb_tc_app
       ; checkTc (isJust mb_tc_app)
                 (mk_error ty' doc1)

       -- Look through any type family
       ; fam_envs <- tcGetFamInstEnvs
       ; let (rep_tc, rep_args, coi)
               = tcLookupDataFamInst fam_envs tc tc_args
            -- coi :: tc tc_args ~R rep_tc rep_args

       ; checkTc (isEnumerationTyCon rep_tc)
                 (mk_error ty' doc2)

       ; arg' <- tcMonoExpr arg (mkCheckExpType intPrimTy)
       ; let fun' = L loc (mkHsWrap (WpTyApp rep_ty) (HsVar (L loc fun)))
             rep_ty = mkTyConApp rep_tc rep_args

       ; return (mkWpCastR (mkTcSymCo coi), fun', [Left arg']) }
                 -- coi is a Representational coercion
  where
    doc1 = vcat [ text "Specify the type by giving a type signature"
                , text "e.g. (tagToEnum# x) :: Bool" ]
    doc2 = text "Result type must be an enumeration type"

    mk_error :: TcType -> SDoc -> SDoc
    mk_error ty what
      = hang (text "Bad call to tagToEnum#"
               <+> text "at type" <+> ppr ty)
           2 what

too_many_args :: String -> [LHsExprArgIn] -> TcM a
too_many_args fun args
  = failWith $
    hang (text "Too many type arguments to" <+> text fun <> colon)
       2 (sep (map pp args))
  where
    pp (Left e)                             = ppr e
    pp (Right (HsWC { hswc_body = L _ t })) = pprHsType t


{-
************************************************************************
*                                                                      *
                 Template Haskell checks
*                                                                      *
************************************************************************
-}

checkThLocalId :: Id -> TcM ()
checkThLocalId id
  = do  { mb_local_use <- getStageAndBindLevel (idName id)
        ; case mb_local_use of
             Just (top_lvl, bind_lvl, use_stage)
                | thLevel use_stage > bind_lvl
                , isNotTopLevel top_lvl
                -> checkCrossStageLifting id use_stage
             _  -> return ()   -- Not a locally-bound thing, or
                               -- no cross-stage link
    }

--------------------------------------
checkCrossStageLifting :: Id -> ThStage -> TcM ()
-- If we are inside typed brackets, and (use_lvl > bind_lvl)
-- we must check whether there's a cross-stage lift to do
-- Examples   \x -> [|| x ||]
--            [|| map ||]
-- There is no error-checking to do, because the renamer did that
--
-- This is similar to checkCrossStageLifting in RnSplice, but
-- this code is applied to *typed* brackets.

checkCrossStageLifting id (Brack _ (TcPending ps_var lie_var))
  =     -- Nested identifiers, such as 'x' in
        -- E.g. \x -> [|| h x ||]
        -- We must behave as if the reference to x was
        --      h $(lift x)
        -- We use 'x' itself as the splice proxy, used by
        -- the desugarer to stitch it all back together.
        -- If 'x' occurs many times we may get many identical
        -- bindings of the same splice proxy, but that doesn't
        -- matter, although it's a mite untidy.
    do  { let id_ty = idType id
        ; checkTc (isTauTy id_ty) (polySpliceErr id)
               -- If x is polymorphic, its occurrence sites might
               -- have different instantiations, so we can't use plain
               -- 'x' as the splice proxy name.  I don't know how to
               -- solve this, and it's probably unimportant, so I'm
               -- just going to flag an error for now

        ; lift <- if isStringTy id_ty then
                     do { sid <- tcLookupId THNames.liftStringName
                                     -- See Note [Lifting strings]
                        ; return (HsVar (noLoc sid)) }
                  else
                     setConstraintVar lie_var   $
                          -- Put the 'lift' constraint into the right LIE
                     newMethodFromName (OccurrenceOf (idName id))
                                       THNames.liftName id_ty

                   -- Update the pending splices
        ; ps <- readMutVar ps_var
        ; let pending_splice = PendingTcSplice (idName id) (nlHsApp (noLoc lift) (nlHsVar id))
        ; writeMutVar ps_var (pending_splice : ps)

        ; return () }

checkCrossStageLifting _ _ = return ()

polySpliceErr :: Id -> SDoc
polySpliceErr id
  = text "Can't splice the polymorphic local variable" <+> quotes (ppr id)

{-
Note [Lifting strings]
~~~~~~~~~~~~~~~~~~~~~~
If we see $(... [| s |] ...) where s::String, we don't want to
generate a mass of Cons (CharL 'x') (Cons (CharL 'y') ...)) etc.
So this conditional short-circuits the lifting mechanism to generate
(liftString "xy") in that case.  I didn't want to use overlapping instances
for the Lift class in TH.Syntax, because that can lead to overlapping-instance
errors in a polymorphic situation.

If this check fails (which isn't impossible) we get another chance; see
Note [Converting strings] in Convert.hs

Local record selectors
~~~~~~~~~~~~~~~~~~~~~~
Record selectors for TyCons in this module are ordinary local bindings,
which show up as ATcIds rather than AGlobals.  So we need to check for
naughtiness in both branches.  c.f. TcTyClsBindings.mkAuxBinds.


************************************************************************
*                                                                      *
\subsection{Record bindings}
*                                                                      *
************************************************************************
-}

getFixedTyVars :: [FieldLabelString] -> [TyVar] -> [ConLike] -> TyVarSet
-- These tyvars must not change across the updates
getFixedTyVars upd_fld_occs univ_tvs cons
      = mkVarSet [tv1 | con <- cons
                      , let (u_tvs, _, eqspec, prov_theta
                             , req_theta, arg_tys, _)
                              = conLikeFullSig con
                            theta = eqSpecPreds eqspec
                                     ++ prov_theta
                                     ++ req_theta
                            flds = conLikeFieldLabels con
                            fixed_tvs = exactTyCoVarsOfTypes fixed_tys
                                    -- fixed_tys: See Note [Type of a record update]
                                        `unionVarSet` tyCoVarsOfTypes theta
                                    -- Universally-quantified tyvars that
                                    -- appear in any of the *implicit*
                                    -- arguments to the constructor are fixed
                                    -- See Note [Implicit type sharing]

                            fixed_tys = [ty | (fl, ty) <- zip flds arg_tys
                                            , not (flLabel fl `elem` upd_fld_occs)]
                      , (tv1,tv) <- univ_tvs `zip` u_tvs
                      , tv `elemVarSet` fixed_tvs ]

{-
Note [Disambiguating record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the -XDuplicateRecordFields extension is used, and the renamer
encounters a record selector or update that it cannot immediately
disambiguate (because it involves fields that belong to multiple
datatypes), it will defer resolution of the ambiguity to the
typechecker.  In this case, the `Ambiguous` constructor of
`AmbiguousFieldOcc` is used.

Consider the following definitions:

        data S = MkS { foo :: Int }
        data T = MkT { foo :: Int, bar :: Int }
        data U = MkU { bar :: Int, baz :: Int }

When the renamer sees `foo` as a selector or an update, it will not
know which parent datatype is in use.

For selectors, there are two possible ways to disambiguate:

1. Check if the pushed-in type is a function whose domain is a
   datatype, for example:

       f s = (foo :: S -> Int) s

       g :: T -> Int
       g = foo

    This is checked by `tcCheckRecSelId` when checking `HsRecFld foo`.

2. Check if the selector is applied to an argument that has a type
   signature, for example:

       h = foo (s :: S)

    This is checked by `tcApp`.


Updates are slightly more complex.  The `disambiguateRecordBinds`
function tries to determine the parent datatype in three ways:

1. Check for types that have all the fields being updated. For example:

        f x = x { foo = 3, bar = 2 }

   Here `f` must be updating `T` because neither `S` nor `U` have
   both fields. This may also discover that no possible type exists.
   For example the following will be rejected:

        f' x = x { foo = 3, baz = 3 }

2. Use the type being pushed in, if it is already a TyConApp. The
   following are valid updates to `T`:

        g :: T -> T
        g x = x { foo = 3 }

        g' x = x { foo = 3 } :: T

3. Use the type signature of the record expression, if it exists and
   is a TyConApp. Thus this is valid update to `T`:

        h x = (x :: T) { foo = 3 }


Note that we do not look up the types of variables being updated, and
no constraint-solving is performed, so for example the following will
be rejected as ambiguous:

     let bad (s :: S) = foo s

     let r :: T
         r = blah
     in r { foo = 3 }

     \r. (r { foo = 3 },  r :: T )

We could add further tests, of a more heuristic nature. For example,
rather than looking for an explicit signature, we could try to infer
the type of the argument to a selector or the record expression being
updated, in case we are lucky enough to get a TyConApp straight
away. However, it might be hard for programmers to predict whether a
particular update is sufficiently obvious for the signature to be
omitted. Moreover, this might change the behaviour of typechecker in
non-obvious ways.

See also Note [HsRecField and HsRecUpdField] in HsPat.
-}

-- Given a RdrName that refers to multiple record fields, and the type
-- of its argument, try to determine the name of the selector that is
-- meant.
disambiguateSelector :: Located RdrName -> Type -> TcM Name
disambiguateSelector lr@(L _ rdr) parent_type
 = do { fam_inst_envs <- tcGetFamInstEnvs
      ; case tyConOf fam_inst_envs parent_type of
          Nothing -> ambiguousSelector lr
          Just p  ->
            do { xs <- lookupParents rdr
               ; let parent = RecSelData p
               ; case lookup parent xs of
                   Just gre -> do { addUsedGRE True gre
                                  ; return (gre_name gre) }
                   Nothing  -> failWithTc (fieldNotInType parent rdr) } }

-- This field name really is ambiguous, so add a suitable "ambiguous
-- occurrence" error, then give up.
ambiguousSelector :: Located RdrName -> TcM a
ambiguousSelector (L _ rdr)
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGRE_RdrName rdr env
       ; setErrCtxt [] $ addNameClashErrRn rdr gres
       ; failM }

-- Disambiguate the fields in a record update.
-- See Note [Disambiguating record fields]
disambiguateRecordBinds :: LHsExpr GhcRn -> TcRhoType
                 -> [LHsRecUpdField GhcRn] -> ExpRhoType
                 -> TcM [LHsRecField' (AmbiguousFieldOcc GhcTc) (LHsExpr GhcRn)]
disambiguateRecordBinds record_expr record_rho rbnds res_ty
    -- Are all the fields unambiguous?
  = case mapM isUnambiguous rbnds of
                     -- If so, just skip to looking up the Ids
                     -- Always the case if DuplicateRecordFields is off
      Just rbnds' -> mapM lookupSelector rbnds'
      Nothing     -> -- If not, try to identify a single parent
        do { fam_inst_envs <- tcGetFamInstEnvs
             -- Look up the possible parents for each field
           ; rbnds_with_parents <- getUpdFieldsParents
           ; let possible_parents = map (map fst . snd) rbnds_with_parents
             -- Identify a single parent
           ; p <- identifyParent fam_inst_envs possible_parents
             -- Pick the right selector with that parent for each field
           ; checkNoErrs $ mapM (pickParent p) rbnds_with_parents }
  where
    -- Extract the selector name of a field update if it is unambiguous
    isUnambiguous :: LHsRecUpdField GhcRn -> Maybe (LHsRecUpdField GhcRn,Name)
    isUnambiguous x = case unLoc (hsRecFieldLbl (unLoc x)) of
                        Unambiguous _ sel_name -> Just (x, sel_name)
                        Ambiguous{}            -> Nothing

    -- Look up the possible parents and selector GREs for each field
    getUpdFieldsParents :: TcM [(LHsRecUpdField GhcRn
                                , [(RecSelParent, GlobalRdrElt)])]
    getUpdFieldsParents
      = fmap (zip rbnds) $ mapM
          (lookupParents . unLoc . hsRecUpdFieldRdr . unLoc)
          rbnds

    -- Given a the lists of possible parents for each field,
    -- identify a single parent
    identifyParent :: FamInstEnvs -> [[RecSelParent]] -> TcM RecSelParent
    identifyParent fam_inst_envs possible_parents
      = case foldr1 intersect possible_parents of
        -- No parents for all fields: record update is ill-typed
        []  -> failWithTc (noPossibleParents rbnds)

        -- Exactly one datatype with all the fields: use that
        [p] -> return p

        -- Multiple possible parents: try harder to disambiguate
        -- Can we get a parent TyCon from the pushed-in type?
        _:_ | Just p <- tyConOfET fam_inst_envs res_ty -> return (RecSelData p)

        -- Does the expression being updated have a type signature?
        -- If so, try to extract a parent TyCon from it
            | Just {} <- obviousSig (unLoc record_expr)
            , Just tc <- tyConOf fam_inst_envs record_rho
            -> return (RecSelData tc)

        -- Nothing else we can try...
        _ -> failWithTc badOverloadedUpdate

    -- Make a field unambiguous by choosing the given parent.
    -- Emits an error if the field cannot have that parent,
    -- e.g. if the user writes
    --     r { x = e } :: T
    -- where T does not have field x.
    pickParent :: RecSelParent
               -> (LHsRecUpdField GhcRn, [(RecSelParent, GlobalRdrElt)])
               -> TcM (LHsRecField' (AmbiguousFieldOcc GhcTc) (LHsExpr GhcRn))
    pickParent p (upd, xs)
      = case lookup p xs of
                      -- Phew! The parent is valid for this field.
                      -- Previously ambiguous fields must be marked as
                      -- used now that we know which one is meant, but
                      -- unambiguous ones shouldn't be recorded again
                      -- (giving duplicate deprecation warnings).
          Just gre -> do { unless (null (tail xs)) $ do
                             let L loc _ = hsRecFieldLbl (unLoc upd)
                             setSrcSpan loc $ addUsedGRE True gre
                         ; lookupSelector (upd, gre_name gre) }
                      -- The field doesn't belong to this parent, so report
                      -- an error but keep going through all the fields
          Nothing  -> do { addErrTc (fieldNotInType p
                                      (unLoc (hsRecUpdFieldRdr (unLoc upd))))
                         ; lookupSelector (upd, gre_name (snd (head xs))) }

    -- Given a (field update, selector name) pair, look up the
    -- selector to give a field update with an unambiguous Id
    lookupSelector :: (LHsRecUpdField GhcRn, Name)
                 -> TcM (LHsRecField' (AmbiguousFieldOcc GhcTc) (LHsExpr GhcRn))
    lookupSelector (L l upd, n)
      = do { i <- tcLookupId n
           ; let L loc af = hsRecFieldLbl upd
                 lbl      = rdrNameAmbiguousFieldOcc af
           ; return $ L l upd { hsRecFieldLbl
                                  = L loc (Unambiguous (L loc lbl) i) } }


-- Extract the outermost TyCon of a type, if there is one; for
-- data families this is the representation tycon (because that's
-- where the fields live).
tyConOf :: FamInstEnvs -> TcSigmaType -> Maybe TyCon
tyConOf fam_inst_envs ty0
  = case tcSplitTyConApp_maybe ty of
      Just (tc, tys) -> Just (fstOf3 (tcLookupDataFamInst fam_inst_envs tc tys))
      Nothing        -> Nothing
  where
    (_, _, ty) = tcSplitSigmaTy ty0

-- Variant of tyConOf that works for ExpTypes
tyConOfET :: FamInstEnvs -> ExpRhoType -> Maybe TyCon
tyConOfET fam_inst_envs ty0 = tyConOf fam_inst_envs =<< checkingExpType_maybe ty0

-- For an ambiguous record field, find all the candidate record
-- selectors (as GlobalRdrElts) and their parents.
lookupParents :: RdrName -> RnM [(RecSelParent, GlobalRdrElt)]
lookupParents rdr
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGRE_RdrName rdr env
       ; mapM lookupParent gres }
  where
    lookupParent :: GlobalRdrElt -> RnM (RecSelParent, GlobalRdrElt)
    lookupParent gre = do { id <- tcLookupId (gre_name gre)
                          ; if isRecordSelector id
                              then return (recordSelectorTyCon id, gre)
                              else failWithTc (notSelector (gre_name gre)) }

-- A type signature on the argument of an ambiguous record selector or
-- the record expression in an update must be "obvious", i.e. the
-- outermost constructor ignoring parentheses.
obviousSig :: HsExpr GhcRn -> Maybe (LHsSigWcType GhcRn)
obviousSig (ExprWithTySig _ ty) = Just ty
obviousSig (HsPar p)            = obviousSig (unLoc p)
obviousSig _                    = Nothing


{-
Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Find the TyCon for the bindings, from the first field label.

2. Instantiate its tyvars and unify (T a1 .. an) with expected_ty.

For each binding field = value

3. Instantiate the field type (from the field label) using the type
   envt from step 2.

4  Type check the value using tcArg, passing the field type as
   the expected argument type.

This extends OK when the field types are universally quantified.
-}

tcRecordBinds
        :: ConLike
        -> [TcType]     -- Expected type for each field
        -> HsRecordBinds GhcRn
        -> TcM (HsRecordBinds GhcTcId)

tcRecordBinds con_like arg_tys (HsRecFields rbinds dd)
  = do  { mb_binds <- mapM do_bind rbinds
        ; return (HsRecFields (catMaybes mb_binds) dd) }
  where
    fields = map flLabel $ conLikeFieldLabels con_like
    flds_w_tys = zipEqual "tcRecordBinds" fields arg_tys

    do_bind :: LHsRecField GhcRn (LHsExpr GhcRn)
            -> TcM (Maybe (LHsRecField GhcTcId (LHsExpr GhcTcId)))
    do_bind (L l fld@(HsRecField { hsRecFieldLbl = f
                                 , hsRecFieldArg = rhs }))

      = do { mb <- tcRecordField con_like flds_w_tys f rhs
           ; case mb of
               Nothing         -> return Nothing
               Just (f', rhs') -> return (Just (L l (fld { hsRecFieldLbl = f'
                                                          , hsRecFieldArg = rhs' }))) }

tcRecordUpd
        :: ConLike
        -> [TcType]     -- Expected type for each field
        -> [LHsRecField' (AmbiguousFieldOcc GhcTc) (LHsExpr GhcRn)]
        -> TcM [LHsRecUpdField GhcTcId]

tcRecordUpd con_like arg_tys rbinds = fmap catMaybes $ mapM do_bind rbinds
  where
    flds_w_tys = zipEqual "tcRecordUpd" (map flLabel $ conLikeFieldLabels con_like) arg_tys

    do_bind :: LHsRecField' (AmbiguousFieldOcc GhcTc) (LHsExpr GhcRn)
            -> TcM (Maybe (LHsRecUpdField GhcTcId))
    do_bind (L l fld@(HsRecField { hsRecFieldLbl = L loc af
                                 , hsRecFieldArg = rhs }))
      = do { let lbl = rdrNameAmbiguousFieldOcc af
                 sel_id = selectorAmbiguousFieldOcc af
                 f = L loc (FieldOcc (L loc lbl) (idName sel_id))
           ; mb <- tcRecordField con_like flds_w_tys f rhs
           ; case mb of
               Nothing         -> return Nothing
               Just (f', rhs') ->
                 return (Just
                         (L l (fld { hsRecFieldLbl
                                      = L loc (Unambiguous (L loc lbl)
                                               (selectorFieldOcc (unLoc f')))
                                   , hsRecFieldArg = rhs' }))) }

tcRecordField :: ConLike -> Assoc FieldLabelString Type
              -> LFieldOcc GhcRn -> LHsExpr GhcRn
              -> TcM (Maybe (LFieldOcc GhcTc, LHsExpr GhcTc))
tcRecordField con_like flds_w_tys (L loc (FieldOcc lbl sel_name)) rhs
  | Just field_ty <- assocMaybe flds_w_tys field_lbl
      = addErrCtxt (fieldCtxt field_lbl) $
        do { rhs' <- tcPolyExprNC rhs field_ty
           ; let field_id = mkUserLocal (nameOccName sel_name)
                                        (nameUnique sel_name)
                                        field_ty loc
                -- Yuk: the field_id has the *unique* of the selector Id
                --          (so we can find it easily)
                --      but is a LocalId with the appropriate type of the RHS
                --          (so the desugarer knows the type of local binder to make)
           ; return (Just (L loc (FieldOcc lbl field_id), rhs')) }
      | otherwise
      = do { addErrTc (badFieldCon con_like field_lbl)
           ; return Nothing }
  where
        field_lbl = occNameFS $ rdrNameOcc (unLoc lbl)


checkMissingFields ::  ConLike -> HsRecordBinds GhcRn -> TcM ()
checkMissingFields con_like rbinds
  | null field_labels   -- Not declared as a record;
                        -- But C{} is still valid if no strict fields
  = if any isBanged field_strs then
        -- Illegal if any arg is strict
        addErrTc (missingStrictFields con_like [])
    else
        return ()

  | otherwise = do              -- A record
    unless (null missing_s_fields)
           (addErrTc (missingStrictFields con_like missing_s_fields))

    warn <- woptM Opt_WarnMissingFields
    unless (not (warn && notNull missing_ns_fields))
           (warnTc (Reason Opt_WarnMissingFields) True
               (missingFields con_like missing_ns_fields))

  where
    missing_s_fields
        = [ flLabel fl | (fl, str) <- field_info,
                 isBanged str,
                 not (fl `elemField` field_names_used)
          ]
    missing_ns_fields
        = [ flLabel fl | (fl, str) <- field_info,
                 not (isBanged str),
                 not (fl `elemField` field_names_used)
          ]

    field_names_used = hsRecFields rbinds
    field_labels     = conLikeFieldLabels con_like

    field_info = zipEqual "missingFields"
                          field_labels
                          field_strs

    field_strs = conLikeImplBangs con_like

    fl `elemField` flds = any (\ fl' -> flSelector fl == fl') flds

{-
************************************************************************
*                                                                      *
\subsection{Errors and contexts}
*                                                                      *
************************************************************************

Boring and alphabetical:
-}

addExprErrCtxt :: LHsExpr GhcRn -> TcM a -> TcM a
addExprErrCtxt expr = addErrCtxt (exprCtxt expr)

exprCtxt :: LHsExpr GhcRn -> SDoc
exprCtxt expr
  = hang (text "In the expression:") 2 (ppr expr)

fieldCtxt :: FieldLabelString -> SDoc
fieldCtxt field_name
  = text "In the" <+> quotes (ppr field_name) <+> ptext (sLit "field of a record")

addFunResCtxt :: Bool  -- There is at least one argument
              -> HsExpr GhcRn -> TcType -> ExpRhoType
              -> TcM a -> TcM a
-- When we have a mis-match in the return type of a function
-- try to give a helpful message about too many/few arguments
--
-- Used for naked variables too; but with has_args = False
addFunResCtxt has_args fun fun_res_ty env_ty
  = addLandmarkErrCtxtM (\env -> (env, ) <$> mk_msg)
      -- NB: use a landmark error context, so that an empty context
      -- doesn't suppress some more useful context
  where
    mk_msg
      = do { mb_env_ty <- readExpType_maybe env_ty
                     -- by the time the message is rendered, the ExpType
                     -- will be filled in (except if we're debugging)
           ; fun_res' <- zonkTcType fun_res_ty
           ; env'     <- case mb_env_ty of
                           Just env_ty -> zonkTcType env_ty
                           Nothing     ->
                             do { dumping <- doptM Opt_D_dump_tc_trace
                                ; MASSERT( dumping )
                                ; newFlexiTyVarTy liftedTypeKind }
           ; let -- See Note [Splitting nested sigma types in mismatched
                 --           function types]
                 (_, _, fun_tau) = tcSplitNestedSigmaTys fun_res'
                 -- No need to call tcSplitNestedSigmaTys here, since env_ty is
                 -- an ExpRhoTy, i.e., it's already deeply instantiated.
                 (_, _, env_tau) = tcSplitSigmaTy env'
                 (args_fun, res_fun) = tcSplitFunTys fun_tau
                 (args_env, res_env) = tcSplitFunTys env_tau
                 n_fun = length args_fun
                 n_env = length args_env
                 info  | n_fun == n_env = Outputable.empty
                       | n_fun > n_env
                       , not_fun res_env
                       = text "Probable cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too few arguments"

                       | has_args
                       , not_fun res_fun
                       = text "Possible cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too many arguments"

                       | otherwise
                       = Outputable.empty  -- Never suggest that a naked variable is                                         -- applied to too many args!
           ; return info }
      where
        not_fun ty   -- ty is definitely not an arrow type,
                     -- and cannot conceivably become one
          = case tcSplitTyConApp_maybe ty of
              Just (tc, _) -> isAlgTyCon tc
              Nothing      -> False

{-
Note [Splitting nested sigma types in mismatched function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When one applies a function to too few arguments, GHC tries to determine this
fact if possible so that it may give a helpful error message. It accomplishes
this by checking if the type of the applied function has more argument types
than supplied arguments.

Previously, GHC computed the number of argument types through tcSplitSigmaTy.
This is incorrect in the face of nested foralls, however! This caused Trac
#13311, for instance:

  f :: forall a. (Monoid a) => forall b. (Monoid b) => Maybe a -> Maybe b

If one uses `f` like so:

  do { f; putChar 'a' }

Then tcSplitSigmaTy will decompose the type of `f` into:

  Tyvars: [a]
  Context: (Monoid a)
  Argument types: []
  Return type: forall b. Monoid b => Maybe a -> Maybe b

That is, it will conclude that there are *no* argument types, and since `f`
was given no arguments, it won't print a helpful error message. On the other
hand, tcSplitNestedSigmaTys correctly decomposes `f`'s type down to:

  Tyvars: [a, b]
  Context: (Monoid a, Monoid b)
  Argument types: [Maybe a]
  Return type: Maybe b

So now GHC recognizes that `f` has one more argument type than it was actually
provided.
-}

badFieldTypes :: [(FieldLabelString,TcType)] -> SDoc
badFieldTypes prs
  = hang (text "Record update for insufficiently polymorphic field"
                         <> plural prs <> colon)
       2 (vcat [ ppr f <+> dcolon <+> ppr ty | (f,ty) <- prs ])

badFieldsUpd
  :: [LHsRecField' (AmbiguousFieldOcc GhcTc) (LHsExpr GhcRn)]
               -- Field names that don't belong to a single datacon
  -> [ConLike] -- Data cons of the type which the first field name belongs to
  -> SDoc
badFieldsUpd rbinds data_cons
  = hang (text "No constructor has all these fields:")
       2 (pprQuotedList conflictingFields)
          -- See Note [Finding the conflicting fields]
  where
    -- A (preferably small) set of fields such that no constructor contains
    -- all of them.  See Note [Finding the conflicting fields]
    conflictingFields = case nonMembers of
        -- nonMember belongs to a different type.
        (nonMember, _) : _ -> [aMember, nonMember]
        [] -> let
            -- All of rbinds belong to one type. In this case, repeatedly add
            -- a field to the set until no constructor contains the set.

            -- Each field, together with a list indicating which constructors
            -- have all the fields so far.
            growingSets :: [(FieldLabelString, [Bool])]
            growingSets = scanl1 combine membership
            combine (_, setMem) (field, fldMem)
              = (field, zipWith (&&) setMem fldMem)
            in
            -- Fields that don't change the membership status of the set
            -- are redundant and can be dropped.
            map (fst . head) $ groupBy ((==) `on` snd) growingSets

    aMember = ASSERT( not (null members) ) fst (head members)
    (members, nonMembers) = partition (or . snd) membership

    -- For each field, which constructors contain the field?
    membership :: [(FieldLabelString, [Bool])]
    membership = sortMembership $
        map (\fld -> (fld, map (Set.member fld) fieldLabelSets)) $
          map (occNameFS . rdrNameOcc . rdrNameAmbiguousFieldOcc . unLoc . hsRecFieldLbl . unLoc) rbinds

    fieldLabelSets :: [Set.Set FieldLabelString]
    fieldLabelSets = map (Set.fromList . map flLabel . conLikeFieldLabels) data_cons

    -- Sort in order of increasing number of True, so that a smaller
    -- conflicting set can be found.
    sortMembership =
      map snd .
      sortBy (compare `on` fst) .
      map (\ item@(_, membershipRow) -> (countTrue membershipRow, item))

    countTrue = count id

{-
Note [Finding the conflicting fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  data A = A {a0, a1 :: Int}
         | B {b0, b1 :: Int}
and we see a record update
  x { a0 = 3, a1 = 2, b0 = 4, b1 = 5 }
Then we'd like to find the smallest subset of fields that no
constructor has all of.  Here, say, {a0,b0}, or {a0,b1}, etc.
We don't really want to report that no constructor has all of
{a0,a1,b0,b1}, because when there are hundreds of fields it's
hard to see what was really wrong.

We may need more than two fields, though; eg
  data T = A { x,y :: Int, v::Int }
          | B { y,z :: Int, v::Int }
          | C { z,x :: Int, v::Int }
with update
   r { x=e1, y=e2, z=e3 }, we

Finding the smallest subset is hard, so the code here makes
a decent stab, no more.  See Trac #7989.
-}

naughtyRecordSel :: RdrName -> SDoc
naughtyRecordSel sel_id
  = text "Cannot use record selector" <+> quotes (ppr sel_id) <+>
    text "as a function due to escaped type variables" $$
    text "Probable fix: use pattern-matching syntax instead"

notSelector :: Name -> SDoc
notSelector field
  = hsep [quotes (ppr field), text "is not a record selector"]

mixedSelectors :: [Id] -> [Id] -> SDoc
mixedSelectors data_sels@(dc_rep_id:_) pat_syn_sels@(ps_rep_id:_)
  = ptext
      (sLit "Cannot use a mixture of pattern synonym and record selectors") $$
    text "Record selectors defined by"
      <+> quotes (ppr (tyConName rep_dc))
      <> text ":"
      <+> pprWithCommas ppr data_sels $$
    text "Pattern synonym selectors defined by"
      <+> quotes (ppr (patSynName rep_ps))
      <> text ":"
      <+> pprWithCommas ppr pat_syn_sels
  where
    RecSelPatSyn rep_ps = recordSelectorTyCon ps_rep_id
    RecSelData rep_dc = recordSelectorTyCon dc_rep_id
mixedSelectors _ _ = panic "TcExpr: mixedSelectors emptylists"


missingStrictFields :: ConLike -> [FieldLabelString] -> SDoc
missingStrictFields con fields
  = header <> rest
  where
    rest | null fields = Outputable.empty  -- Happens for non-record constructors
                                           -- with strict fields
         | otherwise   = colon <+> pprWithCommas ppr fields

    header = text "Constructor" <+> quotes (ppr con) <+>
             text "does not have the required strict field(s)"

missingFields :: ConLike -> [FieldLabelString] -> SDoc
missingFields con fields
  = text "Fields of" <+> quotes (ppr con) <+> ptext (sLit "not initialised:")
        <+> pprWithCommas ppr fields

-- callCtxt fun args = text "In the call" <+> parens (ppr (foldl mkHsApp fun args))

noPossibleParents :: [LHsRecUpdField GhcRn] -> SDoc
noPossibleParents rbinds
  = hang (text "No type has all these fields:")
       2 (pprQuotedList fields)
  where
    fields = map (hsRecFieldLbl . unLoc) rbinds

badOverloadedUpdate :: SDoc
badOverloadedUpdate = text "Record update is ambiguous, and requires a type signature"

fieldNotInType :: RecSelParent -> RdrName -> SDoc
fieldNotInType p rdr
  = unknownSubordinateErr (text "field of type" <+> quotes (ppr p)) rdr

{-
************************************************************************
*                                                                      *
\subsection{Static Pointers}
*                                                                      *
************************************************************************
-}

-- | A data type to describe why a variable is not closed.
data NotClosedReason = NotLetBoundReason
                     | NotTypeClosed VarSet
                     | NotClosed Name NotClosedReason

-- | Checks if the given name is closed and emits an error if not.
--
-- See Note [Not-closed error messages].
checkClosedInStaticForm :: Name -> TcM ()
checkClosedInStaticForm name = do
    type_env <- getLclTypeEnv
    case checkClosed type_env name of
      Nothing -> return ()
      Just reason -> addErrTc $ explain name reason
  where
    -- See Note [Checking closedness].
    checkClosed :: TcTypeEnv -> Name -> Maybe NotClosedReason
    checkClosed type_env n = checkLoop type_env (unitNameSet n) n

    checkLoop :: TcTypeEnv -> NameSet -> Name -> Maybe NotClosedReason
    checkLoop type_env visited n = do
      -- The @visited@ set is an accumulating parameter that contains the set of
      -- visited nodes, so we avoid repeating cycles in the traversal.
      case lookupNameEnv type_env n of
        Just (ATcId { tct_id = tcid, tct_info = info }) -> case info of
          ClosedLet   -> Nothing
          NotLetBound -> Just NotLetBoundReason
          NonClosedLet fvs type_closed -> listToMaybe $
            -- Look for a non-closed variable in fvs
            [ NotClosed n' reason
            | n' <- nameSetElemsStable fvs
            , not (elemNameSet n' visited)
            , Just reason <- [checkLoop type_env (extendNameSet visited n') n']
            ] ++
            if type_closed then
              []
            else
              -- We consider non-let-bound variables easier to figure out than
              -- non-closed types, so we report non-closed types to the user
              -- only if we cannot spot the former.
              [ NotTypeClosed $ tyCoVarsOfType (idType tcid) ]
        -- The binding is closed.
        _ -> Nothing

    -- Converts a reason into a human-readable sentence.
    --
    -- @explain name reason@ starts with
    --
    -- "<name> is used in a static form but it is not closed because it"
    --
    -- and then follows a list of causes. For each id in the path, the text
    --
    -- "uses <id> which"
    --
    -- is appended, yielding something like
    --
    -- "uses <id> which uses <id1> which uses <id2> which"
    --
    -- until the end of the path is reached, which is reported as either
    --
    -- "is not let-bound"
    --
    -- when the final node is not let-bound, or
    --
    -- "has a non-closed type because it contains the type variables:
    -- v1, v2, v3"
    --
    -- when the final node has a non-closed type.
    --
    explain :: Name -> NotClosedReason -> SDoc
    explain name reason =
      quotes (ppr name) <+> text "is used in a static form but it is not closed"
                        <+> text "because it"
                        $$
                        sep (causes reason)

    causes :: NotClosedReason -> [SDoc]
    causes NotLetBoundReason = [text "is not let-bound."]
    causes (NotTypeClosed vs) =
      [ text "has a non-closed type because it contains the"
      , text "type variables:" <+>
        pprVarSet vs (hsep . punctuate comma . map (quotes . ppr))
      ]
    causes (NotClosed n reason) =
      let msg = text "uses" <+> quotes (ppr n) <+> text "which"
       in case reason of
            NotClosed _ _ -> msg : causes reason
            _   -> let (xs0, xs1) = splitAt 1 $ causes reason
                    in fmap (msg <+>) xs0 ++ xs1

-- Note [Not-closed error messages]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- When variables in a static form are not closed, we go through the trouble
-- of explaining why they aren't.
--
-- Thus, the following program
--
-- > {-# LANGUAGE StaticPointers #-}
-- > module M where
-- >
-- > f x = static g
-- >   where
-- >     g = h
-- >     h = x
--
-- produces the error
--
--    'g' is used in a static form but it is not closed because it
--    uses 'h' which uses 'x' which is not let-bound.
--
-- And a program like
--
-- > {-# LANGUAGE StaticPointers #-}
-- > module M where
-- >
-- > import Data.Typeable
-- > import GHC.StaticPtr
-- >
-- > f :: Typeable a => a -> StaticPtr TypeRep
-- > f x = const (static (g undefined)) (h x)
-- >   where
-- >     g = h
-- >     h = typeOf
--
-- produces the error
--
--    'g' is used in a static form but it is not closed because it
--    uses 'h' which has a non-closed type because it contains the
--    type variables: 'a'
--

-- Note [Checking closedness]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- @checkClosed@ checks if a binding is closed and returns a reason if it is
-- not.
--
-- The bindings define a graph where the nodes are ids, and there is an edge
-- from @id1@ to @id2@ if the rhs of @id1@ contains @id2@ among its free
-- variables.
--
-- When @n@ is not closed, it has to exist in the graph some node reachable
-- from @n@ that it is not a let-bound variable or that it has a non-closed
-- type. Thus, the "reason" is a path from @n@ to this offending node.
--
-- When @n@ is not closed, we traverse the graph reachable from @n@ to build
-- the reason.
--
