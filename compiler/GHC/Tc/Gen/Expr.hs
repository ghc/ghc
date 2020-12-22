{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

module GHC.Tc.Gen.Expr
       ( tcCheckPolyExpr, tcCheckPolyExprNC,
         tcCheckMonoExpr, tcCheckMonoExprNC, tcMonoExpr, tcMonoExprNC,
         tcInferRho, tcInferRhoNC,
         tcExpr,
         tcSyntaxOp, tcSyntaxOpGen, SyntaxOpType(..), synKnownType,
         tcCheckId,
         addAmbiguousNameErr,
         getFixedTyVars ) where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-}   GHC.Tc.Gen.Splice( tcSpliceExpr, tcTypedBracket, tcUntypedBracket )

import GHC.Hs
import GHC.Rename.Utils
import GHC.Tc.Utils.Zonk
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Core.Multiplicity
import GHC.Core.UsageEnv
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Gen.App
import GHC.Tc.Gen.Head
import GHC.Tc.Gen.Bind        ( tcLocalBinds )
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs )
import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Rename.Env         ( addUsedGRE )
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.Arrow
import GHC.Tc.Gen.Match
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Pat
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Tc.Types.Evidence
import GHC.Types.Var.Set
import GHC.Builtin.Types
import GHC.Builtin.Names
import GHC.Driver.Session
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Data.List.SetOps
import GHC.Data.Maybe
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import Control.Monad
import GHC.Core.Class(classTyCon)
import GHC.Types.Unique.Set ( UniqSet, mkUniqSet, elementOfUniqSet, nonDetEltsUniqSet )
import qualified GHC.LanguageExtensions as LangExt

import Data.Function
import Data.List (partition, sortBy, groupBy, intersect)

{-
************************************************************************
*                                                                      *
\subsection{Main wrappers}
*                                                                      *
************************************************************************
-}


tcCheckPolyExpr, tcCheckPolyExprNC
  :: LHsExpr GhcRn         -- Expression to type check
  -> TcSigmaType           -- Expected type (could be a polytype)
  -> TcM (LHsExpr GhcTc) -- Generalised expr with expected type

-- tcCheckPolyExpr is a convenient place (frequent but not too frequent)
-- place to add context information.
-- The NC version does not do so, usually because the caller wants
-- to do so themselves.

tcCheckPolyExpr   expr res_ty = tcPolyExpr   expr (mkCheckExpType res_ty)
tcCheckPolyExprNC expr res_ty = tcPolyExprNC expr (mkCheckExpType res_ty)

-- These versions take an ExpType
tcPolyExpr, tcPolyExprNC
  :: LHsExpr GhcRn -> ExpSigmaType
  -> TcM (LHsExpr GhcTc)

tcPolyExpr expr res_ty
  = addLExprCtxt expr $
    do { traceTc "tcPolyExpr" (ppr res_ty)
       ; tcPolyExprNC expr res_ty }

tcPolyExprNC (L loc expr) res_ty
  = set_loc_and_ctxt loc expr $
    do { traceTc "tcPolyExprNC" (ppr res_ty)
       ; (wrap, expr') <- tcSkolemiseET GenSigCtxt res_ty $ \ res_ty ->
                          tcExpr expr res_ty
       ; return $ L loc (mkHsWrap wrap expr') }

  where -- See Note [Rebindable syntax and HsExpansion), which describes
        -- the logic behind this location/context tweaking.
        set_loc_and_ctxt l e m = do
          inGenCode <- inGeneratedCode
          if inGenCode && not (isGeneratedSrcSpan l)
            then setSrcSpan l $
                 addExprCtxt e m
            else setSrcSpan l m

---------------
tcCheckMonoExpr, tcCheckMonoExprNC
    :: LHsExpr GhcRn     -- Expression to type check
    -> TcRhoType         -- Expected type
                         -- Definitely no foralls at the top
    -> TcM (LHsExpr GhcTc)
tcCheckMonoExpr   expr res_ty = tcMonoExpr   expr (mkCheckExpType res_ty)
tcCheckMonoExprNC expr res_ty = tcMonoExprNC expr (mkCheckExpType res_ty)

tcMonoExpr, tcMonoExprNC
    :: LHsExpr GhcRn     -- Expression to type check
    -> ExpRhoType        -- Expected type
                         -- Definitely no foralls at the top
    -> TcM (LHsExpr GhcTc)

tcMonoExpr expr res_ty
  = addLExprCtxt expr $
    tcMonoExprNC expr res_ty

tcMonoExprNC (L loc expr) res_ty
  = setSrcSpan loc $
    do  { expr' <- tcExpr expr res_ty
        ; return (L loc expr') }

---------------
tcInferRho, tcInferRhoNC :: LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)
-- Infer a *rho*-type. The return type is always instantiated.
tcInferRho le = addLExprCtxt le $
                tcInferRhoNC le

tcInferRhoNC (L loc expr)
  = setSrcSpan loc $
    do { (expr', rho) <- tcInfer (tcExpr expr)
       ; return (L loc expr', rho) }


{- *********************************************************************
*                                                                      *
        tcExpr: the main expression typechecker
*                                                                      *
********************************************************************* -}

tcExpr :: HsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)

-- Use tcApp to typecheck appplications, which are treated specially
-- by Quick Look.  Specifically:
--   - HsApp:     value applications
--   - HsTypeApp: type applications
--   - HsVar:     lone variables, to ensure that they can get an
--                impredicative instantiation (via Quick Look
--                driven by res_ty (in checking mode).
--   - ExprWithTySig: (e :: type)
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
tcExpr e@(HsVar {})         res_ty = tcApp e res_ty
tcExpr e@(HsApp {})         res_ty = tcApp e res_ty
tcExpr e@(HsAppType {})     res_ty = tcApp e res_ty
tcExpr e@(ExprWithTySig {}) res_ty = tcApp e res_ty
tcExpr e@(HsRecFld {})      res_ty = tcApp e res_ty

-- Typecheck an occurrence of an unbound Id
--
-- Some of these started life as a true expression hole "_".
-- Others might simply be variables that accidentally have no binding site
tcExpr e@(HsUnboundVar _ occ) res_ty
  = do { ty <- newOpenFlexiTyVarTy  -- Allow Int# etc (#12531)
       ; name <- newSysName occ
       ; let ev = mkLocalId name Many ty
       ; emitNewExprHole occ ev ty
       ; tcEmitBindingUsage bottomUE   -- Holes fit any usage environment
                                       -- (#18491)
       ; tcWrapResultO (UnboundOccurrenceOf occ) e
                       (HsUnboundVar ev occ) ty res_ty }

tcExpr e@(HsLit x lit) res_ty
  = do { let lit_ty = hsLitType lit
       ; tcWrapResult e (HsLit x (convertLit lit)) lit_ty res_ty }

tcExpr (HsPar x expr) res_ty
  = do { expr' <- tcMonoExprNC expr res_ty
       ; return (HsPar x expr') }

tcExpr (HsPragE x prag expr) res_ty
  = do { expr' <- tcMonoExpr expr res_ty
       ; return (HsPragE x (tcExprPrag prag) expr') }

tcExpr (HsOverLit x lit) res_ty
  = do  { lit' <- newOverloadedLit lit res_ty
        ; return (HsOverLit x lit') }

tcExpr (NegApp x expr neg_expr) res_ty
  = do  { (expr', neg_expr')
            <- tcSyntaxOp NegateOrigin neg_expr [SynAny] res_ty $
               \[arg_ty] [arg_mult] ->
               tcScalingUsage arg_mult $ tcCheckMonoExpr expr arg_ty
        ; return (NegApp x expr' neg_expr') }

tcExpr e@(HsIPVar _ x) res_ty
  = do {   {- Implicit parameters must have a *tau-type* not a
              type scheme.  We enforce this by creating a fresh
              type variable as its type.  (Because res_ty may not
              be a tau-type.) -}
         ip_ty <- newOpenFlexiTyVarTy
       ; let ip_name = mkStrLitTy (hsIPNameFS x)
       ; ipClass <- tcLookupClass ipClassName
       ; ip_var <- emitWantedEvVar origin (mkClassPred ipClass [ip_name, ip_ty])
       ; tcWrapResult e
                   (fromDict ipClass ip_name ip_ty (HsVar noExtField (noLoc ip_var)))
                   ip_ty res_ty }
  where
  -- Coerces a dictionary for `IP "x" t` into `t`.
  fromDict ipClass x ty = mkHsWrap $ mkWpCastR $
                          unwrapIP $ mkClassPred ipClass [x,ty]
  origin = IPOccOrigin x

tcExpr e@(HsOverLabel _ mb_fromLabel l) res_ty
  = do { -- See Note [Type-checking overloaded labels]
         loc <- getSrcSpanM
       ; case mb_fromLabel of
           Just fromLabel -> tcExpr (applyFromLabel loc fromLabel) res_ty
           Nothing -> do { isLabelClass <- tcLookupClass isLabelClassName
                         ; alpha <- newFlexiTyVarTy liftedTypeKind
                         ; let pred = mkClassPred isLabelClass [lbl, alpha]
                         ; loc <- getSrcSpanM
                         ; var <- emitWantedEvVar origin pred
                         ; tcWrapResult e
                                       (fromDict pred (HsVar noExtField (L loc var)))
                                        alpha res_ty } }
  where
  -- Coerces a dictionary for `IsLabel "x" t` into `t`,
  -- or `HasField "x" r a into `r -> a`.
  fromDict pred = mkHsWrap $ mkWpCastR $ unwrapIP pred
  origin = OverLabelOrigin l
  lbl = mkStrLitTy l

  applyFromLabel loc fromLabel =
    HsAppType noExtField
         (L loc (HsVar noExtField (L loc fromLabel)))
         (mkEmptyWildCardBndrs (L loc (HsTyLit noExtField (HsStrTy NoSourceText l))))

tcExpr (HsLam x match) res_ty
  = do  { (wrap, match') <- tcMatchLambda herald match_ctxt match res_ty
        ; return (mkHsWrap wrap (HsLam x match')) }
  where
    match_ctxt = MC { mc_what = LambdaExpr, mc_body = tcBody }
    herald = sep [ text "The lambda expression" <+>
                   quotes (pprSetDepth (PartWay 1) $
                           pprMatches match),
                        -- The pprSetDepth makes the abstraction print briefly
                   text "has"]

tcExpr e@(HsLamCase x matches) res_ty
  = do { (wrap, matches')
           <- tcMatchLambda msg match_ctxt matches res_ty
           -- The laziness annotation is because we don't want to fail here
           -- if there are multiple arguments
       ; return (mkHsWrap wrap $ HsLamCase x matches') }
  where
    msg = sep [ text "The function" <+> quotes (ppr e)
              , text "requires"]
    match_ctxt = MC { mc_what = CaseAlt, mc_body = tcBody }

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
-}

tcExpr expr@(OpApp {}) res_ty
  = tcApp expr res_ty

-- Right sections, equivalent to \ x -> x `op` expr, or
--      \ x -> op x expr

tcExpr expr@(SectionR x op arg2) res_ty
  = do { (op', op_ty) <- tcInferRhoNC op
       ; (wrap_fun, [Scaled arg1_mult arg1_ty, arg2_ty], op_res_ty)
                  <- matchActualFunTysRho (mk_op_msg op) fn_orig
                                          (Just (ppr op)) 2 op_ty
       ; arg2' <- tcValArg (unLoc op) arg2 arg2_ty 2
       ; let expr'      = SectionR x (mkLHsWrap wrap_fun op') arg2'
             act_res_ty = mkVisFunTy arg1_mult arg1_ty op_res_ty
       ; tcWrapResultMono expr expr' act_res_ty res_ty }

  where
    fn_orig = lexprCtOrigin op
    -- It's important to use the origin of 'op', so that call-stacks
    -- come out right; they are driven by the OccurrenceOf CtOrigin
    -- See #13285

tcExpr expr@(SectionL x arg1 op) res_ty
  = do { (op', op_ty) <- tcInferRhoNC op
       ; dflags <- getDynFlags      -- Note [Left sections]
       ; let n_reqd_args | xopt LangExt.PostfixOperators dflags = 1
                         | otherwise                            = 2

       ; (wrap_fn, (arg1_ty:arg_tys), op_res_ty)
           <- matchActualFunTysRho (mk_op_msg op) fn_orig
                                   (Just (ppr op)) n_reqd_args op_ty
       ; arg1' <- tcValArg (unLoc op) arg1 arg1_ty 1
       ; let expr'      = SectionL x arg1' (mkLHsWrap wrap_fn op')
             act_res_ty = mkVisFunTys arg_tys op_res_ty
       ; tcWrapResultMono expr expr' act_res_ty res_ty }
  where
    fn_orig = lexprCtOrigin op
    -- It's important to use the origin of 'op', so that call-stacks
    -- come out right; they are driven by the OccurrenceOf CtOrigin
    -- See #13285

tcExpr expr@(ExplicitTuple x tup_args boxity) res_ty
  | all tupArgPresent tup_args
  = do { let arity  = length tup_args
             tup_tc = tupleTyCon boxity arity
               -- NB: tupleTyCon doesn't flatten 1-tuples
               -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make
       ; res_ty <- expTypeToType res_ty
       ; (coi, arg_tys) <- matchExpectedTyConApp tup_tc res_ty
                           -- Unboxed tuples have RuntimeRep vars, which we
                           -- don't care about here
                           -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
       ; let arg_tys' = case boxity of Unboxed -> drop arity arg_tys
                                       Boxed   -> arg_tys
       ; tup_args1 <- tcTupArgs tup_args arg_tys'
       ; return $ mkHsWrapCo coi (ExplicitTuple x tup_args1 boxity) }

  | otherwise
  = -- The tup_args are a mixture of Present and Missing (for tuple sections)
    do { let arity = length tup_args

       ; arg_tys <- case boxity of
           { Boxed   -> newFlexiTyVarTys arity liftedTypeKind
           ; Unboxed -> replicateM arity newOpenFlexiTyVarTy }

       -- Handle tuple sections where
       ; tup_args1 <- tcTupArgs tup_args arg_tys

       ; let expr'       = ExplicitTuple x tup_args1 boxity
             missing_tys = [Scaled mult ty | (L _ (Missing (Scaled mult _)), ty) <- zip tup_args1 arg_tys]

             -- See Note [Linear fields generalization] in GHC.Tc.Gen.App
             act_res_ty
                 = mkVisFunTys missing_tys (mkTupleTy1 boxity arg_tys)
                   -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make

       ; traceTc "ExplicitTuple" (ppr act_res_ty $$ ppr res_ty)

       ; tcWrapResultMono expr expr' act_res_ty res_ty }

tcExpr (ExplicitSum _ alt arity expr) res_ty
  = do { let sum_tc = sumTyCon arity
       ; res_ty <- expTypeToType res_ty
       ; (coi, arg_tys) <- matchExpectedTyConApp sum_tc res_ty
       ; -- Drop levity vars, we don't care about them here
         let arg_tys' = drop arity arg_tys
       ; expr' <- tcCheckPolyExpr expr (arg_tys' `getNth` (alt - 1))
       ; return $ mkHsWrapCo coi (ExplicitSum arg_tys' alt arity expr' ) }

-- This will see the empty list only when -XOverloadedLists.
-- See Note [Empty lists] in GHC.Hs.Expr.
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
                            \ [elt_ty] [_int_mul, list_mul] ->
                              -- We ignore _int_mul because the integer (first
                              -- argument of fromListN) is statically known: it
                              -- is desugared to a literal. Therefore there is
                              -- no variable of which to scale the usage in that
                              -- first argument, and `_int_mul` is completely
                              -- free in this expression.
                            do { exprs' <-
                                    mapM (tcScalingUsage list_mul . tc_elt elt_ty) exprs
                               ; return (exprs', elt_ty) }

                     ; return $ ExplicitList elt_ty (Just fln') exprs' }
     where tc_elt elt_ty expr = tcCheckPolyExpr expr elt_ty

{-
************************************************************************
*                                                                      *
                Let, case, if, do
*                                                                      *
************************************************************************
-}

tcExpr (HsLet x (L l binds) expr) res_ty
  = do  { (binds', expr') <- tcLocalBinds binds $
                             tcMonoExpr expr res_ty
        ; return (HsLet x (L l binds') expr') }

tcExpr (HsCase x scrut matches) res_ty
  = do  {  -- We used to typecheck the case alternatives first.
           -- The case patterns tend to give good type info to use
           -- when typechecking the scrutinee.  For example
           --   case (map f) of
           --     (x:xs) -> ...
           -- will report that map is applied to too few arguments
           --
           -- But now, in the GADT world, we need to typecheck the scrutinee
           -- first, to get type info that may be refined in the case alternatives
          let mult = Many
            -- There is not yet syntax or inference mechanism for case
            -- expressions to be anything else than unrestricted.

          -- Typecheck the scrutinee.  We use tcInferRho but tcInferSigma
          -- would also be possible (tcMatchesCase accepts sigma-types)
          -- Interesting litmus test: do these two behave the same?
          --     case id        of {..}
          --     case (\v -> v) of {..}
          -- This design choice is discussed in #17790
        ; (scrut', scrut_ty) <- tcScalingUsage mult $ tcInferRho scrut

        ; traceTc "HsCase" (ppr scrut_ty)
        ; matches' <- tcMatchesCase match_ctxt (Scaled mult scrut_ty) matches res_ty
        ; return (HsCase x scrut' matches') }
 where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = tcBody }

tcExpr (HsIf x pred b1 b2) res_ty
  = do { pred'    <- tcCheckMonoExpr pred boolTy
       ; (u1,b1') <- tcCollectingUsage $ tcMonoExpr b1 res_ty
       ; (u2,b2') <- tcCollectingUsage $ tcMonoExpr b2 res_ty
       ; tcEmitBindingUsage (supUE u1 u2)
       ; return (HsIf x pred' b1' b2') }

tcExpr (HsMultiIf _ alts) res_ty
  = do { alts' <- mapM (wrapLocM $ tcGRHS match_ctxt res_ty) alts
       ; res_ty <- readExpType res_ty
       ; return (HsMultiIf res_ty alts') }
  where match_ctxt = MC { mc_what = IfAlt, mc_body = tcBody }

tcExpr (HsDo _ do_or_lc stmts) res_ty
  = tcDoStmts do_or_lc stmts res_ty

tcExpr (HsProc x pat cmd) res_ty
  = do  { (pat', cmd', coi) <- tcProc pat cmd res_ty
        ; return $ mkHsWrapCo coi (HsProc x pat' cmd') }

-- Typechecks the static form and wraps it with a call to 'fromStaticPtr'.
-- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable for an overview.
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
            tcCheckPolyExprNC expr expr_ty

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
        ; fromStaticPtr <- newMethodFromName StaticOrigin fromStaticPtrName
                                             [p_ty]
        ; let wrap = mkWpTyApps [expr_ty]
        ; loc <- getSrcSpanM
        ; return $ mkHsWrapCo co $ HsApp noExtField
                                         (L loc $ mkHsWrap wrap fromStaticPtr)
                                         (L loc (HsStatic fvs expr'))
        }

{-
************************************************************************
*                                                                      *
                Record construction and update
*                                                                      *
************************************************************************
-}

tcExpr expr@(RecordCon { rcon_con = L loc con_name
                       , rcon_flds = rbinds }) res_ty
  = do  { con_like <- tcLookupConLike con_name

        -- Check for missing fields
        ; checkMissingFields con_like rbinds

        ; (con_expr, con_sigma) <- tcInferId con_name
        ; (con_wrap, con_tau)   <- topInstantiate orig con_sigma
              -- a shallow instantiation should really be enough for
              -- a data constructor.
        ; let arity = conLikeArity con_like
              Right (arg_tys, actual_res_ty) = tcSplitFunTysN arity con_tau
        ; checkTc (conLikeHasBuilder con_like) $
          nonBidirectionalErr (conLikeName con_like)

        ; rbinds' <- tcRecordBinds con_like (map scaledThing arg_tys) rbinds
                   -- It is currently not possible for a record to have
                   -- multiplicities. When they do, `tcRecordBinds` will take
                   -- scaled types instead. Meanwhile, it's safe to take
                   -- `scaledThing` above, as we know all the multiplicities are
                   -- Many.
        ; let rcon_tc = mkHsWrap con_wrap con_expr
              expr' = RecordCon { rcon_ext = rcon_tc
                                , rcon_con = L loc con_like
                                , rcon_flds = rbinds' }

        ; tcWrapResultMono expr expr' actual_res_ty res_ty }
  where
    orig = OccurrenceOf con_name

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
look at them all; cf #3219

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
(See Note [Naughty record selectors]) in GHC.Tc.TyCl), at least
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
          (record_expr', record_rho) <- tcScalingUsage Many $ tcInferRho record_expr
            -- Record update drops some of the content of the record (namely the
            -- content of the field being updated). As a consequence, unless the
            -- field being updated is unrestricted in the record, or we need an
            -- unrestricted record. Currently, we simply always require an
            -- unrestricted record.
            --
            -- Consider the following example:
            --
            -- data R a = R { self :: a }
            -- bad :: a ⊸ ()
            -- bad x = let r = R x in case r { self = () } of { R x' -> x' }
            --
            -- This should definitely *not* typecheck.

        -- STEP -1  See Note [Disambiguating record fields] in GHC.Tc.Gen.Head
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
              (con1_tvs, _, _, _prov_theta, req_theta, scaled_con1_arg_tys, _)
                 = conLikeFullSig con1
              con1_arg_tys = map scaledThing scaled_con1_arg_tys
                -- We can safely drop the fields' multiplicities because
                -- they are currently always 1: there is no syntax for record
                -- fields with other multiplicities yet. This way we don't need
                -- to handle it in the rest of the function
              con1_flds   = map flLabel $ conLikeFieldLabels con1
              con1_tv_tys = mkTyVarTys con1_tvs
              con1_res_ty = case mtycon of
                              Just tc -> mkFamilyTyConApp tc con1_tv_tys
                              Nothing -> conLikeResTy con1 con1_tv_tys

        -- Check that we're not dealing with a unidirectional pattern
        -- synonym
        ; checkTc (conLikeHasBuilder con1) $
          nonBidirectionalErr (conLikeName con1)

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
              --   c.f. GHC.Tc.Utils.TcMType.newMetaTyVars
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

        ; co_scrut <- unifyType (Just (ppr record_expr)) record_rho scrut_ty
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
        ; let upd_tc = RecordUpdTc { rupd_cons = relevant_cons
                                   , rupd_in_tys = scrut_inst_tys
                                   , rupd_out_tys = result_inst_tys
                                   , rupd_wrap = req_wrap }
              expr' = RecordUpd { rupd_expr = mkLHsWrap fam_co $
                                              mkLHsWrapCo co_scrut record_expr'
                                , rupd_flds = rbinds'
                                , rupd_ext = upd_tc }

        ; tcWrapResult expr expr' rec_res_ty res_ty }


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

{-
************************************************************************
*                                                                      *
                Template Haskell
*                                                                      *
************************************************************************
-}

-- HsSpliced is an annotation produced by 'GHC.Rename.Splice.rnSpliceExpr'.
-- Here we get rid of it and add the finalizers to the global environment.
--
-- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
tcExpr (HsSpliceE _ (HsSpliced _ mod_finalizers (HsSplicedExpr expr)))
       res_ty
  = do addModFinalizersWithLclEnv mod_finalizers
       tcExpr expr res_ty
tcExpr (HsSpliceE _ splice)          res_ty = tcSpliceExpr splice res_ty
tcExpr e@(HsBracket _ brack)         res_ty = tcTypedBracket e brack res_ty
tcExpr e@(HsRnBracketOut _ brack ps) res_ty = tcUntypedBracket e brack ps res_ty

{-
************************************************************************
*                                                                      *
                Rebindable syntax
*                                                                      *
************************************************************************
-}

-- See Note [Rebindable syntax and HsExpansion].
tcExpr (XExpr (HsExpanded a b)) t
  = fmap (XExpr . ExpansionExpr . HsExpanded a) $
      setSrcSpan generatedSrcSpan (tcExpr b t)

{-
************************************************************************
*                                                                      *
                Catch-all
*                                                                      *
************************************************************************
-}

tcExpr other _ = pprPanic "tcExpr" (ppr other)
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
           -> TcM (HsExpr GhcTc)

tcArithSeq witness seq@(From expr) res_ty
  = do { (wrap, elt_mult, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr' <-tcScalingUsage elt_mult $ tcCheckPolyExpr expr elt_ty
       ; enum_from <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromName [elt_ty]
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from wit' (From expr') }

tcArithSeq witness seq@(FromThen expr1 expr2) res_ty
  = do { (wrap, elt_mult, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr1' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr1 elt_ty
       ; expr2' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr2 elt_ty
       ; enum_from_then <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromThenName [elt_ty]
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from_then wit' (FromThen expr1' expr2') }

tcArithSeq witness seq@(FromTo expr1 expr2) res_ty
  = do { (wrap, elt_mult, elt_ty, wit') <- arithSeqEltType witness res_ty
       ; expr1' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr1 elt_ty
       ; expr2' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr2 elt_ty
       ; enum_from_to <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromToName [elt_ty]
       ; return $ mkHsWrap wrap $
         ArithSeq enum_from_to wit' (FromTo expr1' expr2') }

tcArithSeq witness seq@(FromThenTo expr1 expr2 expr3) res_ty
  = do { (wrap, elt_mult, elt_ty, wit') <- arithSeqEltType witness res_ty
        ; expr1' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr1 elt_ty
        ; expr2' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr2 elt_ty
        ; expr3' <- tcScalingUsage elt_mult $ tcCheckPolyExpr expr3 elt_ty
        ; eft <- newMethodFromName (ArithSeqOrigin seq)
                              enumFromThenToName [elt_ty]
        ; return $ mkHsWrap wrap $
          ArithSeq eft wit' (FromThenTo expr1' expr2' expr3') }

-----------------
arithSeqEltType :: Maybe (SyntaxExpr GhcRn) -> ExpRhoType
                -> TcM (HsWrapper, Mult, TcType, Maybe (SyntaxExpr GhcTc))
arithSeqEltType Nothing res_ty
  = do { res_ty <- expTypeToType res_ty
       ; (coi, elt_ty) <- matchExpectedListTy res_ty
       ; return (mkWpCastN coi, One, elt_ty, Nothing) }
arithSeqEltType (Just fl) res_ty
  = do { ((elt_mult, elt_ty), fl')
           <- tcSyntaxOp ListOrigin fl [SynList] res_ty $
              \ [elt_ty] [elt_mult] -> return (elt_mult, elt_ty)
       ; return (idHsWrapper, elt_mult, elt_ty, Just fl') }

----------------
tcTupArgs :: [LHsTupArg GhcRn] -> [TcSigmaType] -> TcM [LHsTupArg GhcTc]
tcTupArgs args tys
  = do MASSERT( equalLength args tys )
       checkTupSize (length args)
       mapM go (args `zip` tys)
  where
    go (L l (Missing {}),     arg_ty) = do { mult <- newFlexiTyVarTy multiplicityTy
                                           ; return (L l (Missing (Scaled mult arg_ty))) }
    go (L l (Present x expr), arg_ty) = do { expr' <- tcCheckPolyExpr expr arg_ty
                                           ; return (L l (Present x expr')) }

---------------------------
-- See TcType.SyntaxOpType also for commentary
tcSyntaxOp :: CtOrigin
           -> SyntaxExprRn
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpRhoType               -- ^ overall result type
           -> ([TcSigmaType] -> [Mult] -> TcM a) -- ^ Type check any arguments,
                                                 -- takes a type per hole and a
                                                 -- multiplicity per arrow in
                                                 -- the shape.
           -> TcM (a, SyntaxExprTc)
-- ^ Typecheck a syntax operator
-- The operator is a variable or a lambda at this stage (i.e. renamer
-- output)t
tcSyntaxOp orig expr arg_tys res_ty
  = tcSyntaxOpGen orig expr arg_tys (SynType res_ty)

-- | Slightly more general version of 'tcSyntaxOp' that allows the caller
-- to specify the shape of the result of the syntax operator
tcSyntaxOpGen :: CtOrigin
              -> SyntaxExprRn
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaType] -> [Mult] -> TcM a)
              -> TcM (a, SyntaxExprTc)
tcSyntaxOpGen orig (SyntaxExprRn op) arg_tys res_ty thing_inside
  = do { (expr, sigma) <- tcInferAppHead op [] Nothing
             -- Nothing here might be improved, but all this
             -- code is scheduled for demolition anyway
       ; traceTc "tcSyntaxOpGen" (ppr op $$ ppr expr $$ ppr sigma)
       ; (result, expr_wrap, arg_wraps, res_wrap)
           <- tcSynArgA orig sigma arg_tys res_ty $
              thing_inside
       ; traceTc "tcSyntaxOpGen" (ppr op $$ ppr expr $$ ppr sigma )
       ; return (result, SyntaxExprTc { syn_expr = mkHsWrap expr_wrap expr
                                      , syn_arg_wraps = arg_wraps
                                      , syn_res_wrap  = res_wrap }) }
tcSyntaxOpGen _ NoSyntaxExprRn _ _ _ = panic "tcSyntaxOpGen"

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
          -> ([TcSigmaType] -> [Mult] -> TcM a) -- ^ check the arguments
          -> TcM (a, HsWrapper)
           -- ^ returns a wrapper :: (type of right shape) "->" (type passed in)
tcSynArgE orig sigma_ty syn_ty thing_inside
  = do { (skol_wrap, (result, ty_wrapper))
           <- tcSkolemise GenSigCtxt sigma_ty $ \ rho_ty ->
              go rho_ty syn_ty
       ; return (result, skol_wrap <.> ty_wrapper) }
    where
    go rho_ty SynAny
      = do { result <- thing_inside [rho_ty] []
           ; return (result, idHsWrapper) }

    go rho_ty SynRho   -- same as SynAny, because we skolemise eagerly
      = do { result <- thing_inside [rho_ty] []
           ; return (result, idHsWrapper) }

    go rho_ty SynList
      = do { (list_co, elt_ty) <- matchExpectedListTy rho_ty
           ; result <- thing_inside [elt_ty] []
           ; return (result, mkWpCastN list_co) }

    go rho_ty (SynFun arg_shape res_shape)
      = do { ( match_wrapper                         -- :: (arg_ty -> res_ty) "->" rho_ty
             , ( ( (result, arg_ty, res_ty, op_mult)
                 , res_wrapper )                     -- :: res_ty_out "->" res_ty
               , arg_wrapper1, [], arg_wrapper2 ) )  -- :: arg_ty "->" arg_ty_out
               <- matchExpectedFunTys herald GenSigCtxt 1 (mkCheckExpType rho_ty) $
                  \ [arg_ty] res_ty ->
                  do { arg_tc_ty <- expTypeToType (scaledThing arg_ty)
                     ; res_tc_ty <- expTypeToType res_ty

                         -- another nested arrow is too much for now,
                         -- but I bet we'll never need this
                     ; MASSERT2( case arg_shape of
                                   SynFun {} -> False;
                                   _         -> True
                               , text "Too many nested arrows in SyntaxOpType" $$
                                 pprCtOrigin orig )

                     ; let arg_mult = scaledMult arg_ty
                     ; tcSynArgA orig arg_tc_ty [] arg_shape $
                       \ arg_results arg_res_mults ->
                       tcSynArgE orig res_tc_ty res_shape $
                       \ res_results res_res_mults ->
                       do { result <- thing_inside (arg_results ++ res_results) ([arg_mult] ++ arg_res_mults ++ res_res_mults)
                          ; return (result, arg_tc_ty, res_tc_ty, arg_mult) }}

           ; return ( result
                    , match_wrapper <.>
                      mkWpFun (arg_wrapper2 <.> arg_wrapper1) res_wrapper
                              (Scaled op_mult arg_ty) res_ty doc ) }
      where
        herald = text "This rebindable syntax expects a function with"
        doc = text "When checking a rebindable syntax operator arising from" <+> ppr orig

    go rho_ty (SynType the_ty)
      = do { wrap   <- tcSubTypePat orig GenSigCtxt the_ty rho_ty
           ; result <- thing_inside [] []
           ; return (result, wrap) }

-- works on "actual" types, instantiating where necessary
-- See Note [tcSynArg]
tcSynArgA :: CtOrigin
          -> TcSigmaType
          -> [SyntaxOpType]              -- ^ argument shapes
          -> SyntaxOpType                -- ^ result shape
          -> ([TcSigmaType] -> [Mult] -> TcM a) -- ^ check the arguments
          -> TcM (a, HsWrapper, [HsWrapper], HsWrapper)
            -- ^ returns a wrapper to be applied to the original function,
            -- wrappers to be applied to arguments
            -- and a wrapper to be applied to the overall expression
tcSynArgA orig sigma_ty arg_shapes res_shape thing_inside
  = do { (match_wrapper, arg_tys, res_ty)
           <- matchActualFunTysRho herald orig Nothing
                                   (length arg_shapes) sigma_ty
              -- match_wrapper :: sigma_ty "->" (arg_tys -> res_ty)
       ; ((result, res_wrapper), arg_wrappers)
           <- tc_syn_args_e (map scaledThing arg_tys) arg_shapes $ \ arg_results arg_res_mults ->
              tc_syn_arg    res_ty  res_shape  $ \ res_results ->
              thing_inside (arg_results ++ res_results) (map scaledMult arg_tys ++ arg_res_mults)
       ; return (result, match_wrapper, arg_wrappers, res_wrapper) }
  where
    herald = text "This rebindable syntax expects a function with"

    tc_syn_args_e :: [TcSigmaType] -> [SyntaxOpType]
                  -> ([TcSigmaType] -> [Mult] -> TcM a)
                  -> TcM (a, [HsWrapper])
                    -- the wrappers are for arguments
    tc_syn_args_e (arg_ty : arg_tys) (arg_shape : arg_shapes) thing_inside
      = do { ((result, arg_wraps), arg_wrap)
               <- tcSynArgE     orig arg_ty  arg_shape  $ \ arg1_results arg1_mults ->
                  tc_syn_args_e      arg_tys arg_shapes $ \ args_results args_mults ->
                  thing_inside (arg1_results ++ args_results) (arg1_mults ++ args_mults)
           ; return (result, arg_wrap : arg_wraps) }
    tc_syn_args_e _ _ thing_inside = (, []) <$> thing_inside [] []

    tc_syn_arg :: TcSigmaType -> SyntaxOpType
               -> ([TcSigmaType] -> TcM a)
               -> TcM (a, HsWrapper)
                  -- the wrapper applies to the overall result
    tc_syn_arg res_ty SynAny thing_inside
      = do { result <- thing_inside [res_ty]
           ; return (result, idHsWrapper) }
    tc_syn_arg res_ty SynRho thing_inside
      = do { (inst_wrap, rho_ty) <- topInstantiate orig res_ty
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
      = do { wrap   <- tcSubType orig GenSigCtxt res_ty the_ty
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
-}

{- *********************************************************************
*                                                                      *
                 Record bindings
*                                                                      *
********************************************************************* -}

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
                            fixed_tvs = exactTyCoVarsOfTypes (map scaledThing fixed_tys)
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


-- Disambiguate the fields in a record update.
-- See Note [Disambiguating record fields] in GHC.Tc.Gen.Head
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
                        Unambiguous sel_name _ -> Just (x, sel_name)
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
                                  = L loc (Unambiguous i (L loc lbl)) } }


{-
Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Find the TyCon for the bindings, from the first field label.

2. Instantiate its tyvars and unify (T a1 .. an) with expected_ty.

For each binding field = value

3. Instantiate the field type (from the field label) using the type
   envt from step 2.

4  Type check the value using tcValArg, passing the field type as
   the expected argument type.

This extends OK when the field types are universally quantified.
-}

tcRecordBinds
        :: ConLike
        -> [TcType]     -- Expected type for each field
        -> HsRecordBinds GhcRn
        -> TcM (HsRecordBinds GhcTc)

tcRecordBinds con_like arg_tys (HsRecFields rbinds dd)
  = do  { mb_binds <- mapM do_bind rbinds
        ; return (HsRecFields (catMaybes mb_binds) dd) }
  where
    fields = map flSelector $ conLikeFieldLabels con_like
    flds_w_tys = zipEqual "tcRecordBinds" fields arg_tys

    do_bind :: LHsRecField GhcRn (LHsExpr GhcRn)
            -> TcM (Maybe (LHsRecField GhcTc (LHsExpr GhcTc)))
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
        -> TcM [LHsRecUpdField GhcTc]

tcRecordUpd con_like arg_tys rbinds = fmap catMaybes $ mapM do_bind rbinds
  where
    fields = map flSelector $ conLikeFieldLabels con_like
    flds_w_tys = zipEqual "tcRecordUpd" fields arg_tys

    do_bind :: LHsRecField' (AmbiguousFieldOcc GhcTc) (LHsExpr GhcRn)
            -> TcM (Maybe (LHsRecUpdField GhcTc))
    do_bind (L l fld@(HsRecField { hsRecFieldLbl = L loc af
                                 , hsRecFieldArg = rhs }))
      = do { let lbl = rdrNameAmbiguousFieldOcc af
                 sel_id = selectorAmbiguousFieldOcc af
                 f = L loc (FieldOcc (idName sel_id) (L loc lbl))
           ; mb <- tcRecordField con_like flds_w_tys f rhs
           ; case mb of
               Nothing         -> return Nothing
               Just (f', rhs') ->
                 return (Just
                         (L l (fld { hsRecFieldLbl
                                      = L loc (Unambiguous
                                               (extFieldOcc (unLoc f'))
                                               (L loc lbl))
                                   , hsRecFieldArg = rhs' }))) }

tcRecordField :: ConLike -> Assoc Name Type
              -> LFieldOcc GhcRn -> LHsExpr GhcRn
              -> TcM (Maybe (LFieldOcc GhcTc, LHsExpr GhcTc))
tcRecordField con_like flds_w_tys (L loc (FieldOcc sel_name lbl)) rhs
  | Just field_ty <- assocMaybe flds_w_tys sel_name
      = addErrCtxt (fieldCtxt field_lbl) $
        do { rhs' <- tcCheckPolyExprNC rhs field_ty
           ; let field_id = mkUserLocal (nameOccName sel_name)
                                        (nameUnique sel_name)
                                        Many field_ty loc
                -- Yuk: the field_id has the *unique* of the selector Id
                --          (so we can find it easily)
                --      but is a LocalId with the appropriate type of the RHS
                --          (so the desugarer knows the type of local binder to make)
           ; return (Just (L loc (FieldOcc field_id lbl), rhs')) }
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
    else do
        warn <- woptM Opt_WarnMissingFields
        when (warn && notNull field_strs && null field_labels)
             (warnTc (Reason Opt_WarnMissingFields) True
                 (missingFields con_like []))

  | otherwise = do              -- A record
    unless (null missing_s_fields)
           (addErrTc (missingStrictFields con_like missing_s_fields))

    warn <- woptM Opt_WarnMissingFields
    when (warn && notNull missing_ns_fields)
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

fieldCtxt :: FieldLabelString -> SDoc
fieldCtxt field_name
  = text "In the" <+> quotes (ppr field_name) <+> ptext (sLit "field of a record")

mk_op_msg :: LHsExpr GhcRn -> SDoc
mk_op_msg op = text "The operator" <+> quotes (ppr op) <+> text "takes"

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
        map (\fld -> (fld, map (fld `elementOfUniqSet`) fieldLabelSets)) $
          map (occNameFS . rdrNameOcc . rdrNameAmbiguousFieldOcc . unLoc . hsRecFieldLbl . unLoc) rbinds

    fieldLabelSets :: [UniqSet FieldLabelString]
    fieldLabelSets = map (mkUniqSet . map flLabel . conLikeFieldLabels) data_cons

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
a decent stab, no more.  See #7989.
-}

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
mixedSelectors _ _ = panic "GHC.Tc.Gen.Expr: mixedSelectors emptylists"


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
  = header <> rest
  where
    rest | null fields = Outputable.empty
         | otherwise = colon <+> pprWithCommas ppr fields
    header = text "Fields of" <+> quotes (ppr con) <+>
             text "not initialised"

-- callCtxt fun args = text "In the call" <+> parens (ppr (foldl' mkHsApp fun args))

noPossibleParents :: [LHsRecUpdField GhcRn] -> SDoc
noPossibleParents rbinds
  = hang (text "No type has all these fields:")
       2 (pprQuotedList fields)
  where
    fields = map (hsRecFieldLbl . unLoc) rbinds

badOverloadedUpdate :: SDoc
badOverloadedUpdate = text "Record update is ambiguous, and requires a type signature"

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
    checkLoop type_env visited n =
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
