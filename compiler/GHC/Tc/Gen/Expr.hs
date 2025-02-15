
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

module GHC.Tc.Gen.Expr
       ( tcCheckPolyExpr, tcCheckPolyExprNC,
         tcCheckMonoExpr, tcCheckMonoExprNC,
         tcMonoExpr, tcMonoExprNC,
         tcInferRho, tcInferRhoNC,
         tcPolyLExpr, tcPolyExpr, tcExpr, tcPolyLExprSig,
         tcSyntaxOp, tcSyntaxOpGen, SyntaxOpType(..), synKnownType,
         tcCheckId,
         ) where

import GHC.Prelude

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import {-# SOURCE #-} GHC.Tc.Gen.Splice
  ( tcTypedSplice, tcTypedBracket, tcUntypedBracket, getUntypedSpliceBody )

import GHC.Hs
import GHC.Hs.Syn.Type
import GHC.Rename.Utils
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.FieldLabel
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map
import GHC.Types.Unique.Set
import GHC.Core.Multiplicity
import GHC.Core.UsageEnv
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep_syntactic, hasFixedRuntimeRep )
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Gen.App
import GHC.Tc.Gen.Head
import GHC.Tc.Gen.Bind        ( tcLocalBinds )
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs )
import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Rename.Env         ( addUsedGRE, getUpdFieldLbls )
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.Arrow
import GHC.Tc.Gen.Match( tcBody, tcLambdaMatches, tcCaseMatches
                       , tcGRHSList, tcDoStmts )
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Zonk.TcType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Core.Class(classTyCon)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Tc.Types.Evidence
import GHC.Builtin.Types
import GHC.Builtin.Names
import GHC.Builtin.Uniques ( mkBuiltinUnique )
import GHC.Driver.DynFlags
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Data.List.SetOps
import GHC.Data.Maybe
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import Control.Monad
import qualified Data.List.NonEmpty as NE

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

tcCheckPolyExpr   expr res_ty = tcPolyLExpr   expr (mkCheckExpType res_ty)
tcCheckPolyExprNC expr res_ty = tcPolyLExprNC expr (mkCheckExpType res_ty)

-----------------
-- These versions take an ExpType
tcPolyLExpr, tcPolyLExprNC :: LHsExpr GhcRn -> ExpSigmaType
                           -> TcM (LHsExpr GhcTc)

tcPolyLExpr (L loc expr) res_ty
  = setSrcSpanA loc  $  -- Set location /first/; see GHC.Tc.Utils.Monad
    addExprCtxt expr $  -- Note [Error contexts in generated code]
    do { expr' <- tcPolyExpr expr res_ty
       ; return (L loc expr') }

tcPolyLExprNC (L loc expr) res_ty
  = setSrcSpanA loc    $
    do { expr' <- tcPolyExpr expr res_ty
       ; return (L loc expr') }

-----------------
tcPolyExpr :: HsExpr GhcRn -> ExpSigmaType -> TcM (HsExpr GhcTc)
tcPolyExpr e (Infer inf) = tcExpr e (Infer inf)
tcPolyExpr e (Check ty)  = tcPolyExprCheck e (Left ty)

-----------------
tcPolyLExprSig :: LHsExpr GhcRn -> TcCompleteSig -> TcM (LHsExpr GhcTc)
tcPolyLExprSig (L loc expr) sig
  = setSrcSpanA loc $
    -- No addExprCtxt.  For (e :: ty) we don't want generate
    --    In the expression e
    --    In the expression e :: ty
    -- We have already got an error-context for (e::ty), so when we
    -- get to `e`, just add the location
    do { traceTc "tcPolyLExprSig" (ppr loc $$ ppr expr)
       ; expr' <- tcPolyExprCheck expr (Right sig)
       ; return (L loc expr') }

-----------------
tcPolyExprCheck :: HsExpr GhcRn
                -> Either TcSigmaType TcCompleteSig
                -> TcM (HsExpr GhcTc)
-- tcPolyExpCheck deals with the special case for HsLam, in case the pushed-down
-- type is a forall-type.  E.g.    (\@a -> blah) :: forall b. b -> Int
--
-- The (Either TcSigmaType TcCompleteSig) deals with:
--   Left ty:    (f e) pushes f's argument type `ty` into `e`
--   Right sig:  (e :: sig) pushes `sig` into `e`
-- The Either stuff is entirely local to this function and its immediate callers.
--
-- See Note [Skolemisation overview] in GHC.Tc.Utils.Unify

tcPolyExprCheck expr res_ty
  = outer_skolemise res_ty $ \pat_tys rho_ty ->
    let
      -- tc_body is a little loop that looks past parentheses
      tc_body (HsPar x (L loc e))
        = setSrcSpanA loc $
          do { e' <- tc_body e
             ; return (HsPar x (L loc e')) }

      -- Look through any untyped splices (#24559)
      -- c.f. Note [Looking through Template Haskell splices in splitHsApps]
      tc_body (HsUntypedSplice splice_res _)
        = do { body <- getUntypedSpliceBody splice_res
             ; tc_body body }

      -- The special case for lambda: go to tcLambdaMatches, passing pat_tys
      tc_body e@(HsLam x lam_variant matches)
        = do { (wrap, matches') <- tcLambdaMatches e lam_variant matches pat_tys
                                                   (mkCheckExpType rho_ty)
               -- NB: tcLambdaMatches concludes with deep skolemisation,
               --     if DeepSubsumption is on;  hence no need to do that here
             ; return (mkHsWrap wrap $ HsLam x lam_variant matches') }

      -- The general case: just do deep skolemisation if necessary,
      -- before handing off to tcExpr
      tc_body e = do { ds_flag <- getDeepSubsumptionFlag
                     ; inner_skolemise ds_flag rho_ty $ \rho_ty' ->
                       tcExpr e (mkCheckExpType rho_ty') }
    in tc_body expr
  where
    -- `outer_skolemise` is used always
    -- It only does shallow skolemisation
    -- It always makes an implication constraint if deferred-errors is on
    outer_skolemise :: Either TcSigmaType TcCompleteSig
                    -> ([ExpPatType] -> TcRhoType -> TcM (HsExpr GhcTc))
                    -> TcM (HsExpr GhcTc)
    outer_skolemise (Left ty) thing_inside
      = do { (wrap, expr') <- tcSkolemiseExpectedType ty thing_inside
           ; return (mkHsWrap wrap expr') }
    outer_skolemise (Right sig) thing_inside
      = do { (wrap, expr') <- tcSkolemiseCompleteSig sig thing_inside
           ; return (mkHsWrap wrap expr') }

    -- inner_skolemise is used when we do not have a lambda
    -- With deep skolemisation we must remember to deeply skolemise
    -- after the (always-shallow) tcSkolemiseCompleteSig
    inner_skolemise :: DeepSubsumptionFlag -> TcRhoType
                    -> (TcRhoType -> TcM (HsExpr GhcTc)) -> TcM (HsExpr GhcTc)
    inner_skolemise Shallow rho_ty thing_inside
      = -- We have already done shallow skolemisation, so nothing further to do
        thing_inside rho_ty
    inner_skolemise Deep rho_ty thing_inside
      = -- Try deep skolemisation
        do { (wrap, expr') <- tcSkolemise Deep ctxt rho_ty thing_inside
           ; return (mkHsWrap wrap expr') }

    ctxt = case res_ty of
             Left {}   -> GenSigCtxt
             Right sig -> sig_ctxt sig


{- *********************************************************************
*                                                                      *
        tcExpr: the main expression typechecker
*                                                                      *
********************************************************************* -}

tcInferRho, tcInferRhoNC :: LHsExpr GhcRn -> TcM (LHsExpr GhcTc, TcRhoType)
-- Infer a *rho*-type. The return type is always instantiated.
tcInferRho (L loc expr)
  = setSrcSpanA loc   $  -- Set location /first/; see GHC.Tc.Utils.Monad
    addExprCtxt expr $  -- Note [Error contexts in generated code]
    do { (expr', rho) <- tcInfer (tcExpr expr)
       ; return (L loc expr', rho) }

tcInferRhoNC (L loc expr)
  = setSrcSpanA loc $
    do { (expr', rho) <- tcInfer (tcExpr expr)
       ; return (L loc expr', rho) }

---------------
tcCheckMonoExpr, tcCheckMonoExprNC
    :: LHsExpr GhcRn     -- Expression to type check
    -> TcRhoType         -- Expected type
                         -- Definitely no foralls at the top
    -> TcM (LHsExpr GhcTc)
tcCheckMonoExpr   expr res_ty = tcMonoExpr   expr (mkCheckExpType res_ty)
tcCheckMonoExprNC expr res_ty = tcMonoExprNC expr (mkCheckExpType res_ty)

---------------
tcMonoExpr, tcMonoExprNC
    :: LHsExpr GhcRn     -- Expression to type check
    -> ExpRhoType        -- Expected type
                         -- Definitely no foralls at the top
    -> TcM (LHsExpr GhcTc)

tcMonoExpr (L loc expr) res_ty
  = setSrcSpanA loc   $  -- Set location /first/; see GHC.Tc.Utils.Monad
    addExprCtxt expr $  -- Note [Error contexts in generated code]
    do  { expr' <- tcExpr expr res_ty
        ; return (L loc expr') }

tcMonoExprNC (L loc expr) res_ty
  = setSrcSpanA loc $
    do  { expr' <- tcExpr expr res_ty
        ; return (L loc expr') }

---------------
tcExpr :: HsExpr GhcRn
       -> ExpRhoType   -- DeepSubsumption <=> when checking, this type
                       --                     is deeply skolemised
       -> TcM (HsExpr GhcTc)

-- Use tcApp to typecheck applications, which are treated specially
-- by Quick Look.  Specifically:
--   - HsVar           lone variables, to ensure that they can get an
--                     impredicative instantiation (via Quick Look
--                     driven by res_ty (in checking mode)).
--   - HsApp           value applications
--   - HsAppType       type applications
--   - ExprWithTySig   (e :: type)
--   - HsRecSel        overloaded record fields
--   - ExpandedThingRn renamer/pre-typechecker expansions
--   - HsOpApp         operator applications
--   - HsOverLit       overloaded literals
-- These constructors are the union of
--   - ones taken apart by GHC.Tc.Gen.Head.splitHsApps
--   - ones understood by GHC.Tc.Gen.Head.tcInferAppHead_maybe
-- See Note [Application chains and heads] in GHC.Tc.Gen.App
tcExpr e@(HsVar {})              res_ty = tcApp e res_ty
tcExpr e@(HsApp {})              res_ty = tcApp e res_ty
tcExpr e@(OpApp {})              res_ty = tcApp e res_ty
tcExpr e@(HsAppType {})          res_ty = tcApp e res_ty
tcExpr e@(ExprWithTySig {})      res_ty = tcApp e res_ty

tcExpr (XExpr e)                 res_ty = tcXExpr e res_ty

tcExpr e@(HsOverLit _ lit) res_ty
  = do { mb_res <- tcShortCutLit lit res_ty
         -- See Note [Short cut for overloaded literals] in GHC.Tc.Zonk.Type
       ; case mb_res of
           Just lit' -> return (HsOverLit noExtField lit')
           Nothing   -> tcApp e res_ty }

-- Typecheck an occurrence of an unbound Id
--
-- Some of these started life as a true expression hole "_".
-- Others might simply be variables that accidentally have no binding site
tcExpr (HsUnboundVar _ occ) res_ty
  = do { ty <- expTypeToType res_ty    -- Allow Int# etc (#12531)
       ; her <- emitNewExprHole occ ty
       ; tcEmitBindingUsage bottomUE   -- Holes fit any usage environment
                                       -- (#18491)
       ; return (HsUnboundVar her occ) }

tcExpr e@(HsLit x lit) res_ty
  = do { let lit_ty = hsLitType lit
       ; tcWrapResult e (HsLit x (convertLit lit)) lit_ty res_ty }

tcExpr (HsPar x expr) res_ty
  = do { expr' <- tcMonoExprNC expr res_ty
       ; return (HsPar x expr') }

tcExpr (HsPragE x prag expr) res_ty
  = do { expr' <- tcMonoExpr expr res_ty
       ; return (HsPragE x (tcExprPrag prag) expr') }

tcExpr (NegApp x expr neg_expr) res_ty
  = do  { (expr', neg_expr')
            <- tcSyntaxOp NegateOrigin neg_expr [SynAny] res_ty $
               \[arg_ty] [arg_mult] ->
               tcScalingUsage arg_mult $ tcCheckMonoExpr expr arg_ty
        ; return (NegApp x expr' neg_expr') }

tcExpr e@(HsIPVar _ x) res_ty
  = do { ip_ty <- newFlexiTyVarTy liftedTypeKind
          -- Create a unification type variable of kind 'Type'.
          -- (The type of an implicit parameter must have kind 'Type'.)
       ; let ip_name = mkStrLitTy (hsIPNameFS x)
       ; ipClass <- tcLookupClass ipClassName
       ; ip_var <- emitWantedEvVar origin (mkClassPred ipClass [ip_name, ip_ty])
       ; tcWrapResult e
                   (fromDict ipClass ip_name ip_ty (HsVar noExtField (noLocA ip_var)))
                   ip_ty res_ty }
  where
  -- Coerces a dictionary for `IP "x" t` into `t`.
  fromDict ipClass x ty = mkHsWrap $ mkWpCastR $
                          unwrapIP $ mkClassPred ipClass [x,ty]
  origin = IPOccOrigin x

tcExpr e@(HsLam x lam_variant matches) res_ty
  = do { (wrap, matches') <- tcLambdaMatches e lam_variant matches [] res_ty
       ; return (mkHsWrap wrap $ HsLam x lam_variant matches') }

{-
************************************************************************
*                                                                      *
                Explicit lists
*                                                                      *
************************************************************************
-}

-- Explicit lists [e1,e2,e3] have been expanded already in the renamer
-- The expansion includes an ExplicitList, but it is always the built-in
-- list type, so that's all we need concern ourselves with here.  See
-- GHC.Rename.Expr. Note [Handling overloaded and rebindable constructs]
tcExpr (ExplicitList _ exprs) res_ty
  = do  { res_ty <- expTypeToType res_ty
        ; (coi, elt_ty) <- matchExpectedListTy res_ty
        ; let tc_elt expr = tcCheckPolyExpr expr elt_ty
        ; exprs' <- mapM tc_elt exprs
        ; return $ mkHsWrapCo coi $ ExplicitList elt_ty exprs' }

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
       ; tup_args1 <- tcCheckExplicitTuple tup_args arg_tys'
       ; return $ mkHsWrapCo coi (ExplicitTuple x tup_args1 boxity) }

  | otherwise
  = -- The tup_args are a mixture of Present and Missing (for tuple sections).
    do { (tup_args1, arg_tys) <- tcInferTupArgs boxity tup_args

       ; let expr'       = ExplicitTuple x tup_args1 boxity
             missing_tys = [Scaled mult ty | (Missing (Scaled mult _), ty) <- zip tup_args1 arg_tys]

             -- See Note [Typechecking data constructors] in GHC.Tc.Gen.Head
             -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make
             act_res_ty = mkScaledFunTys missing_tys (mkTupleTy1 boxity arg_tys)

       ; traceTc "ExplicitTuple" (ppr act_res_ty $$ ppr res_ty)

       ; tcWrapResultMono expr expr' act_res_ty res_ty }

tcExpr (ExplicitSum _ alt arity expr) res_ty
  = do { let sum_tc = sumTyCon arity
       ; res_ty <- expTypeToType res_ty
       ; (coi, arg_tys) <- matchExpectedTyConApp sum_tc res_ty
       ; -- Drop levity vars, we don't care about them here
         let arg_tys' = drop arity arg_tys
             arg_ty   = arg_tys' `getNth` (alt - 1)
       ; expr' <- tcCheckPolyExpr expr arg_ty
       -- Check the whole res_ty, not just the arg_ty, to avoid #20277.
       -- Example:
       --   a :: TYPE rep (representation-polymorphic)
       --   (# 17# | #) :: (# Int# | a #)
       -- This should cause an error, even though (17# :: Int#)
       -- is not representation-polymorphic: we don't know how
       -- wide the concrete representation of the sum type will be.
       ; hasFixedRuntimeRep_syntactic (FRRUnboxedSum Nothing) res_ty
       ; return $ mkHsWrapCo coi (ExplicitSum arg_tys' alt arity expr' ) }


{-
************************************************************************
*                                                                      *
                Let, case, if, do
*                                                                      *
************************************************************************
-}

tcExpr (HsLet x binds expr) res_ty
  = do  { (binds', expr') <- tcLocalBinds binds $
                             tcMonoExpr expr res_ty
        ; return (HsLet x binds' expr') }

tcExpr (HsCase ctxt scrut matches) res_ty
  = do  {  -- We used to typecheck the case alternatives first.
           -- The case patterns tend to give good type info to use
           -- when typechecking the scrutinee.  For example
           --   case (map f) of
           --     (x:xs) -> ...
           -- will report that map is applied to too few arguments
           --
           -- But now, in the GADT world, we need to typecheck the scrutinee
           -- first, to get type info that may be refined in the case alternatives
          mult <- newFlexiTyVarTy multiplicityTy

          -- Typecheck the scrutinee.  We use tcInferRho but tcInferSigma
          -- would also be possible (tcCaseMatches accepts sigma-types)
          -- Interesting litmus test: do these two behave the same?
          --     case id        of {..}
          --     case (\v -> v) of {..}
          -- This design choice is discussed in #17790
        ; (scrut', scrut_ty) <- tcScalingUsage mult $ tcInferRho scrut

        ; hasFixedRuntimeRep_syntactic FRRCase scrut_ty
        ; matches' <- tcCaseMatches tcBody (Scaled mult scrut_ty) matches res_ty
        ; return (HsCase ctxt scrut' matches') }

tcExpr (HsIf x pred b1 b2) res_ty
  = do { pred'    <- tcCheckMonoExpr pred boolTy
       ; (u1,b1') <- tcCollectingUsage $ tcMonoExpr b1 res_ty
       ; (u2,b2') <- tcCollectingUsage $ tcMonoExpr b2 res_ty
       ; tcEmitBindingUsage (supUE u1 u2)
       ; return (HsIf x pred' b1' b2') }

{-
Note [MultiWayIf linearity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we'd like to compute the usage environment for

if | b1 -> e1
   | b2 -> e2
   | otherwise -> e3

and let u1, u2, v1, v2, v3 denote the usage env for b1, b2, e1, e2, e3
respectively.

Since a multi-way if is mere sugar for nested if expressions, the usage
environment should ideally be u1 + sup(v1, u2 + sup(v2, v3)).
However, currently we don't support linear guards (#19193). All variables
used in guards from u1 and u2 will have multiplicity Many.
But in that case, we have equality u1 + sup(x,y) = sup(u1 + x, y),
                      and likewise u2 + sup(x,y) = sup(u2 + x, y) for any x,y.
Using this identity, we can just compute sup(u1 + v1, u2 + v2, v3) instead.
This is simple to do, since we get u_i + v_i directly from tcGRHS.
If we add linear guards, this code will have to be revisited.
Not using 'sup' caused #23814.
-}

tcExpr (HsMultiIf _ alts) res_ty
  = do { alts' <- tcGRHSList IfAlt tcBody alts res_ty
                  -- See Note [MultiWayIf linearity checking]
       ; res_ty <- readExpType res_ty
       ; return (HsMultiIf res_ty alts') }

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
        ; typeableClass <- tcLookupClass typeableClassName
        ; typeable_ev <- emitWantedEvVar StaticOrigin $
                  mkTyConApp (classTyCon typeableClass)
                             [liftedTypeKind, expr_ty]

        -- Insert the constraints of the static form in a global list for later
        -- validation.  See #13499 for an explanation of why this really isn't the
        -- right thing to do: the enclosing skolems aren't in scope any more!
        -- Static forms really aren't well worked out yet.
        ; emitStaticConstraints lie

        -- Wrap the static form with the 'fromStaticPtr' call.
        ; fromStaticPtr <- newMethodFromName StaticOrigin fromStaticPtrName
                                             [p_ty]
        ; let wrap = mkWpEvVarApps [typeable_ev] <.> mkWpTyApps [expr_ty]
        ; loc <- getSrcSpanM
        ; static_ptr_ty_con <- tcLookupTyCon staticPtrTyConName
        ; return $ mkHsWrapCo co $ HsApp noExtField
                            (L (noAnnSrcSpan loc) $ mkHsWrap wrap fromStaticPtr)
                            (L (noAnnSrcSpan loc) (HsStatic (fvs, mkTyConApp static_ptr_ty_con [expr_ty]) expr'))
        }

tcExpr (HsEmbTy _ _)      _ = failWith (TcRnIllegalTypeExpr TypeKeywordSyntax)
tcExpr (HsQual _ _ _)     _ = failWith (TcRnIllegalTypeExpr ContextArrowSyntax)
tcExpr (HsForAll _ _ _)   _ = failWith (TcRnIllegalTypeExpr ForallTelescopeSyntax)
tcExpr (HsFunArr _ _ _ _) _ = failWith (TcRnIllegalTypeExpr FunctionArrowSyntax)

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

        ; (con_expr, con_sigma) <- tcInferConLike con_like
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

        ; ret <- tcWrapResultMono expr expr' actual_res_ty res_ty

        -- Check for missing fields.  We do this after type-checking to get
        -- better types in error messages (cf #18869).  For example:
        --     data T a = MkT { x :: a, y :: a }
        --     r = MkT { y = True }
        -- Then we'd like to warn about a missing field `x :: True`, rather than `x :: a0`.
        --
        -- NB: to do this really properly we should delay reporting until typechecking is complete,
        -- via a new `HoleSort`.  But that seems too much work.
        ; checkMissingFields con_like rbinds arg_tys

        ; return ret }
  where
    orig = OccurrenceOf con_name

-- Record updates via dot syntax are replaced by expanded expressions
-- in the renamer. See Note [Overview of record dot syntax] in
-- GHC.Hs.Expr. This is why we match on 'rupd_flds = Left rbnds' here
-- and panic otherwise.
tcExpr expr@(RecordUpd { rupd_expr = record_expr
                       , rupd_flds =
                           RegularRecUpdFields
                             { xRecUpdFields = possible_parents
                             , recUpdFields  = rbnds }
                       })
       res_ty
  = assert (notNull rbnds) $
    do  { -- Expand the record update. See Note [Record Updates].
        ; (ds_expr, ds_res_ty, err_ctxt)
            <- expandRecordUpd record_expr possible_parents rbnds res_ty

          -- Typecheck the expanded expression.
        ; expr' <- addErrCtxt err_ctxt $
                   tcExpr (mkExpandedExpr expr ds_expr) (Check ds_res_ty)
            -- NB: it's important to use ds_res_ty and not res_ty here.
            -- Test case: T18802b.

        ; addErrCtxt err_ctxt $ tcWrapResultMono expr expr' ds_res_ty res_ty
            -- We need to unify the result type of the expanded
            -- expression with the expected result type.
            --
            -- See Note [Unifying result types in tcRecordUpd].
            -- Test case: T10808.
        }

tcExpr e@(RecordUpd { rupd_flds = OverloadedRecUpdFields {}}) _
  = pprPanic "tcExpr: unexpected overloaded-dot RecordUpd" $ ppr e

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
                Record dot syntax
*                                                                      *
************************************************************************
-}

-- These terms have been replaced by their expanded expressions in the renamer. See
-- Note [Overview of record dot syntax].
tcExpr (HsGetField _ _ _) _ = panic "GHC.Tc.Gen.Expr: tcExpr: HsGetField: Not implemented"
tcExpr (HsProjection _ _) _ = panic "GHC.Tc.Gen.Expr: tcExpr: HsProjection: Not implemented"

{-
************************************************************************
*                                                                      *
                Template Haskell
*                                                                      *
************************************************************************
-}

-- Here we get rid of it and add the finalizers to the global environment.
-- See Note [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
tcExpr (HsTypedSplice ext splice)   res_ty = tcTypedSplice ext splice res_ty
tcExpr e@(HsTypedBracket _ body)    res_ty = tcTypedBracket e body res_ty

tcExpr e@(HsUntypedBracket ps body) res_ty = tcUntypedBracket e body ps res_ty
tcExpr (HsUntypedSplice splice _)   res_ty
  -- Since `tcApp` deals with `HsUntypedSplice` (in `splitHsApps`), you might
  -- wonder why we don't delegate to `tcApp` as we do for `HsVar`, etc.
  -- (See the initial block of equations for `tcExpr`.) But we can't do this
  -- for `HsUntypedSplice`; to see why, read Wrinkle (UTS1) in
  -- Note [Looking through Template Haskell splices in splitHsApps] in
  -- GHC.Tc.Gen.Head.
  = do { expr <- getUntypedSpliceBody splice
       ; tcExpr expr res_ty }

{-
************************************************************************
*                                                                      *
                Catch-all
*                                                                      *
************************************************************************
-}

tcExpr (HsOverLabel {})    ty = pprPanic "tcExpr:HsOverLabel"  (ppr ty)
tcExpr (SectionL {})       ty = pprPanic "tcExpr:SectionL"    (ppr ty)
tcExpr (SectionR {})       ty = pprPanic "tcExpr:SectionR"    (ppr ty)

tcExpr (HsModifiedExpr _ mods e) res_ty = do
  -- We don't do anything with modifiers, but we do need to make sure they type
  -- check.
  _ <- tcModifiers mods (const $ Left DontSuggestLinear)
  e' <- tcMonoExpr e res_ty
  return $ HsModifiedExpr noExtField [] e'


{-
************************************************************************
*                                                                      *
                Expansion Expressions (XXExprGhcRn)
*                                                                      *
************************************************************************
-}

tcXExpr :: XXExprGhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)

tcXExpr (PopErrCtxt (L loc e)) res_ty
  = popErrCtxt $ -- See Part 3 of Note [Expanding HsDo with XXExprGhcRn] in `GHC.Tc.Gen.Do`
      setSrcSpanA loc $
      tcExpr e res_ty

tcXExpr xe@(ExpandedThingRn o e') res_ty
  | OrigStmt ls@(L loc s@LetStmt{}) <- o
  , HsLet x binds e <- e'
  =  do { (binds', e') <-  setSrcSpanA loc $
                           addStmtCtxt s $
                           tcLocalBinds binds $
                           tcMonoExprNC e res_ty -- NB: Do not call tcMonoExpr here as it adds
                                                 -- a duplicate error context
        ; return $ mkExpandedStmtTc ls (HsLet x binds' e')
        }
  | OrigStmt ls@(L loc s@LastStmt{}) <- o
  =  setSrcSpanA loc $
          addStmtCtxt s $
          mkExpandedStmtTc ls <$> tcExpr e' res_ty
                -- It is important that we call tcExpr (and not tcApp) here as
                -- `e` is the last statement's body expression
                -- and not a HsApp of a generated (>>) or (>>=)
                -- This improves error messages e.g. tests: DoExpansion1, DoExpansion2, DoExpansion3
  | OrigStmt ls@(L loc _) <- o
  = setSrcSpanA loc $
       mkExpandedStmtTc ls <$> tcApp (XExpr xe) res_ty
tcXExpr xe res_ty = tcApp (XExpr xe) res_ty

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
       ; return (mkWpCastN coi, OneTy, elt_ty, Nothing) }
arithSeqEltType (Just fl) res_ty
  = do { ((elt_mult, elt_ty), fl')
           <- tcSyntaxOp ListOrigin fl [SynList] res_ty $
              \ [elt_ty] [elt_mult] -> return (elt_mult, elt_ty)
       ; return (idHsWrapper, elt_mult, elt_ty, Just fl') }

----------------

-- | Typecheck an explicit tuple @(a,b,c)@ or @(\#a,b,c\#)@.
--
-- Does not handle tuple sections.
tcCheckExplicitTuple :: [HsTupArg GhcRn]
                     -> [TcSigmaType]
                          -- ^ Argument types.
                          -- This function ensures they all have
                          -- a fixed runtime representation.
                     -> TcM [HsTupArg GhcTc]
tcCheckExplicitTuple args tys
  = do massert (equalLength args tys)
       checkTupSize (length args)
       zipWith3M go [1,2..] args tys
  where
    go :: Int -> HsTupArg GhcRn -> TcType -> TcM (HsTupArg GhcTc)
    go i (Missing {})     arg_ty
      = pprPanic "tcCheckExplicitTuple: tuple sections not handled here"
          (ppr i $$ ppr arg_ty)
    go i (Present x expr) arg_ty
      = do { expr' <- tcCheckPolyExpr expr arg_ty
           ; (co, _) <- hasFixedRuntimeRep (FRRUnboxedTuple i) arg_ty
           ; return (Present x (mkLHsWrap (mkWpCastN co) expr')) }

-- | Typecheck an explicit tuple or tuple section by performing type inference.
tcInferTupArgs :: Boxity
               -> [HsTupArg GhcRn] -- ^ argument types
               -> TcM ([HsTupArg GhcTc], [TcSigmaTypeFRR])
tcInferTupArgs boxity args
  = do { checkTupSize (length args)
       ; zipWithAndUnzipM tc_infer_tup_arg [1,2..] args }
 where
  tc_infer_tup_arg :: Int -> HsTupArg GhcRn -> TcM (HsTupArg GhcTc, TcSigmaTypeFRR)
  tc_infer_tup_arg i (Missing {})
    = do { mult <- newFlexiTyVarTy multiplicityTy
         ; arg_ty <- new_arg_ty i
         ; return (Missing (Scaled mult arg_ty), arg_ty) }
  tc_infer_tup_arg i (Present x lexpr@(L l expr))
    = do { (expr', arg_ty) <- case boxity of
             Unboxed -> tcInferFRR (FRRUnboxedTuple i) (tcPolyExpr expr)
             Boxed   -> do { arg_ty <- newFlexiTyVarTy liftedTypeKind
                           ; L _ expr' <- tcCheckPolyExpr lexpr arg_ty
                           ; return (expr', arg_ty) }
         ; return (Present x (L l expr'), arg_ty) }

  new_arg_ty :: Int -> TcM TcTypeFRR
  new_arg_ty i =
    case boxity of
      Unboxed -> newOpenFlexiFRRTyVarTy (FRRUnboxedTupleSection i)
      Boxed   -> newFlexiTyVarTy liftedTypeKind

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
              -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a)
              -> TcM (a, SyntaxExprTc)
tcSyntaxOpGen orig (SyntaxExprRn op) arg_tys res_ty thing_inside
  = do { (expr, sigma) <- tcInferAppHead (op, VACall op 0 noSrcSpan)
             -- Ugh!! But all this code is scheduled for demolition anyway
       ; traceTc "tcSyntaxOpGen" (ppr op $$ ppr expr $$ ppr sigma)
       ; (result, expr_wrap, arg_wraps, res_wrap)
           <- tcSynArgA orig op sigma arg_tys res_ty $
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
          -> HsExpr GhcRn -- ^ the operator to check (for error messages only)
          -> TcSigmaType
          -> SyntaxOpType                -- ^ shape it is expected to have
          -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a) -- ^ check the arguments
          -> TcM (a, HsWrapper)
           -- ^ returns a wrapper :: (type of right shape) "->" (type passed in)
tcSynArgE orig op sigma_ty syn_ty thing_inside
  = do { (skol_wrap, (result, ty_wrapper))
           <- tcSkolemise Shallow GenSigCtxt sigma_ty $ \rho_ty ->
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
                  \ [ExpFunPatTy arg_ty] res_ty ->
                  do { arg_tc_ty <- expTypeToType (scaledThing arg_ty)
                     ; res_tc_ty <- expTypeToType res_ty

                         -- another nested arrow is too much for now,
                         -- but I bet we'll never need this
                     ; massertPpr (case arg_shape of
                                   SynFun {} -> False;
                                   _         -> True)
                                  (text "Too many nested arrows in SyntaxOpType" $$
                                   pprCtOrigin orig)

                     ; let arg_mult = scaledMult arg_ty
                     ; tcSynArgA orig op arg_tc_ty [] arg_shape $
                       \ arg_results arg_res_mults ->
                       tcSynArgE orig op res_tc_ty res_shape $
                       \ res_results res_res_mults ->
                       do { result <- thing_inside (arg_results ++ res_results) ([arg_mult] ++ arg_res_mults ++ res_res_mults)
                          ; return (result, arg_tc_ty, res_tc_ty, arg_mult) }}

           ; let fun_wrap = mkWpFun (arg_wrapper2 <.> arg_wrapper1) res_wrapper
                              (Scaled op_mult arg_ty) res_ty
               -- NB: arg_ty comes from matchExpectedFunTys, so it has a
               -- fixed RuntimeRep, as needed to call mkWpFun.
           ; return (result, match_wrapper <.> fun_wrap) }
      where
        herald = ExpectedFunTySyntaxOp orig op

    go rho_ty (SynType the_ty)
      = do { wrap   <- tcSubTypePat orig GenSigCtxt the_ty rho_ty
           ; result <- thing_inside [] []
           ; return (result, wrap) }

-- works on "actual" types, instantiating where necessary
-- See Note [tcSynArg]
tcSynArgA :: CtOrigin
          -> HsExpr GhcRn -- ^ the operator we are checking (for error messages)
          -> TcSigmaType
          -> [SyntaxOpType]              -- ^ argument shapes
          -> SyntaxOpType                -- ^ result shape
          -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a) -- ^ check the arguments
          -> TcM (a, HsWrapper, [HsWrapper], HsWrapper)
            -- ^ returns a wrapper to be applied to the original function,
            -- wrappers to be applied to arguments
            -- and a wrapper to be applied to the overall expression
tcSynArgA orig op sigma_ty arg_shapes res_shape thing_inside
  = do { (match_wrapper, arg_tys, res_ty)
           <- matchActualFunTys herald orig (length arg_shapes) sigma_ty
              -- match_wrapper :: sigma_ty "->" (arg_tys -> res_ty)
       ; ((result, res_wrapper), arg_wrappers)
           <- tc_syn_args_e (map scaledThing arg_tys) arg_shapes $ \ arg_results arg_res_mults ->
              tc_syn_arg    res_ty  res_shape  $ \ res_results ->
              thing_inside (arg_results ++ res_results) (map scaledMult arg_tys ++ arg_res_mults)
       ; return (result, match_wrapper, arg_wrappers, res_wrapper) }
  where
    herald = ExpectedFunTySyntaxOp orig op

    tc_syn_args_e :: [TcSigmaTypeFRR] -> [SyntaxOpType]
                  -> ([TcSigmaTypeFRR] -> [Mult] -> TcM a)
                  -> TcM (a, [HsWrapper])
                    -- the wrappers are for arguments
    tc_syn_args_e (arg_ty : arg_tys) (arg_shape : arg_shapes) thing_inside
      = do { ((result, arg_wraps), arg_wrap)
               <- tcSynArgE     orig  op arg_ty  arg_shape  $ \ arg1_results arg1_mults ->
                  tc_syn_args_e          arg_tys arg_shapes $ \ args_results args_mults ->
                  thing_inside (arg1_results ++ args_results) (arg1_mults ++ args_mults)
           ; return (result, arg_wrap : arg_wraps) }
    tc_syn_args_e _ _ thing_inside = (, []) <$> thing_inside [] []

    tc_syn_arg :: TcSigmaTypeFRR -> SyntaxOpType
               -> ([TcSigmaTypeFRR] -> TcM a)
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
           ; return (result, mkWpCastN (mkSymCo list_co) <.> inst_wrap) }
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
                 Expanding record update
*                                                                      *
********************************************************************* -}

{- Note [Record Updates]
~~~~~~~~~~~~~~~~~~~~~~~~
To typecheck a record update, we expand it first.  Suppose we have
    data T p q = T1 { x :: Int, y :: Bool, z :: Char }
               | T2 { v :: Char }
               | T3 { x :: Int }
               | T4 { p :: Float, y :: Bool, x :: Int }
               | T5
Then the record update `e { x=e1, y=e2 }` expands as follows

       e { x=e1, y=e2 }
    ===>
       let { x' = e1; y' = e2 } in
       case e of
          T1 _ _ z -> T1 x' y' z
          T4 p _ _ -> T4 p y' x'
T2, T3 and T5 should not occur, so we omit them from the match.
The critical part of expansion is to identify T and then T1/T4.

Wrinkle [Disambiguating fields]

  As explained in Note [Disambiguating record updates] in GHC.Rename.Pat,
  to typecheck a record update we first need to disambiguate the field labels,
  in order to find a parent which has at least one constructor with all of the fields
  being updated.

  As mentioned in Note [Type-directed record disambiguation], we sometimes use
  type-directed disambiguation, although this mechanism is deprecated and
  scheduled for removal via the implementation of GHC proposal #366
  https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst.


All in all, this means that when typechecking a record update via expansion,
we take the following steps:

  (0) Perform a first typechecking pass on the record expression (`e` in the example above),
      to infer the type of the record being updated.
  (1) Disambiguate the record fields (potentially using the type obtained in (0)).
  (2) Expand the record update as described above, using an XXExprGhcRn.
      (a) Create a let-binding to share the record update right-hand sides.
      (b) Expand the record update to a case expression updating all the
          relevant constructors (those that have all of the fields being updated).
  (3) Typecheck the expanded code.

In (0), we call inferRho to infer the type of the record being updated. This returns the
inferred type of the record, together with a typechecked expression (of type HsExpr GhcTc)
and a collection of residual constraints.
We have no need for the latter two, because we will typecheck again in (D3). So, for
the time being (and until GHC proposal #366 is implemented), we simply drop them.

Wrinkle [Using IdSig]

  As noted above, we want to let-bind the updated fields to avoid code duplication:

    let { x' = e1; y' = e2 } in
    case e of
       T1 _ _ z -> T1 x' y' z
       T4 p _ _ -> T4 p y' x'

  However, doing so in a naive way would cause difficulties for type inference.
  For example:

    data R b = MkR { f :: (forall a. a -> a) -> (Int,b), c :: Int }
    foo r = r { f = \ k -> (k 3, k 'x') }

  If we expand to:

    ds_foo r =
      let f' = \ k -> (k 3, k 'x')
      in case r of
        MkR _ b -> MkR f' b

  then we are unable to infer an appropriately polymorphic type for f', because we
  never infer higher-rank types. To circumvent this problem, we proceed as follows:

    1. Obtain general field types by instantiating any of the constructors
       that contain all the necessary fields. (Note that the field type must be
       identical across different constructors of a given data constructor).
    2. Let-bind an 'IdSig' with this type. This amounts to giving the let-bound
       'Id's a partial type signature.

  In the above example, it's as if we wrote:

    ds_foo r =
      let f' :: (forall a. a -> a) -> (Int, _b)
          f' = \ k -> (k 3, k 'x')
      in case r of
        MkR _ b -> MkR f' b

  This allows us to compute the right type for f', and thus accept this record update.

Note [Type-directed record disambiguation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC currently supports an additional type-directed disambiguation
mechanism, which is deprecated and scheduled for removal as part of
GHC proposal #366 https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst.

To perform this disambiguation, when there are multiple possible parents for
a record update, the renamer defers to the typechecker.
See GHC.Tc.Gen.Expr.disambiguateRecordBinds, and in particular the auxiliary
function identifyParentLabels, which picks a parent for the record update
using the following additional mechanisms:

  (a) Use the type being pushed in, if it is already a TyConApp. The
      following are valid updates at type `R`:

        g :: R -> R
        g x = x { fld1 = 3 }

        g' x = x { fld1 = 3 } :: R

  (b) Use the type signature of the record expression, if it exists and
      is a TyConApp. Thus this is valid update at type `R`:

        h x = (x :: R) { fld1 = 3 }

Note that this type-directed disambiguation mechanism isn't very robust,
as it doesn't properly integrate with the rest of the typechecker.
For example, the following updates will all be rejected as ambiguous:

    let r :: R
        r = blah
    in r { foo = 3 }

    \r. (r { foo = 3 }, r :: R)

Record updates which require constraint-solving should instead use the
-XOverloadedRecordUpdate extension, as described in Note [Overview of record dot syntax].

Note [Unifying result types in tcRecordUpd]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After expanding and typechecking a record update in the way described in
Note [Record Updates], we must take care to unify the result types.

Example:

  type family F (a :: Type) :: Type where {}
  data D a = MkD { fld :: F a }

  f :: F Int -> D Bool -> D Int
  f i r = r { fld = i }

This record update expands to:

  let x :: F alpha -- metavariable
      x = i
  in case r of
    MkD _ -> MkD x

Because the type family F is not injective, our only hope for unifying the
metavariable alpha is through the result type of the record update, which tells
us that we should unify alpha := Int.

Test case: T10808.

Wrinkle [GADT result type in tcRecordUpd]

  When dealing with a GADT, we want to be careful about which result type we use.

  Example:

    data G a b where
      MkG :: { bar :: F a } -> G a Int

    g :: F Int -> G Float b -> G Int b
    g i r = r { bar = i }

    We **do not** want to use the result type from the constructor MkG, which would
    leave us with a result type "G alpha Int". Instead, we should use the result type
    from the GADT header, instantiating as above, to get "G alpha beta" which will get
    unified withy "G Int b".

    Test cases: T18809, HardRecordUpdate.

-}

-- | Expands a record update @record_expr { fld1 = e1, fld2 = e2 }@ into a case expression
-- that matches on the constructors of the record @r@, as described in
-- Note [Record Updates].
--
-- Returns a renamed but not-yet-typechecked expression, together with the
-- result type of this expanded record update.
expandRecordUpd :: LHsExpr GhcRn
                      -- ^ @record_expr@: expression to which the record update is applied
                 -> NE.NonEmpty (HsRecUpdParent GhcRn)
                      -- ^ Possible parent 'TyCon'/'PatSyn's for the record update,
                      -- with the associated constructors and field labels
                 -> [LHsRecUpdField GhcRn GhcRn]
                      -- ^ the record update fields
                 -> ExpRhoType
                      -- ^ the expected result type of the record update
                 -> TcM ( HsExpr GhcRn
                           -- Expanded record update expression
                        , TcType
                           -- result type of expanded record update
                        , SDoc
                           -- error context to push when typechecking
                           -- the expanded code
                        )
expandRecordUpd record_expr possible_parents rbnds res_ty
  = do {  -- STEP 0: typecheck the record_expr, the record to be updated.
          --
          -- Until GHC proposal #366 is implemented, we still use the type of
          -- the record to disambiguate its fields, so we must infer the record
          -- type here before we can expand. See Wrinkle [Disambiguating fields]
          -- in Note [Record Updates].
       ; ((_, record_rho), _lie) <- captureConstraints    $ -- see (1) below
                                    tcScalingUsage ManyTy $ -- see (2) below
                                    tcInferRho record_expr

            -- (1)
            -- Note that we capture, and then discard, the constraints.
            -- This `tcInferRho` is used *only* to identify the data type,
            -- so we can deal with field disambiguation.
            -- Then we are going to generate a expanded record update, including `record_expr`,
            -- and typecheck it from scratch.  We don't want to generate the constraints twice!

            -- (2)
            -- Record update drops some of the content of the record (namely the
            -- content of the field being updated). As a consequence, unless the
            -- field being updated is unrestricted in the record, we need an
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

       -- STEP 1: disambiguate the record update by computing a single parent
       --         which has a constructor with all of the fields being updated.
       --
       -- See Note [Disambiguating record updates] in GHC.Rename.Pat.
       ; (cons, rbinds)
           <- disambiguateRecordBinds record_expr record_rho possible_parents rbnds res_ty
       ; let sel_ids       = map (unLoc . foLabel . unLoc . hfbLHS . unLoc) rbinds
             upd_fld_names = map idName sel_ids
             relevant_cons = nonDetEltsUniqSet cons
             relevant_con  = head relevant_cons

      -- STEP 2: expand the record update.
      --
      --  (a) Create new variables for the fields we are updating,
      --      so that we can share them across constructors.
      --
      --      Example:
      --
      --          e { x=e1, y=e2 }
      --
      --        We want to let-bind variables to `e1` and `e2`:
      --
      --          let x' :: Int
      --              x' = e1
      --              y' :: Bool
      --              y' = e2
      --          in ...

         -- Instantiate the type variables of any relevant constuctor
         -- with metavariables to obtain a type for each 'Id'.
         -- This will allow us to have 'Id's with polymorphic types
         -- by using 'IdSig'. See Wrinkle [Using IdSig] in Note [Record Updates].
       ; let (univ_tvs, ex_tvs, eq_spec, _, _, arg_tys, con_res_ty) = conLikeFullSig relevant_con
       ; (subst, tc_tvs) <- newMetaTyVars (univ_tvs ++ ex_tvs)
       ; let (actual_univ_tys, _actual_ex_tys) = splitAtList univ_tvs $ map mkTyVarTy tc_tvs

             -- See Wrinkle [GADT result type in tcRecordUpd]
             -- for an explanation of the following.
             ds_res_ty = case relevant_con of
               RealDataCon con
                 | not (null eq_spec) -- We only need to do this if we have actual GADT equalities.
                 -> mkFamilyTyConApp (dataConTyCon con) actual_univ_tys
               _ -> substTy subst con_res_ty

       -- Gather pairs of let-bound Ids and their right-hand sides,
       -- e.g. (x', e1), (y', e2), ...
       ; let mk_upd_id :: Name -> LHsFieldBind GhcTc fld (LHsExpr GhcRn) -> TcM (Name, (TcId, LHsExpr GhcRn))
             mk_upd_id fld_nm (L _ rbind)
               = do { let Scaled _ arg_ty = lookupNameEnv_NF arg_ty_env fld_nm
                          nm_occ = rdrNameOcc . nameRdrName $ fld_nm
                          actual_arg_ty = substTy subst arg_ty
                          rhs = hfbRHS rbind
                    ; (_co, actual_arg_ty) <- hasFixedRuntimeRep (FRRRecordUpdate fld_nm (unLoc rhs)) actual_arg_ty
                      -- We get a better error message by doing a (redundant) representation-polymorphism check here,
                      -- rather than delaying until the typechecker typechecks the let-bindings,
                      -- because the let-bound Ids have internal names.
                      -- (As we will typecheck the let-bindings later, we can drop this coercion here.)
                      -- See RepPolyRecordUpdate test.
                    ; nm <- newNameAt nm_occ generatedSrcSpan
                    ; let id = mkLocalId nm ManyTy actual_arg_ty
                      -- NB: create fresh names to avoid any accidental shadowing
                      -- occurring in the RHS expressions when creating the let bindings:
                      --
                      --  let x1 = e1; x2 = e2; ...
                      --
                      -- Above, we use multiplicity Many rather than the one associated to arg_ty.
                      -- Normally, there shouldn't be a difference, since it's a let binding.
                      -- But -XStrict can convert the let to a case, and this causes issues
                      -- in test LinearRecUpd. Since we don't support linear record updates,
                      -- using Many is simple and safe.
                    ; return (fld_nm, (id, rhs))
                    }
             arg_ty_env = mkNameEnv
                        $ zipWith (\ lbl arg_ty -> (flSelector lbl, arg_ty))
                            (conLikeFieldLabels relevant_con)
                            arg_tys

       ; traceTc "tcRecordUpd" $
           vcat [ text "upd_fld_names:" <+> ppr upd_fld_names
                , text "relevant_cons:" <+> ppr relevant_cons ]

       ; upd_ids <- zipWithM mk_upd_id upd_fld_names rbinds
       ; let updEnv :: UniqMap Name (Id, LHsExpr GhcRn)
             updEnv = listToUniqMap $ upd_ids

             make_pat :: ConLike -> LMatch GhcRn (LHsExpr GhcRn)
             -- As explained in Note [Record Updates], to expand
             --
             --   e { x=e1, y=e2 }
             --
             -- we generate a case statement, with an equation for
             -- each constructor of the record. For example, for
             -- the constructor
             --
             --   T1 :: { x :: Int, y :: Bool, z :: Char } -> T p q
             --
             -- we let-bind x' = e1, y' = e2 and generate the equation:
             --
             --   T1 _ _ z -> T1 x' y' z
             make_pat conLike = mkSimpleMatch RecUpd (noLocA [pat]) rhs
               where
                 (lhs_con_pats, rhs_con_args)
                    = zipWithAndUnzip mk_con_arg [1..] con_fields
                 pat = genSimpleConPat con lhs_con_pats
                 rhs = wrapGenSpan $ genHsApps con rhs_con_args
                 con = conLikeName conLike
                 con_fields = conLikeFieldLabels conLike

             mk_con_arg :: Int
                        -> FieldLabel
                        -> ( LPat GhcRn
                              -- LHS constructor pattern argument
                           , LHsExpr GhcRn )
                              -- RHS constructor argument
             mk_con_arg i fld_lbl =
               -- The following generates the pattern matches of the expanded `case` expression.
               -- For fields being updated (for example `x`, `y` in T1 and T4 in Note [Record Updates]),
               -- wildcards are used to avoid creating unused variables.
               case lookupUniqMap updEnv $ flSelector fld_lbl of
                 -- Field is being updated: LHS = wildcard pattern, RHS = appropriate let-bound Id.
                 Just (upd_id, _) -> (genWildPat, genLHsVar (idName upd_id))
                 -- Field is not being updated: LHS = variable pattern, RHS = that same variable.
                 _  -> let fld_nm = mkInternalName (mkBuiltinUnique i)
                                      (nameOccName $ flSelector $ fld_lbl)
                                      generatedSrcSpan
                       in (genVarPat fld_nm, genLHsVar fld_nm)

       -- STEP 2 (b): expand to HsCase, as per note [Record Updates]
       ; let ds_expr :: HsExpr GhcRn
             ds_expr = HsLet noExtField let_binds (L gen case_expr)

             case_expr :: HsExpr GhcRn
             case_expr = HsCase RecUpd record_expr
                       $ mkMatchGroup (Generated OtherExpansion DoPmc) (wrapGenSpan matches)
             matches :: [LMatch GhcRn (LHsExpr GhcRn)]
             matches = map make_pat relevant_cons

             let_binds :: HsLocalBindsLR GhcRn GhcRn
             let_binds = HsValBinds noAnn $ XValBindsLR
                       $ NValBinds upd_ids_lhs (map mk_idSig upd_ids)
             upd_ids_lhs :: [(RecFlag, LHsBindsLR GhcRn GhcRn)]
             upd_ids_lhs = [ (NonRecursive, [genSimpleFunBind (idName id) [] rhs])
                           | (_, (id, rhs)) <- upd_ids ]
             mk_idSig :: (Name, (Id, LHsExpr GhcRn)) -> LSig GhcRn
             mk_idSig (_, (id, _)) = L gen $ XSig $ IdSig id
               -- We let-bind variables using 'IdSig' in order to accept
               -- record updates involving higher-rank types.
               -- See Wrinkle [Using IdSig] in Note [Record Updates].
             gen = noAnnSrcSpan generatedSrcSpan

        ; traceTc "expandRecordUpd" $
            vcat [ text "relevant_con:" <+> ppr relevant_con
                 , text "res_ty:" <+> ppr res_ty
                 , text "ds_res_ty:" <+> ppr ds_res_ty
                 ]

        ; let cons = pprQuotedList relevant_cons
              err_lines =
                (text "In a record update at field" <> plural upd_fld_names <+> pprQuotedList upd_fld_names :)
                $ case relevant_con of
                     RealDataCon con ->
                        [ text "with type constructor" <+> quotes (ppr (dataConTyCon con))
                        , text "data constructor" <+> plural relevant_cons <+> cons ]
                     PatSynCon {} ->
                        [ text "with pattern synonym" <+> plural relevant_cons <+> cons ]
                ++ if null ex_tvs
                   then []
                   else [ text "existential variable" <> plural ex_tvs <+> pprQuotedList ex_tvs ]
              err_ctxt = make_lines_msg err_lines

        ; return (ds_expr, ds_res_ty, err_ctxt) }

-- | Pretty-print a collection of lines, adding commas at the end of each line,
-- and adding "and" to the start of the last line.
make_lines_msg :: [SDoc] -> SDoc
make_lines_msg []      = empty
make_lines_msg [last]  = ppr last <> dot
make_lines_msg [l1,l2] = l1 $$ text "and" <+> l2 <> dot
make_lines_msg (l:ls)  = l <> comma $$ make_lines_msg ls

{- *********************************************************************
*                                                                      *
                 Record bindings
*                                                                      *
**********************************************************************-}

-- | Disambiguate the fields in a record update.
--
-- Most of the disambiguation has been done by the renamer; this function
-- performs a final type-directed disambiguation pass, as explained in
-- Note [Type-directed record disambiguation].
disambiguateRecordBinds :: LHsExpr GhcRn -> TcRhoType
                        -> NE.NonEmpty (HsRecUpdParent GhcRn)
                        -> [LHsRecUpdField GhcRn GhcRn] -> ExpRhoType
                        -> TcM (UniqSet ConLike, [LHsRecUpdField GhcTc GhcRn])
disambiguateRecordBinds record_expr record_rho possible_parents rbnds res_ty
  = do { fam_inst_envs <- tcGetFamInstEnvs
         -- Identify a single parent, using type-directed disambiguation
         -- if necessary. (Note that type-directed disambiguation of
         -- record field updates is is scheduled for removal, as per
         -- Note [Type-directed record disambiguation].)
       ; TcRecUpdParent
           { tcRecUpdLabels = lbls
           , tcRecUpdCons   = cons }
             <- identifyParentLabels fam_inst_envs possible_parents
         -- Pick the right selector with that parent for each field
       ; rbnds' <- zipWithM lookupField (NE.toList lbls) rbnds
       ; return (cons, rbnds') }
  where

    -- Try to identify a single parent, using type-directed disambiguation.
    --
    -- Any non-type-directed disambiguation will have been done already.
    -- See GHC.Rename.Env.lookupRecUpdFields.
    identifyParentLabels :: FamInstEnvs
                         -> NE.NonEmpty (HsRecUpdParent GhcRn)
                         -> TcM (HsRecUpdParent GhcTc)
    identifyParentLabels fam_inst_envs possible_parents
      = case possible_parents of

        -- Exactly one possible parent for the record update!
        p NE.:| [] -> lookup_parent_flds p

        -- Multiple possible parents: try harder to disambiguate.
        -- Can we get a parent TyCon from the pushed-in type?
        --
        -- See (a) in Note [Type-directed record disambiguation] in GHC.Rename.Pat.
        _ NE.:| _ : _
          | Just tc <- tyConOfET fam_inst_envs res_ty
          -> do { reportAmbiguousUpdate possible_parents tc
                ; try_disambiguated_tycon tc possible_parents }

        -- Does the expression being updated have a type signature?
        -- If so, try to extract a parent TyCon from it.
        --
        -- See (b) inNote [Type-directed record disambiguation] in GHC.Rename.Pat.
          | Just {} <- obviousSig (unLoc record_expr)
          , Just tc <- tyConOf fam_inst_envs record_rho
          -> do { reportAmbiguousUpdate possible_parents tc
                ; try_disambiguated_tycon tc possible_parents }

        -- Nothing else we can try...
        p1 NE.:| p2 : ps
          -> do { p1 <- tcLookupRecSelParent p1
                ; p2 <- tcLookupRecSelParent p2
                ; ps <- mapM tcLookupRecSelParent ps
                ; failWithTc $ TcRnBadRecordUpdate (getUpdFieldLbls rbnds)
                             $ MultiplePossibleParents (p1, p2, ps) }

    -- Try to use the 'TyCon' we learned from type-directed disambiguation.
    -- This might not work, if it doesn't match up with any of the parents we had
    -- computed on the basis of the field labels.
    -- (See test cases overloadedrecfields01 and T21946.)
    try_disambiguated_tycon :: TyCon
                            -> NE.NonEmpty (HsRecUpdParent GhcRn)
                            -> TcM (HsRecUpdParent GhcTc)
    try_disambiguated_tycon tc pars
      = do { pars <- mapMaybeM (fmap (guard_parent tc) . lookup_parent_flds) (NE.toList pars)
           ; case pars of
               [par] -> return par
               []    -> do { pars <- mapM tcLookupRecSelParent possible_parents
                           ; failWithTc $ TcRnBadRecordUpdate (getUpdFieldLbls rbnds)
                                        $ InvalidTyConParent tc pars }
               _     -> pprPanic "try_disambiguated_tycon: more than 1 valid parent"
                          (ppr $ map tcRecUpdParent pars) }

    guard_parent :: TyCon -> HsRecUpdParent GhcTc -> Maybe (HsRecUpdParent GhcTc)
    guard_parent disamb_tc cand_parent@(TcRecUpdParent { tcRecUpdParent = cand_tc })
      = do { guard (RecSelData disamb_tc == cand_tc)
           ; return cand_parent }

    lookup_parent_flds :: HsRecUpdParent GhcRn
                       -> TcM (HsRecUpdParent GhcTc)
    lookup_parent_flds par@(RnRecUpdParent { rnRecUpdLabels = lbls, rnRecUpdCons = cons })
      = do { let cons' :: NonDetUniqFM ConLike ConLikeName
                 cons' = NonDetUniqFM $ unsafeCastUFMKey $ getUniqSet cons
           ; cons <- traverse (tcLookupConLike . conLikeName_Name) cons'
           ; tc   <- tcLookupRecSelParent par
           ; return $
               TcRecUpdParent
                 { tcRecUpdParent = tc
                 , tcRecUpdLabels = lbls
                 , tcRecUpdCons   = unsafeUFMToUniqSet $ getNonDet cons } }

    lookupField :: FieldGlobalRdrElt
                -> LHsRecUpdField GhcRn GhcRn
                -> TcM (LHsRecUpdField GhcTc GhcRn)
    lookupField fld_gre (L l upd)
      = do { let L loc af = hfbLHS upd
                 lbl      = fieldOccRdrName af
                 mb_gre   = pickGREs lbl [fld_gre]
                      -- NB: this GRE can be 'Nothing' when in GHCi.
                      -- See test T10439.

             -- Mark the record fields as used, now that we have disambiguated.
             -- There is no risk of duplicate deprecation warnings, as we have
             -- not marked the GREs as used previously.
           ; setSrcSpanA loc $ mapM_ (addUsedGRE AllDeprecationWarnings) mb_gre
           ; sel <- tcLookupId (greName fld_gre)
           ; return $ L l HsFieldBind
               { hfbAnn = hfbAnn upd
               , hfbLHS = L (l2l loc) (FieldOcc lbl  (L (l2l loc) sel))
               , hfbRHS = hfbRHS upd
               , hfbPun = hfbPun upd
               } }

    -- The type-directed disambiguation mechanism is scheduled for removal,
    -- as per Note [Type-directed record disambiguation].
    -- So we emit a warning whenever the user relies on it.
    reportAmbiguousUpdate :: NE.NonEmpty (HsRecUpdParent GhcRn)
                          -> TyCon -> TcM ()
    reportAmbiguousUpdate parents parent_type =
        setSrcSpan loc $ addDiagnostic $ TcRnAmbiguousRecordUpdate rupd parent_type
      where
        rupd = RecordUpd { rupd_expr = record_expr
                         , rupd_flds =
                             RegularRecUpdFields
                              { xRecUpdFields = parents
                              , recUpdFields  = rbnds }
                         , rupd_ext = noExtField }
        loc  = getLocA (head rbnds)

{-
Game plan for record bindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Find the TyCon for the bindings, from the first field label.

2. Instantiate its tyvars and unify (T a1 .. an) with expected_ty.

For each binding field = value

3. Instantiate the field type (from the field label) using the type
   envt from step 2.

4  Type check the value using tcCheckPolyExprNC (in tcRecordField),
   passing the field type as the expected argument type.

This extends OK when the field types are universally quantified.
-}

tcRecordBinds
        :: ConLike
        -> [TcType]     -- Expected type for each field
        -> HsRecordBinds GhcRn
        -> TcM (HsRecordBinds GhcTc)

tcRecordBinds con_like arg_tys (HsRecFields x rbinds dd)
  = do  { mb_binds <- mapM do_bind rbinds
        ; return (HsRecFields x (catMaybes mb_binds) dd) }
  where
    fields = map flSelector $ conLikeFieldLabels con_like
    flds_w_tys = zipEqual "tcRecordBinds" fields arg_tys

    do_bind :: LHsRecField GhcRn (LHsExpr GhcRn)
            -> TcM (Maybe (LHsRecField GhcTc (LHsExpr GhcTc)))
    do_bind (L l fld@(HsFieldBind { hfbLHS = f
                                 , hfbRHS = rhs }))

      = do { mb <- tcRecordField con_like flds_w_tys f rhs
           ; case mb of
               Nothing         -> return Nothing
               Just (f', rhs') -> return (Just (L l (HsFieldBind
                                                     { hfbAnn = hfbAnn fld
                                                     , hfbLHS = f'
                                                     , hfbRHS = rhs'
                                                     , hfbPun = hfbPun fld}))) }

fieldCtxt :: FieldLabelString -> SDoc
fieldCtxt field_name
  = text "In the" <+> quotes (ppr field_name) <+> text "field of a record"

tcRecordField :: ConLike -> Assoc Name Type
              -> LFieldOcc GhcRn -> LHsExpr GhcRn
              -> TcM (Maybe (LFieldOcc GhcTc, LHsExpr GhcTc))
tcRecordField con_like flds_w_tys (L loc (FieldOcc rdr (L l sel_name))) rhs
  | Just field_ty <- assocMaybe flds_w_tys sel_name
      = addErrCtxt (fieldCtxt field_lbl) $
        do { rhs' <- tcCheckPolyExprNC rhs field_ty
           ; hasFixedRuntimeRep_syntactic (FRRRecordCon rdr (unLoc rhs'))
                field_ty
           ; let field_id = mkUserLocal (nameOccName sel_name)
                                        (nameUnique sel_name)
                                        ManyTy field_ty (locA loc)
                -- Yuk: the field_id has the *unique* of the selector Id
                --          (so we can find it easily)
                --      but is a LocalId with the appropriate type of the RHS
                --          (so the expansion knows the type of local binder to make)
           ; return (Just (L loc (FieldOcc rdr (L l field_id)), rhs')) }
      | otherwise
      = do { addErrTc (badFieldConErr (getName con_like) field_lbl)
           ; return Nothing }
  where
        field_lbl = FieldLabelString $ occNameFS $ rdrNameOcc rdr


checkMissingFields ::  ConLike -> HsRecordBinds GhcRn -> [Scaled TcType] -> TcM ()
checkMissingFields con_like rbinds arg_tys
  | null field_labels   -- Not declared as a record;
                        -- But C{} is still valid if no strict fields
  = if any isBanged field_strs then
        -- Illegal if any arg is strict
        addErrTc (TcRnMissingStrictFields con_like [])
    else do
        when (notNull field_strs && null field_labels) $ do
          let msg = TcRnMissingFields con_like []
          (diagnosticTc True msg)

  | otherwise = do              -- A record
    unless (null missing_s_fields) $ do
        fs <- liftZonkM $ zonk_fields missing_s_fields
        -- It is an error to omit a strict field, because
        -- we can't substitute it with (error "Missing field f")
        addErrTc (TcRnMissingStrictFields con_like fs)

    warn <- woptM Opt_WarnMissingFields
    when (warn && notNull missing_ns_fields) $ do
        fs <- liftZonkM $ zonk_fields missing_ns_fields
        -- It is not an error (though we may want) to omit a
        -- lazy field, because we can always use
        -- (error "Missing field f") instead.
        let msg = TcRnMissingFields con_like fs
        diagnosticTc True msg

  where
    -- we zonk the fields to get better types in error messages (#18869)
    zonk_fields fs = forM fs $ \(str,ty) -> do
        ty' <- zonkTcType ty
        return (str,ty')
    missing_s_fields
        = [ (flLabel fl, scaledThing ty) | (fl,str,ty) <- field_info,
                 isBanged str,
                 not (fl `elemField` field_names_used)
          ]
    missing_ns_fields
        = [ (flLabel fl, scaledThing ty) | (fl,str,ty) <- field_info,
                 not (isBanged str),
                 not (fl `elemField` field_names_used)
          ]

    field_names_used = hsRecFields rbinds
    field_labels     = conLikeFieldLabels con_like

    field_info = zip3 field_labels field_strs arg_tys

    field_strs = conLikeImplBangs con_like

    fl `elemField` flds = any (\ fl' -> flSelector fl == fl') flds

{-
************************************************************************
*                                                                      *
\subsection{Static Pointers}
*                                                                      *
************************************************************************
-}

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
    explain :: Name -> NotClosedReason -> TcRnMessage
    explain = TcRnStaticFormNotClosed

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
