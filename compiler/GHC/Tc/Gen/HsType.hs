{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecursiveDo        #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typechecking user-specified @MonoTypes@
module GHC.Tc.Gen.HsType (
        -- Type signatures
        kcClassSigType, tcClassSigType,
        tcHsSigType, tcHsSigWcType,
        tcHsPartialSigType,
        tcStandaloneKindSig,
        funsSigCtxt, addSigCtxt, pprSigCtxt,

        tcHsClsInstType,
        tcHsDefault, tcHsDeriv, tcDerivStrategy,
        tcHsTypeApp,
        UserTypeCtxt(..),
        bindImplicitTKBndrs_Tv, bindImplicitTKBndrs_Skol,
            bindImplicitTKBndrs_Q_Tv, bindImplicitTKBndrs_Q_Skol,
        bindExplicitTKBndrs_Tv, bindExplicitTKBndrs_Skol,
            bindExplicitTKBndrs_Q_Tv, bindExplicitTKBndrs_Q_Skol,

        bindOuterFamEqnTKBndrs_Q_Tv, bindOuterFamEqnTKBndrs,
        tcOuterTKBndrs, scopedSortOuter, outerTyVars, outerTyVarBndrs,
        bindOuterSigTKBndrs_Tv,
        tcExplicitTKBndrs,
        bindNamedWildCardBinders,

        -- Type checking type and class decls, and instances thereof
        bindTyClTyVars, bindTyClTyVarsAndZonk,
        tcFamTyPats,
        etaExpandAlgTyCon, tcbVisibilities,

          -- tyvars
        zonkAndScopedSort,

        -- Kind-checking types
        -- No kind generalisation, no checkValidType
        InitialKindStrategy(..),
        SAKS_or_CUSK(..),
        ContextKind(..),
        kcDeclHeader, checkForDuplicateScopedTyVars,
        tcHsLiftedType,   tcHsOpenType,
        tcHsLiftedTypeNC, tcHsOpenTypeNC,
        tcInferLHsType, tcInferLHsTypeKind, tcInferLHsTypeUnsaturated,
        tcCheckLHsTypeInContext, tcCheckLHsType,
        tcHsContext, tcLHsPredType,

        kindGeneralizeAll,

        -- Sort-checking kinds
        tcLHsKindSig, checkDataKindSig, DataSort(..),
        checkClassKindSig,

        -- Multiplicity
        tcArrow, tcMult,

        -- Pattern type signatures
        tcHsPatSigType, tcHsTyPat,
        HoleMode(..),

        -- Error messages
        funAppCtxt, addTyConFlavCtxt,

        -- Utils
        tyLitFromLit, tyLitFromOverloadedLit,
   ) where

import GHC.Prelude hiding ( head, init, last, tail )

import GHC.Hs
import GHC.Rename.Utils
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Origin
import GHC.Tc.Types.LclEnv
import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType
import GHC.Tc.Validity
import GHC.Tc.Utils.Unify
import GHC.IfaceToCore
import GHC.Tc.Solver
import GHC.Tc.Zonk.Type
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Instantiate ( tcInstInvisibleTyBinders, tcInstInvisibleTyBindersN,
                                  tcInstInvisibleTyBinder, tcSkolemiseInvisibleBndrs,
                                  tcInstTypeBndrs )
import GHC.Tc.Zonk.TcType

import GHC.Core.Type
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr

import GHC.Builtin.Types.Prim
import GHC.Types.Error
import GHC.Types.Name.Env
import GHC.Types.Name.Reader( lookupLocalRdrOcc )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Core.TyCon
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Types.Name
import GHC.Types.Var.Env
import GHC.Builtin.Types
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Utils.Misc
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Builtin.Names hiding ( wildCardName )
import GHC.Driver.DynFlags
import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.FastString
import GHC.Data.List.Infinite ( Infinite (..) )
import qualified GHC.Data.List.Infinite as Inf
import GHC.Data.List.SetOps
import GHC.Data.Maybe
import GHC.Data.Bag( unitBag )

import Data.Function ( on )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.List ( mapAccumL )
import Control.Monad
import Data.Tuple( swap )
import GHC.Types.SourceText

{-
        ----------------------------
                General notes
        ----------------------------

Unlike with expressions, type-checking types both does some checking and
desugars at the same time. This is necessary because we often want to perform
equality checks on the types right away, and it would be incredibly painful
to do this on un-desugared types. Luckily, desugared types are close enough
to HsTypes to make the error messages sane.

During type-checking, we perform as little validity checking as possible.
Generally, after type-checking, you will want to do validity checking, say
with GHC.Tc.Validity.checkValidType.

Validity checking
~~~~~~~~~~~~~~~~~
Some of the validity check could in principle be done by the kind checker,
but not all:

- During desugaring, we normalise by expanding type synonyms.  Only
  after this step can we check things like type-synonym saturation
  e.g.  type T k = k Int
        type S a = a
  Then (T S) is ok, because T is saturated; (T S) expands to (S Int);
  and then S is saturated.  This is a GHC extension.

- Similarly, also a GHC extension, we look through synonyms before complaining
  about the form of a class or instance declaration

- Ambiguity checks involve functional dependencies

Also, in a mutually recursive group of types, we can't look at the TyCon until we've
finished building the loop.  So to keep things simple, we postpone most validity
checking until step (3).

%************************************************************************
%*                                                                      *
              Check types AND do validity checking
*                                                                      *
************************************************************************

Note [Keeping implicitly quantified variables in order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the user implicitly quantifies over variables (say, in a type
signature), we need to come up with some ordering on these variables.
This is done by bumping the TcLevel, bringing the tyvars into scope,
and then type-checking the thing_inside. The constraints are all
wrapped in an implication, which is then solved. Finally, we can
zonk all the binders and then order them with scopedSort.

It's critical to solve before zonking and ordering in order to uncover
any unifications. You might worry that this eager solving could cause
trouble elsewhere. I don't think it will. Because it will solve only
in an increased TcLevel, it can't unify anything that was mentioned
elsewhere. Additionally, we require that the order of implicitly
quantified variables is manifest by the scope of these variables, so
we're not going to learn more information later that will help order
these variables.

Note [Recipe for checking a signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Kind-checking a user-written signature requires several steps:

 0. Bump the TcLevel
 1.   Bind any lexically-scoped type variables.
 2.   Generate constraints.
 3. Solve constraints.
 4. Sort any implicitly-bound variables into dependency order
 5. Promote tyvars and/or kind-generalize.
 6. Zonk.
 7. Check validity.

Very similar steps also apply when kind-checking a type or class
declaration.

The general pattern looks something like this.  (But NB every
specific instance varies in one way or another!)

    do { (tclvl, wanted, (spec_tkvs, ty))
              <- pushLevelAndSolveEqualitiesX "tc_top_lhs_type" $
                 bindImplicitTKBndrs_Skol sig_vars              $
                 <kind-check the type>

       ; spec_tkvs <- zonkAndScopedSort spec_tkvs

       ; reportUnsolvedEqualities skol_info spec_tkvs tclvl wanted

       ; let ty1 = mkSpecForAllTys spec_tkvs ty
       ; kvs <- kindGeneralizeAll ty1

       ; final_ty <- zonkTcTypeToType (mkInfForAllTys kvs ty1)

       ; checkValidType final_ty

This pattern is repeated many times in GHC.Tc.Gen.HsType,
GHC.Tc.Gen.Sig, and GHC.Tc.TyCl, with variations.  In more detail:

* pushLevelAndSolveEqualitiesX (Step 0, step 3) bumps the TcLevel,
  calls the thing inside to generate constraints, solves those
  constraints as much as possible, returning the residual unsolved
  constraints in 'wanted'.

* bindImplicitTKBndrs_Skol (Step 1) binds the user-specified type
  variables E.g.  when kind-checking f :: forall a. F a -> a we must
  bring 'a' into scope before kind-checking (F a -> a)

* zonkAndScopedSort (Step 4) puts those user-specified variables in
  the dependency order.  (For "implicit" variables the order is no
  user-specified.  E.g.  forall (a::k1) (b::k2). blah k1 and k2 are
  implicitly brought into scope.

* reportUnsolvedEqualities (Step 3 continued) reports any unsolved
  equalities, carefully wrapping them in an implication that binds the
  skolems.  We can't do that in pushLevelAndSolveEqualitiesX because
  that function doesn't have access to the skolems.

* kindGeneralize (Step 5). See Note [Kind generalisation]

* The final zonkTcTypeToType must happen after promoting/generalizing,
  because promoting and generalizing fill in metavariables.


Doing Step 3 (constraint solving) eagerly (rather than building an
implication constraint and solving later) is necessary for several
reasons:

* Exactly as for Solver.simplifyInfer: when generalising, we solve all
  the constraints we can so that we don't have to quantify over them
  or, since we don't quantify over constraints in kinds, float them
  and inhibit generalisation.

* Most signatures also bring implicitly quantified variables into
  scope, and solving is necessary to get these in the right order
  (Step 4) see Note [Keeping implicitly quantified variables in
  order]).

Note [Kind generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Step 5 of Note [Recipe for checking a signature], namely
kind-generalisation, is done by
    kindGeneraliseAll
    kindGeneraliseSome
    kindGeneraliseNone

Here, we have to deal with the fact that metatyvars generated in the
type will have a bumped TcLevel, because explicit foralls raise the
TcLevel. To avoid these variables from ever being visible in the
surrounding context, we must obey the following dictum:

  Every metavariable in a type must either be
    (A) generalized, or
    (B) promoted, or        See Note [Promotion in signatures]
    (C) a cause to error    See Note [Naughty quantification candidates]
                            in GHC.Tc.Utils.TcMType

There are three steps (look at kindGeneraliseSome):

1. candidateQTyVarsOfType finds the free variables of the type or kind,
   to generalise

2. filterConstrainedCandidates filters out candidates that appear
   in the unsolved 'wanteds', and promotes the ones that get filtered out
   thereby.

3. quantifyTyVars quantifies the remaining type variables

The kindGeneralize functions do not require pre-zonking; they zonk as they
go.

kindGeneraliseAll specialises for the case where step (2) is vacuous.
kindGeneraliseNone specialises for the case where we do no quantification,
but we must still promote.

If you are actually doing kind-generalization, you need to bump the
level before generating constraints, as we will only generalize
variables with a TcLevel higher than the ambient one.
Hence the "pushLevel" in pushLevelAndSolveEqualities.

Note [Promotion in signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If an unsolved metavariable in a signature is not generalized
(because we're not generalizing the construct -- e.g., pattern
sig -- or because the metavars are constrained -- see kindGeneralizeSome)
we need to promote to maintain (WantedInv) of Note [TcLevel invariants]
in GHC.Tc.Utils.TcType. Note that promotion is identical in effect to generalizing
and the reinstantiating with a fresh metavariable at the current level.
So in some sense, we generalize *all* variables, but then re-instantiate
some of them.

Here is an example of why we must promote:
  foo (x :: forall a. a -> Proxy b) = ...

In the pattern signature, `b` is unbound, and will thus be brought into
scope. We do not know its kind: it will be assigned kappa[2]. Note that
kappa is at TcLevel 2, because it is invented under a forall. (A priori,
the kind kappa might depend on `a`, so kappa rightly has a higher TcLevel
than the surrounding context.) This kappa cannot be solved for while checking
the pattern signature (which is not kind-generalized). When we are checking
the *body* of foo, though, we need to unify the type of x with the argument
type of bar. At this point, the ambient TcLevel is 1, and spotting a
metavariable with level 2 would violate the (WantedInv) invariant of
Note [TcLevel invariants]. So, instead of kind-generalizing,
we promote the metavariable to level 1. This is all done in kindGeneralizeNone.

-}

funsSigCtxt :: [LocatedN Name] -> UserTypeCtxt
-- Returns FunSigCtxt, with no redundant-context-reporting,
-- form a list of located names
funsSigCtxt (L _ name1 : _) = FunSigCtxt name1 NoRRC
funsSigCtxt []              = panic "funSigCtxt"

addSigCtxt :: Outputable hs_ty => UserTypeCtxt -> LocatedA hs_ty -> TcM a -> TcM a
addSigCtxt ctxt hs_ty thing_inside
  = setSrcSpan (getLocA hs_ty) $
    addErrCtxt (pprSigCtxt ctxt hs_ty) $
    thing_inside

pprSigCtxt :: Outputable hs_ty => UserTypeCtxt -> LocatedA hs_ty -> SDoc
-- (pprSigCtxt ctxt <extra> <type>)
-- prints    In the type signature for 'f':
--              f :: <type>
-- The <extra> is either empty or "the ambiguity check for"
pprSigCtxt ctxt hs_ty
  | Just n <- isSigMaybe ctxt
  = hang (text "In the type signature:")
       2 (pprPrefixOcc n <+> dcolon <+> ppr hs_ty)

  | otherwise
  = hang (text "In" <+> pprUserTypeCtxt ctxt <> colon)
       2 (ppr hs_ty)

tcHsSigWcType :: UserTypeCtxt -> LHsSigWcType GhcRn -> TcM Type
-- This one is used when we have a LHsSigWcType, but in
-- a place where wildcards aren't allowed. The renamer has
-- already checked this, so we can simply ignore it.
tcHsSigWcType ctxt sig_ty = tcHsSigType ctxt (dropWildCards sig_ty)

kcClassSigType :: [LocatedN Name] -> LHsSigType GhcRn -> TcM ()
-- This is a special form of tcClassSigType that is used during the
-- kind-checking phase to infer the kind of class variables. Cf. tc_lhs_sig_type.
-- Importantly, this does *not* kind-generalize. Consider
--   class SC f where
--     meth :: forall a (x :: f a). Proxy x -> ()
-- When instantiating Proxy with kappa, we must unify kappa := f a. But we're
-- still working out the kind of f, and thus f a will have a coercion in it.
-- Coercions may block unification (Note [Equalities with incompatible kinds] in
-- GHC.Tc.Solver.Equality, wrinkle (EIK2)) and so we fail to unify. If we try to
-- kind-generalize, we'll end up promoting kappa to the top level (because
-- kind-generalization is normally done right before adding a binding to the context),
-- and then we can't set kappa := f a, because a is local.
kcClassSigType names
    sig_ty@(L _ (HsSig { sig_bndrs = hs_outer_bndrs, sig_body = hs_ty }))
  = addSigCtxt (funsSigCtxt names) sig_ty $
    do { _ <- bindOuterSigTKBndrs_Tv hs_outer_bndrs    $
              tcCheckLHsType hs_ty liftedTypeKind
       ; return () }

tcClassSigType :: [LocatedN Name] -> LHsSigType GhcRn -> TcM Type
-- Does not do validity checking
tcClassSigType names sig_ty
  = addSigCtxt sig_ctxt sig_ty $
    do { skol_info <- mkSkolemInfo skol_info_anon
       ; (implic, ty) <- tc_lhs_sig_type skol_info sig_ty (TheKind liftedTypeKind)
       ; emitImplication implic
       ; return ty }
       -- Do not zonk-to-Type, nor perform a validity check
       -- We are in a knot with the class and associated types
       -- Zonking and validity checking is done by tcClassDecl
       --
       -- No need to fail here if the type has an error:
       --   If we're in the kind-checking phase, the solveEqualities
       --     in kcTyClGroup catches the error
       --   If we're in the type-checking phase, the solveEqualities
       --     in tcClassDecl1 gets it
       -- Failing fast here degrades the error message in, e.g., tcfail135:
       --   class Foo f where
       --     baa :: f a -> f
       -- If we fail fast, we're told that f has kind `k1` when we wanted `*`.
       -- It should be that f has kind `k2 -> *`, but we never get a chance
       -- to run the solver where the kind of f is touchable. This is
       -- painfully delicate.
  where
    sig_ctxt = funsSigCtxt names
    skol_info_anon = SigTypeSkol sig_ctxt

tcHsSigType :: UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
-- Does validity checking
-- See Note [Recipe for checking a signature]
tcHsSigType ctxt sig_ty
  = addSigCtxt ctxt sig_ty $
    do { traceTc "tcHsSigType {" (ppr sig_ty)
       ; skol_info <- mkSkolemInfo skol_info
          -- Generalise here: see Note [Kind generalisation]
       ; (implic, ty) <- tc_lhs_sig_type skol_info sig_ty  (expectedKindInCtxt ctxt)

       -- Float out constraints, failing fast if not possible
       -- See Note [Failure in local type signatures] in GHC.Tc.Solver
       ; traceTc "tcHsSigType 2" (ppr implic)
       ; simplifyAndEmitFlatConstraints (mkImplicWC (unitBag implic))

       ; ty <- liftZonkM $ zonkTcType ty
       ; checkValidType ctxt ty
       ; traceTc "end tcHsSigType }" (ppr ty)
       ; return ty }
  where
    skol_info = SigTypeSkol ctxt

tc_lhs_sig_type :: SkolemInfo -> LHsSigType GhcRn
               -> ContextKind -> TcM (Implication, TcType)
-- Kind-checks/desugars an 'LHsSigType',
--   solve equalities,
--   and then kind-generalizes.
-- This will never emit constraints, as it uses solveEqualities internally.
-- No validity checking or zonking
-- Returns also an implication for the unsolved constraints
tc_lhs_sig_type skol_info full_hs_ty@(L loc (HsSig { sig_bndrs = hs_outer_bndrs
                                                   , sig_body = hs_ty })) ctxt_kind
  = setSrcSpanA loc $
    do { (tc_lvl, wanted, (exp_kind, (outer_bndrs, ty)))
              <- pushLevelAndSolveEqualitiesX "tc_lhs_sig_type" $
                 -- See Note [Failure in local type signatures]
                 do { exp_kind <- newExpectedKind ctxt_kind
                          -- See Note [Escaping kind in type signatures]
                    ; stuff <- tcOuterTKBndrs skol_info hs_outer_bndrs $
                               tcCheckLHsType hs_ty exp_kind
                    ; return (exp_kind, stuff) }

       -- Default any unconstrained variables free in the kind
       -- See Note [Escaping kind in type signatures]
       ; exp_kind_dvs <- candidateQTyVarsOfType exp_kind
       ; doNotQuantifyTyVars exp_kind_dvs (err_ctx exp_kind)

       ; traceTc "tc_lhs_sig_type" (ppr hs_outer_bndrs $$ ppr outer_bndrs)
       ; outer_bndrs <- scopedSortOuter outer_bndrs

       ; let outer_tv_bndrs :: [InvisTVBinder] = outerTyVarBndrs outer_bndrs
             ty1 = mkInvisForAllTys outer_tv_bndrs ty
       ; kvs <- kindGeneralizeSome skol_info wanted ty1

       -- Build an implication for any as-yet-unsolved kind equalities
       -- See Note [Skolem escape in type signatures]
       ; implic <- buildTvImplication (getSkolemInfo skol_info) kvs tc_lvl wanted

       ; return (implic, mkInfForAllTys kvs ty1) }
  where
    err_ctx exp_kind tidy_env
      = do { (tidy_env2, exp_kind) <- zonkTidyTcType tidy_env exp_kind
           ; return (tidy_env2, UninfTyCtx_Sig exp_kind full_hs_ty) }



{- Note [Escaping kind in type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider kind-checking the signature for `foo` (#19495, #24686):
  type family T (r :: RuntimeRep) :: TYPE r

  foo :: forall (r :: RuntimeRep). T r
  foo = ...

We kind-check the type with expected kind `TYPE delta` (see newExpectedKind),
where `delta :: RuntimeRep` is as-yet unknown. (We can't use `TYPE LiftedRep`
because we allow signatures like `foo :: Int#`.)

Suppose we are at level L currently.  We do this
  * pushLevelAndSolveEqualitiesX: moves to level L+1
  * newExpectedKind: allocates delta{L+1}. Note carefully that
    this call is /outside/ the tcOuterTKBndrs call.
  * tcOuterTKBndrs: pushes the level again to L+2, binds skolem r{L+2}
  * kind-check the body (T r) :: TYPE delta{L+1}

Then
* We can't unify delta{L+1} with r{L+2}.
  And rightly so: skolem would escape.

* If delta{L+1} is unified with some-kind{L}, that is fine.
  This can happen
      \(x::a) -> let y :: a; y = x in ...(x :: Int#)...
  Here (x :: a :: TYPE gamma) is in the environment when we check
  the signature y::a.  We unify delta := gamma, and all is well.

* If delta{L+1} is unconstrained, we /must not/ quantify over it!
  E.g. Consider f :: Any   where Any :: forall k. k
  We kind-check this with expected kind TYPE kappa. We get
      Any @(TYPE kappa) :: TYPE kappa
  We don't want to generalise to     forall k. Any @k
  because that is ill-kinded: the kind of the body of the forall,
  (Any @k :: k) mentions the bound variable k.

  Instead we want to default it to LiftedRep.

  An alternative would be to promote it, similar to the monomorphism
  restriction, but the MR is pretty confusing.  Defaulting seems better

How does that defaulting happen?  Well, since we /currently/ default
RuntimeRep variables during generalisation, it'll happen in
kindGeneralize. But in principle we might allow generalisation over
RuntimeRep variables in future.  Moreover, what if we had
   kappa{L+1} := F alpha{L+1}
where F :: Type -> RuntimeRep.   Now it's alpha that is free in the kind
and it /won't/ be defaulted.

So we use doNotQuantifyTyVars to either default the free vars of
exp_kind (if possible), or error (if not).

Note [Skolem escape in type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcHsSigType is tricky.  Consider (T11142)
  foo :: forall b. (forall k (a :: k). SameKind a b) -> ()
This is ill-kinded because of a nested skolem-escape.

That will show up as an un-solvable constraint in the implication
returned by buildTvImplication in tc_lhs_sig_type.  See Note [Skolem
escape prevention] in GHC.Tc.Utils.TcType for why it is unsolvable
(the unification variable for b's kind is untouchable).

Then, in GHC.Tc.Solver.simplifyAndEmitFlatConstraints (called from tcHsSigType)
we'll try to float out the constraint, be unable to do so, and fail.
See GHC.Tc.Solver Note [Failure in local type signatures] for more
detail on this.

The separation between tcHsSigType and tc_lhs_sig_type is because
tcClassSigType wants to use the latter, but *not* fail fast, because
there are skolems from the class decl which are in scope; but it's fine
not to because tcClassDecl1 has a solveEqualities wrapped around all
the tcClassSigType calls.

That's why tcHsSigType does simplifyAndEmitFlatConstraints (which
fails fast) but tcClassSigType just does emitImplication (which does
not).  Ugh.

c.f. see also Note [Skolem escape and forall-types]. The difference
is that we don't need to simplify at a forall type, only at the
top level of a signature.
-}

-- Does validity checking and zonking.
tcStandaloneKindSig :: LStandaloneKindSig GhcRn -> TcM (Name, Kind)
tcStandaloneKindSig (L _ (StandaloneKindSig _ (L _ name) ksig))
  = addSigCtxt ctxt ksig $
    do { kind <- tc_top_lhs_type KindLevel ctxt ksig
       ; checkValidType ctxt kind
       ; return (name, kind) }
  where
   ctxt = StandaloneKindSigCtxt name

tcTopLHsType :: UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
tcTopLHsType ctxt lsig_ty
  = checkNoErrs $  -- Fail eagerly to avoid follow-on errors.  We are at
                   -- top level so these constraints will never be solved later.
    tc_top_lhs_type TypeLevel ctxt lsig_ty

tc_top_lhs_type :: TypeOrKind -> UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
-- tc_top_lhs_type is used for kind-checking top-level LHsSigTypes where
--   we want to fully solve /all/ equalities, and report errors
-- Does zonking, but not validity checking because it's used
--   for things (like deriving and instances) that aren't
--   ordinary types
-- Used for both types and kinds
tc_top_lhs_type tyki ctxt (L loc sig_ty@(HsSig { sig_bndrs = hs_outer_bndrs
                                               , sig_body = body }))
  = setSrcSpanA loc $
    do { traceTc "tc_top_lhs_type {" (ppr sig_ty)
       ; skol_info <- mkSkolemInfo skol_info_anon
       ; (tclvl, wanted, (outer_bndrs, ty))
              <- pushLevelAndSolveEqualitiesX "tc_top_lhs_type"    $
                 do { kind <- newExpectedKind (expectedKindInCtxt ctxt)
                    ; tcOuterTKBndrs skol_info hs_outer_bndrs $
                      tc_check_lhs_type (mkMode tyki) body kind }

       ; outer_bndrs <- scopedSortOuter outer_bndrs
       ; let outer_tv_bndrs = outerTyVarBndrs outer_bndrs
             ty1 = mkInvisForAllTys outer_tv_bndrs ty

       ; kvs <- kindGeneralizeAll skol_info ty1  -- "All" because it's a top-level type
       ; reportUnsolvedEqualities skol_info kvs tclvl wanted

       ; final_ty <- initZonkEnv DefaultFlexi
                   $ zonkTcTypeToTypeX (mkInfForAllTys kvs ty1)
       ; traceTc "tc_top_lhs_type }" (vcat [ppr sig_ty, ppr final_ty])
       ; return final_ty }
  where
    skol_info_anon = SigTypeSkol ctxt

tcClassConstraint :: Type -> TcM (Either (Maybe TyCon) ([TyVar], Class, [Type], [Kind]))
-- Like tcHsSigType, but for a simple class constraint of form ( C ty1 ty2 )
-- Returns the C, [ty1, ty2], and the kinds of C's remaining arguments
-- E.g.    class C (a::*) (b::k->k)
--         tcClassConstraint ( C Int ) returns Right ([k], C, [k, Int], [k->k])
-- Return values are fully zonked
tcClassConstraint ty
  = do { let (tvs, pred)    = splitForAllTyCoVars ty
             (kind_args, _) = splitFunTys (typeKind pred)
      -- Checking that `pred` a is type class application
       ; case splitTyConApp_maybe pred of
          Just (tyCon, tyConArgs) ->
            case tyConClass_maybe tyCon of
              Just clas ->
                return (Right (tvs, clas, tyConArgs, map scaledThing kind_args))
              Nothing -> return (Left (Just tyCon))
          Nothing -> return (Left Nothing) }

tcHsDefault :: LHsSigType GhcRn -> TcM ([TyVar], Class, [Type], [Kind])
-- Like tcHsSigType, but for the default ( C ty1 ty2 ) (ty1', ty2', ...) declaration
-- See Note [Named default declarations] in GHC.Tc.Gen.Default
tcHsDefault hs_ty
  = tcTopLHsType DefaultDeclCtxt hs_ty
    >>= tcClassConstraint
    >>= either (const $ failWithTc $ TcRnIllegalDefaultClass hs_ty) return

-----------------
tcHsDeriv :: LHsSigType GhcRn -> TcM ([TyVar], Class, [Type], [Kind])
-- Like tcHsSigType, but for the ...deriving( C ty1 ty2 ) clause
-- Returns the C, [ty1, ty2], and the kinds of C's remaining arguments
-- E.g.    class C (a::*) (b::k->k)
--         data T a b = ... deriving( C Int )
--    returns ([k], C, [k, Int], [k->k])
-- Return values are fully zonked
tcHsDeriv hs_ty
  = do { ty <- tcTopLHsType DerivClauseCtxt hs_ty
       ; constrained <- tcClassConstraint ty
       ; case constrained of
           Left Nothing -> failWithTc (TcRnIllegalDerivingItem hs_ty)
           Left (Just tyCon) ->
             failWithTc $ TcRnIllegalInstance
                        $ IllegalClassInstance (TypeThing ty)
                        $ IllegalInstanceHead
                        $ InstHeadNonClass
                        $ Just tyCon
           Right result -> return result }

-- | Typecheck a deriving strategy. For most deriving strategies, this is a
-- no-op, but for the @via@ strategy, this requires typechecking the @via@ type.
tcDerivStrategy :: Maybe (LDerivStrategy GhcRn)
                   -- ^ The deriving strategy
                -> TcM (Maybe (LDerivStrategy GhcTc), [TcTyVar])
                   -- ^ The typechecked deriving strategy and the tyvars that it binds
                   -- (if using 'ViaStrategy').
tcDerivStrategy mb_lds
  = case mb_lds of
      Nothing -> boring_case Nothing
      Just (L loc ds) ->
        setSrcSpanA loc $ do
          (ds', tvs) <- tc_deriv_strategy ds
          pure (Just (L loc ds'), tvs)
  where
    tc_deriv_strategy :: DerivStrategy GhcRn
                      -> TcM (DerivStrategy GhcTc, [TyVar])
    tc_deriv_strategy (StockStrategy    _) = boring_case (StockStrategy    noExtField)
    tc_deriv_strategy (AnyclassStrategy _) = boring_case (AnyclassStrategy noExtField)
    tc_deriv_strategy (NewtypeStrategy  _) = boring_case (NewtypeStrategy  noExtField)
    tc_deriv_strategy (ViaStrategy hs_sig)
      = do { ty <- tcTopLHsType DerivClauseCtxt hs_sig
             -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
             --           in GHC.Tc.Utils.TcType
           ; rec { (via_tvs, via_pred) <- tcSkolemiseInvisibleBndrs (DerivSkol via_pred) ty }
           ; pure (ViaStrategy via_pred, via_tvs) }

    boring_case :: ds -> TcM (ds, [a])
    boring_case ds = pure (ds, [])

tcHsClsInstType :: UserTypeCtxt    -- InstDeclCtxt or SpecInstCtxt
                -> LHsSigType GhcRn
                -> TcM Type
-- Like tcHsSigType, but for a class instance declaration
tcHsClsInstType user_ctxt hs_inst_ty
  = setSrcSpan (getLocA hs_inst_ty) $
    do { inst_ty <- tcTopLHsType user_ctxt hs_inst_ty
       ; checkValidInstance user_ctxt hs_inst_ty inst_ty
       ; return inst_ty }

----------------------------------------------
-- | Type-check a visible type application
tcHsTypeApp :: LHsWcType GhcRn -> Kind -> TcM Type
-- See Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
tcHsTypeApp wc_ty kind
  | HsWC { hswc_ext = sig_wcs, hswc_body = hs_ty } <- wc_ty
  = do { mode <- mkHoleMode TypeLevel HM_VTA
                 -- HM_VTA: See Note [Wildcards in visible type application]
       ; ty <- addTypeCtxt hs_ty                  $
               solveEqualities "tcHsTypeApp" $
               -- We are looking at a user-written type, very like a
               -- signature so we want to solve its equalities right now
               bindNamedWildCardBinders sig_wcs $ \ _ ->
               tc_check_lhs_type mode hs_ty kind

       -- We do not kind-generalize type applications: we just
       -- instantiate with exactly what the user says.
       -- See Note [No generalization in type application]
       -- We still must call kindGeneralizeNone, though, according
       -- to Note [Recipe for checking a signature]
       ; kindGeneralizeNone ty
       ; ty <- liftZonkM $ zonkTcType ty
       ; checkValidType TypeAppCtxt ty
       ; return ty }

{- Note [Wildcards in visible type application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A HsWildCardBndrs's hswc_ext now only includes /named/ wildcards, so
any unnamed wildcards stay unchanged in hswc_body.  When called in
tcHsTypeApp, tcCheckLHsTypeInContext will call emitAnonTypeHole
on these anonymous wildcards. However, this would trigger
error/warning when an anonymous wildcard is passed in as a visible type
argument, which we do not want because users should be able to write
@_ to skip a instantiating a type variable variable without fuss. The
solution is to switch the PartialTypeSignatures flags here to let the
typechecker know that it's checking a '@_' and do not emit hole
constraints on it.  See related Note [Wildcards in visible kind
application] and Note [The wildcard story for types] in GHC.Hs.Type

Ugh!

Note [No generalization in type application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not kind-generalize type applications. Imagine

  id @(Proxy Nothing)

If we kind-generalized, we would get

  id @(forall {k}. Proxy @(Maybe k) (Nothing @k))

which is very sneakily impredicative instantiation.

There is also the possibility of mentioning a wildcard
(`id @(Proxy _)`), which definitely should not be kind-generalized.

-}

tcFamTyPats :: TyCon
            -> HsFamEqnPats GhcRn                -- Patterns
            -> TcM (TcType, TcKind)          -- (lhs_type, lhs_kind)
-- Check the LHS of a type/data family instance
-- e.g.   type instance F ty1 .. tyn = ...
-- Used for both type and data families
tcFamTyPats fam_tc hs_pats
  = do { traceTc "tcFamTyPats {" $
         vcat [ ppr fam_tc, text "arity:" <+> ppr fam_arity ]

       ; mode <- mkHoleMode TypeLevel HM_FamPat
                 -- HM_FamPat: See Note [Wildcards in family instances] in
                 -- GHC.Rename.Module
       ; let fun_ty = mkTyConApp fam_tc []
       ; (fam_app, res_kind) <- tcInferTyApps mode lhs_fun fun_ty hs_pats

       -- Hack alert: see Note [tcFamTyPats: zonking the result kind]
       ; res_kind <- liftZonkM $ zonkTcType res_kind

       ; traceTc "End tcFamTyPats }" $
         vcat [ ppr fam_tc, text "res_kind:" <+> ppr res_kind ]

       ; return (fam_app, res_kind) }
  where
    fam_name  = tyConName fam_tc
    fam_arity = tyConArity fam_tc
    lhs_fun   = noLocA (HsTyVar noAnn NotPromoted (noLocA fam_name))

{- Note [tcFamTyPats: zonking the result kind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#19250)
    F :: forall k. k -> k
    type instance F (x :: Constraint) = ()

The tricky point is this:
  is that () an empty type tuple (() :: Type), or
  an empty constraint tuple (() :: Constraint)?
We work this out in a hacky way, by looking at the expected kind:
see Note [Inferring tuple kinds].

In this case, we kind-check the RHS using the kind gotten from the LHS:
see the call to tcCheckLHsTypeInContext in tcTyFamInstEqnGuts in GHC.Tc.Tycl.

But we want the kind from the LHS to be /zonked/, so that when
kind-checking the RHS (tcCheckLHsTypeInContext) we can "see" what we learned
from kind-checking the LHS (tcFamTyPats).  In our example above, the
type of the LHS is just `kappa` (by instantiating the forall k), but
then we learn (from x::Constraint) that kappa ~ Constraint.  We want
that info when kind-checking the RHS.

Easy solution: just zonk that return kind.  Of course this won't help
if there is lots of type-family reduction to do, but it works fine in
common cases.
-}


{-
************************************************************************
*                                                                      *
            The main kind checker: no validity checks here
*                                                                      *
************************************************************************
-}

---------------------------
tcHsOpenType, tcHsLiftedType,
  tcHsOpenTypeNC, tcHsLiftedTypeNC :: LHsType GhcRn -> TcM TcType
-- Used for type signatures
-- Do not do validity checking
tcHsOpenType   hs_ty = addTypeCtxt hs_ty $ tcHsOpenTypeNC hs_ty
tcHsLiftedType hs_ty = addTypeCtxt hs_ty $ tcHsLiftedTypeNC hs_ty

tcHsOpenTypeNC   hs_ty = do { ek <- newOpenTypeKind; tcCheckLHsType hs_ty ek }
tcHsLiftedTypeNC hs_ty = tcCheckLHsType hs_ty liftedTypeKind

-- Like tcCheckLHsType, but takes an expected kind
tcCheckLHsTypeInContext :: LHsType GhcRn -> ContextKind -> TcM TcType
tcCheckLHsTypeInContext hs_ty exp_kind
  = addTypeCtxt hs_ty $
    do { ek <- newExpectedKind exp_kind
       ; tcCheckLHsType hs_ty ek }

tcInferLHsType :: LHsType GhcRn -> TcM TcType
tcInferLHsType hs_ty
  = do { (ty,_kind) <- tcInferLHsTypeKind hs_ty
       ; return ty }

tcInferLHsTypeKind :: LHsType GhcRn -> TcM (TcType, TcKind)
-- Called from outside: set the context
-- Eagerly instantiate any trailing invisible binders
tcInferLHsTypeKind lhs_ty@(L loc hs_ty)
  = addTypeCtxt lhs_ty $
    setSrcSpanA loc    $  -- Cover the tcInstInvisibleTyBinders
    do { (res_ty, res_kind) <- tc_infer_hs_type typeLevelMode hs_ty
       ; tcInstInvisibleTyBinders res_ty res_kind }
  -- See Note [Do not always instantiate eagerly in types]

-- Used to check the argument of GHCi :kind
-- Allow and report wildcards, e.g. :kind T _
-- Do not saturate family applications: see Note [Dealing with :kind]
-- Does not instantiate eagerly; See Note [Do not always instantiate eagerly in types]
tcInferLHsTypeUnsaturated :: LHsType GhcRn -> TcM (TcType, TcKind)
tcInferLHsTypeUnsaturated hs_ty
  = addTypeCtxt hs_ty $
    do { mode <- mkHoleMode TypeLevel HM_Sig  -- Allow and report holes
       ; case splitHsAppTys_maybe (unLoc hs_ty) of
           Just (hs_fun_ty, hs_args)
              -> do { (fun_ty, _ki) <- tcInferTyAppHead mode hs_fun_ty
                    ; tcInferTyApps_nosat mode hs_fun_ty fun_ty hs_args }
                      -- Notice the 'nosat'; do not instantiate trailing
                      -- invisible arguments of a type family.
                      -- See Note [Dealing with :kind]
           Nothing -> tc_infer_lhs_type mode hs_ty }

{- Note [Dealing with :kind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GHCi command
  ghci> type family F :: Either j k
  ghci> :kind F
  F :: forall {j,k}. Either j k

We will only get the 'forall' if we /refrain/ from saturating those
invisible binders. But generally we /do/ saturate those invisible
binders (see tcInferTyApps), and we want to do so for nested application
even in GHCi.  Consider for example (#16287)
  ghci> type family F :: k
  ghci> data T :: (forall k. k) -> Type
  ghci> :kind T F
We want to reject this. It's just at the very top level that we want
to switch off saturation.

So tcInferLHsTypeUnsaturated does a little special case for top level
applications.  Actually the common case is a bare variable, as above.

Note [Do not always instantiate eagerly in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Terms are eagerly instantiated. This means that if you say

  x = id

then `id` gets instantiated to have type alpha -> alpha. The variable
alpha is then unconstrained and regeneralized. So we may well end up with
  x = /\x. id @a
But we cannot do this in types, as we have no type-level lambda.

So, we must be careful only to instantiate at the last possible moment, when
we're sure we're never going to want the lost polymorphism again. This is done
in calls to `tcInstInvisibleTyBinders`; a particular case in point is in
`checkExpectedKind`.

Otherwise, we are careful /not/ to instantiate.  For example:
* at a variable  in `tcTyVar`
* in `tcInferLHsTypeUnsaturated`, which is used by :kind in GHCi.

************************************************************************
*                                                                      *
      Type-checking modes
*                                                                      *
************************************************************************

The kind-checker is parameterised by a TcTyMode, which contains some
information about where we're checking a type.

The renamer issues errors about what it can. All errors issued here must
concern things that the renamer can't handle.

-}

tcArrow :: HsArrow GhcRn -> TcM Mult
tcArrow hc = tc_arrow typeLevelMode hc

tcMult :: [HsModifier GhcRn] -> TcM (Maybe Mult)
tcMult hc = tc_mult typeLevelMode hc

-- | Info about the context in which we're checking a type. Currently,
-- differentiates only between types and kinds, but this will likely
-- grow, at least to include the distinction between patterns and
-- not-patterns.
--
-- To find out where the mode is used, search for 'mode_tyki'
--
-- This data type is purely local, not exported from this module
data TcTyMode
  = TcTyMode { mode_tyki :: TypeOrKind
             , mode_holes :: HoleInfo   }

-- See Note [Levels for wildcards]
-- Nothing <=> no wildcards expected
type HoleInfo = Maybe (TcLevel, HoleMode)

-- HoleMode says how to treat the occurrences
-- of anonymous wildcards; see tcAnonWildCardOcc
data HoleMode = HM_Sig      -- Partial type signatures: f :: _ -> Int
              | HM_FamPat   -- Family instances: F _ Int = Bool
              | HM_VTA      -- Visible type and kind application:
                            --   f @(Maybe _)
                            --   Maybe @(_ -> _)
              | HM_TyAppPat -- Visible type applications in patterns:
                            --   foo (Con @_ @t x) = ...
                            --   case x of Con @_ @t v -> ...

mkMode :: TypeOrKind -> TcTyMode
mkMode tyki = TcTyMode { mode_tyki = tyki, mode_holes = Nothing }

typeLevelMode, kindLevelMode :: TcTyMode
-- These modes expect no wildcards (holes) in the type
kindLevelMode = mkMode KindLevel
typeLevelMode = mkMode TypeLevel

mkHoleMode :: TypeOrKind -> HoleMode -> TcM TcTyMode
mkHoleMode tyki hm
  = do { lvl <- getTcLevel
       ; return (TcTyMode { mode_tyki  = tyki
                          , mode_holes = Just (lvl,hm) }) }

instance Outputable HoleMode where
  ppr HM_Sig      = text "HM_Sig"
  ppr HM_FamPat   = text "HM_FamPat"
  ppr HM_VTA      = text "HM_VTA"
  ppr HM_TyAppPat = text "HM_TyAppPat"

instance Outputable TcTyMode where
  ppr (TcTyMode { mode_tyki = tyki, mode_holes = hm })
    = text "TcTyMode" <+> braces (sep [ ppr tyki <> comma
                                      , ppr hm ])

{-
Note [Bidirectional type checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In types, as in terms, we use bidirectional type infefence.  The main workhorse
function looks like this:

    type ExpKind = ExpType
    data ExpType = Check TcSigmaKind | Infer ...(hole TcRhoType)...

    tcHsType :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType

* When the `ExpKind` argument is (Check ki), we /check/ that the type has
  kind `ki`
* When the `ExpKind` argument is (Infer hole), we /infer/ the kind of the
  type, and fill the hole with that kind
-}

------------------------------------------
-- | Check and desugar a type, returning the core type and its
-- possibly-polymorphic kind. Much like 'tcInferRho' at the expression
-- level.
tc_infer_lhs_type :: TcTyMode -> LHsType GhcRn -> TcM (TcType, TcKind)
tc_infer_lhs_type mode (L span ty)
  = setSrcSpanA span $
    tc_infer_hs_type mode ty

---------------------------
-- | Infer the kind of a type and desugar. This is the "up" type-checker,
-- as described in Note [Bidirectional type checking]
tc_infer_hs_type :: TcTyMode -> HsType GhcRn -> TcM (TcType, TcKind)

tc_infer_hs_type mode rn_ty
  = tcInfer $ \exp_kind -> tcHsType mode rn_ty exp_kind

{-
Note [Typechecking HsCoreTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsCoreTy is an escape hatch that allows embedding Core Types in HsTypes.
As such, there's not much to be done in order to typecheck an HsCoreTy,
since it's already been typechecked to some extent. There is one thing that
we must do, however: we must substitute the type variables from the tcl_env.
To see why, consider GeneralizedNewtypeDeriving, which is one of the main
clients of HsCoreTy (example adapted from #14579):

  newtype T a = MkT a deriving newtype Eq

This will produce an InstInfo GhcPs that looks roughly like this:

  instance forall a_1. Eq a_1 => Eq (T a_1) where
    (==) = coerce @(  a_1 ->   a_1 -> Bool) -- The type within @(...) is an HsCoreTy
                  @(T a_1 -> T a_1 -> Bool) -- So is this
                  (==)

This is then fed into the renamer. Since all of the type variables in this
InstInfo use Exact RdrNames, the resulting InstInfo GhcRn looks basically
identical. Things get more interesting when the InstInfo is fed into the
typechecker, however. GHC will first generate fresh skolems to instantiate
the instance-bound type variables with. In the example above, we might generate
the skolem a_2 and use that to instantiate a_1, which extends the local type
environment (tcl_env) with [a_1 :-> a_2]. This gives us:

  instance forall a_2. Eq a_2 => Eq (T a_2) where ...

To ensure that the body of this instance is well scoped, every occurrence of
the `a` type variable should refer to a_2, the new skolem. However, the
HsCoreTys mention a_1, not a_2. Luckily, the tcl_env provides exactly the
substitution we need ([a_1 :-> a_2]) to fix up the scoping. We apply this
substitution to each HsCoreTy and all is well:

  instance forall a_2. Eq a_2 => Eq (T a_2) where
    (==) = coerce @(  a_2 ->   a_2 -> Bool)
                  @(T a_2 -> T a_2 -> Bool)
                  (==)
-}

------------------------------------------
tcCheckLHsType :: LHsType GhcRn -> TcKind -> TcM TcType
tcCheckLHsType hs_ty exp_kind
  = tc_check_lhs_type typeLevelMode hs_ty exp_kind

tc_check_lhs_type :: TcTyMode -> LHsType GhcRn -> TcKind -> TcM TcType
tc_check_lhs_type mode (L span ty) exp_kind
  = setSrcSpanA span $
    tc_check_hs_type mode ty exp_kind

tc_check_hs_type :: TcTyMode -> HsType GhcRn -> TcKind -> TcM TcType
-- See Note [Bidirectional type checking]
tc_check_hs_type mode ty ek = tcHsType mode ty (Check ek)

tcLHsType :: TcTyMode -> LHsType GhcRn -> ExpKind -> TcM TcType
tcLHsType mode (L span ty) exp_kind
  = setSrcSpanA span $
    tcHsType mode ty exp_kind

tcHsType :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
-- The main workhorse for type kind checking
-- See Note [Bidirectional type checking]

tcHsType mode (HsParTy _ ty)   exp_kind = tcLHsType mode ty exp_kind
tcHsType mode (HsDocTy _ ty _) exp_kind = tcLHsType mode ty exp_kind
tcHsType _ ty@(HsBangTy _ bang _) _
    -- While top-level bangs at this point are eliminated (eg !(Maybe Int)),
    -- other kinds of bangs are not (eg ((!Maybe) Int)). These kinds of
    -- bangs are invalid, so fail. (#7210, #14761)
    = failWith $ TcRnUnexpectedAnnotation ty bang
tcHsType _ ty@(HsRecTy {})      _
      -- Record types (which only show up temporarily in constructor
      -- signatures) should have been removed by now
    = failWithTc $ TcRnIllegalRecordSyntax (Right ty)

-- HsSpliced is an annotation produced by 'GHC.Rename.Splice.rnSpliceType'.
-- Here we get rid of it and add the finalizers to the global environment
-- while capturing the local environment.
--
-- See Note [Delaying modFinalizers in untyped splices].
tcHsType mode (HsSpliceTy (HsUntypedSpliceTop mod_finalizers ty) _)
           exp_kind
  = do addModFinalizersWithLclEnv mod_finalizers
       tcLHsType mode ty exp_kind

tcHsType _ (HsSpliceTy (HsUntypedSpliceNested n) s) _ = pprPanic "tcHsType: invalid nested splice" (pprUntypedSplice True (Just n) s)

---------- Functions and applications
tcHsType mode (HsFunTy _ mult ty1 ty2) exp_kind
  = tc_fun_type mode mult ty1 ty2 exp_kind

tcHsType mode (HsOpTy _ _ ty1 (L _ op) ty2) exp_kind
  | op `hasKey` unrestrictedFunTyConKey
  = tc_fun_type mode (HsStandardArrow noExtField []) ty1 ty2 exp_kind

--------- Foralls
tcHsType mode t@(HsForAllTy { hst_tele = tele, hst_body = ty }) exp_kind
  | HsForAllInvis{} <- tele
  = tc_hs_forall_ty tele ty exp_kind
                 -- For an invisible forall, we allow the body to have
                 -- an arbitrary kind (hence exp_kind above).
                 -- See Note [Body kind of a HsForAllTy]

  | HsForAllVis{} <- tele
  = do { ek <- newOpenTypeKind
       ; r <- tc_hs_forall_ty tele ty (Check ek)
       ; checkExpKind t r ek exp_kind }
                 -- For a visible forall, we require that the body is of kind TYPE r.
                 -- See Note [Body kind of a HsForAllTy]

  where
    tc_hs_forall_ty tele ty ek
      = do { (tv_bndrs, ty') <- tcTKTelescope mode tele $
                                tcLHsType mode ty ek
                 -- Pass on the mode from the type, to any wildcards
                 -- in kind signatures on the forall'd variables
                 -- e.g.      f :: _ -> Int -> forall (a :: _). blah

             -- Do not kind-generalise here!  See Note [Kind generalisation]
           ; return (mkForAllTys tv_bndrs ty') }

tcHsType mode t@(HsQualTy { hst_ctxt = ctxt, hst_body = rn_ty }) exp_kind
  | null (unLoc ctxt)
  = tcLHsType mode rn_ty exp_kind
    -- See Note [Body kind of a HsQualTy], point (BK1)
  | Check kind <- exp_kind     -- Checking mode
  , isConstraintLikeKind kind  -- CONSTRAINT rep
  = do { ctxt' <- tc_hs_context mode ctxt
         -- See Note [Body kind of a HsQualTy], point (BK2)
       ; ty'   <- tc_check_lhs_type mode rn_ty constraintKind
       ; let res_ty = tcMkDFunPhiTy ctxt' ty'
       ; checkExpKind t res_ty constraintKind exp_kind }

  | otherwise
  = do { ctxt' <- tc_hs_context mode ctxt

      ; ek <- newOpenTypeKind  -- The body kind (result of the function) can
                                -- be TYPE r, for any r, hence newOpenTypeKind
      ; ty' <- tc_check_lhs_type mode rn_ty ek
      ; let res_ty = tcMkPhiTy ctxt' ty'
      ; checkExpKind t res_ty liftedTypeKind exp_kind }

--------- Lists, arrays, and tuples
tcHsType mode rn_ty@(HsListTy _ elt_ty) exp_kind
  = do { tau_ty <- tc_check_lhs_type mode elt_ty liftedTypeKind
       ; checkWiredInTyCon listTyCon
       ; checkExpKind rn_ty (mkListTy tau_ty) liftedTypeKind exp_kind }

tcHsType mode rn_ty@(HsTupleTy _ tup_sort tys) exp_kind
  = do k <- expTypeToType exp_kind
       tc_hs_tuple_ty rn_ty mode tup_sort tys k

tcHsType mode rn_ty@(HsSumTy _ hs_tys) exp_kind
  = do { let arity = length hs_tys
       ; arg_kinds <- mapM (\_ -> newOpenTypeKind) hs_tys
       ; tau_tys   <- zipWithM (tc_check_lhs_type mode) hs_tys arg_kinds
       ; let arg_reps = map kindRep arg_kinds
             arg_tys  = arg_reps ++ tau_tys
             sum_ty   = mkTyConApp (sumTyCon arity) arg_tys
             sum_kind = unboxedSumKind arg_reps
       ; checkExpKind rn_ty sum_ty sum_kind exp_kind
       }

--------- Promoted lists and tuples
tcHsType mode rn_ty@(HsExplicitListTy _ _ tys) exp_kind
  -- See Note [Kind-checking explicit lists]

  | null tys
  = do let ty = mkTyConTy promotedNilDataCon
       let kind = mkSpecForAllTys [alphaTyVar] $ mkListTy alphaTy
       checkExpKind rn_ty ty kind exp_kind

  | otherwise
  = do { tks <- mapM (tc_infer_lhs_type mode) tys
       ; (taus', kind) <- unifyKinds tys tks
       ; let ty = (foldr (mk_cons kind) (mk_nil kind) taus')
       ; checkExpKind rn_ty ty (mkListTy kind) exp_kind }
  where
    mk_cons k a b = mkTyConApp (promoteDataCon consDataCon) [k, a, b]
    mk_nil  k     = mkTyConApp (promoteDataCon nilDataCon) [k]

tcHsType mode rn_ty@(HsExplicitTupleTy _ tys) exp_kind
  -- using newMetaKindVar means that we force instantiations of any polykinded
  -- types. At first, I just used tc_infer_lhs_type, but that led to #11255.
  = do { ks   <- replicateM arity newMetaKindVar
       ; taus <- zipWithM (tc_check_lhs_type mode) tys ks
       ; let kind_con   = tupleTyCon           Boxed arity
             ty_con     = promotedTupleDataCon Boxed arity
             tup_k      = mkTyConApp kind_con ks
       ; checkTupSize arity
       ; checkExpKind rn_ty (mkTyConApp ty_con (ks ++ taus)) tup_k exp_kind }
  where
    arity = length tys

--------- Constraint types
tcHsType mode rn_ty@(HsIParamTy _ (L _ n) ty) exp_kind
  = do { massert (isTypeLevel (mode_tyki mode))
       ; ty' <- tc_check_lhs_type mode ty liftedTypeKind
       ; let n' = mkStrLitTy $ hsIPNameFS n
       ; ipClass <- tcLookupClass ipClassName
       ; checkExpKind rn_ty (mkClassPred ipClass [n',ty'])
                           constraintKind exp_kind }

tcHsType _ rn_ty@(HsStarTy _ _) exp_kind
  -- Desugaring 'HsStarTy' to 'Data.Kind.Type' here means that we don't
  -- have to handle it in 'coreView'
  = checkExpKind rn_ty liftedTypeKind liftedTypeKind exp_kind

--------- Literals
tcHsType _ rn_ty@(HsTyLit _ (HsNumTy _ n)) exp_kind
  = do { checkWiredInTyCon naturalTyCon
       ; checkExpKind rn_ty (mkNumLitTy n) naturalTy exp_kind }

tcHsType _ rn_ty@(HsTyLit _ (HsStrTy _ s)) exp_kind
  = do { checkWiredInTyCon typeSymbolKindCon
       ; checkExpKind rn_ty (mkStrLitTy s) typeSymbolKind exp_kind }
tcHsType _ rn_ty@(HsTyLit _ (HsCharTy _ c)) exp_kind
  = do { checkWiredInTyCon charTyCon
       ; checkExpKind rn_ty (mkCharLitTy c) charTy exp_kind }

--------- Wildcards

tcHsType mode ty@(HsWildCardTy _) ek
  = do k <- expTypeToType ek
       tcAnonWildCardOcc NoExtraConstraint mode ty k

--------- Type applications
tcHsType mode rn_ty@(HsTyVar{})     exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsAppTy{})     exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsAppKindTy{}) exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsOpTy{})      exp_kind = tc_app_ty mode rn_ty exp_kind

tcHsType mode rn_ty@(HsKindSig _ ty sig) exp_kind
  = do { let mode' = mode { mode_tyki = KindLevel }
       ; sig' <- tc_lhs_kind_sig mode' KindSigCtxt sig
                 -- We must typecheck the kind signature, and solve all
                 -- its equalities etc; from this point on we may do
                 -- things like instantiate its foralls, so it needs
                 -- to be fully determined (#14904)
       ; traceTc "tcHsType:sig" (ppr ty $$ ppr sig')
       ; ty' <- tcAddKindSigPlaceholders sig $
                tc_check_lhs_type mode ty sig'
       ; checkExpKind rn_ty ty' sig' exp_kind }

-- See Note [Typechecking HsCoreTys]
tcHsType _ rn_ty@(XHsType ty) exp_kind
  = do env <- getLclEnv
       -- Raw uniques since we go from NameEnv to TvSubstEnv.
       let subst_prs :: [(Unique, TcTyVar)]
           subst_prs = [ (getUnique nm, tv)
                       | ATyVar nm tv <- nonDetNameEnvElts (getLclEnvTypeEnv env) ]
           subst = mkTvSubst
                     (mkInScopeSetList $ map snd subst_prs)
                     (listToUFM_Directly $ map (fmap mkTyVarTy) subst_prs)
           ty' = substTy subst ty
       checkExpKind rn_ty ty' (typeKind ty') exp_kind

tc_hs_tuple_ty :: HsType GhcRn
               -> TcTyMode
               -> HsTupleSort
               -> [LHsType GhcRn]
               -> TcKind
               -> TcM TcType
-- See Note [Distinguishing tuple kinds] in GHC.Hs.Type
-- See Note [Inferring tuple kinds]
tc_hs_tuple_ty rn_ty mode HsBoxedOrConstraintTuple hs_tys exp_kind
     -- (NB: not zonking before looking at exp_k, to avoid left-right bias)
  | Just tup_sort <- tupKindSort_maybe exp_kind
  = traceTc "tcHsType tuple" (ppr hs_tys) >>
    tc_tuple rn_ty mode tup_sort hs_tys exp_kind
  | otherwise
  = do { traceTc "tcHsType tuple 2" (ppr hs_tys)
       ; (tys, kinds) <- mapAndUnzipM (tc_infer_lhs_type mode) hs_tys
       ; kinds <- liftZonkM $ mapM zonkTcType kinds
           -- Infer each arg type separately, because errors can be
           -- confusing if we give them a shared kind.  Eg #7410
           -- (Either Int, Int), we do not want to get an error saying
           -- "the second argument of a tuple should have kind *->*"

       ; let (arg_kind, tup_sort)
               = case [ (k,s) | k <- kinds
                              , Just s <- [tupKindSort_maybe k] ] of
                    ((k,s) : _) -> (k,s)
                    [] -> (liftedTypeKind, BoxedTuple)
         -- In the [] case, it's not clear what the kind is, so guess *

       ; tys' <- sequence [ setSrcSpanA loc $
                            checkExpectedKind hs_ty ty kind arg_kind
                          | ((L loc hs_ty),ty,kind) <- zip3 hs_tys tys kinds ]

       ; finish_tuple rn_ty tup_sort tys' (map (const arg_kind) tys') exp_kind }
tc_hs_tuple_ty rn_ty mode HsUnboxedTuple tys exp_kind =
    tc_tuple rn_ty mode UnboxedTuple tys exp_kind

{-
Note [Kind-checking explicit lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type, suppose we have an application (F [t1,t2]),
where [t1,t2] is an explicit list, and
   F :: [ki] -> blah

Then we want to return the type
   F ((:) @ki t2 ((:) @ki t2 ([] @ki)))
where the argument list is instantiated to F's argument kind `ki`.

But what about (G []), where
   G :: (forall k. [k]) -> blah

Here we want to return (G []), with no instantiation at all.  But since we have
no lambda in types, we must be careful not to instantiate that `[]`, because we
can't re-generalise it.  Hence, when kind-checking an explicit list, we need a
special case for `[]`.

Note [Variable Specificity and Forall Visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A HsForAllTy contains an HsForAllTelescope to denote the visibility of the forall
binder. Furthermore, each invisible type variable binder also has a
Specificity. Together, these determine the variable binders (ForAllTyFlag) for each
variable in the generated ForAllTy type.

This table summarises this relation:
----------------------------------------------------------------------------
| User-written type         HsForAllTelescope   Specificity        ForAllTyFlag
|---------------------------------------------------------------------------
| f :: forall a. type       HsForAllInvis       SpecifiedSpec      Specified
| f :: forall {a}. type     HsForAllInvis       InferredSpec       Inferred
| f :: forall a -> type     HsForAllVis         SpecifiedSpec      Required
| f :: forall {a} -> type   HsForAllVis         InferredSpec       /
|   This last form is nonsensical and is thus rejected.
----------------------------------------------------------------------------

For more information regarding the interpretation of the resulting ForAllTyFlag, see
Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility] in "GHC.Core.TyCo.Rep".
-}

------------------------------------------

-- | Extract the optional Multiplicity modifier from a list. Nothing if none of
-- them are Multiplicities, error if more than one is. Non-Multiplicity
-- modifiers generate warnings but are otherwise ignored.
--
-- With -XNoLinearTypes, no modifiers count as Multiplicities. With
-- -XLinearTypes -XNoModifiers, there must be at most one modifier, and we check
-- that it's a Multiplicity rather than inferring (so less need for kind
-- annotations).
tc_mult :: TcTyMode -> [HsModifier GhcRn] -> TcM (Maybe Mult)
tc_mult mode mods = do
  modifiers <- xoptM LangExt.Modifiers
  linearTypes <- xoptM LangExt.LinearTypes
  case (linearTypes, modifiers) of
    (False, _) -> go_infer (const False) -- MODS_TODO should we suggest enabling -XLinearTypes if there are any modifiers? Or maybe only if there are Multiplicity modifiers?
    (True, False) -> go_check
    (True, True) -> go_infer isMultiplicityTy
  where
    go_infer is_mult = do
      mults <- tc_modifiers mode mods is_mult
      return $ case mults of
        [] -> Nothing
        [m] -> Just m
        _ -> error "MODS_TODO too many multiplicities"

    go_check = case mods of
      [] -> pure Nothing
      [HsModifier _ m] -> Just <$> tc_check_lhs_type mode m multiplicityTy
      _ -> error "MODS_TODO too many multiplicities"

tc_arrow :: TcTyMode -> HsArrow GhcRn -> TcM Mult
tc_arrow mode arr = case arr of
  HsStandardArrow _ ms -> fromMaybe manyDataConTy <$> tc_mult mode ms
  HsLinearArrow _ ms -> do
    mMult <- tc_mult mode ms
    case mMult of
      Just _ -> error "MODS_TODO too many multiplicities"
      Nothing -> pure oneDataConTy

tc_modifier :: TcTyMode -> HsModifier GhcRn -> (TcKind -> Bool) -> TcM (Maybe TcType)
tc_modifier mode mod@(HsModifier _ ty) is_expected_kind = do
  (inf_ty, inf_kind) <- tc_infer_lhs_type mode ty
  if is_expected_kind inf_kind
    then return $ Just inf_ty
    else case inf_kind of
      -- MODS_TODO the point of this is to check for a modifier of unknown kind.
      -- Seems hacky, presumably there's a standard way to do it? This only
      -- affects modifiers that get typechecked, but rename-only modifiers
      -- attached to class/instance declarations get an error if a modifier uses
      -- a type var not in scope, so maybe unknown kinds are impossible?
      TyVarTy _ -> failWithTc $ TcRnUnknownModifierKind mod
      _ -> do
        warn_unknown <- woptM Opt_WarnUnknownModifiers
        diagnosticTc warn_unknown $ TcRnUnknownModifier mod
        pure Nothing

tc_modifiers :: TcTyMode -> [HsModifier GhcRn] -> (TcKind -> Bool) -> TcM [TcType]
tc_modifiers mode mods is_expected_kind =
  catMaybes <$> mapM (\m -> tc_modifier mode m is_expected_kind) mods

------------------------------------------
tc_fun_type :: TcTyMode -> HsArrow GhcRn -> LHsType GhcRn -> LHsType GhcRn -> ExpKind
            -> TcM TcType
tc_fun_type mode mult ty1 ty2 exp_kind = case mode_tyki mode of
  TypeLevel ->
    do { traceTc "tc_fun_type" (ppr ty1 $$ ppr ty2)
       ; arg_k <- newOpenTypeKind
       ; res_k <- newOpenTypeKind
       ; ty1'  <- tc_check_lhs_type mode ty1 arg_k
       ; ty2'  <- tc_check_lhs_type mode ty2 res_k
       ; mult' <- tc_arrow mode mult
       ; checkExpKind (HsFunTy noExtField mult ty1 ty2)
                      (tcMkVisFunTy mult' ty1' ty2')
                      liftedTypeKind exp_kind }
  KindLevel ->  -- no representation polymorphism in kinds. yet.
    do { ty1'  <- tc_check_lhs_type mode ty1 liftedTypeKind
       ; ty2'  <- tc_check_lhs_type mode ty2 liftedTypeKind
       ; mult' <- tc_arrow mode mult
       ; checkExpKind (HsFunTy noExtField mult ty1 ty2)
                      (tcMkVisFunTy mult' ty1' ty2')
                      liftedTypeKind exp_kind }

{- Note [Skolem escape and forall-types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Checking telescopes].

Consider
  f :: forall a. (forall kb (b :: kb). Proxy '[a, b]) -> ()

The Proxy '[a,b] forces a and b to have the same kind.  But a's
kind must be bound outside the 'forall a', and hence escapes.
We discover this by building an implication constraint for
each forall.  So the inner implication constraint will look like
    forall kb (b::kb).  kb ~ ka
where ka is a's kind.  We can't unify these two, /even/ if ka is
unification variable, because it would be untouchable inside
this inner implication.

That's what the pushLevelAndCaptureConstraints, plus subsequent
buildTvImplication/emitImplication is all about, when kind-checking
HsForAllTy.

Note that

* We don't need to /simplify/ the constraints here
  because we aren't generalising. We just capture them.

* We can't use emitResidualTvConstraint, because that has a fast-path
  for empty constraints.  We can't take that fast path here, because
  we must do the bad-telescope check even if there are no inner wanted
  constraints. See Note [Checking telescopes] in
  GHC.Tc.Types.Constraint.  Lacking this check led to #16247.
-}

{- *********************************************************************
*                                                                      *
                Tuples
*                                                                      *
********************************************************************* -}

---------------------------
tupKindSort_maybe :: TcKind -> Maybe TupleSort
tupKindSort_maybe k
  | Just (k', _) <- splitCastTy_maybe k = tupKindSort_maybe k'
  | Just k'      <- coreView k          = tupKindSort_maybe k'
  | isConstraintKind k                  = Just ConstraintTuple
  | tcIsLiftedTypeKind k                = Just BoxedTuple
  | otherwise                           = Nothing

tc_tuple :: HsType GhcRn -> TcTyMode -> TupleSort -> [LHsType GhcRn] -> TcKind -> TcM TcType
tc_tuple rn_ty mode tup_sort tys exp_kind
  = do { arg_kinds <- case tup_sort of
           BoxedTuple      -> return (replicate arity liftedTypeKind)
           UnboxedTuple    -> replicateM arity newOpenTypeKind
           ConstraintTuple -> return (replicate arity constraintKind)
       ; tau_tys <- zipWithM (tc_check_lhs_type mode) tys arg_kinds
       ; finish_tuple rn_ty tup_sort tau_tys arg_kinds exp_kind }
  where
    arity   = length tys

finish_tuple :: HsType GhcRn
             -> TupleSort
             -> [TcType]    -- ^ argument types
             -> [TcKind]    -- ^ of these kinds
             -> TcKind      -- ^ expected kind of the whole tuple
             -> TcM TcType
finish_tuple rn_ty tup_sort tau_tys tau_kinds exp_kind = do
  traceTc "finish_tuple" (ppr tup_sort $$ ppr tau_kinds $$ ppr exp_kind)
  case tup_sort of
    ConstraintTuple
      |  [tau_ty] <- tau_tys
         -- Drop any uses of 1-tuple constraints here.
         -- See Note [Ignore unary constraint tuples]
      -> check_expected_kind tau_ty constraintKind
      |  otherwise
      -> do let tycon = cTupleTyCon arity
            checkCTupSize arity
            check_expected_kind (mkTyConApp tycon tau_tys) constraintKind
    BoxedTuple -> do
      let tycon = tupleTyCon Boxed arity
      checkTupSize arity
      checkWiredInTyCon tycon
      check_expected_kind (mkTyConApp tycon tau_tys) liftedTypeKind
    UnboxedTuple -> do
      let tycon    = tupleTyCon Unboxed arity
          tau_reps = map kindRep tau_kinds
          -- See also Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
          arg_tys  = tau_reps ++ tau_tys
          res_kind = unboxedTupleKind tau_reps
      checkTupSize arity
      check_expected_kind (mkTyConApp tycon arg_tys) res_kind
  where
    arity = length tau_tys
    check_expected_kind ty act_kind =
      checkExpectedKind rn_ty ty act_kind exp_kind

{-
Note [Ignore unary constraint tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC provides unary tuples and unboxed tuples (see Note [One-tuples] in
GHC.Builtin.Types) but does *not* provide unary constraint tuples. Why? First,
recall the definition of a unary tuple data type:

  data Solo a = Solo a

Note that `Solo a` is *not* the same thing as `a`, since Solo is boxed and
lazy. Therefore, the presence of `Solo` matters semantically. On the other
hand, suppose we had a unary constraint tuple:

  class a => Solo% a

This compiles down a newtype (i.e., a cast) in Core, so `Solo% a` is
semantically equivalent to `a`. Therefore, a 1-tuple constraint would have
no user-visible impact, nor would it allow you to express anything that
you couldn't otherwise.

We could simply add Solo% for consistency with tuples (Solo) and unboxed
tuples (Solo#), but that would require even more magic to wire in another
magical class, so we opt not to do so. We must be careful, however, since
one can try to sneak in uses of unary constraint tuples through Template
Haskell, such as in this program (from #17511):

  f :: $(pure (ForallT [] [TupleT 1 `AppT` (ConT ''Show `AppT` ConT ''Int)]
                       (ConT ''String)))
  -- f :: Solo% (Show Int) => String
  f = "abc"

This use of `TupleT 1` will produce an HsBoxedOrConstraintTuple of arity 1,
and since it is used in a Constraint position, GHC will attempt to treat
it as thought it were a constraint tuple, which can potentially lead to
trouble if one attempts to look up the name of a constraint tuple of arity
1 (as it won't exist). To avoid this trouble, we simply take any unary
constraint tuples discovered when typechecking and drop them—i.e., treat
"Solo% a" as though the user had written "a". This is always safe to do
since the two constraints should be semantically equivalent.
-}

{- *********************************************************************
*                                                                      *
                Type applications
*                                                                      *
********************************************************************* -}

splitHsAppTys_maybe :: HsType GhcRn -> Maybe (LHsType GhcRn, [LHsTypeArg GhcRn])
splitHsAppTys_maybe hs_ty
  | is_app hs_ty = Just (splitHsAppTys hs_ty)
  | otherwise    = Nothing
  where
    is_app :: HsType GhcRn -> Bool
    is_app (HsAppKindTy {})        = True
    is_app (HsAppTy {})            = True
    is_app (HsOpTy _ _ _ (L _ op) _) = not (op `hasKey` unrestrictedFunTyConKey)
      -- I'm not sure why this funTyConKey test is necessary
      -- Can it even happen?  Perhaps for   t1 `(->)` t2
      -- but then maybe it's ok to treat that like a normal
      -- application rather than using the special rule for HsFunTy
    is_app (HsTyVar {})            = True
    is_app (HsParTy _ (L _ ty))    = is_app ty
    is_app _                       = False

splitHsAppTys :: HsType GhcRn -> (LHsType GhcRn, [LHsTypeArg GhcRn])

splitHsAppTys hs_ty = go (noLocA hs_ty) []
  where
    go :: LHsType GhcRn
       -> [HsArg GhcRn (LHsType GhcRn) (LHsKind GhcRn)]
       -> (LHsType GhcRn,
           [HsArg GhcRn (LHsType GhcRn) (LHsKind GhcRn)]) -- AZ temp
    go (L _  (HsAppTy _ f a))      as = go f (HsValArg noExtField a : as)
    go (L _  (HsAppKindTy _ ty k)) as = go ty (HsTypeArg noExtField k : as)
    go (L sp (HsParTy _ f))        as = go f (HsArgPar (locA sp) : as)
    go (L _  (HsOpTy _ prom l op@(L sp _) r)) as
      = ( L (l2l sp) (HsTyVar noAnn prom op)
        , HsValArg noExtField l : HsValArg noExtField r : as )
    go f as = (f, as)

---------------------------
tcInferTyAppHead :: TcTyMode -> LHsType GhcRn -> TcM (TcType, TcKind)
-- Version of tc_infer_lhs_type specialised for the head of an
-- application. In particular, for a HsTyVar (which includes type
-- constructors, it does not zoom off into tcInferTyApps and family
-- saturation
tcInferTyAppHead _ (L _ (HsTyVar _ _ (L _ tv)))
  = tcTyVar tv
tcInferTyAppHead mode ty
  = tc_infer_lhs_type mode ty

tc_app_ty :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
tc_app_ty mode rn_ty exp_kind
  = do { (fun_ty, _ki) <- tcInferTyAppHead mode hs_fun_ty
       ; (ty, infered_kind) <- tcInferTyApps mode hs_fun_ty fun_ty hs_args
       ; checkExpKind rn_ty ty infered_kind exp_kind }
  where
    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty

---------------------------
-- | Apply a type of a given kind to a list of arguments. This instantiates
-- invisible parameters as necessary. Always consumes all the arguments,
-- using matchExpectedFunKind as necessary.
-- This takes an optional @VarEnv Kind@ which maps kind variables to kinds.-
-- These kinds should be used to instantiate invisible kind variables;
-- they come from an enclosing class for an associated type/data family.
--
-- tcInferTyApps also arranges to saturate any trailing invisible arguments
--   of a type-family application, which is usually the right thing to do
-- tcInferTyApps_nosat does not do this saturation; it is used only
--   by ":kind" in GHCi
tcInferTyApps, tcInferTyApps_nosat
    :: TcTyMode
    -> LHsType GhcRn        -- ^ Function (for printing only)
    -> TcType               -- ^ Function
    -> [LHsTypeArg GhcRn]   -- ^ Args
    -> TcM (TcType, TcKind) -- ^ (f args, result kind)
tcInferTyApps mode hs_ty fun hs_args
  = do { (f_args, res_k) <- tcInferTyApps_nosat mode hs_ty fun hs_args
       ; saturateFamApp f_args res_k }

tcInferTyApps_nosat mode orig_hs_ty fun orig_hs_args
  = do { traceTc "tcInferTyApps {" (ppr orig_hs_ty $$ ppr orig_hs_args)
       ; (f_args, res_k) <- go_init 1 fun orig_hs_args
       ; traceTc "tcInferTyApps }" (ppr f_args <+> dcolon <+> ppr res_k)
       ; return (f_args, res_k) }
  where

    -- go_init just initialises the auxiliary
    -- arguments of the 'go' loop
    go_init n fun all_args
      = go n fun empty_subst fun_ki all_args
      where
        fun_ki = typeKind fun
           -- We do (typeKind fun) here, even though the caller
           -- knows the function kind, to absolutely guarantee
           -- INVARIANT for 'go'
           -- Note that in a typical application (F t1 t2 t3),
           -- the 'fun' is just a TyCon, so typeKind is fast

        empty_subst = mkEmptySubst $ mkInScopeSet $
                      tyCoVarsOfType fun_ki

    go :: Int             -- The # of the next argument
       -> TcType          -- Function applied to some args
       -> Subst        -- Applies to function kind
       -> TcKind          -- Function kind
       -> [LHsTypeArg GhcRn]    -- Un-type-checked args
       -> TcM (TcType, TcKind)  -- Result type and its kind
    -- INVARIANT: in any call (go n fun subst fun_ki args)
    --               typeKind fun  =  subst(fun_ki)
    -- So the 'subst' and 'fun_ki' arguments are simply
    -- there to avoid repeatedly calling typeKind.
    --
    -- Reason for INVARIANT: to support the Purely Kinded Type Invariant
    -- it's important that if fun_ki has a forall, then so does
    -- (typeKind fun), because the next thing we are going to do
    -- is apply 'fun' to an argument type.

    -- Dispatch on all_args first, for performance reasons
    go n fun subst fun_ki all_args = case (all_args, tcSplitPiTy_maybe fun_ki) of

      ---------------- No user-written args left. We're done!
      ([], _) -> return (fun, substTy subst fun_ki)

      ---------------- HsArgPar: We don't care about parens here
      (HsArgPar _ : args, _) -> go n fun subst fun_ki args

      ---------------- HsTypeArg: a kind application (fun @ki)
      (HsTypeArg _ hs_ki_arg : hs_args, Just (ki_binder, inner_ki)) ->
        case ki_binder of

        -- FunTy with PredTy on LHS, or ForAllTy with Inferred
        Named (Bndr kv Inferred)         -> instantiate kv inner_ki

        Named (Bndr _ Specified) ->  -- Visible kind application
          do { traceTc "tcInferTyApps (vis kind app)"
                       (vcat [ ppr ki_binder, ppr hs_ki_arg
                             , ppr (piTyBinderType ki_binder)
                             , ppr subst ])

             ; let exp_kind = substTy subst $ piTyBinderType ki_binder
             ; arg_mode <- mkHoleMode KindLevel HM_VTA
                   -- HM_VKA: see Note [Wildcards in visible kind application]
             ; ki_arg <- addErrCtxt (funAppCtxt orig_hs_ty hs_ki_arg n) $
                         tc_check_lhs_type arg_mode hs_ki_arg exp_kind

             ; traceTc "tcInferTyApps (vis kind app)" (ppr exp_kind)
             ; (subst', fun') <- mkAppTyM subst fun ki_binder ki_arg
             ; go (n+1) fun' subst' inner_ki hs_args }

        -- Attempted visible kind application (fun @ki), but fun_ki is
        --   forall k -> blah   or   k1 -> k2
        -- So we need a normal application.  Error.
        _ -> ty_app_err hs_ki_arg $ substTy subst fun_ki

      -- No binder; try applying the substitution, or fail if that's not possible
      (HsTypeArg _ ki_arg : _, Nothing) -> try_again_after_substing_or $
                                           ty_app_err ki_arg substed_fun_ki

      ---------------- HsValArg: a normal argument (fun ty)
      (HsValArg _ arg : args, Just (ki_binder, inner_ki))
        -- next binder is invisible; need to instantiate it
        | Named (Bndr kv flag) <- ki_binder
        , isInvisibleForAllTyFlag flag   -- ForAllTy with Inferred or Specified
         -> instantiate kv inner_ki

        -- "normal" case
        | otherwise
         -> do { traceTc "tcInferTyApps (vis normal app)"
                          (vcat [ ppr ki_binder
                                , ppr arg
                                , ppr (piTyBinderType ki_binder)
                                , ppr subst ])
                ; let exp_kind = substTy subst $ piTyBinderType ki_binder
                ; arg' <- addErrCtxt (funAppCtxt orig_hs_ty arg n) $
                          tc_check_lhs_type mode arg exp_kind
                ; traceTc "tcInferTyApps (vis normal app) 2" (ppr exp_kind)
                ; (subst', fun') <- mkAppTyM subst fun ki_binder arg'
                ; go (n+1) fun' subst' inner_ki args }

          -- no binder; try applying the substitution, or infer another arrow in fun kind
      (HsValArg _ _ : _, Nothing)
        -> try_again_after_substing_or $
           do { let arrows_needed = n_initial_val_args all_args
              ; co <- matchExpectedFunKind (HsTypeRnThing $ unLoc hs_ty) arrows_needed substed_fun_ki

              ; fun' <- liftZonkM $ zonkTcType (fun `mkCastTy` co)
                     -- This zonk is essential, to expose the fruits
                     -- of matchExpectedFunKind to the 'go' loop

              ; traceTc "tcInferTyApps (no binder)" $
                   vcat [ ppr fun <+> dcolon <+> ppr fun_ki
                        , ppr arrows_needed
                        , ppr co
                        , ppr fun' <+> dcolon <+> ppr (typeKind fun')]
              ; go_init n fun' all_args }
                -- Use go_init to establish go's INVARIANT
      where
        instantiate ki_binder inner_ki
          = do { traceTc "tcInferTyApps (need to instantiate)"
                         (vcat [ ppr ki_binder, ppr subst])
               ; (subst', arg') <- tcInstInvisibleTyBinder subst ki_binder
               ; go n (mkAppTy fun arg') subst' inner_ki all_args }
                 -- Because tcInvisibleTyBinder instantiate ki_binder,
                 -- the kind of arg' will have the same shape as the kind
                 -- of ki_binder.  So we don't need mkAppTyM here.

        try_again_after_substing_or fallthrough
          | not (isEmptyTCvSubst subst)
          = go n fun zapped_subst substed_fun_ki all_args
          | otherwise
          = fallthrough

        zapped_subst   = zapSubst subst
        substed_fun_ki = substTy subst fun_ki
        hs_ty          = appTypeToArg orig_hs_ty (take (n-1) orig_hs_args)

    n_initial_val_args :: [HsArg p tm ty] -> Arity
    -- Count how many leading HsValArgs we have
    n_initial_val_args (HsValArg {} : args) = 1 + n_initial_val_args args
    n_initial_val_args (HsArgPar {} : args) = n_initial_val_args args
    n_initial_val_args _                    = 0

    ty_app_err arg ty
      = failWith $ TcRnInvalidVisibleKindArgument arg ty

mkAppTyM :: Subst
         -> TcType -> PiTyBinder    -- fun, plus its top-level binder
         -> TcType                  -- arg
         -> TcM (Subst, TcType)  -- Extended subst, plus (fun arg)
-- Precondition: the application (fun arg) is well-kinded after zonking
--               That is, the application makes sense
--
-- Precondition: for (mkAppTyM subst fun bndr arg)
--       typeKind fun  =  Pi bndr. body
--  That is, fun always has a ForAllTy or FunTy at the top
--           and 'bndr' is fun's pi-binder
--
-- Postcondition: if fun and arg satisfy (PKTI), the purely-kinded type
--                invariant, then so does the result type (fun arg)
--
-- We do not require that
--    typeKind arg = tyVarKind (binderVar bndr)
-- This must be true after zonking (precondition 1), but it's not
-- required for the (PKTI).
mkAppTyM subst fun ki_binder arg
  | -- See Note [mkAppTyM]: Nasty case 2
    TyConApp tc args <- fun
  , isTypeSynonymTyCon tc
  , args `lengthIs` (tyConArity tc - 1)
  , any isTrickyTvBinder (tyConTyVars tc) -- We could cache this in the synonym
  = do { (arg':args') <- liftZonkM $ zonkTcTypes (arg:args)
       ; let subst' = case ki_binder of
                        Anon {}           -> subst
                        Named (Bndr tv _) -> extendTvSubstAndInScope subst tv arg'
       ; return (subst', mkTyConApp tc (args' ++ [arg'])) }


mkAppTyM subst fun (Anon {}) arg
   = return (subst, mk_app_ty fun arg)

mkAppTyM subst fun (Named (Bndr tv _)) arg
  = do { arg' <- if isTrickyTvBinder tv
                 then -- See Note [mkAppTyM]: Nasty case 1
                      liftZonkM $ zonkTcType arg
                 else return     arg
       ; return ( extendTvSubstAndInScope subst tv arg'
                , mk_app_ty fun arg' ) }

mk_app_ty :: TcType -> TcType -> TcType
-- This function just adds an ASSERT for mkAppTyM's precondition
mk_app_ty fun arg
  = assertPpr (isPiTy fun_kind)
              (ppr fun <+> dcolon <+> ppr fun_kind $$ ppr arg) $
    mkAppTy fun arg
  where
    fun_kind = typeKind fun

isTrickyTvBinder :: TcTyVar -> Bool
-- NB: isTrickyTvBinder is just an optimisation
-- It would be absolutely sound to return True always
isTrickyTvBinder tv = isPiTy (tyVarKind tv)

{- Note [The Purely Kinded Type Invariant (PKTI)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During type inference, we maintain this invariant

 (PKTI) It is legal to call 'typeKind' on any Type ty,
        on any sub-term of ty, /without/ zonking ty

        Moreover, any such returned kind
        will itself satisfy (PKTI)

By "legal to call typeKind" we mean "typeKind will not crash".
The way in which typeKind can crash is in applications
    (a t1 t2 .. tn)
if 'a' is a type variable whose kind doesn't have enough arrows
or foralls.  (The crash is in piResultTys.)

The loop in tcInferTyApps has to be very careful to maintain the (PKTI).
For example, suppose
    kappa is a unification variable
    We have already unified kappa := Type
      yielding    co :: Refl (Type -> Type)
    a :: kappa
then consider the type
    (a Int)
If we call typeKind on that, we'll crash, because the (un-zonked)
kind of 'a' is just kappa, not an arrow kind.  So we must zonk first.

So the type inference engine is very careful when building applications.
This happens in tcInferTyApps. Suppose we are kind-checking the type (a Int),
where (a :: kappa).  Then in tcInferApps we'll run out of binders on
a's kind, so we'll call matchExpectedFunKind, and unify
   kappa := kappa1 -> kappa2,  with evidence co :: kappa ~ (kappa1 ~ kappa2)
At this point we must zonk the function type to expose the arrrow, so
that (a Int) will satisfy (PKTI).

The absence of this caused #14174 and #14520.

The calls to mkAppTyM is the other place we are very careful; see Note [mkAppTyM].

Wrinkle around FunTy:
Note that the PKTI does *not* guarantee anything about the shape of FunTys.
Specifically, when we have (FunTy vis mult arg res), it should be the case
that arg :: TYPE rr1 and res :: TYPE rr2, for some rr1 and rr2. However, we
might not have this. Example: if the user writes (a -> b), then we might
invent a :: kappa1 and b :: kappa2. We soon will check whether kappa1 ~ TYPE rho1
(for some rho1), and that will lead to kappa1 := TYPE rho1 (ditto for kappa2).
However, when we build the FunTy, we might not have zonked `a`, and so the
FunTy will be built without being able to purely extract the RuntimeReps.

Because the PKTI does not guarantee that the RuntimeReps are available in a FunTy,
we must be aware of this when splitting: splitTyConApp and splitAppTy will *not*
split a FunTy if the RuntimeReps are not available. See also Note [Decomposing FunTy]
in GHC.Tc.Solver.Equality.

Note [mkAppTyM]
~~~~~~~~~~~~~~~
mkAppTyM is trying to guarantee the Purely Kinded Type Invariant
(PKTI) for its result type (fun arg).  There are two ways it can go wrong:

* Nasty case 1: forall types (polykinds/T14174a)
    T :: forall (p :: *->*). p Int -> p Bool
  Now kind-check (T x), where x::kappa.
  Well, T and x both satisfy the PKTI, but
     T x :: x Int -> x Bool
  and (x Int) does /not/ satisfy the PKTI.

* Nasty case 2: type synonyms
    type S f a = f a
  Even though (S ff aa) would satisfy the (PKTI) if S was a data type
  (i.e. nasty case 1 is dealt with), it might still not satisfy (PKTI)
  if S is a type synonym, because the /expansion/ of (S ff aa) is
  (ff aa), and /that/ does not satisfy (PKTI).  E.g. perhaps
  (ff :: kappa), where 'kappa' has already been unified with (*->*).

  We check for nasty case 2 on the final argument of a type synonym.

Notice that in both cases the trickiness only happens if the
bound variable has a pi-type.  Hence isTrickyTvBinder.
-}


saturateFamApp :: TcType -> TcKind -> TcM (TcType, TcKind)
-- Precondition for (saturateFamApp ty kind):
--     typeKind ty = kind
--
-- If 'ty' is an unsaturated family application with trailing
-- invisible arguments, instantiate them.
-- See Note [saturateFamApp]

saturateFamApp ty kind
  | Just (tc, args) <- tcSplitTyConApp_maybe ty
  , tyConMustBeSaturated tc
  , let n_to_inst = tyConArity tc - length args
  = do { (extra_args, ki') <- tcInstInvisibleTyBindersN n_to_inst kind
       ; return (ty `mkAppTys` extra_args, ki') }
  | otherwise
  = return (ty, kind)

{- Note [saturateFamApp]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   type family F :: Either j k
   type instance F @Type = Right Maybe
   type instance F @Type = Right Either```

Then F :: forall {j,k}. Either j k

The two type instances do a visible kind application that instantiates
'j' but not 'k'.  But we want to end up with instances that look like
  type instance F @Type @(*->*) = Right @Type @(*->*) Maybe

so that F has arity 2.  We must instantiate that trailing invisible
binder. In general, Invisible binders precede Specified and Required,
so this is only going to bite for apparently-nullary families.

Note that
  type family F2 :: forall k. k -> *
is quite different and really does have arity 0.

It's not just type instances where we need to saturate those
unsaturated arguments: see #11246.  Hence doing this in tcInferApps.
-}

appTypeToArg :: LHsType GhcRn -> [LHsTypeArg GhcRn] -> LHsType GhcRn
appTypeToArg f []                       = f
appTypeToArg f (HsValArg _ arg   : args) = appTypeToArg (mkHsAppTy f arg) args
appTypeToArg f (HsArgPar _       : args) = appTypeToArg f                 args
appTypeToArg f (HsTypeArg _ arg  : args)
  = appTypeToArg (mkHsAppKindTy noExtField f arg) args


{- *********************************************************************
*                                                                      *
                checkExpectedKind
*                                                                      *
********************************************************************* -}

-- | This instantiates invisible arguments for the type being checked if it must
-- be saturated and is not yet saturated. It then calls and uses the result
-- from checkExpectedKindX to build the final type
checkExpectedKind :: HasDebugCallStack
                  => HsType GhcRn       -- ^ type we're checking (for printing)
                  -> TcType             -- ^ type we're checking
                  -> TcKind             -- ^ the known kind of that type
                  -> TcKind             -- ^ the expected kind
                  -> TcM TcType
-- Just a convenience wrapper to save calls to 'ppr'
checkExpectedKind hs_ty ty act_kind exp_kind
  = do { traceTc "checkExpectedKind" (ppr ty $$ ppr act_kind)

       ; (new_args, act_kind') <- tcInstInvisibleTyBindersN n_to_inst act_kind

       ; let origin = TypeEqOrigin { uo_actual   = act_kind'
                                   , uo_expected = exp_kind
                                   , uo_thing    = Just (HsTypeRnThing hs_ty)
                                   , uo_visible  = True } -- the hs_ty is visible

       ; traceTc "checkExpectedKindX" $
         vcat [ ppr hs_ty
              , text "act_kind':" <+> ppr act_kind'
              , text "exp_kind:" <+> ppr exp_kind ]

       ; let res_ty = ty `mkAppTys` new_args

       ; if act_kind' `tcEqType` exp_kind
         then return res_ty  -- This is very common
         else do { co_k <- unifyTypeAndEmit KindLevel origin act_kind' exp_kind
                 ; traceTc "checkExpectedKind" (vcat [ ppr act_kind
                                                     , ppr exp_kind
                                                     , ppr co_k ])
                ; return (res_ty `mkCastTy` co_k) } }
    where
      -- We need to make sure that both kinds have the same number of implicit
      -- foralls and constraints out front. If the actual kind has more, instantiate
      -- accordingly. Otherwise, just pass the type & kind through: the errors
      -- are caught in unifyType.
      n_exp_invis_bndrs = invisibleBndrCount exp_kind
      n_act_invis_bndrs = invisibleBndrCount act_kind
      n_to_inst         = n_act_invis_bndrs - n_exp_invis_bndrs


-- tyr <- checkExpKind hs_ty ty (act_ki :: Kind) (exp_ki :: ExpKind)
--     requires that `ty` has kind `act_ki`
-- It checks that the actual kind `act_ki` matches the expected kind `exp_ki`
-- and returns `tyr`, a possibly-casted form of `ty`, that has precisely kind `exp_ki`
-- `hs_ty` is purely for error messages
checkExpKind :: HsType GhcRn -> TcType -> TcKind -> ExpKind -> TcM TcType
checkExpKind rn_ty ty ki (Check ki') =
  checkExpectedKind rn_ty ty ki ki'
checkExpKind _rn_ty ty ki (Infer cell) = do
  co <- fillInferResult ki cell
  pure (ty `mkCastTy` co)

---------------------------

tcHsContext :: Maybe (LHsContext GhcRn) -> TcM [PredType]
tcHsContext Nothing    = return []
tcHsContext (Just cxt) = tc_hs_context typeLevelMode cxt

tcLHsPredType :: LHsType GhcRn -> TcM PredType
tcLHsPredType pred = tc_lhs_pred typeLevelMode pred

tc_hs_context :: TcTyMode -> LHsContext GhcRn -> TcM [PredType]
tc_hs_context mode ctxt = mapM (tc_lhs_pred mode) (unLoc ctxt)

tc_lhs_pred :: TcTyMode -> LHsType GhcRn -> TcM PredType
tc_lhs_pred mode pred = tc_check_lhs_type mode pred constraintKind

---------------------------
tcTyVar :: Name -> TcM (TcType, TcKind)
-- See Note [Type checking recursive type and class declarations]
-- in GHC.Tc.TyCl
-- This does not instantiate. See Note [Do not always instantiate eagerly in types]
tcTyVar name         -- Could be a tyvar, a tycon, or a datacon
  = do { traceTc "lk1" (ppr name)
       ; thing <- tcLookup name
       ; case thing of
           ATyVar _ tv -> return (mkTyVarTy tv, tyVarKind tv)

           -- See Note [Recursion through the kinds]
           (tcTyThingTyCon_maybe -> Just tc) -- TyCon or TcTyCon
             -> return (mkTyConTy tc, tyConKind tc)

           AGlobal (AConLike (RealDataCon dc))
             -> do { when (isFamInstTyCon (dataConTyCon dc)) $
                       -- see #15245
                       promotionErr name FamDataConPE
                   ; let (_, _, _, theta, _, _) = dataConFullSig dc
                   ; traceTc "tcTyVar" (ppr dc <+> ppr theta)
                     -- promotionErr: Note [No constraints in kinds] in GHC.Tc.Validity
                   ; unless (null theta) $
                     promotionErr name (ConstrainedDataConPE theta)
                   ; let tc = promoteDataCon dc
                   ; return (mkTyConApp tc [], tyConKind tc) }

           AGlobal AnId{}    -> promotionErr name TermVariablePE
           ATcId{}           -> promotionErr name TermVariablePE
           APromotionErr err -> promotionErr name err

           _  -> wrongThingErr WrongThingType thing name }

{-
Note [Recursion through the kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these examples

Ticket #11554:
  data P (x :: k) = Q
  data A :: Type where
    MkA :: forall (a :: A). P a -> A

Ticket #12174
  data V a
  data T = forall (a :: T). MkT (V a)

The type is recursive (which is fine) but it is recursive /through the
kinds/.  In earlier versions of GHC this caused a loop in the compiler
(to do with knot-tying) but there is nothing fundamentally wrong with
the code (kinds are types, and the recursive declarations are OK). But
it's hard to distinguish "recursion through the kinds" from "recursion
through the types". Consider this (also #11554):

  data PB k (x :: k) = Q
  data B :: Type where
    MkB :: P B a -> B

Here the occurrence of B is not obviously in a kind position.

So now GHC allows all these programs.  #12081 and #15942 are other
examples.

Note [Body kind of a HsForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The body of a forall is usually a type.
Because of representation polymorphism, it can be a TYPE r, for any r.
(In fact, GHC can itself construct a function with an
unboxed tuple inside a for-all via CPR analysis; see
typecheck/should_compile/tc170).

A forall can also be used in an instance head, then the body should
be a constraint.

Right now, we do not have any easy way to enforce that a type is
either a TYPE something or CONSTRAINT something, so we accept any kind.
This is unsound (#22063). We could fix this by implementing a TypeLike
predicate, see #20000.

For a forall with a required argument, we do not allow constraints;
e.g. forall a -> Eq a is invalid. Therefore, we can enforce that the body
is a TYPE something in this case (#24176).

Note [Body kind of a HsQualTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If ctxt is non-empty, the HsQualTy really is a /function/, so the
kind of the result really is '*', and in that case the kind of the
body-type can be lifted or unlifted.

However, consider
    instance Eq a => Eq [a] where ...
or
    f :: (Eq a => Eq [a]) => blah
Here both body-kind and result kind of the HsQualTy is Constraint rather than *.
Rather crudely we tell the difference by looking at exp_kind. It's
very convenient to typecheck instance types like any other HsSigType.

(BK1) How do we figure out the right body kind?

Well, it's a bit of a kludge: I just look at the expected kind, `exp_kind`.
If we are in checking mode (`exp_kind` = `Check k`), and the pushed-in kind
`k` is `CONSTRAINT rep`, then we check that the body type has kind `Constraint` too.

This is a kludge because it wouldn't work if any unification was
involved to compute that result kind -- but it isn't.

Note that in the kludgy "figure out whether we are in a type or constraint"
check, we only check if `k` is a `CONSTRAINT rep`, not `Constraint`.
That turns out to give a better error message in T25243.

(BK2)

Note that, once we are in the constraint case, we check that the body has
kind Constraint; see the call to tc_check_lhs_type. (In contrast, for
types we check that the body has kind TYPE kappa for some fresh unification
variable kappa.)
Reason: we don't yet have support for constraints that are not lifted: it's
not possible to declare a class returning a different type than CONSTRAINT LiftedRep.
Evidence is always lifted, the fat arrow c => t requires c to be
a lifted constraint. In a far future, if we add support for non-lifted
constraints, we could allow c1 => c2 where
c1 :: CONSTRAINT rep1, c2 :: CONSTRAINT rep2
have arbitrary representations rep1 and rep2.

Note [Inferring tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Give a tuple type (a,b,c), which the parser labels as HsBoxedOrConstraintTuple,
we try to figure out whether it's a tuple of kind * or Constraint.
  Step 1: look at the expected kind
  Step 2: infer argument kinds

If after Step 2 it's not clear from the arguments that it's
Constraint, then it must be *.  Once having decided that we re-check
the arguments to give good error messages in
  e.g.  (Maybe, Maybe)

Note that we will still fail to infer the correct kind in this case:

  type T a = ((a,a), D a)
  type family D :: Constraint -> Constraint

While kind checking T, we do not yet know the kind of D, so we will default the
kind of T to * -> *. It works if we annotate `a` with kind `Constraint`.

Note [Desugaring types]
~~~~~~~~~~~~~~~~~~~~~~~
The type desugarer is phase 2 of dealing with HsTypes.  Specifically:

  * It transforms from HsType to Type

  * It zonks any kinds.  The returned type should have no mutable kind
    or type variables (hence returning Type not TcType):
      - any unconstrained kind variables are defaulted to (Any @Type) just
        as in GHC.Tc.Zonk.Type.
      - there are no mutable type variables because we are
        kind-checking a type
    Reason: the returned type may be put in a TyCon or DataCon where
    it will never subsequently be zonked.

You might worry about nested scopes:
        ..a:kappa in scope..
            let f :: forall b. T '[a,b] -> Int
In this case, f's type could have a mutable kind variable kappa in it;
and we might then default it to (Any @Type) when dealing with f's type
signature.  But we don't expect this to happen because we can't get a
lexically scoped type variable with a mutable kind variable in it.  A
delicate point, this.  If it becomes an issue we might need to
distinguish top-level from nested uses.

Moreover
  * it cannot fail,
  * it does no unifications
  * it does no validity checking, except for structural matters, such as
        (a) spurious ! annotations.
        (b) a class used as a type

Note [Kind of a type splice]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these terms, each with TH type splice inside:
     [| e1 :: Maybe $(..blah..) |]
     [| e2 :: $(..blah..) |]
When kind-checking the type signature, we'll kind-check the splice
$(..blah..); we want to give it a kind that can fit in any context,
as if $(..blah..) :: forall k. k.

In the e1 example, the context of the splice fixes kappa to *.  But
in the e2 example, we'll desugar the type, zonking the kind unification
variables as we go.  When we encounter the unconstrained kappa, we
want to default it to 'Type', not to (Any @Type).

-}

addTypeCtxt :: LHsType GhcRn -> TcM a -> TcM a
        -- Wrap a context around only if we want to show that contexts.
        -- Omit invisible ones and ones user's won't grok
addTypeCtxt (L _ (HsWildCardTy _)) thing = thing   -- "In the type '_'" just isn't helpful.
addTypeCtxt (L _ ty) thing
  = addErrCtxt doc thing
  where
    doc = text "In the type" <+> quotes (ppr ty)


{- *********************************************************************
*                                                                      *
                Type-variable binders
*                                                                      *
********************************************************************* -}

bindNamedWildCardBinders :: [Name]
                         -> ([(Name, TcTyVar)] -> TcM a)
                         -> TcM a
-- Bring into scope the /named/ wildcard binders.  Remember that
-- plain wildcards _ are anonymous and dealt with by HsWildCardTy
-- Soe Note [The wildcard story for types] in GHC.Hs.Type
bindNamedWildCardBinders wc_names thing_inside
  = do { wcs <- mapM newNamedWildTyVar wc_names
       ; let wc_prs = wc_names `zip` wcs
       ; tcExtendNameTyVarEnv wc_prs $
         thing_inside wc_prs }

newNamedWildTyVar :: Name -> TcM TcTyVar
-- ^ New unification variable '_' for a wildcard
newNamedWildTyVar _name   -- Currently ignoring the "_x" wildcard name used in the type
  = do { kind <- newMetaKindVar
       ; details <- newMetaDetails TauTv
       ; wc_name <- newMetaTyVarName (fsLit "w")   -- See Note [Wildcard names]
       ; let tyvar = mkTcTyVar wc_name kind details
       ; traceTc "newWildTyVar" (ppr tyvar)
       ; return tyvar }

---------------------------
tcAnonWildCardOcc :: IsExtraConstraint
                  -> TcTyMode -> HsType GhcRn -> Kind -> TcM TcType
tcAnonWildCardOcc is_extra (TcTyMode { mode_holes = Just (hole_lvl, hole_mode) })
                  ty exp_kind
    -- hole_lvl: see Note [Checking partial type signatures]
    --           esp the bullet on nested forall types
  = do { kv_details <- newTauTvDetailsAtLevel hole_lvl
       ; kv_name    <- newMetaTyVarName (fsLit "k")
       ; wc_details <- newTauTvDetailsAtLevel hole_lvl
       ; wc_name    <- newMetaTyVarName wc_nm
       ; let kv      = mkTcTyVar kv_name liftedTypeKind kv_details
             wc_kind = mkTyVarTy kv
             wc_tv   = mkTcTyVar wc_name wc_kind wc_details

       ; traceTc "tcAnonWildCardOcc" (ppr hole_lvl <+> ppr emit_holes)
       ; when emit_holes $
         emitAnonTypeHole is_extra wc_tv
         -- Why the 'when' guard?
         -- See Note [Wildcards in visible kind application]

       -- You might think that this would always just unify
       -- wc_kind with exp_kind, so we could avoid even creating kv
       -- But the level numbers might not allow that unification,
       -- so we have to do it properly (T14140a)
       ; checkExpectedKind ty (mkTyVarTy wc_tv) wc_kind exp_kind }
  where
     -- See Note [Wildcard names]
     wc_nm = case hole_mode of
               HM_Sig      -> fsLit "w"
               HM_FamPat   -> fsLit "_"
               HM_VTA      -> fsLit "w"
               HM_TyAppPat -> fsLit "_"

     emit_holes = case hole_mode of
                     HM_Sig     -> True
                     HM_FamPat  -> False
                     HM_VTA     -> False
                     HM_TyAppPat -> False

tcAnonWildCardOcc is_extra _ _ _
-- mode_holes is Nothing. This means we have an anonymous wildcard
-- in an unexpected place. The renamer rejects these wildcards in 'checkAnonWildcard',
-- but it is possible for a wildcard to be introduced by a Template Haskell splice,
-- as per #15433. To account for this, we throw a generic catch-all error message.
  = failWith $ TcRnIllegalWildcardInType Nothing reason
    where
      reason =
        case is_extra of
          YesExtraConstraint ->
            ExtraConstraintWildcardNotAllowed
              SoleExtraConstraintWildcardNotAllowed
          NoExtraConstraint  ->
            WildcardsNotAllowedAtAll

{- Note [Wildcard names]
~~~~~~~~~~~~~~~~~~~~~~~~
So we hackily use the mode_holes flag to control the name used
for wildcards:

* For proper holes (whether in a visible type application (VTA) or no),
  we rename the '_' to 'w'. This is so that we see variables like 'w0'
  or 'w1' in error messages, a vast improvement upon '_0' and '_1'. For
  example, we prefer
       Found type wildcard ‘_’ standing for ‘w0’
  over
       Found type wildcard ‘_’ standing for ‘_1’

  Even in the VTA case, where we do not emit an error to be printed, we
  want to do the renaming, as the variables may appear in other,
  non-wildcard error messages.

* However, holes in the left-hand sides of type families ("type
  patterns") stand for type variables which we do not care to name --
  much like the use of an underscore in an ordinary term-level
  pattern. When we spot these, we neither wish to generate an error
  message nor to rename the variable.  We don't rename the variable so
  that we can pretty-print a type family LHS as, e.g.,
    F _ Int _ = ...
  and not
     F w1 Int w2 = ...

  See also Note [Wildcards in family instances] in
  GHC.Rename.Module. The choice of HM_FamPat is made in
  tcFamTyPats. There is also some unsavory magic, relying on that
  underscore, in GHC.Core.Coercion.tidyCoAxBndrsForUser.

Note [Wildcards in visible kind application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are cases where users might want to pass in a wildcard as a visible kind
argument, for instance:

data T :: forall k1 k2. k1 → k2 → Type where
  MkT :: T a b
x :: T @_ @Nat False n
x = MkT

So we should allow '@_' without emitting any hole constraints, and
regardless of whether PartialTypeSignatures is enabled or not. But how
would the typechecker know which '_' is being used in VKA and which is
not when it calls emitNamedTypeHole in
tcHsPartialSigType on all HsWildCardBndrs?  The solution is to neither
rename nor include unnamed wildcards in HsWildCardBndrs, but instead
give every anonymous wildcard a fresh wild tyvar in tcAnonWildCardOcc.

And whenever we see a '@', we set mode_holes to HM_VKA, so that
we do not call emitAnonTypeHole in tcAnonWildCardOcc.
See related Note [Wildcards in visible type application] here and
Note [The wildcard story for types] in GHC.Hs.Type
-}

{- *********************************************************************
*                                                                      *
             Kind inference for type declarations
*                                                                      *
********************************************************************* -}

-- See Note [kcCheckDeclHeader vs kcInferDeclHeader]
data InitialKindStrategy
  = InitialKindCheck SAKS_or_CUSK
  | InitialKindInfer

-- Does the declaration have a standalone kind signature (SAKS) or a complete
-- user-supplied kind (CUSK)?
data SAKS_or_CUSK
  = SAKS Kind  -- Standalone kind signature, fully zonked! (zonkTcTypeToType)
  | CUSK       -- Complete user-supplied kind (CUSK)

instance Outputable SAKS_or_CUSK where
  ppr (SAKS k) = text "SAKS" <+> ppr k
  ppr CUSK = text "CUSK"

-- See Note [kcCheckDeclHeader vs kcInferDeclHeader]
kcDeclHeader
  :: InitialKindStrategy
  -> Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn   -- ^ Binders in the header
  -> TcM ContextKind    -- ^ The result kind
  -> TcM TcTyCon        -- ^ A suitably-kinded TcTyCon
kcDeclHeader (InitialKindCheck msig) = kcCheckDeclHeader msig
kcDeclHeader InitialKindInfer = kcInferDeclHeader

{- Note [kcCheckDeclHeader vs kcInferDeclHeader]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kcCheckDeclHeader and kcInferDeclHeader are responsible for getting the initial kind
of a type constructor.

* kcCheckDeclHeader: the TyCon has a standalone kind signature or a CUSK. In that
  case, find the full, final, poly-kinded kind of the TyCon.  It's very like a
  term-level binding where we have a complete type signature for the function.

* kcInferDeclHeader: the TyCon has neither a standalone kind signature nor a
  CUSK. Find a monomorphic kind, with unification variables in it; they will be
  generalised later.  It's very like a term-level binding where we do not have a
  type signature (or, more accurately, where we have a partial type signature),
  so we infer the type and generalise.
-}

------------------------------
kcCheckDeclHeader
  :: SAKS_or_CUSK
  -> Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn   -- ^ Binders in the header
  -> TcM ContextKind    -- ^ The result kind. AnyKind == no result signature
  -> TcM PolyTcTyCon    -- ^ A suitably-kinded generalized TcTyCon
kcCheckDeclHeader (SAKS sig) = kcCheckDeclHeader_sig sig
kcCheckDeclHeader CUSK       = kcCheckDeclHeader_cusk

kcCheckDeclHeader_cusk
  :: Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn   -- ^ Binders in the header
  -> TcM ContextKind    -- ^ The result kind
  -> TcM PolyTcTyCon    -- ^ A suitably-kinded generalized TcTyCon
kcCheckDeclHeader_cusk name flav
              (HsQTvs { hsq_ext = kv_ns
                      , hsq_explicit = hs_tvs }) kc_res_ki
  -- CUSK case
  -- See Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
  = addTyConFlavCtxt name flav $
    do { skol_info <- mkSkolemInfo skol_info_anon
       ; (tclvl, wanted, (scoped_kvs, (tc_bndrs, res_kind)))
           <- pushLevelAndSolveEqualitiesX "kcCheckDeclHeader_cusk" $
              bindImplicitTKBndrs_Q_Skol skol_info kv_ns                      $
              bindExplicitTKBndrs_Q_Skol skol_info ctxt_kind hs_tvs           $
              newExpectedKind =<< kc_res_ki

           -- Now, because we're in a CUSK,
           -- we quantify over the mentioned kind vars
       ; let spec_req_tkvs = scoped_kvs ++ binderVars tc_bndrs
             all_kinds     = res_kind : map tyVarKind spec_req_tkvs

       ; candidates <- candidateQTyVarsOfKinds all_kinds
             -- 'candidates' are all the variables that we are going to
             -- skolemise and then quantify over.  We do not include spec_req_tvs
             -- because they are /already/ skolems

       ; inferred <- quantifyTyVars skol_info DefaultNonStandardTyVars $
                     candidates `delCandidates` spec_req_tkvs
                     -- NB: 'inferred' comes back sorted in dependency order

       ; (scoped_kvs, tc_bndrs, res_kind) <- liftZonkM $
          do { scoped_kvs <- mapM zonkTyCoVarKind scoped_kvs      -- scoped_kvs and tc_bndrs are skolems,
             ; tc_bndrs   <- mapM zonkTyCoVarBndrKind tc_bndrs    -- so zonkTyCoVarBndrKind suffices
             ; res_kind   <- zonkTcType res_kind
             ; return (scoped_kvs, tc_bndrs, res_kind) }

       ; let mentioned_kv_set = candidateKindVars candidates
             specified        = scopedSort scoped_kvs
                                -- NB: maintain the L-R order of scoped_kvs

             all_tcbs =  mkNamedTyConBinders Inferred  inferred
                      ++ mkNamedTyConBinders Specified specified
                      ++ map (mkExplicitTyConBinder mentioned_kv_set) tc_bndrs

       -- Eta expand if necessary; we are building a PolyTyCon
       ; (eta_tcbs, res_kind) <- etaExpandAlgTyCon flav skol_info all_tcbs res_kind

       ; let all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ binderVars tc_bndrs)
             final_tcbs = all_tcbs `chkAppend` eta_tcbs
             tycon = mkTcTyCon name final_tcbs res_kind all_tv_prs
                               True -- Make a PolyTcTyCon, fully generalised
                               flav

       ; reportUnsolvedEqualities skol_info (binderVars final_tcbs)
                                  tclvl wanted

         -- If the ordering from
         -- Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
         -- doesn't work, we catch it here, before an error cascade
       ; checkTyConTelescope tycon

       ; traceTc "kcCheckDeclHeader_cusk " $
         vcat [ text "name" <+> ppr name
              , text "candidates" <+> ppr candidates
              , text "mentioned_kv_set" <+> ppr mentioned_kv_set
              , text "kv_ns" <+> ppr kv_ns
              , text "hs_tvs" <+> ppr hs_tvs
              , text "scoped_kvs" <+> ppr scoped_kvs
              , text "spec_req_tvs" <+> pprTyVars spec_req_tkvs
              , text "all_kinds" <+> ppr all_kinds
              , text "tc_tvs" <+> pprTyVars (binderVars tc_bndrs)
              , text "res_kind" <+> ppr res_kind
              , text "inferred" <+> ppr inferred
              , text "specified" <+> ppr specified
              , text "final_tcbs" <+> ppr final_tcbs
              , text "mkTyConKind final_tc_bndrs res_kind"
                <+> ppr (mkTyConKind final_tcbs res_kind)
              , text "all_tv_prs" <+> ppr all_tv_prs ]

       ; return tycon }
  where
    skol_info_anon = TyConSkol flav name
    ctxt_kind | tcFlavourIsOpen flav = TheKind liftedTypeKind
              | otherwise            = AnyKind

-- | Create a TyConBinder for a user-written type variable binder.
mkExplicitTyConBinder :: TyCoVarSet -- variables that are used dependently
                      -> VarBndr TyVar (HsBndrVis GhcRn)
                      -> TyConBinder
mkExplicitTyConBinder dep_set (Bndr tv flag) =
  case flag of
    HsBndrRequired{}  -> mkRequiredTyConBinder dep_set tv
    HsBndrInvisible{} -> mkNamedTyConBinder Specified tv

-- | Kind-check a 'LHsQTyVars'. Used in 'inferInitialKind' (for tycon kinds and
-- other kinds).
--
-- This function does not do telescope checking.
kcInferDeclHeader
  :: Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn
  -> TcM ContextKind    -- ^ The result kind
  -> TcM MonoTcTyCon    -- ^ A suitably-kinded non-generalized TcTyCon
kcInferDeclHeader name flav
              (HsQTvs { hsq_ext = kv_ns
                      , hsq_explicit = hs_bndrs }) kc_res_ki
  -- No standalone kind signature and no CUSK.
  -- See Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
  = addTyConFlavCtxt name flav $
    do { rejectInvisibleBinders name hs_bndrs
       ; (scoped_kvs, (tc_bndrs, res_kind))
           -- Why bindImplicitTKBndrs_Q_Tv which uses newTyVarTyVar?
           -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl
           <- bindImplicitTKBndrs_Q_Tv kv_ns              $
              bindExplicitTKBndrs_Q_Tv ctxt_kind hs_bndrs $
              newExpectedKind =<< kc_res_ki
              -- Why "_Tv" not "_Skol"? See third wrinkle in
              -- Note [Inferring kinds for type declarations] in GHC.Tc.TyCl,

       ; let   -- NB: Don't add scoped_kvs to tyConTyVars, because they
               -- might unify with kind vars in other types in a mutually
               -- recursive group.
               -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl

             tc_tvs = binderVars tc_bndrs
               -- Discard visibility flags. We made sure all of them are HsBndrRequired
               -- by the call to rejectInvisibleBinders above.

             tc_binders = mkAnonTyConBinders tc_tvs
               -- This has to be mkAnonTyConBinder!
               -- See Note [No polymorphic recursion in type decls] in GHC.Tc.TyCl
               --
               -- Also, note that tc_binders has the tyvars from only the
               -- user-written type variable binders.
               -- See S1 Note [TcTyCon, MonoTcTyCon, and PolyTcTyCon] in GHC.Tc.TyCl

             all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
               -- NB: bindExplicitTKBndrs_Q_Tv does not clone;
               --     ditto Implicit
               -- See Note [Cloning for type variable binders]

             tycon = mkTcTyCon name tc_binders res_kind all_tv_prs
                               False -- Make a MonoTcTyCon
                               flav

       ; traceTc "kcInferDeclHeader: not-cusk" $
         vcat [ ppr name, ppr kv_ns, ppr hs_bndrs
              , ppr scoped_kvs
              , ppr tc_tvs, ppr (mkTyConKind tc_binders res_kind) ]
       ; return tycon }
  where
    ctxt_kind | tcFlavourIsOpen flav = TheKind liftedTypeKind
              | otherwise            = AnyKind

-- rejectInvisibleBinders is called on on the inference code path; there is
-- no standalone kind signature, nor CUSK.
-- See Note [No inference for invisible binders in type decls] in GHC.Tc.TyCl
rejectInvisibleBinders :: Name -> [LHsTyVarBndr (HsBndrVis GhcRn) GhcRn] -> TcM ()
rejectInvisibleBinders name = mapM_ check_bndr_vis
  where
    check_bndr_vis :: LHsTyVarBndr (HsBndrVis GhcRn) GhcRn -> TcM ()
    check_bndr_vis bndr =
      when (isHsBndrInvisible (hsTyVarBndrFlag (unLoc bndr))) $
        addErr (TcRnInvisBndrWithoutSig name bndr)

-- | Kind-check a declaration header against a standalone kind signature.
-- See Note [kcCheckDeclHeader_sig]
kcCheckDeclHeader_sig
  :: Kind               -- ^ Standalone kind signature, fully zonked! (zonkTcTypeToType)
  -> Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn   -- ^ Binders in the header
  -> TcM ContextKind    -- ^ The result kind. AnyKind == no result signature
  -> TcM PolyTcTyCon    -- ^ A suitably-kinded, fully generalised TcTyCon
-- Postcondition to (kcCheckDeclHeader_sig sig_kind n f hs_tvs kc_res_ki):
--   kind(returned PolyTcTyCon) = sig_kind
--
kcCheckDeclHeader_sig sig_kind name flav
          (HsQTvs { hsq_ext      = implicit_nms
                  , hsq_explicit = hs_tv_bndrs }) kc_res_ki
  = addTyConFlavCtxt name flav $
    do { skol_info <- mkSkolemInfo (TyConSkol flav name)
       ; let avoid_occs = map nameOccName (hsLTyVarNames hs_tv_bndrs)
       ; (sig_tcbs :: [TcTyConBinder], sig_res_kind :: Kind)
             <- splitTyConKind skol_info emptyInScopeSet
                               avoid_occs sig_kind

       ; traceTc "kcCheckDeclHeader_sig {" $
           vcat [ text "sig_kind:" <+> ppr sig_kind
                , text "sig_tcbs:" <+> ppr sig_tcbs
                , text "sig_res_kind:" <+> ppr sig_res_kind
                , text "implict_nms:" <+> ppr implicit_nms
                , text "hs_tv_bndrs:" <+> ppr hs_tv_bndrs ]

       ; (tclvl, wanted, (implicit_tvs, (skol_tcbs, skol_scoped_tvs, (extra_tcbs, tycon_res_kind))))
           <- pushLevelAndSolveEqualitiesX "kcCheckDeclHeader_sig" $  -- #16687
              bindImplicitTKBndrs_Q_Tv implicit_nms                $  -- Q means don't clone
              matchUpSigWithDecl name sig_tcbs sig_res_kind hs_tv_bndrs $ \ excess_sig_tcbs sig_res_kind ->
              do { -- Kind-check the result kind annotation, if present:
                   --    data T a b :: res_ki where ...
                   --               ^^^^^^^^^
                   -- We do it here because at this point the environment has been
                   -- extended with both 'implicit_tcv_prs' and 'explicit_tv_prs'.
                   --
                   -- Also see Note [Arity of type families and type synonyms]
                 ; res_kind :: ContextKind <- kc_res_ki

                 ; let sig_res_kind' = mkTyConKind excess_sig_tcbs sig_res_kind

                 ; traceTc "kcCheckDeclHeader_sig 2" $
                    vcat [ text "excess_sig_tcbs" <+> ppr excess_sig_tcbs
                         , text "res_kind" <+> ppr res_kind
                         , text "sig_res_kind'" <+> ppr sig_res_kind'
                         ]

                 -- Unify res_ki (from the type declaration) with
                 -- sig_res_kind', the residual kind from the kind signature.
                 ; checkExpectedResKind sig_res_kind' res_kind

                 -- Add more binders for data/newtype, so the result kind has no arrows
                 -- See Note [Datatype return kinds]
                 ; if null excess_sig_tcbs || not (needsEtaExpansion flav)
                   then return ([],              sig_res_kind')
                   else return (excess_sig_tcbs, sig_res_kind)
          }


        -- Check that there are no unsolved equalities
        ; let all_tcbs = skol_tcbs ++ extra_tcbs
        ; reportUnsolvedEqualities skol_info (binderVars all_tcbs) tclvl wanted

        -- Check that distinct binders map to distinct tyvars (see #20916). For example
        --    type T :: k -> k -> Type
        --    data T (a::p) (b::q) = ...
        -- Here p and q both map to the same kind variable k.  We don't allow this
        -- so we must check that they are distinct.  A similar thing happens
        -- in GHC.Tc.TyCl.swizzleTcTyConBinders during inference.
        --
        -- With visible dependent quantification, one of the binders involved
        -- may be explicit.  Consider #24604
        --    type UF :: forall zk -> zk -> Constraint
        --    class UF kk (xb :: k)
        -- Here `k` and `kk` both denote the same variable; but only `k` is implicit
        -- Hence we need to add skol_scoped_tvs
        ; implicit_tvs <- liftZonkM $ zonkTcTyVarsToTcTyVars implicit_tvs
        ; let implicit_prs = implicit_nms `zip` implicit_tvs
              dup_chk_prs  = implicit_prs ++ mkTyVarNamePairs skol_scoped_tvs
        ; unless (null implicit_nms) $  -- No need if no implicit tyvars
          checkForDuplicateScopedTyVars dup_chk_prs
        ; checkForDisconnectedScopedTyVars name flav all_tcbs implicit_prs

        -- Swizzle the Names so that the TyCon uses the user-declared implicit names
        -- E.g  type T :: k -> Type
        --      data T (a :: j) = ....
        -- We want the TyConBinders of T to be [j, a::j], not [k, a::k]
        -- Why? So that the TyConBinders of the TyCon will lexically scope over the
        -- associated types and methods of a class.
        ; let swizzle_env = mkVarEnv (map swap implicit_prs)
              (subst, swizzled_tcbs) = mapAccumL (swizzleTcb swizzle_env) emptySubst all_tcbs
              swizzled_kind          = substTy subst tycon_res_kind
              all_tv_prs             = mkTyVarNamePairs (binderVars swizzled_tcbs)

        ; traceTc "kcCheckDeclHeader swizzle" $ vcat
          [ text "sig_tcbs ="       <+> ppr sig_tcbs
          , text "implicit_prs ="   <+> ppr implicit_prs
          , text "hs_tv_bndrs ="    <+> ppr hs_tv_bndrs
          , text "all_tcbs ="       <+> pprTyVars (binderVars all_tcbs)
          , text "swizzled_tcbs ="  <+> pprTyVars (binderVars swizzled_tcbs)
          , text "tycon_res_kind =" <+> ppr tycon_res_kind
          , text "swizzled_kind ="  <+> ppr swizzled_kind ]

        -- Build the final, generalized PolyTcTyCon
        -- NB: all_tcbs must bind the tyvars in the range of all_tv_prs
        --     because the tv_prs is used when (say) typechecking the RHS of
        --     a type synonym.
        ; let tc = mkTcTyCon name swizzled_tcbs swizzled_kind all_tv_prs
                             True -- Make a PolyTcTyCon, fully generalised
                             flav

        ; traceTc "kcCheckDeclHeader_sig }" $ vcat
          [ text "tyConName = " <+> ppr (tyConName tc)
          , text "sig_kind =" <+> debugPprType sig_kind
          , text "tyConKind =" <+> debugPprType (tyConKind tc)
          , text "tyConBinders = " <+> ppr (tyConBinders tc)
          , text "tyConResKind" <+> debugPprType (tyConResKind tc)
          ]
        ; return tc }

-- | Check the result kind annotation on a type constructor against
-- the corresponding section of the standalone kind signature.
-- Drops invisible binders that interfere with unification.
checkExpectedResKind :: TcKind       -- ^ the result kind from the separate kind signature
                     -> ContextKind  -- ^ the result kind from the declaration header
                     -> TcM ()
checkExpectedResKind _ AnyKind
  = return ()  -- No signature in the declaration header
checkExpectedResKind sig_kind res_ki
  = do { actual_res_ki <- newExpectedKind res_ki

       ; let -- Drop invisible binders from sig_kind until they match up
             -- with res_ki.  By analogy with checkExpectedKind.
             n_res_invis_bndrs = invisibleBndrCount actual_res_ki
             n_sig_invis_bndrs = invisibleBndrCount sig_kind
             n_to_inst         = n_sig_invis_bndrs - n_res_invis_bndrs

             (_, sig_kind') = splitInvisPiTysN n_to_inst sig_kind

       ; discardResult $ unifyKind Nothing sig_kind' actual_res_ki }

matchUpSigWithDecl
  :: Name                        -- Name of the type constructor for error messages
  -> [TcTyConBinder]             -- TcTyConBinders (with skolem TcTyVars) from the separate kind signature
  -> TcKind                      -- The tail end of the kind signature
  -> [LHsTyVarBndr (HsBndrVis GhcRn) GhcRn]     -- User-written binders in decl
  -> ([TcTyConBinder] -> TcKind -> TcM a)  -- All user-written binders are in scope
                                           --   Argument is excess TyConBinders and tail kind
  -> TcM ( [TcTyConBinder]       -- Skolemised binders, with TcTyVars
         , [TcTyVar]             -- Skolem tyvars brought into lexical scope by this matching-up
         , a )
-- See Note [Matching a kind signature with a declaration]
-- Invariant: Length of returned TyConBinders + length of excess TyConBinders
--            = length of incoming TyConBinders
matchUpSigWithDecl name sig_tcbs sig_res_kind hs_bndrs thing_inside
  = go emptySubst sig_tcbs hs_bndrs
  where
    go subst tcbs []
      = do { let (subst', tcbs') = substTyConBindersX subst tcbs
           ; res <- thing_inside tcbs' (substTy subst' sig_res_kind)
           ; return ([], [], res) }

    go _ [] hs_bndrs
      = failWithTc (TcRnTooManyBinders sig_res_kind hs_bndrs)

    go subst (tcb : tcbs') hs_bndrs@(hs_bndr : hs_bndrs')
      | zippable (binderFlag tcb) (hsTyVarBndrFlag (unLoc hs_bndr))
      = -- Visible TyConBinder, so match up with the hs_bndrs
        do { let Bndr tv vis = tcb
                 tv' = updateTyVarKind (substTy subst) $
                       maybe tv (setTyVarName tv) (hsLTyVarName hs_bndr)
                   -- Give the skolem the Name of the HsTyVarBndr, so that if it
                   -- appears in an error message it has a name and binding site
                   -- that come from the type declaration, not the kind signature
                 subst' = extendTCvSubstWithClone subst tv tv'
           ; tc_hs_bndr (unLoc hs_bndr) (tyVarKind tv')
           ; traceTc "musd1" (ppr tcb $$ ppr hs_bndr $$ ppr tv')
           ; (tcbs', tvs, res) <- tcExtendTyVarEnv [tv'] $
                                  go subst' tcbs' hs_bndrs'
           ; return (Bndr tv' vis : tcbs', tv':tvs, res) }
             -- We do a tcExtendTyVarEnv [tv'], so we return tv' in
             -- the list of lexically-scoped skolem type variables

      | skippable (binderFlag tcb)
      = -- Invisible TyConBinder, so do not consume one of the hs_bndrs
        do { let (subst', tcb') = substTyConBinderX subst tcb
           ; traceTc "musd2" (ppr tcb $$ ppr hs_bndr $$ ppr tcb')
           ; (tcbs', tvs, res) <- go subst' tcbs' hs_bndrs
                   -- NB: pass on hs_bndrs unchanged; we do not consume a
                   --     HsTyVarBndr for an invisible TyConBinder
           ; return (tcb' : tcbs', tvs, res) }
                   -- Return `tvs`; no new lexically-scoped TyVars brought into scope

      | otherwise =
          -- At this point we conclude that:
          --   * the quantifier (tcb) is visible: (ty -> ...), (forall a -> ...)
          --   * the binder (hs_bndr) is invisible: @t, @(t :: k)
          -- Any other combination should have been handled by the zippable/skippable clauses.
          failWithTc (TcRnInvalidInvisTyVarBndr name hs_bndr)

    tc_hs_bndr :: HsTyVarBndr (HsBndrVis GhcRn) GhcRn -> TcKind -> TcM ()
    tc_hs_bndr (HsTvb { tvb_kind = HsBndrNoKind _ }) _ = return ()
    tc_hs_bndr (HsTvb { tvb_kind = HsBndrKind _ kind, tvb_var = bvar })
               expected_kind
      = do { traceTc "musd3:unifying" (ppr kind $$ ppr expected_kind)
           ; tcHsTvbKind bvar kind expected_kind }

    -- See GHC Proposal #425, section "Kind checking",
    -- where zippable and skippable are defined.
    -- In particular: we match up if
    -- (a) HsBndr looks like @k, and TyCon binder is forall k. (NamedTCB Specified)
    -- (b) HsBndr looks like a,  and TyCon binder is forall k -> (NamedTCB Required)
    --                                            or k -> (AnonTCB)
    zippable :: TyConBndrVis -> HsBndrVis GhcRn -> Bool
    zippable vis (HsBndrInvisible _) = isInvisSpecTcbVis vis  -- (a)
    zippable vis (HsBndrRequired _)  = isVisibleTcbVis vis    -- (b)

    -- See GHC Proposal #425, section "Kind checking",
    -- where zippable and skippable are defined.
    skippable :: TyConBndrVis -> Bool
    skippable vis = not (isVisibleTcbVis vis)

-- Check the kind of a type variable binder
tcHsTvbKind :: HsBndrVar GhcRn -> LHsKind GhcRn -> TcKind -> TcM ()
tcHsTvbKind bvar kind expected_kind =
  do { sig_kind <- tcLHsKindSig ctxt kind
     ; traceTc "tcHsTvbKind:unifying" (ppr sig_kind $$ ppr expected_kind)
     ; discardResult $ -- See Note [discardResult in tcHsTvbKind]
       unifyKind mb_thing sig_kind expected_kind }
  where
    (ctxt, mb_thing) = case bvar of
      HsBndrVar _ (L _ hs_nm) -> (TyVarBndrKindCtxt hs_nm, Just (NameThing hs_nm))
      HsBndrWildCard _ -> (KindSigCtxt, Nothing)

substTyConBinderX :: Subst -> TyConBinder -> (Subst, TyConBinder)
substTyConBinderX subst (Bndr tv vis)
  = (subst', Bndr tv' vis)
  where
    (subst', tv') = substTyVarBndr subst tv

substTyConBindersX :: Subst -> [TyConBinder] -> (Subst, [TyConBinder])
substTyConBindersX = mapAccumL substTyConBinderX

swizzleTcb :: VarEnv Name -> Subst -> TyConBinder -> (Subst, TyConBinder)
swizzleTcb swizzle_env subst (Bndr tv vis)
  = (subst', Bndr tv2 vis)
  where
    subst' = extendTCvSubstWithClone subst tv tv2
    tv1 = updateTyVarKind (substTy subst) tv
    tv2 = case lookupVarEnv swizzle_env tv of
             Just user_name -> setTyVarName tv1 user_name
             Nothing        -> tv1
    -- NB: the SrcSpan on an implicitly-bound name deliberately spans
    -- the whole declaration. e.g.
    --    data T (a :: k) (b :: Type -> k) = ....
    -- There is no single binding site for 'k'.
    -- See Note [Source locations for implicitly bound type variables]
    -- in GHC.Tc.Rename.HsType

{- Note [kcCheckDeclHeader_sig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a kind signature 'sig_kind' and a declaration header,
kcCheckDeclHeader_sig verifies that the declaration conforms to the
signature. The end result is a PolyTcTyCon 'tc' such that:
  tyConKind tc == sig_kind

Basic plan is this:
  * splitTyConKind: Take the Kind from the separate kind signature, and
    decompose it all the way to a [TyConBinder] and a Kind in the corner.

    NB: these TyConBinders contain TyVars, not TcTyVars.

  * matchUpSigWithDecl: match the [TyConBinder] from the signature with
    the [LHsTyVarBndr (HsBndrVis GhcRn) GhcRn] from the declaration.  The latter are the
    explicit, user-written binders.  e.g.
        data T (a :: k) b = ....
    There may be more of the former than the latter, because the former
    include invisible binders.  matchUpSigWithDecl uses isVisibleTcbVis
    to decide which TyConBinders are visible.

  * matchUpSigWithDecl also skolemises the [TyConBinder] to produce
    a [TyConBinder], corresponding 1-1 with the consumed [TyConBinder].
    Each new TyConBinder
      - Uses the Name from the LHsTyVarBndr, if available, both because that's
        what the user expects, and because the binding site accurately comes
        from the data/type declaration.
      - Uses a skolem TcTyVar.  We need these to allow unification.

  * machUpSigWithDecl also unifies the user-supplied kind signature for each
    LHsTyVarBndr with the kind that comes from the TyConBinder (itself coming
    from the separate kind signature).

  * Finally, kcCheckDeclHeader_sig unifies the return kind of the separate
    signature with the kind signature (if any) in the data/type declaration.
    E.g.
           type S :: forall k. k -> k -> Type
           type family S (a :: j) :: j -> Type
    Here we match up the 'k ->' with (a :: j); and then must unify the leftover
    part of the signature (k -> Type) with the kind signature of the decl,
    (j -> Type).  This unification, done in kcCheckDeclHeader, needs TcTyVars.

Note [Arity of type families and type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  type F0 :: forall k. k -> k -> Type
  type family F0

  type F1 :: forall k. k -> k -> Type
  type family F1 @k

  type F2a :: forall k. k -> k -> Type
  type family F2a @k a

  type F2b :: forall k. k -> k -> Type
  type family F2b a

  type F3 :: forall k. k -> k -> Type
  type family F3 a b

All five have the same /kind/, but what /arity/ do they have?
For a type family, the arity is critical:
* A type family must always appear saturated (up to its arity)
* A type family can match only on `arity` arguments, not further ones
* The arity is recorded by `tyConArity`, and is equal to the number of
  `TyConBinders` in the `TyCon`.
* In this context "arity" includes both kind and type arguments.

The arity is not determined by the kind signature (all five have the same signature).
Rather, it is determined by the declaration of the family:
* `F0` has arity 0.
* `F1` has arity 1.
* `F2a` has arity 2.
* `F2b` also has arity 2: the kind argument is invisible.
* `F3` has arity 3; again the kind argument is invisible.

The matching-up of kind signature with the declaration itself is done by
`matchUpWithSigDecl`.

Note [discardResult in tcHsTvbKind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use 'unifyKind' to check inline kind annotations in declaration headers
against the signature.

  type T :: [i] -> Maybe j -> Type
  data T (a :: [k1]) (b :: Maybe k2) :: Type where ...

Here, we will unify:

       [k1] ~ [i]
  Maybe k2  ~ Maybe j
      Type  ~ Type

The end result is that we fill in unification variables k1, k2:

    k1  :=  i
    k2  :=  j

We also validate that the user isn't confused:

  type T :: Type -> Type
  data T (a :: Bool) = ...

This will report that (Type ~ Bool) failed to unify.

Now, consider the following example:

  type family Id a where Id x = x
  type T :: Bool -> Type
  type T (a :: Id Bool) = ...

We will unify (Bool ~ Id Bool), and this will produce a non-reflexive coercion.
However, we are free to discard it, as the kind of 'T' is determined by the
signature, not by the inline kind annotation:

      we have   T ::    Bool -> Type
  rather than   T :: Id Bool -> Type

This (Id Bool) will not show up anywhere after we're done validating it, so we
have no use for the produced coercion.

Note [Kind variable ordering for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should be the kind of `T` in the following example? (#15591)

  class C (a :: Type) where
    type T (x :: f a)

As per Note [Ordering of implicit variables] in GHC.Rename.HsType, we want to quantify
the kind variables in left-to-right order of first occurrence in order to
support visible kind application. But we cannot perform this analysis on just
T alone, since its variable `a` actually occurs /before/ `f` if you consider
the fact that `a` was previously bound by the parent class `C`. That is to say,
the kind of `T` should end up being:

  T :: forall a f. f a -> Type

(It wouldn't necessarily be /wrong/ if the kind ended up being, say,
forall f a. f a -> Type, but that would not be as predictable for users of
visible kind application.)

In contrast, if `T` were redefined to be a top-level type family, like `T2`
below:

  type family T2 (x :: f (a :: Type))

Then `a` first appears /after/ `f`, so the kind of `T2` should be:

  T2 :: forall f a. f a -> Type

In order to make this distinction, we need to know (in kcCheckDeclHeader) which
type variables have been bound by the parent class (if there is one). With
the class-bound variables in hand, we can ensure that we always quantify
these first.
-}


{- *********************************************************************
*                                                                      *
             Expected kinds
*                                                                      *
********************************************************************* -}

-- | Describes the kind expected in a certain context.
data ContextKind = TheKind TcKind   -- ^ a specific kind
                 | AnyKind        -- ^ any kind will do
                 | OpenKind       -- ^ something of the form @TYPE _@

-- debug only
instance Outputable ContextKind where
  ppr AnyKind = text "AnyKind"
  ppr OpenKind = text "OpenKind"
  ppr (TheKind k) = text "TheKind" <+> ppr k

-----------------------
newExpectedKind :: ContextKind -> TcM TcKind
newExpectedKind (TheKind k)   = return k
newExpectedKind AnyKind       = newMetaKindVar
newExpectedKind OpenKind      = newOpenTypeKind

-----------------------
expectedKindInCtxt :: UserTypeCtxt -> ContextKind
-- Depending on the context, we might accept any kind (for instance, in a TH
-- splice), or only certain kinds (like in type signatures).
expectedKindInCtxt (TySynCtxt _)   = AnyKind
expectedKindInCtxt (GhciCtxt {})   = AnyKind
-- The types in a 'default' decl can have varying kinds
-- See Note [Extended defaults]" in GHC.Tc.Utils.Env
expectedKindInCtxt DefaultDeclCtxt     = AnyKind
expectedKindInCtxt DerivClauseCtxt     = AnyKind
expectedKindInCtxt TypeAppCtxt         = AnyKind
expectedKindInCtxt (ForSigCtxt _)      = TheKind liftedTypeKind
expectedKindInCtxt (InstDeclCtxt {})   = TheKind constraintKind
expectedKindInCtxt SpecInstCtxt        = TheKind constraintKind
expectedKindInCtxt _                   = OpenKind


{- *********************************************************************
*                                                                      *
          Scoped tyvars that map to the same thing
*                                                                      *
********************************************************************* -}

checkForDisconnectedScopedTyVars :: Name -> TyConFlavour TyCon -> [TcTyConBinder]
                                 -> [(Name,TcTyVar)] -> TcM ()
-- See Note [Disconnected type variables]
-- For the type synonym case see Note [Out of arity type variables]
-- `scoped_prs` is the mapping gotten by unifying
--    - the standalone kind signature for T, with
--    - the header of the type/class declaration for T
checkForDisconnectedScopedTyVars name flav all_tcbs scoped_prs
         -- needsEtaExpansion: see wrinkle (DTV1) in Note [Disconnected type variables]
  | needsEtaExpansion flav     = mapM_ report_disconnected (filterOut ok scoped_prs)
  | flav == TypeSynonymFlavour = mapM_ report_out_of_arity (filterOut ok scoped_prs)
  | otherwise = pure ()
  where
    all_tvs = mkVarSet (binderVars all_tcbs)
    ok (_, tc_tv) = tc_tv `elemVarSet` all_tvs

    report_disconnected :: (Name,TcTyVar) -> TcM ()
    report_disconnected (nm, _)
      = setSrcSpan (getSrcSpan nm) $
        addErrTc $ TcRnDisconnectedTyVar nm

    report_out_of_arity :: (Name,TcTyVar) -> TcM ()
    report_out_of_arity (tv_nm, _)
      = setSrcSpan (getSrcSpan tv_nm) $
        addErrTc $ TcRnOutOfArityTyVar name tv_nm

checkForDuplicateScopedTyVars :: [(Name,TcTyVar)] -> TcM ()
-- Check for duplicates
-- See Note [Aliasing in type and class declarations]
checkForDuplicateScopedTyVars scoped_prs
  = unless (null err_prs) $
    do { mapM_ report_dup err_prs; failM }
  where
    -------------- Error reporting ------------
    err_prs :: [(Name,Name)]
    err_prs = [ (n1,n2)
              | prs :: NonEmpty (Name,TyVar) <- findDupsEq ((==) `on` snd) scoped_prs
              , (n1,_) :| ((n2,_) : _) <- [NE.nubBy ((==) `on` fst) prs] ]
              -- This nubBy avoids bogus error reports when we have
              --    [("f", f), ..., ("f",f)....] in swizzle_prs
              -- which happens with  class C f where { type T f }

    report_dup :: (Name,Name) -> TcM ()
    report_dup (n1,n2)
      = setSrcSpan (getSrcSpan n2) $
        addErrTc $ TcRnDifferentNamesForTyVar n1 n2


{- Note [Aliasing in type and class declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data SameKind (a::k) (b::k)
  data T1 (a::k1) (b::k2) c = MkT (SameKind a b) c
We do not allow this, because `k1` and `k2` would both stand for the same type
variable -- they are both aliases for `k`.

Other examples
  data T2 (a::k1)                 = MkT2 (SameKind a Int) -- k1 stands for Type
  data T3 @k1 @k2 (a::k1) (b::k2) = MkT (SameKind a b)    -- k1 and k2 are aliases

  type UF :: forall zk. zk -> Constraint
  class UF @kk (xb :: k) where   -- kk and k are aliases
    op :: (xs::kk) -> Bool

See #24604 for an example that crashed GHC.

There is a design choice here. It would be possible to allow implicit type variables
like `k1` and `k2` in T1's declartion to stand for /abitrary kinds/.  This is in fact
the rule we use in /terms/ pattern signatures:
    f :: [Int] -> Int
    f ((x::a) : xs) = ...
Here `a` stands for `Int`.  But in type /signatures/ we make a different choice:
    f1 :: forall (a::k1) (b::k2). SameKind a b -> blah
    f2 :: forall (a::k). SameKind a Int -> blah

Here f1's signature is rejected because `k1` and `k2` are aliased; and f2's is
rejected because `k` stands for `Int`.

Our current choice is that type and class declarations behave more like signatures;
we do not allow aliasing.  That is what `checkForDuplicateScopedTyVars` checks.
See !12328 for some design discussion.


Note [Disconnected type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This note applies when kind-checking the header of a type/class decl that has
a separate, standalone kind signature.  See #24083.

Consider:
   type S a = Type

   type C :: forall k. S k -> Constraint
   class C (a :: S kk) where
     op :: ...kk...

Note that the class has a separate kind signature, so the elaborated decl should
look like
   class C @kk (a :: S kk) where ...

But how can we "connect up" the scoped variable `kk` with the skolem kind from the
standalone kind signature for `C`?  In general we do this by unifying the two.
For example
   type T k = (k,Type)
   type W :: forall k. T k -> Type
   data W (a :: (x,Type)) = ..blah blah..

When we encounter (a :: (x,Type)) we unify the kind (x,Type) with the kind (T k)
from the standalone kind signature.  Of course, unification looks through synonyms
so we end up with the mapping [x :-> k] that connects the scoped type variable `x`
with the kind from the signature.

But in our earlier example this unification is ineffective -- because `S` is a
phantom synonym that just discards its argument.  So our plan is this:

  if matchUpSigWithDecl fails to connect `kk with `k`, by unification,
  we give up and complain about a "disconnected" type variable.

See #24083 for dicussion of alternatives, none satisfactory.  Also the fix is
easy: just add an explicit `@kk` parameter to the declaration, to bind `kk`
explicitly, rather than binding it implicitly via unification.

(DTV1) We only want to make this check when there /are/ scoped type variables; and
  that is determined by needsEtaExpansion.  Examples:

     type C :: x -> y -> Constraint
     class C a :: b -> Constraint where { ... }
     -- The a,b scope over the "..."

     type D :: forall k. k -> Type
     data family D :: kk -> Type
     -- Nothing for `kk` to scope over!

  In the latter data-family case, the match-up stuff in kcCheckDeclHeader_sig will
  return [] for `extra_tcbs`, and in fact `all_tcbs` will be empty.  So if we do
  the check-for-disconnected-tyvars check we'll complain that `kk` is not bound
  to one of `all_tcbs` (see #24083, comments about the `singletons` package).

  The scoped-tyvar stuff is needed precisely for data/class/newtype declarations,
  where needsEtaExpansion is True.

Note [Out of arity type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(Relevant ticket: #24470)
Type synonyms have a special scoping rule that allows implicit quantification in
the outermost kind signature:

  type P_e :: k -> Type
  type P_e @k = Proxy :: k -> Type     -- explicit binding

  type P_i    = Proxy :: k -> Type     -- implicit binding (relies on the special rule)

This is a deprecated feature (warning flag: -Wimplicit-rhs-quantification) but
we have to support it for a couple more releases. It is explained in more detail
in Note [Implicit quantification in type synonyms] in GHC.Rename.HsType.

Type synonyms `P_e` and `P_i` are equivalent.  Both of them have kind
`forall k. k -> Type` and arity 1. (Recall that the arity of a type synonym is
the number of arguments it requires at use sites; the arity matter because
unsaturated application of type families and type synonyms is not allowed).

We start to see problems when implicit RHS quantification (as in `P_i`) is
combined with a standalone king signature (like the one that `P_e` has).
That is:

  type P_i_sig :: k -> Type
  type P_i_sig = Proxy :: k -> Type

Per GHC Proposal #425, the arity of `P_i_sig` is determined /by the LHS only/,
which has no binders. So the arity of `P_i_sig` is 0.
At the same time, the legacy implicit quantification rule dictates that `k` is
brought into scope, as if there was a binder `@k` on the LHS.

We end up with a `k` that is in scope on the RHS but cannot be bound implicitly
on the LHS without affecting the arity. This led to #24470 (a compiler crash)

  GHC internal error: ‘k’ is not in scope during type checking,
                      but it passed the renamer

This problem occurs only if the arity of the type synonym is insufficiently
high to accommodate an implicit binding. It can be worked around by adding an
unused binder on the LHS:

  type P_w :: k -> Type
  type P_w @_w = Proxy :: k -> Type

The variable `_w` is unused. The only effect of the `@_w` binder is that the
arity of `P_w` is changed from 0 to 1. However, bumping the arity is exactly
what's needed to make the implicit binding of `k` possible.

All this is a rather unfortunate bit of accidental complexity that will go away
when GHC drops support for implicit RHS quantification. In the meantime, we
ought to produce a proper error message instead of a compiler panic, and we do
that with a check in checkForDisconnectedScopedTyVars:

  | flav == TypeSynonymFlavour = mapM_ report_out_of_arity (filterOut ok scoped_prs)

-}

{- *********************************************************************
*                                                                      *
             Bringing type variables into scope
*                                                                      *
********************************************************************* -}

--------------------------------------
--    HsForAllTelescope
--------------------------------------

tcTKTelescope :: TcTyMode
              -> HsForAllTelescope GhcRn
              -> TcM a
              -> TcM ([TcTyVarBinder], a)
-- A HsForAllTelescope comes only from a HsForAllTy,
-- an explicit, user-written forall type
tcTKTelescope mode tele thing_inside = case tele of
  HsForAllVis { hsf_vis_bndrs = bndrs }
    -> do { skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn (unLoc <$> bndrs)))
          ; let skol_mode = smVanilla { sm_clone = False, sm_holes = mode_holes mode
                                      , sm_tvtv = SMDSkolemTv skol_info }
          ; (req_tv_bndrs, thing) <- tcExplicitTKBndrsX skol_mode bndrs thing_inside
            -- req_tv_bndrs :: [VarBndr TyVar ()],
            -- but we want [VarBndr TyVar ForAllTyFlag]
          ; return (tyVarReqToBinders req_tv_bndrs, thing) }

  HsForAllInvis { hsf_invis_bndrs = bndrs }
    -> do { skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn (unLoc <$> bndrs)))
          ; let skol_mode = smVanilla { sm_clone = False, sm_holes = mode_holes mode
                                      , sm_tvtv = SMDSkolemTv skol_info }
          ; (inv_tv_bndrs, thing) <- tcExplicitTKBndrsX skol_mode bndrs thing_inside
            -- inv_tv_bndrs :: [VarBndr TyVar Specificity],
            -- but we want [VarBndr TyVar ForAllTyFlag]
          ; return (tyVarSpecToBinders inv_tv_bndrs, thing) }

--------------------------------------
--    HsOuterTyVarBndrs
--------------------------------------

bindOuterTKBndrsX :: OutputableBndrFlag flag 'Renamed  -- Only to support traceTc
                  => SkolemMode
                  -> HsOuterTyVarBndrs flag GhcRn
                  -> TcM a
                  -> TcM (HsOuterTyVarBndrs flag GhcTc, a)
bindOuterTKBndrsX skol_mode outer_bndrs thing_inside
  = case outer_bndrs of
      HsOuterImplicit{hso_ximplicit = imp_tvs} ->
        do { (imp_tvs', thing) <- bindImplicitTKBndrsX skol_mode imp_tvs thing_inside
           ; return ( HsOuterImplicit{hso_ximplicit = imp_tvs'}
                    , thing) }
      HsOuterExplicit{hso_bndrs = exp_bndrs} ->
        do { (exp_tvs', thing) <- bindExplicitTKBndrsX skol_mode exp_bndrs thing_inside
           ; return ( HsOuterExplicit { hso_xexplicit = exp_tvs'
                                      , hso_bndrs     = exp_bndrs }
                    , thing) }

---------------
outerTyVars :: HsOuterTyVarBndrs flag GhcTc -> [TcTyVar]
-- The returned [TcTyVar] is not necessarily in dependency order
-- at least for the HsOuterImplicit case
outerTyVars (HsOuterImplicit { hso_ximplicit = tvs })  = tvs
outerTyVars (HsOuterExplicit { hso_xexplicit = tvbs }) = binderVars tvbs

---------------
outerTyVarBndrs :: HsOuterTyVarBndrs Specificity GhcTc -> [InvisTVBinder]
outerTyVarBndrs (HsOuterImplicit{hso_ximplicit = imp_tvs}) = [Bndr tv SpecifiedSpec | tv <- imp_tvs]
outerTyVarBndrs (HsOuterExplicit{hso_xexplicit = exp_tvs}) = exp_tvs

---------------
scopedSortOuter :: HsOuterTyVarBndrs flag GhcTc -> TcM (HsOuterTyVarBndrs flag GhcTc)
-- Sort any /implicit/ binders into dependency order
--     (zonking first so we can see the dependencies)
-- /Explicit/ ones are already in the right order
scopedSortOuter (HsOuterImplicit{hso_ximplicit = imp_tvs})
  = do { imp_tvs <- zonkAndScopedSort imp_tvs
       ; return (HsOuterImplicit { hso_ximplicit = imp_tvs }) }
scopedSortOuter bndrs@(HsOuterExplicit{})
  = -- No need to dependency-sort (or zonk) explicit quantifiers
    return bndrs

---------------
bindOuterSigTKBndrs_Tv :: HsOuterSigTyVarBndrs GhcRn
                       -> TcM a -> TcM (HsOuterSigTyVarBndrs GhcTc, a)
bindOuterSigTKBndrs_Tv
  = bindOuterTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDTyVarTv })

bindOuterSigTKBndrs_Tv_M :: TcTyMode
                         -> HsOuterSigTyVarBndrs GhcRn
                         -> TcM a -> TcM (HsOuterSigTyVarBndrs GhcTc, a)
-- Do not push level; do not make implication constraint; use Tvs
-- Two major clients of this "bind-only" path are:
--    Note [Using TyVarTvs for kind-checking GADTs] in GHC.Tc.TyCl
--    Note [Checking partial type signatures]
bindOuterSigTKBndrs_Tv_M mode
  = bindOuterTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDTyVarTv
                                 , sm_holes = mode_holes mode })

bindOuterFamEqnTKBndrs_Q_Tv :: HsOuterFamEqnTyVarBndrs GhcRn
                            -> TcM a
                            -> TcM (HsOuterFamEqnTyVarBndrs GhcTc, a)
bindOuterFamEqnTKBndrs_Q_Tv hs_bndrs thing_inside
  = bindOuterTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                 , sm_tvtv = SMDTyVarTv })
                      hs_bndrs thing_inside
    -- sm_clone=False: see Note [Cloning for type variable binders]

bindOuterFamEqnTKBndrs :: SkolemInfo
                       -> HsOuterFamEqnTyVarBndrs GhcRn
                       -> TcM a
                       -> TcM (HsOuterFamEqnTyVarBndrs GhcTc, a)
bindOuterFamEqnTKBndrs skol_info
  = bindOuterTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                 , sm_tvtv = SMDSkolemTv skol_info })
    -- sm_clone=False: see Note [Cloning for type variable binders]

---------------
tcOuterTKBndrs :: OutputableBndrFlag flag 'Renamed   -- Only to support traceTc
               => SkolemInfo
               -> HsOuterTyVarBndrs flag GhcRn
               -> TcM a -> TcM (HsOuterTyVarBndrs flag GhcTc, a)
tcOuterTKBndrs skol_info
  = tcOuterTKBndrsX (smVanilla { sm_clone = False
                               , sm_tvtv = SMDSkolemTv skol_info })
                    skol_info
  -- Do not clone the outer binders
  -- See Note [Cloning for type variable binders] under "must not"

tcOuterTKBndrsX :: OutputableBndrFlag flag 'Renamed   -- Only to support traceTc
                => SkolemMode -> SkolemInfo
                -> HsOuterTyVarBndrs flag GhcRn
                -> TcM a -> TcM (HsOuterTyVarBndrs flag GhcTc, a)
-- Push level, capture constraints, make implication
tcOuterTKBndrsX skol_mode skol_info outer_bndrs thing_inside
  = case outer_bndrs of
      HsOuterImplicit{hso_ximplicit = imp_tvs} ->
        do { (imp_tvs', thing) <- tcImplicitTKBndrsX skol_mode skol_info imp_tvs thing_inside
           ; return ( HsOuterImplicit{hso_ximplicit = imp_tvs'}
                    , thing) }
      HsOuterExplicit{hso_bndrs = exp_bndrs} ->
        do { (exp_tvs', thing) <- tcExplicitTKBndrsX skol_mode exp_bndrs thing_inside
           ; return ( HsOuterExplicit { hso_xexplicit = exp_tvs'
                                      , hso_bndrs     = exp_bndrs }
                    , thing) }

--------------------------------------
--    Explicit tyvar binders
--------------------------------------

tcExplicitTKBndrs :: OutputableBndrFlag flag 'Renamed    -- Only to suppor traceTc
                  => SkolemInfo
                  -> [LHsTyVarBndr flag GhcRn]
                  -> TcM a
                  -> TcM ([VarBndr TyVar flag], a)
tcExplicitTKBndrs skol_info
  = tcExplicitTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDSkolemTv skol_info })

tcExplicitTKBndrsX :: OutputableBndrFlag flag 'Renamed    -- Only to suppor traceTc
                   => SkolemMode
                   -> [LHsTyVarBndr flag GhcRn]
                   -> TcM a
                   -> TcM ([VarBndr TyVar flag], a)
-- Push level, capture constraints, and emit an implication constraint.
-- The implication constraint has a ForAllSkol ic_info,
--   so that it is subject to a telescope test.
tcExplicitTKBndrsX skol_mode bndrs thing_inside = case nonEmpty bndrs of
    Nothing -> do
       { res <- thing_inside
       ; return ([], res) }

    Just bndrs1 -> do
       { (tclvl, wanted, (skol_tvs, res))
             <- pushLevelAndCaptureConstraints $
                bindExplicitTKBndrsX skol_mode bndrs $
                thing_inside

       -- Set up SkolemInfo for telescope test
       ; let bndr_1 = NE.head bndrs1; bndr_n = NE.last bndrs1
       ; skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn  (unLoc <$> bndrs)))
         -- Notice that we use ForAllSkol here, ignoring the enclosing
         -- skol_info unlike tcImplicitTKBndrs, because the bad-telescope
         -- test applies only to ForAllSkol

       ; setSrcSpan (combineSrcSpans (getLocA bndr_1) (getLocA bndr_n))
       $ emitResidualTvConstraint skol_info (binderVars skol_tvs) tclvl wanted

       ; return (skol_tvs, res) }

----------------
-- | Skolemise the 'HsTyVarBndr's in an 'HsForAllTelescope' with the supplied
-- 'TcTyMode'.
bindExplicitTKBndrs_Skol
    :: (OutputableBndrFlag flag 'Renamed)      -- Only to suppor traceTc
    => SkolemInfo
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)

bindExplicitTKBndrs_Tv
    :: (OutputableBndrFlag flag 'Renamed)    -- Only to suppor traceTc
    => [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)

bindExplicitTKBndrs_Skol skol_info = bindExplicitTKBndrsX (smVanilla { sm_clone = False, sm_tvtv = SMDSkolemTv skol_info })
bindExplicitTKBndrs_Tv   = bindExplicitTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDTyVarTv })
   -- sm_clone: see Note [Cloning for type variable binders]

bindExplicitTKBndrs_Q_Skol
    :: (OutputableBndrFlag flag 'Renamed)   -- Only to support traceTc
    => SkolemInfo
    -> ContextKind
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)

bindExplicitTKBndrs_Q_Tv
    :: (OutputableBndrFlag flag 'Renamed)   -- Only to support traceTc
    => ContextKind
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)
-- These do not clone: see Note [Cloning for type variable binders]
bindExplicitTKBndrs_Q_Skol skol_info ctxt_kind hs_bndrs thing_inside
  = bindExplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                    , sm_kind = ctxt_kind, sm_tvtv = SMDSkolemTv skol_info })
                         hs_bndrs thing_inside
    -- sm_clone=False: see Note [Cloning for type variable binders]

bindExplicitTKBndrs_Q_Tv  ctxt_kind hs_bndrs thing_inside
  = bindExplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                    , sm_tvtv = SMDTyVarTv, sm_kind = ctxt_kind })
                         hs_bndrs thing_inside
    -- sm_clone=False: see Note [Cloning for type variable binders]

bindExplicitTKBndrsX
    :: (OutputableBndrFlag flag 'Renamed)   -- Only to support traceTc
    => SkolemMode
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)  -- Returned [TcTyVar] are in 1-1 correspondence
                                      -- with the passed-in [LHsTyVarBndr]
bindExplicitTKBndrsX skol_mode@(SM { sm_parent = check_parent, sm_kind = ctxt_kind
                                   , sm_holes = hole_info })
                     hs_tvs thing_inside
  = do { traceTc "bindExplicitTKBndrs" (ppr hs_tvs)
       ; go hs_tvs }
  where
    tc_ki_mode = TcTyMode { mode_tyki = KindLevel, mode_holes = hole_info }
                 -- Inherit the HoleInfo from the context

    go [] = do { res <- thing_inside
               ; return ([], res) }
    go (L _ hs_tv : hs_tvs)
       = do { lcl_env <- getLclTypeEnv
            ; tv <- tc_hs_bndr lcl_env hs_tv
            -- Extend the environment as we go, in case a binder
            -- is mentioned in the kind of a later binder
            --   e.g. forall k (a::k). blah
            -- NB: tv's Name may differ from hs_tv's
            -- See Note [Cloning for type variable binders]
            ; (tvs,res) <- tcExtendNameTyVarEnv (mk_tvb_pairs hs_tv tv) $
                           go hs_tvs
            ; return (Bndr tv (hsTyVarBndrFlag hs_tv):tvs, res) }

    tc_hs_bndr :: TcTypeEnv -> HsTyVarBndr flag GhcRn -> TcM TcTyVar
    tc_hs_bndr lcl_env (HsTvb { tvb_var = bvar, tvb_kind = kind })
      | check_parent
      , HsBndrVar _ (L _ name) <- bvar
      , Just (ATyVar _ tv) <- lookupNameEnv lcl_env name
      = do { check_hs_bndr_kind name (tyVarKind tv) kind
           ; return tv }
      | otherwise
      = do { name <- tcHsBndrVarName bvar
           ; kind' <- tc_hs_bndr_kind name kind
           ; newTyVarBndr skol_mode name kind' }

    tc_hs_bndr_kind :: Name -> HsBndrKind GhcRn -> TcM Kind
    tc_hs_bndr_kind _    (HsBndrNoKind _)    = newExpectedKind ctxt_kind
    tc_hs_bndr_kind name (HsBndrKind _ kind) = tc_lhs_kind_sig tc_ki_mode (TyVarBndrKindCtxt name) kind

    -- Check the HsBndrKind against the kind of the parent type variable,
    -- e.g. the following is rejected:
    --   class C (m :: * -> *) where
    --     type F (m :: *) = ...
    check_hs_bndr_kind :: Name -> Kind -> HsBndrKind GhcRn -> TcM ()
    check_hs_bndr_kind _    _           (HsBndrNoKind _)    = return ()
    check_hs_bndr_kind name parent_kind (HsBndrKind _ kind) =
      do { kind' <- tc_lhs_kind_sig tc_ki_mode (TyVarBndrKindCtxt name) kind
         ; discardResult $
           unifyKind (Just $ NameThing name) kind' parent_kind }

tcHsBndrVarName :: HsBndrVar GhcRn -> TcM Name
tcHsBndrVarName (HsBndrVar _ (L _ name)) = return name
tcHsBndrVarName (HsBndrWildCard _) = newSysName (mkTyVarOcc "_")

mk_tvb_pairs :: HsTyVarBndr flag GhcRn -> TcTyVar -> [(Name, TcTyVar)]
mk_tvb_pairs tvb tv =
  case hsTyVarName tvb of
    Nothing -> []
    Just nm -> [(nm, tv)]

newTyVarBndr :: SkolemMode -> Name -> Kind -> TcM TcTyVar
newTyVarBndr (SM { sm_clone = clone, sm_tvtv = tvtv }) name kind
  = do { name <- case clone of
              True -> do { uniq <- newUnique
                         ; return (setNameUnique name uniq) }
              False -> return name
       ; details <- case tvtv of
                 SMDTyVarTv  -> newMetaDetails TyVarTv
                 SMDSkolemTv skol_info ->
                  do { lvl <- getTcLevel
                     ; return (SkolemTv skol_info lvl False) }
       ; return (mkTcTyVar name kind details) }

--------------------------------------
--    Implicit tyvar binders
--------------------------------------

tcImplicitTKBndrsX :: SkolemMode -> SkolemInfo
                   -> [Name]
                   -> TcM a
                   -> TcM ([TcTyVar], a)
-- The workhorse:
--    push level, capture constraints, and emit an implication constraint
tcImplicitTKBndrsX skol_mode skol_info bndrs thing_inside
  | null bndrs  -- Short-cut the common case with no quantifiers
                -- E.g. f :: Int -> Int
                --      makes a HsOuterImplicit with empty bndrs,
                --      and tcOuterTKBndrsX goes via here
  = do { res <- thing_inside; return ([], res) }
  | otherwise
  = do { (tclvl, wanted, (skol_tvs, res))
             <- pushLevelAndCaptureConstraints       $
                bindImplicitTKBndrsX skol_mode bndrs $
                thing_inside

       ; emitResidualTvConstraint skol_info skol_tvs tclvl wanted

       ; return (skol_tvs, res) }

------------------
bindImplicitTKBndrs_Skol,
  bindImplicitTKBndrs_Q_Skol :: SkolemInfo -> [Name] -> TcM a -> TcM ([TcTyVar], a)

bindImplicitTKBndrs_Tv, bindImplicitTKBndrs_Q_Tv :: [Name] -> TcM a -> TcM ([TcTyVar], a)
bindImplicitTKBndrs_Skol skol_info = bindImplicitTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDSkolemTv skol_info })
bindImplicitTKBndrs_Tv   = bindImplicitTKBndrsX (smVanilla { sm_clone = True, sm_tvtv = SMDTyVarTv })
bindImplicitTKBndrs_Q_Skol skol_info = bindImplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True, sm_tvtv = SMDSkolemTv skol_info })
bindImplicitTKBndrs_Q_Tv = bindImplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True, sm_tvtv = SMDTyVarTv })

bindImplicitTKBndrsX
   :: SkolemMode
   -> [Name]               -- Generated by renamer; not in dependency order
   -> TcM a
   -> TcM ([TcTyVar], a)   -- Returned [TcTyVar] are in 1-1 correspondence
                           -- with the passed in [Name]
bindImplicitTKBndrsX skol_mode@(SM { sm_parent = check_parent, sm_kind = ctxt_kind })
                     tv_names thing_inside
  = do { lcl_env <- getLclTypeEnv
       ; tkvs <- mapM (new_tv lcl_env) tv_names
       ; traceTc "bindImplicitTKBndrsX" (ppr tv_names $$ ppr tkvs)
       ; res <- tcExtendNameTyVarEnv (tv_names `zip` tkvs)
                thing_inside
       ; return (tkvs, res) }
  where
    new_tv lcl_env name
      | check_parent
      , Just (ATyVar _ tv) <- lookupNameEnv lcl_env name
      = return tv
      | otherwise
      = do { kind <- newExpectedKind ctxt_kind
           ; newTyVarBndr skol_mode name kind }

--------------------------------------
--           SkolemMode
--------------------------------------

-- | 'SkolemMode' describes how to typecheck an explicit ('HsTyVarBndr') or
-- implicit ('Name') binder in a type. It is just a record of flags
-- that describe what sort of 'TcTyVar' to create.
data SkolemMode
  = SM { sm_parent :: Bool    -- True <=> check the in-scope parent type variable
                              -- Used only for asssociated types

       , sm_clone  :: Bool    -- True <=> fresh unique
                              -- See Note [Cloning for type variable binders]

       , sm_tvtv   :: SkolemModeDetails    -- True <=> use a TyVarTv, rather than SkolemTv
                              -- Why?  See Note [Inferring kinds for type declarations]
                              -- in GHC.Tc.TyCl, and (in this module)
                              -- Note [Checking partial type signatures]

       , sm_kind   :: ContextKind  -- Use this for the kind of any new binders

       , sm_holes  :: HoleInfo     -- What to do for wildcards in the kind
       }

data SkolemModeDetails
  = SMDTyVarTv
  | SMDSkolemTv SkolemInfo


smVanilla :: HasDebugCallStack => SkolemMode
smVanilla = SM { sm_clone  = panic "sm_clone"  -- We always override this
               , sm_parent = False
               , sm_tvtv   = pprPanic "sm_tvtv" callStackDoc -- We always override this
               , sm_kind   = AnyKind
               , sm_holes  = Nothing }

{- Note [Cloning for type variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes we must clone the Name of a type variable binder (written in
the source program); and sometimes we must not. This is controlled by
the sm_clone field of SkolemMode.

In some cases it doesn't matter whether or not we clone. Perhaps
it'd be better to use MustClone/MayClone/MustNotClone.

When we /must not/ clone
* In the binders of a type signature (tcOuterTKBndrs)
      f :: forall a{27}. blah
      f = rhs
  Then 'a' scopes over 'rhs'. When we kind-check the signature (tcHsSigType),
  we must get the type (forall a{27}. blah) for the Id f, because
  we bring that type variable into scope when we typecheck 'rhs'.

* In the binders of a data family instance (bindOuterFamEqnTKBndrs)
     data instance
       forall p q. D (p,q) = D1 p | D2 q
  We kind-check the LHS in tcDataFamInstHeader, and then separately
  (in tcDataFamInstDecl) bring p,q into scope before looking at the
  the constructor decls.

* bindExplicitTKBndrs_Q_Tv/bindImplicitTKBndrs_Q_Tv do not clone
  We take advantage of this in kcInferDeclHeader:
     all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
  If we cloned, we'd need to take a bit more care here; not hard.

* bindExplicitTKBndrs_Q_Skol, bindExplicitTKBndrs_Skol, do not clone.
  There is no need, I think.

  The payoff here is that avoiding gratuitous cloning means that we can
  almost always take the fast path in swizzleTcTyConBndrs.

When we /must/ clone.
* bindOuterSigTKBndrs_Tv, bindExplicitTKBndrs_Tv do cloning

  This for a narrow and tricky reason which, alas, I couldn't find a
  simpler way round.  #16221 is the poster child:

     data SameKind :: k -> k -> *
     data T a = forall k2 (b :: k2). MkT (SameKind a b) !Int

  When kind-checking T, we give (a :: kappa1). Then:

  - In kcConDecl we make a TyVarTv unification variable kappa2 for k2
    (as described in Note [Using TyVarTvs for kind-checking GADTs],
    even though this example is an existential)
  - So we get (b :: kappa2) via bindExplicitTKBndrs_Tv
  - We end up unifying kappa1 := kappa2, because of the (SameKind a b)

  Now we generalise over kappa2. But if kappa2's Name is precisely k2
  (i.e. we did not clone) we'll end up giving T the utterly final kind
    T :: forall k2. k2 -> *
  Nothing directly wrong with that but when we typecheck the data constructor
  we have k2 in scope; but then it's brought into scope /again/ when we find
  the forall k2.  This is chaotic, and we end up giving it the type
    MkT :: forall k2 (a :: k2) k2 (b :: k2).
           SameKind @k2 a b -> Int -> T @{k2} a
  which is bogus -- because of the shadowing of k2, we can't
  apply T to the kind or a!

  And there no reason /not/ to clone the Name when making a unification
  variable.  So that's what we do.
-}

--------------------------------------
-- Binding type/class variables in the
-- kind-checking and typechecking phases
--------------------------------------

bindTyClTyVars :: Name -> ([TcTyConBinder] -> TcKind -> TcM a) -> TcM a
-- ^ Bring into scope the binders of a PolyTcTyCon
-- Used for the type variables of a type or class decl
-- in the "kind checking" and "type checking" pass,
-- but not in the initial-kind run.
bindTyClTyVars tycon_name thing_inside
  = do { tycon <- tcLookupTcTyCon tycon_name     -- The tycon is a PolyTcTyCon
       ; let res_kind   = tyConResKind tycon
             binders    = tyConBinders tycon
       ; traceTc "bindTyClTyVars" (ppr tycon_name $$ ppr binders)
       ; tcExtendTyVarEnv (binderVars binders) $
         thing_inside binders res_kind }

bindTyClTyVarsAndZonk :: Name -> ([TyConBinder] -> Kind -> TcM a) -> TcM a
-- Like bindTyClTyVars, but in addition
-- zonk the skolem TcTyVars of a PolyTcTyCon to TyVars
-- We always do this same zonking after a call to bindTyClTyVars, but
-- here we do it right away because there are no more unifications to come
bindTyClTyVarsAndZonk tycon_name thing_inside
  = bindTyClTyVars tycon_name $ \ tc_bndrs tc_kind ->
    do { (bndrs, kind) <- initZonkEnv NoFlexi $
          runZonkBndrT (zonkTyVarBindersX tc_bndrs) $ \ bndrs ->
            do { kind <- zonkTcTypeToTypeX tc_kind
               ; return (bndrs, kind) }
       ; thing_inside bndrs kind }


{- *********************************************************************
*                                                                      *
             Kind generalisation
*                                                                      *
********************************************************************* -}

zonkAndScopedSort :: [TcTyVar] -> TcM [TcTyVar]
zonkAndScopedSort spec_tkvs
  = do { spec_tkvs <- liftZonkM $ zonkTcTyVarsToTcTyVars spec_tkvs
         -- Zonk the kinds, to we can do the dependency analysis

       -- Do a stable topological sort, following
       -- Note [Ordering of implicit variables] in GHC.Rename.HsType
       ; return (scopedSort spec_tkvs) }

-- | Generalize some of the free variables in the given type.
-- All such variables should be *kind* variables; any type variables
-- should be explicitly quantified (with a `forall`) before now.
--
-- The WantedConstraints are un-solved kind constraints. Generally
-- they'll be reported as errors later, but meanwhile we refrain
-- from quantifying over any variable free in these unsolved
-- constraints. See Note [Failure in local type signatures].
--
-- But in all cases, generalize only those variables whose TcLevel is
-- strictly greater than the ambient level. This "strictly greater
-- than" means that you likely need to push the level before creating
-- whatever type gets passed here.
--
-- Any variable whose level is greater than the ambient level but is
-- not selected to be generalized will be promoted. (See [Promoting
-- unification variables] in "GHC.Tc.Solver" and Note [Recipe for
-- checking a signature].)
--
-- The resulting KindVar are the variables to quantify over, in the
-- correct, well-scoped order. They should generally be Inferred, not
-- Specified, but that's really up to the caller of this function.
kindGeneralizeSome :: SkolemInfo
                   -> WantedConstraints
                   -> TcType    -- ^ needn't be zonked
                   -> TcM [KindVar]
kindGeneralizeSome skol_info wanted kind_or_type
  = do { -- Use the "Kind" variant here, as any types we see
         -- here will already have all type variables quantified;
         -- thus, every free variable is really a kv, never a tv.
       ; dvs <- candidateQTyVarsOfKind kind_or_type
       ; filtered_dvs <- filterConstrainedCandidates wanted dvs
       ; traceTc "kindGeneralizeSome" $
         vcat [ text "type:" <+> ppr kind_or_type
              , text "dvs:" <+> ppr dvs
              , text "filtered_dvs:" <+> ppr filtered_dvs ]
       ; quantifyTyVars skol_info DefaultNonStandardTyVars filtered_dvs }

filterConstrainedCandidates
  :: WantedConstraints    -- Don't quantify over variables free in these
                          --   Not necessarily fully zonked
  -> CandidatesQTvs       -- Candidates for quantification
  -> TcM CandidatesQTvs
-- filterConstrainedCandidates removes any candidates that are free in
-- 'wanted'; instead, it promotes them.  This bit is very much like
-- decidePromotedTyVars in GHC.Tc.Solver, but constraints are so much
-- simpler in kinds, it is much easier here. (In particular, we never
-- quantify over a constraint in a type.)
filterConstrainedCandidates wanted dvs
  | isEmptyWC wanted   -- Fast path for a common case
  = return dvs
  | otherwise
  = do { wc_tvs <- liftZonkM $ zonkTyCoVarsAndFV (tyCoVarsOfWC wanted)
       ; let (to_promote, dvs') = partitionCandidates dvs (`elemVarSet` wc_tvs)
       ; _ <- promoteTyVarSet to_promote
       ; return dvs' }

-- |- Specialised version of 'kindGeneralizeSome', but with empty
-- WantedConstraints, so no filtering is needed
-- i.e.   kindGeneraliseAll = kindGeneralizeSome emptyWC
kindGeneralizeAll :: SkolemInfo -> TcType -> TcM [KindVar]
kindGeneralizeAll skol_info kind_or_type
  = do { traceTc "kindGeneralizeAll" (ppr kind_or_type)
       ; dvs <- candidateQTyVarsOfKind kind_or_type
       ; quantifyTyVars skol_info DefaultNonStandardTyVars dvs }

-- | Specialized version of 'kindGeneralizeSome', but where no variables
-- can be generalized, but perhaps some may need to be promoted.
-- Use this variant when it is unknowable whether metavariables might
-- later be constrained.
--
-- To see why this promotion is needed, see
-- Note [Recipe for checking a signature], and especially
-- Note [Promotion in signatures].
kindGeneralizeNone :: TcType  -- needn't be zonked
                   -> TcM ()
kindGeneralizeNone kind_or_type
  = do { traceTc "kindGeneralizeNone" (ppr kind_or_type)
       ; dvs <- candidateQTyVarsOfKind kind_or_type
       ; _ <- promoteTyVarSet (candidateKindVars dvs)
       ; return () }

{- Note [Levels and generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f x = e
with no type signature. We are currently at level i.
We must
  * Push the level to level (i+1)
  * Allocate a fresh alpha[i+1] for the result type
  * Check that e :: alpha[i+1], gathering constraint WC
  * Solve WC as far as possible
  * Zonking the result type alpha[i+1], say to beta[i-1] -> gamma[i]
  * Find the free variables with level > i, in this case gamma[i]
  * Skolemise those free variables and quantify over them, giving
       f :: forall g. beta[i-1] -> g
  * Emit the residual constraint wrapped in an implication for g,
    thus   forall g. WC

All of this happens for types too.  Consider
  f :: Int -> (forall a. Proxy a -> Int)

Note [Kind generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We do kind generalisation only at the outer level of a type signature.
For example, consider
  T :: forall k. k -> *
  f :: (forall a. T a -> Int) -> Int
When kind-checking f's type signature we generalise the kind at
the outermost level, thus:
  f1 :: forall k. (forall (a:k). T k a -> Int) -> Int  -- YES!
and *not* at the inner forall:
  f2 :: (forall k. forall (a:k). T k a -> Int) -> Int  -- NO!
Reason: same as for HM inference on value level declarations,
we want to infer the most general type.  The f2 type signature
would be *less applicable* than f1, because it requires a more
polymorphic argument.

NB: There are no explicit kind variables written in f's signature.
When there are, the renamer adds these kind variables to the list of
variables bound by the forall, so you can indeed have a type that's
higher-rank in its kind. But only by explicit request.

Note [Kinds of quantified type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcTyVarBndrsGen quantifies over a specified list of type variables,
*and* over the kind variables mentioned in the kinds of those tyvars.

Note that we must zonk those kinds (obviously) but less obviously, we
must return type variables whose kinds are zonked too. Example
    (a :: k7)  where  k7 := k9 -> k9
We must return
    [k9, a:k9->k9]
and NOT
    [k9, a:k7]
Reason: we're going to turn this into a for-all type,
   forall k9. forall (a:k7). blah
which the type checker will then instantiate, and instantiate does not
look through unification variables!

Hence using zonked_kinds when forming tvs'.

-}

-----------------------------------
etaExpandAlgTyCon :: TyConFlavour tc  -> SkolemInfo
                  -> [TcTyConBinder] -> Kind
                  -> TcM ([TcTyConBinder], Kind)
etaExpandAlgTyCon flav skol_info tcbs res_kind
  | needsEtaExpansion flav
  = splitTyConKind skol_info in_scope avoid_occs res_kind
  | otherwise
  = return ([], res_kind)
  where
    tyvars     = binderVars tcbs
    in_scope   = mkInScopeSetList tyvars
    avoid_occs = map getOccName tyvars

needsEtaExpansion :: TyConFlavour tc -> Bool
needsEtaExpansion NewtypeFlavour  = True
needsEtaExpansion DataTypeFlavour = True
needsEtaExpansion ClassFlavour    = True
needsEtaExpansion _               = False

splitTyConKind :: SkolemInfo
               -> InScopeSet
               -> [OccName]  -- Avoid these OccNames
               -> Kind       -- Must be zonked
               -> TcM ([TcTyConBinder], TcKind)
-- GADT decls can have a (perhaps partial) kind signature
--      e.g.  data T a :: * -> * -> * where ...
-- This function makes up suitable (kinded) TyConBinders for the
-- argument kinds.  E.g. in this case it might return
--   ([b::*, c::*], *)
-- Skolemises the type as it goes, returning skolem TcTyVars
-- Never emits constraints.
-- It's a little trickier than you might think: see Note [splitTyConKind]
-- See also Note [Datatype return kinds] in GHC.Tc.TyCl
splitTyConKind skol_info in_scope avoid_occs kind
  = do  { loc     <- getSrcSpanM
        ; new_uniqs <- getUniquesM
        ; rdr_env <- getLocalRdrEnv
        ; lvl     <- getTcLevel
        ; let new_occs = Inf.filter (\ occ ->
                  isNothing (lookupLocalRdrOcc rdr_env occ) &&
                  -- Note [Avoid name clashes for associated data types]
                  not (occ `elem` avoid_occs)) $ mkOccName tvName <$> allNameStrings
              subst = mkEmptySubst in_scope
              details = SkolemTv skol_info (pushTcLevel lvl) False
                        -- As always, allocate skolems one level in

              go occs uniqs subst acc kind
                = case splitPiTy_maybe kind of
                    Nothing -> (reverse acc, substTy subst kind)

                    Just (Anon arg af, kind')
                      -> assert (af == FTF_T_T) $
                         go occs' uniqs' subst' (tcb : acc) kind'
                      where
                        tcb    = Bndr tv AnonTCB
                        arg'   = substTy subst (scaledThing arg)
                        name   = mkInternalName uniq occ loc
                        tv     = mkTcTyVar name arg' details
                        subst' = extendSubstInScope subst tv
                        uniq:uniqs' = uniqs
                        Inf occ occs' = occs

                    Just (Named (Bndr tv vis), kind')
                      -> go occs uniqs subst' (tcb : acc) kind'
                      where
                        tcb           = Bndr tv' (NamedTCB vis)
                        tc_tyvar      = mkTcTyVar (tyVarName tv) (tyVarKind tv) details
                        (subst', tv') = substTyVarBndr subst tc_tyvar

        ; return (go new_occs new_uniqs subst [] kind) }

isAllowedDataResKind :: AllowedDataResKind -> Kind -> Bool
isAllowedDataResKind AnyTYPEKind  kind = isTypeLikeKind     kind
isAllowedDataResKind AnyBoxedKind kind = tcIsBoxedTypeKind  kind
isAllowedDataResKind LiftedKind   kind = tcIsLiftedTypeKind kind

-- | Checks that the return kind in a data declaration's kind signature is
-- permissible. There are three cases:
--
-- If dealing with a @data@, @newtype@, @data instance@, or @newtype instance@
-- declaration, check that the return kind is @Type@.
--
-- If the declaration is a @newtype@ or @newtype instance@ and the
-- @UnliftedNewtypes@ extension is enabled, this check is slightly relaxed so
-- that a return kind of the form @TYPE r@ (for some @r@) is permitted.
-- See @Note [Implementation of UnliftedNewtypes]@ in "GHC.Tc.TyCl".
--
-- If dealing with a @data family@ declaration, check that the return kind is
-- either of the form:
--
-- 1. @TYPE r@ (for some @r@), or
--
-- 2. @k@ (where @k@ is a bare kind variable; see #12369)
--
-- See also Note [Datatype return kinds] in "GHC.Tc.TyCl"
checkDataKindSig :: DataSort -> Kind  -- any arguments in the kind are stripped off
                 -> TcM ()
checkDataKindSig data_sort kind
  = do { dflags <- getDynFlags
       ; traceTc "checkDataKindSig" (ppr kind)
       ; checkTc (tYPE_ok dflags || is_kind_var)
                 (err_msg dflags) }
  where
    res_kind = snd (tcSplitPiTys kind)
       -- Look for the result kind after
       -- peeling off any foralls and arrows

    is_newtype :: Bool
    is_newtype =
      case data_sort of
        DataDeclSort     new_or_data -> new_or_data == NewType
        DataInstanceSort new_or_data -> new_or_data == NewType
        DataFamilySort               -> False

    is_datatype :: Bool
    is_datatype =
      case data_sort of
        DataDeclSort     DataType -> True
        DataInstanceSort DataType -> True
        _                         -> False

    is_data_family :: Bool
    is_data_family =
      case data_sort of
        DataDeclSort{}     -> False
        DataInstanceSort{} -> False
        DataFamilySort     -> True

    allowed_kind :: DynFlags -> AllowedDataResKind
    allowed_kind dflags
      | is_newtype && xopt LangExt.UnliftedNewtypes dflags
        -- With UnliftedNewtypes, we allow kinds other than Type, but they
        -- must still be of the form `TYPE r` since we don't want to accept
        -- Constraint or Nat.
        -- See Note [Implementation of UnliftedNewtypes] in GHC.Tc.TyCl.
      = AnyTYPEKind
      | is_data_family
        -- If this is a `data family` declaration, we don't need to check if
        -- UnliftedNewtypes is enabled, since data family declarations can
        -- have return kind `TYPE r` unconditionally (#16827).
      = AnyTYPEKind
      | is_datatype && xopt LangExt.UnliftedDatatypes dflags
        -- With UnliftedDatatypes, we allow kinds other than Type, but they
        -- must still be of the form `TYPE (BoxedRep l)`, so that we don't
        -- accept result kinds like `TYPE IntRep`.
        -- See Note [Implementation of UnliftedDatatypes] in GHC.Tc.TyCl.
      = AnyBoxedKind
      | otherwise
      = LiftedKind

    tYPE_ok :: DynFlags -> Bool
    tYPE_ok dflags = isAllowedDataResKind (allowed_kind dflags) res_kind

    -- In the particular case of a data family, permit a return kind of the
    -- form `:: k` (where `k` is a bare kind variable).
    is_kind_var :: Bool
    is_kind_var | is_data_family = isJust (getCastedTyVar_maybe res_kind)
                | otherwise      = False

    err_msg :: DynFlags -> TcRnMessage
    err_msg dflags =
      TcRnInvalidReturnKind data_sort (allowed_kind dflags) kind (ext_hint dflags)

    ext_hint dflags
      | isTypeLikeKind kind
      , is_newtype
      , not (xopt LangExt.UnliftedNewtypes dflags)
      = Just SuggestUnliftedNewtypes
      | tcIsBoxedTypeKind kind
      , is_datatype
      , not (xopt LangExt.UnliftedDatatypes dflags)
      = Just SuggestUnliftedDatatypes
      | otherwise
      = Nothing

-- | Checks that the result kind of a class is exactly `Constraint`, rejecting
-- type synonyms and type families that reduce to `Constraint`. See #16826.
checkClassKindSig :: Kind -> TcM ()
checkClassKindSig kind = checkTc (isConstraintKind kind) err_msg
  where
    err_msg :: TcRnMessage
    err_msg = TcRnClassKindNotConstraint kind

tcbVisibilities :: TyCon -> [Type] -> [TyConBndrVis]
-- Result is in 1-1 correspondence with orig_args
tcbVisibilities tc orig_args
  = go (tyConKind tc) init_subst orig_args
  where
    init_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfTypes orig_args))
    go _ _ []
      = []

    go fun_kind subst all_args@(arg : args)
      | Just (tcb, inner_kind) <- splitPiTy_maybe fun_kind
      = case tcb of
          Anon _ af           -> assert (af == FTF_T_T) $
                                 AnonTCB      : go inner_kind subst  args
          Named (Bndr tv vis) -> NamedTCB vis : go inner_kind subst' args
                 where
                    subst' = extendTCvSubst subst tv arg

      | not (isEmptyTCvSubst subst)
      = go (substTy subst fun_kind) init_subst all_args

      | otherwise
      = pprPanic "addTcbVisibilities" (ppr tc <+> ppr orig_args)


{- Note [splitTyConKind]
~~~~~~~~~~~~~~~~~~~~~~~~
Given
  data T (a::*) :: * -> forall k. k -> *
we want to generate the extra TyConBinders for T, so we finally get
  (a::*) (b::*) (k::*) (c::k)
The function splitTyConKind generates these extra TyConBinders from
the result kind signature.  The same function is also used by
kcCheckDeclHeader_sig to get the [TyConBinder] from the Kind of
the TyCon given in a standalone kind signature.  E.g.
  type T :: forall (a::*). * -> forall k. k -> *

We need to take care to give the TyConBinders
  (a) Uniques that are fresh: the TyConBinders of a TyCon
      must have distinct uniques.

  (b) Preferably, OccNames that are fresh. If we happen to re-use
      OccNames that are other TyConBinders, we'll get a TyCon with
      TyConBinders like [a_72, a_53]; same OccName, different Uniques.
      Then when pretty-printing (e.g. in GHCi :info) we'll see
          data T a a0
      whereas we'd prefer
          data T a b
      (NB: the tidying happens in the conversion to Iface syntax,
      which happens as part of pretty-printing a TyThing.)

      Using fresh OccNames is not essential; it's cosmetic.
      And also see Note [Avoid name clashes for associated data types].

For (a) perhaps surprisingly, duplicated uniques can happen, even if
we use fresh uniques for Anon arrows.  Consider
   data T :: forall k. k -> forall k. k -> *
where the two k's are identical even up to their uniques.  Surprisingly,
this can happen: see #14515, #19092,3,4.  Then if we use those k's in
as TyConBinders we'll get duplicated uniques.

For (b) we'd like to avoid OccName clashes with the tyvars declared by
the user before the "::"; in the above example that is 'a'.

It's reasonably easy to solve all this; just run down the list with a
substitution; hence the recursive 'go' function.  But for the Uniques
it has to be done.

Note [Avoid name clashes for associated data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider    class C a b where
               data D b :: * -> *
When typechecking the decl for D, we'll invent an extra type variable
for D, to fill out its kind.  Ideally we don't want this type variable
to be 'a', because when pretty printing we'll get
            class C a b where
               data D b a0
(NB: the tidying happens in the conversion to Iface syntax, which happens
as part of pretty-printing a TyThing.)

That's why we look in the LocalRdrEnv to see what's in scope. This is
important only to get nice-looking output when doing ":info C" in GHCi.
It isn't essential for correctness.


************************************************************************
*                                                                      *
             Partial signatures
*                                                                      *
************************************************************************

-}

tcHsPartialSigType
  :: UserTypeCtxt
  -> LHsSigWcType GhcRn       -- The type signature
  -> TcM ( [(Name, TcTyVar)]  -- Wildcards
         , Maybe TcType       -- Extra-constraints wildcard
         , [(Name,InvisTVBinder)] -- Original tyvar names, in correspondence with
                              --   the implicitly and explicitly bound type variables
         , TcThetaType        -- Theta part
         , TcType )           -- Tau part
-- See Note [Checking partial type signatures]
tcHsPartialSigType ctxt sig_ty
  | HsWC { hswc_ext  = sig_wcs, hswc_body = sig_ty } <- sig_ty
  , L _ (HsSig{sig_bndrs = hs_outer_bndrs, sig_body = body_ty}) <- sig_ty
  , (hs_ctxt, hs_tau) <- splitLHsQualTy body_ty
  = addSigCtxt ctxt sig_ty $
    do { mode <- mkHoleMode TypeLevel HM_Sig
       ; (outer_bndrs, (wcs, wcx, theta, tau))
            <- solveEqualities "tcHsPartialSigType" $
               -- See Note [Failure in local type signatures]
               bindNamedWildCardBinders sig_wcs             $ \ wcs ->
               bindOuterSigTKBndrs_Tv_M mode hs_outer_bndrs $
               do {   -- Instantiate the type-class context; but if there
                      -- is an extra-constraints wildcard, just discard it here
                    (theta, wcx) <- tcPartialContext mode hs_ctxt

                  ; ek <- newOpenTypeKind
                  ; tau <- -- Don't do (addTypeCtxt hs_tau) here else we get
                           --   In the type <blah>
                           --   In the type signature: foo :: <blah>
                           tc_check_lhs_type mode hs_tau ek

                  ; return (wcs, wcx, theta, tau) }

       ; traceTc "tcHsPartialSigType 2" empty
       ; outer_bndrs <- scopedSortOuter outer_bndrs
       ; let outer_tv_bndrs = outerTyVarBndrs outer_bndrs
       ; traceTc "tcHsPartialSigType 3" empty

         -- No kind-generalization here:
       ; kindGeneralizeNone (mkInvisForAllTys outer_tv_bndrs $
                             tcMkPhiTy theta $
                             tau)

       -- Spit out the wildcards (including the extra-constraints one)
       -- as "hole" constraints, so that they'll be reported if necessary
       -- See Note [Extra-constraint holes in partial type signatures]
       ; mapM_ emitNamedTypeHole wcs

         -- The "tau" from tcHsPartialSigType might very well have some foralls
         -- at the top, hidden behind a type synonym. Instantiate them! E.g.
         --    type T x = forall b. x -> b -> b
         --    f :: forall a. T (a,_)
         -- We must instantiate the `forall b` just as we do the `forall a`!
         -- Missing this led to #21667.
       ; (tv_prs', theta', tau) <- tcInstTypeBndrs tau

         -- We return a proper (Name,InvisTVBinder) environment, to be sure that
         -- we bring the right name into scope in the function body.
         -- Test case: partial-sigs/should_compile/LocalDefinitionBug
       ; let outer_bndr_names :: [Name]
             outer_bndr_names = hsOuterTyVarNames hs_outer_bndrs
             tv_prs :: [(Name,InvisTVBinder)]
             tv_prs = outer_bndr_names `zip` outer_tv_bndrs

       -- Zonk, so that any nested foralls can "see" their occurrences
       -- See Note [Checking partial type signatures], and in particular
       -- Note [Levels for wildcards]
       ; (tv_prs, theta, tau) <- liftZonkM $
         do { tv_prs <- mapSndM zonkInvisTVBinder (tv_prs ++ tv_prs')
            ; theta  <- mapM    zonkTcType        (theta ++ theta')
            ; tau    <- zonkTcType                tau
            ; return (tv_prs, theta, tau) }

      -- NB: checkValidType on the final inferred type will be
      --     done later by checkInferredPolyId.  We can't do it
      --     here because we don't have a complete type to check

       ; traceTc "tcHsPartialSigType" (ppr tv_prs)
       ; return (wcs, wcx, tv_prs, theta, tau) }

tcPartialContext :: TcTyMode -> Maybe (LHsContext GhcRn) -> TcM (TcThetaType, Maybe TcType)
tcPartialContext _ Nothing = return ([], Nothing)
tcPartialContext mode (Just (L _ hs_theta))
  | Just (hs_theta1, hs_ctxt_last) <- snocView hs_theta
  , L wc_loc ty@(HsWildCardTy _) <- ignoreParens hs_ctxt_last
  = do { wc_tv_ty <- setSrcSpanA wc_loc $
                     tcAnonWildCardOcc YesExtraConstraint mode ty constraintKind
       ; theta <- mapM (tc_lhs_pred mode) hs_theta1
       ; return (theta, Just wc_tv_ty) }
  | otherwise
  = do { theta <- mapM (tc_lhs_pred mode) hs_theta
       ; return (theta, Nothing) }

{- Note [Checking partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note is about tcHsPartialSigType.  See also
Note [Recipe for checking a signature]

When we have a partial signature like
   f :: forall a. a -> _
we do the following

* tcHsPartialSigType does not make quantified type (forall a. blah)
  and then instantiate it -- it makes no sense to instantiate a type
  with wildcards in it.  Rather, tcHsPartialSigType just returns the
  'a' and the 'blah' separately.

  Nor, for the same reason, do we push a level in tcHsPartialSigType.

* We instantiate 'a' to a unification variable, a TyVarTv, and /not/
  a skolem; hence the "_Tv" in bindExplicitTKBndrs_Tv.  Consider
    f :: forall a. a -> _
    g :: forall b. _ -> b
    f = g
    g = f
  They are typechecked as a recursive group, with monomorphic types,
  so 'a' and 'b' will get unified together.  Very like kind inference
  for mutually recursive data types (sans CUSKs or SAKS); see
  Note [Cloning for type variable binders]

* In GHC.Tc.Gen.Sig.tcUserSigType we return a PartialSig, which (unlike
  the companion CompleteSig) contains the original, as-yet-unchecked
  source-code LHsSigWcType

* Then, for f and g /separately/, we call tcInstSig, which in turn
  call tcHsPartialSig (defined near this Note).  It kind-checks the
  LHsSigWcType, creating fresh unification variables for each "_"
  wildcard.  It's important that the wildcards for f and g are distinct
  because they might get instantiated completely differently.  E.g.
     f,g :: forall a. a -> _
     f x = a
     g x = True
  It's really as if we'd written two distinct signatures.

* Nested foralls. See Note [Levels for wildcards]

* Just as for ordinary signatures, we must solve local equalities and
  zonk the type after kind-checking it, to ensure that all the nested
  forall binders can "see" their occurrences

  Just as for ordinary signatures, this zonk also gets any Refl casts
  out of the way of instantiation.  Example: #18008 had
       foo :: (forall a. (Show a => blah) |> Refl) -> _
  and that Refl cast messed things up.  See #18062.

Note [Levels for wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
     f :: forall b. (forall a. a -> _) -> b
We do /not/ allow the "_" to be instantiated to 'a'; although we do
(as before) allow it to be instantiated to the (top level) 'b'.
Why not?  Suppose
   f x = (x True, x 'c')

During typecking the RHS we must instantiate that (forall a. a -> _),
so we must know /precisely/ where all the a's are; they must not be
hidden under (possibly-not-yet-filled-in) unification variables!

We achieve this as follows:

- For /named/ wildcards such sas
     g :: forall b. (forall la. a -> _x) -> b
  there is no problem: we create them at the outer level (ie the
  ambient level of the signature itself), and push the level when we
  go inside a forall.  So now the unification variable for the "_x"
  can't unify with skolem 'a'.

- For /anonymous/ wildcards, such as 'f' above, we carry the ambient
  level of the signature to the hole in the TcLevel part of the
  mode_holes field of TcTyMode.  Then, in tcAnonWildCardOcc we us that
  level (and /not/ the level ambient at the occurrence of "_") to
  create the unification variable for the wildcard.  That is the sole
  purpose of the TcLevel in the mode_holes field: to transport the
  ambient level of the signature down to the anonymous wildcard
  occurrences.

Note [Extra-constraint holes in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f :: (_) => a -> a
  f x = ...

* The renamer leaves '_' untouched.

* Then, in tcHsPartialSigType, we make a new hole TcTyVar, in
  tcWildCardBinders.

* GHC.Tc.Gen.Bind.chooseInferredQuantifiers fills in that hole TcTyVar
  with the inferred constraints, e.g. (Eq a, Show a)

* GHC.Tc.Errors.mkHoleError finally reports the error.

An annoying difficulty happens if there are more than 64 inferred
constraints. Then we need to fill in the TcTyVar with (say) a 70-tuple.
Where do we find the TyCon?  For good reasons we only have constraint
tuples up to 62 (see Note [How tuples work] in GHC.Builtin.Types).  So how
can we make a 70-tuple?  This was the root cause of #14217.

It's incredibly tiresome, because we only need this type to fill
in the hole, to communicate to the error reporting machinery.  Nothing
more.  So I use a HACK:

* I make an /ordinary/ tuple of the constraints, in
  GHC.Tc.Gen.Bind.chooseInferredQuantifiers. This is ill-kinded because
  ordinary tuples can't contain constraints, but it works fine. And for
  ordinary tuples we don't have the same limit as for constraint
  tuples (which need selectors and an associated class).

* Because it is ill-kinded (unifying something of kind Constraint with
  something of kind Type), it should trip an assert in writeMetaTyVarRef.

Result works fine, but it may eventually bite us.

See also Note [Do not simplify ConstraintHoles] in GHC.Tc.Solver for
information about how these are printed.

************************************************************************
*                                                                      *
      Pattern signatures (i.e signatures that occur in patterns)
*                                                                      *
********************************************************************* -}

tcHsPatSigType :: UserTypeCtxt
               -> HoleMode -- HM_Sig when in a SigPat, HM_TyAppPat when in a ConPat checking type applications.
               -> HsPatSigType GhcRn          -- The type signature
               -> ContextKind                -- What kind is expected
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                              -- the scoped type variables
                      , TcType)       -- The type
-- Used for type-checking type signatures in
-- (a) patterns           e.g  f (x::Int) = e
-- (b) RULE forall bndrs  e.g. forall (x::Int). f x = x
-- See Note [Pattern signature binders and scoping] in GHC.Hs.Type
--
-- This may emit constraints
-- See Note [Recipe for checking a signature]
tcHsPatSigType ctxt hole_mode
  (HsPS { hsps_ext  = HsPSRn { hsps_nwcs = sig_wcs, hsps_imp_tvs = sig_ns }
        , hsps_body = hs_ty })
  ctxt_kind
  = tc_type_in_pat ctxt hole_mode hs_ty sig_wcs sig_ns ctxt_kind


-- Typecheck type patterns, in data constructor patterns, e.g
--    f (MkT @a @(Maybe b) ...) = ...
--
-- We have two completely separate typing rules,
--   one for binder type patterns  (handled by `tc_bndr_in_pat`)
--   one for unifier type patterns (handled by `tc_type_in_pat`)
-- The two cases are distinguished by `tyPatToBndr`.
-- See Note [Type patterns: binders and unifiers]
tcHsTyPat :: HsTyPat GhcRn               -- The type pattern
          -> Kind                        -- What kind is expected
          -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                 , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                         -- the scoped type variables
                 , TcType)               -- The type
tcHsTyPat hs_pat@(HsTP{hstp_ext = hstp_rn, hstp_body = hs_ty}) expected_kind
  = case tyPatToBndr hs_pat of
    Nothing   -> tc_unif_in_pat hs_ty wcs all_ns (TheKind expected_kind)
    Just bndr -> tc_bndr_in_pat bndr  wcs imp_ns  expected_kind
  where
    all_ns = imp_ns ++ exp_ns
    HsTPRn{hstp_nwcs = wcs, hstp_imp_tvs = imp_ns, hstp_exp_tvs = exp_ns} = hstp_rn
    tc_unif_in_pat = tc_type_in_pat TypeAppCtxt HM_TyAppPat

-- `tc_bndr_in_pat` is used in type patterns to handle the binders case.
-- See Note [Type patterns: binders and unifiers]
tc_bndr_in_pat :: HsTyVarBndr flag GhcRn
               -> [Name]  -- All named wildcards in type
               -> [Name]  -- Implicit (but not explicit) binders in type
               -> Kind    -- Expected kind
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                              -- the scoped type variables
                      , TcType)               -- The type
tc_bndr_in_pat bndr wcs imp_ns expected_kind = do
  let HsTvb { tvb_var = bvar, tvb_kind = bkind } = bndr
  traceTc "tc_bndr_in_pat 1" (ppr expected_kind)
  name <- tcHsBndrVarName bvar
  tv <- newPatTyVar name expected_kind
  case bkind of
    HsBndrNoKind _ ->
      pure ([], mk_tvb_pairs bndr tv, mkTyVarTy tv)
    HsBndrKind _ ki -> do
      tkv_prs <- mapM new_implicit_tv imp_ns
      wcs <- addTypeCtxt ki              $
             solveEqualities "tc_bndr_in_pat" $
               -- See Note [Failure in local type signatures]
               -- and c.f #16033
             bindNamedWildCardBinders wcs $ \ wcs ->
             tcExtendNameTyVarEnv tkv_prs $
             do { tcHsTvbKind bvar ki expected_kind
                ; pure wcs }

      mapM_ emitNamedTypeHole wcs

      traceTc "tc_bndr_in_pat 2" $ vcat
        [ text "expected_kind" <+> ppr expected_kind
        , text "wcs" <+> ppr wcs
        , text "(name,tv)" <+>  ppr (name,tv)
        , text "tkv_prs" <+> ppr tkv_prs]

      let tvb_prs = mk_tvb_pairs bndr tv
      pure (wcs, tvb_prs ++ tkv_prs, mkTyVarTy tv)
  where
    new_implicit_tv name
      = do { kind <- newMetaKindVar
           ; tv   <- newPatTyVar name kind
             -- NB: tv's Name is fresh
           ; return (name, tv) }

-- * In type patterns `tc_type_in_pat` is used to handle the unifiers case.
--   See Note [Type patterns: binders and unifiers]
--
-- * In patterns `tc_type_in_pat` is used to check pattern signatures.
tc_type_in_pat :: UserTypeCtxt
               -> HoleMode -- HM_Sig when in a SigPat, HM_TyAppPat when in a ConPat checking type applications.
               -> LHsType GhcRn          -- The type in pattern
               -> [Name]                 -- All named wildcards in type
               -> [Name]                 -- All binders in type
               -> ContextKind                -- What kind is expected
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                              -- the scoped type variables
                      , TcType)       -- The type
tc_type_in_pat ctxt hole_mode hs_ty wcs ns ctxt_kind
  = addSigCtxt ctxt hs_ty $
    do { tkv_prs <- mapM new_implicit_tv ns
       ; mode <- mkHoleMode TypeLevel hole_mode
       ; (wcs, ty)
            <- addTypeCtxt hs_ty                $
               solveEqualities "tc_type_in_pat" $
                 -- See Note [Failure in local type signatures]
                 -- and c.f #16033
               bindNamedWildCardBinders wcs $ \ wcs ->
               tcExtendNameTyVarEnv tkv_prs $
               do { ek <- newExpectedKind ctxt_kind
                  ; ty <- tc_check_lhs_type mode hs_ty ek
                  ; return (wcs, ty) }

        ; mapM_ emitNamedTypeHole wcs

          -- ty might have tyvars that are at a higher TcLevel (if hs_ty
          -- contains a forall). Promote these.
          -- Ex: f (x :: forall a. Proxy a -> ()) = ... x ...
          -- When we instantiate x, we have to compare the kind of the argument
          -- to a's kind, which will be a metavariable.
          -- kindGeneralizeNone does this:
        ; kindGeneralizeNone ty
        ; ty <- liftZonkM $ zonkTcType ty
        ; checkValidType ctxt ty

        ; traceTc "tc_type_in_pat" (ppr tkv_prs)
        ; return (wcs, tkv_prs, ty) }
  where
    new_implicit_tv name
      = do { kind <- newMetaKindVar
           ; tv   <- case ctxt of
                       RuleSigCtxt rname _  -> do
                        skol_info <- mkSkolemInfo (RuleSkol rname)
                        newSkolemTyVar skol_info name kind
                       _              -> newPatTyVar name kind
                       -- See Note [Typechecking pattern signature binders]
             -- NB: tv's Name may be fresh (in the case of newPatTyVar)
           ; return (name, tv) }

-- See Note [Type patterns: binders and unifiers]
tyPatToBndr :: HsTyPat GhcRn -> Maybe (HsTyVarBndr () GhcRn)
tyPatToBndr HsTP{hstp_body = (L _ hs_ty)} = go hs_ty where
  go :: HsType GhcRn -> Maybe (HsTyVarBndr () GhcRn)
  go (HsParTy _ (L _ ty)) = go ty
  go (HsKindSig _ (L _ ty) ki) = do
    bvar <- go_bvar ty
    let bkind = HsBndrKind noExtField ki
    Just (HsTvb noAnn () bvar bkind)
  go ty = do
    bvar <- go_bvar ty
    let bkind = HsBndrNoKind noExtField
    Just (HsTvb noAnn () bvar bkind)

  go_bvar :: HsType GhcRn -> Maybe (HsBndrVar GhcRn)
  go_bvar (HsTyVar _ _ name)
    | isTyVarName (unLoc name)
    = Just (HsBndrVar noExtField name)
  go_bvar (HsWildCardTy _)
    = Just (HsBndrWildCard noExtField)
  go_bvar _ = Nothing

{- Note [Type patterns: binders and unifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type pattern, of type `HsTyPat`, represents a type argument in a data
constructor pattern.  For example
    f (MkT @a @(Maybe b) p q) = ...
Here the `@a` and `@(Maybe b)` are type patterns.  In general, then a
`HsTyPat` is represented by a `HsType`.

However, for /typechecking/ purposes (only) we distinguish two categories of
type pattern:
* Binder type patterns
* Unifier type patterns

Binder type patterns are a subset of type patterns described by the following grammar:

  bvar ::= tv  | '_'        -- type variable or wildcard
  tp_bndr ::=
      bvar                  -- plain binder
    | bvar '::' kind        -- binder with kind annotation
    | '(' tp_bndr ')'       -- parentheses

This subset of HsTyPat can be represented by HsTyVarBndr, which is also used
in foralls and type declaration headers.

Unifier type patterns include all other forms of type patterns, such as `Maybe x`.
This distinction allows the typechecker to accept more programs.
Consider this example from #18986:

  data T where
    MkT :: forall (f :: forall k. k -> Type).
      f Int -> f Maybe -> T

  k :: T -> ()
  k (MkT @f (x :: f Int) (y :: f Maybe)) = ()

In general case (if we treat `f` as a unifier) we would create a metavariable for its kind:
  f :: kappa
Checking `x :: f Int` would unify
  kappa := Type -> Type
and then checking `y :: f Maybe` would unify
  kappa := (Type -> Type) -> Type
leading to a type error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’

However, `@f` is a simple type variable binder, we don't need a metavariable for its kind, we
can add it directly to the context with its polymorphic kind:
  f :: forall k . k -> Type
This way both `f Int` and `f Maybe` can be accepted because `k` can be instantiated differently at
each call site.

Note [Typechecking pattern signature binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Type variables in the type environment] in GHC.Tc.Utils.
Consider

  data T where
    MkT :: forall a. a -> (a -> Int) -> T

  f :: T -> ...
  f (MkT x (f :: b -> c)) = <blah>

Here
 * The pattern (MkT p1 p2) creates a *skolem* type variable 'a_sk',
   It must be a skolem so that it retains its identity, and
   GHC.Tc.Errors.getSkolemInfo can thereby find the binding site for the skolem.

 * The type signature pattern (f :: b -> c) makes fresh meta-tyvars
   beta and gamma (TauTvs), and binds "b" :-> beta, "c" :-> gamma in the
   environment

 * Then unification makes beta := a_sk, gamma := Int
   That's why we must make beta and gamma a MetaTv,
   not a SkolemTv, so that it can unify to a_sk (or Int, respectively).

 * Finally, in '<blah>' we have the envt "b" :-> beta, "c" :-> gamma,
   so we return the pairs ("b" :-> beta, "c" :-> gamma) from tcHsPatSigType,

Another example (#13881):
   fl :: forall (l :: [a]). Sing l -> Sing l
   fl (SNil :: Sing (l :: [y])) = SNil
When we reach the pattern signature, 'l' is in scope from the
outer 'forall':
   "a" :-> a_sk :: *
   "l" :-> l_sk :: [a_sk]
We make up a fresh meta-TauTv, y_sig, for 'y', and kind-check
the pattern signature
   Sing (l :: [y])
That unifies y_sig := a_sk.  We return from tcHsPatSigType with
the pair ("y" :-> y_sig).

For RULE binders, though, things are a bit different (yuk).
  RULE "foo" forall (x::a) (y::[a]).  f x y = ...
Here this really is the binding site of the type variable so we'd like
to use a skolem, so that we get a complaint if we unify two of them
together.  Hence the new_implicit_tv function in tcHsPatSigType.


************************************************************************
*                                                                      *
        Checking kinds
*                                                                      *
************************************************************************

-}

unifyKinds :: [LHsType GhcRn] -> [(TcType, TcKind)] -> TcM ([TcType], TcKind)
unifyKinds rn_tys act_kinds
  = do { kind <- newMetaKindVar
       ; let check rn_ty (ty, act_kind)
               = checkExpectedKind (unLoc rn_ty) ty act_kind kind
       ; tys' <- zipWithM check rn_tys act_kinds
       ; return (tys', kind) }

{-
************************************************************************
*                                                                      *
        Sort checking kinds
*                                                                      *
************************************************************************

tcLHsKindSig converts a user-written kind to an internal, sort-checked kind.
It does sort checking and desugaring at the same time, in one single pass.
-}

tcLHsKindSig :: UserTypeCtxt -> LHsKind GhcRn -> TcM Kind
tcLHsKindSig ctxt hs_kind
  = tc_lhs_kind_sig kindLevelMode ctxt hs_kind

tc_lhs_kind_sig :: TcTyMode -> UserTypeCtxt -> LHsKind GhcRn -> TcM Kind
tc_lhs_kind_sig mode ctxt hs_kind
-- See  Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
-- Result is zonked
  = do { kind <- addErrCtxt (text "In the kind" <+> quotes (ppr hs_kind)) $
                 solveEqualities "tcLHsKindSig" $
                 tc_check_lhs_type mode hs_kind liftedTypeKind
       ; traceTc "tcLHsKindSig" (ppr hs_kind $$ ppr kind)
       -- No generalization:
       ; kindGeneralizeNone kind
       ; kind <- liftZonkM $ zonkTcType kind
         -- This zonk is very important in the case of higher rank kinds
         -- E.g. #13879    f :: forall (p :: forall z (y::z). <blah>).
         --                          <more blah>
         --      When instantiating p's kind at occurrences of p in <more blah>
         --      it's crucial that the kind we instantiate is fully zonked,
         --      else we may fail to substitute properly

       ; checkValidType ctxt kind
       ; traceTc "tcLHsKindSig2" (ppr kind)
       ; return kind }

promotionErr :: Name -> PromotionErr -> TcM a
promotionErr name err
  = failWithTc $ TcRnUnpromotableThing name err

{-
************************************************************************
*                                                                      *
          Error messages and such
*                                                                      *
************************************************************************
-}


-- | Make an appropriate message for an error in a function argument.
-- Used for both expressions and types.
funAppCtxt :: (Outputable fun, Outputable arg) => fun -> arg -> Int -> SDoc
funAppCtxt fun arg arg_no
  = hang (hsep [ text "In the", speakNth arg_no, text "argument of",
                    quotes (ppr fun) <> text ", namely"])
       2 (quotes (ppr arg))

-- | Add a "In the data declaration for T" or some such.
addTyConFlavCtxt :: Name -> TyConFlavour tc -> TcM a -> TcM a
addTyConFlavCtxt name flav
  = addErrCtxt $ hsep [ text "In the", ppr flav
                      , text "declaration for", quotes (ppr name) ]

{-
************************************************************************
*                                                                      *
          Utils for constructing TyLit
*                                                                      *
************************************************************************
-}


tyLitFromLit :: HsLit GhcRn -> Maybe (HsTyLit GhcRn)
tyLitFromLit (HsString x str) = Just (HsStrTy x str)
tyLitFromLit (HsMultilineString x str) = Just (HsStrTy x str)
tyLitFromLit (HsChar x char) = Just (HsCharTy x char)
tyLitFromLit _ = Nothing

tyLitFromOverloadedLit :: OverLitVal -> Maybe (HsTyLit GhcRn)
tyLitFromOverloadedLit (HsIntegral n) = Just $ HsNumTy NoSourceText (il_value n)
tyLitFromOverloadedLit (HsIsString _ s) = Just $ HsStrTy NoSourceText s
tyLitFromOverloadedLit HsFractional{} = Nothing
