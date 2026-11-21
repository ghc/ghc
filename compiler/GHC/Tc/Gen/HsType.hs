{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecursiveDo        #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typechecking user-specified @MonoTypes@
module GHC.Tc.Gen.HsType (
        -- Type signatures
        kcClassSigType, tcClassSigType,
        tcHsSigType, tcHsSigWcType,
        tcHsPartialSigType, PendingSigCalls(..),
        tcStandaloneKindSig,
        funsSigCtxt, addSigCtxt, pprSigCtxt,

        tcHsClsInstType,
        tcDefaultDeclClass, tcHsDeriv, tcDerivStrategy,
        tcHsTypeApp,
        UserTypeCtxt(..),
        bindImplicitTKBndrs_Tv, bindImplicitTKBndrs_Skol,
            bindImplicitTKBndrs_Q_Tv, bindImplicitTKBndrs_Q_Skol,
        bindExplicitTKBndrs_Tv, bindExplicitTKBndrs_Skol,
            bindExplicitTKBndrs_Q_Tv, bindExplicitTKBndrs_Q_Skol,

        bindOuterFamEqnTKBndrs_Q_Tv, bindOuterFamEqnTKBndrs,
        tcOuterTKBndrs, scopedSortOuter, outerTyVars, outerTyVarBndrs,
        tcGadtConTyVarBndrs, tcConDeclTelescope, tcCheckRetainedConType,
        bindOuterSigTKBndrs_Tv,
        tcExplicitTKBndrs, tcRuntimeInvisForAllTelescopes,
        tcCheckRuntimeForAllMatchability,
        bindNamedWildCardBinders,

        -- Type checking type and class decls, and instances thereof
        bindTyClTyVars, bindTyClTyVarsAndZonk,
        tcFamTyPats, tcInferredMatchabilityFamPats, matchabilityArrow, tcCheckMatchabilityArrow,
        inferredMatchabilityIndex, familyKindUsesMatchability, tcCheckMatchabilitySynRhs,
        synonymCallbackIndex, tcCheckSynonymMetadata,
        maybeEtaExpandAlgTyCon, tcbVisibilities,
        etaExpandAlgTyCon,

          -- tyvars
        zonkAndScopedSort,

        -- Kind-checking types
        -- No kind generalisation, no checkValidType
        InitialKindStrategy(..),
        SAKS_or_CUSK(..),
        ContextKind(..),
        kcDeclHeader, kcInferSumPayloadHeader, checkForDuplicateScopedTyVars,
        tcHsLiftedType,   tcHsOpenType,
        tcHsLiftedTypeNC, tcHsOpenTypeNC,
        tcInferLHsType, tcInferLHsTypeKind, tcInferLHsTypeUnsaturated,
        tcCheckLHsTypeInContext, tcCheckFamilyRhs, tcCheckInferredFamilyRhs, tcCheckDatatypeField, tcCheckLHsType,
        tcHsContext, tcLHsPredType,

        kindGeneralizeAll,

        -- Sort-checking kinds
        tcLHsKindSig, tcLHsFamilyResultKindSig, checkDataKindSig, DataSort(..),
        checkClassKindSig,

        -- Modifiers
        tcModifiersAndWarn, tcMult, tcConArrowMult, tcModifiersMult,

        -- Pattern type signatures
        tcHsPatSigType, tcHsTyPat, tcRuleBndrSig,
        HoleMode(..),

        -- Utils
        tyLitFromOverloadedLit,

   ) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))

import GHC.Prelude hiding ( head, init, last, tail )

import GHC.Hs.Type (HsTypeGhcRnExt(..))
import GHC.Hs
import GHC.Rename.Utils
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.BasicTypes (TcTyThing(ATcTypeLet, ATcCaseLet))
import GHC.Tc.Types.Origin
import GHC.Tc.Types.LclEnv
import GHC.Tc.Types.ErrCtxt
import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType
import GHC.Tc.Validity
import GHC.Tc.Gen.OpenTypeEval (instantiateOpenTypeFunction, observeOpenType, openTypeFailureReason)
import GHC.Tc.Gen.SourceTable (recordSourceUses, recordUnfoldedSourceUses, sourceTableFamilyUse, sourceTableTypeUses,
                              sourceCaseKindUse, sourceTableReferenceUse, indexedProjectionSource, isSourceConstructorCase, monomorphicEnumCaseArity, sourceCaseVisibleArity, sourceTableOwner, sourcePairBuilderField,
                              lookupObservedPairField, lookupPairObserver, lookupPairRecipe, canObservePairSource,
                              lookupSourceBinaryMethod, lookupSourceListFoldMethod, recordSourceMethodUse)
import {-# SOURCE #-} GHC.Tc.Gen.Splice (tcPrepareClosedApplication, tcPrepareClosedFunctionValue, tcPrepareClosedResult, tcPrepareClosedObservation, tcCheckClosedCallContext, tcPreflightClosedCall, tcDecodeClosedCallOperand, tcHasClosedCallExecutable)
import GHC.Tc.Utils.Unify
import GHC.Tc.Instance.Family (tcGetFamInstEnvs)
import GHC.Iface.Env (lookupOrig)
import GHC.IfaceToCore
import GHC.Tc.Solver
import GHC.Tc.Zonk.Type
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Instantiate ( tcInstInvisibleTyBinders, tcInstInvisibleTyBindersN,
                                  tcInstInvisibleTyBinder, tcSkolemiseInvisibleBndrs,
                                  tcInstTypeBndrs, tcInstSkolTyVarBndrsX,
                                  tcRuntimeForAllBndrs )
import GHC.Tc.Zonk.TcType

import GHC.Core.Reduction (reductionReducedType)
import GHC.Core.TyCon
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Core.FamInstEnv (familyNameInstances, famInstTyCon, lookupFamInstEnv, fim_instance, reduceTyFamApp_maybe)
import qualified GHC.Core as Core
import GHC.Core.Dependent ( hasNativeDependentExpr )
import GHC.Core.Predicate ( getClassPredTys_maybe, isCTupleClass )
import GHC.Core.Type
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Unify ( BindFlag(..), tcMatchTy, tcMatchTyX, tcMatchTyX_BM )

import GHC.Builtin( allNameStrings )
import GHC.Builtin.Modules (gHC_INTERNAL_TYPENATS)
import GHC.Builtin.KnownKeys
import GHC.Builtin.KnownOccs (typeRepIdOcc)
import GHC.Builtin.WiredIn.Types
import GHC.Builtin.WiredIn.Prim

import GHC.Types.Error
import GHC.Types.Id ( isClassOpId_maybe, mkLocalId, mkSysLocalM, sourceTableId_maybe, sourceTableKeepsExecutable )
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.Name.Set (NameSet, mkNameSet, unionNameSet, emptyNameSet, elemNameSet, extendNameSet)
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Var.Env
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM
import GHC.Types.Unique.Supply

import GHC.Unit.Module (mkModule, mkModuleName, moduleName, moduleNameString, moduleUnit, stringToUnit, unitString)

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Driver.DynFlags
import GHC.Driver.Env (hsc_units)
import GHC.Driver.Ppr (showPpr, showSDocForUser)
import GHC.Driver.Session (GeneralFlag(Opt_SolveConstantDicts))
import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.FastString
import GHC.Data.List.Infinite ( Infinite (..) )
import qualified GHC.Data.List.Infinite as Inf
import GHC.Data.List.SetOps
import GHC.Data.Maybe
import GHC.Data.Bag( unitBag, mapBagM_ )

import Data.Function ( on )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.List ( intercalate, mapAccumL )
import Control.Monad
import Data.Data (Data, cast, gmapQ)
import Data.Tuple( swap )
import Text.Read (readMaybe)

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

addSigCtxt :: UserTypeCtxt -> UserSigType -> TcM a -> TcM a
addSigCtxt ctxt hs_ty thing_inside
  = setSrcSpan l $
    addErrCtxt (UserSigCtxt ctxt hs_ty) $
    thing_inside
  where
    l = case hs_ty of
      UserLHsSigType ty -> getLocA ty
      UserLHsType ty    -> getLocA ty

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

tcHsSigWcType :: Bool -> UserTypeCtxt -> LHsSigWcType GhcRn -> TcM Type
-- This one is used when we have a LHsSigWcType, but in
-- a place where wildcards aren't allowed. The renamer has
-- already checked this, so we can simply ignore it.
-- See Note [Exported function-valued retained signatures].
tcHsSigWcType allow_function ctxt sig_ty =
  tc_hs_sig_type allow_function ctxt (dropWildCards sig_ty)

kcClassSigType :: [LocatedN Name] -> LHsSigType GhcRn -> TcM ()
-- This is a special form of tcClassSigType that is used during the
-- kind-checking phase to infer the kind of class variables. Cf. tc_lhs_sig_type.
-- Importantly, this does *not* kind-generalize. Consider
--   class SC f where
--     meth :: forall a (x :: f a). Proxy x -> ()
-- When instantiating Proxy with kappa, we must unify kappa := f a. But we're
-- still working out the kind of f, and thus f a will have a coercion in it.
-- Coercions may block unification (Note [Equalities with heterogeneous kinds] in
-- GHC.Tc.Solver.Equality, wrinkle (EIK2)) and so we fail to unify. If we try to
-- kind-generalize, we'll end up promoting kappa to the top level (because
-- kind-generalization is normally done right before adding a binding to the context),
-- and then we can't set kappa := f a, because a is local.
kcClassSigType names
    sig_ty@(L _ (HsSig { sig_bndrs = hs_outer_bndrs, sig_body = hs_ty }))
  = addSigCtxt (funsSigCtxt names) (UserLHsSigType sig_ty) $
    do { _ <- bindOuterSigTKBndrs_Tv hs_outer_bndrs    $
              tcCheckLHsType hs_ty liftedTypeKind
       ; return () }

tcClassSigType :: [LocatedN Name] -> LHsSigType GhcRn -> TcM Type
-- Does not do validity checking
tcClassSigType names sig_ty
  = addSigCtxt sig_ctxt (UserLHsSigType sig_ty) $
    do { skol_info <- mkSkolemInfo skol_info_anon
       ; (implic, ty) <- tc_lhs_sig_type Nothing skol_info sig_ty (TheKind liftedTypeKind)
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
tcHsSigType = tc_hs_sig_type False

-- | Check a signature with explicit permission for retained function domains.
tc_hs_sig_type :: Bool -> UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
tc_hs_sig_type allow_function ctxt sig_ty
  = addSigCtxt ctxt (UserLHsSigType sig_ty) $
    do { traceTc "tcHsSigType {" (ppr sig_ty)
       ; skol_info <- mkSkolemInfo skol_info
          -- Generalise here: see Note [Kind generalisation]
       ; let retained_sig = case ctxt of
               FunSigCtxt{} -> Just allow_function
               _ -> Nothing
       ; (implic, ty) <- tc_lhs_sig_type retained_sig skol_info sig_ty (expectedKindInCtxt ctxt)

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

tc_lhs_sig_type :: Maybe Bool -> SkolemInfo -> LHsSigType GhcRn
               -> ContextKind -> TcM (Implication, TcType)
-- Nothing selects the ordinary type checker. Just selects the retained
-- signature walker and supplies its function-domain permission.
-- Kind-checks/desugars an 'LHsSigType',
--   solve equalities,
--   and then kind-generalizes.
-- This will never emit constraints, as it uses solveEqualities internally.
-- No validity checking or zonking
-- Returns also an implication for the unsolved constraints
tc_lhs_sig_type retained_sig skol_info full_hs_ty@(L loc (HsSig { sig_bndrs = hs_outer_bndrs
                                                   , sig_body = hs_ty })) ctxt_kind
  = setSrcSpanA loc $
    do { (tc_lvl, wanted, (exp_kind, outer_ma, (outer_bndrs, ty)))
              <- pushLevelAndSolveKindArrowsX "tc_lhs_sig_type" $
                 -- See Note [Failure in local type signatures]
                 do { exp_kind <- newExpectedKind ctxt_kind
                          -- See Note [Escaping kind in type signatures]
                    ; outer_ma <- tcForAllMatchability (hsOuterMatchability hs_outer_bndrs)
                    ; let binder_mode = smVanilla
                            { sm_clone = False
                            , sm_tvtv = SMDSkolemTv skol_info
                            , sm_sig_kind_arrows = True }
                    ; stuff <- tcOuterTKBndrsX binder_mode skol_info hs_outer_bndrs $
                               (case retained_sig of
                                  Just allow_function -> tcCheckRetainedSigType allow_function skol_info
                                  Nothing -> tcCheckLHsType) hs_ty exp_kind
                    ; return (exp_kind, outer_ma, stuff) }

       -- Default any unconstrained variables free in the kind
       -- See Note [Escaping kind in type signatures]
       ; exp_kind_dvs <- candidateQTyVarsOfType exp_kind
       ; doNotQuantifyTyVars exp_kind_dvs (err_ctx exp_kind)

       ; traceTc "tc_lhs_sig_type" (ppr hs_outer_bndrs $$ ppr outer_bndrs)
       ; outer_bndrs <- scopedSortOuter outer_bndrs

       ; let outer_tv_bndrs = map (fromMaybe unmatchableDataConTy outer_ma,) (outerTyVarBndrs outer_bndrs)
             ty1 = mkInvisForAllTys outer_tv_bndrs ty
       ; kvs <- kindGeneralizeSome skol_info wanted ty1

       -- Build an implication for any as-yet-unsolved kind equalities
       -- See Note [Skolem escape in type signatures]
       ; implic <- buildTvImplication (getSkolemInfo skol_info) kvs tc_lvl wanted

       ; return (implic, mkInfForAllTys (map (unmatchableDataConTy,) kvs) ty1) }
  where
    err_ctx exp_kind tidy_env
      = do { (tidy_env2, exp_kind) <- zonkTidyTcType tidy_env exp_kind
           ; return (tidy_env2, UninfTyCtx_Sig exp_kind full_hs_ty) }

-- | Resolve free runtime Names to their existing lexical index Ids.
-- See Note [Local runtime indices in signatures] and
-- Note [Runtime indices in visible type arguments] in GHC.Rename.HsType.
tcRetainedAmbientIndices :: RetainedQuantificationError -> LHsType GhcRn -> TcM (NameEnv Id)
tcRetainedAmbientIndices invalid_reason hs_ty
  = do { experimental <- xoptM LangExt.ExperimentalDependentTypes
       ; if experimental
         then mkNameEnv . catMaybes <$> mapM check_index (ambient_names hs_ty)
         else return emptyNameEnv }
  where
    check_index (span, name) = setSrcSpan span $
      do { thing <- tcLookupLcl_maybe name
         ; case thing of
             Just ATyVar{} -> return Nothing
             Just (ATcId { tct_id = index }) ->
               do { index <- liftZonkM (zonkId index)
                  ; unless (not (isExternalName name)
                            && isNativeOpaqueValueId index) invalid_index
                  ; level <- getThLevel
                  ; case level of
                      Comp -> return ()
                      _ -> failWithTc $ TcRnRetainedQuantificationError RetainedStaging
                  ; return (Just (name, index)) }
             _ -> invalid_index }
    invalid_index :: TcM a
    invalid_index = failWithTc $ TcRnRetainedQuantificationError invalid_reason

    -- Keep only free runtime occurrences. Telescope binders have their own Ids.
    -- Imported source calls are handled by tc_app_ty.
    ambient_names :: Data a => a -> [(SrcSpan, Name)]
    ambient_names thing
      | Just (L loc (HsTyVar _ _ (L _ (WithUserRdr _ name))))
          <- (cast thing :: Maybe (LHsType GhcRn))
      , isVarName name
      , not (isExternalName name) = [(locA loc, name)]
      | Just (L _ (HsForEachTy _ bndrs _)) <- (cast thing :: Maybe (LHsType GhcRn))
      = let bound = mapMaybe hsLTyVarName bndrs
        in filter (\(_, name) -> name `notElem` bound) (concat (gmapQ ambient_names thing))
      | Just _ <- (cast thing :: Maybe Name) = []
      | Just _ <- (cast thing :: Maybe Type) = []
      | otherwise = concat (gmapQ ambient_names thing)

-- | Check retained telescopes in function positions of a complete signature.
-- See Note [Higher-order retained signatures].
tcCheckRetainedSigType :: Bool -> SkolemInfo -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckRetainedSigType allow_function skol_info hs_ty kind
  = do { indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex hs_ty
       ; tcCheckRetainedType allow_function (Just skol_info)
           (typeLevelMode { mode_indices = indices }) hs_ty kind }

-- | Check a retained type without changing the caller's index environment.
-- See Note [Retained Pi type arguments].
tcCheckRetainedType :: Bool -> Maybe SkolemInfo -> TcTyMode -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckRetainedType allow_function witness_skol = go
  where
    go :: TcTyMode -> LHsType GhcRn -> TcKind -> TcM TcType
    go mode (L _ (HsParTy _ body)) kind = go mode body kind
    go mode (L _ (HsSpliceTy (HsUntypedSpliceTop finalizers body) _)) kind =
      do { addModFinalizersWithLclEnv finalizers
         ; go mode body kind }
    go mode (L loc (HsForAllTy { hst_tele = tele@HsForAllInvis{}, hst_body = body })) kind
      | not (isEmptyNameEnv (mode_indices mode)) || hsHasForEachTy (unLoc body)
      = setSrcSpanA loc $
        tcExtendIdEnv (nonDetNameEnvElts (mode_indices mode)) $
        do { (binders, body_ty) <- tcTKTelescope mode tele (go mode body kind)
           ; return (mkForAllTys binders body_ty) }
    go mode (L loc ty@(HsQualTy _ ctxt body)) kind
      | hsHasForEachTy (unLoc body)
      = setSrcSpanA loc $
        do { theta <- tc_hs_context mode ctxt
           ; dictionaries <- if allow_function
                              then newEvVars theta
                              else pure []
           ; let body_mode = (sourceMethodContext mode theta)
                             { mode_dictionaries = dictionaries ++ mode_dictionaries mode }
           ; body_ty <- go body_mode body liftedTypeKind
           ; let sigma
                   | null dictionaries = tcMkPhiTy theta body_ty
                   | otherwise = foldr bind_dictionary body_ty dictionaries
           ; checkExpKind mode ty sigma liftedTypeKind (Check kind) }
    go mode (L loc (HsFunTy _ mult arg res)) kind
      = setSrcSpanA loc $
        tc_fun_type_x (go mode) (pure unmatchableDataConTy) mode mult arg res (Check kind)
    go mode (L loc ty@(HsForAllTy { hst_tele = tele@HsForAllVis{}
                                 , hst_body = body })) kind
      | allow_function
      , not (isEmptyNameEnv (mode_indices mode)) || hsHasForEachTy (unLoc body)
      = setSrcSpanA loc $
        tcExtendIdEnv (nonDetNameEnvElts (mode_indices mode)) $
        tc_vis_forall_type mode ty tele (go mode body) (Check kind)
    go mode (L loc ty@(HsForEachTy ann bndrs body)) kind
      = setSrcSpanA loc $
        do { level <- getThLevel
           ; case level of
               Comp -> return ()
               _ -> failWithTc $ TcRnRetainedQuantificationError RetainedStaging
           ; when (null bndrs) $
               failWithTc $ TcRnRetainedQuantificationError RetainedBinderCount
           ; body_ty <- check_bndrs mode bndrs
           ; checkExpKind mode ty body_ty liftedTypeKind (Check kind) }
      where
        check_bndrs :: TcTyMode -> [LHsTyVarBndr () GhcRn] -> TcM TcType
        check_bndrs mode [] = go mode body liftedTypeKind
        check_bndrs mode (L lb bndr : rest)
          = setSrcSpanA lb $
            case bndr of
              HsTvb _ () (HsBndrVar _ (L _ name)) (HsBndrKind _ domain) ->
                do { domain_ty <- tcRetainedBinderDomain valid_domain mode domain
                   ; case witness_skol of
                       Just skol | isLiftedTypeKind domain_ty ->
                         -- See Note [Explicit retained Type witnesses].
                         do { tv_name <- newSysName (mkTyVarOccFS (occNameFS (nameOccName name)))
                            ; tv <- newSkolemTyVar skol tv_name liftedTypeKind
                            ; selector <- tcLookupKnownOccId typeRepIdOcc
                            ; let (_, rep_ty) = tcSplitPhiTy $
                                    piResultTys (varType selector) [liftedTypeKind, mkTyVarTy tv]
                            ; body_ty <- tcExtendNameTyVarEnv [(name, tv)] $
                                           check_bndrs mode rest
                            ; let fun_ty | visible = tcMkVisFunTy unmatchableDataConTy ManyTy rep_ty body_ty
                                         | otherwise = mkDepFunTyWithVis DepFunInvisible
                                                         (mkLocalId name ManyTy rep_ty) body_ty
                            ; return (mkInfForAllTy unmatchableDataConTy tv fun_ty) }
                       _ ->
                         do { when (not allow_function && isFunTy domain_ty) $
                                failWithTc $ TcRnRetainedQuantificationError $
                                  if mode_complete_type_arg mode == Just False
                                  then RetainedPartialTypeArgument
                                  else RetainedFunctionSignature
                            ; let index = mkLocalId name ManyTy domain_ty
                                  mode' = mode { mode_indices = extendNameEnv (mode_indices mode) name index }
                            ; body_ty <- check_bndrs mode' rest
                            ; return (mkDepFunTyWithVis (hsForEachVisibility ann) index body_ty) } }
              _ -> failWithTc $ TcRnRetainedQuantificationError RetainedBinderCount

        visible = hsForEachVisibility ann == DepFunVisible
        valid_domain ty = isNativeRetainedBinderType ty
                       || (isJust witness_skol && isLiftedTypeKind ty)
    go mode body kind = tc_check_lhs_type mode body kind

    bind_dictionary dict body
      | isNativeClassDictionaryId dict, dict `elemVarSet` tyCoVarsOfType body
      = DepDictTy dict body
      | otherwise = tcMkPhiTy [varType dict] body

{- Note [Local runtime indices in signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A complete local value signature can mention an already-bound runtime index,
for example xs :: Vec Int n under a do-bound n. The binding renamer adds a
signature-local type lookup alias for the actual term Name. Preserve that Name
and find its existing ATcId in the local type environment. Zonk its type and
multiplicity without changing its Unique. Require a local unrestricted Id of
a supported closed index domain. A call head can instead have an eligible
ordinary function type. See Note [Source opaque call indices]. Do not look
through a global environment or create a replacement Id when lookup fails.

Seed mode_indices with only the runtime Names referenced by this signature.
Registering all ambient index Ids would activate retained forall and context
restrictions for unrelated ordinary signatures. Exclude the signature's own
foreach binders; check_bndrs introduces those Ids in telescope order. Leave
ATyVar occurrences on their ordinary path. Renaming retains erased-variable
precedence, rejects unavailable current-group terms and gates partial capture.

Check the RHS with the ordinary complete-signature rules. The captured Id is
opaque. A let alias supplies no equality between different runtime indices.
Agda checks a local declaration in its current context before extending that
context with its value. Its transparent lets do not establish Haskell equality
or demand. Keep ambient capture outside quotations, as for foreach signatures.
See docs/dependent-haskell/REFERENCES.md#checked-local-payload-signatures.
-}

-- | Check the field suffix of a retained GADT constructor signature.
-- The Boolean selects final domain validation. Kind inference checks only
-- domain kinds because the datatype constructor knots are not yet available.
-- Return explicit metadata too: mkDepFunTy can erase a vacuous binder.
tcCheckRetainedConType :: Bool
                      -> (HsModifiedFunArr GhcRn -> TcM Mult)
                      -> LHsType GhcRn -> TcM ([Maybe Id], TcType)
tcCheckRetainedConType validate_domain tc_field_mult = go typeLevelMode
  where
    valid_field_domain ty = not validate_domain || isNativeRetainedFieldType ty
    go mode (L _ (HsParTy _ body)) = go mode body
    go mode (L loc (HsForEachTy ann bndrs body))
      = setSrcSpanA loc $
        do { when (null bndrs) $
               failWithTc $ TcRnRetainedQuantificationError RetainedBinderCount
           ; when (hsForEachVisibility ann == DepFunInvisible) $
               failWithTc $ TcRnRetainedQuantificationError RetainedInvisibleField
           ; bind_indices mode bndrs body }
    go mode (L loc (HsFunTy _ mult arg res))
      = setSrcSpanA loc $
        do { arg_ty <- tc_check_lhs_type mode arg liftedTypeKind
           ; mult_ty <- tc_field_mult mult
           ; (binders, body_ty) <- go mode res
           ; return (Nothing : binders, tcMkVisFunTy unmatchableDataConTy mult_ty arg_ty body_ty) }
    go _ (L loc HsForAllTy{}) = unsupported loc
    go _ (L loc HsQualTy{})   = unsupported loc
    go mode body@(L loc _)
      = setSrcSpanA loc $
        do { res_ty <- tc_check_lhs_type mode body liftedTypeKind
           ; when (anyFreeVarsOfType isId res_ty) $ unsupported loc
           ; return ([], res_ty) }

    bind_indices mode [] body = go mode body
    bind_indices mode (L loc bndr : rest) body
      = setSrcSpanA loc $
        case bndr of
          HsTvb _ () (HsBndrVar _ (L _ name)) (HsBndrKind _ domain) ->
            do { domain_ty <- tcRetainedBinderDomain valid_field_domain mode domain
               ; let index = mkLocalId name ManyTy domain_ty
                     mode' = mode { mode_indices = extendNameEnv (mode_indices mode) name index }
               ; (binders, body_ty) <- bind_indices mode' rest body
               ; return (Just index : binders, mkDepFunTy index body_ty) }
          _ -> failWithTc $ TcRnRetainedQuantificationError RetainedBinderCount

    unsupported loc = setSrcSpanA loc $
      failWithTc $ TcRnRetainedQuantificationError RetainedPackageDeclaration

-- | Check the domain of a retained binder before bringing it into scope.
-- Constructor kind inference skips the domain predicate until the datatype
-- constructor knots are available.
tcRetainedBinderDomain :: (Type -> Bool) -> TcTyMode -> LHsType GhcRn -> TcM TcType
tcRetainedBinderDomain valid_domain mode domain
  = do { domain_ty <- tc_check_lhs_type mode domain liftedTypeKind
       ; domain_ty <- liftZonkM $ zonkTcType domain_ty
       ; unless (valid_domain domain_ty) $
           failWithTc $ TcRnRetainedQuantificationError (RetainedUnsupportedDomain domain_ty)
       ; return domain_ty }

{- Note [Source retained signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A native value index denotes one runtime Id. The renamer gives that
Id a VarName and binds a type-variable occurrence as an alias of the same
Name. It does not make a TyVar or change the Name's namespace. An ordinary
inner forall can shadow the alias through its normal type-variable key.

The signature checker registers the Id in mode_indices. Only registered
Names become TermVarTy. Ordinary term names still use the normal rejection
path in tcTyVar. Check each binder domain before extending this environment.
Then check the next binder and the body in the extended environment. The
domain is a value type, so its annotation does not require KindSignatures.
It must satisfy isNativeRetainedBinderType. Each runtime binder has multiplicity
Many. See Note [Exported function-valued retained signatures] for the function
domain permission. Apply that permission check to an arrow domain. A lifted
type parameter is already a valid opaque domain. For example, a synonym
can bind foreach (x :: a) and refer to x without applying it as a function.
Check before mkDepFunTy can remove a vacuous binder.
An eligible function Id can also occur as a value index in a closed algebraic
constructor. Its complete fixed type retains U matchability. Retained
constructor fields use the same function-domain policy.
See Note [Function-valued retained binders] in GHC.Core.Type.
The ordinary forall checker scopes each erased binder over the rest of its
telescope. Pass the current runtime indices to the checker for its binder
kinds and body. An erased forall can follow a retained binder in a function
signature. Keep the specified and inferred flags in their original positions.
The result has kind Type. Retained binders in other type positions use
Note [Higher-order retained signatures]. Template Haskell remains gated.

The delimiter determines visibility. Keep a dot binder even when the body
does not use the index. Both delimiters bind a runtime value. See
Note [Retained function visibility] in GHC.Core.TyCo.Rep.

This checks a dependent function directly. Agda's checkPiTelescope checks
each domain before extending the context for the remaining telescope.
See docs/dependent-haskell/REFERENCES.md#agda-provenance and Agda v2.8.0,
Agda.TypeChecking.Rules.Term.checkTelescope'. This scope rule does not
evaluate a runtime argument.
The runtime Id follows the native representation described in
Note [Restricted retained dependent types] in GHC.Core.TyCo.Rep.
-}

{- Note [Explicit retained Type witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a complete value signature, translate a visible `foreach (a :: Type)`
to `forall {a :: Type}. TypeRep a ->` at the same position. Bind the source
Name to a real type variable for the rest of the telescope. The function
equation matches an ordinary value of type `TypeRep a`. A pattern type
signature can name the erased parameter in that equation.

Use the result type of the canonical `typeRep#` selector to obtain
`TypeRep a` at kind Type. This does not generate a Typeable constraint
or call the selector. The caller supplies the lifted witness explicitly.
The ordinary arrow gives the witness the same demand semantics as an
explicit TypeRep argument. The interface stores the inferred forall and
ordinary arrow, so a client can read and use them with the extension off.
The interface does not record the original foreach spelling.

An invisible `foreach (a :: Type).` uses the same erased binder and an
invisible runtime TypeRep binder. An explicit operand is still a value.
Omission constructs the witness using Typeable. See Note [Instantiating a
retained Type witness] in GHC.Tc.Utils.Instantiate.

Constructor telescopes and retained Type binders in general type arguments
are outside this translation. See the phase distinction in GHC proposal
#378.
-}

{- Note [Exported function-valued retained signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A complete ordinary value signature can bind a retained function, for example:

  idCall :: forall a. foreach (f :: Nat -> Nat)
         -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)

tcUserTypeSig grants the function-domain permission only in its complete named
branch. tcHsSigWcType carries that permission to tc_hs_sig_type. FunSigCtxt
alone does not grant it: instance method signatures and SPECIALISE also use
that diagnostic context. The generic tcHsSigType entry point and hs-boot
checking pass False. Keep the scalar retained signature walker in those paths.
Other signature contexts keep their existing checks. Explicit type arguments
use the separate permission in Note [Retained Pi type arguments].

Check each domain before adding its Id to the telescope. The function must
satisfy the closed visible Many A -> B policy.
See Note [Function-valued retained binders] in GHC.Core.Type. A bare function
is a value index at its fixed type. Application keeps the opaque call policy.

The interface already encodes this telescope with IfaceDepFunTy and
IfaceTermAppTy. tcIfaceType uses isNativeOpaqueValueId for both a binder
and a bare IfaceTermVar value. Decode each binder
before its body and check that the body has kind Type. No wire tag or layout
changes. With the same header version, an old reader can display the raw
signature. Its scalar-only binder check rejects a typed import. No new
version is needed.
-}

{- Note [Higher-order retained signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A qualified signature can have further retained binders in its body. Keep the
retained signature walker through HsQualTy. Rename and check its constraints
with the ordinary context routines in the current index environment. Then
check the body with the retained walker and build the usual evidence arrows.
Later binders scope over the body, not the preceding constraints. The syntax
selector hsHasForEachTy must also traverse qualified bodies. This permits a
leading class context and a context between retained arguments. Ordinary
qualified bodies without retained syntax keep the ordinary kind checker.
See docs/dependent-haskell/REFERENCES.md#later-retained-arguments-in-constructor-equations.

A complete value signature can put a retained function in an ordinary arrow
argument or result. Rename and check both children in their original context.
Each nested retained telescope extends only its own body's index environment.
It can refer to an outer runtime index. Its binder must not enter the enclosing
arrow's result. The closure parameter is an ordinary value, not a runtime index.

Use tc_fun_type_x with the signature checker for these children. Keep the
ordinary open-kind and multiplicity checks. Other type positions use
tc_check_lhs_type; see Note [Retained Pi type arguments]. The renamer and complete-signature
guard use hsHasForEachTy. Reject partial signatures even when a wildcard lies
outside the nested Pi.

A required forall in a complete signature keeps the retained checker for
its body. For example, forall (p :: a %U -> Bool) -> can precede a retained
callback on a. Its type variable is erased, and the ordinary telescope
checker still binds it. This preserves the required argument and the normal
ambiguity check. A required forall can also follow a retained binder in a
complete signature. Its binder kinds and body can see the preceding indices.

The application checker supplies the closure's expected type. A source lambda
uses Note [Checked retained lambdas] in GHC.Tc.Gen.Match. A named function can
match an alpha-equivalent retained type without structural Pi unification.
Agda's expected-argument and scoped-Pi rules supply the checking precedent.
See docs/dependent-haskell/REFERENCES.md#explicit-higher-order-arguments.
-}

{- Note [Retained Pi type arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained function is a value type of kind Type. A checked type argument can
therefore contain a supported Pi, including an operand of a type equality.
Use tcCheckRetainedType with the caller's mode. tcHsTypeApp records completeness
of the whole explicit argument in mode_complete_type_arg. Only Just True grants
function-binder permission. Just False selects the incomplete-argument error
when a function binder is reached. Nothing retains the signature-context gate.
Carry this context through ordinary nested type checking. Do not grant it from
the wildcard mode or the outer syntax shape. A type-constructor operand or a
kind-annotated value type uses the same rule. Fresh kind modes clear the context,
and the TypeLevel guard still rejects literal retained syntax in a kind.
The shared syntax predicate comes from Note [Anonymous wildcards in complete
types] in GHC.Hs.Type. Check the renamer's named-wildcard list separately.
Reconstructing the signature mode would lose ambient runtime indices and the
current wildcard policy.
Each telescope extends only its body's environment. Check its closed domain,
Many multiplicity, body kind and staging with the existing signature rules.
Keep the original source span when entering this checker through tcHsType.

Do not select this rule by the printed name of an equality constructor.
Ordinary checked type applications share the same type formation. This does
not admit literal foreach syntax in a kind. It does not change isTauTy, erased
forall checking or the restrictions on mutable type-variable assignment. Checking
an existing Given is separate; see Note [Rigid retained indices] in
GHC.Tc.Utils.Unify. Agda's Pi formation and equality transport motivate this
rule. See docs/dependent-haskell/REFERENCES.md#retained-function-types-as-equality-operands.
-}



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
tcStandaloneKindSig :: TyClDecl GhcRn
                    -> LStandaloneKindSig GhcRn -> TcM (Name, Kind)
tcStandaloneKindSig decl (L _ (StandaloneKindSig _ (L _ name) ksig))
  = addSigCtxt ctxt (UserLHsSigType ksig) $
    do { kind <- tc_top_lhs_type_x (Just decl) KindLevel ctxt ksig
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
tc_top_lhs_type = tc_top_lhs_type_x Nothing

-- | Check a top-level signature with optional declaration-prefix constraints.
-- See Note [Checked declaration-kind matchability].
tc_top_lhs_type_x :: Maybe (TyClDecl GhcRn)
                  -> TypeOrKind -> UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
tc_top_lhs_type_x decl tyki ctxt (L loc sig_ty@(HsSig { sig_bndrs = hs_outer_bndrs
                                               , sig_body = body }))
  = setSrcSpanA loc $
    do { traceTc "tc_top_lhs_type {" (ppr sig_ty)
       ; skol_info <- mkSkolemInfo skol_info_anon
       ; (tclvl, wanted, (outer_bndrs, ma_bndrs, ty))
              <- pushLevelAndSolveEqualitiesX "tc_top_lhs_type"    $
                 do { kind <- newExpectedKind (expectedKindInCtxt ctxt)
                    ; outer_ma <- tcForAllMatchability (hsOuterMatchability hs_outer_bndrs)
                    ; (outer_bndrs, ty) <- tcOuterTKBndrs skol_info hs_outer_bndrs $
                        tc_check_lhs_type mode body kind
                    ; ma_bndrs <- forAllMatchabilities mode outer_ma (outerTyVarBndrs outer_bndrs)
                    ; return (outer_bndrs, ma_bndrs, ty) }

       ; outer_bndrs <- scopedSortOuter outer_bndrs
       ; let ma_env = mkVarEnv [(binderVar bndr, ma) | (ma,bndr) <- ma_bndrs]
             outer_tv_bndrs = [(lookupVarEnv_NF ma_env (binderVar bndr), bndr)
                              | bndr <- outerTyVarBndrs outer_bndrs]
             ty1 = mkInvisForAllTys outer_tv_bndrs ty

       ; wanted <- case decl of
           Nothing -> return wanted
           Just decl ->
             do { (_, wanted', ()) <- pushLevelAndSolveEqualitiesX
                                       "checkDeclKindMatchability" $
                    do { emitConstraints wanted
                       ; checkDeclKindMatchability decl ty1 }
                ; return wanted' }
       ; when (isJust decl) $ defaultDeclKindMatchabilities ty1
       ; kvs <- kindGeneralizeAll skol_info ty1  -- "All" because it's a top-level type
       ; reportUnsolvedEqualities skol_info kvs tclvl wanted

       ; let inferred_bndrs = map (mkForAllTyBinder Inferred) kvs
       ; provisional <- liftZonkM $ zonkTcType $
           mkForAllTys (map (defaultForAllMatchability tyki,) inferred_bndrs) ty1
       ; let prefix_len = maybe 0 (\decl -> length (declKindPrefix decl provisional)) decl
             inferred_mas = [ case decl of
                                Just decl | i < prefix_len -> tyConFlavourMatchability (tyClDeclFlavour decl)
                                _ -> defaultForAllMatchability tyki
                            | i <- [0 .. length kvs - 1] ]
       ; final_ty <- initZonkEnv DefaultFlexi
                   $ zonkTcTypeToTypeX (mkInfForAllTys (zip inferred_mas kvs) ty1)
       ; traceTc "tc_top_lhs_type }" (vcat [ppr sig_ty, ppr final_ty])
       ; return final_ty }
  where
    skol_info_anon = SigTypeSkol ctxt
    mode = (mkMode tyki) { mode_declaration = isJust decl }

{- Note [Checked declaration-kind matchability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A declaration's consumed Pi prefix is M for data, data families and
classes. It is U for type families and synonyms. Its result kind is independent.
Explicit metadata must satisfy this rule. Do not replace it in mkTyCon.

During a standalone kind signature check, omitted metadata gets a local
metavariable for each semantic binder. A source group does not share an omitted
annotation. checkDeclKindMatchability constrains only the consumed Pi prefix.
Its header walk shares the zippable/skippable rules with matchUpSigWithDecl.
Invisible signature binders belong to this prefix only while a header binder
remains. Data and class declarations also consume their eta-expanded prefix.
The check uses the elaborated kind, so it also checks aliases and computed
metadata. A family with no header binders keeps its entire residual kind.
The solver checks explicit metadata with ordinary equality constraints.
defaultDeclKindMatchabilities then defaults remaining local metavariables to M
before generalization. Explicit bound metadata variables remain bound.
No new matchability quantifier is inferred.

Solve the formation equalities before selecting the declaration prefix. A
visible forall checks its body against an outer open kind. Inside its binder
scope, this check can defer an equality and put a CastTy around the body.
For example, the cast can hide the second argument of
  type F :: forall k -> k -> Type
  type family F k a
The prefix splitter stops at that cast until the formation solver fills its
coercion hole. Check the prefix after that solve. Solve its metadata constraints
with the original residual constraints at the same pushed level. Only then
default metadata. Do not discard a cast to select the prefix.

Keep implicit outer annotations associated with their binder identities when
scopedSortOuter changes their order. Kind generalization can introduce more
inferred binders after this defaulting step. Assign those new binders concrete
metadata from the same prefix partition. The residual binders get the normal
kind default M. Ordinary source types instead default their foralls to U.

Inline data-kind signatures use etaExpandAlgTyCon. It checks the metadata of
each Pi node that becomes a declared argument. This also rejects U
arrows in data signatures without a standalone kind signature.

The earlier matchability prototype used a declaration-wide M/U default for
standalone signatures. That default also changed nested and result arrows.
This check instead constrains the declared prefix before it applies the M
kind default. Existing aliases retain their metadata and their source shape.
-}

-- | Check all Pi nodes consumed by the declaration header.
checkDeclKindMatchability :: TyClDecl GhcRn -> TcKind -> TcM ()
checkDeclKindMatchability decl kind
  = do { kind <- liftZonkM $ zonkTcType kind
       ; forM_ (zip [0..] (declKindPrefix decl kind)) $ \ (i, (ma, _)) ->
           checkDeclArrowMatchability kind i ma expected_ma }
  where
    expected_ma = tyConFlavourMatchability (tyClDeclFlavour decl)

-- | Preserve the source synonym in an error for a consumed declaration binder.
checkDeclArrowMatchability :: Kind -> Int -> Matchability -> Matchability -> TcM ()
checkDeclArrowMatchability kind index actual expected
  | actual `tcEqType` unmatchableDataConTy
  , expected `tcEqType` matchableDataConTy
  , Just (name, bndr) <- synonymAt index kind = do
      mod <- tcg_semantic_mod <$> getGblEnv
      let forall_vis = case bndr of
            Named (Bndr _ vis) -> Just vis
            Anon{}             -> Nothing
      addErrCtxt (DeclKindSynonymCtxt name forall_vis (nameIsLocalOrFrom mod name)) check
  | otherwise = check
  where
    check = discardResult $ unifyType Nothing actual expected

    -- An alias can merely forward a U arrow supplied by its caller.
    -- Only report metadata supplied by the unsubstituted synonym RHS.
    synonymAt i ty
      | Just (tc, _) <- splitTyConAppNoView_maybe ty
      , let name = tyConName tc
      , not (isWiredInName name)
      , Just rhs <- synTyConRhs_maybe tc
      , Just (ma, bndr) <- binderAt i rhs
      , ma `tcEqType` unmatchableDataConTy = Just (name, bndr)
      | i > 0, Just (_, _, rest) <- splitPiTy_maybe ty =
          synonymAt (i - 1) rest
      | otherwise = Nothing

    binderAt i ty = do
      (ma, bndr, rest) <- splitPiTy_maybe ty
      if i == 0 then Just (ma, bndr) else binderAt (i - 1) rest

-- | Select exactly the prefix consumed by header matching and data eta expansion.
-- See Note [Checked declaration-kind matchability].
declKindPrefix :: TyClDecl GhcRn -> TcKind -> [(Matchability, PiTyBinder)]
declKindPrefix decl = go hs_bndrs
  where
    flav = tyClDeclFlavour decl
    HsQTvs { hsq_explicit = hs_bndrs } = tyClDeclTyVars decl

    go [] kind
      | needsEtaExpansion flav = eta_prefix kind
      | otherwise = []
    go hs_bndrs@(hs_bndr : rest) kind
      | Just (ma, bndr, body) <- splitPiTy_maybe kind
      , let vis = case bndr of
                    Named (Bndr _ vis) -> NamedTCB vis
                    Anon{} -> AnonTCB
      = if zippableTyConBinder vis (hsTyVarBndrFlag (unLoc hs_bndr))
        then (ma,bndr) : go rest body
        else if skippableTyConBinder vis
             then (ma,bndr) : go hs_bndrs body
             else [] -- The header matcher reports incompatible visibility.
      | otherwise = [] -- The header matcher reports missing binders.

    eta_prefix kind
      | Just (ma, bndr, body) <- splitPiTy_maybe kind
      = (ma,bndr) : eta_prefix body
      | otherwise = []

-- | Default unresolved local arrow metadata before kind generalization.
defaultDeclKindMatchabilities :: TcKind -> TcM ()
defaultDeclKindMatchabilities kind
  = do { candidates <- candidateQTyVarsOfKind kind
       ; forM_ (dVarSetElems (dv_kvs candidates)) $ \ tv ->
           when (isMetaTyVar tv && not (isTyVarTyVar tv)
                 && tcEqType (tyVarKind tv) matchabilityTy) $
             liftZonkM $ writeMetaTyVar tv matchableDataConTy }

-- | Typecheck the class in a default declaration, checking that:
--
--  - it is indeed a class (not e.g. a type family),
--  - that the class expects some invisible arguments followed
--    by a single visible argument.
tcDefaultDeclClass :: LIdP GhcRn -> TcM (Maybe Class)
tcDefaultDeclClass l_nm
  = setSrcSpan (locA l_nm) $
  do { let nm = unLoc l_nm
     ; thing <- tcLookupGlobal nm
     ; case thing of
        ATyCon tc
          | Just cls <- tyConClass_maybe tc
          -> if is_unary (tyConBinders tc)
             then return $ Just cls
             else
               do { addErrTc $ TcRnNonUnaryTypeclassConstraint DefaultDeclCtxt (NameThing nm)
                  ; return Nothing }

        _ -> do { addErrTc $ TcRnIllegalDefaultClass nm
                ; return Nothing }
     }
  where
    is_unary :: [TyConBinder] -> Bool
    is_unary = ( `lengthIs` 1 ) . dropWhile isInvisibleTyConBinder

-----------------
tcHsDeriv :: LHsSigType GhcRn -> TcM (Maybe (Class, [TyCoVar], [Type], Kind))
-- ^ Like tcHsSigType, but for the @...deriving( C ty1 ty2 )@ clause
--
-- Returns a class constraint with the last argument missing, and the
-- expected kind of the remaining argument.
--
-- E.g.:
--
--  @class C (a::*) (b::k->k)@
--  @data T a b = ... deriving( C Int )@
--
-- This function returns @(C, [k], [k, Int], k->k)@.
--
-- Return values are fully zonked
tcHsDeriv hs_ty
  = do { ty <- tcTopLHsType DerivClauseCtxt hs_ty

       ; let (ma_tvs, pred) = splitForAllTyCoVars ty
             tvs = map snd ma_tvs
             (_, kind_args, _) = splitFunTys (typeKind pred)
      -- Checking that `pred` a is type class application

       ; case splitTyConApp_maybe pred of
            Just (tc, tc_args) ->
              case tyConClass_maybe tc of
                Just cls ->
                  case kind_args of
                    [Scaled _ last_kind] ->
                      return $ Just $
                        (cls, tvs, tc_args, last_kind)
                    _ ->
                      do { addErrTc $ TcRnNonUnaryTypeclassConstraint DerivClauseCtxt (TypeThing pred)
                         ; return Nothing
                         }
                Nothing ->
                  do { addErrTc $ TcRnIllegalInstance
                                $ IllegalClassInstance (TypeThing ty)
                                $ IllegalInstanceHead
                                $ InstHeadNonClassHead
                                $ InstNonClassTyCon
                                    (noUserRdr $ tyConName tc)
                                    (fmap tyConName $ tyConFlavour tc)
                     ; return Nothing }
            Nothing ->
              do { addErrTc $ TcRnIllegalDerivingItem hs_ty; return Nothing }
       }

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
       -- Check before instance consumers discard the forall annotations.
       -- See Note [Runtime forall matchability] in GHC.Tc.Utils.Instantiate.
       ; skol_info <- mkSkolemInfo (SigTypeSkol user_ctxt)
       ; let (bndrs, body) = splitForAllForAllTyBinders inst_ty
             subst0 = mkEmptySubst (mkInScopeSet (tyCoVarsOfType inst_ty))
       ; (subst, skol_bndrs) <- tcInstSkolTyVarBndrsX skol_info subst0 bndrs
       ; (_, prefix_implics) <- tcRuntimeForAllBndrs skol_info [] [] skol_bndrs
                                  (substTy subst body)
       ; mapBagM_ checkImplicationInvariants prefix_implics
       ; emitImplications prefix_implics
       ; checkValidInstance user_ctxt hs_inst_ty inst_ty }

----------------------------------------------
-- | Type-check a visible type application
tcHsTypeApp :: LHsWcType GhcRn -> Kind -> TcM Type
-- See Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
tcHsTypeApp wc_ty kind
  | HsWC { hswc_ext = sig_wcs, hswc_body = hs_ty } <- wc_ty
  = do { mode <- mkHoleMode TypeLevel HM_VTA
                 -- HM_VTA: See Note [Wildcards in visible type application]
       ; indices <- tcRetainedAmbientIndices RetainedTypeArgumentIndex hs_ty
       ; let complete = null sig_wcs && hsTypeHasNoAnonWildcards hs_ty
             mode' = mode { mode_indices = indices
                          , mode_complete_type_arg = Just complete }
       ; ty <- addTypeCtxt hs_ty                  $
               solveEqualities "tcHsTypeApp" $
               -- We are looking at a user-written type, very like a
               -- signature so we want to solve its equalities right now
               bindNamedWildCardBinders sig_wcs $ \ _ ->
               tc_check_lhs_type mode' hs_ty kind

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
tcFamTyPats = tcFamTyPatsX []

-- | Give an inferred metadata binder a LHS argument before checking the RHS.
tcInferredMatchabilityFamPats :: TyCon -> HsFamEqnPats GhcRn
                               -> TcM (TcType, TcKind, [TcType])
tcInferredMatchabilityFamPats fam_tc hs_pats = do
  let indices = takeWhile inferredMatchabilityIndex (tyConBinders fam_tc)
  vars <- replicateM (length indices) (newFlexiTyVar matchabilityTy)
  owner <- tcl_kind_arrow_defaults <$> getLclEnv
  forM_ owner $ \(_, ref) ->
    updTcRef ref ([v | (b, v) <- zip indices vars, familyKindUsesMatchability fam_tc b] ++)
  let mas = mkTyVarTys vars
  (lhs, result_kind) <- tcFamTyPatsX mas fam_tc hs_pats
  pure (lhs, result_kind, mas)

tcFamTyPatsX :: [TcType] -> TyCon -> HsFamEqnPats GhcRn -> TcM (TcType, TcKind)
tcFamTyPatsX prefix fam_tc hs_pats
  = do { traceTc "tcFamTyPats {" $
         vcat [ ppr fam_tc, text "arity:" <+> ppr fam_arity ]

       ; mode <- mkHoleMode TypeLevel HM_FamPat
                 -- HM_FamPat: See Note [Wildcards in family instances] in
                 -- GHC.Rename.Module
       ; let fun_ty = mkTyConApp fam_tc prefix
       ; (fam_app, res_kind) <- tcInferTyApps mode lhs_fun fun_ty hs_pats

       -- Hack alert: see Note [tcFamTyPats: zonking the result kind]
       ; res_kind <- liftZonkM $ zonkTcType res_kind

       ; traceTc "End tcFamTyPats }" $
         vcat [ ppr fam_tc, text "res_kind:" <+> ppr res_kind ]

       ; return (fam_app, res_kind) }
  where
    fam_name  = tyConName fam_tc
    fam_arity = tyConArity fam_tc
    lhs_fun   = noLocA (HsTyVar noAnn NotPromoted (noLocA $ noUserRdr fam_name))

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

-- | Infer a callback arrow for a parameter of an unsigned datatype.
-- See Note [Inferred datatype callback arrows].
tcCheckDatatypeField :: NameSet -> LHsType GhcRn -> ContextKind -> TcM TcType
tcCheckDatatypeField parameters field exp_kind = addTypeCtxt field $
  do { ek <- newExpectedKind exp_kind
     ; tc_check_lhs_type
         (typeLevelMode {mode_callback_arrow = InferDatatypeArrows parameters}) field ek }

{- Note [Inferred datatype callback arrows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In an unsigned Haskell 98 declaration such as data Box f = Box (f Bool),
the kind inference pass gives the application of a datatype parameter
a fresh arrow modifier. The kind of Box can quantify that modifier.
A locally bound existential has a different Name and keeps the usual
checking rules.

When applying Box, defer an inferred modifier if a supplied argument
checks against an arrow kind whose modifier is that variable. Its kind
can then constrain the same variable. Generalise it while inferring a
datatype or a closed family. In the final constructor check, use the
modifier in the already inferred kind, including for nested recursive
fields. If the context supplies no constraint, keep the previous default.
No new inference follows from the result of applying the callback.
-}

-- | Check an equation of an unsigned closed family. See Note
-- [Inferred closed family callback arrows].
tcCheckInferredFamilyRhs :: LHsType GhcRn -> TcKind -> TcM TcType
tcCheckInferredFamilyRhs rhs kind = do
  experimental <- xoptM LangExt.ExperimentalDependentTypes
  poly_kinds <- xoptM LangExt.PolyKinds
  let arrows
        | experimental && poly_kinds = InferUnsignedFamilyArrows
        | otherwise = DefaultUnsignedFamilyArrows
  addTypeCtxt rhs $
    tc_check_lhs_type (typeLevelMode {mode_callback_arrow = arrows}) rhs kind

-- | Check a family equation without generalising a new arrow modifier.
-- See Note [Callback kinds in checked family equations].
tcCheckFamilyRhs :: LHsType GhcRn -> TcKind -> TcM TcType
tcCheckFamilyRhs rhs kind = do
  experimental <- xoptM LangExt.ExperimentalDependentTypes
  if experimental
     then addTypeCtxt rhs $
          tc_check_lhs_type (typeLevelMode {mode_callback_arrow = CheckFamilyRhsArrows}) rhs kind
     else tcCheckLHsTypeInContext rhs (TheKind kind)

{- Note [Inferred closed family callback arrows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In an unsigned closed family, an application such as f x can determine
the kind of f. With ExperimentalDependentTypes and PolyKinds, use a fresh
matchability for the missing arrow. Generalise it with the other kind
variables. Use fresh matchabilities also when forwarding f to an inferred
family. Defaulting the receiver's index would specialise the new family.

In the final equation check, a recursive reference is a TcTyCon. It and
other inferred family RHSs must still use fresh matchabilities. Otherwise
a forwarding or recursive equation can specialise its axiom to U even
when the family kind is polymorphic. An explicit signature or a LHS
pattern keeps its separate checking mode. When generalisation is disabled
an unsigned family retains the default M kind for its callback.
-}

{- Note [Callback kinds in checked family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a declared callback kind Type %(Flip m) -> Type. Checking
the equation's LHS emits a wanted for the kind of the callback.
If Flip m has to reduce, the callback may still have an unknown kind
when the RHS applies it. Use an arrow with an owned matchability
metavariable. The solver can read Flip m from the LHS wanted before
defaulting the arrow to M. See Note [Owning omitted arrows in
kind-valued arguments] in GHC.Tc.Solver. This mode does not generalise
the arrow or infer a higher-rank kind.
-}

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
       ; tcInstInvisibleTyBinders (Just TypeLevel) res_ty res_kind }
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
              -> do { (fun_ty, _ki) <- tcInferTyAppHead mode hs_fun_ty hs_args
                    ; tcInferTyApps_nosat TypeLevel Nothing mode hs_fun_ty fun_ty hs_args }
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
  x = /\a. id @a
But we cannot do this in types, as we have no type-level lambda.

So, we must be careful only to instantiate at the last possible moment, when
we're sure we're never going to want the lost polymorphism again. This is done
in calls to `tcInstInvisibleTyBinders`; a particular case in point is in
`checkExpectedKind`.

For example, suppose we have:
    Actual:  ∀ k2 k. k -> k2   -> k
  Expected:  ∀    k. k -> Type -> k
We must very delicately instantiate just k2 to kappa, and then unify
  (∀ k. k -> Type -> k) ~ (∀ k. k -> kappa -> k)

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

-- | Check the multiplicity and the matchability of a constructor field arrow.
tcConArrowMult :: HsModifiedFunArr GhcRn -> TcM (Maybe Mult)
tcConArrowMult ann = do
  (mult, ma) <- tc_arrow_modifiers typeLevelMode ann
  forM_ ma $ \actual -> void (unifyType Nothing actual matchableDataConTy)
  pure mult

-- | Check multiplicity annotations in a field declaration.
tcMult :: HsModifiedFunArr GhcRn -> TcM (Maybe Mult)
tcMult = tc_mult typeLevelMode

tcModifiersAndWarn :: [LHsModifier GhcRn] -> TcM [TcType]
tcModifiersAndWarn mods =
  tc_modifiers typeLevelMode mods Nothing (const $ Left DontSuggestLinear)

tcModifiersMult :: [LHsModifier GhcRn] -> TcM (Maybe Mult)
tcModifiersMult = tc_modifiers_mult typeLevelMode

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
             , mode_declaration :: Bool
             , mode_callback_arrow :: CallbackArrowMode
             , mode_source_givens :: [PredType]
             , mode_holes :: HoleInfo
             , mode_indices :: NameEnv Id
             , mode_dictionaries :: [EvVar]
             , mode_pending_calls :: Maybe (NameEnv TcTyVar, PendingSigCalls, TcRef [TcPendingTypeCall])
               -- See Note [Pending calls in partial signatures] in GHC.Tc.Solver.
             , mode_synonym_metadata :: Maybe (Name -> [Bool] -> TcM [Type], Maybe (TcM Matchability))
             , mode_complete_type_arg :: Maybe Bool }
               -- Nothing outside a visible type argument. Just records
               -- completeness of the whole argument, not the current child.
               -- See Note [Retained Pi type arguments].

data CallbackArrowMode
  = OrdinaryArrowChecking
  | DefaultUnsignedFamilyArrows
  | CheckFamilyRhsArrows
  | InferUnsignedFamilyArrows
  | InferDatatypeArrows NameSet

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
mkMode tyki = TcTyMode { mode_tyki = tyki, mode_holes = Nothing
                      , mode_declaration = False
                      , mode_callback_arrow = OrdinaryArrowChecking
                      , mode_source_givens = []
                      , mode_indices = emptyNameEnv
                      , mode_dictionaries = []
                      , mode_pending_calls = Nothing
                      , mode_synonym_metadata = Nothing
                      , mode_complete_type_arg = Nothing }

typeLevelMode, kindLevelMode :: TcTyMode
-- These modes expect no wildcards (holes) in the type
kindLevelMode = mkMode KindLevel
typeLevelMode = mkMode TypeLevel

mkHoleMode :: TypeOrKind -> HoleMode -> TcM TcTyMode
mkHoleMode tyki hm
  = do { lvl <- getTcLevel
       ; return (TcTyMode { mode_tyki  = tyki
                          , mode_declaration = False
                         , mode_callback_arrow = OrdinaryArrowChecking
                          , mode_source_givens = []
                          , mode_holes = Just (lvl,hm)
                          , mode_indices = emptyNameEnv
                          , mode_dictionaries = []
                          , mode_pending_calls = Nothing
                          , mode_synonym_metadata = Nothing
                          , mode_complete_type_arg = Nothing }) }

instance Outputable HoleMode where
  ppr HM_Sig      = text "HM_Sig"
  ppr HM_FamPat   = text "HM_FamPat"
  ppr HM_VTA      = text "HM_VTA"
  ppr HM_TyAppPat = text "HM_TyAppPat"

instance Outputable TcTyMode where
  ppr (TcTyMode { mode_tyki = tyki, mode_holes = hm, mode_indices = indices
               , mode_complete_type_arg = complete, mode_declaration = declaration })
    = text "TcTyMode" <+> braces (sep [ ppr tyki <> comma
                                      , ppr hm, ppr (nonDetNameEnvElts indices)
                                      , ppr complete, ppr declaration ])

-- | Defer implicit metadata only while a declaration kind is checked.
matchabilityInstantiationMode :: TcTyMode -> Maybe TypeOrKind
matchabilityInstantiationMode mode
  | mode_declaration mode = Nothing
  | otherwise = Just (mode_tyki mode)

-- | Associate metadata with each semantic binder.
-- See Note [Source forall matchability] in GHC.Hs.Type.
forAllMatchabilities :: TcTyMode -> Maybe Matchability -> [bndr] -> TcM [(Matchability, bndr)]
forAllMatchabilities mode explicit_ma = mapM $ \ bndr ->
  do { ma <- case explicit_ma of
               Just ma -> return ma
               Nothing | mode_declaration mode -> newFlexiTyVarTy matchabilityTy
                       | otherwise -> return (defaultForAllMatchability (mode_tyki mode))
     ; return (ma,bndr) }

-- | Check a source annotation before its whole binder group enters scope.
tcForAllMatchability :: Maybe (LHsType GhcRn) -> TcM (Maybe Matchability)
tcForAllMatchability = traverse (\ma -> tc_check_lhs_type kindLevelMode ma matchabilityTy)

-- | Require runtime declaration quantifiers to have unmatchable metadata.
-- Use ordinary equality so type synonyms and reducing families are accepted.
tcCheckRuntimeForAllMatchability :: Maybe (LHsType GhcRn) -> TcM ()
tcCheckRuntimeForAllMatchability Nothing = return ()
tcCheckRuntimeForAllMatchability (Just hs_ma) = setSrcSpanA (getLoc hs_ma) $
  do { ma <- tc_check_lhs_type kindLevelMode hs_ma matchabilityTy
     ; _ <- unifyType Nothing ma unmatchableDataConTy
     ; return () }

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
  = runInferKind $ \exp_kind ->
    tcHsType mode rn_ty exp_kind

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

-- | Instantiate the already checked captures of a lifted let RHS.
-- See Note [Checked RHSs for cases in type lets] in GHC.Tc.TyCl.
tcInferTypeLetRhs :: TcTyMode -> LHsType GhcRn -> TcM (TcType, TcKind)
tcInferTypeLetRhs mode source
  | (L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))), args) <- splitHsAppTys (unLoc source)
  , Just captures <- traverse capture args = do
      env <- getLclTypeEnv
      case lookupNameEnv env name of
        Just (ATcCaseLet params rhs kind Nothing)
          | equalLength params captures -> do
              actuals <- check [] [] params captures
              pure (instantiate params actuals rhs kind)
        Just (ATcCaseLet params rhs kind (Just tc)) -> do
          (applied, _) <- tcInferTyApps mode source (mkTyConTy tc) args
          case splitTyConAppNoView_maybe applied of
            Just (tc', actuals) | tc' == tc, equalLength params actuals ->
              pure (instantiate params actuals rhs kind)
            _ -> panic "tcInferTypeLetRhs: checked helper arity"
        _ -> ordinary
  | otherwise = ordinary
  where
    ordinary = tc_infer_lhs_type mode source
    instantiate params actuals rhs kind =
      (substTyWith params actuals rhs, substTyWith params actuals kind)
    capture (HsValArg _ arg@(L _ HsTyVar{})) = Just arg
    capture _ = Nothing

    check _ _ [] [] = pure []
    check vs args (v:rest) (arg:others) = do
      actual <- tc_check_lhs_type mode arg (substTyWith vs args (varType v))
      (actual :) <$> check (v:vs) (actual:args) rest others
    check _ _ _ _ = panic "tcInferTypeLetRhs: capture arity"

tcHsType :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
-- The main workhorse for type kind checking
-- See Note [Bidirectional type checking]

tcHsType mode (HsParTy _ ty)   exp_kind = tcLHsType mode ty exp_kind
tcHsType mode (HsDocTy _ ty _) exp_kind = tcLHsType mode ty exp_kind

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

tcHsType mode (HsOpTy _ ty1 tyop ty2) exp_kind
  | L _ (HsTyVar _ _ op) <- tyop
  , unLocWithUserRdr op `hasKey` unrestrictedFunTyConKey
  = tc_fun_type mode (HsModifiedFunArr noExtField [] $ HsStandardArr noExtField) ty1 ty2 exp_kind

--------- Foralls
tcHsType mode ty@HsForEachTy{} exp_kind
  | TypeLevel <- mode_tyki mode
  = do { experimental <- xoptM LangExt.ExperimentalDependentTypes
       ; unless experimental $ failWithTc TcRnRetainedQuantificationNotImplemented
       ; ty' <- tcCheckRetainedType (mode_complete_type_arg mode == Just True) Nothing
                                    mode (noLocA ty) liftedTypeKind
       ; checkExpKind mode ty ty' liftedTypeKind exp_kind }
  | otherwise
  = failWithTc TcRnRetainedQuantificationNotImplemented

tcHsType mode HsForAllTy{} _
  | not (isEmptyNameEnv (mode_indices mode))
  = failWithTc $ TcRnRetainedQuantificationError RetainedPolymorphicSignature

tcHsType mode t@(HsForAllTy { hst_tele = tele, hst_body = ty }) exp_kind
  | HsForAllInvis{} <- tele
  = do { (tv_bndrs, ty') <- tcTKTelescope mode tele (tcLHsType mode ty exp_kind)
       ; return (mkForAllTys tv_bndrs ty') }
                 -- For an invisible forall, we allow the body to have
                 -- an arbitrary kind (hence exp_kind above).
                 -- See Note [Body kind of a HsForAllTy]

  | HsForAllVis{} <- tele
  = tc_vis_forall_type mode t tele (tc_check_lhs_type mode ty) exp_kind

tcHsType mode t@(HsQualTy { hst_ctxt = ctxt, hst_body = rn_ty }) exp_kind
  | null (hsc_ctxt $ unLoc ctxt)
  = tcLHsType mode rn_ty exp_kind
    -- See Note [Body kind of a HsQualTy], point (BK1)
  | Check kind <- exp_kind     -- Checking mode
  , isConstraintLikeKind kind  -- CONSTRAINT rep
  = do { ctxt' <- tc_hs_context mode ctxt
         -- See Note [Body kind of a HsQualTy], point (BK2)
       ; ty'   <- tc_check_lhs_type (sourceMethodContext mode ctxt') rn_ty constraintKind
       ; let res_ty = tcMkDFunPhiTy ctxt' ty'
       ; checkExpKind mode t res_ty constraintKind exp_kind }

  | otherwise
  = do { ctxt' <- tc_hs_context mode ctxt

      ; ek <- newOpenTypeKind  -- The body kind (result of the function) can
                                -- be TYPE r, for any r, hence newOpenTypeKind
      ; ty' <- tc_check_lhs_type (sourceMethodContext mode ctxt') rn_ty ek
      ; let res_ty = tcMkPhiTy ctxt' ty'
      ; checkExpKind mode t res_ty liftedTypeKind exp_kind }

--------- Lists, arrays, and tuples
tcHsType mode rn_ty@(HsListTy _ elt_ty) exp_kind
  = do { tau_ty <- tc_check_lhs_type mode elt_ty liftedTypeKind
       ; checkWiredInTyCon listTyCon
       ; checkExpKind mode rn_ty (mkListTy tau_ty) liftedTypeKind exp_kind }

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
       ; checkExpKind mode rn_ty sum_ty sum_kind exp_kind
       }

--------- Promoted lists and tuples
tcHsType mode rn_ty@(HsExplicitListTy _ _ tys) exp_kind
  -- See Note [Kind-checking explicit lists]

  | null tys
  = do let ty = mkTyConTy promotedNilDataCon
       let kind = mkSpecForAllTys [(matchableDataConTy, alphaTyVar)] $ mkListTy alphaTy
       checkExpKind mode rn_ty ty kind exp_kind

  | otherwise
  = do { tks <- mapM (tc_infer_lhs_type mode) tys
       ; (taus', kind) <- unifyKinds mode tys tks
       ; let ty = (foldr (mk_cons kind) (mk_nil kind) taus')
       ; checkExpKind mode rn_ty ty (mkListTy kind) exp_kind }
  where
    mk_cons k a b = mkTyConApp (promoteDataCon consDataCon) [k, a, b]
    mk_nil  k     = mkTyConApp (promoteDataCon nilDataCon) [k]

tcHsType mode rn_ty@(HsExplicitTupleTy _ _ tys) exp_kind
  -- using newMetaKindVar means that we force instantiations of any polykinded
  -- types. At first, I just used tc_infer_lhs_type, but that led to #11255.
  = do { ks   <- replicateM arity newMetaKindVar
       ; taus <- zipWithM (tc_check_lhs_type mode) tys ks
       ; let kind_con   = tupleTyCon           Boxed arity
             ty_con     = promotedTupleDataCon Boxed arity
             tup_k      = mkTyConApp kind_con ks
       ; checkTupSize arity
       ; checkExpKind mode rn_ty (mkTyConApp ty_con (ks ++ taus)) tup_k exp_kind }
  where
    arity = length tys

--------- Constraint types
tcHsType mode rn_ty@(HsIParamTy _ (L _ n) ty) exp_kind
  = do { massert (isTypeLevel (mode_tyki mode))
       ; ty' <- tc_check_lhs_type mode ty liftedTypeKind
       ; let n' = mkStrLitTy $ hsIPNameFS n
       ; ipClass <- tcLookupKnownKeyClass ipClassKey
       ; checkExpKind mode rn_ty (mkClassPred ipClass [n',ty'])
                           constraintKind exp_kind }

tcHsType mode rn_ty@(HsStarTy _) exp_kind
  -- Desugaring 'HsStarTy' to 'Data.Kind.Type' here means that we don't
  -- have to handle it in 'coreView'
  = checkExpKind mode rn_ty liftedTypeKind liftedTypeKind exp_kind

--------- Literals
tcHsType mode rn_ty@(HsTyLit _ lit) exp_kind
  = tc_hs_lit_ty mode rn_ty lit exp_kind

--------- Wildcards

tcHsType mode ty@(HsWildCardTy _) ek
  | Just (_, HM_VTA) <- mode_holes mode
  , not (isEmptyNameEnv (mode_indices mode))
  = failWithTc $ TcRnRetainedQuantificationError RetainedPartialTypeArgument
  | otherwise
  = do k <- expTypeToType ek
       tcAnonWildCardOcc NoExtraConstraint mode ty k

--------- Type applications
tcHsType mode rn_ty@(HsTyVar{})     exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsAppTy{})     exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsAppKindTy{}) exp_kind = tc_app_ty mode rn_ty exp_kind
tcHsType mode rn_ty@(HsOpTy{})      exp_kind = tc_app_ty mode rn_ty exp_kind

tcHsType mode rn_ty@(HsKindSig _ ty sig) exp_kind
  = do { let mode' = mode { mode_tyki = KindLevel
                            , mode_pending_calls = Nothing
                            , mode_complete_type_arg = Nothing }
       ; checked_kind <- tc_lhs_kind_sig mode' KindSigCtxt sig
                 -- We must typecheck the kind signature, and solve all
                 -- its equalities etc; from this point on we may do
                 -- things like instantiate its foralls, so it needs
                 -- to be fully determined (#14904)
       ; sig' <- infer_known_arrow checked_kind
       ; traceTc "tcHsType:sig" (ppr ty $$ ppr sig')
       ; ty' <- tcAddKindSigPlaceholders sig $
                tc_check_lhs_type mode ty sig'
       ; checkExpKind mode rn_ty ty' sig' exp_kind }
  where
    -- Keep the separate kind-shape check required by #14904. A declared arrow is
    -- enough evidence to select U here, without delaying the M default for an
    -- annotation whose subject has an undetermined kind.
    infer_known_arrow checked_kind
      | TypeLevel <- mode_tyki mode
      , not (mode_declaration mode)
      , isEmptyNameEnv (mode_indices mode)
      , matchabilityArrow sig
      , L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))) <- ignoreParens ty
      , FunTy flag _ mult domain result <- checked_kind
      = do { thing <- tcLookup name
           ; known <- traverse (liftZonkM . zonkTcType) (declared_kind thing)
           ; return $ case known >>= tcSplitFunTy_maybe of
               Just (ma, _, _) | ma `tcEqType` unmatchableDataConTy ->
                 FunTy flag ma mult domain result
               _ -> checked_kind }
      | otherwise = pure checked_kind

    declared_kind (ATyVar _ tv) = Just (tyVarKind tv)
    declared_kind (AGlobal (ATyCon tc))
      | tyConArity tc == 0 = Just (tyConKind tc)
    declared_kind _ = Nothing

-- The case alias needs the checked pattern from its enclosing family equation.
-- See Note [Type case aliases] in GHC.Tc.TyCl.
tcHsType mode src@(XHsType (HsTypeExistsRn flag binder body)) exp_kind
  = do { skol <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn [unLoc binder]))
       ; (binders, body') <- tcExplicitTKBndrs skol [binder] $
           tcLHsType mode body (Check liftedTypeKind)
       ; case binders of
           [Bndr tv _] -> do
             setSrcSpanA (getLoc binder) $ do
               kind <- liftZonkM $ zonkTcType (varType tv)
               unless (typeKind kind `tcEqType` liftedTypeKind) $
                 failWithTc (TcRnExistentialBinderKind kind)
             checkExpKind mode src (ExistsTy flag tv body') liftedTypeKind exp_kind
           _ -> panic "tcHsType: existential binder count" }

tcHsType _ (XHsType HsTypeCaseAliasRn{}) _ = failWithTc TcRnTypeCaseContext
tcHsType _ (XHsType HsTypeCaseRn{}) _ = failWithTc TcRnTypeCaseContext

-- See Note [Typechecking type lets].
tcHsType mode (XHsType (HsTypeLetRn (L _ binder) rhs body)) exp_kind = do
  (rhs_ty, rhs_kind) <- tcInferTypeLetRhs mode rhs
  updLclCtxt (tcExtendLocalTypeEnv [(binder, ATcTypeLet binder rhs_ty rhs_kind)]) $
    tcLHsType mode body exp_kind

{- Note [Typechecking type lets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Kind check the RHS once per pass in the enclosing scope. Check it even
when the body does not use the name. Every use shares the checked Type and its kind
metavariables. The binder can shadow an outer type name. The renamer
gives each binder its own Name and keeps the new name out of its RHS.
Read the Type at each occurrence. The let itself never enters Core.
-}

-- See Note [Typechecking HsCoreTys]
tcHsType mode rn_ty@(XHsType (HsCoreTyRn ty)) exp_kind
  = do env <- getLclEnv
       -- Raw uniques since we go from NameEnv to TvSubstEnv.
       let subst_prs :: [(Unique, TcType)]
           subst_prs = [ (getUnique nm, mkTyVarTy tv)
                       | ATyVar nm tv <- nonDetNameEnvElts (getLclEnvTypeEnv env) ]
           in_scope = mkInScopeSet $
                      tyCoVarsOfTypes (ty : map snd subst_prs)
           subst = mkTvSubst in_scope (listToUFM_Directly subst_prs)
           ty'   = substTy subst ty
       checkExpKind mode rn_ty ty' (typeKind ty') exp_kind

tc_hs_lit_ty :: TcTyMode
             -> HsType GhcRn
             -> HsLit GhcRn
             -> ExpKind
             -> TcM TcType
tc_hs_lit_ty mode rn_ty (HsNatural _ lit) exp_kind
  = do let n = il_value lit
       -- Ensure that a type-level integer is nonnegative (#8306, #8412, #26861)
       when (n < 0) $
         addErr $ TcRnNegativeNumTypeLiteral lit
       checkWiredInTyCon naturalTyCon
       let raw = mkNumLitTy n
       converted <- if n < 0 then pure Nothing else tcConvertedNumeral mode exp_kind raw
       let ty = fromMaybe raw converted
       checkExpKind mode rn_ty ty (typeKind ty) exp_kind
tc_hs_lit_ty mode rn_ty (HsString _ s) exp_kind
  = do checkWiredInTyCon typeSymbolKindCon
       checkExpKind mode rn_ty (mkStrLitTy $ mkFastStringShortText s) typeSymbolKind exp_kind
tc_hs_lit_ty mode rn_ty (HsChar _ c) exp_kind
  = do checkWiredInTyCon charTyCon
       checkExpKind mode rn_ty (mkCharLitTy c) charTy exp_kind
tc_hs_lit_ty _mode _ lit _
  = failWithTc $ TcRnUnpromotableLit lit

-- | Elaborate a literal at a known result kind. See Note [Registered type numerals].
tcConvertedNumeral :: TcTyMode -> ExpKind -> Type -> TcM (Maybe Type)
tcConvertedNumeral mode (Check expected) raw
  | Just (_, HM_FamPat) <- mode_holes mode = pure Nothing
  | otherwise = do
      enabled <- xoptM LangExt.ExperimentalDependentTypes
      if not enabled then pure Nothing else do
        target <- liftZonkM (zonkTcType expected)
        case splitTyConApp_maybe target of
          Just (tc, args)
            | tc /= naturalTyCon, isVanillaAlgTyCon tc, isLiftedAlgTyCon tc
            , length args == tyConArity tc, noFreeVarsOfType target ->
                registered target
          _ -> pure Nothing
  where
    registered target = do
      name <- lookupOrig gHC_INTERNAL_TYPENATS (mkTcOcc "FromNatural")
      envs <- tcGetFamInstEnvs
      env <- getGblEnv
      let visible inst = let m = nameModule (getName inst)
                         in m == tcg_semantic_mod env || m `elem` imp_finsts (tcg_imports env)
      case filter visible (familyNameInstances envs name) of
        inst : _ | let fam = famInstTyCon inst
                       args = [target, raw]
                 , isTypeFamilyTyCon fam, tyConArity fam == 2
                 , any (visible . fim_instance) (lookupFamInstEnv envs fam args)
                 -> do marker <- lookupOrig gHC_INTERNAL_TYPENATS (mkTcOcc "NaturalLiteral")
                       tc <- tcLookupTyCon marker
                       pure (Just (mkTyConApp tc args))
        _ -> pure Nothing
tcConvertedNumeral _ (Infer _) _ = pure Nothing

{- Note [Registered type numerals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Core literal 3 always has kind Natural. Suppose T is a user-defined
Peano type, and a module in the import closure provides this equation:

  type instance FromNatural T n = ToPeano n

In a dependent module, when a type signature expects T, elaborate 3 to
FromNatural T (3 :: Natural). The instance and the ordinary family
reducer determine its value. This follows the explicit conversion in
Agda's FROMNAT and the promoted FromInteger in singletons. It does not
require a change to the Core literal or to runtime number conversion.

Keep the converted type inside the ordinary synonym NaturalLiteral T 3
until reduction needs its expansion. GHC.Iface.Type can then display
the source numeral with its kind in an expected type. An explicitly
written FromNatural application can be stuck even without a registered
instance. Do not print that application as a source numeral.

Use the usual Natural literal during inference and when checking family
patterns. Family applications cannot occur in a family pattern. A missing
registration leaves the ordinary kind mismatch at the source numeral.
Do not evaluate the instance RHS when deciding whether the syntax applies.

Find the family by its defining Name in the instance environment. This
also works when boot libraries contain no FromNatural declaration. Limit
the witness to instances in scope here. Loading an unrelated interface
can put more instances in the external package state. See Note [The
type family instance consistency story] in GHC.Tc.Instance.Family.
-}

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
                            checkExpectedKind mode hs_ty ty kind arg_kind
                          | ((L loc hs_ty),ty,kind) <- zip3 hs_tys tys kinds ]

       ; finish_tuple mode rn_ty tup_sort tys' (map (const arg_kind) tys') exp_kind }
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

tc_mult :: TcTyMode -> HsModifiedFunArr GhcRn -> TcM (Maybe Mult)
tc_mult mode (HsModifiedFunArr _ mods arr) = do
  mMult <- tc_modifiers_mult mode mods
  case (arr, mMult) of
    (HsStandardArr _, _) -> pure mMult
    (HsLinearArr _, Nothing) -> pure $ Just oneDataConTy
    (HsLinearArr _, Just _) -> failWithTc TcRnTooManyMultiplicities

-- | The two modifier kinds that can annotate a function arrow.
data ArrowModifier = ArrowMultiplicity | ArrowMatchability

-- | Check the arrow annotations. See Note [Arrow matchability modifiers].
tc_arrow_modifiers :: TcTyMode -> HsModifiedFunArr GhcRn
                   -> TcM (Maybe Mult, Maybe Matchability)
tc_arrow_modifiers _ (HsModifiedFunArr _ [] arr) =
  case arr of
    HsStandardArr _ -> pure (Nothing, Nothing)
    HsLinearArr _   -> pure (Just oneDataConTy, Nothing)
tc_arrow_modifiers mode ann@(HsModifiedFunArr _ mods arr) = do
  modifiers <- xoptM LangExt.Modifiers
  if not modifiers then (, Nothing) <$> tc_mult mode ann else do
    linear_types <- xoptM LangExt.LinearTypes
    let classify k
          | tcEqType k matchabilityTy = Right ArrowMatchability
          | isMultiplicityTy k && linear_types = Right ArrowMultiplicity
          | isMultiplicityTy k = Left SuggestLinear
          | otherwise = Left DontSuggestLinear
    (mult, ma) <- foldM (addModifier classify) (Nothing, Nothing) mods
    case (arr, mult) of
      (HsLinearArr _, Nothing) -> pure (Just oneDataConTy, ma)
      (HsLinearArr _, Just _) -> failWithTc TcRnTooManyMultiplicities
      (HsStandardArr _, _) -> pure (mult, ma)
  where
    addModifier classify (mult, ma) modifier = setSrcSpanA (getLoc modifier) $ do
      found <- tc_modifier_with mode modifier Nothing classify
      case found of
        Nothing -> pure (mult, ma)
        Just (ArrowMultiplicity, m)
          | isNothing mult -> pure (Just m, ma)
          | otherwise -> failWithTc TcRnTooManyMultiplicities
        Just (ArrowMatchability, m)
          | isNothing ma -> pure (mult, Just m)
          | otherwise -> failWithTc TcRnTooManyMatchabilities

{- Note [Arrow matchability modifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With Modifiers, an arrow can have one annotation of kind Matchability.
With LinearTypes, it can also have one of kind Multiplicity. Infer each
modifier's kind once so the order of these annotations is immaterial.
An explicit matchability takes precedence over the context's default.
For example, Map can specify an unmatchable callback as (a %U -> b).
A constructor field arrow must still be matchable.

With NoModifiers, keep checking the multiplicity against the expected
kind. Older linear signatures can use %p without a separate kind for p.
-}

-- | Extract the optional Multiplicity modifier from a list. Nothing if none of
-- them are Multiplicities, error if more than one is. Non-Multiplicity
-- modifiers generate warnings but are otherwise ignored.
--
-- See Note [Typechecking Multiplicity modifiers].
tc_modifiers_mult :: TcTyMode -> [LHsModifier GhcRn] -> TcM (Maybe Mult)
tc_modifiers_mult mode mods = do
  modifiers <- xoptM LangExt.Modifiers
  linearTypes <- xoptM LangExt.LinearTypes
  case (linearTypes, modifiers) of
    (False, _) ->
      go_infer $ \k -> if isMultiplicityTy k
        then Left SuggestLinear
        else Left DontSuggestLinear
    (True, False) -> go_check
    (True, True) ->
      go_infer $ \k -> if isMultiplicityTy k
        then Right ()
        else Left DontSuggestLinear
  where
    go_infer is_mult = do
      mults <- tc_modifiers mode mods (Just multiplicityTyConName) is_mult
      case mults of
        [] -> pure Nothing
        [m] -> pure $ Just m
        _ -> failWithTc TcRnTooManyMultiplicities

    go_check = case mods of
      [] -> pure Nothing
      [L _ (HsModifier _ m)] -> Just <$> tc_check_lhs_type mode m multiplicityTy
      _ -> failWithTc TcRnTooManyMultiplicities

-- | Typecheck a single modifier.
--
-- If the modifier's kind is unknown, emit an error, optionally suggesting the
-- name of a kind to give as a type signature.
--
-- If it has a known kind, it's passed to the @check_expected_kind@ argument to
-- determine if the modifier is recognised in this context. @'Right' '()'@
-- indicates that it's recognised, and we return 'Just' the modifier's type.
-- @'Left' 'DontSuggestLinear'@ indicates that it's not recognised, and we
-- return 'Nothing'. @'Left' 'SuggestLinear' indicates that it's not recognised,
-- but the user may want to enable @-XLinearTypes@.
--
-- For example, when typechecking function arrows, this argument is
--
-- > if isMultiplicityTy k then Right () else Left DontSuggestLinear
--
-- That means that if we see
--
-- * @Int %m -> Int@, we emit an error (unless @m@'s kind is known from context)
-- * @Int %(m :: Multiplicity) -> Int@, we return @'Just' m@
-- * @Int %True -> Int@, we return 'Nothing' and emit a warning.
tc_modifier :: TcTyMode
            -> LHsModifier GhcRn
            -> Maybe Name -- ^ If modifier has unknown kind, suggest this one.
            -> (TcKind -> Either SuggestLinear ())
               -- ^ Given the Kind of a modifier, is the modifier expected?
            -> TcM (Maybe TcType)
tc_modifier mode modifier mSuggestKind check_expected_kind =
  fmap (fmap snd) $ tc_modifier_with mode modifier mSuggestKind check_expected_kind

tc_modifier_with :: TcTyMode -> LHsModifier GhcRn -> Maybe Name
                 -> (TcKind -> Either SuggestLinear a) -> TcM (Maybe (a, TcType))
tc_modifier_with mode (L _ mod@(HsModifier modPrintsAs ty)) mSuggestKind check_expected_kind = do
  (inf_ty, inf_kind) <- tc_infer_lhs_type mode ty
  -- bug: zonking here means that (1) is rejected, but (2) is accepted.
  --     (1): Int %(m :: Multiplicity) -> Int %m -> Int
  --     (2): Int %m -> Int %(m :: Multiplicity) -> Int
  inf_kind' <- liftZonkM $ zonkTcType inf_kind
  case inf_kind' of
    -- This checks for a modifier of unknown kind. It doesn't detect poly-kinded
    -- modifiers. e.g. %Just and %Nothing don't fail here, and possibly they
    -- should.
    TyVarTy _ -> failWithTc $ TcRnUnknownModifierKind mod mSuggestKind
    _ -> case check_expected_kind inf_kind' of
      Right tag -> return $ Just (tag, inf_ty)
      Left suggestLinear -> do
        linearTypes <- xoptM LangExt.LinearTypes
        warn_unrecognised <- woptM Opt_WarnUnrecognisedModifiers
        let suggestLinear' = case modPrintsAs of
              ModifierPrintsAs1 | not linearTypes -> SuggestLinear
              _ -> suggestLinear
        diagnosticTc warn_unrecognised $ TcRnUnrecognisedModifier mod suggestLinear'
        pure Nothing

tc_modifiers :: TcTyMode
             -> [LHsModifier GhcRn]
             -> Maybe Name
             -> (TcKind -> Either SuggestLinear ())
             -> TcM [TcType]
tc_modifiers mode mods mSuggestName is_expected_kind =
  catMaybes <$> mapM (\m -> tc_modifier mode m mSuggestName is_expected_kind) mods

{-
Note [Typechecking Multiplicity modifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When looking for Multiplicity modifiers, we need to consider various
combinations of extensions.

- -XLinearTypes -XModifiers is the basic case. We infer the kind of each
   modifier, modifier to see whether it's a Multiplicity. If there's more than
   one Multiplicity modifier, we throw an error.

     %One %True           => Just One; warning (unrecognised modifier True)
     %True                => Nothing; warning (unrecognised modifier True)
     %One %One            => error (too many multiplicities)
     %(m :: Multiplicity) => Just m
     %m                   => error (modifier m has unknown kind)

- With -XLinearTypes -XNoModifiers, we permit at most one modfier. We /check/
  that its kind is Multiplicity, rather than inferring, which has less need for
  type annotations. This is for backwards compatibility with the days when
  LinearTypes existed but Modifiers didn't.

     %One %True           => error (too many multiplicities)
     %True                => error (True is not of kind Multiplicity)
     %(m :: Multiplicity) => Just m
     %m                   => Just m

- With -XNoLinearTypes, we never return a Multiplicity, and we warn about all
  modifiers. We don't need to check for -XModifiers, because with
  -XNoLinearTypes -XNoModifiers, there won't be any modifiers to typecheck in
  the first place.

     %One %True => Nothing; warning (unrecognised modifier One, suggest enabling
                   -XLinearTypes); warning (unrecognised modifier True)
     %m         => error (modifier m has unknown kind)
-}

------------------------------------------
tc_fun_type :: TcTyMode -> HsModifiedFunArr GhcRn -> LHsType GhcRn -> LHsType GhcRn -> ExpKind
            -> TcM TcType
tc_fun_type mode = tc_fun_type_x (tc_check_lhs_type mode) (pure unmatchableDataConTy) mode

-- | Recognize the existing kind scheme used by the inferred arrow index.
inferredMatchabilityIndex :: TyConBinder -> Bool
inferredMatchabilityIndex b =
  isInferredTyConBinder b && tcEqType (tyVarKind (binderVar b)) matchabilityTy

-- | A receiver can learn its index from a kind in its checked telescope.
familyKindUsesMatchability :: TyCon -> TyConBinder -> Bool
familyKindUsesMatchability tc b =
  any (elemVarSet (binderVar b) . tyCoVarsOfType)
      (tyConResKind tc : map (tyVarKind . binderVar) (tyConBinders tc))

-- | Does the index annotate a curried arrow in a synonym parameter's kind?
synonymCallbackIndex :: TyVar -> Kind -> Bool
synonymCallbackIndex tv (FunTy { ft_af = af, ft_ma = arrow, ft_res = result })
  | isVisibleFunArg af =
      case arrow of
        TyVarTy index | index == tv -> True
        _ -> synonymCallbackIndex tv result
synonymCallbackIndex _ _ = False

-- | Check a synonym RHS with controlled instantiation of metadata.
-- The flag for each index records whether it occurs on a callback arrow.
tcCheckSynonymMetadata :: (Name -> [Bool] -> TcM [Type]) -> Maybe (TcM Matchability)
                       -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckSynonymMetadata get_indices callback_arrow rhs kind =
  addTypeCtxt rhs $
    tc_check_lhs_type
      (typeLevelMode {mode_synonym_metadata = Just (get_indices, callback_arrow)}) rhs kind

hasInferredMatchability :: TyCon -> Bool
hasInferredMatchability tc
  | b : _ <- tyConBinders tc, inferredMatchabilityIndex b =
      case tyConFlavour tc of
        ClosedTypeFamilyFlavour -> True
        OpenFamilyFlavour IAmType _ -> True
        TypeSynonymFlavour -> True
        _ -> False
  | otherwise = False

-- | Check an omitted arrow or pass the index directly through a type alias.
tcCheckMatchabilitySynRhs :: TcType -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckMatchabilitySynRhs ma hs_ty kind
  | matchabilityArrow hs_ty = tcCheckMatchabilityArrow ma hs_ty kind
  | otherwise = tcCheckIndexedMatchability typeLevelMode (pure ma) hs_ty kind

-- | Supply metadata to a direct application of a declared indexed type.
tcCheckIndexedMatchability :: TcTyMode -> TcM TcType -> LHsType GhcRn
                            -> TcKind -> TcM TcType
tcCheckIndexedMatchability mode index hs_ty@(L loc _) kind
  | (hs_fun@(L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name)))), hs_args)
      <- splitHsAppTys (unLoc (ignoreParens hs_ty))
  , isTyConName name =
      setSrcSpanA loc $ do
        (fun_ty, _) <- tcInferTyAppHead mode hs_fun hs_args
        case fun_ty of
          TyConApp tc [] | hasInferredMatchability tc -> do
            ma <- index
            (ty, inferred) <- tcInferTyApps mode hs_fun (mkTyConApp tc [ma]) hs_args
            checkExpKind mode (unLoc hs_ty) ty inferred (Check kind)
          _ -> ordinary
  | otherwise = ordinary
  where
    ordinary = tc_check_lhs_type mode hs_ty kind

-- | Match the source arrow whose metadata can come from a declaration.
matchabilityArrow :: LHsType GhcRn -> Bool
matchabilityArrow hs_ty
  | L _ (HsFunTy _ (HsModifiedFunArr _ [] HsStandardArr{}) _ _) <- ignoreParens hs_ty
  = True
  | otherwise = False

-- | Check one omitted arrow using its declaration's metadata index.
tcCheckMatchabilityArrow :: TcType -> LHsType GhcRn -> TcKind -> TcM TcType
tcCheckMatchabilityArrow ma hs_ty kind
  | L l (HsFunTy _ arr a b) <- ignoreParens hs_ty
  , matchabilityArrow hs_ty
  = setSrcSpanA l $
      tc_fun_type_x (tcCheckMatchabilitySynRhs ma) (pure ma) typeLevelMode arr a b (Check kind)
  | otherwise = tcCheckLHsType hs_ty kind

-- | Check a required forall with the supplied checker for its body.
-- See Note [Body kind of a HsForAllTy] and Note [Higher-order retained signatures].
tc_vis_forall_type :: TcTyMode -> HsType GhcRn -> HsForAllTelescope GhcRn
                   -> (TcKind -> TcM TcType) -> ExpKind -> TcM TcType
tc_vis_forall_type mode ty tele check_body exp_kind
  = do { ek <- newOpenTypeKind
       ; (tv_bndrs, body) <- tcTKTelescope mode tele (check_body ek)
       -- Keep the caller's mode and let the enclosing signature generalise kinds.
       ; checkExpKind mode ty (mkForAllTys tv_bndrs body) ek exp_kind }

-- | Check an ordinary arrow with the supplied checker for its child types.
-- See Note [Higher-order retained signatures].
tc_fun_type_x :: (LHsType GhcRn -> TcKind -> TcM TcType)
              -> TcM Type -> TcTyMode -> HsModifiedFunArr GhcRn -> LHsType GhcRn -> LHsType GhcRn
              -> ExpKind -> TcM TcType
tc_fun_type_x check type_level_ma mode mult ty1 ty2 exp_kind = case mode_tyki mode of
  TypeLevel ->
    do { traceTc "tc_fun_type" (ppr ty1 $$ ppr ty2)
       ; arg_k <- newOpenTypeKind
       ; res_k <- newOpenTypeKind
       ; ty1'  <- check ty1 arg_k
       ; ty2'  <- check ty2 res_k
       ; (mult', explicit_ma) <- tc_arrow_modifiers mode mult
       ; ma <- maybe type_level_ma pure explicit_ma
       ; checkExpKind mode (HsFunTy noExtField mult ty1 ty2)
                      (tcMkVisFunTy ma (fromMaybe manyDataConTy mult') ty1' ty2')
                      liftedTypeKind exp_kind }
  KindLevel ->  -- no representation polymorphism in kinds. yet.
    do { ty1'  <- check ty1 liftedTypeKind
       ; ty2'  <- check ty2 liftedTypeKind
       ; (mult', explicit_ma) <- tc_arrow_modifiers mode mult
       ; ma <- case explicit_ma of
                 Just ma -> pure ma
                 Nothing | mode_declaration mode -> newFlexiTyVarTy matchabilityTy
                         | otherwise -> pure matchableDataConTy
       ; checkExpKind mode (HsFunTy noExtField mult ty1 ty2)
                      (tcMkVisFunTy ma (fromMaybe manyDataConTy mult') ty1' ty2')
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
       ; finish_tuple mode rn_ty tup_sort tau_tys arg_kinds exp_kind }
  where
    arity   = length tys

finish_tuple :: TcTyMode
             -> HsType GhcRn
             -> TupleSort
             -> [TcType]    -- ^ argument types
             -> [TcKind]    -- ^ of these kinds
             -> TcKind      -- ^ expected kind of the whole tuple
             -> TcM TcType
finish_tuple mode rn_ty tup_sort tau_tys tau_kinds exp_kind = do
  traceTc "finish_tuple" (ppr tup_sort $$ ppr tau_kinds $$ ppr exp_kind)
  case tup_sort of
    ConstraintTuple -> do
      let tycon = cTupleTyCon arity
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
      checkExpectedKind mode rn_ty ty act_kind exp_kind

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
    is_app (HsOpTy _ _ tyop _)
      | L _ (HsTyVar _ _ op) <- tyop
      , unLocWithUserRdr op `hasKey` unrestrictedFunTyConKey
      -- I'm not sure why this funTyConKey test is necessary
      -- Can it even happen?  Perhaps for   t1 `(->)` t2
      -- but then maybe it's ok to treat that like a normal
      -- application rather than using the special rule for HsFunTy
      = False
    is_app (HsOpTy {})             = True
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
    go (L _  (HsOpTy _ l tyop r))  as =
      (tyop, HsValArg noExtField l : HsValArg noExtField r : as)
    go f as = (f, as)

---------------------------
tcInferTyAppHead :: TcTyMode -> LHsType GhcRn -> [LHsTypeArg GhcRn] -> TcM (TcType, TcKind)
-- Version of tc_infer_lhs_type specialised for the head of an
-- application. In particular, for a HsTyVar (which includes type
-- constructors, it does not zoom off into tcInferTyApps and family
-- saturation
tcInferTyAppHead mode (L _ (HsTyVar _ _ (L _ (WithUserRdr _ tv)))) args
  -- A saturated case records its rule use with the instantiated kinds.
  -- See Note [Source family evidence uses] in GHC.Tc.Gen.SourceTable.
  | isSourceFamilyName tv
  = do table <- tcLookupTyCon tv
       case sourceCaseVisibleArity table of
         Just arity | lengthIs [() | HsValArg{} <- args] arity
           -> return (mkTyConTy table, tyConKind table)
         _ -> do
           -- Keep legacy eager checking for fully applied source tables.
           let source_arity = length (dropWhile isInvisibleTyConBinder (tyConBinders table))
           when (length [() | HsValArg{} <- args] >= source_arity) $
             sourceTableReferenceUse table
           tcTyVar mode tv
  | otherwise = tcTyVar mode tv
tcInferTyAppHead mode ty _
  = tc_infer_lhs_type mode ty

tc_app_ty :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
tc_app_ty mode rn_ty exp_kind
  | not (null (mode_dictionaries mode))
  , ( L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr _ method)))
    , [HsValArg _ arg]) <- splitHsAppTys rn_ty
  , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ operand))) <- ignoreParens arg
  , Just index <- lookupNameEnv (mode_indices mode) operand
  , isVarName method, isExternalName method
  = do { mb_sel <- tcLookupIdMaybe method
       ; case mb_sel of
           Just sel | isJust (isClassOpId_maybe sel)
             -> setSrcSpanA loc $ do
                  let inst = varType index
                      invalid = failWithTc (TcRnRetainedQuantificationError RetainedCallIndex)
                  case Core.nativeClassSelectorTys sel inst of
                    Just (predicate, domain, result)
                      | domain `tcEqType` inst
                      , [dict] <- filter (\d -> varType d `tcEqType` predicate) (mode_dictionaries mode)
                      , isNativeClassDictionaryId dict
                      -> checkExpKind mode rn_ty
                           (ClassAppTy sel inst (TermVarTy dict) (TermVarTy index) result) result exp_kind
                    _ -> invalid
           _ -> tc_app_ty_other mode rn_ty exp_kind }

  | otherwise = tc_app_ty_other mode rn_ty exp_kind

tc_app_ty_other :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
tc_app_ty_other mode rn_ty exp_kind
  | L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))) <- hs_fun_ty
  , Just head_id <- lookupNameEnv (mode_indices mode) name
  , isNativeIndexFunctionId head_id
  = case filterOut is_par hs_args of
      [] -> checkExpKind mode rn_ty (TermVarTy head_id) (varType head_id) exp_kind
      [HsValArg _ arg] ->
        do { (domain, result) <- case nativeIndexFunctionTys_maybe head_id of
                   Just domains -> return domains
                   Nothing -> invalid_call
           ; arg_ty <- tc_check_lhs_type mode arg domain
           ; checkExpKind mode rn_ty (TermAppTy head_id arg_ty) result exp_kind }
      _ -> invalid_call
    -- See Note [Source opaque call indices].
  where
    is_par HsArgPar{} = True
    is_par _ = False
    invalid_call :: TcM a
    invalid_call = failWithTc $ TcRnRetainedQuantificationError RetainedCallIndex
    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty
tc_app_ty_other mode rn_ty exp_kind
  | L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr rdr name))) <- hs_fun_ty
  , isVarName name
  , isExternalName name
  = setSrcSpanA loc $
    do { env <- getGblEnv
       ; if nameIsLocalOrFrom (tcg_semantic_mod env) name
           then case (lookupNameEnv (tcg_local_open_defs env) name, value_args) of
             (Just (fun, _), Just args)
               | isTypeLevel (mode_tyki mode), ordinary_call
               , Just (instantiated, domains, result_kind) <- instantiateOpenTypeFunction True fun
               , equalLength domains args, all is_local_open_operand args
               -> do { result <- tcOpenTypeCall mode name instantiated result_kind (zip args domains)
                     ; checkExpKind mode rn_ty result result_kind exp_kind }
             (Just _, Just [arg])
               | isTypeLevel (mode_tyki mode), ordinary_call
               , isJust (closedCallLiteralKind arg)
               -> failWithTc (TcRnClosedTypeEval name
                    "Closed type evaluation requires an imported definition. This function belongs to the module being compiled.")
             _ -> unsupported name
           else imported_call env rdr name }
  where
    is_local_open_operand arg = isOpenTypeOperand arg || is_type_constructor_app arg

    -- The observer checks that the completed type is closed and family-free.
    is_type_constructor_app arg = case unLoc (ignoreParens arg) of
      HsTyVar _ NotPromoted (L _ (WithUserRdr _ name)) -> isTyConName name
      HsAppTy _ fun operand -> is_type_constructor_app fun && is_type_constructor_app operand
      _ -> False

    imported_call env rdr name =
      case value_args of
           Just args
             | isTypeLevel (mode_tyki mode)
             , ordinary_call || isJust (mode_pending_calls mode)
             , not (nameIsLocalOrFrom (tcg_semantic_mod env) name)
             -> do { thing <- tcLookupGlobal name
                   ; case thing of
                       AnId fun
                         | ordinary_call
                         , [left, _] <- args
                         , Just (table, nullary : recursive) <- indexedProjectionSource fun
                         , (L _ (HsTyVar _ _ (L _ (WithUserRdr _ operand))), left_args) <-
                             splitHsAppTys (unLoc (indexed_operand left))
                         , (null left_args && (indexed_variable operand || operand == getName nullary)) ||
                           (any ((== operand) . getName) recursive &&
                            any open_indexed_operand args)
                         -> do { sourceTableReferenceUse table
                               ; tc_app_ty_with_head mode rn_ty exp_kind (mkTyConTy table) }
                         | [arg] <- args, isQual rdr
                         , Just (wildcards, call_owner, pending) <- mode_pending_calls mode
                         , Just (owner_level, HM_Sig) <- mode_holes mode
                         , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ operand))) <- ignoreParens arg
                         , Just _ <- lookupNameEnv wildcards operand
                         -> do { (domain, result_kind) <- tcClosedCallKinds Nothing name (varType fun) arg RetryClosedOperand
                               ; tcCheckClosedCallContext name
                               ; case call_owner of
                                   RetrySigCalls -> return ()
                                   RejectSigCalls -> failWithTc (TcRnClosedTypeEval name
                                     ("This binding group has a pattern binding with an explicit multiplicity. " ++
                                      "Give the binding that uses this call a complete type signature."))
                               ; checked <- tc_check_lhs_type mode arg domain
                               ; result <- setTcLevel owner_level (newFlexiTyVarTy result_kind)
                               ; call_span <- getSrcSpanM
                               ; updTcRef pending (TcPendingTypeCall call_span fun checked result :)
                               ; checkExpKind mode rn_ty result result_kind exp_kind }
                         | ordinary_call
                         , Just (instantiated, domains, result_kind) <- instantiateOpenTypeFunction False fun
                         , equalLength domains args
                         , all isOpenTypeOperand args
                         -> do { result <- tcOpenTypeCall mode name instantiated result_kind (zip args domains)
                               ; checkExpKind mode rn_ty result result_kind exp_kind }
                         | not (null args)
                         , ordinary_call || (isQual rdr && ready_partial_call && length args == 1)
                         -> do { selected <- if ordinary_call then checkedClassMethod fun args else pure Nothing
                               ; case selected of
                                   Just (dfun, target, table) -> do
                                     recordSourceMethodUse dfun fun target
                                     checkSourceMethodSelection mode dfun name
                                     (ty, kind) <- tcInferTyApps mode hs_fun_ty (mkTyConTy table) hs_args
                                     checkExpKind mode rn_ty ty kind exp_kind
                                   Nothing -> do
                                     hint <- closedCallResultHint ordinary_call exp_kind
                                     (result_kind, plan) <- tcPlanOrdinaryCall mode ordinary_call hint fun args
                                     checkClosedCallExpectedKind rn_ty exp_kind result_kind
                                     run <- prepareClosedCallResult result_kind plan
                                     result <- run
                                     checkExpKind mode rn_ty result result_kind exp_kind }
                       _ -> unsupported name }
           _ -> unsupported name
    -- See Note [Nullary indexed source projections] in GHC.Tc.Gen.SourceTable.
    indexed_variable name = isTyVarName name || isJust (lookupNameEnv (mode_indices mode) name)
    open_indexed_operand ty = case unLoc (indexed_operand ty) of
      HsTyVar _ NotPromoted (L _ (WithUserRdr _ name)) -> indexed_variable name
      HsAppTy _ fun arg -> open_indexed_operand fun || open_indexed_operand arg
      HsExplicitTupleTy _ _ fields -> any open_indexed_operand fields
      HsExplicitListTy _ _ fields -> any open_indexed_operand fields
      _ -> False

    -- Inspect the constructor head for routing. Check the original operand
    -- and its annotation when applying the selected kind.
    indexed_operand ty = case ignoreParens ty of
      L _ (HsKindSig _ operand _) -> indexed_operand operand
      operand -> operand

    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty
    value_args = traverse value_arg (filterOut is_par hs_args)
    is_par HsArgPar{} = True
    is_par _ = False
    value_arg (HsValArg _ arg) = Just arg
    value_arg _ = Nothing
    ordinary_call = isNothing (mode_holes mode) || isJust (mode_complete_type_arg mode)
    ready_partial_call
      | Just (_, RetrySigCalls, _) <- mode_pending_calls mode = True
      | otherwise = False
    unsupported = failWithTc . TcRnNoSourceTable
    checkedClassMethod fun args@[_, _, receiver]
      | Just _ <- isClassOpId_maybe fun
      , any isOpenTypeOperand args = do
          (_, kind) <- tc_infer_lhs_type mode receiver
          actual <- liftZonkM (zonkTcType kind)
          case splitTyConApp_maybe actual of
            Just (tc, [_]) | tc == listTyCon -> lookupSourceListFoldMethod fun (mkTyConTy tc)
            _ -> pure Nothing
    checkedClassMethod fun [left, right]
      | Just _ <- isClassOpId_maybe fun
      , Just domain <- retainedDomain left <|> retainedDomain right
      = liftZonkM (zonkTcType domain) >>= lookupSourceBinaryMethod fun
    checkedClassMethod _ _ = pure Nothing

    -- A retained operand fixes the class parameter for a method of
    -- type a -> a -> a. Check both original operands against its table.
    retainedDomain arg
      | L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))) <- indexed_operand arg
      , Just index <- lookupNameEnv (mode_indices mode) name
      , isNativeOpaqueValueId index
      = Just (varType index)
      | otherwise = Nothing
tc_app_ty_other mode rn_ty exp_kind
  | L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr rdr name))) <- hs_fun_ty
  , isSourceFamilyName name
  , Just args <- traverse value_arg (filterOut is_par hs_args)
  , not (null args)
  , isTypeLevel (mode_tyki mode)
  , ordinary_call || (lengthIs args 1 && isQual rdr && ready_partial_call)
  = do { staging <- closedOperandStaging args
       ; executable <- tcClosedCaseExecutableOwner rdr name args staging
       ; case executable of
           Just owner -> run loc (\hint -> tcPlanExecutableCase mode ordinary_call hint owner args)
           Nothing
             | ordinary_call, isNothing staging, any (isJust . bareClosedCallback) args
             -> do { table <- tcLookupTyCon name
                   ; checkOpenCallbackInterfaces table args
                   ; mapM_ checkNominalCallback args
                   ; tc_app_ty_generic mode rn_ty exp_kind }
             | ordinary_call, any nominalCallback args
             -> do { mapM_ checkNominalCallback args
                   ; tc_app_ty_generic mode rn_ty exp_kind }
             | ordinary_call, staging == Just True
             -> do { table <- tcLookupTyCon name
                   ; if supportsClosedFamilyTree table args
                       then run loc (\hint -> tcPlanClosedTree mode True hint (CheckClosedCase table) args)
                       else tc_app_ty_generic mode rn_ty exp_kind }
           Nothing -> tc_app_ty_generic mode rn_ty exp_kind }
  where
    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty
    is_par HsArgPar{} = True
    is_par _ = False
    value_arg (HsValArg _ arg) = Just arg
    value_arg _ = Nothing
    ordinary_call = isNothing (mode_holes mode) || isJust (mode_complete_type_arg mode)
    ready_partial_call
      | Just (_, RetrySigCalls, _) <- mode_pending_calls mode = True
      | otherwise = False
    run loc plan = setSrcSpanA loc $ do
      hint <- closedCallResultHint ordinary_call exp_kind
      (kind, value) <- plan hint
      checkClosedCallExpectedKind rn_ty exp_kind kind
      observe <- prepareClosedCallResult kind value
      result <- observe
      checkExpKind mode rn_ty result kind exp_kind
tc_app_ty_other mode rn_ty exp_kind = tc_app_ty_generic mode rn_ty exp_kind

-- | Add the written constraints to the scope of a promoted method call.
sourceMethodContext :: TcTyMode -> [PredType] -> TcTyMode
sourceMethodContext mode theta = mode { mode_source_givens = theta ++ mode_source_givens mode }

-- | Require the ordinary solver to select the exact compiled dictionary.
-- See Note [Select the dictionary for a promoted method] in GHC.Tc.Gen.SourceTable.
checkSourceMethodSelection :: TcTyMode -> Id -> Name -> TcM ()
checkSourceMethodSelection mode dfun selector = do
  lexical <- getLexicalGivens
  written <- newEvVars (mode_source_givens mode)
  (_, constraints) <- captureConstraints $
    checkConstraints (SigTypeSkol (ExprSigCtxt NoRRC)) [] (written ++ lexical) $ do
      wanted <- newWanted (OccurrenceOf selector) Nothing (varType dfun)
      emitSimple (mkNonCanonical wanted)
      emitDelayedErrors (unitBag (DE_SourceMethod (ctEvEvId wanted) dfun selector (ctEvLoc wanted)))
  discardResult (unsetGOptM Opt_SolveConstantDicts (simplifyTopNoDefaulting constraints))

tc_app_ty_generic :: TcTyMode -> HsType GhcRn -> ExpKind -> TcM TcType
tc_app_ty_generic mode rn_ty exp_kind
  = do { (fun_ty, _) <- tcInferTyAppHead mode hs_fun_ty hs_args
       ; tc_app_ty_with_head mode rn_ty exp_kind fun_ty }
  where
    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty

-- | Infer the arguments of an already resolved type application.
tc_app_ty_with_head :: TcTyMode -> HsType GhcRn -> ExpKind -> TcType -> TcM TcType
tc_app_ty_with_head mode rn_ty exp_kind fun_ty
       -- See Note [Owning omitted arrows in kind-valued arguments] in GHC.Tc.Solver.
  = do { let index_level = case exp_kind of
               Check expected | isTypeLikeKind expected -> KindLevel
               _ -> TypeLevel
             expected_kind = checkingExpType_maybe exp_kind
       ; (prefix, prefix_kind) <- tcInferTyApps_nosat index_level expected_kind mode hs_fun_ty fun_ty hs_args
       -- See Note [saturateFamApp] for a checked partial family.
       ; allow_partial <- xoptM LangExt.ExperimentalDependentTypes
       ; (ty, inferred_kind) <- case (exp_kind, tcSplitTyConApp_maybe prefix) of
           (Check expected, Just (tc, args))
             | allow_partial, isUnsaturatedUserFamily tc args
             , let n_expected = invisibleBndrCount expected
             , expected `tcEqType` prefix_kind
               || (n_expected > 0 && n_expected <= invisibleBndrCount prefix_kind)
             -> return (prefix, prefix_kind)
           _ -> saturateFamApp mode prefix prefix_kind
       ; checkExpKind mode rn_ty ty inferred_kind exp_kind }
  where
    (hs_fun_ty, hs_args) = splitHsAppTys rn_ty

-- | Check named rigid operands and record definitions used to reduce the call.
-- See Note [Open source calls] in GHC.Tc.Gen.OpenTypeEval.
tcOpenTypeCall :: TcTyMode -> Name -> Core.CoreExpr -> Kind -> [(LHsType GhcRn, Kind)] -> TcM Type
tcOpenTypeCall mode name instantiated result_kind args = do
  when (mode_complete_type_arg mode == Just False) $
    reject "A visible type argument containing an open call must be complete. Replace its wildcards with types."
  level <- getThLevel
  bootOrSig <- tcIsHsBootOrSig
  explicitLevels <- xoptM LangExt.ExplicitLevelImports
  case level of
    Comp | not bootOrSig && not explicitLevels -> pure ()
    _ -> reject "This call requires an ordinary module outside quotes and splices, without ExplicitLevelImports."
  env <- getGblEnv
  let local = nameIsLocalOrFrom (tcg_semantic_mod env) name
  operands <- mapM (checkOperand local) args
  outcome <- observeOpenType local instantiated result_kind operands
  case outcome of
    Left reason -> let (definition, message) = openTypeFailureReason reason
                   in failWithTc (TcRnOpenTypeEval name definition message)
    Right (result, used, constructors) -> do
      let locals = tcg_local_open_defs env
          lookup_owner n = case lookupNameEnv locals n of
            Just (owner, _) -> pure owner
            Nothing -> tcLookupId n
      dependencies <- forM used $ \(n, first) -> (, first) <$> lookup_owner n
      recordUnfoldedSourceUses dependencies
      updTcRef (tcg_evaluated_term_uses env) (\seen -> seen `unionNameSet` constructors `unionNameSet` mkNameSet (map fst used))
      pure result
  where
    checkOperand local (arg, kind) = setSrcSpanA (getLoc arg) $ do
      let check = tc_check_lhs_type mode arg kind
      -- Report an insoluble kind equality before observing a local operand.
      -- A cast with an unsolved kind equality is not an opaque Type.
      checked <- if local then solveEqualities "tcOpenTypeCall operand" check else check
      liftZonkM (zonkTcType checked)

    reject :: String -> TcM a
    reject = failWithTc . TcRnOpenTypeEval name Nothing

-- | Admit a rigid variable or a promoted constructor with symbolic fields.
isOpenTypeOperand :: LHsType GhcRn -> Bool
isOpenTypeOperand = isOpenTypeOperandWith (\(WithUserRdr _ name) -> isTyVarName name)
                                             (\(WithUserRdr _ name) -> isDataConName name)

-- | Find a resolved term call or a unary checked family call.
nestedClosedCall :: LHsType GhcRn -> Maybe (SrcSpan, RdrName, Name, [LHsType GhcRn])
nestedClosedCall arg
  | L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr rdr name))) <- head_ty
  , isExternalName name
  , Just operands <- traverse val_arg (filterOut is_par args)
  , not (null operands)
  , isVarName name || isSourceFamilyName name
  = Just (locA loc, rdr, name, operands)
  | otherwise = Nothing
  where
    (head_ty, args) = splitHsAppTys (unLoc (ignoreParens arg))
    is_par HsArgPar{} = True
    is_par _ = False
    val_arg (HsValArg _ operand) = Just operand
    val_arg _ = Nothing

-- | Recognize the internal Name selected for a checked source family.
isSourceFamilyName :: Name -> Bool
isSourceFamilyName = isJust . sourceTableTermOcc . nameOccName

-- | The information available about the operand before specialization.
data ClosedCallOperand
  = RetryClosedOperand
  | SourceClosedOperand (Maybe Kind)
  | ComputedClosedOperand Kind

-- | Distinguish equally spelled input types in the caller's import context.
closedOperandMismatch :: Name -> Kind -> Type -> TcM a
closedOperandMismatch owner actual expected = do
  flags <- getDynFlags
  let short_actual = showPpr flags actual
      short_expected = showPpr flags expected
  (a, e) <- if short_actual /= short_expected
            then return (short_actual, short_expected)
            else do ctx <- getNamePprCtx
                    units <- hsc_units <$> getTopEnv
                    let qualify = showSDocForUser flags units ctx . ppr
                    return (qualify actual, qualify expected)
  failWithTc (TcRnClosedTypeEval owner
    ("The operand has kind " ++ a ++ ", which does not match the declared input type " ++ e ++ "."))

-- | Specialize the declared arrow from the argument and a known result kind.
tcClosedCallKinds :: Maybe Kind -> Name -> Type -> LHsType GhcRn
                  -> ClosedCallOperand -> TcM (Type, Type)
tcClosedCallKinds hint name fun_ty arg operand
  | Just (FTF_T_T, _, _, domain, codomain) <- splitFunTy_maybe rho
  , all isTyVar tvs
  = do input <- argumentDomain domain
       operand_subst <- case tcMatchTy domain input of
         Just subst -> return subst
         Nothing -> case operand of
           RetryClosedOperand -> unsupported
           _ -> at_operand (closedOperandMismatch name input domain)
       let subst = refineClosedCallResult hint codomain operand_subst
           undetermined tv = maybe True (not . noFreeVarsOfType) (Subst.lookupTyVar subst tv)
           unresolved = filter undetermined tvs
           result = Subst.substTy subst codomain
           ground_class p = let pred_ty = Subst.substTy subst p
                            in noFreeVarsOfType pred_ty && isJust (getClassPredTys_maybe pred_ty)
       unless (null unresolved) $
         bad (operand_description ++ " does not determine the following type parameters: "
              ++ unwords (map getOccString unresolved) ++ ".")
       unless (all ground_class predicates && noFreeVarsOfType result &&
               Subst.substTy subst rho `tcEqType` mkVisFunTyManyU input result) unsupported
       return (input, result)
  | otherwise = unsupported
  where
    (_, binders, rho) = splitInvisPiTys fun_ty
    tvs = mapMaybe namedPiTyBinder_maybe binders
    predicates = mapMaybe anonPiTyBinderType_maybe binders
    natural_list = mkListTy naturalTy
    retry_inputs = [charTy, natural_list]
    list_inputs = map mkListTy closedCallScalarKinds
    operand_description = case operand of
      ComputedClosedOperand{} -> "The checked operand"
      _ -> "The literal argument"
    at_operand :: TcM a -> TcM a
    at_operand = setSrcSpanA (getLoc arg)
    argumentDomain domain = case operand of
      ComputedClosedOperand actual -> return actual
      RetryClosedOperand
        | any (tcEqType domain) retry_inputs -> return domain
        | not (null predicates), any (isJust . tcMatchTy domain) retry_inputs ->
            bad "The argument must be a closed Char literal or a closed list of Natural literals."
        | otherwise -> unsupported
      SourceClosedOperand (Just actual)
        | L _ (HsExplicitListTy _ _ []) <- ignoreParens arg ->
            case matching list_inputs of
              [input] -> return input
              inputs | any (tcEqType natural_list) inputs -> return natural_list
              [] -> at_operand $ do
                dflags <- getDynFlags
                bad ("The empty promoted list does not match the declared input type "
                     ++ showPpr dflags domain ++ ".")
              _ -> at_operand $
                bad "The declared input type does not determine the element kind of the empty list."
        | otherwise -> return actual
      SourceClosedOperand Nothing -> case matching (closedCallScalarKinds ++ list_inputs) of
        [input]
          | input `tcEqType` charTy ->
              bad "The argument must be a closed Char literal, such as 'x'."
          | input `tcEqType` natural_list ->
              bad "The argument must be a closed list of Natural literals, such as '[1, 2, 3]."
          | otherwise -> at_operand $ do
              dflags <- getDynFlags
              bad ("The argument must be a closed literal of kind " ++ showPpr dflags input ++ ".")
        [] -> unsupported
        inputs -> at_operand $ do
          dflags <- getDynFlags
          let kinds = intercalate ", " (map (showPpr dflags) inputs)
          bad ("The argument must be a closed literal of one of these kinds: " ++ kinds ++ ".")
      where
        matching = filter (isJust . tcMatchTy domain)

    bad :: String -> TcM a
    bad = failWithTc . TcRnClosedTypeEval name
    unsupported :: TcM a
    unsupported = failWithTc (TcRnNoSourceTable name)

-- | Use an already known expected kind as an input to ordinary specialization.
-- See Note [Expected result kinds for closed calls].
closedCallResultHint :: Bool -> ExpKind -> TcM (Maybe Kind)
closedCallResultHint True (Check expected) = do
  kind <- liftZonkM (zonkTcType expected)
  return (nestedClosedResultHint True kind)
closedCallResultHint _ _ = return Nothing

-- | A nested application can only specialize against a known closed kind.
nestedClosedResultHint :: Bool -> Kind -> Maybe Kind
nestedClosedResultHint ordinary kind =
  kind <$ guard (ordinary && noFreeVarsOfType kind && isTyFamFree kind)

-- | Extend the operand substitution when the result still contains a parameter.
refineClosedCallResult :: Maybe Kind -> Kind -> Subst.Subst -> Subst.Subst
refineClosedCallResult (Just expected) codomain subst
  | let result = Subst.substTy subst codomain
  , not (noFreeVarsOfType result), isTyFamFree result
  = fromMaybe subst (tcMatchTyX subst codomain expected)
refineClosedCallResult _ _ subst = subst

{- Note [Expected result kinds for closed calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A Vec index supplies a known Nat kind. A cast such as fromIntegral can
use that kind to determine its result parameter. The operand must
still determine its own checked kind. A missing dictionary or a type
parameter absent from both input and result stays an error.

Take the hint only from a ground, family-free Check context. Do not
guess from an inference variable, default a dictionary, or change
the pending partial-signature path. A result determined by operands
keeps its existing expected-kind check. For nested ordinary calls,
use a ground argument hint at a unary or checked-family boundary.
For the ordinary tree, instantiate each application once and solve its
argument and result kinds together. A computed operand can then constrain
a sibling in either order. Resolve checked boundaries and choose legacy
empty-list defaults before requiring fully ground ordinary applications.
After the operands determine the erased parameters, a ground result can
still contain family applications. Reduce its kind using nominal family
axioms before preparing readback. Ordinary term checking still validates
the complete application at the resulting type.
-}

-- | An ordinary application can remain Core until its result is observed.
-- See Note [Closed source calls].
data ClosedCallValue
  = OrdinaryClosedCall SrcSpan Name (TcM Core.CoreExpr)
  | ClosedFamilyValue SrcSpan (TcM Type)

-- | Prepare the outer observation before running any checked-family operand.
prepareClosedCallResult :: Kind -> ClosedCallValue -> TcM (TcM Type)
prepareClosedCallResult kind (OrdinaryClosedCall loc name expression) = setSrcSpan loc $ do
  reify <- tcPrepareClosedResult name kind
  return (expression >>= reify)
prepareClosedCallResult _ (ClosedFamilyValue _ action) = return action

-- | Keep an ordinary operand in Core. Demote a family result at its source span.
closedCallRuntimeOperand :: Name -> ClosedCallValue -> TcM Core.CoreExpr
closedCallRuntimeOperand _ (OrdinaryClosedCall _ _ expression) = expression
closedCallRuntimeOperand name (ClosedFamilyValue loc action) = do
  checked <- action
  setSrcSpan loc $ do
    value <- tcDecodeClosedCallOperand name checked
    case value of
      Just argument -> return argument
      Nothing -> failWithTc (TcRnClosedTypeEval name
        "The checked operand does not represent a supported closed value.")

-- | Check the kind and evidence before running a unary chain.
-- See Note [Closed source calls].
tcPlanClosedCall :: TcTyMode -> Bool -> Maybe Kind -> Id -> LHsType GhcRn -> TcM (Kind, ClosedCallValue)
tcPlanClosedCall mode ordinary_call hint fun arg = do
  loc <- getSrcSpanM
  let name = getName fun
  (domain, result_kind, operand) <- case nestedClosedCall arg of
    Just (child_loc, child_rdr, child_name, child_arg) -> do
      checkCompleteClosedCall mode name
      tcPreflightClosedCall name
      let (_, _, rho) = splitInvisPiTys (varType fun)
          operand_hint = do
            (_, _, _, domain, codomain) <- splitFunTy_maybe rho
            let initial = Subst.mkEmptySubst (mkInScopeSet (tyCoVarsOfType rho))
                subst = refineClosedCallResult hint codomain initial
            nestedClosedResultHint ordinary_call (Subst.substTy subst domain)
      (child_kind, run_child) <-
        setSrcSpan child_loc (tcPlanClosedHead mode ordinary_call operand_hint child_rdr child_name child_arg)
      (domain, result_kind) <- tcClosedCallKinds hint name (varType fun) arg (ComputedClosedOperand child_kind)
      return (domain, result_kind, closedCallRuntimeOperand name run_child)
    Nothing -> do
      let literal_kind = closedCallLiteralKind arg
      unless (ordinary_call || isJust literal_kind) $
        failWithTc (TcRnNoSourceTable name)
      (domain, result_kind) <- tcClosedCallKinds hint name (varType fun) arg
                               (SourceClosedOperand literal_kind)
      argument <- tcClosedTypeCallArgument mode name domain arg
      return (domain, result_kind, return argument)
  apply <- tcPrepareClosedApplication fun [domain] result_kind
  return (result_kind, OrdinaryClosedCall loc name (operand >>= apply . (:[])))

-- | An ordinary application tree before producer dictionaries are selected.
data ClosedCallTree
  = ClosedLiteral Name (LHsType GhcRn) Type
  | ClosedBoundary (LHsType GhcRn) Kind
  | CheckedBoundary ClosedCallValue
  | ClosedCallback SrcSpan Id Type
  | ClosedFamilyApplication SrcSpan Name TyCon [Kind] [Kind] Kind [ClosedCallTree]
  | ClosedApplication SrcSpan Id [(TyVar, Type)] [Type] [Type] Kind [ClosedCallTree]

-- | Report a partial or oversaturated closed call.
closedCallArityError :: Name -> [Type] -> [LHsType GhcRn] -> TcM a
closedCallArityError name domains args =
  failWithTc (TcRnClosedTypeEval name
    ("Expected " ++ show (length domains) ++
     (if lengthIs domains 1 then " value argument, but got " else " value arguments, but got ") ++
     show (length args) ++ ". Closed calls must be fully applied."))

-- | Prepare a source call without evaluating its operands.
tcPlanOrdinaryCall :: TcTyMode -> Bool -> Maybe Kind -> Id -> [LHsType GhcRn] -> TcM (Kind, ClosedCallValue)
tcPlanOrdinaryCall mode ordinary hint fun args
  | not (equalLength domains args) = do
      checkCompleteClosedCall mode (getName fun)
      tcPreflightClosedCall (getName fun)
      closedCallArityError (getName fun) domains args
  | ordinary, isJust hint
  , [arg] <- args, L _ (HsExplicitListTy _ _ []) <- ignoreParens arg
  = plan
  | ordinary
  , [arg] <- args, L _ (HsExplicitListTy _ _ []) <- ignoreParens arg
  , [domain] <- domains, noFreeVarsOfType domain
  , Just (tc, [element]) <- splitTyConApp_maybe domain, tc == listTyCon
  , not (any (tcEqType element) closedCallScalarKinds)
  = plan
  | ordinary, any (isJust . bareClosedCallback) args
  = plan
  | [arg] <- args
  , not ordinary || isJust (closedCallLiteralKind arg) || isJust (nestedClosedCall arg)
  = tcPlanClosedCall mode ordinary hint fun arg
  | [arg] <- args
  , not (closedConstructorLiteral arg)
  = do alias <- closedConstructorAlias arg
       if alias then plan else tcPlanClosedCall mode ordinary hint fun arg
  | otherwise = plan
  where
    plan = tcPlanClosedTree mode ordinary hint (RunClosedFunction fun) args
    (_, _, rho) = splitInvisPiTys (varType fun)
    (_, arrows, _) = splitFunTys rho
    domains = map scaledThing arrows

-- | A top-level type can keep the checked family until the solver needs its axiom.
data ClosedCallHead = RunClosedFunction Id | CheckClosedCase TyCon | ReduceClosedCase TyCon

-- | Solve one kind problem for the whole ordinary tree before preparing evidence.
tcPlanClosedTree :: TcTyMode -> Bool -> Maybe Kind -> ClosedCallHead -> [LHsType GhcRn]
                 -> TcM (Kind, ClosedCallValue)
tcPlanClosedTree mode ordinary hint root arguments = do
  tree <- solveEqualities "tcPlanClosedTree" $ case root of
    RunClosedFunction fun -> collect True fun hint arguments
    CheckClosedCase table -> collect_family table hint arguments
    ReduceClosedCase table -> collect_family table hint arguments
  checked <- solveEqualities "tcPlanClosedTree: checked families" (prepare_boundaries tree)
  solveEqualities "tcPlanClosedTree: default empty lists" (default_empty checked)
  ground <- verify checked
  case ground of
    ClosedApplication _ _ _ _ _ result _ -> (result,) <$> prepare ground
    ClosedFamilyApplication loc owner table kinds [input] result [child]
      | CheckClosedCase{} <- root -> setSrcSpan loc $ do
          -- Forming a type needs the argument, but does not use the case axiom.
          -- See Note [Source family evidence uses] in GHC.Tc.Gen.SourceTable.
          run <- case child of
            ClosedApplication call_loc fun vars _ domains _ args -> do
              selected <- closedPairProjectionField table kinds input result
              case (selected, sourceTableId_maybe fun >>= sourcePairBuilderField) of
                (Just side, Just (copied, copied_arg))
                  | side == copied
                  , Just builder <- sourceTableId_maybe fun
                  , and [i == copied_arg || literal child_arg | (i, child_arg) <- zip [0..] args] -> do
                      operands <- zipWithM prepare_family_operand domains args
                      pure $ do
                        values <- zipWithM (checkClosedFamilyOperand (getName fun)) domains operands
                        pure (mkTyConApp builder (map snd vars ++ values))
                (Just side, _) -> do
                  observed <- prepareObservedPair side call_loc fun (map snd vars) domains args input result
                  maybe (prepare_family_operand input child) pure observed
                _ -> prepare_family_operand input child
            _ -> prepare_family_operand input child
          let application = do
                arg <- checkClosedFamilyOperand owner input run
                return (mkTyConApp table (kinds ++ [arg]))
          return (result, ClosedFamilyValue loc application)
    ClosedFamilyApplication _ _ _ _ _ result _ -> (result,) <$> prepare ground
    _ -> panic "tcPlanClosedTree: missing call root"
  where
    -- Quoting another computed input could force it even if the builder ignores it.
    literal ClosedLiteral{} = True
    literal _ = False

    collect outer_call fun expected args = do
      loc <- getSrcSpanM
      let name = getName fun
          (_, binders, rho) = splitInvisPiTys (varType fun)
          tvs = mapMaybe namedPiTyBinder_maybe binders
          predicates = mapMaybe anonPiTyBinderType_maybe binders
          (_, arrows, codomain) = splitFunTys rho
          domains = map scaledThing arrows
      checkCompleteClosedCall mode name
      tcPreflightClosedCall name
      unless (equalLength domains args) (closedCallArityError name domains args)
      unless (all isTyVar tvs && rho `tcEqType` foldr mkVisFunTyManyU codomain domains) $
        unsupported name
      (subst, variables) <- newMetaTyVars tvs
      let inputs = map (Subst.substTy subst) domains
          result = Subst.substTy subst codomain
          constraints = map (Subst.substTy subst) predicates
      forM_ expected $ \kind -> discardResult (unifyKind Nothing result kind)
      children <- zipWithM (collect_arg outer_call name) args inputs
      return (ClosedApplication loc fun (zip tvs (mkTyVarTys variables))
                constraints inputs result children)

    collect_family table expected args = do
      loc <- getSrcSpanM
      owner <- closedFamilyOwner (getName table)
      let (invisible, visible) = span isInvisibleTyConBinder (tyConBinders table)
          tvs = [tv | Bndr tv _ <- invisible]
          domain (Bndr tv AnonTCB) = Just (tyVarKind tv)
          domain _ = Nothing
      checkCompleteClosedCall mode owner
      case traverse domain visible of
        Just domains | supportsClosedFamilyTree table args
                     , all isTyVar tvs
                     , tyConArity table == length tvs + length domains -> do
          (subst, variables) <- newMetaTyVars tvs
          let inputs = map (Subst.substTy subst) domains
              result = Subst.substTy subst (tyConResKind table)
          forM_ expected $ \kind -> discardResult (unifyKind Nothing result kind)
          children <- zipWithM (collect_arg False owner) args inputs
          return (ClosedFamilyApplication loc owner table (mkTyVarTys variables) inputs result children)
        _ -> bad_family owner

    collect_arg outer_call name arg expected
      | ordinary, Just (loc, rdr, child) <- bareClosedCallback arg
      = setSrcSpan loc $ do
          callback <- tcClosedCallback rdr child
          let (_, binders, rho) = splitInvisPiTys (varType callback)
              tvs = mapMaybe namedPiTyBinder_maybe binders
              (_, arrows, result) = splitFunTys rho
              domains = map scaledThing arrows
          unless (all isTyVar tvs && equalLength tvs binders &&
                  not (null domains) && rho `tcEqType` foldr mkVisFunTyManyU result domains) $
            failWithTc (TcRnClosedTypeEval (getName callback)
              "A closed callback must have an ordinary function type without constraints.")
          (subst, _) <- newMetaTyVars tvs
          let actual = Subst.substTy subst rho
          discardResult (unifyKind (Just (HsTypeRnThing (unLoc arg))) actual expected)
          return (ClosedCallback loc callback actual)
      | Just (loc, rdr, child, args) <- nestedClosedCall arg
      , isSourceFamilyName child, ordinary
      = setSrcSpan loc $ do
          staging <- closedOperandStaging args
          table <- tcLookupTyCon child
          if admissibleFamilyOperands args staging && supportsClosedFamilyTree table args
            then do
              executable <- tcClosedCaseExecutableOwner rdr child args staging
              case executable of
                Just fun -> collect False fun (Just expected) args
                Nothing -> collect_family table (Just expected) args
            else return (ClosedBoundary arg expected)
      | Just (loc, _, child, args) <- nestedClosedCall arg
      = if isSourceFamilyName child || any bare_empty args
          then return (ClosedBoundary arg expected)
          else setSrcSpan loc $ do
            env <- getGblEnv
            when (nameIsLocalOrFrom (tcg_semantic_mod env) child) $
              failWithTc (TcRnNoSourceTable child)
            thing <- tcLookupGlobal child
            case thing of
              AnId fun -> collect False fun (Just expected) args
              _ -> failWithTc (TcRnNoSourceTable child)
      | otherwise = setSrcSpanA (getLoc arg) $ do
          eligible <- if closedConstructorLiteral arg
                        then pure True
                        else closedConstructorAlias arg
          unless eligible $
            failWithTc (TcRnClosedTypeEval name
              "Each constructor field must be a closed literal. This operand contains an open or unsupported field.")
          unless (bare_empty arg) $
            forM_ (closedCallLiteralKind arg) $ \actual -> do
              -- Keep the closed-call diagnostic for direct operands.
              -- A nested application uses the ordinary equality error so
              -- the diagnostic can report the conflicting computed kinds.
              when outer_call $ do
                declared <- liftZonkM (zonkTcType expected)
                when (noFreeVarsOfType declared && isTyFamFree declared &&
                      not (actual `tcEqType` declared)) $
                  closedOperandMismatch name actual declared
              discardResult (unifyKind (Just (HsTypeRnThing (unLoc arg))) actual expected)
          literal <- tc_check_lhs_type mode arg expected
          return (ClosedLiteral name arg literal)

    prepare_boundaries (ClosedBoundary arg expected)
      | Just (loc, rdr, name, args) <- nestedClosedCall arg = do
        expected' <- liftZonkM (zonkTcType expected)
        let hint' = nestedClosedResultHint ordinary expected'
        (actual, value) <- setSrcSpan loc (tcPlanClosedHead mode ordinary hint' rdr name args)
        setSrcSpanA (getLoc arg) $
          discardResult (unifyKind (Just (HsTypeRnThing (unLoc arg))) actual expected)
        return (CheckedBoundary value)
    prepare_boundaries (ClosedApplication loc fun vars predicates inputs result children) =
      ClosedApplication loc fun vars predicates inputs result <$> traverse prepare_boundaries children
    prepare_boundaries (ClosedFamilyApplication loc owner table kinds inputs result children) =
      ClosedFamilyApplication loc owner table kinds inputs result <$> traverse prepare_boundaries children
    prepare_boundaries tree = return tree

    default_empty (ClosedLiteral _ arg literal)
      | bare_empty arg = do
          actual <- liftZonkM (zonkTcType (typeKind literal))
          case splitTyConApp_maybe actual of
            Just (tc, [element]) | tc == listTyCon
                                 , Just tv <- getTyVar_maybe element
                                 , isMetaTyVar tv ->
              setSrcSpanA (getLoc arg) (discardResult (unifyKind Nothing element naturalTy))
            _ -> return ()
    default_empty (ClosedApplication _ _ _ _ _ _ children) = mapM_ default_empty children
    default_empty (ClosedFamilyApplication _ _ _ _ _ _ children) = mapM_ default_empty children
    default_empty _ = return ()

    verify (ClosedLiteral name arg literal) = do
      checked <- liftZonkM (zonkTcType literal)
      unless (noFreeVarsOfType checked) $ setSrcSpanA (getLoc arg) $
        failWithTc (TcRnClosedTypeEval name
          "The constructor literal does not determine a closed operand kind.")
      return (ClosedLiteral name arg checked)
    verify (ClosedCallback loc fun ty) = setSrcSpan loc $ do
      checked <- liftZonkM (zonkTcType ty)
      unless (noFreeVarsOfType checked && isTyFamFree checked) $
        failWithTc (TcRnClosedTypeEval (getName fun)
          "The enclosing call does not determine the callback's ordinary type.")
      return (ClosedCallback loc fun checked)
    verify tree@CheckedBoundary{} = return tree
    verify ClosedBoundary{} = panic "tcPlanClosedTree: unprepared boundary"
    verify (ClosedApplication loc fun vars constraints inputs result children) = setSrcSpan loc $ do
      let name = getName fun
          ground ty = noFreeVarsOfType ty && isTyFamFree ty
      values <- liftZonkM (zonkTcTypes (map snd vars))
      let unresolved = [getOccString tv | ((tv, _), value) <- zip vars values, not (ground value)]
      unless (null unresolved) $
        failWithTc (TcRnClosedTypeEval name
          ("The operands do not determine the following type parameters: " ++ unwords unresolved ++ "."))
      constraints <- liftZonkM (zonkTcTypes constraints)
      inputs <- liftZonkM (zonkTcTypes inputs)
      unless (all ground inputs &&
              all (\p -> ground p && isJust (getClassPredTys_maybe p)) constraints) $
        unsupported name
      zonked_result <- liftZonkM (zonkTcType result)
      -- See Note [Expected result kinds for closed calls].
      result <- if noFreeVarsOfType zonked_result && not (isTyFamFree zonked_result)
        then do norm_loc <- getCtLocM AnnOrigin Nothing
                reductionReducedType <$> tcRewriteFamilyAxioms norm_loc zonked_result
        else return zonked_result
      unless (ground result) $ unsupported name
      ClosedApplication loc fun (zip (map fst vars) values) [] inputs result <$> traverse verify children

    verify (ClosedFamilyApplication loc owner table kinds inputs result children) = setSrcSpan loc $ do
      kinds <- liftZonkM (zonkTcTypes kinds)
      inputs <- liftZonkM (zonkTcTypes inputs)
      result <- liftZonkM (zonkTcType result)
      unless (all (\ty -> noFreeVarsOfType ty && isTyFamFree ty) (result : inputs ++ kinds)) $
        bad_family owner
      ClosedFamilyApplication loc owner table kinds inputs result <$> traverse verify children

    bad_family owner = failWithTc (TcRnClosedTypeEval owner
      "The operands do not determine the checked family's input and result kinds.")

    prepare (ClosedCallback loc fun ty) = setSrcSpan loc $ do
      core <- tcPrepareClosedFunctionValue fun ty
      return (OrdinaryClosedCall loc (getName fun) (return core))
    prepare (CheckedBoundary value) = return value
    prepare (ClosedLiteral name arg literal) = setSrcSpanA (getLoc arg) $ do
      loc <- getSrcSpanM
      value <- tcDecodeClosedCallOperand name literal
      case value of
        Just core -> return (OrdinaryClosedCall loc name (return core))
        Nothing -> failWithTc (TcRnClosedTypeEval name
          "The checked argument does not represent a supported closed value.")
    prepare (ClosedApplication loc fun _ _ inputs result children) = setSrcSpan loc $ do
      let owner = getName fun
      apply <- tcPrepareClosedApplication fun inputs result
      operands <- mapM (fmap (closedCallRuntimeOperand owner) . prepare) children
      return (OrdinaryClosedCall loc owner (sequence operands >>= apply))
    prepare (ClosedFamilyApplication loc owner table kinds [input] result [child]) =
      setSrcSpan loc $ case child of
        ClosedLiteral _ _ literal -> nominal (pure literal)
        _ -> do
          value <- prepare child
          projection <- prepareClosedPairProjection owner table kinds input result value
          case projection of
            Just projected -> pure projected
            Nothing -> prepareClosedCallResult input value >>= nominal
      where
        nominal operand =
          ClosedFamilyValue loc <$> prepareClosedFamilyResult owner table kinds [input] result [operand]
    prepare (ClosedFamilyApplication loc owner table kinds inputs result children) = setSrcSpan loc $ do
      operands <- zipWithM prepare_family_operand inputs children
      ClosedFamilyValue loc <$> prepareClosedFamilyResult owner table kinds inputs result operands
    prepare ClosedBoundary{} = panic "tcPlanClosedTree: unprepared boundary"

    -- The companion certifies the field that this projection leaves unread.
    prepareObservedPair side loc fun params domains args input result = runMaybeT $ do
      (tc, [left, right]) <- MaybeT (pure (splitTyConApp_maybe input))
      guard (tc == pairTyCon)
      field <- MaybeT (lookupObservedPairField side fun params (pickLR side (right, left)))
      observer <- MaybeT (lookupPairObserver side fun (length domains))
      let owner = getName fun
      available <- lift (tcHasClosedCallExecutable owner)
      permitted <- lift (canObservePairSource fun)
      guard (available && permitted)
      lift $ setSrcSpan loc $ do
        apply <- tcPrepareClosedApplication fun domains input
        operands <- mapM (fmap (closedCallRuntimeOperand owner) . prepare) args
        select <- closedPairProjectionExpression side input left right result
        reify <- tcPrepareClosedResult owner input
        reify_selected <- tcPrepareClosedResult owner result
        env <- getGblEnv
        pure $ do
          partial <- readPairRecipe owner emptyNameSet observer
          expression <- sequence operands >>= apply
          if partial then do
            selected <- reify_selected (select expression)
            recordSourceUses [(fun, Nothing)]
            let site = tcg_zany_n env
                mod = tcg_semantic_mod env
            ordinal <- readTcRef site
            updTcRef site (+ 1)
            updTcRef (tcg_evaluated_term_uses env) (`extendNameSet` tyConName field)
            let key = mkStrLitTy (fsLit (show (unitString (moduleUnit mod),
                                             moduleNameString (moduleName mod), ordinal)))
                unread = mkTyConApp field (params ++ [key])
            let (x, y) = pickLR side ((selected, unread), (unread, selected))
            pure (mkPromotedPairTy left right x y)
          else reify expression

    prepare_family_operand _ (ClosedLiteral _ _ literal) = return (return literal)
    prepare_family_operand kind child = prepare child >>= prepareClosedCallResult kind

    unsupported name = failWithTc (TcRnClosedTypeEval name
      "The call must have a fully instantiated ordinary function type.")

    bare_empty arg = case ignoreParens arg of
      L _ (HsExplicitListTy _ _ []) -> True
      _ -> False

-- | Resolve optional scalar entries without entering the pair producer.
readPairRecipe :: Name -> NameSet -> Id -> TcM Bool
readPairRecipe owner seen entry
  | name `elemNameSet` seen = pure False
  | otherwise = do
      recipe <- readRecipeEntry owner entry
      maybe (pure False) (readRecipeValue owner (extendNameSet seen name)) recipe
  where
    name = getName entry

-- | Read a recipe through the optional native symbol check.
readRecipeEntry :: Name -> Id -> TcM (Maybe String)
readRecipeEntry owner entry = do
    observe <- tcPrepareClosedObservation owner stringTy (getName entry)
    quoted <- observe (Core.Var entry)
    pure (quoted >>= readbackString)

-- | Decode a versioned terminal proof or a reference to another owner.
readRecipeValue :: Name -> NameSet -> String -> TcM Bool
readRecipeValue _ _ "!" = pure True
readRecipeValue owner seen reference
  | Just (unit, mod, occurrence, arity) <- readMaybe reference = do
      let defining = mkModule (stringToUnit unit) (mkModuleName mod)
      (table, legacy) <- lookupPairRecipe defining (mkVarOcc occurrence) arity
      rows <- maybe (pure Nothing) (readRecipeEntry owner) table
      case rows of
        Nothing -> maybe (pure False) (readPairRecipe owner seen) legacy
        Just contents -> case legacy of
          Just key
            | let name = getName key
            , not (name `elemNameSet` seen)
            , Just entries <- readMaybe contents
            , [recipe] <- [row | (occ, n, row) <- entries, occ == occurrence, n == arity]
            -> readRecipeValue owner (extendNameSet seen name) recipe
          _ -> pure False
  | otherwise = pure False

-- | Read the promoted list produced by the closed Char list codec.
readbackString :: Type -> Maybe String
readbackString ty = case splitTyConApp_maybe ty of
    Just (con, [_]) | con == promoteDataCon nilDataCon -> Just []
    Just (con, [_, char, tail_ty]) | con == promoteDataCon consDataCon ->
      (:) <$> isCharLitTy char <*> readbackString tail_ty
    _ -> Nothing

-- | Read a certified projection without first observing the whole pair.
-- See Note [Demand of an interface-only pair projection].
prepareClosedPairProjection :: Name -> TyCon -> [Kind] -> Kind -> Kind
                            -> ClosedCallValue -> TcM (Maybe ClosedCallValue)
prepareClosedPairProjection owner table kinds input result (OrdinaryClosedCall _ _ expression)
  | Just (_, [left_kind, right_kind]) <- splitTyConApp_maybe input = do
      field <- closedPairProjectionField table kinds input result
      case field of
        Just side -> do
          loc <- getSrcSpanM
          sourceCaseKindUse table kinds
          observe <- closedPairProjectionExpression side input left_kind right_kind result
          return (Just (OrdinaryClosedCall loc owner (observe <$> expression)))
        Nothing -> return Nothing
prepareClosedPairProjection _ _ _ _ _ _ = return Nothing

-- | Force the actual pair and return only its selected field.
closedPairProjectionExpression :: LeftOrRight -> Kind -> Kind -> Kind -> Kind
                               -> TcM (Core.CoreExpr -> Core.CoreExpr)
closedPairProjectionExpression side input left right result = do
    binder <- mkSysLocalM (fsLit "closed_pair") ManyTy input
    a <- mkSysLocalM (fsLit "closed_first") ManyTy left
    b <- mkSysLocalM (fsLit "closed_second") ManyTy right
    pure $ \scrutinee -> Core.Case scrutinee binder result
      [Core.Alt (Core.DataAlt (tupleDataCon Boxed 2)) [a, b] (Core.Var (pickLR side (a, b)))]

-- | Read the selected field without recording a use of the projection's equation.
closedPairProjectionField :: TyCon -> [Kind] -> Kind -> Kind -> TcM (Maybe LeftOrRight)
closedPairProjectionField table kinds input result
  | sourceCaseVisibleArity table == Just 1
  , Just (tc, [left_kind, right_kind]) <- splitTyConApp_maybe input
  , tc == tupleTyCon Boxed 2 = do
      loc <- getSrcSpanM
      let fresh_field kind = do
            name <- mkInternalName <$> newUnique <*> pure (mkTyVarOcc "field") <*> pure loc
            return (mkTyVarTy (mkTyVar name kind))
      left <- fresh_field left_kind
      right <- fresh_field right_kind
      envs <- tcGetFamInstEnvs
      let pair = mkPromotedPairTy left_kind right_kind left right
      return $ do
        reduction <- reduceTyFamApp_maybe envs Nominal table (kinds ++ [pair])
        let field = reductionReducedType reduction
        if field `tcEqType` left && result `tcEqType` left_kind then Just CLeft
        else if field `tcEqType` right && result `tcEqType` right_kind then Just CRight
        else Nothing
closedPairProjectionField _ _ _ _ = return Nothing

{- Note [Demand of an interface-only pair projection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ordinary executable route can project a pair before encoding the
result. When only the projection interface is available, encoding the
whole intermediate pair would force both fields. The checked source
family describes which field to return. Match its nominal equation on
a promoted pair of distinct fresh variables. If it returns exactly one
variable, build a Core case on the runtime pair and pass only that field
to the next application or result renderer. This forces the pair
constructor and preserves the other field's laziness.

The tentative reduction does not emit equality evidence or record a
source-family use. After choosing this route, record the same RULE and
interface dependencies as for a checked family reduction. The ordinary
operand retains its interpreter and object dependencies. No fresh type
variable enters the runtime Core or the result type.
-}

-- | Check a known enclosing kind before executing a staged call.
-- Inference and non-ground expected kinds still use checkExpKind.
checkClosedCallExpectedKind :: HsType GhcRn -> ExpKind -> Kind -> TcM ()
checkClosedCallExpectedKind rn_ty (Check expected) actual = do
  expected' <- liftZonkM (zonkTcType expected)
  when (noFreeVarsOfType expected' && isTyFamFree expected' && not (actual `tcEqType` expected')) $
    solveEqualities "checkClosedCallExpectedKind" $
      discardResult (unifyKind (Just (HsTypeRnThing rn_ty)) actual expected')
checkClosedCallExpectedKind _ (Infer _) _ = return ()

-- | Prepare an already resolved nested occurrence.
tcPlanClosedHead :: TcTyMode -> Bool -> Maybe Kind -> RdrName -> Name
                 -> [LHsType GhcRn] -> TcM (Kind, ClosedCallValue)
tcPlanClosedHead mode ordinary_call hint rdr name args
  | isSourceFamilyName name = do
      staging <- closedOperandStaging args
      executable <- tcClosedCaseExecutableOwner rdr name args staging
      case executable of
        Just owner -> tcPlanExecutableCase mode ordinary_call hint owner args
        Nothing -> do
          table <- tcLookupTyCon name
          if ordinary_call && admissibleFamilyOperands args staging && supportsClosedFamilyTree table args
            then tcPlanClosedTree mode ordinary_call hint (ReduceClosedCase table) args
            else case args of
              [arg] -> do
                loc <- getSrcSpanM
                (kind, run) <- tcPlanSourceFamilyCall mode ordinary_call name arg
                return (kind, ClosedFamilyValue loc run)
              _ -> failWithTc (TcRnNoSourceTable name)
  | otherwise = do
      env <- getGblEnv
      when (nameIsLocalOrFrom (tcg_semantic_mod env) name) $
        failWithTc (TcRnNoSourceTable name)
      thing <- tcLookupGlobal name
      case thing of
        AnId fun -> tcPlanOrdinaryCall mode ordinary_call hint fun args
        _ -> failWithTc (TcRnNoSourceTable name)

-- | Share the input and result kind constraints of a saturated constructor case.
-- Use the same tree when the producer becomes available to the linker.
tcPlanExecutableCase :: TcTyMode -> Bool -> Maybe Kind -> Id -> [LHsType GhcRn]
                     -> TcM (Kind, ClosedCallValue)
tcPlanExecutableCase mode True hint owner = tcPlanClosedTree mode True hint (RunClosedFunction owner)
tcPlanExecutableCase mode False hint owner = tcPlanOrdinaryCall mode False hint owner

-- | Preserve the ordinary execution route for an existing staged constructor case call.
-- See Note [Selecting the closed constructor case route].
tcClosedCaseExecutableOwner :: RdrName -> Name -> [LHsType GhcRn] -> Maybe Bool -> TcM (Maybe Id)
tcClosedCaseExecutableOwner rdr name args staging
  | isQual rdr || isUnqual rdr
  , Just needs_execution <- staging
  = do
      table <- tcLookupTyCon name
      let expect_eager = monomorphicEnumCaseArity (tyConKind table) == Just (length args)
          visible = filterOut isInvisibleTyConBinder (tyConBinders table)
          nominal_prefix = length args < length visible && all nominalCallback args
          staged = needs_execution &&
                   (sourceCaseVisibleArity table == Just (length args) ||
                    (not nominal_prefix && any (isJust . bareClosedCallback) args))
      owner <- if expect_eager || staged then sourceTableOwner table else pure Nothing
      case owner of
        Just fun | staged || sourceTableKeepsExecutable fun -> do
          imported <- isClosedOrdinaryImport rdr (getName fun)
          if imported
            then do
              executable <- tcHasClosedCallExecutable (getName fun)
              pure (fun <$ guard executable)
            else pure Nothing
        _ -> pure Nothing
  | otherwise = pure Nothing

-- | Recover a bare imported callback without interpreting it as a type.
bareClosedCallback :: LHsType GhcRn -> Maybe (SrcSpan, RdrName, Name)
bareClosedCallback arg
  | L _ (HsTyVar _ NotPromoted (L loc (WithUserRdr rdr name))) <- ignoreParens arg
  , isExternalName name, isVarName name || isSourceFamilyName name
  = Just (locA loc, rdr, name)
  | otherwise = Nothing

-- | A checked bare function can enter the nominal route without object code.
nominalCallback :: LHsType GhcRn -> Bool
nominalCallback arg
  | Just (_, _, name) <- bareClosedCallback arg = isSourceFamilyName name
  | otherwise = False

-- | Report an unavailable imported callback at its written operand.
checkOpenCallbackInterfaces :: TyCon -> [LHsType GhcRn] -> TcM ()
checkOpenCallbackInterfaces table args
  | lengthIs visible (length args) = zipWithM_ check visible args
  | otherwise = pure ()
  where
    visible = dropWhile isInvisibleTyConBinder (tyConBinders table)
    check (Bndr binder AnonTCB) operand
      | Just (loc, rdr, name) <- bareClosedCallback operand
      , isVarName name
      , Just (FTF_T_T, ma, mult, _, _) <- splitFunTy_maybe (varType binder)
      , ma `tcEqType` unmatchableDataConTy, isManyTy mult
      = setSrcSpan loc (failWithTc (TcRnNoSourceCallback rdr))
    check _ _ = pure ()

-- | Verify provenance of a named callback without using its equation.
checkNominalCallback :: LHsType GhcRn -> TcM ()
checkNominalCallback arg
  | Just (loc, rdr, name) <- bareClosedCallback arg, isSourceFamilyName name
  = setSrcSpan loc $ do
      fun <- tcClosedCallback rdr name
      table <- tcLookupTyCon name
      owner <- sourceTableOwner table
      unless (owner == Just fun) $
        failWithTc (TcRnNoSourceTable (getName fun))
  | otherwise = return ()

-- | Select the exact imported term, including re-exports of a certified owner.
tcClosedCallback :: RdrName -> Name -> TcM Id
tcClosedCallback rdr name = do
  owner <- closedFamilyOwner name
  imported <- isClosedOrdinaryImport rdr owner
  unless imported $ failWithTc (TcRnClosedTypeEval owner
    "A closed callback needs an ordinary non-SOURCE import.")
  thing <- tcLookupGlobal owner
  case thing of
    AnId fun -> return fun
    _ -> failWithTc (TcRnNoSourceTable owner)

-- | Check that the written name permits executable use of the exact term.
isClosedOrdinaryImport :: RdrName -> Name -> TcM Bool
isClosedOrdinaryImport rdr name = do
  rdr_env <- getGlobalRdrEnv
  let occurrence = nameOccName name
      term_rdr = case isQual_maybe rdr of
        Just (qualifier, _) -> mkRdrQual qualifier occurrence
        Nothing -> mkRdrUnqual occurrence
      imported gre = greName gre == name &&
                     any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre)
  return (any imported (lookupGRE rdr_env (LookupRdrName term_rdr (RelevantGREsFOS WantNormal))))

-- | A checked family inside an ordinary tree can also have two literal operands.
admissibleFamilyOperands :: [LHsType GhcRn] -> Maybe Bool -> Bool
admissibleFamilyOperands args = maybe False (|| lengthIs args 2)

-- | Check the complete operand syntax before selecting a staged route.
closedOperandStaging :: [LHsType GhcRn] -> TcM (Maybe Bool)
closedOperandStaging = runMaybeT . fmap or . traverse contains_staged_call
  where
    -- A tree of only checked families can stay on the nominal route.
    contains_staged_call operand
      | Just _ <- bareClosedCallback operand = return True
      | Just (_, _, head_name, args) <- nestedClosedCall operand = do
          children <- traverse contains_staged_call args
          return (isVarName head_name || or children)
      | closedConstructorLiteral operand = return False
      | otherwise = do
          rhs <- MaybeT (closedConstructorAliasRhs operand)
          -- A synonym for an ordinary type (such as Int) can be a nominal
          -- operand. The executable route requires a promoted value.
          guard (closed_value rhs)
          return False

    closed_value ty = case splitTyConApp_maybe ty of
      Just (con, args)
        | Just dc <- isPromotedDataCon_maybe con
        -> let (_, fields) = splitAt (length (dataConUserTyVars dc)) args
           in all closed_value fields
      _ -> isJust (isNumLitTy ty) || isJust (isCharLitTy ty)

-- | Support the checked unary case and one carried fallback.
supportsClosedFamilyTree :: TyCon -> [LHsType GhcRn] -> Bool
supportsClosedFamilyTree table args =
  let arity = length args
  in arity <= 2 && sourceCaseVisibleArity table == Just arity

{- Note [Selecting the closed constructor case route]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary computed operand needs the staged evaluator. If the linker can
plan execution of the checked constructor case function, use the ordinary plan before
registering any nominal family use. This preserves existing staged calls
even if later declarations add a rule for the projection. An imported
constraint, a rigid operand, or an unavailable executable still uses the
checked family. Never recover from a failed execution by trying an axiom.

A partial application that supplies only checked callbacks uses the
nominal route even when object code is available. The staged evaluator
requires a saturated call. The caller checks the callbacks' owners
before using the source family.

The availability query uses the linker dependency plan and does not load
code. It does not replace the usual stage, type or linker checks.
-}

-- | Scalar kinds admitted by the source operand grammar.
closedCallScalarKinds :: [Kind]
closedCallScalarKinds = [naturalTy, charTy, boolTy]

-- | Prepare a unary checked family in a chain of closed calls.
tcPlanSourceFamilyCall :: TcTyMode -> Bool -> Name -> LHsType GhcRn -> TcM (Kind, TcM Type)
tcPlanSourceFamilyCall mode ordinary_call name arg = do
  loc <- getSrcSpanM
  table <- tcLookupTyCon name
  unless (isSourceConstructorCase table) (sourceTableFamilyUse table)
  owner <- closedFamilyOwner name
  let bad :: String -> TcM a
      bad = failWithTc . TcRnClosedTypeEval owner
      at_operand :: TcM a -> TcM a
      at_operand = setSrcSpanA (getLoc arg)
  let (invisible, visible) = span isInvisibleTyConBinder (tyConBinders table)
      supported kind =
        any (tcEqType kind) (closedCallScalarKinds ++ map mkListTy closedCallScalarKinds)
      ground kind = noFreeVarsOfType kind && isTyFamFree kind
      ty_params = [tv | Bndr tv _ <- invisible]
      result = tyConResKind table
      unsupported = bad "The checked family does not have a supported unary input and result."
  domain <- case visible of
    [Bndr tv AnonTCB] | all isTyVar ty_params
                     , tyConArity table == length ty_params + 1 -> return (tyVarKind tv)
    _ -> unsupported
  -- Keep the existing ground source-family grammar. The polymorphic route
  -- must determine every invisible kind from a single prepared operand.
  when (null ty_params && not (supported domain && supported result)) unsupported
  unless (null ty_params || isSourceConstructorCase table) unsupported
  let specialize actual
        | null ty_params = do
            unless (actual `tcEqType` domain) $
              at_operand (closedOperandMismatch owner actual domain)
            return ([], domain, result)
        | otherwise = do
            unless (ground actual) $
              at_operand (bad "The checked family operand does not have a ground kind.")
            case tcMatchTy domain actual of
              Just subst
                | Just kinds <- traverse (Subst.lookupTyVar subst) ty_params
                , all ground kinds
                , and (zipWith (\tv k -> typeKind k `tcEqType` Subst.substTy subst (tyVarKind tv))
                               ty_params kinds)
                , let checked_domain = Subst.substTy subst domain
                      checked_result = Subst.substTy subst result
                , checked_domain `tcEqType` actual
                , ground checked_result ->
                    return (kinds, checked_domain, checked_result)
              _ -> at_operand $ do
                dflags <- getDynFlags
                bad ("The operand has kind " ++ showPpr dflags actual ++
                     ", which does not determine the checked family's input and result kinds "
                     ++ showPpr dflags domain ++ " and " ++ showPpr dflags result ++ ".")
  (kind_args, input_kind, result_kind, run_arg) <- case nestedClosedCall arg of
    Just (child_loc, child_rdr, child_name, child_args) -> do
      (kind, plan) <- setSrcSpan child_loc (tcPlanClosedHead mode ordinary_call Nothing child_rdr child_name child_args)
      (kinds, checked_domain, checked_result) <- specialize kind
      run <- prepareClosedCallResult kind plan
      return (kinds, checked_domain, checked_result, run)
    Nothing -> do
      let literal_kind = if null ty_params then closedCallLiteralKind else familyLiteralKind
      literal <- case literal_kind arg of
        Just kind -> return kind
        Nothing -> at_operand (bad "The checked family operand must be a closed literal or a closed call.")
      (kinds, checked_domain, checked_result) <-
        specialize (if null ty_params then domain else literal)
      checked <- solveEqualities "tcPlanSourceFamilyCall" $
        tc_check_lhs_type mode arg checked_domain
      zonked <- liftZonkM (zonkTcType checked)
      return (kinds, checked_domain, checked_result, return zonked)
  run <- setSrcSpan loc $
    prepareClosedFamilyResult owner table kind_args [input_kind] result_kind [run_arg]
  return (result_kind, run)

-- | Use the ordinary source name in staging diagnostics.
closedFamilyOwner :: Name -> TcM Name
closedFamilyOwner name = case sourceTableTermOcc (nameOccName name) of
  Just occ -> lookupOrig (nameModule name) occ
  Nothing -> return name

-- | Check the closed value before placing it in a nominal family application.
checkClosedFamilyOperand :: Name -> Kind -> TcM Type -> TcM Type
checkClosedFamilyOperand owner kind run = do
  value <- run
  unless (typeKind value `tcEqType` kind && noFreeVarsOfType value) $
    failWithTc (TcRnClosedTypeEval owner "The checked family operand does not have the planned closed kind.")
  return value

-- | Prepare the saturated family application after its kinds are known.
prepareClosedFamilyResult :: Name -> TyCon -> [Kind] -> [Kind] -> Kind
                          -> [TcM Type] -> TcM (TcM Type)
prepareClosedFamilyResult owner table kind_args input_kinds result_kind run_args = do
  when (isSourceConstructorCase table) (sourceCaseKindUse table kind_args)
  loc <- getSrcSpanM
  let bad :: String -> TcM a
      bad = failWithTc . TcRnClosedTypeEval owner
      checked = checkClosedFamilyOperand owner
      reduce args = do
        norm_loc <- getCtLocM AnnOrigin Nothing
        reductionReducedType <$> tcRewriteFamilyAxioms norm_loc
          (mkTyConApp table (kind_args ++ args))
  return $ setSrcSpan loc $ do
    result <- case (input_kinds, run_args) of
      ([domain], [run]) -> checked domain run >>= reduce . (:[])
      ([domain, fallback_kind], [run, fallback]) -> do
        -- See Note [Demand of a closed constructor case fallback].
        input <- checked domain run
        uniq <- newUnique
        let marker = mkTyVarTy (mkTyVar (mkInternalName uniq (mkTyVarOcc "fallback") loc) fallback_kind)
        reduced <- reduce [input, marker]
        if reduced `tcEqType` marker
          then checked fallback_kind fallback
          else if noFreeVarsOfType reduced
            then return reduced
            else bad "The checked family did not select a closed result or its fallback."
      _ -> bad "The checked family does not have a supported operand count."
    unless (typeKind result `tcEqType` result_kind && noFreeVarsOfType result) $
      bad "The checked family result does not have the planned closed kind."
    return result

{- Note [Demand of a closed constructor case fallback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A checked binary constructor case scrutinizes its first operand. Its
second operand is a carried variable. Prepare both ordinary plans before
observing either. Reduce the nominal family on the checked scrutinee and
a fresh type variable at the fallback kind. A result equal to that
variable demands the fallback. A closed result came from the scrutinee
and needs no fallback. Any other result still depends on the private
variable. Reject it before it can leave this computation.
-}

-- | Infer the kind of a closed literal accepted by a checked source family.
familyLiteralKind :: LHsType GhcRn -> Maybe Kind
familyLiteralKind arg
  | L _ (HsExplicitTupleTy _ _ [first, second]) <- ignoreParens arg = do
      first_kind <- familyLiteralKind first
      second_kind <- familyLiteralKind second
      pure (mkTyConApp (tupleTyCon Boxed 2) [first_kind, second_kind])
  | otherwise = closedCallLiteralKind arg

-- | Reject a closed call in an incomplete visible type argument.
checkCompleteClosedCall :: TcTyMode -> Name -> TcM ()
checkCompleteClosedCall mode name =
  when (mode_complete_type_arg mode == Just False) $
    failWithTc (TcRnClosedTypeEval name
      "A visible type argument containing a closed call must be complete. Replace its wildcards with types.")

-- | Admit only syntax that cannot run an ordinary call during kind checking.
-- The value decoder checks the actual DataCon identity.
closedConstructorLiteral :: LHsType GhcRn -> Bool
closedConstructorLiteral arg = case unLoc (ignoreParens arg) of
  HsExplicitListTy _ _ elements -> all closedConstructorLiteral elements
  HsExplicitTupleTy _ _ fields -> lengthIs fields 2 && all closedConstructorLiteral fields
  HsTyLit _ HsNatural{} -> True
  HsTyLit _ HsChar{} -> True
  syntax -> case splitHsAppTys syntax of
    (L _ (HsTyVar _ _ (L _ (WithUserRdr _ con))), args)
      | isDataConName con -> all closed_argument args
    _ -> False
  where
    closed_argument HsArgPar{} = True
    closed_argument (HsValArg _ operand) = closedConstructorLiteral operand
    closed_argument _ = False

-- | A bare imported nullary synonym can name a constructor operand.
-- Its checked expansion still has to pass runtime operand admission.
closedConstructorAlias :: LHsType GhcRn -> TcM Bool
closedConstructorAlias = fmap isJust . closedConstructorAliasRhs

-- | Retrieve the checked expansion of a bare imported nullary synonym.
closedConstructorAliasRhs :: LHsType GhcRn -> TcM (Maybe Type)
closedConstructorAliasRhs arg
  | L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))) <- ignoreParens arg
  , isExternalName name, isTyConName name = do
      this_mod <- tcg_semantic_mod <$> getGblEnv
      if nameIsLocalOrFrom this_mod name then return Nothing else do
        thing <- tcLookupGlobal name
        return $ case thing of
          ATyCon tc | tyConArity tc == 0 -> synTyConRhs_maybe tc
          _ -> Nothing
  | otherwise = return Nothing

-- | Check the literal at the declared input kind and build its runtime value.
tcClosedTypeCallArgument :: TcTyMode -> Name -> Type -> LHsType GhcRn -> TcM Core.CoreExpr
tcClosedTypeCallArgument mode name domain arg = do
  checkCompleteClosedCall mode name
  checked <- solveEqualities "tcClosedTypeCallArgument" $
    tc_check_lhs_type mode arg domain
  value <- setSrcSpanA (getLoc arg) $
    tcDecodeClosedCallOperand name =<< liftZonkM (zonkTcType checked)
  maybe (bad "The checked argument does not have the expected literal representation.") return value
  where
    bad :: String -> TcM a
    bad = failWithTc . TcRnClosedTypeEval name

-- | Identify candidate kinds without constructing or evaluating a value.
-- The ordinary type checker checks that all elements of a list have that kind.
closedCallLiteralKind :: LHsType GhcRn -> Maybe Kind
closedCallLiteralKind arg
  | L _ (HsExplicitListTy _ _ elements) <- ignoreParens arg =
      case elements of
        [] -> Just (mkListTy naturalTy)
        first : rest
          | all (isJust . scalarKind) rest -> mkListTy <$> scalarKind first
        _ -> Nothing
  | otherwise = scalarKind arg
  where
    scalarKind operand = case ignoreParens operand of
      L _ (HsTyLit _ HsNatural{}) -> Just naturalTy
      L _ (HsTyLit _ HsChar{}) -> Just charTy
      L _ (HsTyVar _ _ (L _ (WithUserRdr _ con)))
        | con == trueDataConName || con == falseDataConName -> Just boolTy
      _ -> Nothing

{- Note [Closed source calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Serokell's closed term evaluator example uses an ordinary length call as
a vector index. Support imported functions on Natural, Char, Bool, or a
promoted list of those literals. The renamer keeps the resolved term Name
after trying the ordinary type and checked source-table paths. An existing
type binder still wins. An explicit forall or the RequiredTypeArguments
capture rule can leave an unqualified term name available.

For a constructor operand, instantiate the producer telescope once and
check all operands together. A later input can determine a kind omitted
by an earlier constructor. Collect the kinds of nested calls without
running their actions. Complete the source kind and dictionary checks
before executing any child. A bare empty list keeps the existing Natural
default when other operands leave its element kind undetermined.

Accept the explicit promoted-list syntax chosen by the parser, with or
without a tick. Under ListTuplePuns, an unticked singleton list is a list
type and an unticked empty list is a type constructor. Neither is an
explicit promoted list in the parsed syntax.

Literal arguments supply the kinds that specialize a polymorphic
function. In a saturated call, match all known operand kinds together
before selecting kinds for empty lists. A later operand can determine
the earlier empty list's element kind. Check each literal with the ordinary
kind checker before decoding its runtime value. An expected result can
also determine the element kind when the producer relates its input to
its result. Keep the [Natural] default when the element kind is still
unknown, as in Prelude.reverse '[] without an expected kind.

For a nested call, check the outer call's staging
first. Prepare the inner call and use its inferred result kind to
specialize the outer call. No function runs until the whole chain has
checked kinds, staging, and evidence.

A constructor case can share its input and result kinds with the
surrounding ordinary calls. Instantiate its invisible telescope before
checking the operands. Check all kinds and ordinary class dictionaries,
then use the nominal family rewriter with the checked rule and interface
dependencies. See Note [Demand of a closed constructor case fallback]
for the binary case.

The normal checker still checks a family at the head of an application.
If that family expects a ground kind for an ordinary call, check that kind
before running the call. This leaves pure and rigid family applications
on the normal checking path.

Compose adjacent ordinary applications as one typed Core expression. For
example, first (make 7) can select one field of a private datatype while
the other field contains bottom. Rendering the complete intermediate
value would force that unused field. A checked family needs a Type
argument, so its boundary still requires readback. Check constructor
visibility wherever a value must be returned to the typechecker.

Check each application's inputs at their own spans. Run the composed
expression at the span of its outer observation. An exception raised by
a thunk need not come from the function that returned that thunk.
The expected result kind can specialize an operand when the declared
function type relates their kinds.

For map not '[True, False], the first operand is an ordinary function.
The renamer first checks the type namespace at that occurrence. Keep the
written qualifier when looking up the imported term, including the term
that owns a generated source family. Check its ordinary arrow type with
the other operands. Pass the specialized Core expression to the outer
call. Only the result list needs to cross the boundary back into Type.
Record the callback among the evaluated names so recompilation accounts
for its unfolding as well as any linked code.

A partial signature can use this path when the chain ends in a literal.
A call on a signature wildcard continues to use the inference retry path.

The declared domain must determine all type variables and every class
constraint must become ground. The ordinary expression checker solves
those constraints before evaluation.
Elaborate Natural, Char, and Bool results to literals or promoted
constructors. Elaborate lists of these results to promoted lists. See
Note [Closed literal type evaluation] in GHC.Tc.Gen.Splice.

A visible type argument permits wildcards even when it has none. Admit
complete visible arguments at this entry point using the check described
in Note [Retained Pi type arguments].
-}

{- Note [Source opaque call indices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A complete local type can mention a lexical function in a saturated call,
for example Vec Int (f n). The renamer keeps the actual term Names. Register
eligible function heads in mode_indices along with scalar indices. Require
the closed ordinary Many arrow accepted by nativeIndexFunctionTys_maybe.
Keep function-head eligibility separate from scalar-index eligibility.

Recognize the registered head before tcInferTyApps. Check its sole value
argument at the declared domain, then form TermAppTy at the result domain.
Ignore parentheses. With no arguments, form TermVarTy at the complete fixed
U function type. Reject kind arguments and more than one value argument.
Canonical ordinary
application of that value forms TermAppTy, including after a saturated family
exposes it. This adds no matchable application or inverse rule. Erased type
variables keep their ordinary application path and lookup precedence.

This rule checks a type. It does not admit f n as a retained runtime argument
or evaluate the call. Strict source cases use the separate checked call
refinement path. Validity checks the actual lexical binding and argument kind.
No type metavariable receives a runtime index.

Agda v2.8.0 checks an application argument before substituting it into the
codomain. See Rules/Application.hs 774--808, blob
82ba94ac864d19b947d4c92b9af252b0fe81347a. This is prior art for domain-directed
checking. The fixed domains, Many restriction and FC node are GHC choices.
See docs/dependent-haskell/REFERENCES.md for the pinned source record.
-}

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
-- tcInferTyApps_nosat preserves the remaining invisible binders for :kind
-- and for checking an application against an expected kind.
tcInferTyApps
    :: TcTyMode
    -> LHsType GhcRn        -- ^ Function (for printing only)
    -> TcType               -- ^ Function
    -> [LHsTypeArg GhcRn]   -- ^ Args
    -> TcM (TcType, TcKind) -- ^ (f args, result kind)
{- Note [Implicit ordinary-arrow matchability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FUN has a specified invisible matchability parameter before multiplicity.
When a source application must instantiate an omitted Matchability binder,
use U at TypeLevel and M at KindLevel. This rule also applies to user-defined
Matchability binders. An explicit @ argument is checked normally.

Checked declaration kinds defer omitted metadata until the declaration prefix
has supplied its constraints. See Note [Checked declaration-kind matchability].

Do not instantiate trailing binders in the application loop. First compare
actual and expected kinds in checkExpectedKind. Instantiate only the excess
invisible binders. This preserves a polymorphic argument when the expected
kind has the same invisible forall prefix. For example, P must retain its
binder when P :: forall (m :: Matchability). Type is passed to a constructor
that expects this kind. The same rule preserves bare FUN in that position.

Use the same mode-aware instantiation in the application loop when a real
source argument requires it, and when family saturation requires it. The
fully instantiated inference entry point tcInferLHsTypeKind uses TypeLevel.
The unsaturated inference entry point keeps its invisible forall binders.
See Note [Do not always instantiate eagerly in types].

This is the initial concrete policy for the ordinary-arrow port. It does not
infer or generalize a new matchability quantifier. The forall stage must
revisit the broader polymorphic inference policy from proposal #242. See
Note [Ordinary arrow matchability] in GHC.Core.TyCo.Rep.
-}

tcInferTyApps mode hs_ty fun hs_args
  = do { (f_args, res_k) <- tcInferTyApps_nosat TypeLevel Nothing mode hs_ty fun hs_args
       ; saturateFamApp mode f_args res_k }

-- | Apply the arguments while retaining the remaining invisible binders.
tcInferTyApps_nosat :: TypeOrKind -> Maybe Kind -> TcTyMode -> LHsType GhcRn -> TcType
                    -> [LHsTypeArg GhcRn] -> TcM (TcType, TcKind)
tcInferTyApps_nosat index_level expected_kind mode orig_hs_ty fun orig_hs_args
  = do { traceTc "tcInferTyApps {" (ppr orig_hs_ty $$ ppr orig_hs_args)
       ; keep_inferred_prefix <- case (expected_kind, fun) of
           (Just expected, TyConApp tc [])
             | hasInferredMatchability tc
             , isUnsaturatedUserFamily tc []
             , all is_par orig_hs_args
             , invisibleBndrCount expected == invisibleBndrCount (typeKind fun)
             -> xoptM LangExt.ExperimentalDependentTypes
           _ -> pure False
       ; fun <- case (mode_synonym_metadata mode, fun) of
           _ | keep_inferred_prefix -> pure fun
           (Just (get_indices, _), TyConApp tc []) | hasInferredMatchability tc -> do
             let binders = tyConBinders tc
                 indices = takeWhile inferredMatchabilityIndex binders
                 domains = [tyVarKind (binderVar b) | b <- binders, isVisibleTyConBinder b]
                 from_argument b
                   | TypeSynonymFlavour <- tyConFlavour tc =
                       any (synonymCallbackIndex (binderVar b)) domains
                   | otherwise = False
             args <- get_indices (tyConName tc) (map from_argument indices)
             pure (mkTyConApp tc args)
           (Nothing, TyConApp tc [])
             | TypeLevel <- mode_tyki mode
             , isTypeFamilyTyCon tc ||
               (isTcTyCon tc && is_closed_family tc && infer_family_arrows)
             , hasInferredMatchability tc
             , let indices = takeWhile inferredMatchabilityIndex (tyConBinders tc)
             , any (familyKindUsesMatchability tc) indices -> do
                 defaults <- tcl_kind_arrow_defaults <$> getLclEnv
                 let policy = mode_callback_arrow mode
                     default_level = case policy of
                       DefaultUnsignedFamilyArrows -> KindLevel
                       _ -> index_level
                     choose b
                       | not (familyKindUsesMatchability tc b) = pure unmatchableDataConTy
                       | infer_family_arrows = newFlexiTyVarTy matchabilityTy
                       | Just (owner, _) <- defaults = fresh_kind_arrow default_level owner
                       | otherwise = pure (defaultForAllMatchability default_level)
                 case (policy, defaults, index_level) of
                   (OrdinaryArrowChecking, Nothing, TypeLevel) -> pure fun
                   _ -> mkTyConApp tc <$> traverse choose indices
           _ -> pure fun
       ; (f_args, res_k) <- go_init 1 fun orig_hs_args Nothing
       ; traceTc "tcInferTyApps }" (ppr f_args <+> dcolon <+> ppr res_k)
       ; return (f_args, res_k) }
  where

    infer_family_arrows
      | InferUnsignedFamilyArrows <- mode_callback_arrow mode = True
      | otherwise = False

    is_closed_family tc = case tyConFlavour tc of
      ClosedTypeFamilyFlavour -> True
      _ -> False

    -- go_init just initialises the auxiliary
    -- arguments of the 'go' loop
    go_init n fun all_args checked_arg
      = go n fun empty_subst fun_ki all_args checked_arg
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
       -> Maybe (TcType, TcKind) -- Checked current visible argument
       -> TcM (TcType, TcKind)  -- Result type and its kind
    -- INVARIANT: in any call (go n fun subst fun_ki args checked_arg)
    --               typeKind fun  =  subst(fun_ki)
    -- So the 'subst' and 'fun_ki' arguments are simply
    -- there to avoid repeatedly calling typeKind.
    --
    -- Reason for INVARIANT: to support the Purely Kinded Type Invariant
    -- it's important that if fun_ki has a forall, then so does
    -- (typeKind fun), because the next thing we are going to do
    -- is apply 'fun' to an argument type.

    -- Dispatch on all_args first, for performance reasons
    go n fun subst fun_ki all_args checked_arg = case (all_args, tcSplitPiTy_maybe fun_ki) of

      ---------------- No user-written args left. We're done!
      ([], _) -> return (fun, substTy subst fun_ki)

      ---------------- HsArgPar: We don't care about parens here
      (HsArgPar _ : args, _) -> go n fun subst fun_ki args checked_arg

      ---------------- HsTypeArg: a kind application (fun @ki)
      (HsTypeArg _ hs_ki_arg : hs_args, Just (_ma, ki_binder, inner_ki)) ->
        case ki_binder of

        -- FunTy with PredTy on LHS, or ForAllTy with Inferred
        Named (Bndr kv Inferred)         -> instantiate kv Inferred inner_ki

        Named (Bndr _ Specified) ->  -- Visible kind application
          do { traceTc "tcInferTyApps (vis kind app)"
                       (vcat [ ppr ki_binder, ppr hs_ki_arg
                             , ppr (piTyBinderType ki_binder)
                             , ppr subst ])

             ; let exp_kind = substTy subst $ piTyBinderType ki_binder
             ; arg_mode <- mkHoleMode KindLevel HM_VTA
                   -- HM_VKA: see Note [Wildcards in visible kind application]
             ; ki_arg <- addErrCtxt (FunAppCtxt (FunAppCtxtTy orig_hs_ty hs_ki_arg) n) $
                         tc_check_lhs_type (arg_mode { mode_indices = mode_indices mode })
                                           hs_ki_arg exp_kind

             ; traceTc "tcInferTyApps (vis kind app)" (ppr exp_kind)
             ; (subst', fun') <- mkAppTyM subst fun ki_binder ki_arg
             ; go (n+1) fun' subst' inner_ki hs_args Nothing }

        -- Attempted visible kind application (fun @ki), but fun_ki is
        --   forall k -> blah   or   k1 -> k2
        -- So we need a normal application.  Error.
        _ -> ty_app_err hs_ki_arg $ substTy subst fun_ki

      -- No binder; try applying the substitution, or fail if that's not possible
      (HsTypeArg _ ki_arg : _, Nothing) -> try_again_after_substing_or $
                                           ty_app_err ki_arg substed_fun_ki

      ---------------- HsValArg: a normal argument (fun ty)
      (HsValArg _ arg : args, Just (_ma, ki_binder, inner_ki))
        -- next binder is invisible; need to instantiate it
        | Named (Bndr kv flag) <- ki_binder
        , isInvisibleForAllTyFlag flag   -- ForAllTy with Inferred or Specified
         -> if mentions_index arg
            then do { (arg_ty, arg_kind) <- case checked_arg of
                        Just checked -> return checked
                        Nothing ->
                          do { (arg_ty, arg_kind) <-
                                 addErrCtxt (FunAppCtxt (FunAppCtxtTy orig_hs_ty arg) n) $
                                   tc_infer_lhs_type mode arg
                             ; [arg_ty, arg_kind] <- liftZonkM $ zonkTcTypes [arg_ty, arg_kind]
                             ; return (arg_ty, arg_kind) }
                    ; let checked = Just (arg_ty, arg_kind)
                    ; case native_choices subst fun_ki arg_kind of
                        Just (binders, choices, result_kind) ->
                          do { (subst', fun') <- foldM
                                 (\(s, f) (binder, choice) -> mkAppTyM s f binder choice)
                                 (subst, fun) (zipEqual binders choices)
                             ; valid <- native_scope
                                 (arg_ty : arg_kind : fun' : substTy subst' result_kind : choices)
                             ; if valid
                               then go n fun' subst' result_kind all_args checked
                               else instantiate_checked kv flag inner_ki checked }
                        Nothing -> instantiate_checked kv flag inner_ki checked }
            else instantiate kv flag inner_ki

        -- "normal" case
        | otherwise
         -> do { traceTc "tcInferTyApps (vis normal app)"
                          (vcat [ ppr ki_binder
                                , ppr arg
                                , ppr (piTyBinderType ki_binder)
                                , ppr subst ])
                ; let exp_kind = substTy subst $ piTyBinderType ki_binder
                ; arg' <- addErrCtxt (FunAppCtxt (FunAppCtxtTy orig_hs_ty arg) n) $
                          case checked_arg of
                            Nothing -> check_kind_value ki_binder inner_ki args arg exp_kind
                            Just (arg_ty, arg_kind) ->
                              checkExpectedKind mode (unLoc arg) arg_ty arg_kind exp_kind
                ; traceTc "tcInferTyApps (vis normal app) 2" (ppr exp_kind)
                ; (subst', fun') <- mkAppTyM subst fun ki_binder arg'
                ; go (n+1) fun' subst' inner_ki args Nothing }

          -- no binder; try applying the substitution, or infer another arrow in fun kind
      (HsValArg _ _ : _, Nothing)
        -> try_again_after_substing_or $
           do { let arrows_needed = n_initial_val_args all_args
              ; co <- matchExpectedFunKind (HsTypeRnThing $ unLoc hs_ty)
                          missing_arrow arrows_needed substed_fun_ki

              ; fun' <- liftZonkM $ zonkTcType (fun `mkCastTy` co)
                     -- This zonk is essential, to expose the fruits
                     -- of matchExpectedFunKind to the 'go' loop

              ; traceTc "tcInferTyApps (no binder)" $
                   vcat [ ppr fun <+> dcolon <+> ppr fun_ki
                        , ppr arrows_needed
                        , ppr co
                        , ppr fun' <+> dcolon <+> ppr (typeKind fun')]
              ; go_init n fun' all_args checked_arg }
                -- Use go_init to establish go's INVARIANT
      where
        instantiate kv flag inner_ki
          = instantiate_checked kv flag inner_ki checked_arg

        instantiate_checked kv flag inner_ki checked
          = do { traceTc "tcInferTyApps (need to instantiate)"
                         (vcat [ ppr kv, ppr subst])
               ; (subst', arg') <- choose_matchability kv flag inner_ki
               ; go n (mkAppTy fun arg') subst' inner_ki all_args checked }
                 -- Because tcInvisibleTyBinder instantiate ki_binder,
                 -- the kind of arg' will have the same shape as the kind
                 -- of ki_binder.  So we don't need mkAppTyM here.

        choose_matchability tv flag inner_ki
          -- See Note [Callback arrows in inferred synonym kinds] in GHC.Tc.TyCl.
          | Inferred <- flag
          , tyVarKind tv `tcEqType` matchabilityTy
          , Just (tc, _) <- tcSplitTyConApp_maybe fun
          , TypeSynonymFlavour <- tyConFlavour tc
          , any (synonymCallbackIndex tv) (take supplied_args (visible_domains inner_ki))
          = do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
               ; case owner of
                   Just (defaults, _) -> bind <$> fresh_kind_arrow KindLevel defaults
                   Nothing -> fresh }
          -- See Note [Inferred datatype callback arrows].
          | Inferred <- flag
          , tyVarKind tv `tcEqType` matchabilityTy
          , Just (tc, _) <- tcSplitTyConApp_maybe fun
          , case tyConFlavour tc of
              DataTypeFlavour -> True
              NewtypeFlavour -> True
              _ -> False
          , any (callback_arrow tv) (take supplied_args (visible_domains inner_ki))
          = case mode_callback_arrow mode of
              InferUnsignedFamilyArrows -> fresh
              InferDatatypeArrows {} -> fresh
              _ -> do { defaults <- tcl_kind_arrow_defaults <$> getLclEnv
                      ; case defaults of
                          Just (owner, _) ->
                            bind <$> fresh_kind_arrow (mode_tyki mode) owner
                          Nothing -> default_binder tv }
          | Specified <- flag
          , TypeLevel <- mode_tyki mode
          , Nothing <- mode_holes mode
          , HsValArg{} : _ <- dropWhile is_par all_args
          , substTy subst (tyVarKind tv) `tcEqType` matchabilityTy
          = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
               ; if not enabled
                 then default_binder tv
                 else do
                   { owner <- tcl_kind_arrow_defaults <$> getLclEnv
                   ; case owner of
                       Just (defaults, _)
                         | Just (tc, _) <- tcSplitTyConApp_maybe fun
                         , isTypeFamilyTyCon tc || isAlgTyCon tc
                         , higher_order_use tv inner_ki
                         -> do { ma <- fresh_kind_arrow TypeLevel defaults
                               ; return (extendTvSubstAndInScope subst tv ma, ma) }
                       _ -> default_binder tv } }
          | otherwise = default_binder tv
          where
            bind ma = (extendTvSubstAndInScope subst tv ma, ma)
            fresh = bind <$> newFlexiTyVarTy matchabilityTy

        callback_arrow tv domain
          | Just (arrow, _, _) <- tcSplitFunTy_maybe domain = getTyVar_maybe arrow == Just tv
          | otherwise = False

        supplied_args = length [() | HsValArg{} <- all_args]

        higher_order_use tv inner_ki =
          any (isJust . tcSplitFunTy_maybe) (filter uses_index domains) &&
          (maybe False (isJust . tcSplitFunTy_maybe) expected_kind ||
           lengthAtLeast (filter uses_index (take supplied_args domains)) 2)
          where
            domains = visible_domains inner_ki
            uses_index domain = tv `elemVarSet` tyCoVarsOfType domain

        default_binder =
          tcInstInvisibleTyBinder (matchabilityInstantiationMode mode) subst

        -- See Note [Omitted matchability in higher-order arguments].
        visible_domains kind
          | Just (_, binder, rest) <- tcSplitPiTy_maybe kind
          , isInvisiblePiTyBinder binder = visible_domains rest
          | Just (_, binder, rest) <- tcSplitPiTy_maybe kind
          = piTyBinderType binder : visible_domains rest
          | otherwise = []

        try_again_after_substing_or fallthrough
          | not (isEmptyTCvSubst subst)
          = go n fun zapped_subst substed_fun_ki all_args checked_arg
          | otherwise
          = fallthrough

        zapped_subst   = zapSubst subst
        substed_fun_ki = substTy subst fun_ki
        hs_ty          = appTypeToArg orig_hs_ty (take (n-1) orig_hs_args)

    -- A visible argument is used as a kind when it supplies the whole kind
    -- of the next visible argument. Keep this limited to the checked telescope.
    -- An indexed family may expose its metadata on reduction, as in
    -- Bridge (K 'True) Maybe. Use the same owned default as for a bare
    -- arrow so the equality solver can select M before the U default.
    check_kind_value binder inner args arg expected
      | TypeLevel <- mode_tyki mode
      , isEmptyNameEnv (mode_indices mode)
      , Named (Bndr tv _) <- binder
      , isLiftedTypeKind expected
      , HsValArg{} : _ <- dropWhile is_par args
      , Just (_, next, _) <- tcSplitPiTy_maybe inner
      , not (isInvisiblePiTyBinder next)
      , Just kind_var <- getTyVar_maybe (piTyBinderType next)
      , kind_var == tv
      = do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
           ; case owner of
               Nothing -> tc_check_lhs_type mode arg expected
               Just (defaults, _) -> check_arrows defaults arg expected }
      | otherwise = tc_check_lhs_type mode arg expected

    is_par HsArgPar{} = True
    is_par _ = False

    check_arrows defaults (L _ (HsParTy _ arg)) expected =
      check_arrows defaults arg expected
    check_arrows defaults (L loc (HsFunTy _ mult domain range)) expected =
      setSrcSpanA loc $
      tc_fun_type_x (check_arrows defaults) (fresh_kind_arrow TypeLevel defaults)
                    mode mult domain range (Check expected)
    check_arrows defaults arg expected =
      let deferred_mode = case mode_synonym_metadata mode of
            Just _ -> mode
            Nothing -> mode {mode_synonym_metadata =
                       Just (\_ callbacks -> traverse (const (fresh_kind_arrow TypeLevel defaults)) callbacks
                            , Nothing)}
      in tc_check_lhs_type deferred_mode arg expected

    missing_arrow
      | infer_family_arrows =
          Just (newFlexiTyVarTy matchabilityTy)
      | CheckFamilyRhsArrows <- mode_callback_arrow mode =
          Just $ do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
                    ; case owner of
                        Just (defaults, _) -> fresh_kind_arrow KindLevel defaults
                        Nothing -> pure matchableDataConTy }
      | InferDatatypeArrows parameters <- mode_callback_arrow mode
      , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))) <- ignoreParens orig_hs_ty
      , name `elemNameSet` parameters =
          Just (newFlexiTyVarTy matchabilityTy)
      | Just (_, callback_arrow) <- mode_synonym_metadata mode
      , L _ (HsTyVar _ NotPromoted (L _ (WithUserRdr _ name))) <- ignoreParens orig_hs_ty
      , isTyVarName name = callback_arrow
      | otherwise = Nothing

    fresh_kind_arrow level defaults =
      do { ma <- newFlexiTyVar matchabilityTy
         ; updTcRef defaults ((ma, level) :)
         ; return (mkTyVarTy ma) }

    -- Detect a registered runtime Name in a source argument.
    -- See Note [Immutable native kind arguments].
    mentions_index :: Data a => a -> Bool
    mentions_index thing
      | Just (L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))))
          <- (cast thing :: Maybe (LHsType GhcRn))
      = isJust (lookupNameEnv (mode_indices mode) name)
      | Just _ <- (cast thing :: Maybe Name) = False
      | Just _ <- (cast thing :: Maybe Type) = False
      | otherwise = or (gmapQ mentions_index thing)

    -- Match complete invisible choices from an actual native kind.
    native_choices :: Subst -> TcKind -> TcKind
                   -> Maybe ([PiTyBinder], [TcType], TcKind)
    native_choices subst kind actual
      | hasNativeDependentExpr (Core.Type actual)
      , let (binders, result) = invisible_prefix kind
            tvs = [tv | Named (Bndr tv _) <- binders]
      , not (null tvs), all isTyVar tvs
      , Just (_ma, visible, _) <- tcSplitPiTy_maybe result
      , let formal = substTy subst (piTyBinderType visible)
            bind_outer tv _ | tv `elem` tvs = BindMe
                            | otherwise = DontBindMe
            target_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType actual))
            match_subst = Subst.extendSubstInScopeSetFromSource target_subst
                            (tyCoVarsOfTypes [formal, substTy subst kind])
      , Just matched <- tcMatchTyX_BM bind_outer match_subst formal actual
      , Just (complete, choices) <- choose matched tvs tvs
      , substTy complete formal `tcEqType` actual
      = Just (binders, choices, result)
      | otherwise = Nothing
      where
        choose matched _ [] = Just (matched, [])
        choose matched all_tvs (tv : tvs) = do
          let expected_kind = substTy matched (substTy subst (tyVarKind tv))
          choice <- if expected_kind `tcEqType` matchabilityTy
                    then do { tyki <- matchabilityInstantiationMode mode
                            ; let ma = defaultForAllMatchability tyki
                            ; guard (maybe True (`tcEqType` ma) (Subst.lookupTyVar matched tv))
                            ; return ma }
                    else Subst.lookupTyVar matched tv
          guard (typeKind choice `tcEqType` expected_kind)
          guard (not (any (`elemVarSet` tyCoVarsOfType choice) all_tvs))
          (complete, choices) <- choose (extendTvSubstAndInScope matched tv choice) all_tvs tvs
          return (complete, choice : choices)

    -- Check runtime metadata against the source-owned lexical context.
    native_scope :: [TcType] -> TcM Bool
    native_scope tys = do
      bindings <- mapM (liftZonkM . zonkId) (nonDetNameEnvElts (mode_indices mode))
      let metadata = tyCoVarsOfTypes (concatMap (\v -> [varType v, idMult v]) bindings)
          in_scope = extendInScopeSetList (mkInScopeSet metadata) bindings
          valid v = isNativeOpaqueValueId v
                 && maybe False (\binding -> isNativeOpaqueValueId binding
                                 && varType binding `tcEqType` varType v)
                                (lookupInScope in_scope v)
      indices <- mapM (liftZonkM . zonkId)
                   (filter isNonCoVarId (tyCoVarsOfTypesList tys))
      return (all valid indices)

    -- Observe the invisible prefix without rebuilding its annotations.
    invisible_prefix kind
      | Just (_ma, binder@(Named (Bndr _ flag)), inner) <- tcSplitPiTy_maybe kind
      , isInvisibleForAllTyFlag flag
      = let (binders, result) = invisible_prefix inner
        in (binder : binders, result)
      | otherwise = ([], kind)

    n_initial_val_args :: [HsArg p tm ty] -> Arity
    -- Count how many leading HsValArgs we have
    n_initial_val_args (HsValArg {} : args) = 1 + n_initial_val_args args
    n_initial_val_args (HsArgPar {} : args) = n_initial_val_args args
    n_initial_val_args _                    = 0

    ty_app_err arg ty
      = failWith $ TcRnInvalidVisibleKindArgument arg ty

{- Note [Omitted matchability in higher-order arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider Receiver (Select 'True) 'Match. Both Receiver and Select have a
specified invisible matchability parameter. The kind of the last argument
of Receiver fixes M. When checking the family prefix first, defaulting
either index to U would lose this choice.

In an experimental type application, defer an omitted index used by
a function-valued domain when another supplied domain shares that index.
When an argument is checked against a function kind, allow its omitted
index to agree with the expected kind. This also checks the inner family
prefix in the example. The enclosing declaration solver collects those
equalities and defaults any remaining index to U.

An isolated partial application with an inferred kind keeps its original
default. Otherwise its synonym kind could gain an extra invisible binder.
An explicit kind argument and a family pattern keep their usual paths.
-}

{- Note [Immutable native kind arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider Count :: forall a. List a -> Nat and xs :: List (Vec Int n), where
n is an earlier runtime index. Instantiating a with a mutable meta before
checking xs requires that meta to contain n. The native escape check correctly
rejects that assignment. Choose a from the checked kind of xs instead.

Before instantiating an invisible prefix, inspect whether the source argument
mentions an actual Name in mode_indices. Infer that argument once and cache
its Type and kind. This also handles a constructor or saturated family around
the index: inference checks its smaller source arguments first. Applications
without registered runtime Names keep the ordinary checking path.

When the checked kind contains native metadata, match the visible domain with
tcMatchTyX_BM. Only the pending invisible variables can bind. Require complete
choices, check their substituted kinds, and reject free pending variables in
those choices. Seed matching with the actual target's free variables, then add
source kind variables with extendSubstInScopeSetFromSource. Target entries
retain their prepared metadata. Check the entire instantiated domain too.
Preserve the existing
mode default for Matchability binders; do not infer an omitted M/U choice from
the argument. A checked declaration's deferred default keeps its old path.

Apply a successful choice through mkAppTyM. It preserves the Purely Kinded
Type Invariant and the original forall annotations. Then run checkExpectedKind
on the cached argument. If matching fails, instantiate normally and retain
the same cache. A smaller pending prefix can try matching again. Never repeat
argument inference, splice finalizers or constraints after a failed candidate.
The cache survives a kind-substitution or arrow-exposure restart and is cleared
when its visible argument is consumed.

Check runtime occurrences against mode_indices and its deep earlier metadata
dependencies. Direct mode entries take priority. This covers signature binders
which are not ATcIds yet. It also covers an ambient xs whose kind mentions an
earlier n not written separately in this argument. The ambient resolver already
checked xs. Zonk the original prototypes and compare their complete types and
Many multiplicities. Do not consult a global type environment or IdInfo.

This changes immutable instantiation, not meta assignment or native admission.
The source argument supplies its lexical scope. Ordinary kind checking and
complete-signature validity still check the resulting type. No new quantifier,
runtime use, family evaluator or escape exception is introduced.
-}

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


saturateFamApp :: TcTyMode -> TcType -> TcKind -> TcM (TcType, TcKind)
-- Precondition for (saturateFamApp mode ty kind):
--     typeKind ty = kind
--
-- If 'ty' is an unsaturated family application with trailing
-- invisible arguments, instantiate them.
-- See Note [saturateFamApp]

saturateFamApp mode ty kind
  | Just (tc, args) <- tcSplitTyConApp_maybe ty
  , tyConMustBeSaturated tc
  , let n_to_inst = tyConArity tc - length args
  = do { (extra_args, ki') <- tcInstInvisibleTyBindersN (matchabilityInstantiationMode mode) n_to_inst kind
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

An expected kind can ask for a suffix of the invisible binders of a
partial user family. For example, F True might have kind
  forall a. forall b. a
where the receiver expects forall b. Bool. Keep the partial application
until checkExpectedKind. It instantiates a, checks the remaining forall,
and learns a ~ Bool. Saturating both binders here would discard b before
the receiver could use it. If a bare family has inferred matchability
parameters and the expected kind needs its whole invisible telescope,
keep those parameters before the argument loop as well. For example,
a receiver can take the polymorphic Map itself before applying it at a
particular callback matchability. Inference without an expected kind
keeps the ordinary saturation rule. See Note [Partial type family applications]
in GHC.Core.Type for the eligible families.
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
                  => TcTyMode
                  -> HsType GhcRn       -- ^ type we're checking (for printing)
                  -> TcType             -- ^ type we're checking
                  -> TcKind             -- ^ the known kind of that type
                  -> TcKind             -- ^ the expected kind
                  -> TcM TcType
-- Just a convenience wrapper to save calls to 'ppr'
checkExpectedKind mode hs_ty ty act_kind exp_kind
  = do { traceTc "checkExpectedKind" (ppr ty $$ ppr act_kind)

       ; (new_args, act_kind') <- tcInstInvisibleTyBindersN (matchabilityInstantiationMode mode) n_to_inst act_kind

       ; let origin = TypeEqOrigin { uo_actual    = act_kind'
                                   , uo_expected  = exp_kind
                                   , uo_thing     = Just (HsTypeRnThing hs_ty)
                                   , uo_invisible = Nothing } -- the hs_ty is visible

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


-- tyr <- checkExpKind mode hs_ty ty (act_ki :: Kind) (exp_ki :: ExpKind)
--     requires that `ty` has kind `act_ki`
-- It checks that the actual kind `act_ki` matches the expected kind `exp_ki`
-- and returns `tyr`, a possibly-casted form of `ty`, that has precisely kind `exp_ki`
-- `hs_ty` is purely for error messages
checkExpKind :: TcTyMode -> HsType GhcRn -> TcType -> TcKind -> ExpKind -> TcM TcType
checkExpKind mode rn_ty ty ki (Check ki') =
  checkExpectedKind mode rn_ty ty ki ki'
checkExpKind _mode _rn_ty ty ki (Infer cell) = do
  -- NB: do not instantiate.
  -- See Note [Do not always instantiate eagerly in types]
  co <- fillInferResultNoInst ki cell
  pure (ty `mkCastTy` co)

---------------------------

tcHsContext :: Maybe (LHsContext GhcRn) -> TcM [PredType]
tcHsContext Nothing    = return []
tcHsContext (Just cxt) = tc_hs_context typeLevelMode cxt

tcLHsPredType :: LHsType GhcRn -> TcM PredType
tcLHsPredType pred = tc_lhs_pred typeLevelMode pred

tc_hs_context :: TcTyMode -> LHsContext GhcRn -> TcM [PredType]
tc_hs_context mode ctxt = mapM (tc_lhs_pred mode) (hsc_ctxt $ unLoc ctxt)

tc_lhs_pred :: TcTyMode -> LHsType GhcRn -> TcM PredType
tc_lhs_pred mode pred@(L loc _)
  = setSrcSpanA loc $
    do { pred_ty <- tc_check_lhs_type mode pred constraintKind
       ; unless (isEmptyNameEnv (mode_indices mode)) $
         do { pred_ty <- liftZonkM $ zonkTcType pred_ty
            ; unless (supported_predicate pred_ty) $
                failWithTc $ TcRnRetainedQuantificationError
                             (RetainedConstraintType pred_ty) }
       ; pure pred_ty }
  where
    -- Check native components hidden by constraint-tuple synonyms too.
    supported_predicate :: PredType -> Bool
    supported_predicate ty
      | not (hasNativeDependentExpr (Core.Type ty)) = True
      | Just (cls, args) <- getClassPredTys_maybe ty
      = not (isCTupleClass cls) || all supported_predicate args
      | otherwise = False

{- Note [Retained source constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check each predicate in the registered runtime-index environment. Permit ordinary
class and equality predicates. The retained equation checker introduces their
Given dictionaries below the runtime binders. Application checking keeps native
Wanted evidence at its call site. See Note [Retained application evidence] in
GHC.Tc.Gen.App. The solver uses its ordinary instance and equality rules.

Keep native quantified and irreducible predicates gated at their source span.
Check constraint-tuple components after synonym expansion as well. A tuple class
must not hide an unsupported native predicate in one of its components.
Those unsupported predicates need separate solver scope and termination checks.
Predicates without native syntax keep the ordinary checking rules. This rule does
not evaluate an index or infer equality between different runtime Ids. See
docs/dependent-haskell/REFERENCES.md#source-retained-constraints.
-}

---------------------------
tcTyVar :: TcTyMode -> Name -> TcM (TcType, TcKind)
-- See Note [Type checking recursive type and class declarations]
-- in GHC.Tc.TyCl
-- This does not instantiate. See Note [Do not always instantiate eagerly in types]
tcTyVar mode name
  | Just index <- lookupNameEnv (mode_indices mode) name
  = if isNativeOpaqueValueId index
    then pure (TermVarTy index, varType index)
    else failWithTc $ TcRnRetainedQuantificationError RetainedCallIndex
tcTyVar _ name         -- Could be a tyvar, a tycon, or a datacon
  = do { traceTc "lk1" (ppr name)
       ; pack <- isExistsPackName name
       ; when pack $ failWithTc (TcRnExistsPack ExistsPackInvalidContext)
       ; thing <- tcLookup name
       ; case thing of
           ATyVar _ tv -> return (mkTyVarTy tv, tyVarKind tv)
           ATcTypeLet _ ty kind -> return (ty, kind)

           -- An ordinary module can import a synonym for a checked computation.
           -- See Note [Source table dependency closure] in GHC.Tc.Module.
           AGlobal (ATyCon tc)
             | Just rhs <- synTyConRhs_maybe tc
             -> do { sourceTableTypeUses rhs
                   ; return (mkTyConTy tc, tyConKind tc) }
             | isJust (sourceTableTermOcc (nameOccName (tyConName tc)))
             -> do { sourceTableTypeUses (mkTyConTy tc)
                   ; return (mkTyConTy tc, tyConKind tc) }

           -- See Note [Recursion through the kinds]
           (tcTyThingTyCon_maybe -> Just tc) -- TyCon or TcTyCon
             -> return (mkTyConTy tc, tyConKind tc)

           AGlobal (AConLike (RealDataCon dc))
             -> do { unless (null (dataConRepArgBinders dc) || dataConHasPromotedRetained dc) $
                       failWithTc $ TcRnRetainedQuantificationError RetainedPackagePromotion
                   ; when (isDataFamInstTyCon (dataConTyCon dc)) $
                       do { enabled <- xoptM LangExt.ExperimentalDependentTypes
                          ; unless (enabled && canPromoteDataFamilyCon dc) $
                              promotionErr name FamDataConPE }
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

-- | Admit finished lifted constructors with no stored context or existentials.
-- See Note [Promoting a simple data family instance constructor].
canPromoteDataFamilyCon :: DataCon -> Bool
canPromoteDataFamilyCon dc =
  null exs && null theta
  && null (dataConStupidTheta dc)
  && null (dataConRepArgBinders dc)
  && not (isNewTyCon (dataConTyCon dc))
  && isLiftedTypeKind (typeKind (dataConOrigResTy dc))
  && all (isLiftedTypeKind . typeKind . scaledThing) fields
  where
    (_, exs, _, theta, fields, _) = dataConFullSig dc

{- Note [Promoting a simple data family instance constructor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A data family constructor has a runtime worker that returns the
representation type. Its wrapper uses the representational instance
axiom to return the public family application.

The promoted constructor already uses dataConOrigResTy as its result
kind. For a data family constructor this is the public family
application. An equality refinement in EqSpec already appears in this
result kind, as it does for an ordinary promoted GADT constructor.
See Note [Promoted data constructors] in GHC.Core.TyCon.
Use the existing promoted DataCon and its Name. Its kind contains
neither the representation TyCon nor the representation coercion.

Discussion #456 distinguishes this case from constructors whose
promotion needs to bind evidence. Keep that case and constructors of
unchecked instances out of scope. See Note [AFamDataCon: not
promoting data family constructors] in GHC.Tc.Utils.Env.
-}

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
addTypeCtxt ty thing = addErrCtxt (TypeCtxt ty) thing



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
tcAnonWildCardOcc is_extra mode@(TcTyMode { mode_holes = Just (hole_lvl, hole_mode) })
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
       ; checkExpectedKind mode ty (mkTyVarTy wc_tv) wc_kind exp_kind }
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
kcDeclHeader InitialKindInfer = kcInferDeclHeader False

-- | Infer the header of a generated sum payload family.
-- See Note [Provisional kinds of sum payloads] in GHC.Tc.TyCl.
kcInferSumPayloadHeader :: Name -> LHsQTyVars GhcRn -> TcM ContextKind -> TcM MonoTcTyCon
kcInferSumPayloadHeader name = kcInferDeclHeader True name ClosedTypeFamilyFlavour

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
  = addErrCtxt (TyConDeclCtxt name flav) $
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

             -- The user-written kind, before eta-expansion
             user_kind :: TcKind
             user_kind = mkTyConKindWithMatchability
                           (tyConFlavourMatchability flav) all_tcbs res_kind

       -- Eta expand if necessary; we are building a PolyTyCon
       ; (eta_tcbs, res_kind) <- maybeEtaExpandAlgTyCon flav skol_info all_tcbs res_kind

       ; let all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ binderVars tc_bndrs)
             final_tcbs = all_tcbs `chkAppend` eta_tcbs
             tycon = mkTcTyCon name user_kind final_tcbs (length eta_tcbs) res_kind all_tv_prs
                               True -- Make a PolyTcTyCon, fully generalised
                               flav

       ; reportUnsolvedEqualities skol_info (binderVars final_tcbs)
                                  tclvl wanted

         -- If the ordering from
         -- Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
         -- doesn't work, we catch it here, before an error cascade
       ; checkTyConTelescope tycon

       ; traceTc "kcCheckDeclHeader_cusk" $
         vcat [ text "name" <+> ppr name
              , text "user_kind" <+> ppr user_kind
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
  :: Bool               -- ^ Generated sum payload family
  -> Name               -- ^ of the thing being checked
  -> TyConFlavour TyCon -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn
  -> TcM ContextKind    -- ^ The result kind
  -> TcM MonoTcTyCon    -- ^ A suitably-kinded non-generalized TcTyCon
kcInferDeclHeader is_sum name flav
              (HsQTvs { hsq_ext = kv_ns
                      , hsq_explicit = hs_bndrs }) kc_res_ki
  -- No standalone kind signature and no CUSK.
  -- See Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
  = addErrCtxt (TyConDeclCtxt name flav) $
    do { rejectInvisibleBinders name hs_bndrs
       ; (scoped_kvs, (tc_bndrs, res_kind))
           -- Why bindImplicitTKBndrs_Q_Tv which uses newTyVarTyVar?
           -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl
           <- bindImplicitTKBndrs_Q_Tv kv_ns              $
              bindExplicitTKBndrsX (smVanilla { sm_clone = False, sm_parent = True
                                              , sm_tvtv = SMDTyVarTv, sm_kind = ctxt_kind
                                              , sm_family_metadata = tcFlavourIsOpen flav }) hs_bndrs $
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

             tc_binders
               | is_sum
               = map (mkRequiredTyConBinder (tyCoVarsOfTypes (map tyVarKind tc_tvs))) tc_tvs
               | otherwise
               = mkAnonTyConBinders tc_tvs
               -- The sum payload path keeps dependencies already present in
               -- the checked header. See Note [Provisional kinds of sum
               -- payloads] in GHC.Tc.TyCl.
               --
               -- Ordinary declarations must use mkAnonTyConBinders.
               -- See Note [No polymorphic recursion in type decls] in GHC.Tc.TyCl
               --
               -- Also, note that tc_binders has the tyvars from only the
               -- user-written type variable binders.
               -- See S1 Note [TcTyCon, MonoTcTyCon, and PolyTcTyCon] in GHC.Tc.TyCl

             all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
               -- NB: bindExplicitTKBndrs_Q_Tv does not clone;
               --     ditto Implicit
               -- See Note [Cloning for type variable binders]

             kind = mkTyConKindWithMatchability
                      (tyConFlavourMatchability flav) tc_binders res_kind
             tycon = mkTcTyCon name kind tc_binders 0 res_kind all_tv_prs
                               False -- Make a MonoTcTyCon
                               flav

       ; traceTc "kcInferDeclHeader: not-cusk" $
         vcat [ text "name:" <+> ppr name
              , text "kind:" <+> ppr kind
              , text "kv_ns:" <+>  ppr kv_ns
              , text "hs_bndrs:" <+> ppr hs_bndrs
              , text "scoped_kvs:" <+> ppr scoped_kvs
              , text "tc_tvs:" <+> ppr tc_tvs
              , text "tc_binders:" <+> ppr tc_binders
              , text "res_kind:" <+> ppr res_kind
              ]
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
  = addErrCtxt (TyConDeclCtxt name flav) $
    do { skol_info <- mkSkolemInfo (TyConSkol flav name)
       ; let avoid_occs = map nameOccName (hsLTyVarNames hs_tv_bndrs)
       ; (sig_ma_tcbs :: [(Type, TcTyConBinder)], sig_res_kind :: Kind)
             <- splitTyConKind skol_info emptyInScopeSet
                               avoid_occs sig_kind
       ; let sig_tcbs = map snd sig_ma_tcbs

       ; traceTc "kcCheckDeclHeader_sig {" $
           vcat [ text "sig_kind:" <+> ppr sig_kind
                , text "sig_tcbs:" <+> ppr sig_tcbs
                , text "sig_res_kind:" <+> ppr sig_res_kind
                , text "implict_nms:" <+> ppr implicit_nms
                , text "hs_tv_bndrs:" <+> ppr hs_tv_bndrs ]

       ; (tclvl, wanted, (implicit_tvs, (skol_tcbs, skol_scoped_tvs, (extra_tcbs, tycon_res_kind))))
           <- pushLevelAndSolveEqualitiesX "kcCheckDeclHeader_sig" $  -- #16687
              bindImplicitTKBndrs_Q_Tv implicit_nms                $  -- Q means don't clone
              matchUpSigWithDecl name sig_ma_tcbs sig_res_kind hs_tv_bndrs $ \ excess_ma_tcbs sig_res_kind ->
              do { -- Kind-check the result kind annotation, if present:
                   --    data T a b :: res_ki where ...
                   --               ^^^^^^^^^
                   -- We do it here because at this point the environment has been
                   -- extended with both 'implicit_tcv_prs' and 'explicit_tv_prs'.
                   --
                   -- Also see Note [Arity of type families and type synonyms]
                 ; res_kind :: ContextKind <- kc_res_ki

                 ; let (excess_mas, excess_sig_tcbs) = unzip excess_ma_tcbs
                       sig_res_kind' = mkPiTys
                         (zip excess_mas (tyConBindersPiTyBinders excess_sig_tcbs)) sig_res_kind

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
              swizzled_res_kind      = substTy subst tycon_res_kind
              all_tv_prs             = mkTyVarNamePairs (binderVars swizzled_tcbs)

        ; traceTc "kcCheckDeclHeader swizzle" $ vcat
          [ text "sig_kind ="          <+> debugPprType sig_kind
          , text "sig_tcbs ="          <+> ppr sig_tcbs
          , text "implicit_prs ="      <+> ppr implicit_prs
          , text "hs_tv_bndrs ="       <+> ppr hs_tv_bndrs
          , text "all_tcbs ="          <+> pprTyVars (binderVars all_tcbs)
          , text "swizzled_tcbs ="     <+> pprTyVars (binderVars swizzled_tcbs)
          , text "tycon_res_kind ="    <+> ppr tycon_res_kind
          , text "subst ="             <+> ppr subst
          , text "swizzled_res_kind =" <+> ppr swizzled_res_kind ]

        -- Build the final, generalized PolyTcTyCon
        -- NB: all_tcbs must bind the tyvars in the range of all_tv_prs
        --     because the tv_prs is used when (say) typechecking the RHS of
        --     a type synonym.
        ; let
            tc = mkTcTyCon name sig_kind swizzled_tcbs (length extra_tcbs) swizzled_res_kind all_tv_prs
                 True -- Make a PolyTcTyCon, fully generalised
                 flav

        ; traceTc "kcCheckDeclHeader_sig }" $ vcat
          [ text "tyConName =" <+> ppr (tyConName tc)
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

             (_, _, sig_kind') = splitInvisPiTysN n_to_inst sig_kind

       ; discardResult $ unifyKind Nothing sig_kind' actual_res_ki }

matchUpSigWithDecl
  :: Name                        -- Name of the type constructor for error messages
  -> [(Type, TcTyConBinder)]     -- Metadata and skolem binders from the separate kind signature
  -> TcKind                      -- The tail end of the kind signature
  -> [LHsTyVarBndr (HsBndrVis GhcRn) GhcRn]     -- User-written binders in decl
  -> ([(Type, TcTyConBinder)] -> TcKind -> TcM a)  -- All user-written binders are in scope
                                           --   Argument is excess TyConBinders and tail kind
  -> TcM ( [TcTyConBinder]       -- Skolemised binders, with TcTyVars
         , [TcTyVar]             -- Skolem tyvars brought into lexical scope by this matching-up
         , a )
-- See Note [Matching a kind signature with a declaration]
-- Invariant: Length of returned TyConBinders + length of excess TyConBinders
--            = length of incoming TyConBinders
matchUpSigWithDecl name sig_ma_tcbs sig_res_kind hs_bndrs thing_inside
  = go emptySubst sig_ma_tcbs hs_bndrs
  where
    go subst ma_tcbs []
      = do { let (mas, tcbs) = unzip ma_tcbs
                 (subst', tcbs') = substTyConBindersX subst tcbs
           ; res <- thing_inside (zip (map (substTy subst') mas) tcbs')
                                  (substTy subst' sig_res_kind)
           ; return ([], [], res) }

    go _ [] hs_bndrs
      = failWithTc (TcRnTooManyBinders sig_res_kind hs_bndrs)

    go subst ((_ma, tcb) : ma_tcbs') hs_bndrs@(hs_bndr : hs_bndrs')
      | zippableTyConBinder (binderFlag tcb) (hsTyVarBndrFlag (unLoc hs_bndr))
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
                                  go subst' ma_tcbs' hs_bndrs'
           ; return (Bndr tv' vis : tcbs', tv':tvs, res) }
             -- We do a tcExtendTyVarEnv [tv'], so we return tv' in
             -- the list of lexically-scoped skolem type variables

      | skippableTyConBinder (binderFlag tcb)
      = -- Invisible TyConBinder, so do not consume one of the hs_bndrs
        do { let (subst', tcb') = substTyConBinderX subst tcb
           ; traceTc "musd2" (ppr tcb $$ ppr hs_bndr $$ ppr tcb')
           ; (tcbs', tvs, res) <- go subst' ma_tcbs' hs_bndrs
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

-- | Match a declaration binder to a signature binder, as in proposal 425.
zippableTyConBinder :: TyConBndrVis -> HsBndrVis GhcRn -> Bool
zippableTyConBinder vis (HsBndrInvisible _) = isInvisSpecTcbVis vis
zippableTyConBinder vis (HsBndrRequired _)  = isVisibleTcbVis vis

-- | Skip an invisible signature binder while a declaration binder remains.
skippableTyConBinder :: TyConBndrVis -> Bool
skippableTyConBinder vis = not (isVisibleTcbVis vis)

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
              -> TcM ([(Matchability, TcTyVarBinder)], a)
-- A HsForAllTelescope comes only from a HsForAllTy,
-- an explicit, user-written forall type
tcTKTelescope mode tele thing_inside =
 do { explicit_ma <- tcForAllMatchability (hsf_matchability tele)
    ; case tele of
      HsForAllVis { hsf_vis_bndrs = bndrs }
        -> do { skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn (unLoc <$> bndrs)))
              ; let skol_mode = smVanilla { sm_clone = False, sm_holes = mode_holes mode
                                          , sm_indices = mode_indices mode
                                          , sm_tvtv = SMDSkolemTv skol_info }
              ; (req_tv_bndrs, thing) <- tcExplicitTKBndrsX skol_mode bndrs thing_inside
                -- req_tv_bndrs :: [VarBndr TyVar ()],
                -- but we want [VarBndr TyVar ForAllTyFlag]
              ; ma_bndrs <- forAllMatchabilities mode explicit_ma (tyVarReqToBinders req_tv_bndrs)
              ; return (ma_bndrs, thing) }

      HsForAllInvis { hsf_invis_bndrs = bndrs }
        -> do { skol_info <- mkSkolemInfo (ForAllSkol (HsTyVarBndrsRn (unLoc <$> bndrs)))
              ; let skol_mode = smVanilla { sm_clone = False, sm_holes = mode_holes mode
                                          , sm_indices = mode_indices mode
                                          , sm_tvtv = SMDSkolemTv skol_info }
              ; (inv_tv_bndrs, thing) <- tcExplicitTKBndrsX skol_mode bndrs thing_inside
                -- inv_tv_bndrs :: [VarBndr TyVar Specificity],
                -- but we want [VarBndr TyVar ForAllTyFlag]
              ; ma_bndrs <- forAllMatchabilities mode explicit_ma (tyVarSpecToBinders inv_tv_bndrs)
              ; return (ma_bndrs, thing) }
    }

-- | Check runtime invisible forall groups without flattening their scopes.
tcRuntimeInvisForAllTelescopes :: SkolemInfo -> [HsForAllTelescope GhcRn]
                              -> TcM a -> TcM ([InvisTVBinder], a)
tcRuntimeInvisForAllTelescopes _ [] inside = ([],) <$> inside
tcRuntimeInvisForAllTelescopes skol_info (tele@HsForAllInvis{hsf_invis_bndrs = bndrs} : rest) inside =
  do { tcCheckRuntimeForAllMatchability (hsf_matchability tele)
     ; (tvs, (rest_tvs, a)) <- tcExplicitTKBndrs skol_info bndrs $
         tcRuntimeInvisForAllTelescopes skol_info rest inside
     ; return (tvs ++ rest_tvs, a) }
tcRuntimeInvisForAllTelescopes _ (HsForAllVis{} : _) _ =
  pprPanic "tcRuntimeInvisForAllTelescopes: visible group" empty

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
      HsOuterExplicit{hso_matchability = ma, hso_bndrs = exp_bndrs} ->
        do { tcCheckRuntimeForAllMatchability ma
           ; (exp_tvs', thing) <- bindExplicitTKBndrsX skol_mode exp_bndrs thing_inside
           ; return ( HsOuterExplicit { hso_xexplicit = exp_tvs'
                                      , hso_matchability = ma
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
      HsOuterExplicit{hso_matchability = ma, hso_bndrs = exp_bndrs} ->
        do { (exp_tvs', thing) <- tcExplicitTKBndrsX skol_mode exp_bndrs thing_inside
           ; return ( HsOuterExplicit { hso_xexplicit = exp_tvs'
                                      , hso_matchability = ma
                                      , hso_bndrs     = exp_bndrs }
                    , thing) }

---------------
tcGadtConTyVarBndrs :: SkolemInfo
                    -> HsOuterSigTyVarBndrs GhcRn
                    -> [LHsGadtTelescope GhcRn]
                    -> TcM a -> TcM ([TcTyVarBinder], TcThetaType, a)
tcGadtConTyVarBndrs skol_info outer inner thing_inside
  = do { tcCheckRuntimeForAllMatchability (hsOuterMatchability outer)
       ; (outer_bndrs, (inner_tvbs, theta, a)) <-
            tcOuterTKBndrs skol_info outer $
            tcConDeclTelescope skol_info inner thing_inside
       ; outer_bndrs <- scopedSortOuter outer_bndrs
       ; let outer_tvbs = tyVarSpecToBinders (outerTyVarBndrs outer_bndrs)
       ; return (outer_tvbs ++ inner_tvbs, theta, a) }

-- | Check constructor contexts before later groups enter scope.
-- See Note [Constructor telescope contexts] in GHC.Hs.Decls.
tcConDeclTelescope :: SkolemInfo -> [LHsGadtTelescope GhcRn]
                   -> TcM a -> TcM ([TcTyVarBinder], TcThetaType, a)
tcConDeclTelescope _ [] inside = do
  a <- inside
  return ([], [], a)
tcConDeclTelescope skol_info (L _ HsGadtPar{} : rest) inside =
  tcConDeclTelescope skol_info rest inside
tcConDeclTelescope skol_info (L _ (HsGadtContext ctxt) : rest) inside =
  do { theta <- tcHsContext (Just ctxt)
       -- Do not install constructor-local theta as Givens. See the owning Note.
     ; (tvbs, rest_theta, a) <- tcConDeclTelescope skol_info rest inside
     ; return (tvbs, theta ++ rest_theta, a) }
tcConDeclTelescope skol_info (L _ (HsGadtForAll _ tele) : rest) inside =
  do { tcCheckRuntimeForAllMatchability (hsf_matchability tele)
     ; (tvbs, (rest_tvbs, theta, a)) <- case tele of
         HsForAllInvis{hsf_invis_bndrs = bndrs} -> do
           (tvs, a) <- tcExplicitTKBndrs skol_info bndrs (tcConDeclTelescope skol_info rest inside)
           return (tyVarSpecToBinders tvs, a)
         HsForAllVis{hsf_vis_bndrs = bndrs} -> do
           (tvs, a) <- tcExplicitTKBndrs skol_info bndrs (tcConDeclTelescope skol_info rest inside)
           return (tyVarReqToBinders tvs, a)
     ; return (tvbs ++ rest_tvbs, theta, a) }

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
                                   , sm_indices = indices
                                   , sm_holes = hole_info, sm_family_metadata = family_metadata
                                   , sm_sig_kind_arrows = sig_kind_arrows })
                     hs_tvs thing_inside
  = do { traceTc "bindExplicitTKBndrs" (ppr hs_tvs)
       ; go hs_tvs }
  where
    tc_ki_mode = TcTyMode { mode_tyki = KindLevel, mode_holes = hole_info
                         , mode_declaration = False
                         , mode_callback_arrow = OrdinaryArrowChecking
                         , mode_source_givens = []
                         , mode_indices = indices
                         , mode_dictionaries = []
                         , mode_pending_calls = Nothing
                         , mode_synonym_metadata = if family_metadata then familyKindMetadata else Nothing
                         , mode_complete_type_arg = Nothing }
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

    -- Let a complete signature constrain omitted arrows in a binder kind.
    -- Check its shape first. Give each written bare arrow a separate variable
    -- in the signature's defaulting pass. scopedSortOuter checks the dependency
    -- order of the resulting binder kinds.
    tc_hs_bndr_kind :: Name -> HsBndrKind GhcRn -> TcM Kind
    tc_hs_bndr_kind _    (HsBndrNoKind _)    = newExpectedKind ctxt_kind
    tc_hs_bndr_kind name (HsBndrKind _ kind)
      = do { checked <- tc_lhs_kind_sig tc_ki_mode (TyVarBndrKindCtxt name) kind
           ; case checked of
               FunTy{} | sig_kind_arrows ->
                 do { owner <- tcl_kind_arrow_defaults <$> getLclEnv
                    ; case owner of
                        Just (defaults, _) -> defer_kind_arrows defaults kind checked
                        Nothing -> pure checked }
               _ -> pure checked }

    defer_kind_arrows defaults hs_kind checked
      | L _ (HsFunTy _ arrow domain result) <- ignoreParens hs_kind
      , FunTy flag ma mult domain_ty result_ty <- checked
      = do { ma' <- case arrow of
                       HsModifiedFunArr _ [] HsStandardArr{} ->
                         do { v <- newFlexiTyVar matchabilityTy
                            ; updTcRef defaults ((v, KindLevel) :)
                            ; pure (mkTyVarTy v) }
                       _ -> pure ma
           ; domain_ty' <- defer_kind_arrows defaults domain domain_ty
           ; result_ty' <- defer_kind_arrows defaults result result_ty
           ; pure (FunTy flag ma' mult domain_ty' result_ty') }
      | otherwise = pure checked

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

       , sm_family_metadata :: Bool
       , sm_sig_kind_arrows :: Bool -- Defer bare binder arrows until the signature is checked.
       , sm_indices :: NameEnv Id  -- Runtime indices in binder kinds
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
               , sm_family_metadata = False
               , sm_sig_kind_arrows = False
               , sm_indices = emptyNameEnv
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

bindTyClTyVars :: Name -> (TcKind -> [TcTyConBinder] -> Int -> TcKind -> TcM a) -> TcM a
-- ^ Bring into scope the binders of a PolyTcTyCon
-- Used for the type variables of a type or class decl
-- in the "kind checking" and "type checking" pass,
-- but not in the initial-kind run.
bindTyClTyVars tycon_name thing_inside
  = do { tycon <- tcLookupTcTyCon tycon_name     -- The tycon is a PolyTcTyCon
       ; let kind       = tyConKind tycon
             res_kind   = tyConResKind tycon
             binders    = tyConBinders tycon
             nb_eta     = tyConEtaBinders tycon
       ; traceTc "bindTyClTyVars" (ppr tycon_name $$ ppr binders)
       ; tcExtendTyVarEnv (binderVars binders) $
         thing_inside kind binders nb_eta res_kind }

bindTyClTyVarsAndZonk :: Name -> (Kind -> [TyConBinder] -> Int -> Kind -> TcM a) -> TcM a
-- Like bindTyClTyVars, but in addition
-- zonk the skolem TcTyVars of a PolyTcTyCon to TyVars
-- We always do this same zonking after a call to bindTyClTyVars, but
-- here we do it right away because there are no more unifications to come
bindTyClTyVarsAndZonk tycon_name thing_inside
  = bindTyClTyVars tycon_name $ \ tc_kind tc_bndrs nb_eta tc_res_kind ->
    do { (kind, bndrs, res_kind) <- initZonkEnv NoFlexi $ do
          kind <- zonkTcTypeToTypeX tc_kind
          runZonkBndrT (zonkTyVarBindersX tc_bndrs) $ \ bndrs ->
            do { res_kind <- zonkTcTypeToTypeX tc_res_kind
               ; return (kind, bndrs, res_kind) }
       ; thing_inside kind bndrs nb_eta res_kind }


{- *********************************************************************
*                                                                      *
             Kind generalisation
*                                                                      *
********************************************************************* -}

zonkAndScopedSort :: [TcTyVar] -> TcM [TcTyVar]
zonkAndScopedSort spec_tkvs
  = do { spec_tkvs <- liftZonkM $ zonkTcTyVarsToTcTyVars spec_tkvs
         -- Zonk the kinds, so that we can do the dependency analysis

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
maybeEtaExpandAlgTyCon :: TyConFlavour tc  -> SkolemInfo
                  -> [TcTyConBinder] -> Kind
                  -> TcM ([TcTyConBinder], Kind)
maybeEtaExpandAlgTyCon flav skol_info tcbs res_kind
  | needsEtaExpansion flav
  = etaExpandAlgTyCon skol_info tcbs res_kind
  | otherwise
  = return ([], res_kind)

etaExpandAlgTyCon :: SkolemInfo
                  -> [TcTyConBinder] -> Kind
                  -> TcM ([TcTyConBinder], Kind)
etaExpandAlgTyCon skol_info tcbs declared_kind
  = do { (ma_tcbs, res_kind) <- splitTyConKind skol_info in_scope avoid_occs declared_kind
       ; solveEqualities "etaExpandAlgTyCon matchability" $
           forM_ (zip [0..] ma_tcbs) $ \ (i, (ma, _)) ->
             checkDeclArrowMatchability declared_kind i ma matchableDataConTy
       ; return (map snd ma_tcbs, res_kind) }
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
               -> TcM ([(Type, TcTyConBinder)], TcKind)
-- ^ GADT decls can have a (perhaps partial) kind signature
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

                    Just (ma, Anon arg af, kind')
                      -> assert (af == FTF_T_T) $
                         go occs' uniqs' subst' ((substTy subst ma, tcb) : acc) kind'
                      where
                        tcb    = Bndr tv AnonTCB
                        arg'   = substTy subst (scaledThing arg)
                        name   = mkInternalName uniq occ loc
                        tv     = mkTcTyVar name arg' details
                        subst' = extendSubstInScope subst tv
                        (uniq,uniqs') = case uniqs of
                            uniq:uniqs' -> (uniq,uniqs')
                            _           -> panic "impossible"
                        Inf occ occs' = occs

                    Just (ma, Named (Bndr tv vis), kind')
                      -> go occs uniqs subst' ((substTy subst ma, tcb) : acc) kind'
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
    (_, _, res_kind) = tcSplitPiTys kind
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
      | Just (_, tcb, inner_kind) <- splitPiTy_maybe fun_kind
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

-- | Whether the caller checks pending computations in its inference group.
data PendingSigCalls = RetrySigCalls | RejectSigCalls

tcHsPartialSigType
  :: PendingSigCalls
  -> UserTypeCtxt
  -> LHsSigWcType GhcRn       -- The type signature
  -> TcM ( [(Name, TcTyVar)]  -- Wildcards
         , Maybe TcType       -- Extra-constraints wildcard
         , [(Name,(Matchability, InvisTVBinder))] -- Original tyvar names, in correspondence with
                              --   the implicitly and explicitly bound type variables
         , TcThetaType        -- Theta part
         , TcType             -- Tau part
         , [TcPendingTypeCall] )
-- See Note [Checking partial type signatures]
tcHsPartialSigType call_owner ctxt sig_ty
  | HsWC { hswc_ext  = sig_wcs, hswc_body = sig_ty } <- sig_ty
  , L _ (HsSig{sig_bndrs = hs_outer_bndrs, sig_body = body_ty}) <- sig_ty
  , (hs_ctxt, hs_tau) <- splitLHsQualTy body_ty
  = addSigCtxt ctxt (UserLHsSigType sig_ty) $
    do { mode <- mkHoleMode TypeLevel HM_Sig
       ; experimental <- xoptM LangExt.ExperimentalDependentTypes
       ; pending <- if experimental && not (null sig_wcs)
                    then Just <$> newTcRef []
                    else pure Nothing
       ; (outer_ma, (outer_bndrs, (wcs, wcx, theta, tau)))
            <- solveEqualities "tcHsPartialSigType" $
               -- See Note [Failure in local type signatures]
               do { outer_ma <- tcForAllMatchability (hsOuterMatchability hs_outer_bndrs)
                  ; stuff <- bindNamedWildCardBinders sig_wcs $ \ wcs ->
                             bindOuterSigTKBndrs_Tv_M mode hs_outer_bndrs $
               do {   -- Instantiate the type-class context; but if there
                      -- is an extra-constraints wildcard, just discard it here
                    (theta, wcx) <- tcPartialContext mode hs_ctxt

                  ; ek <- newOpenTypeKind
                  ; let tau_mode
                          | null theta, isNothing wcx, Just calls <- pending
                          = mode { mode_pending_calls = Just (mkNameEnv wcs, call_owner, calls) }
                          | otherwise = mode
                  ; tau <- -- Don't do (addTypeCtxt hs_tau) here else we get
                           --   In the type <blah>
                           --   In the type signature: foo :: <blah>
                           tc_check_lhs_type tau_mode hs_tau ek

                  ; return (wcs, wcx, theta, tau) }
                  ; return (outer_ma, stuff) }

       ; traceTc "tcHsPartialSigType 2" empty
       ; outer_bndrs <- scopedSortOuter outer_bndrs
       ; let outer_tv_bndrs = map (fromMaybe unmatchableDataConTy outer_ma,) (outerTyVarBndrs outer_bndrs)
       ; traceTc "tcHsPartialSigType 3" empty

         -- No kind-generalization here:
       ; let written_ty = mkInvisForAllTys outer_tv_bndrs (tcMkPhiTy theta tau)
       ; kindGeneralizeNone written_ty
       ; checkConstrainedSignature ctxt written_ty
         -- See Note [Constrained associated family formation] in GHC.Tc.Validity.

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
             tv_prs :: [(Name,(Matchability, InvisTVBinder))]
             tv_prs = outer_bndr_names `zip` outer_tv_bndrs

       -- Zonk, so that any nested foralls can "see" their occurrences
       -- See Note [Checking partial type signatures], and in particular
       -- Note [Levels for wildcards]
       ; (tv_prs, theta, tau) <- liftZonkM $
         do { tv_prs <- mapSndM (\ (ma,bndr) -> (,) <$> zonkTcType ma <*> zonkInvisTVBinder bndr)
                               (tv_prs ++ tv_prs')
            ; theta  <- mapM    zonkTcType        (theta ++ theta')
            ; tau    <- zonkTcType                tau
            ; return (tv_prs, theta, tau) }

      -- NB: checkValidType on the final inferred type will be
      --     done later by checkInferredPolyId.  We can't do it
      --     here because we don't have a complete type to check

       ; traceTc "tcHsPartialSigType" (ppr tv_prs)
       ; calls <- maybe (pure []) (fmap reverse . readTcRef) pending
       ; return (wcs, wcx, tv_prs, theta, tau, calls) }

tcPartialContext :: TcTyMode -> Maybe (LHsContext GhcRn) -> TcM (TcThetaType, Maybe TcType)
tcPartialContext _ Nothing = return ([], Nothing)
tcPartialContext mode (Just (L _ (HsContext _ hs_theta)))
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
tuples up to 62 (see Note [How tuples work] in GHC.Builtin.WiredIn.Types).  So how
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
  = tc_type_in_pat ctxt Nothing hole_mode hs_ty sig_wcs sig_ns ctxt_kind

tcRuleBndrSig :: Name
              -> SkolemInfo
              -> HsPatSigType GhcRn          -- The type signature
              -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                     , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                             -- the scoped type variables
                     , TcType)       -- The type
-- Used for type-checking type signatures in
--     RULE forall bndrs  e.g. forall (x::Int). f x = x
-- See Note [Pattern signature binders and scoping] in GHC.Hs.Type
--
-- This may emit constraints
-- See Note [Recipe for checking a signature]
tcRuleBndrSig name skol_info
    (HsPS { hsps_ext  = HsPSRn { hsps_nwcs = sig_wcs, hsps_imp_tvs = sig_ns }
          , hsps_body = hs_ty })
  = tc_type_in_pat (RuleBndrTypeCtxt name) (Just skol_info)
                   HM_Sig hs_ty sig_wcs sig_ns OpenKind

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
    tc_unif_in_pat = tc_type_in_pat TypeAppCtxt Nothing HM_TyAppPat

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
               -> Maybe SkolemInfo    -- Just sk for RULE and SPECIALISE pragmas only
               -> HoleMode -- HM_Sig when in a SigPat, HM_TyAppPat when in a ConPat checking type applications.
               -> LHsType GhcRn          -- The type in pattern
               -> [Name]                 -- All named wildcards in type
               -> [Name]                 -- All binders in type
               -> ContextKind                -- What kind is expected
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                              -- the scoped type variables
                      , TcType)       -- The type
tc_type_in_pat ctxt mb_skol hole_mode hs_ty wcs ns ctxt_kind
  = addSigCtxt ctxt (UserLHsType hs_ty) $
    do { tkvs <- mapM new_implicit_tv ns
       ; let tkv_prs = ns `zip` tkvs
       ; mode <- mkHoleMode TypeLevel hole_mode
       ; (wcs, ty)
            <- addTypeCtxt hs_ty                $
               solveEqualities "tc_type_in_pat" $
                 -- See Note [Failure in local type signatures]
                 -- and c.f #16033
               bindNamedWildCardBinders wcs $ \ wcs ->
               tcExtendNameTyVarEnv tkv_prs $
               do { ek <- newExpectedKind ctxt_kind
                  ; mode <- case ctxt of
                      RuleBndrTypeCtxt{} -> do
                        indices <- tcRetainedAmbientIndices RetainedLocalSignatureIndex hs_ty
                        return (mode { mode_indices = indices })
                      _ -> return mode
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
           ; case mb_skol of
                Just skol_info -> newSkolemTyVar skol_info name kind
                Nothing        -> newPatTyVar name kind }
                -- See Note [Typechecking pattern signature binders]
                -- NB: tv's Name may be fresh (in the case of newPatTyVar)

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
  go_bvar (HsTyVar _ _ tv)
    | isTyVarName (getName tv)
    = Just (HsBndrVar noExtField (fmap getName tv))
  go_bvar (HsWildCardTy h)
    = Just (HsBndrWildCard h)
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

unifyKinds :: TcTyMode -> [LHsType GhcRn] -> [(TcType, TcKind)] -> TcM ([TcType], TcKind)
unifyKinds mode rn_tys act_kinds
  = do { kind <- newMetaKindVar
       ; let check rn_ty (ty, act_kind)
               = checkExpectedKind mode (unLoc rn_ty) ty act_kind kind
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

-- | A type-family result can depend on metadata chosen by each application.
tcLHsFamilyResultKindSig :: UserTypeCtxt -> LHsKind GhcRn -> TcM Kind
tcLHsFamilyResultKindSig = tc_lhs_kind_sig
  (kindLevelMode { mode_synonym_metadata = familyKindMetadata })

-- | Give each checked kind application its own metadata variables.
familyKindMetadata :: Maybe (Name -> [Bool] -> TcM [Type], Maybe (TcM Matchability))
familyKindMetadata = Just (indices, Nothing)
  where
    indices _ callbacks = do
      vars <- mapM (const (newFlexiTyVar matchabilityTy)) callbacks
      owner <- tcl_kind_arrow_defaults <$> getLclEnv
      forM_ owner $ \(_, ref) -> updTcRef ref (vars ++)
      pure (mkTyVarTys vars)

tc_lhs_kind_sig :: TcTyMode -> UserTypeCtxt -> LHsKind GhcRn -> TcM Kind
tc_lhs_kind_sig mode ctxt hs_kind
-- See  Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
-- Result is zonked
  = do { kind <- addErrCtxt (KindCtxt hs_kind) $
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
  = do { traceTc "promotionError" (ppr name)
       ; failWithTc $ TcRnUnpromotableThing name err }

{-
************************************************************************
*                                                                      *
          Utils for constructing TyLit
*                                                                      *
************************************************************************
-}

tyLitFromOverloadedLit :: OverLitVal GhcRn -> HsLit GhcRn
tyLitFromOverloadedLit (HsIntegral   n) = HsNatural noExtField n
tyLitFromOverloadedLit (HsFractional f) = HsDouble  noExtField f
tyLitFromOverloadedLit (HsIsString   s) = HsString  (sl_src s) (sl_fs s)
