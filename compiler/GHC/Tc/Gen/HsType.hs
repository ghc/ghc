{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE CPP, TupleSections, MultiWayIf, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Typechecking user-specified @MonoTypes@
module GHC.Tc.Gen.HsType (
        -- Type signatures
        kcClassSigType, tcClassSigType,
        tcHsSigType, tcHsSigWcType,
        tcHsPartialSigType,
        tcStandaloneKindSig,
        funsSigCtxt, addSigCtxt, pprSigCtxt,

        tcHsClsInstType,
        tcHsDeriv, tcDerivStrategy,
        tcHsTypeApp,
        UserTypeCtxt(..),
        bindImplicitTKBndrs_Tv, bindImplicitTKBndrs_Skol,
            bindImplicitTKBndrs_Q_Tv, bindImplicitTKBndrs_Q_Skol,
        bindExplicitTKBndrs_Tv, bindExplicitTKBndrs_Skol,
            bindExplicitTKBndrs_Q_Tv, bindExplicitTKBndrs_Q_Skol,
        ContextKind(..),

                -- Type checking type and class decls
        bindTyClTyVars,
        etaExpandAlgTyCon, tcbVisibilities,

          -- tyvars
        zonkAndScopedSort,

        -- Kind-checking types
        -- No kind generalisation, no checkValidType
        InitialKindStrategy(..),
        SAKS_or_CUSK(..),
        kcDeclHeader,
        tcNamedWildCardBinders,
        tcHsLiftedType,   tcHsOpenType,
        tcHsLiftedTypeNC, tcHsOpenTypeNC,
        tcLHsType, tcLHsTypeUnsaturated, tcCheckLHsType,
        tcHsMbContext, tcHsContext, tcLHsPredType, tcInferApps,
        failIfEmitsConstraints,
        solveEqualities, -- useful re-export

        typeLevelMode, kindLevelMode,

        kindGeneralizeAll, kindGeneralizeSome, kindGeneralizeNone,

        -- Sort-checking kinds
        tcLHsKindSig, checkDataKindSig, DataSort(..),
        checkClassKindSig,

        -- Pattern type signatures
        tcHsPatSigType,

        -- Error messages
        funAppCtxt, addTyConFlavCtxt
   ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Hs
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Origin
import GHC.Core.Predicate
import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType
import GHC.Tc.Validity
import GHC.Tc.Utils.Unify
import GHC.IfaceToCore
import GHC.Tc.Solver
import GHC.Tc.Utils.Zonk
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Tc.Errors      ( reportAllUnsolved )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Instantiate ( tcInstInvisibleTyBinders, tcInstInvisibleTyBinder )
import GHC.Core.Type
import GHC.Builtin.Types.Prim
import GHC.Types.Name.Reader( lookupLocalRdrOcc )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Core.TyCon
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Types.Name
-- import GHC.Types.Name.Set
import GHC.Types.Var.Env
import GHC.Builtin.Types
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Settings.Constants ( mAX_CTUPLE_SIZE )
import GHC.Utils.Error( MsgDoc )
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Utils.Misc
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Builtin.Names hiding ( wildCardName )
import GHC.Driver.Session
import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.Maybe
import Data.List ( find )
import Control.Monad

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
-}

funsSigCtxt :: [Located Name] -> UserTypeCtxt
-- Returns FunSigCtxt, with no redundant-context-reporting,
-- form a list of located names
funsSigCtxt (L _ name1 : _) = FunSigCtxt name1 False
funsSigCtxt []              = panic "funSigCtxt"

addSigCtxt :: UserTypeCtxt -> LHsType GhcRn -> TcM a -> TcM a
addSigCtxt ctxt hs_ty thing_inside
  = setSrcSpan (getLoc hs_ty) $
    addErrCtxt (pprSigCtxt ctxt hs_ty) $
    thing_inside

pprSigCtxt :: UserTypeCtxt -> LHsType GhcRn -> SDoc
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

kcClassSigType :: SkolemInfo -> [Located Name] -> LHsSigType GhcRn -> TcM ()
-- This is a special form of tcClassSigType that is used during the
-- kind-checking phase to infer the kind of class variables. Cf. tc_hs_sig_type.
-- Importantly, this does *not* kind-generalize. Consider
--   class SC f where
--     meth :: forall a (x :: f a). Proxy x -> ()
-- When instantiating Proxy with kappa, we must unify kappa := f a. But we're
-- still working out the kind of f, and thus f a will have a coercion in it.
-- Coercions block unification (Note [Equalities with incompatible kinds] in
-- TcCanonical) and so we fail to unify. If we try to kind-generalize, we'll
-- end up promoting kappa to the top level (because kind-generalization is
-- normally done right before adding a binding to the context), and then we
-- can't set kappa := f a, because a is local.
kcClassSigType skol_info names (HsIB { hsib_ext  = sig_vars
                                     , hsib_body = hs_ty })
  = addSigCtxt (funsSigCtxt names) hs_ty $
    do { (tc_lvl, (wanted, (spec_tkvs, _)))
           <- pushTcLevelM                           $
              solveLocalEqualitiesX "kcClassSigType" $
              bindImplicitTKBndrs_Skol sig_vars      $
              tc_lhs_type typeLevelMode hs_ty liftedTypeKind

       ; emitResidualTvConstraint skol_info Nothing spec_tkvs
                                  tc_lvl wanted }

tcClassSigType :: SkolemInfo -> [Located Name] -> LHsSigType GhcRn -> TcM Type
-- Does not do validity checking
tcClassSigType skol_info names sig_ty
  = addSigCtxt (funsSigCtxt names) (hsSigType sig_ty) $
    snd <$> tc_hs_sig_type skol_info sig_ty (TheKind liftedTypeKind)
       -- Do not zonk-to-Type, nor perform a validity check
       -- We are in a knot with the class and associated types
       -- Zonking and validity checking is done by tcClassDecl
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

tcHsSigType :: UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
-- Does validity checking
-- See Note [Recipe for checking a signature]
tcHsSigType ctxt sig_ty
  = addSigCtxt ctxt (hsSigType sig_ty) $
    do { traceTc "tcHsSigType {" (ppr sig_ty)

          -- Generalise here: see Note [Kind generalisation]
       ; (insol, ty) <- tc_hs_sig_type skol_info sig_ty
                                       (expectedKindInCtxt ctxt)
       ; ty <- zonkTcType ty

       ; when insol failM
       -- See Note [Fail fast if there are insoluble kind equalities] in GHC.Tc.Solver

       ; checkValidType ctxt ty
       ; traceTc "end tcHsSigType }" (ppr ty)
       ; return ty }
  where
    skol_info = SigTypeSkol ctxt

-- Does validity checking and zonking.
tcStandaloneKindSig :: LStandaloneKindSig GhcRn -> TcM (Name, Kind)
tcStandaloneKindSig (L _ kisig) = case kisig of
  StandaloneKindSig _ (L _ name) ksig ->
    let ctxt = StandaloneKindSigCtxt name in
    addSigCtxt ctxt (hsSigType ksig) $
    do { kind <- tcTopLHsType kindLevelMode ksig (expectedKindInCtxt ctxt)
       ; checkValidType ctxt kind
       ; return (name, kind) }

tc_hs_sig_type :: SkolemInfo -> LHsSigType GhcRn
               -> ContextKind -> TcM (Bool, TcType)
-- Kind-checks/desugars an 'LHsSigType',
--   solve equalities,
--   and then kind-generalizes.
-- This will never emit constraints, as it uses solveEqualities internally.
-- No validity checking or zonking
-- Returns also a Bool indicating whether the type induced an insoluble constraint;
-- True <=> constraint is insoluble
tc_hs_sig_type skol_info hs_sig_type ctxt_kind
  | HsIB { hsib_ext = sig_vars, hsib_body = hs_ty } <- hs_sig_type
  = do { (tc_lvl, (wanted, (spec_tkvs, ty)))
              <- pushTcLevelM                           $
                 solveLocalEqualitiesX "tc_hs_sig_type" $
                 bindImplicitTKBndrs_Skol sig_vars      $
                 do { kind <- newExpectedKind ctxt_kind
                    ; tc_lhs_type typeLevelMode hs_ty kind }
       -- Any remaining variables (unsolved in the solveLocalEqualities)
       -- should be in the global tyvars, and therefore won't be quantified

       ; spec_tkvs <- zonkAndScopedSort spec_tkvs
       ; let ty1 = mkSpecForAllTys spec_tkvs ty

       -- This bit is very much like decideMonoTyVars in GHC.Tc.Solver,
       -- but constraints are so much simpler in kinds, it is much
       -- easier here. (In particular, we never quantify over a
       -- constraint in a type.)
       ; constrained <- zonkTyCoVarsAndFV (tyCoVarsOfWC wanted)
       ; let should_gen = not . (`elemVarSet` constrained)

       ; kvs <- kindGeneralizeSome should_gen ty1
       ; emitResidualTvConstraint skol_info Nothing (kvs ++ spec_tkvs)
                                  tc_lvl wanted

       ; return (insolubleWC wanted, mkInfForAllTys kvs ty1) }

tcTopLHsType :: TcTyMode -> LHsSigType GhcRn -> ContextKind -> TcM Type
-- tcTopLHsType is used for kind-checking top-level HsType where
--   we want to fully solve /all/ equalities, and report errors
-- Does zonking, but not validity checking because it's used
--   for things (like deriving and instances) that aren't
--   ordinary types
tcTopLHsType mode hs_sig_type ctxt_kind
  | HsIB { hsib_ext = sig_vars, hsib_body = hs_ty } <- hs_sig_type
  = do { traceTc "tcTopLHsType {" (ppr hs_ty)
       ; (spec_tkvs, ty)
              <- pushTcLevelM_                     $
                 solveEqualities                   $
                 bindImplicitTKBndrs_Skol sig_vars $
                 do { kind <- newExpectedKind ctxt_kind
                    ; tc_lhs_type mode hs_ty kind }

       ; spec_tkvs <- zonkAndScopedSort spec_tkvs
       ; let ty1 = mkSpecForAllTys spec_tkvs ty
       ; kvs <- kindGeneralizeAll ty1  -- "All" because it's a top-level type
       ; final_ty <- zonkTcTypeToType (mkInfForAllTys kvs ty1)
       ; traceTc "End tcTopLHsType }" (vcat [ppr hs_ty, ppr final_ty])
       ; return final_ty}

-----------------
tcHsDeriv :: LHsSigType GhcRn -> TcM ([TyVar], Class, [Type], [Kind])
-- Like tcHsSigType, but for the ...deriving( C t1 ty2 ) clause
-- Returns the C, [ty1, ty2, and the kinds of C's remaining arguments
-- E.g.    class C (a::*) (b::k->k)
--         data T a b = ... deriving( C Int )
--    returns ([k], C, [k, Int], [k->k])
-- Return values are fully zonked
tcHsDeriv hs_ty
  = do { ty <- checkNoErrs $  -- Avoid redundant error report
                              -- with "illegal deriving", below
               tcTopLHsType typeLevelMode hs_ty AnyKind
       ; let (tvs, pred)    = splitForAllTys ty
             (kind_args, _) = splitFunTys (tcTypeKind pred)
       ; case getClassPredTys_maybe pred of
           Just (cls, tys) -> return (tvs, cls, tys, kind_args)
           Nothing -> failWithTc (text "Illegal deriving item" <+> quotes (ppr hs_ty)) }

-- | Typecheck a deriving strategy. For most deriving strategies, this is a
-- no-op, but for the @via@ strategy, this requires typechecking the @via@ type.
tcDerivStrategy ::
     Maybe (LDerivStrategy GhcRn)
     -- ^ The deriving strategy
  -> TcM (Maybe (LDerivStrategy GhcTc), [TyVar])
     -- ^ The typechecked deriving strategy and the tyvars that it binds
     -- (if using 'ViaStrategy').
tcDerivStrategy mb_lds
  = case mb_lds of
      Nothing -> boring_case Nothing
      Just (L loc ds) ->
        setSrcSpan loc $ do
          (ds', tvs) <- tc_deriv_strategy ds
          pure (Just (L loc ds'), tvs)
  where
    tc_deriv_strategy :: DerivStrategy GhcRn
                      -> TcM (DerivStrategy GhcTc, [TyVar])
    tc_deriv_strategy StockStrategy    = boring_case StockStrategy
    tc_deriv_strategy AnyclassStrategy = boring_case AnyclassStrategy
    tc_deriv_strategy NewtypeStrategy  = boring_case NewtypeStrategy
    tc_deriv_strategy (ViaStrategy ty) = do
      ty' <- checkNoErrs $ tcTopLHsType typeLevelMode ty AnyKind
      let (via_tvs, via_pred) = splitForAllTys ty'
      pure (ViaStrategy via_pred, via_tvs)

    boring_case :: ds -> TcM (ds, [TyVar])
    boring_case ds = pure (ds, [])

tcHsClsInstType :: UserTypeCtxt    -- InstDeclCtxt or SpecInstCtxt
                -> LHsSigType GhcRn
                -> TcM Type
-- Like tcHsSigType, but for a class instance declaration
tcHsClsInstType user_ctxt hs_inst_ty
  = setSrcSpan (getLoc (hsSigType hs_inst_ty)) $
    do { -- Fail eagerly if tcTopLHsType fails.  We are at top level so
         -- these constraints will never be solved later. And failing
         -- eagerly avoids follow-on errors when checkValidInstance
         -- sees an unsolved coercion hole
         inst_ty <- checkNoErrs $
                    tcTopLHsType typeLevelMode hs_inst_ty (TheKind constraintKind)
       ; checkValidInstance user_ctxt hs_inst_ty inst_ty
       ; return inst_ty }

----------------------------------------------
-- | Type-check a visible type application
tcHsTypeApp :: LHsWcType GhcRn -> Kind -> TcM Type
-- See Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
tcHsTypeApp wc_ty kind
  | HsWC { hswc_ext = sig_wcs, hswc_body = hs_ty } <- wc_ty
  = do { ty <- solveLocalEqualities "tcHsTypeApp" $
               -- We are looking at a user-written type, very like a
               -- signature so we want to solve its equalities right now
               unsetWOptM Opt_WarnPartialTypeSignatures $
               setXOptM LangExt.PartialTypeSignatures $
               -- See Note [Wildcards in visible type application]
               tcNamedWildCardBinders sig_wcs $ \ _ ->
               tcCheckLHsType hs_ty (TheKind kind)
       -- We do not kind-generalize type applications: we just
       -- instantiate with exactly what the user says.
       -- See Note [No generalization in type application]
       -- We still must call kindGeneralizeNone, though, according
       -- to Note [Recipe for checking a signature]
       ; kindGeneralizeNone ty
       ; ty <- zonkTcType ty
       ; checkValidType TypeAppCtxt ty
       ; return ty }

{- Note [Wildcards in visible type application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A HsWildCardBndrs's hswc_ext now only includes /named/ wildcards, so
any unnamed wildcards stay unchanged in hswc_body.  When called in
tcHsTypeApp, tcCheckLHsType will call emitAnonTypeHole
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
tcHsOpenType ty   = addTypeCtxt ty $ tcHsOpenTypeNC ty
tcHsLiftedType ty = addTypeCtxt ty $ tcHsLiftedTypeNC ty

tcHsOpenTypeNC   ty = do { ek <- newOpenTypeKind
                         ; tc_lhs_type typeLevelMode ty ek }
tcHsLiftedTypeNC ty = tc_lhs_type typeLevelMode ty liftedTypeKind

-- Like tcHsType, but takes an expected kind
tcCheckLHsType :: LHsType GhcRn -> ContextKind -> TcM TcType
tcCheckLHsType hs_ty exp_kind
  = addTypeCtxt hs_ty $
    do { ek <- newExpectedKind exp_kind
       ; tc_lhs_type typeLevelMode hs_ty ek }

tcLHsType :: LHsType GhcRn -> TcM (TcType, TcKind)
-- Called from outside: set the context
tcLHsType ty = addTypeCtxt ty (tc_infer_lhs_type typeLevelMode ty)

-- Like tcLHsType, but use it in a context where type synonyms and type families
-- do not need to be saturated, like in a GHCi :kind call
tcLHsTypeUnsaturated :: LHsType GhcRn -> TcM (TcType, TcKind)
tcLHsTypeUnsaturated hs_ty
  | Just (hs_fun_ty, hs_args) <- splitHsAppTys (unLoc hs_ty)
  = addTypeCtxt hs_ty $
    do { (fun_ty, _ki) <- tcInferAppHead mode hs_fun_ty
       ; tcInferApps_nosat mode hs_fun_ty fun_ty hs_args }
         -- Notice the 'nosat'; do not instantiate trailing
         -- invisible arguments of a type family.
         -- See Note [Dealing with :kind]

  | otherwise
  = addTypeCtxt hs_ty $
    tc_infer_lhs_type mode hs_ty

  where
    mode = typeLevelMode

{- Note [Dealing with :kind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GHCi command
  ghci> type family F :: Either j k
  ghci> :kind F
  F :: forall {j,k}. Either j k

We will only get the 'forall' if we /refrain/ from saturating those
invisible binders. But generally we /do/ saturate those invisible
binders (see tcInferApps), and we want to do so for nested application
even in GHCi.  Consider for example (#16287)
  ghci> type family F :: k
  ghci> data T :: (forall k. k) -> Type
  ghci> :kind T F
We want to reject this. It's just at the very top level that we want
to switch off saturation.

So tcLHsTypeUnsaturated does a little special case for top level
applications.  Actually the common case is a bare variable, as above.


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

-- | Info about the context in which we're checking a type. Currently,
-- differentiates only between types and kinds, but this will likely
-- grow, at least to include the distinction between patterns and
-- not-patterns.
--
-- To find out where the mode is used, search for 'mode_level'
data TcTyMode = TcTyMode { mode_level :: TypeOrKind }

typeLevelMode :: TcTyMode
typeLevelMode = TcTyMode { mode_level = TypeLevel }

kindLevelMode :: TcTyMode
kindLevelMode = TcTyMode { mode_level = KindLevel }

-- switch to kind level
kindLevel :: TcTyMode -> TcTyMode
kindLevel mode = mode { mode_level = KindLevel }

instance Outputable TcTyMode where
  ppr = ppr . mode_level

{-
Note [Bidirectional type checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In expressions, whenever we see a polymorphic identifier, say `id`, we are
free to instantiate it with metavariables, knowing that we can always
re-generalize with type-lambdas when necessary. For example:

  rank2 :: (forall a. a -> a) -> ()
  x = rank2 id

When checking the body of `x`, we can instantiate `id` with a metavariable.
Then, when we're checking the application of `rank2`, we notice that we really
need a polymorphic `id`, and then re-generalize over the unconstrained
metavariable.

In types, however, we're not so lucky, because *we cannot re-generalize*!
There is no lambda. So, we must be careful only to instantiate at the last
possible moment, when we're sure we're never going to want the lost polymorphism
again. This is done in calls to tcInstInvisibleTyBinders.

To implement this behavior, we use bidirectional type checking, where we
explicitly think about whether we know the kind of the type we're checking
or not. Note that there is a difference between not knowing a kind and
knowing a metavariable kind: the metavariables are TauTvs, and cannot become
forall-quantified kinds. Previously (before dependent types), there were
no higher-rank kinds, and so we could instantiate early and be sure that
no types would have polymorphic kinds, and so we could always assume that
the kind of a type was a fresh metavariable. Not so anymore, thus the
need for two algorithms.

For HsType forms that can never be kind-polymorphic, we implement only the
"down" direction, where we safely assume a metavariable kind. For HsType forms
that *can* be kind-polymorphic, we implement just the "up" (functions with
"infer" in their name) version, as we gain nothing by also implementing the
"down" version.

Note [Future-proofing the type checker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As discussed in Note [Bidirectional type checking], each HsType form is
handled in *either* tc_infer_hs_type *or* tc_hs_type. These functions
are mutually recursive, so that either one can work for any type former.
But, we want to make sure that our pattern-matches are complete. So,
we have a bunch of repetitive code just so that we get warnings if we're
missing any patterns.

-}

------------------------------------------
-- | Check and desugar a type, returning the core type and its
-- possibly-polymorphic kind. Much like 'tcInferRho' at the expression
-- level.
tc_infer_lhs_type :: TcTyMode -> LHsType GhcRn -> TcM (TcType, TcKind)
tc_infer_lhs_type mode (L span ty)
  = setSrcSpan span $
    tc_infer_hs_type mode ty

---------------------------
-- | Call 'tc_infer_hs_type' and check its result against an expected kind.
tc_infer_hs_type_ek :: HasDebugCallStack => TcTyMode -> HsType GhcRn -> TcKind -> TcM TcType
tc_infer_hs_type_ek mode hs_ty ek
  = do { (ty, k) <- tc_infer_hs_type mode hs_ty
       ; checkExpectedKind hs_ty ty k ek }

---------------------------
-- | Infer the kind of a type and desugar. This is the "up" type-checker,
-- as described in Note [Bidirectional type checking]
tc_infer_hs_type :: TcTyMode -> HsType GhcRn -> TcM (TcType, TcKind)

tc_infer_hs_type mode (HsParTy _ t)
  = tc_infer_lhs_type mode t

tc_infer_hs_type mode ty
  | Just (hs_fun_ty, hs_args) <- splitHsAppTys ty
  = do { (fun_ty, _ki) <- tcInferAppHead mode hs_fun_ty
       ; tcInferApps mode hs_fun_ty fun_ty hs_args }

tc_infer_hs_type mode (HsKindSig _ ty sig)
  = do { sig' <- tcLHsKindSig KindSigCtxt sig
                 -- We must typecheck the kind signature, and solve all
                 -- its equalities etc; from this point on we may do
                 -- things like instantiate its foralls, so it needs
                 -- to be fully determined (#14904)
       ; traceTc "tc_infer_hs_type:sig" (ppr ty $$ ppr sig')
       ; ty' <- tc_lhs_type mode ty sig'
       ; return (ty', sig') }

-- HsSpliced is an annotation produced by 'GHC.Rename.Splice.rnSpliceType' to communicate
-- the splice location to the typechecker. Here we skip over it in order to have
-- the same kind inferred for a given expression whether it was produced from
-- splices or not.
--
-- See Note [Delaying modFinalizers in untyped splices].
tc_infer_hs_type mode (HsSpliceTy _ (HsSpliced _ _ (HsSplicedTy ty)))
  = tc_infer_hs_type mode ty

tc_infer_hs_type mode (HsDocTy _ ty _) = tc_infer_lhs_type mode ty
tc_infer_hs_type _    (XHsType (NHsCoreTy ty))
  = return (ty, tcTypeKind ty)

tc_infer_hs_type _ (HsExplicitListTy _ _ tys)
  | null tys  -- this is so that we can use visible kind application with '[]
              -- e.g ... '[] @Bool
  = return (mkTyConTy promotedNilDataCon,
            mkSpecForAllTys [alphaTyVar] $ mkListTy alphaTy)

tc_infer_hs_type mode other_ty
  = do { kv <- newMetaKindVar
       ; ty' <- tc_hs_type mode other_ty kv
       ; return (ty', kv) }

------------------------------------------
tc_lhs_type :: TcTyMode -> LHsType GhcRn -> TcKind -> TcM TcType
tc_lhs_type mode (L span ty) exp_kind
  = setSrcSpan span $
    tc_hs_type mode ty exp_kind

tc_hs_type :: TcTyMode -> HsType GhcRn -> TcKind -> TcM TcType
-- See Note [Bidirectional type checking]

tc_hs_type mode (HsParTy _ ty)   exp_kind = tc_lhs_type mode ty exp_kind
tc_hs_type mode (HsDocTy _ ty _) exp_kind = tc_lhs_type mode ty exp_kind
tc_hs_type _ ty@(HsBangTy _ bang _) _
    -- While top-level bangs at this point are eliminated (eg !(Maybe Int)),
    -- other kinds of bangs are not (eg ((!Maybe) Int)). These kinds of
    -- bangs are invalid, so fail. (#7210, #14761)
    = do { let bangError err = failWith $
                 text "Unexpected" <+> text err <+> text "annotation:" <+> ppr ty $$
                 text err <+> text "annotation cannot appear nested inside a type"
         ; case bang of
             HsSrcBang _ SrcUnpack _           -> bangError "UNPACK"
             HsSrcBang _ SrcNoUnpack _         -> bangError "NOUNPACK"
             HsSrcBang _ NoSrcUnpack SrcLazy   -> bangError "laziness"
             HsSrcBang _ _ _                   -> bangError "strictness" }
tc_hs_type _ ty@(HsRecTy {})      _
      -- Record types (which only show up temporarily in constructor
      -- signatures) should have been removed by now
    = failWithTc (text "Record syntax is illegal here:" <+> ppr ty)

-- HsSpliced is an annotation produced by 'GHC.Rename.Splice.rnSpliceType'.
-- Here we get rid of it and add the finalizers to the global environment
-- while capturing the local environment.
--
-- See Note [Delaying modFinalizers in untyped splices].
tc_hs_type mode (HsSpliceTy _ (HsSpliced _ mod_finalizers (HsSplicedTy ty)))
           exp_kind
  = do addModFinalizersWithLclEnv mod_finalizers
       tc_hs_type mode ty exp_kind

-- This should never happen; type splices are expanded by the renamer
tc_hs_type _ ty@(HsSpliceTy {}) _exp_kind
  = failWithTc (text "Unexpected type splice:" <+> ppr ty)

---------- Functions and applications
tc_hs_type mode (HsFunTy _ ty1 ty2) exp_kind
  = tc_fun_type mode ty1 ty2 exp_kind

tc_hs_type mode (HsOpTy _ ty1 (L _ op) ty2) exp_kind
  | op `hasKey` funTyConKey
  = tc_fun_type mode ty1 ty2 exp_kind

--------- Foralls
tc_hs_type mode forall@(HsForAllTy { hst_fvf = fvf, hst_bndrs = hs_tvs
                                   , hst_body = ty }) exp_kind
  = do { (tclvl, wanted, (inv_tv_bndrs, ty'))
            <- pushLevelAndCaptureConstraints $
               bindExplicitTKBndrs_Skol hs_tvs $
               tc_lhs_type mode ty exp_kind
    -- Do not kind-generalise here!  See Note [Kind generalisation]
    -- Why exp_kind?  See Note [Body kind of HsForAllTy]
       ; let skol_info   = ForAllSkol (ppr forall)
             m_telescope = Just (sep (map ppr hs_tvs))

       ; tv_bndrs <- mapM construct_bndr inv_tv_bndrs

       ; emitResidualTvConstraint skol_info m_telescope (binderVars tv_bndrs) tclvl wanted

       ; return (mkForAllTys tv_bndrs ty') }
  where
    construct_bndr :: TcInvisTVBinder -> TcM TcTyVarBinder
    construct_bndr (Bndr tv spec) = do { argf <- spec_to_argf spec
                                       ; return $ mkTyVarBinder argf tv }

    -- See Note [Variable Specificity and Forall Visibility]
    spec_to_argf :: Specificity -> TcM ArgFlag
    spec_to_argf SpecifiedSpec = case fvf of
      ForallVis   -> return Required
      ForallInvis -> return Specified
    spec_to_argf InferredSpec  = case fvf of
      ForallVis   -> do { addErrTc (hang (text "Unexpected inferred variable in visible forall binder:")
                                         2 (ppr forall))
                        ; return Required }
      ForallInvis -> return Inferred

tc_hs_type mode (HsQualTy { hst_ctxt = ctxt, hst_body = rn_ty }) exp_kind
  | null (unLoc ctxt)
  = tc_lhs_type mode rn_ty exp_kind

  -- See Note [Body kind of a HsQualTy]
  | tcIsConstraintKind exp_kind
  = do { ctxt' <- tc_hs_context mode ctxt
       ; ty'   <- tc_lhs_type mode rn_ty constraintKind
       ; return (mkPhiTy ctxt' ty') }

  | otherwise
  = do { ctxt' <- tc_hs_context mode ctxt

       ; ek <- newOpenTypeKind  -- The body kind (result of the function) can
                                -- be TYPE r, for any r, hence newOpenTypeKind
       ; ty' <- tc_lhs_type mode rn_ty ek
       ; checkExpectedKind (unLoc rn_ty) (mkPhiTy ctxt' ty')
                           liftedTypeKind exp_kind }

--------- Lists, arrays, and tuples
tc_hs_type mode rn_ty@(HsListTy _ elt_ty) exp_kind
  = do { tau_ty <- tc_lhs_type mode elt_ty liftedTypeKind
       ; checkWiredInTyCon listTyCon
       ; checkExpectedKind rn_ty (mkListTy tau_ty) liftedTypeKind exp_kind }

-- See Note [Distinguishing tuple kinds] in GHC.Hs.Type
-- See Note [Inferring tuple kinds]
tc_hs_type mode rn_ty@(HsTupleTy _ HsBoxedOrConstraintTuple hs_tys) exp_kind
     -- (NB: not zonking before looking at exp_k, to avoid left-right bias)
  | Just tup_sort <- tupKindSort_maybe exp_kind
  = traceTc "tc_hs_type tuple" (ppr hs_tys) >>
    tc_tuple rn_ty mode tup_sort hs_tys exp_kind
  | otherwise
  = do { traceTc "tc_hs_type tuple 2" (ppr hs_tys)
       ; (tys, kinds) <- mapAndUnzipM (tc_infer_lhs_type mode) hs_tys
       ; kinds <- mapM zonkTcType kinds
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

       ; tys' <- sequence [ setSrcSpan loc $
                            checkExpectedKind hs_ty ty kind arg_kind
                          | ((L loc hs_ty),ty,kind) <- zip3 hs_tys tys kinds ]

       ; finish_tuple rn_ty tup_sort tys' (map (const arg_kind) tys') exp_kind }


tc_hs_type mode rn_ty@(HsTupleTy _ hs_tup_sort tys) exp_kind
  = tc_tuple rn_ty mode tup_sort tys exp_kind
  where
    tup_sort = case hs_tup_sort of  -- Fourth case dealt with above
                  HsUnboxedTuple    -> UnboxedTuple
                  HsBoxedTuple      -> BoxedTuple
                  HsConstraintTuple -> ConstraintTuple
                  _                 -> panic "tc_hs_type HsTupleTy"

tc_hs_type mode rn_ty@(HsSumTy _ hs_tys) exp_kind
  = do { let arity = length hs_tys
       ; arg_kinds <- mapM (\_ -> newOpenTypeKind) hs_tys
       ; tau_tys   <- zipWithM (tc_lhs_type mode) hs_tys arg_kinds
       ; let arg_reps = map kindRep arg_kinds
             arg_tys  = arg_reps ++ tau_tys
             sum_ty   = mkTyConApp (sumTyCon arity) arg_tys
             sum_kind = unboxedSumKind arg_reps
       ; checkExpectedKind rn_ty sum_ty sum_kind exp_kind
       }

--------- Promoted lists and tuples
tc_hs_type mode rn_ty@(HsExplicitListTy _ _ tys) exp_kind
  = do { tks <- mapM (tc_infer_lhs_type mode) tys
       ; (taus', kind) <- unifyKinds tys tks
       ; let ty = (foldr (mk_cons kind) (mk_nil kind) taus')
       ; checkExpectedKind rn_ty ty (mkListTy kind) exp_kind }
  where
    mk_cons k a b = mkTyConApp (promoteDataCon consDataCon) [k, a, b]
    mk_nil  k     = mkTyConApp (promoteDataCon nilDataCon) [k]

tc_hs_type mode rn_ty@(HsExplicitTupleTy _ tys) exp_kind
  -- using newMetaKindVar means that we force instantiations of any polykinded
  -- types. At first, I just used tc_infer_lhs_type, but that led to #11255.
  = do { ks   <- replicateM arity newMetaKindVar
       ; taus <- zipWithM (tc_lhs_type mode) tys ks
       ; let kind_con   = tupleTyCon           Boxed arity
             ty_con     = promotedTupleDataCon Boxed arity
             tup_k      = mkTyConApp kind_con ks
       ; checkExpectedKind rn_ty (mkTyConApp ty_con (ks ++ taus)) tup_k exp_kind }
  where
    arity = length tys

--------- Constraint types
tc_hs_type mode rn_ty@(HsIParamTy _ (L _ n) ty) exp_kind
  = do { MASSERT( isTypeLevel (mode_level mode) )
       ; ty' <- tc_lhs_type mode ty liftedTypeKind
       ; let n' = mkStrLitTy $ hsIPNameFS n
       ; ipClass <- tcLookupClass ipClassName
       ; checkExpectedKind rn_ty (mkClassPred ipClass [n',ty'])
                           constraintKind exp_kind }

tc_hs_type _ rn_ty@(HsStarTy _ _) exp_kind
  -- Desugaring 'HsStarTy' to 'Data.Kind.Type' here means that we don't have to
  -- handle it in 'coreView' and 'tcView'.
  = checkExpectedKind rn_ty liftedTypeKind liftedTypeKind exp_kind

--------- Literals
tc_hs_type _ rn_ty@(HsTyLit _ (HsNumTy _ n)) exp_kind
  = do { checkWiredInTyCon typeNatKindCon
       ; checkExpectedKind rn_ty (mkNumLitTy n) typeNatKind exp_kind }

tc_hs_type _ rn_ty@(HsTyLit _ (HsStrTy _ s)) exp_kind
  = do { checkWiredInTyCon typeSymbolKindCon
       ; checkExpectedKind rn_ty (mkStrLitTy s) typeSymbolKind exp_kind }

--------- Potentially kind-polymorphic types: call the "up" checker
-- See Note [Future-proofing the type checker]
tc_hs_type mode ty@(HsTyVar {})            ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsAppTy {})            ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsAppKindTy{})         ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsOpTy {})             ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsKindSig {})          ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(XHsType (NHsCoreTy{})) ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type _    wc@(HsWildCardTy _)        ek = tcAnonWildCardOcc wc ek

{-
Note [Variable Specificity and Forall Visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A HsForAllTy contains a ForAllVisFlag to denote the visibility of the forall
binder. Furthermore, each bound variable also has a Specificity. Together these
determine the variable binders (ArgFlag) for each variable in the generated
ForAllTy type.

This table summarises this relation:
--------------------------------------------------------------------------
| User-written type         ForAllVisFlag     Specificity        ArgFlag
|-------------------------------------------------------------------------
| f :: forall a. type       ForallInvis       SpecifiedSpec      Specified
| f :: forall {a}. type     ForallInvis       InferredSpec       Inferred
| f :: forall a -> type     ForallVis         SpecifiedSpec      Required
| f :: forall {a} -> type   ForallVis         InferredSpec       /
|   This last form is non-sensical and is thus rejected.
--------------------------------------------------------------------------

For more information regarding the interpretation of the resulting ArgFlag, see
Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in TyCoRep.
-}

------------------------------------------
tc_fun_type :: TcTyMode -> LHsType GhcRn -> LHsType GhcRn -> TcKind
            -> TcM TcType
tc_fun_type mode ty1 ty2 exp_kind = case mode_level mode of
  TypeLevel ->
    do { arg_k <- newOpenTypeKind
       ; res_k <- newOpenTypeKind
       ; ty1' <- tc_lhs_type mode ty1 arg_k
       ; ty2' <- tc_lhs_type mode ty2 res_k
       ; checkExpectedKind (HsFunTy noExtField ty1 ty2) (mkVisFunTy ty1' ty2')
                           liftedTypeKind exp_kind }
  KindLevel ->  -- no representation polymorphism in kinds. yet.
    do { ty1' <- tc_lhs_type mode ty1 liftedTypeKind
       ; ty2' <- tc_lhs_type mode ty2 liftedTypeKind
       ; checkExpectedKind (HsFunTy noExtField ty1 ty2) (mkVisFunTy ty1' ty2')
                           liftedTypeKind exp_kind }

---------------------------
tcAnonWildCardOcc :: HsType GhcRn -> Kind -> TcM TcType
tcAnonWildCardOcc wc exp_kind
  = do { wc_tv <- newWildTyVar  -- The wildcard's kind will be an un-filled-in meta tyvar

       ; part_tysig <- xoptM LangExt.PartialTypeSignatures
       ; warning <- woptM Opt_WarnPartialTypeSignatures

       ; unless (part_tysig && not warning) $
         emitAnonTypeHole wc_tv
         -- Why the 'unless' guard?
         -- See Note [Wildcards in visible kind application]

       ; checkExpectedKind wc (mkTyVarTy wc_tv)
                           (tyVarKind wc_tv) exp_kind }

{- Note [Wildcards in visible kind application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are cases where users might want to pass in a wildcard as a visible kind
argument, for instance:

data T :: forall k1 k2. k1 → k2 → Type where
  MkT :: T a b
x :: T @_ @Nat False n
x = MkT

So we should allow '@_' without emitting any hole constraints, and
regardless of whether PartialTypeSignatures is enabled or not. But how would
the typechecker know which '_' is being used in VKA and which is not when it
calls emitNamedTypeHole in tcHsPartialSigType on all HsWildCardBndrs?
The solution then is to neither rename nor include unnamed wildcards in HsWildCardBndrs,
but instead give every anonymous wildcard a fresh wild tyvar in tcAnonWildCardOcc.
And whenever we see a '@', we automatically turn on PartialTypeSignatures and
turn off hole constraint warnings, and do not call emitAnonTypeHole
under these conditions.
See related Note [Wildcards in visible type application] here and
Note [The wildcard story for types] in GHC.Hs.Type

Note [Skolem escape and forall-types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
emitResidualTvConstraint is all about, when kind-checking
HsForAllTy.

Note that we don't need to /simplify/ the constraints here
because we aren't generalising. We just capture them.
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
  | Just k'      <- tcView k            = tupKindSort_maybe k'
  | tcIsConstraintKind k = Just ConstraintTuple
  | tcIsLiftedTypeKind k   = Just BoxedTuple
  | otherwise            = Nothing

tc_tuple :: HsType GhcRn -> TcTyMode -> TupleSort -> [LHsType GhcRn] -> TcKind -> TcM TcType
tc_tuple rn_ty mode tup_sort tys exp_kind
  = do { arg_kinds <- case tup_sort of
           BoxedTuple      -> return (replicate arity liftedTypeKind)
           UnboxedTuple    -> replicateM arity newOpenTypeKind
           ConstraintTuple -> return (replicate arity constraintKind)
       ; tau_tys <- zipWithM (tc_lhs_type mode) tys arg_kinds
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
      |  arity > mAX_CTUPLE_SIZE
      -> failWith (bigConstraintTuple arity)
      |  otherwise
      -> do tycon <- tcLookupTyCon (cTupleTyConName arity)
            check_expected_kind (mkTyConApp tycon tau_tys) constraintKind
    BoxedTuple -> do
      let tycon = tupleTyCon Boxed arity
      checkWiredInTyCon tycon
      check_expected_kind (mkTyConApp tycon tau_tys) liftedTypeKind
    UnboxedTuple ->
      let tycon    = tupleTyCon Unboxed arity
          tau_reps = map kindRep tau_kinds
          -- See also Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
          arg_tys  = tau_reps ++ tau_tys
          res_kind = unboxedTupleKind tau_reps in
      check_expected_kind (mkTyConApp tycon arg_tys) res_kind
  where
    arity = length tau_tys
    check_expected_kind ty act_kind =
      checkExpectedKind rn_ty ty act_kind exp_kind

bigConstraintTuple :: Arity -> MsgDoc
bigConstraintTuple arity
  = hang (text "Constraint tuple arity too large:" <+> int arity
          <+> parens (text "max arity =" <+> int mAX_CTUPLE_SIZE))
       2 (text "Instead, use a nested tuple")

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

splitHsAppTys :: HsType GhcRn -> Maybe (LHsType GhcRn, [LHsTypeArg GhcRn])
splitHsAppTys hs_ty
  | is_app hs_ty = Just (go (noLoc hs_ty) [])
  | otherwise    = Nothing
  where
    is_app :: HsType GhcRn -> Bool
    is_app (HsAppKindTy {})        = True
    is_app (HsAppTy {})            = True
    is_app (HsOpTy _ _ (L _ op) _) = not (op `hasKey` funTyConKey)
      -- I'm not sure why this funTyConKey test is necessary
      -- Can it even happen?  Perhaps for   t1 `(->)` t2
      -- but then maybe it's ok to treat that like a normal
      -- application rather than using the special rule for HsFunTy
    is_app (HsTyVar {})            = True
    is_app (HsParTy _ (L _ ty))    = is_app ty
    is_app _                       = False

    go (L _  (HsAppTy _ f a))      as = go f (HsValArg a : as)
    go (L _  (HsAppKindTy l ty k)) as = go ty (HsTypeArg l k : as)
    go (L sp (HsParTy _ f))        as = go f (HsArgPar sp : as)
    go (L _  (HsOpTy _ l op@(L sp _) r)) as
      = ( L sp (HsTyVar noExtField NotPromoted op)
        , HsValArg l : HsValArg r : as )
    go f as = (f, as)

---------------------------
tcInferAppHead :: TcTyMode -> LHsType GhcRn -> TcM (TcType, TcKind)
-- Version of tc_infer_lhs_type specialised for the head of an
-- application. In particular, for a HsTyVar (which includes type
-- constructors, it does not zoom off into tcInferApps and family
-- saturation
tcInferAppHead mode (L _ (HsTyVar _ _ (L _ tv)))
  = tcTyVar mode tv
tcInferAppHead mode ty
  = tc_infer_lhs_type mode ty

---------------------------
-- | Apply a type of a given kind to a list of arguments. This instantiates
-- invisible parameters as necessary. Always consumes all the arguments,
-- using matchExpectedFunKind as necessary.
-- This takes an optional @VarEnv Kind@ which maps kind variables to kinds.-
-- These kinds should be used to instantiate invisible kind variables;
-- they come from an enclosing class for an associated type/data family.
--
-- tcInferApps also arranges to saturate any trailing invisible arguments
--   of a type-family application, which is usually the right thing to do
-- tcInferApps_nosat does not do this saturation; it is used only
--   by ":kind" in GHCi
tcInferApps, tcInferApps_nosat
    :: TcTyMode
    -> LHsType GhcRn        -- ^ Function (for printing only)
    -> TcType               -- ^ Function
    -> [LHsTypeArg GhcRn]   -- ^ Args
    -> TcM (TcType, TcKind) -- ^ (f args, args, result kind)
tcInferApps mode hs_ty fun hs_args
  = do { (f_args, res_k) <- tcInferApps_nosat mode hs_ty fun hs_args
       ; saturateFamApp f_args res_k }

tcInferApps_nosat mode orig_hs_ty fun orig_hs_args
  = do { traceTc "tcInferApps {" (ppr orig_hs_ty $$ ppr orig_hs_args)
       ; (f_args, res_k) <- go_init 1 fun orig_hs_args
       ; traceTc "tcInferApps }" (ppr f_args <+> dcolon <+> ppr res_k)
       ; return (f_args, res_k) }
  where

    -- go_init just initialises the auxiliary
    -- arguments of the 'go' loop
    go_init n fun all_args
      = go n fun empty_subst fun_ki all_args
      where
        fun_ki = tcTypeKind fun
           -- We do (tcTypeKind fun) here, even though the caller
           -- knows the function kind, to absolutely guarantee
           -- INVARIANT for 'go'
           -- Note that in a typical application (F t1 t2 t3),
           -- the 'fun' is just a TyCon, so tcTypeKind is fast

        empty_subst = mkEmptyTCvSubst $ mkInScopeSet $
                      tyCoVarsOfType fun_ki

    go :: Int             -- The # of the next argument
       -> TcType          -- Function applied to some args
       -> TCvSubst        -- Applies to function kind
       -> TcKind          -- Function kind
       -> [LHsTypeArg GhcRn]    -- Un-type-checked args
       -> TcM (TcType, TcKind)  -- Result type and its kind
    -- INVARIANT: in any call (go n fun subst fun_ki args)
    --               tcTypeKind fun  =  subst(fun_ki)
    -- So the 'subst' and 'fun_ki' arguments are simply
    -- there to avoid repeatedly calling tcTypeKind.
    --
    -- Reason for INVARIANT: to support the Purely Kinded Type Invariant
    -- it's important that if fun_ki has a forall, then so does
    -- (tcTypeKind fun), because the next thing we are going to do
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
        Named (Bndr _ Inferred) -> instantiate ki_binder inner_ki
        Anon InvisArg _         -> instantiate ki_binder inner_ki

        Named (Bndr _ Specified) ->  -- Visible kind application
          do { traceTc "tcInferApps (vis kind app)"
                       (vcat [ ppr ki_binder, ppr hs_ki_arg
                             , ppr (tyBinderType ki_binder)
                             , ppr subst ])

             ; let exp_kind = substTy subst $ tyBinderType ki_binder

             ; ki_arg <- addErrCtxt (funAppCtxt orig_hs_ty hs_ki_arg n) $
                         unsetWOptM Opt_WarnPartialTypeSignatures $
                         setXOptM LangExt.PartialTypeSignatures $
                             -- Urgh!  see Note [Wildcards in visible kind application]
                             -- ToDo: must kill this ridiculous messing with DynFlags
                         tc_lhs_type (kindLevel mode) hs_ki_arg exp_kind

             ; traceTc "tcInferApps (vis kind app)" (ppr exp_kind)
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
      (HsValArg arg : args, Just (ki_binder, inner_ki))
        -- next binder is invisible; need to instantiate it
        | isInvisibleBinder ki_binder   -- FunTy with InvisArg on LHS;
                                        -- or ForAllTy with Inferred or Specified
         -> instantiate ki_binder inner_ki

        -- "normal" case
        | otherwise
         -> do { traceTc "tcInferApps (vis normal app)"
                          (vcat [ ppr ki_binder
                                , ppr arg
                                , ppr (tyBinderType ki_binder)
                                , ppr subst ])
                ; let exp_kind = substTy subst $ tyBinderType ki_binder
                ; arg' <- addErrCtxt (funAppCtxt orig_hs_ty arg n) $
                          tc_lhs_type mode arg exp_kind
                ; traceTc "tcInferApps (vis normal app) 2" (ppr exp_kind)
                ; (subst', fun') <- mkAppTyM subst fun ki_binder arg'
                ; go (n+1) fun' subst' inner_ki args }

          -- no binder; try applying the substitution, or infer another arrow in fun kind
      (HsValArg _ : _, Nothing)
        -> try_again_after_substing_or $
           do { let arrows_needed = n_initial_val_args all_args
              ; co <- matchExpectedFunKind hs_ty arrows_needed substed_fun_ki

              ; fun' <- zonkTcType (fun `mkTcCastTy` co)
                     -- This zonk is essential, to expose the fruits
                     -- of matchExpectedFunKind to the 'go' loop

              ; traceTc "tcInferApps (no binder)" $
                   vcat [ ppr fun <+> dcolon <+> ppr fun_ki
                        , ppr arrows_needed
                        , ppr co
                        , ppr fun' <+> dcolon <+> ppr (tcTypeKind fun')]
              ; go_init n fun' all_args }
                -- Use go_init to establish go's INVARIANT
      where
        instantiate ki_binder inner_ki
          = do { traceTc "tcInferApps (need to instantiate)"
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

        zapped_subst   = zapTCvSubst subst
        substed_fun_ki = substTy subst fun_ki
        hs_ty          = appTypeToArg orig_hs_ty (take (n-1) orig_hs_args)

    n_initial_val_args :: [HsArg tm ty] -> Arity
    -- Count how many leading HsValArgs we have
    n_initial_val_args (HsValArg {} : args) = 1 + n_initial_val_args args
    n_initial_val_args (HsArgPar {} : args) = n_initial_val_args args
    n_initial_val_args _                    = 0

    ty_app_err arg ty
      = failWith $ text "Cannot apply function of kind" <+> quotes (ppr ty)
                $$ text "to visible kind argument" <+> quotes (ppr arg)


mkAppTyM :: TCvSubst
         -> TcType -> TyCoBinder    -- fun, plus its top-level binder
         -> TcType                  -- arg
         -> TcM (TCvSubst, TcType)  -- Extended subst, plus (fun arg)
-- Precondition: the application (fun arg) is well-kinded after zonking
--               That is, the application makes sense
--
-- Precondition: for (mkAppTyM subst fun bndr arg)
--       tcTypeKind fun  =  Pi bndr. body
--  That is, fun always has a ForAllTy or FunTy at the top
--           and 'bndr' is fun's pi-binder
--
-- Postcondition: if fun and arg satisfy (PKTI), the purely-kinded type
--                invariant, then so does the result type (fun arg)
--
-- We do not require that
--    tcTypeKind arg = tyVarKind (binderVar bndr)
-- This must be true after zonking (precondition 1), but it's not
-- required for the (PKTI).
mkAppTyM subst fun ki_binder arg
  | -- See Note [mkAppTyM]: Nasty case 2
    TyConApp tc args <- fun
  , isTypeSynonymTyCon tc
  , args `lengthIs` (tyConArity tc - 1)
  , any isTrickyTvBinder (tyConTyVars tc) -- We could cache this in the synonym
  = do { arg'  <- zonkTcType  arg
       ; args' <- zonkTcTypes args
       ; let subst' = case ki_binder of
                        Anon {}           -> subst
                        Named (Bndr tv _) -> extendTvSubstAndInScope subst tv arg'
       ; return (subst', mkTyConApp tc (args' ++ [arg'])) }


mkAppTyM subst fun (Anon {}) arg
   = return (subst, mk_app_ty fun arg)

mkAppTyM subst fun (Named (Bndr tv _)) arg
  = do { arg' <- if isTrickyTvBinder tv
                 then -- See Note [mkAppTyM]: Nasty case 1
                      zonkTcType arg
                 else return     arg
       ; return ( extendTvSubstAndInScope subst tv arg'
                , mk_app_ty fun arg' ) }

mk_app_ty :: TcType -> TcType -> TcType
-- This function just adds an ASSERT for mkAppTyM's precondition
mk_app_ty fun arg
  = ASSERT2( isPiTy fun_kind
           ,  ppr fun <+> dcolon <+> ppr fun_kind $$ ppr arg )
    mkAppTy fun arg
  where
    fun_kind = tcTypeKind fun

isTrickyTvBinder :: TcTyVar -> Bool
-- NB: isTrickyTvBinder is just an optimisation
-- It would be absolutely sound to return True always
isTrickyTvBinder tv = isPiTy (tyVarKind tv)

{- Note [The Purely Kinded Type Invariant (PKTI)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During type inference, we maintain this invariant

 (PKTI) It is legal to call 'tcTypeKind' on any Type ty,
        on any sub-term of ty, /without/ zonking ty

        Moreover, any such returned kind
        will itself satisfy (PKTI)

By "legal to call tcTypeKind" we mean "tcTypeKind will not crash".
The way in which tcTypeKind can crash is in applications
    (a t1 t2 .. tn)
if 'a' is a type variable whose kind doesn't have enough arrows
or foralls.  (The crash is in piResultTys.)

The loop in tcInferApps has to be very careful to maintain the (PKTI).
For example, suppose
    kappa is a unification variable
    We have already unified kappa := Type
      yielding    co :: Refl (Type -> Type)
    a :: kappa
then consider the type
    (a Int)
If we call tcTypeKind on that, we'll crash, because the (un-zonked)
kind of 'a' is just kappa, not an arrow kind.  So we must zonk first.

So the type inference engine is very careful when building applications.
This happens in tcInferApps. Suppose we are kind-checking the type (a Int),
where (a :: kappa).  Then in tcInferApps we'll run out of binders on
a's kind, so we'll call matchExpectedFunKind, and unify
   kappa := kappa1 -> kappa2,  with evidence co :: kappa ~ (kappa1 ~ kappa2)
At this point we must zonk the function type to expose the arrrow, so
that (a Int) will satisfy (PKTI).

The absence of this caused #14174 and #14520.

The calls to mkAppTyM is the other place we are very careful.

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
--     tcTypeKind ty = kind
--
-- If 'ty' is an unsaturated family application with trailing
-- invisible arguments, instanttiate them.
-- See Note [saturateFamApp]

saturateFamApp ty kind
  | Just (tc, args) <- tcSplitTyConApp_maybe ty
  , mustBeSaturated tc
  , let n_to_inst = tyConArity tc - length args
  = do { (extra_args, ki') <- tcInstInvisibleTyBinders n_to_inst kind
       ; return (ty `mkTcAppTys` extra_args, ki') }
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
appTypeToArg f (HsValArg arg    : args) = appTypeToArg (mkHsAppTy f arg) args
appTypeToArg f (HsArgPar _      : args) = appTypeToArg f                 args
appTypeToArg f (HsTypeArg l arg : args)
  = appTypeToArg (mkHsAppKindTy l f arg) args


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

       ; (new_args, act_kind') <- tcInstInvisibleTyBinders n_to_inst act_kind

       ; let origin = TypeEqOrigin { uo_actual   = act_kind'
                                   , uo_expected = exp_kind
                                   , uo_thing    = Just (ppr hs_ty)
                                   , uo_visible  = True } -- the hs_ty is visible

       ; traceTc "checkExpectedKindX" $
         vcat [ ppr hs_ty
              , text "act_kind':" <+> ppr act_kind'
              , text "exp_kind:" <+> ppr exp_kind ]

       ; let res_ty = ty `mkTcAppTys` new_args

       ; if act_kind' `tcEqType` exp_kind
         then return res_ty  -- This is very common
         else do { co_k <- uType KindLevel origin act_kind' exp_kind
                 ; traceTc "checkExpectedKind" (vcat [ ppr act_kind
                                                     , ppr exp_kind
                                                     , ppr co_k ])
                ; return (res_ty `mkTcCastTy` co_k) } }
    where
      -- We need to make sure that both kinds have the same number of implicit
      -- foralls out front. If the actual kind has more, instantiate accordingly.
      -- Otherwise, just pass the type & kind through: the errors are caught
      -- in unifyType.
      n_exp_invis_bndrs = invisibleTyBndrCount exp_kind
      n_act_invis_bndrs = invisibleTyBndrCount act_kind
      n_to_inst         = n_act_invis_bndrs - n_exp_invis_bndrs

---------------------------
tcHsMbContext :: Maybe (LHsContext GhcRn) -> TcM [PredType]
tcHsMbContext Nothing    = return []
tcHsMbContext (Just cxt) = tcHsContext cxt

tcHsContext :: LHsContext GhcRn -> TcM [PredType]
tcHsContext = tc_hs_context typeLevelMode

tcLHsPredType :: LHsType GhcRn -> TcM PredType
tcLHsPredType = tc_lhs_pred typeLevelMode

tc_hs_context :: TcTyMode -> LHsContext GhcRn -> TcM [PredType]
tc_hs_context mode ctxt = mapM (tc_lhs_pred mode) (unLoc ctxt)

tc_lhs_pred :: TcTyMode -> LHsType GhcRn -> TcM PredType
tc_lhs_pred mode pred = tc_lhs_type mode pred constraintKind

---------------------------
tcTyVar :: TcTyMode -> Name -> TcM (TcType, TcKind)
-- See Note [Type checking recursive type and class declarations]
-- in GHC.Tc.TyCl
tcTyVar mode name         -- Could be a tyvar, a tycon, or a datacon
  = do { traceTc "lk1" (ppr name)
       ; thing <- tcLookup name
       ; case thing of
           ATyVar _ tv -> return (mkTyVarTy tv, tyVarKind tv)

           ATcTyCon tc_tc
             -> do { -- See Note [GADT kind self-reference]
                     unless (isTypeLevel (mode_level mode))
                            (promotionErr name TyConPE)
                   ; check_tc tc_tc
                   ; return (mkTyConTy tc_tc, tyConKind tc_tc) }

           AGlobal (ATyCon tc)
             -> do { check_tc tc
                   ; return (mkTyConTy tc, tyConKind tc) }

           AGlobal (AConLike (RealDataCon dc))
             -> do { data_kinds <- xoptM LangExt.DataKinds
                   ; unless (data_kinds || specialPromotedDc dc) $
                       promotionErr name NoDataKindsDC
                   ; when (isFamInstTyCon (dataConTyCon dc)) $
                       -- see #15245
                       promotionErr name FamDataConPE
                   ; let (_, _, _, theta, _, _) = dataConFullSig dc
                   ; traceTc "tcTyVar" (ppr dc <+> ppr theta $$ ppr (dc_theta_illegal_constraint theta))
                   ; case dc_theta_illegal_constraint theta of
                       Just pred -> promotionErr name $
                                    ConstrainedDataConPE pred
                       Nothing   -> pure ()
                   ; let tc = promoteDataCon dc
                   ; return (mkTyConApp tc [], tyConKind tc) }

           APromotionErr err -> promotionErr name err

           _  -> wrongThingErr "type" thing name }
  where
    check_tc :: TyCon -> TcM ()
    check_tc tc = do { data_kinds   <- xoptM LangExt.DataKinds
                     ; unless (isTypeLevel (mode_level mode) ||
                               data_kinds ||
                               isKindTyCon tc) $
                       promotionErr name NoDataKindsTC }

    -- We cannot promote a data constructor with a context that contains
    -- constraints other than equalities, so error if we find one.
    -- See Note [Constraints in kinds] in GHC.Core.TyCo.Rep
    dc_theta_illegal_constraint :: ThetaType -> Maybe PredType
    dc_theta_illegal_constraint = find (not . isEqPred)

{-
Note [GADT kind self-reference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A promoted type cannot be used in the body of that type's declaration.
#11554 shows this example, which made GHC loop:

  import Data.Kind
  data P (x :: k) = Q
  data A :: Type where
    B :: forall (a :: A). P a -> A

In order to check the constructor B, we need to have the promoted type A, but in
order to get that promoted type, B must first be checked. To prevent looping, a
TyConPE promotion error is given when tcTyVar checks an ATcTyCon in kind mode.
Any ATcTyCon is a TyCon being defined in the current recursive group (see data
type decl for TcTyThing), and all such TyCons are illegal in kinds.

#11962 proposes checking the head of a data declaration separately from
its constructors. This would allow the example above to pass.

Note [Body kind of a HsForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The body of a forall is usually a type, but in principle
there's no reason to prohibit *unlifted* types.
In fact, GHC can itself construct a function with an
unboxed tuple inside a for-all (via CPR analysis; see
typecheck/should_compile/tc170).

Moreover in instance heads we get forall-types with
kind Constraint.

It's tempting to check that the body kind is either * or #. But this is
wrong. For example:

  class C a b
  newtype N = Mk Foo deriving (C a)

We're doing newtype-deriving for C. But notice how `a` isn't in scope in
the predicate `C a`. So we quantify, yielding `forall a. C a` even though
`C a` has kind `* -> Constraint`. The `forall a. C a` is a bit cheeky, but
convenient. Bottom line: don't check for * or # here.

Note [Body kind of a HsQualTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If ctxt is non-empty, the HsQualTy really is a /function/, so the
kind of the result really is '*', and in that case the kind of the
body-type can be lifted or unlifted.

However, consider
    instance Eq a => Eq [a] where ...
or
    f :: (Eq a => Eq [a]) => blah
Here both body-kind of the HsQualTy is Constraint rather than *.
Rather crudely we tell the difference by looking at exp_kind. It's
very convenient to typecheck instance types like any other HsSigType.

Admittedly the '(Eq a => Eq [a]) => blah' case is erroneous, but it's
better to reject in checkValidType.  If we say that the body kind
should be '*' we risk getting TWO error messages, one saying that Eq
[a] doesn't have kind '*', and one saying that we need a Constraint to
the left of the outer (=>).

How do we figure out the right body kind?  Well, it's a bit of a
kludge: I just look at the expected kind.  If it's Constraint, we
must be in this instance situation context. It's a kludge because it
wouldn't work if any unification was involved to compute that result
kind -- but it isn't.  (The true way might be to use the 'mode'
parameter, but that seemed like a sledgehammer to crack a nut.)

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
      - any unconstrained kind variables are defaulted to (Any *) just
        as in GHC.Tc.Utils.Zonk.
      - there are no mutable type variables because we are
        kind-checking a type
    Reason: the returned type may be put in a TyCon or DataCon where
    it will never subsequently be zonked.

You might worry about nested scopes:
        ..a:kappa in scope..
            let f :: forall b. T '[a,b] -> Int
In this case, f's type could have a mutable kind variable kappa in it;
and we might then default it to (Any *) when dealing with f's type
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
want to default it to '*', not to (Any *).

Help functions for type applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

addTypeCtxt :: LHsType GhcRn -> TcM a -> TcM a
        -- Wrap a context around only if we want to show that contexts.
        -- Omit invisible ones and ones user's won't grok
addTypeCtxt (L _ (HsWildCardTy _)) thing = thing   -- "In the type '_'" just isn't helpful.
addTypeCtxt (L _ ty) thing
  = addErrCtxt doc thing
  where
    doc = text "In the type" <+> quotes (ppr ty)

{-
************************************************************************
*                                                                      *
                Type-variable binders
%*                                                                      *
%************************************************************************

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
Checking a user-written signature requires several steps:

 1. Generate constraints.
 2. Solve constraints.
 3. Promote tyvars and/or kind-generalize.
 4. Zonk.
 5. Check validity.

There may be some surprises in here:

Step 2 is necessary for two reasons: most signatures also bring
implicitly quantified variables into scope, and solving is necessary
to get these in the right order (see Note [Keeping implicitly
quantified variables in order]). Additionally, solving is necessary in
order to kind-generalize correctly: otherwise, we do not know which
metavariables are left unsolved.

Step 3 is done by a call to candidateQTyVarsOfType, followed by a call to
kindGeneralize{All,Some,None}. Here, we have to deal with the fact that
metatyvars generated in the type may have a bumped TcLevel, because explicit
foralls raise the TcLevel. To avoid these variables from ever being visible in
the surrounding context, we must obey the following dictum:

  Every metavariable in a type must either be
    (A) generalized, or
    (B) promoted, or        See Note [Promotion in signatures]
    (C) a cause to error    See Note [Naughty quantification candidates] in GHC.Tc.Utils.TcMType

The kindGeneralize functions do not require pre-zonking; they zonk as they
go.

If you are actually doing kind-generalization, you need to bump the level
before generating constraints, as we will only generalize variables with
a TcLevel higher than the ambient one.

After promoting/generalizing, we need to zonk again because both
promoting and generalizing fill in metavariables.

Note [Promotion in signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If an unsolved metavariable in a signature is not generalized
(because we're not generalizing the construct -- e.g., pattern
sig -- or because the metavars are constrained -- see kindGeneralizeSome)
we need to promote to maintain (WantedTvInv) of Note [TcLevel and untouchable type variables]
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
matavariable with level 2 would violate the (WantedTvInv) invariant of
Note [TcLevel and untouchable type variables]. So, instead of kind-generalizing,
we promote the metavariable to level 1. This is all done in kindGeneralizeNone.

-}

tcNamedWildCardBinders :: [Name]
                       -> ([(Name, TcTyVar)] -> TcM a)
                       -> TcM a
-- Bring into scope the /named/ wildcard binders.  Remember that
-- plain wildcards _ are anonymous and dealt with by HsWildCardTy
-- Soe Note [The wildcard story for types] in GHC.Hs.Type
tcNamedWildCardBinders wc_names thing_inside
  = do { wcs <- mapM (const newWildTyVar) wc_names
       ; let wc_prs = wc_names `zip` wcs
       ; tcExtendNameTyVarEnv wc_prs $
         thing_inside wc_prs }

newWildTyVar :: TcM TcTyVar
-- ^ New unification variable '_' for a wildcard
newWildTyVar
  = do { kind <- newMetaKindVar
       ; uniq <- newUnique
       ; details <- newMetaDetails TauTv
       ; let name  = mkSysTvName uniq (fsLit "_")
             tyvar = mkTcTyVar name kind details
       ; traceTc "newWildTyVar" (ppr tyvar)
       ; return tyvar }

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
-- user-specified kind (CUSK)?
data SAKS_or_CUSK
  = SAKS Kind  -- Standalone kind signature, fully zonked! (zonkTcTypeToType)
  | CUSK       -- Complete user-specified kind (CUSK)

instance Outputable SAKS_or_CUSK where
  ppr (SAKS k) = text "SAKS" <+> ppr k
  ppr CUSK = text "CUSK"

-- See Note [kcCheckDeclHeader vs kcInferDeclHeader]
kcDeclHeader
  :: InitialKindStrategy
  -> Name              -- ^ of the thing being checked
  -> TyConFlavour      -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn  -- ^ Binders in the header
  -> TcM ContextKind   -- ^ The result kind
  -> TcM TcTyCon       -- ^ A suitably-kinded TcTyCon
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
  -> Name              -- ^ of the thing being checked
  -> TyConFlavour      -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn  -- ^ Binders in the header
  -> TcM ContextKind   -- ^ The result kind. AnyKind == no result signature
  -> TcM TcTyCon       -- ^ A suitably-kinded generalized TcTyCon
kcCheckDeclHeader (SAKS sig) = kcCheckDeclHeader_sig sig
kcCheckDeclHeader CUSK       = kcCheckDeclHeader_cusk

kcCheckDeclHeader_cusk
  :: Name              -- ^ of the thing being checked
  -> TyConFlavour      -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn  -- ^ Binders in the header
  -> TcM ContextKind   -- ^ The result kind
  -> TcM TcTyCon       -- ^ A suitably-kinded generalized TcTyCon
kcCheckDeclHeader_cusk name flav
              (HsQTvs { hsq_ext = kv_ns
                      , hsq_explicit = hs_tvs }) kc_res_ki
  -- CUSK case
  -- See note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
  = addTyConFlavCtxt name flav $
    do { (scoped_kvs, (tc_tvs, res_kind))
           <- pushTcLevelM_                               $
              solveEqualities                             $
              bindImplicitTKBndrs_Q_Skol kv_ns            $
              bindExplicitTKBndrs_Q_Skol ctxt_kind hs_tvs $
              newExpectedKind =<< kc_res_ki

           -- Now, because we're in a CUSK,
           -- we quantify over the mentioned kind vars
       ; let spec_req_tkvs = scoped_kvs ++ tc_tvs
             all_kinds     = res_kind : map tyVarKind spec_req_tkvs

       ; candidates' <- candidateQTyVarsOfKinds all_kinds
             -- 'candidates' are all the variables that we are going to
             -- skolemise and then quantify over.  We do not include spec_req_tvs
             -- because they are /already/ skolems

       ; let non_tc_candidates = filter (not . isTcTyVar) (nonDetEltsUniqSet (tyCoVarsOfTypes all_kinds))
             candidates = candidates' { dv_kvs = dv_kvs candidates' `extendDVarSetList` non_tc_candidates }
             inf_candidates = candidates `delCandidates` spec_req_tkvs

       ; inferred <- quantifyTyVars inf_candidates
                     -- NB: 'inferred' comes back sorted in dependency order

       ; scoped_kvs <- mapM zonkTyCoVarKind scoped_kvs
       ; tc_tvs     <- mapM zonkTyCoVarKind tc_tvs
       ; res_kind   <- zonkTcType           res_kind

       ; let mentioned_kv_set = candidateKindVars candidates
             specified        = scopedSort scoped_kvs
                                -- NB: maintain the L-R order of scoped_kvs

             final_tc_binders =  mkNamedTyConBinders Inferred  inferred
                              ++ mkNamedTyConBinders Specified specified
                              ++ map (mkRequiredTyConBinder mentioned_kv_set) tc_tvs

             all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
             tycon = mkTcTyCon name final_tc_binders res_kind all_tv_prs
                               True -- it is generalised
                               flav
         -- If the ordering from
         -- Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
         -- doesn't work, we catch it here, before an error cascade
       ; checkTyConTelescope tycon

       ; traceTc "kcCheckDeclHeader_cusk " $
         vcat [ text "name" <+> ppr name
              , text "kv_ns" <+> ppr kv_ns
              , text "hs_tvs" <+> ppr hs_tvs
              , text "scoped_kvs" <+> ppr scoped_kvs
              , text "tc_tvs" <+> ppr tc_tvs
              , text "res_kind" <+> ppr res_kind
              , text "candidates" <+> ppr candidates
              , text "inferred" <+> ppr inferred
              , text "specified" <+> ppr specified
              , text "final_tc_binders" <+> ppr final_tc_binders
              , text "mkTyConKind final_tc_bndrs res_kind"
                <+> ppr (mkTyConKind final_tc_binders res_kind)
              , text "all_tv_prs" <+> ppr all_tv_prs ]

       ; return tycon }
  where
    ctxt_kind | tcFlavourIsOpen flav = TheKind liftedTypeKind
              | otherwise            = AnyKind

-- | Kind-check a 'LHsQTyVars'. Used in 'inferInitialKind' (for tycon kinds and
-- other kinds).
--
-- This function does not do telescope checking.
kcInferDeclHeader
  :: Name              -- ^ of the thing being checked
  -> TyConFlavour      -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn
  -> TcM ContextKind   -- ^ The result kind
  -> TcM TcTyCon       -- ^ A suitably-kinded non-generalized TcTyCon
kcInferDeclHeader name flav
              (HsQTvs { hsq_ext = kv_ns
                      , hsq_explicit = hs_tvs }) kc_res_ki
  -- No standalane kind signature and no CUSK.
  -- See note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl
  = addTyConFlavCtxt name flav $
    do { (scoped_kvs, (tc_tvs, res_kind))
           -- Why bindImplicitTKBndrs_Q_Tv which uses newTyVarTyVar?
           -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl
           <- bindImplicitTKBndrs_Q_Tv kv_ns            $
              bindExplicitTKBndrs_Q_Tv ctxt_kind hs_tvs $
              newExpectedKind =<< kc_res_ki
              -- Why "_Tv" not "_Skol"? See third wrinkle in
              -- Note [Inferring kinds for type declarations] in GHC.Tc.TyCl,

       ; let   -- NB: Don't add scoped_kvs to tyConTyVars, because they
               -- might unify with kind vars in other types in a mutually
               -- recursive group.
               -- See Note [Inferring kinds for type declarations] in GHC.Tc.TyCl

             tc_binders = mkAnonTyConBinders VisArg tc_tvs
               -- Also, note that tc_binders has the tyvars from only the
               -- user-written tyvarbinders. See S1 in Note [How TcTyCons work]
               -- in GHC.Tc.TyCl
               --
               -- mkAnonTyConBinder: see Note [No polymorphic recursion]

             all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
               -- NB: bindExplicitTKBndrs_Q_Tv does not clone;
               --     ditto Implicit
               -- See Note [Non-cloning for tyvar binders]

             tycon = mkTcTyCon name tc_binders res_kind all_tv_prs
                               False -- not yet generalised
                               flav

       ; traceTc "kcInferDeclHeader: not-cusk" $
         vcat [ ppr name, ppr kv_ns, ppr hs_tvs
              , ppr scoped_kvs
              , ppr tc_tvs, ppr (mkTyConKind tc_binders res_kind) ]
       ; return tycon }
  where
    ctxt_kind | tcFlavourIsOpen flav = TheKind liftedTypeKind
              | otherwise            = AnyKind

-- | Kind-check a declaration header against a standalone kind signature.
-- See Note [Arity inference in kcCheckDeclHeader_sig]
kcCheckDeclHeader_sig
  :: Kind              -- ^ Standalone kind signature, fully zonked! (zonkTcTypeToType)
  -> Name              -- ^ of the thing being checked
  -> TyConFlavour      -- ^ What sort of 'TyCon' is being checked
  -> LHsQTyVars GhcRn  -- ^ Binders in the header
  -> TcM ContextKind   -- ^ The result kind. AnyKind == no result signature
  -> TcM TcTyCon       -- ^ A suitably-kinded TcTyCon
kcCheckDeclHeader_sig kisig name flav
          (HsQTvs { hsq_ext      = implicit_nms
                  , hsq_explicit = explicit_nms }) kc_res_ki
  = addTyConFlavCtxt name flav $
    do {  -- Step 1: zip user-written binders with quantifiers from the kind signature.
          -- For example:
          --
          --   type F :: forall k -> k -> forall j. j -> Type
          --   data F i a b = ...
          --
          -- Results in the following 'zipped_binders':
          --
          --                   TyBinder      LHsTyVarBndr
          --    ---------------------------------------
          --    ZippedBinder   forall k ->   i
          --    ZippedBinder   k ->          a
          --    ZippedBinder   forall j.
          --    ZippedBinder   j ->          b
          --
          let (zipped_binders, excess_bndrs, kisig') = zipBinders kisig explicit_nms

          -- Report binders that don't have a corresponding quantifier.
          -- For example:
          --
          --   type T :: Type -> Type
          --   data T b1 b2 b3 = ...
          --
          -- Here, b1 is zipped with Type->, while b2 and b3 are excess binders.
          --
        ; unless (null excess_bndrs) $ failWithTc (tooManyBindersErr kisig' excess_bndrs)

          -- Convert each ZippedBinder to TyConBinder        for  tyConBinders
          --                       and to [(Name, TcTyVar)]  for  tcTyConScopedTyVars
        ; (vis_tcbs, concat -> explicit_tv_prs) <- mapAndUnzipM zipped_to_tcb zipped_binders

        ; (implicit_tvs, (invis_binders, r_ki))
             <- pushTcLevelM_ $
                solveEqualities $  -- #16687
                bindImplicitTKBndrs_Tv implicit_nms $
                tcExtendNameTyVarEnv explicit_tv_prs  $
                do { -- Check that inline kind annotations on binders are valid.
                     -- For example:
                     --
                     --   type T :: Maybe k -> Type
                     --   data T (a :: Maybe j) = ...
                     --
                     -- Here we unify   Maybe k ~ Maybe j
                     mapM_ check_zipped_binder zipped_binders

                     -- Kind-check the result kind annotation, if present:
                     --
                     --    data T a b :: res_ki where
                     --               ^^^^^^^^^
                     -- We do it here because at this point the environment has been
                     -- extended with both 'implicit_tcv_prs' and 'explicit_tv_prs'.
                   ; ctx_k <- kc_res_ki
                   ; m_res_ki <- case ctx_k of
                                  AnyKind -> return Nothing
                                  _ -> Just <$> newExpectedKind ctx_k

                     -- Step 2: split off invisible binders.
                     -- For example:
                     --
                     --   type F :: forall k1 k2. (k1, k2) -> Type
                     --   type family F
                     --
                     -- Does 'forall k1 k2' become a part of 'tyConBinders' or 'tyConResKind'?
                     -- See Note [Arity inference in kcCheckDeclHeader_sig]
                   ; let (invis_binders, r_ki) = split_invis kisig' m_res_ki

                     -- Check that the inline result kind annotation is valid.
                     -- For example:
                     --
                     --   type T :: Type -> Maybe k
                     --   type family T a :: Maybe j where
                     --
                     -- Here we unify   Maybe k ~ Maybe j
                   ; whenIsJust m_res_ki $ \res_ki ->
                      discardResult $ -- See Note [discardResult in kcCheckDeclHeader_sig]
                      unifyKind Nothing r_ki res_ki

                   ; return (invis_binders, r_ki) }

        -- Zonk the implicitly quantified variables.
        ; implicit_tvs <- mapM zonkTcTyVarToTyVar implicit_tvs

        -- Convert each invisible TyCoBinder to TyConBinder for tyConBinders.
        ; invis_tcbs <- mapM invis_to_tcb invis_binders

        -- Build the final, generalized TcTyCon
        ; let tcbs            = vis_tcbs ++ invis_tcbs
              implicit_tv_prs = implicit_nms `zip` implicit_tvs
              all_tv_prs      = implicit_tv_prs ++ explicit_tv_prs
              tc = mkTcTyCon name tcbs r_ki all_tv_prs True flav

        ; traceTc "kcCheckDeclHeader_sig done:" $ vcat
          [ text "tyConName = " <+> ppr (tyConName tc)
          , text "kisig =" <+> debugPprType kisig
          , text "tyConKind =" <+> debugPprType (tyConKind tc)
          , text "tyConBinders = " <+> ppr (tyConBinders tc)
          , text "tcTyConScopedTyVars" <+> ppr (tcTyConScopedTyVars tc)
          , text "tyConResKind" <+> debugPprType (tyConResKind tc)
          ]
        ; return tc }
  where
    -- Consider this declaration:
    --
    --    type T :: forall a. forall b -> (a~b) => Proxy a -> Type
    --    data T x p = MkT
    --
    -- Here, we have every possible variant of ZippedBinder:
    --
    --                   TyBinder           LHsTyVarBndr
    --    ----------------------------------------------
    --    ZippedBinder   forall {k}.
    --    ZippedBinder   forall (a::k).
    --    ZippedBinder   forall (b::k) ->   x
    --    ZippedBinder   (a~b) =>
    --    ZippedBinder   Proxy a ->         p
    --
    -- Given a ZippedBinder zipped_to_tcb produces:
    --
    --  * TyConBinder      for  tyConBinders
    --  * (Name, TcTyVar)  for  tcTyConScopedTyVars, if there's a user-written LHsTyVarBndr
    --
    zipped_to_tcb :: ZippedBinder -> TcM (TyConBinder, [(Name, TcTyVar)])
    zipped_to_tcb zb = case zb of

      -- Inferred variable, no user-written binder.
      -- Example:   forall {k}.
      ZippedBinder (Named (Bndr v Specified)) Nothing ->
        return (mkNamedTyConBinder Specified v, [])

      -- Specified variable, no user-written binder.
      -- Example:   forall (a::k).
      ZippedBinder (Named (Bndr v Inferred)) Nothing ->
        return (mkNamedTyConBinder Inferred v, [])

      -- Constraint, no user-written binder.
      -- Example:   (a~b) =>
      ZippedBinder (Anon InvisArg bndr_ki) Nothing -> do
        name <- newSysName (mkTyVarOccFS (fsLit "ev"))
        let tv = mkTyVar name bndr_ki
        return (mkAnonTyConBinder InvisArg tv, [])

      -- Non-dependent visible argument with a user-written binder.
      -- Example:   Proxy a ->
      ZippedBinder (Anon VisArg bndr_ki) (Just b) ->
        return $
          let v_name = getName b
              tv = mkTyVar v_name bndr_ki
              tcb = mkAnonTyConBinder VisArg tv
          in (tcb, [(v_name, tv)])

      -- Dependent visible argument with a user-written binder.
      -- Example:   forall (b::k) ->
      ZippedBinder (Named (Bndr v Required)) (Just b) ->
        return $
          let v_name = getName b
              tcb = mkNamedTyConBinder Required v
          in (tcb, [(v_name, v)])

      -- 'zipBinders' does not produce any other variants of ZippedBinder.
      _ -> panic "goVis: invalid ZippedBinder"

    -- Given an invisible binder that comes from 'split_invis',
    -- convert it to TyConBinder.
    invis_to_tcb :: TyCoBinder -> TcM TyConBinder
    invis_to_tcb tb = do
      (tcb, stv) <- zipped_to_tcb (ZippedBinder tb Nothing)
      MASSERT(null stv)
      return tcb

    -- Check that the inline kind annotation on a binder is valid
    -- by unifying it with the kind of the quantifier.
    check_zipped_binder :: ZippedBinder -> TcM ()
    check_zipped_binder (ZippedBinder _ Nothing) = return ()
    check_zipped_binder (ZippedBinder tb (Just b)) =
      case unLoc b of
        UserTyVar _ _ _ -> return ()
        KindedTyVar _ _ v v_hs_ki -> do
          v_ki <- tcLHsKindSig (TyVarBndrKindCtxt (unLoc v)) v_hs_ki
          discardResult $ -- See Note [discardResult in kcCheckDeclHeader_sig]
            unifyKind (Just (HsTyVar noExtField NotPromoted v))
                      (tyBinderType tb)
                      v_ki

    -- Split the invisible binders that should become a part of 'tyConBinders'
    -- rather than 'tyConResKind'.
    -- See Note [Arity inference in kcCheckDeclHeader_sig]
    split_invis :: Kind -> Maybe Kind -> ([TyCoBinder], Kind)
    split_invis sig_ki Nothing =
      -- instantiate all invisible binders
      splitPiTysInvisible sig_ki
    split_invis sig_ki (Just res_ki) =
      -- subtraction a la checkExpectedKind
      let n_res_invis_bndrs = invisibleTyBndrCount res_ki
          n_sig_invis_bndrs = invisibleTyBndrCount sig_ki
          n_inst = n_sig_invis_bndrs - n_res_invis_bndrs
      in splitPiTysInvisibleN n_inst sig_ki

-- A quantifier from a kind signature zipped with a user-written binder for it.
data ZippedBinder =
  ZippedBinder TyBinder (Maybe (LHsTyVarBndr () GhcRn))

-- See Note [Arity inference in kcCheckDeclHeader_sig]
zipBinders
  :: Kind                      -- kind signature
  -> [LHsTyVarBndr () GhcRn]   -- user-written binders
  -> ([ZippedBinder],          -- zipped binders
      [LHsTyVarBndr () GhcRn], -- remaining user-written binders
      Kind)                    -- remainder of the kind signature
zipBinders = zip_binders []
  where
    zip_binders acc ki [] = (reverse acc, [], ki)
    zip_binders acc ki (b:bs) =
      case tcSplitPiTy_maybe ki of
        Nothing -> (reverse acc, b:bs, ki)
        Just (tb, ki') ->
          let
            (zb, bs') | zippable  = (ZippedBinder tb (Just b),  bs)
                      | otherwise = (ZippedBinder tb Nothing, b:bs)
            zippable =
              case tb of
                Named (Bndr _ (Invisible _)) -> False
                Named (Bndr _ Required)      -> True
                Anon InvisArg _ -> False
                Anon VisArg   _ -> True
          in
            zip_binders (zb:acc) ki' bs'

tooManyBindersErr :: Kind -> [LHsTyVarBndr () GhcRn] -> SDoc
tooManyBindersErr ki bndrs =
   hang (text "Not a function kind:")
      4 (ppr ki) $$
   hang (text "but extra binders found:")
      4 (fsep (map ppr bndrs))

{- Note [Arity inference in kcCheckDeclHeader_sig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a kind signature 'kisig' and a declaration header, kcCheckDeclHeader_sig
verifies that the declaration conforms to the signature. The end result is a
TcTyCon 'tc' such that:

  tyConKind tc == kisig

This TcTyCon would be rather easy to produce if we didn't have to worry about
arity. Consider these declarations:

  type family S1 :: forall k. k -> Type
  type family S2 (a :: k) :: Type

Both S1 and S2 can be given the same standalone kind signature:

  type S2 :: forall k. k -> Type

And, indeed, tyConKind S1 == tyConKind S2. However, tyConKind is built from
tyConBinders and tyConResKind, such that

  tyConKind tc == mkTyConKind (tyConBinders tc) (tyConResKind tc)

For S1 and S2, tyConBinders and tyConResKind are different:

  tyConBinders S1  ==  []
  tyConResKind S1  ==  forall k. k -> Type
  tyConKind    S1  ==  forall k. k -> Type

  tyConBinders S2  ==  [spec k, anon-vis (a :: k)]
  tyConResKind S2  ==  Type
  tyConKind    S1  ==  forall k. k -> Type

This difference determines the arity:

  tyConArity tc == length (tyConBinders tc)

That is, the arity of S1 is 0, while the arity of S2 is 2.

'kcCheckDeclHeader_sig' needs to infer the desired arity to split the standalone
kind signature into binders and the result kind. It does so in two rounds:

1. zip user-written binders (vis_tcbs)
2. split off invisible binders (invis_tcbs)

Consider the following declarations:

    type F :: Type -> forall j. j -> forall k1 k2. (k1, k2) -> Type
    type family F a b

    type G :: Type -> forall j. j -> forall k1 k2. (k1, k2) -> Type
    type family G a b :: forall r2. (r1, r2) -> Type

In step 1 (zip user-written binders), we zip the quantifiers in the signature
with the binders in the header using 'zipBinders'. In both F and G, this results in
the following zipped binders:

                   TyBinder     LHsTyVarBndr
    ---------------------------------------
    ZippedBinder   Type ->      a
    ZippedBinder   forall j.
    ZippedBinder   j ->         b


At this point, we have accumulated three zipped binders which correspond to a
prefix of the standalone kind signature:

  Type -> forall j. j -> ...

In step 2 (split off invisible binders), we have to decide how much remaining
invisible binders of the standalone kind signature to split off:

    forall k1 k2. (k1, k2) -> Type
    ^^^^^^^^^^^^^
    split off or not?

This decision is made in 'split_invis':

* If a user-written result kind signature is not provided, as in F,
  then split off all invisible binders. This is why we need special treatment
  for AnyKind.
* If a user-written result kind signature is provided, as in G,
  then do as checkExpectedKind does and split off (n_sig - n_res) binders.
  That is, split off such an amount of binders that the remainder of the
  standalone kind signature and the user-written result kind signature have the
  same amount of invisible quantifiers.

For F, split_invis splits away all invisible binders, and we have 2:

    forall k1 k2. (k1, k2) -> Type
    ^^^^^^^^^^^^^
    split away both binders

The resulting arity of F is 3+2=5.  (length vis_tcbs = 3,
                                     length invis_tcbs = 2,
                                     length tcbs = 5)

For G, split_invis decides to split off 1 invisible binder, so that we have the
same amount of invisible quantifiers left:

    res_ki  =  forall    r2. (r1, r2) -> Type
    kisig   =  forall k1 k2. (k1, k2) -> Type
                     ^^^
                     split off this one.

The resulting arity of G is 3+1=4. (length vis_tcbs = 3,
                                    length invis_tcbs = 1,
                                    length tcbs = 4)

-}

{- Note [discardResult in kcCheckDeclHeader_sig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
-}

{- Note [No polymorphic recursion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Should this kind-check?
  data T ka (a::ka) b  = MkT (T Type           Int   Bool)
                             (T (Type -> Type) Maybe Bool)

Notice that T is used at two different kinds in its RHS.  No!
This should not kind-check.  Polymorphic recursion is known to
be a tough nut.

Previously, we laboriously (with help from the renamer)
tried to give T the polymorphic kind
   T :: forall ka -> ka -> kappa -> Type
where kappa is a unification variable, even in the inferInitialKinds
phase (which is what kcInferDeclHeader is all about).  But
that is dangerously fragile (see the ticket).

Solution: make kcInferDeclHeader give T a straightforward
monomorphic kind, with no quantification whatsoever. That's why
we use mkAnonTyConBinder for all arguments when figuring out
tc_binders.

But notice that (#16322 comment:3)

* The algorithm successfully kind-checks this declaration:
    data T2 ka (a::ka) = MkT2 (T2 Type a)

  Starting with (inferInitialKinds)
    T2 :: (kappa1 :: kappa2 :: *) -> (kappa3 :: kappa4 :: *) -> *
  we get
    kappa4 := kappa1   -- from the (a:ka) kind signature
    kappa1 := Type     -- From application T2 Type

  These constraints are soluble so generaliseTcTyCon gives
    T2 :: forall (k::Type) -> k -> *

  But now the /typechecking/ (aka desugaring, tcTyClDecl) phase
  fails, because the call (T2 Type a) in the RHS is ill-kinded.

  We'd really prefer all errors to show up in the kind checking
  phase.

* This algorithm still accepts (in all phases)
     data T3 ka (a::ka) = forall b. MkT3 (T3 Type b)
  although T3 is really polymorphic-recursive too.
  Perhaps we should somehow reject that.

Note [Kind-checking tyvar binders for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When kind-checking the type-variable binders for associated
   data/newtype decls
   family decls
we behave specially for type variables that are already in scope;
that is, bound by the enclosing class decl.  This is done in
kcLHsQTyVarBndrs:
  * The use of tcImplicitQTKBndrs
  * The tcLookupLocal_maybe code in kc_hs_tv

See Note [Associated type tyvar names] in GHC.Core.Class and
    Note [TyVar binders for associated decls] in GHC.Hs.Decls

We must do the same for family instance decls, where the in-scope
variables may be bound by the enclosing class instance decl.
Hence the use of tcImplicitQTKBndrs in tcFamTyPatsAndGen.

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
data ContextKind = TheKind Kind   -- ^ a specific kind
                 | AnyKind        -- ^ any kind will do
                 | OpenKind       -- ^ something of the form @TYPE _@

-----------------------
newExpectedKind :: ContextKind -> TcM Kind
newExpectedKind (TheKind k) = return k
newExpectedKind AnyKind     = newMetaKindVar
newExpectedKind OpenKind    = newOpenTypeKind

-----------------------
expectedKindInCtxt :: UserTypeCtxt -> ContextKind
-- Depending on the context, we might accept any kind (for instance, in a TH
-- splice), or only certain kinds (like in type signatures).
expectedKindInCtxt (TySynCtxt _)   = AnyKind
expectedKindInCtxt ThBrackCtxt     = AnyKind
expectedKindInCtxt (GhciCtxt {})   = AnyKind
-- The types in a 'default' decl can have varying kinds
-- See Note [Extended defaults]" in GHC.Tc.Utils.Env
expectedKindInCtxt DefaultDeclCtxt     = AnyKind
expectedKindInCtxt TypeAppCtxt         = AnyKind
expectedKindInCtxt (ForSigCtxt _)      = TheKind liftedTypeKind
expectedKindInCtxt (InstDeclCtxt {})   = TheKind constraintKind
expectedKindInCtxt SpecInstCtxt        = TheKind constraintKind
expectedKindInCtxt _                   = OpenKind


{- *********************************************************************
*                                                                      *
             Bringing type variables into scope
*                                                                      *
********************************************************************* -}

{- Note [Non-cloning for tyvar binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bindExplictTKBndrs_Q_Skol, bindExplictTKBndrs_Skol, do not clone;
and nor do the Implicit versions.  There is no need.

bindExplictTKBndrs_Q_Tv does not clone; and similarly Implicit.
We take advantage of this in kcInferDeclHeader:
     all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
If we cloned, we'd need to take a bit more care here; not hard.

The main payoff is that avoidng gratuitious cloning means that we can
almost always take the fast path in swizzleTcTyConBndrs.  "Almost
always" means not the case of mutual recursion with polymorphic kinds.


Note [Cloning for tyvar binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bindExplicitTKBndrs_Tv does cloning, making up a Name with a fresh Unique,
unlike bindExplicitTKBndrs_Q_Tv.  (Nor do the Skol variants clone.)
And similarly for bindImplicit...

This for a narrow and tricky reason which, alas, I couldn't find a
simpler way round.  #16221 is the poster child:

   data SameKind :: k -> k -> *
   data T a = forall k2 (b :: k2). MkT (SameKind a b) !Int

When kind-checking T, we give (a :: kappa1). Then:

- In kcConDecl we make a TyVarTv unification variable kappa2 for k2
  (as described in Note [Kind-checking for GADTs], even though this
  example is an existential)
- So we get (b :: kappa2) via bindExplicitTKBndrs_Tv
- We end up unifying kappa1 := kappa2, because of the (SameKind a b)

Now we generalise over kappa2. But if kappa2's Name is precisely k2
(i.e. we did not clone) we'll end up giving T the utterlly final kind
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
-- Implicit binders
--------------------------------------

bindImplicitTKBndrs_Skol, bindImplicitTKBndrs_Tv,
  bindImplicitTKBndrs_Q_Skol, bindImplicitTKBndrs_Q_Tv
  :: [Name] -> TcM a -> TcM ([TcTyVar], a)
bindImplicitTKBndrs_Q_Skol = bindImplicitTKBndrsX (newImplicitTyVarQ newFlexiKindedSkolemTyVar)
bindImplicitTKBndrs_Q_Tv   = bindImplicitTKBndrsX (newImplicitTyVarQ newFlexiKindedTyVarTyVar)
bindImplicitTKBndrs_Skol   = bindImplicitTKBndrsX newFlexiKindedSkolemTyVar
bindImplicitTKBndrs_Tv     = bindImplicitTKBndrsX cloneFlexiKindedTyVarTyVar
  -- newFlexiKinded...           see Note [Non-cloning for tyvar binders]
  -- cloneFlexiKindedTyVarTyVar: see Note [Cloning for tyvar binders]

bindImplicitTKBndrsX
   :: (Name -> TcM TcTyVar) -- new_tv function
   -> [Name]
   -> TcM a
   -> TcM ([TcTyVar], a)   -- Returned [TcTyVar] are in 1-1 correspondence
                           -- with the passed in [Name]
bindImplicitTKBndrsX new_tv tv_names thing_inside
  = do { tkvs <- mapM new_tv tv_names
       ; traceTc "bindImplicitTKBndrs" (ppr tv_names $$ ppr tkvs)
       ; res <- tcExtendNameTyVarEnv (tv_names `zip` tkvs)
                thing_inside
       ; return (tkvs, res) }

newImplicitTyVarQ :: (Name -> TcM TcTyVar) ->  Name -> TcM TcTyVar
-- Behave like new_tv, except that if the tyvar is in scope, use it
newImplicitTyVarQ new_tv name
  = do { mb_tv <- tcLookupLcl_maybe name
       ; case mb_tv of
           Just (ATyVar _ tv) -> return tv
           _ -> new_tv name }

newFlexiKindedTyVar :: (Name -> Kind -> TcM TyVar) -> Name -> TcM TyVar
newFlexiKindedTyVar new_tv name
  = do { kind <- newMetaKindVar
       ; new_tv name kind }

newFlexiKindedSkolemTyVar :: Name -> TcM TyVar
newFlexiKindedSkolemTyVar = newFlexiKindedTyVar newSkolemTyVar

newFlexiKindedTyVarTyVar :: Name -> TcM TyVar
newFlexiKindedTyVarTyVar = newFlexiKindedTyVar newTyVarTyVar

cloneFlexiKindedTyVarTyVar :: Name -> TcM TyVar
cloneFlexiKindedTyVarTyVar = newFlexiKindedTyVar cloneTyVarTyVar
   -- See Note [Cloning for tyvar binders]

--------------------------------------
-- Explicit binders
--------------------------------------

bindExplicitTKBndrs_Skol, bindExplicitTKBndrs_Tv
    :: (OutputableBndrFlag flag)
    => [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)

bindExplicitTKBndrs_Skol = bindExplicitTKBndrsX (tcHsTyVarBndr newSkolemTyVar)
bindExplicitTKBndrs_Tv   = bindExplicitTKBndrsX (tcHsTyVarBndr cloneTyVarTyVar)
  -- newSkolemTyVar:  see Note [Non-cloning for tyvar binders]
  -- cloneTyVarTyVar: see Note [Cloning for tyvar binders]

bindExplicitTKBndrs_Q_Skol, bindExplicitTKBndrs_Q_Tv
    :: ContextKind
    -> [LHsTyVarBndr () GhcRn]
    -> TcM a
    -> TcM ([TcTyVar], a)

bindExplicitTKBndrs_Q_Skol ctxt_kind = bindExplicitTKBndrsX_Q (tcHsQTyVarBndr ctxt_kind newSkolemTyVar)
bindExplicitTKBndrs_Q_Tv   ctxt_kind = bindExplicitTKBndrsX_Q (tcHsQTyVarBndr ctxt_kind newTyVarTyVar)
  -- See Note [Non-cloning for tyvar binders]

bindExplicitTKBndrsX_Q
    :: (HsTyVarBndr () GhcRn -> TcM TcTyVar)
    -> [LHsTyVarBndr () GhcRn]
    -> TcM a
    -> TcM ([TcTyVar], a)  -- Returned [TcTyVar] are in 1-1 correspondence
                           -- with the passed-in [LHsTyVarBndr]
bindExplicitTKBndrsX_Q tc_tv hs_tvs thing_inside
  = do { (tv_bndrs,res) <- bindExplicitTKBndrsX tc_tv hs_tvs thing_inside
       ; return ((binderVars tv_bndrs),res) }

bindExplicitTKBndrsX :: (OutputableBndrFlag flag)
    => (HsTyVarBndr flag GhcRn -> TcM TcTyVar)
    -> [LHsTyVarBndr flag GhcRn]
    -> TcM a
    -> TcM ([VarBndr TyVar flag], a)  -- Returned [TcTyVar] are in 1-1 correspondence
                                      -- with the passed-in [LHsTyVarBndr]
bindExplicitTKBndrsX tc_tv hs_tvs thing_inside
  = do { traceTc "bindExplicTKBndrs" (ppr hs_tvs)
       ; go hs_tvs }
  where
    go [] = do { res <- thing_inside
               ; return ([], res) }
    go (L _ hs_tv : hs_tvs)
       = do { tv <- tc_tv hs_tv
            -- Extend the environment as we go, in case a binder
            -- is mentioned in the kind of a later binder
            --   e.g. forall k (a::k). blah
            -- NB: tv's Name may differ from hs_tv's
            -- See GHC.Tc.Utils.TcMType Note [Cloning for tyvar binders]
            ; (tvs,res) <- tcExtendNameTyVarEnv [(hsTyVarName hs_tv, tv)] $
                           go hs_tvs
            ; return ((Bndr tv (hsTyVarBndrFlag hs_tv)):tvs, res) }

-----------------
tcHsTyVarBndr :: (Name -> Kind -> TcM TyVar)
              -> HsTyVarBndr flag GhcRn -> TcM TcTyVar
tcHsTyVarBndr new_tv (UserTyVar _ _ (L _ tv_nm))
  = do { kind <- newMetaKindVar
       ; new_tv tv_nm kind }
tcHsTyVarBndr new_tv (KindedTyVar _ _ (L _ tv_nm) lhs_kind)
  = do { kind <- tcLHsKindSig (TyVarBndrKindCtxt tv_nm) lhs_kind
       ; new_tv tv_nm kind }

-----------------
tcHsQTyVarBndr :: ContextKind
               -> (Name -> Kind -> TcM TyVar)
               -> HsTyVarBndr () GhcRn -> TcM TcTyVar
-- Just like tcHsTyVarBndr, but also
--   - uses the in-scope TyVar from class, if it exists
--   - takes a ContextKind to use for the no-sig case
tcHsQTyVarBndr ctxt_kind new_tv (UserTyVar _ _ (L _ tv_nm))
  = do { mb_tv <- tcLookupLcl_maybe tv_nm
       ; case mb_tv of
           Just (ATyVar _ tv) -> return tv
           _ -> do { kind <- newExpectedKind ctxt_kind
                   ; new_tv tv_nm kind } }

tcHsQTyVarBndr _ new_tv (KindedTyVar _ _ (L _ tv_nm) lhs_kind)
  = do { kind <- tcLHsKindSig (TyVarBndrKindCtxt tv_nm) lhs_kind
       ; mb_tv <- tcLookupLcl_maybe tv_nm
       ; case mb_tv of
           Just (ATyVar _ tv)
             -> do { discardResult $ unifyKind (Just hs_tv)
                                        kind (tyVarKind tv)
                       -- This unify rejects:
                       --    class C (m :: * -> *) where
                       --      type F (m :: *) = ...
                   ; return tv }

           _ -> new_tv tv_nm kind }
  where
    hs_tv = HsTyVar noExtField NotPromoted (noLoc tv_nm)
            -- Used for error messages only

--------------------------------------
-- Binding type/class variables in the
-- kind-checking and typechecking phases
--------------------------------------

bindTyClTyVars :: Name
               -> (TcTyCon -> [TyConBinder] -> Kind -> TcM a) -> TcM a
-- ^ Used for the type variables of a type or class decl
-- in the "kind checking" and "type checking" pass,
-- but not in the initial-kind run.
bindTyClTyVars tycon_name thing_inside
  = do { tycon <- tcLookupTcTyCon tycon_name
       ; let scoped_prs = tcTyConScopedTyVars tycon
             res_kind   = tyConResKind tycon
             binders    = tyConBinders tycon
       ; traceTc "bindTyClTyVars" (ppr tycon_name <+> ppr binders $$ ppr scoped_prs)
       ; tcExtendNameTyVarEnv scoped_prs $
         thing_inside tycon binders res_kind }


{- *********************************************************************
*                                                                      *
             Kind generalisation
*                                                                      *
********************************************************************* -}

zonkAndScopedSort :: [TcTyVar] -> TcM [TcTyVar]
zonkAndScopedSort spec_tkvs
  = do { spec_tkvs <- mapM zonkAndSkolemise spec_tkvs
          -- Use zonkAndSkolemise because a skol_tv might be a TyVarTv

       -- Do a stable topological sort, following
       -- Note [Ordering of implicit variables] in GHC.Rename.HsType
       ; return (scopedSort spec_tkvs) }

-- | Generalize some of the free variables in the given type.
-- All such variables should be *kind* variables; any type variables
-- should be explicitly quantified (with a `forall`) before now.
-- The supplied predicate says which free variables to quantify.
-- But in all cases,
-- generalize only those variables whose TcLevel is strictly greater
-- than the ambient level. This "strictly greater than" means that
-- you likely need to push the level before creating whatever type
-- gets passed here. Any variable whose level is greater than the
-- ambient level but is not selected to be generalized will be
-- promoted. (See [Promoting unification variables] in GHC.Tc.Solver
-- and Note [Recipe for checking a signature].)
-- The resulting KindVar are the variables to
-- quantify over, in the correct, well-scoped order. They should
-- generally be Inferred, not Specified, but that's really up to
-- the caller of this function.
kindGeneralizeSome :: (TcTyVar -> Bool)
                   -> TcType    -- ^ needn't be zonked
                   -> TcM [KindVar]
kindGeneralizeSome should_gen kind_or_type
  = do { traceTc "kindGeneralizeSome {" (ppr kind_or_type)

         -- use the "Kind" variant here, as any types we see
         -- here will already have all type variables quantified;
         -- thus, every free variable is really a kv, never a tv.
       ; dvs <- candidateQTyVarsOfKind kind_or_type

       -- So 'dvs' are the variables free in kind_or_type, with a level greater
       -- than the ambient level, hence candidates for quantification
       -- Next: filter out the ones we don't want to generalize (specified by should_gen)
       -- and promote them instead

       ; let (to_promote, dvs') = partitionCandidates dvs (not . should_gen)

       ; (_, promoted) <- promoteTyVarSet (dVarSetToVarSet to_promote)
       ; qkvs <- quantifyTyVars dvs'

       ; traceTc "kindGeneralizeSome }" $
         vcat [ text "Kind or type:" <+> ppr kind_or_type
              , text "dvs:" <+> ppr dvs
              , text "dvs':" <+> ppr dvs'
              , text "to_promote:" <+> pprTyVars (dVarSetElems to_promote)
              , text "promoted:" <+> pprTyVars (nonDetEltsUniqSet promoted)
              , text "qkvs:" <+> pprTyVars qkvs ]

       ; return qkvs }

-- | Specialized version of 'kindGeneralizeSome', but where all variables
-- can be generalized. Use this variant when you can be sure that no more
-- constraints on the type's metavariables will arise or be solved.
kindGeneralizeAll :: TcType  -- needn't be zonked
                  -> TcM [KindVar]
kindGeneralizeAll ty = do { traceTc "kindGeneralizeAll" empty
                          ; kindGeneralizeSome (const True) ty }

-- | Specialized version of 'kindGeneralizeSome', but where no variables
-- can be generalized, but perhaps some may neeed to be promoted.
-- Use this variant when it is unknowable whether metavariables might
-- later be constrained.
--
-- To see why this promotion is needed, see
-- Note [Recipe for checking a signature], and especially
-- Note [Promotion in signatures].
kindGeneralizeNone :: TcType  -- needn't be zonked
                   -> TcM ()
kindGeneralizeNone ty
  = do { traceTc "kindGeneralizeNone" empty
       ; kvs <- kindGeneralizeSome (const False) ty
       ; MASSERT( null kvs )
       }

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
  * Emit the residiual constraint wrapped in an implication for g,
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
etaExpandAlgTyCon :: [TyConBinder]
                  -> Kind   -- must be zonked
                  -> TcM ([TyConBinder], Kind)
-- GADT decls can have a (perhaps partial) kind signature
--      e.g.  data T a :: * -> * -> * where ...
-- This function makes up suitable (kinded) TyConBinders for the
-- argument kinds.  E.g. in this case it might return
--   ([b::*, c::*], *)
-- Never emits constraints.
-- It's a little trickier than you might think: see
-- Note [TyConBinders for the result kind signature of a data type]
-- See Note [Datatype return kinds] in GHC.Tc.TyCl
etaExpandAlgTyCon tc_bndrs kind
  = do  { loc     <- getSrcSpanM
        ; uniqs   <- newUniqueSupply
        ; rdr_env <- getLocalRdrEnv
        ; let new_occs = [ occ
                         | str <- allNameStrings
                         , let occ = mkOccName tvName str
                         , isNothing (lookupLocalRdrOcc rdr_env occ)
                         -- Note [Avoid name clashes for associated data types]
                         , not (occ `elem` lhs_occs) ]
              new_uniqs = uniqsFromSupply uniqs
              subst = mkEmptyTCvSubst (mkInScopeSet (mkVarSet lhs_tvs))
        ; return (go loc new_occs new_uniqs subst [] kind) }
  where
    lhs_tvs  = map binderVar tc_bndrs
    lhs_occs = map getOccName lhs_tvs

    go loc occs uniqs subst acc kind
      = case splitPiTy_maybe kind of
          Nothing -> (reverse acc, substTy subst kind)

          Just (Anon af arg, kind')
            -> go loc occs' uniqs' subst' (tcb : acc) kind'
            where
              arg'   = substTy subst arg
              tv     = mkTyVar (mkInternalName uniq occ loc) arg'
              subst' = extendTCvInScope subst tv
              tcb    = Bndr tv (AnonTCB af)
              (uniq:uniqs') = uniqs
              (occ:occs')   = occs

          Just (Named (Bndr tv vis), kind')
            -> go loc occs uniqs subst' (tcb : acc) kind'
            where
              (subst', tv') = substTyVarBndr subst tv
              tcb = Bndr tv' (NamedTCB vis)

-- | A description of whether something is a
--
-- * @data@ or @newtype@ ('DataDeclSort')
--
-- * @data instance@ or @newtype instance@ ('DataInstanceSort')
--
-- * @data family@ ('DataFamilySort')
--
-- At present, this data type is only consumed by 'checkDataKindSig'.
data DataSort
  = DataDeclSort     NewOrData
  | DataInstanceSort NewOrData
  | DataFamilySort

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
-- See also Note [Datatype return kinds] in GHC.Tc.TyCl
checkDataKindSig :: DataSort -> Kind -> TcM ()
checkDataKindSig data_sort kind = do
  dflags <- getDynFlags
  checkTc (is_TYPE_or_Type dflags || is_kind_var) (err_msg dflags)
  where
    pp_dec :: SDoc
    pp_dec = text $
      case data_sort of
        DataDeclSort     DataType -> "Data type"
        DataDeclSort     NewType  -> "Newtype"
        DataInstanceSort DataType -> "Data instance"
        DataInstanceSort NewType  -> "Newtype instance"
        DataFamilySort            -> "Data family"

    is_newtype :: Bool
    is_newtype =
      case data_sort of
        DataDeclSort     new_or_data -> new_or_data == NewType
        DataInstanceSort new_or_data -> new_or_data == NewType
        DataFamilySort               -> False

    is_data_family :: Bool
    is_data_family =
      case data_sort of
        DataDeclSort{}     -> False
        DataInstanceSort{} -> False
        DataFamilySort     -> True

    tYPE_ok :: DynFlags -> Bool
    tYPE_ok dflags =
         (is_newtype && xopt LangExt.UnliftedNewtypes dflags)
           -- With UnliftedNewtypes, we allow kinds other than Type, but they
           -- must still be of the form `TYPE r` since we don't want to accept
           -- Constraint or Nat.
           -- See Note [Implementation of UnliftedNewtypes] in GHC.Tc.TyCl.
      || is_data_family
           -- If this is a `data family` declaration, we don't need to check if
           -- UnliftedNewtypes is enabled, since data family declarations can
           -- have return kind `TYPE r` unconditionally (#16827).

    is_TYPE :: Bool
    is_TYPE = tcIsRuntimeTypeKind kind

    is_TYPE_or_Type :: DynFlags -> Bool
    is_TYPE_or_Type dflags | tYPE_ok dflags = is_TYPE
                           | otherwise      = tcIsLiftedTypeKind kind

    -- In the particular case of a data family, permit a return kind of the
    -- form `:: k` (where `k` is a bare kind variable).
    is_kind_var :: Bool
    is_kind_var | is_data_family = isJust (tcGetCastedTyVar_maybe kind)
                | otherwise      = False

    err_msg :: DynFlags -> SDoc
    err_msg dflags =
      sep [ (sep [ pp_dec <+>
                   text "has non-" <>
                   (if tYPE_ok dflags then text "TYPE" else ppr liftedTypeKind)
                 , (if is_data_family then text "and non-variable" else empty) <+>
                   text "return kind" <+> quotes (ppr kind) ])
          , if not (tYPE_ok dflags) && is_TYPE && is_newtype &&
               not (xopt LangExt.UnliftedNewtypes dflags)
            then text "Perhaps you intended to use UnliftedNewtypes"
            else empty ]

-- | Checks that the result kind of a class is exactly `Constraint`, rejecting
-- type synonyms and type families that reduce to `Constraint`. See #16826.
checkClassKindSig :: Kind -> TcM ()
checkClassKindSig kind = checkTc (tcIsConstraintKind kind) err_msg
  where
    err_msg :: SDoc
    err_msg =
      text "Kind signature on a class must end with" <+> ppr constraintKind $$
      text "unobscured by type families"

tcbVisibilities :: TyCon -> [Type] -> [TyConBndrVis]
-- Result is in 1-1 correspondence with orig_args
tcbVisibilities tc orig_args
  = go (tyConKind tc) init_subst orig_args
  where
    init_subst = mkEmptyTCvSubst (mkInScopeSet (tyCoVarsOfTypes orig_args))
    go _ _ []
      = []

    go fun_kind subst all_args@(arg : args)
      | Just (tcb, inner_kind) <- splitPiTy_maybe fun_kind
      = case tcb of
          Anon af _           -> AnonTCB af   : go inner_kind subst  args
          Named (Bndr tv vis) -> NamedTCB vis : go inner_kind subst' args
                 where
                    subst' = extendTCvSubst subst tv arg

      | not (isEmptyTCvSubst subst)
      = go (substTy subst fun_kind) init_subst all_args

      | otherwise
      = pprPanic "addTcbVisibilities" (ppr tc <+> ppr orig_args)


{- Note [TyConBinders for the result kind signature of a data type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given
  data T (a::*) :: * -> forall k. k -> *
we want to generate the extra TyConBinders for T, so we finally get
  (a::*) (b::*) (k::*) (c::k)
The function etaExpandAlgTyCon generates these extra TyConBinders from
the result kind signature.

We need to take care to give the TyConBinders
  (a) OccNames that are fresh (because the TyConBinders of a TyCon
      must have distinct OccNames

  (b) Uniques that are fresh (obviously)

For (a) we need to avoid clashes with the tyvars declared by
the user before the "::"; in the above example that is 'a'.
And also see Note [Avoid name clashes for associated data types].

For (b) suppose we have
   data T :: forall k. k -> forall k. k -> *
where the two k's are identical even up to their uniques.  Surprisingly,
this can happen: see #14515.

It's reasonably easy to solve all this; just run down the list with a
substitution; hence the recursive 'go' function.  But it has to be
done.

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
  | HsWC { hswc_ext  = sig_wcs, hswc_body = ib_ty } <- sig_ty
  , HsIB { hsib_ext = implicit_hs_tvs
         , hsib_body = hs_ty } <- ib_ty
  , (explicit_hs_tvs, L _ hs_ctxt, hs_tau) <- splitLHsSigmaTyInvis hs_ty
  = addSigCtxt ctxt hs_ty $
    do { (implicit_tvs, (explicit_tvbndrs, (wcs, wcx, theta, tau)))
            <- solveLocalEqualities "tcHsPartialSigType"    $
                 -- This solveLocalEqualiltes fails fast if there are
                 -- insoluble equalities. See GHC.Tc.Solver
                 -- Note [Fail fast if there are insoluble kind equalities]
               tcNamedWildCardBinders sig_wcs $ \ wcs ->
               bindImplicitTKBndrs_Tv implicit_hs_tvs       $
               bindExplicitTKBndrs_Tv explicit_hs_tvs       $
               do {   -- Instantiate the type-class context; but if there
                      -- is an extra-constraints wildcard, just discard it here
                    (theta, wcx) <- tcPartialContext hs_ctxt

                  ; tau <- tcHsOpenType hs_tau

                  ; return (wcs, wcx, theta, tau) }

       ; let implicit_tvbndrs = map (mkTyVarBinder SpecifiedSpec) implicit_tvs

         -- No kind-generalization here:
       ; kindGeneralizeNone (mkInvisForAllTys implicit_tvbndrs $
                             mkInvisForAllTys explicit_tvbndrs $
                             mkPhiTy theta $
                             tau)

       -- Spit out the wildcards (including the extra-constraints one)
       -- as "hole" constraints, so that they'll be reported if necessary
       -- See Note [Extra-constraint holes in partial type signatures]
       ; mapM_ emitNamedTypeHole wcs

       -- Zonk, so that any nested foralls can "see" their occurrences
       -- See Note [Checking partial type signatures], in
       -- the bullet on Nested foralls.
       ; theta        <- mapM zonkTcType theta
       ; tau          <- zonkTcType tau

         -- We return a proper (Name,InvisTVBinder) environment, to be sure that
         -- we bring the right name into scope in the function body.
         -- Test case: partial-sigs/should_compile/LocalDefinitionBug
       ; let tv_prs = (implicit_hs_tvs                  `zip` implicit_tvbndrs)
                      ++ (hsLTyVarNames explicit_hs_tvs `zip` explicit_tvbndrs)

      -- NB: checkValidType on the final inferred type will be
      --     done later by checkInferredPolyId.  We can't do it
      --     here because we don't have a complete type to check

       ; traceTc "tcHsPartialSigType" (ppr tv_prs)
       ; return (wcs, wcx, tv_prs, theta, tau) }

tcPartialContext :: HsContext GhcRn -> TcM (TcThetaType, Maybe TcType)
tcPartialContext hs_theta
  | Just (hs_theta1, hs_ctxt_last) <- snocView hs_theta
  , L wc_loc wc@(HsWildCardTy _) <- ignoreParens hs_ctxt_last
  = do { wc_tv_ty <- setSrcSpan wc_loc $
                     tcAnonWildCardOcc wc constraintKind
       ; theta <- mapM tcLHsPredType hs_theta1
       ; return (theta, Just wc_tv_ty) }
  | otherwise
  = do { theta <- mapM tcLHsPredType hs_theta
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
  Note [Cloning for tyvar binders] in GHC.Tc.Gen.HsType

* In GHC.Tc.Gen.Sig.tcUserSigType we return a PartialSig, which (unlike
  the companion CompleteSig) contains the original, as-yet-unchecked
  source-code LHsSigWcType

* Then, for f and g /separately/, we call tcInstSig, which in turn
  call tchsPartialSig (defined near this Note).  It kind-checks the
  LHsSigWcType, creating fresh unification variables for each "_"
  wildcard.  It's important that the wildcards for f and g are distinct
  because they might get instantiated completely differently.  E.g.
     f,g :: forall a. a -> _
     f x = a
     g x = True
  It's really as if we'd written two distinct signatures.

* Nested foralls. Consider
     f :: forall b. (forall a. a -> _) -> b
  We do /not/ allow the "_" to be instantiated to 'a'; but we do
  (as before) allow it to be instantiated to the (top level) 'b'.
  Why not?  Because suppose
     f x = (x True, x 'c')
  We must instantiate that (forall a. a -> _) when typechecking
  f's body, so we must know precisely where all the a's are; they
  must not be hidden under (filled-in) unification variables!

  We achieve this in the usual way: we push a level at a forall,
  so now the unification variable for the "_" can't unify with
  'a'.

* Just as for ordinary signatures, we must zonk the type after
  kind-checking it, to ensure that all the nested forall binders can
  see their occurrenceds

  Just as for ordinary signatures, this zonk also gets any Refl casts
  out of the way of instantiation.  Example: #18008 had
       foo :: (forall a. (Show a => blah) |> Refl) -> _
  and that Refl cast messed things up.  See #18062.

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

An annoying difficulty happens if there are more than 62 inferred
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

* Because it is ill-kinded, it trips an assert in writeMetaTyVar,
  so now I disable the assertion if we are writing a type of
  kind Constraint.  (That seldom/never normally happens so we aren't
  losing much.)

Result works fine, but it may eventually bite us.


************************************************************************
*                                                                      *
      Pattern signatures (i.e signatures that occur in patterns)
*                                                                      *
********************************************************************* -}

tcHsPatSigType :: UserTypeCtxt
               -> HsPatSigType GhcRn          -- The type signature
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
tcHsPatSigType ctxt
  (HsPS { hsps_ext  = HsPSRn { hsps_nwcs = sig_wcs, hsps_imp_tvs = sig_ns }
        , hsps_body = hs_ty })
  = addSigCtxt ctxt hs_ty $
    do { sig_tkv_prs <- mapM new_implicit_tv sig_ns
       ; (wcs, sig_ty)
            <- solveLocalEqualities "tcHsPatSigType" $
                 -- Always solve local equalities if possible,
                 -- else casts get in the way of deep skolemisation
                 -- (#16033)
               tcNamedWildCardBinders sig_wcs        $ \ wcs ->
               tcExtendNameTyVarEnv sig_tkv_prs $
               do { sig_ty <- tcHsOpenType hs_ty
                  ; return (wcs, sig_ty) }

        ; mapM_ emitNamedTypeHole wcs

          -- sig_ty might have tyvars that are at a higher TcLevel (if hs_ty
          -- contains a forall). Promote these.
          -- Ex: f (x :: forall a. Proxy a -> ()) = ... x ...
          -- When we instantiate x, we have to compare the kind of the argument
          -- to a's kind, which will be a metavariable.
          -- kindGeneralizeNone does this:
        ; kindGeneralizeNone sig_ty
        ; sig_ty <- zonkTcType sig_ty
        ; checkValidType ctxt sig_ty

        ; traceTc "tcHsPatSigType" (ppr sig_tkv_prs)
        ; return (wcs, sig_tkv_prs, sig_ty) }
  where
    new_implicit_tv name
      = do { kind <- newMetaKindVar
           ; tv   <- case ctxt of
                       RuleSigCtxt {} -> newSkolemTyVar name kind
                       _              -> newPatSigTyVar name kind
                       -- See Note [Typechecking pattern signature binders]
             -- NB: tv's Name may be fresh (in the case of newPatSigTyVar)
           ; return (name, tv) }

{- Note [Typechecking pattern signature binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Type variables in the type environment] in GHC.Tc.Utils.
Consider

  data T where
    MkT :: forall a. a -> (a -> Int) -> T

  f :: T -> ...
  f (MkT x (f :: b -> c)) = <blah>

Here
 * The pattern (MkT p1 p2) creates a *skolem* type variable 'a_sk',
   It must be a skolem so that that it retains its identity, and
   GHC.Tc.Errors.getSkolemInfo can thereby find the binding site for the skolem.

 * The type signature pattern (f :: b -> c) makes freshs meta-tyvars
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
together.  Hence the new_tv function in tcHsPatSigType.


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
-- See  Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
-- Result is zonked
  = do { kind <- solveLocalEqualities "tcLHsKindSig" $
                 tc_lhs_kind kindLevelMode hs_kind
       ; traceTc "tcLHsKindSig" (ppr hs_kind $$ ppr kind)
       -- No generalization:
       ; kindGeneralizeNone kind
       ; kind <- zonkTcType kind
         -- This zonk is very important in the case of higher rank kinds
         -- E.g. #13879    f :: forall (p :: forall z (y::z). <blah>).
         --                          <more blah>
         --      When instantiating p's kind at occurrences of p in <more blah>
         --      it's crucial that the kind we instantiate is fully zonked,
         --      else we may fail to substitute properly

       ; checkValidType ctxt kind
       ; traceTc "tcLHsKindSig2" (ppr kind)
       ; return kind }

tc_lhs_kind :: TcTyMode -> LHsKind GhcRn -> TcM Kind
tc_lhs_kind mode k
  = addErrCtxt (text "In the kind" <+> quotes (ppr k)) $
    tc_lhs_type (kindLevel mode) k liftedTypeKind

promotionErr :: Name -> PromotionErr -> TcM a
promotionErr name err
  = failWithTc (hang (pprPECategory err <+> quotes (ppr name) <+> text "cannot be used here")
                   2 (parens reason))
  where
    reason = case err of
               ConstrainedDataConPE pred
                              -> text "it has an unpromotable context"
                                 <+> quotes (ppr pred)
               FamDataConPE   -> text "it comes from a data family instance"
               NoDataKindsTC  -> text "perhaps you intended to use DataKinds"
               NoDataKindsDC  -> text "perhaps you intended to use DataKinds"
               PatSynPE       -> text "pattern synonyms cannot be promoted"
               _ -> text "it is defined and used in the same recursive group"

{-
************************************************************************
*                                                                      *
          Error messages and such
*                                                                      *
************************************************************************
-}


-- | If the inner action emits constraints, report them as errors and fail;
-- otherwise, propagates the return value. Useful as a wrapper around
-- 'tcImplicitTKBndrs', which uses solveLocalEqualities, when there won't be
-- another chance to solve constraints
failIfEmitsConstraints :: TcM a -> TcM a
failIfEmitsConstraints thing_inside
  = checkNoErrs $  -- We say that we fail if there are constraints!
                   -- c.f same checkNoErrs in solveEqualities
    do { (res, lie) <- captureConstraints thing_inside
       ; reportAllUnsolved lie
       ; return res
       }

-- | Make an appropriate message for an error in a function argument.
-- Used for both expressions and types.
funAppCtxt :: (Outputable fun, Outputable arg) => fun -> arg -> Int -> SDoc
funAppCtxt fun arg arg_no
  = hang (hsep [ text "In the", speakNth arg_no, ptext (sLit "argument of"),
                    quotes (ppr fun) <> text ", namely"])
       2 (quotes (ppr arg))

-- | Add a "In the data declaration for T" or some such.
addTyConFlavCtxt :: Name -> TyConFlavour -> TcM a -> TcM a
addTyConFlavCtxt name flav
  = addErrCtxt $ hsep [ text "In the", ppr flav
                      , text "declaration for", quotes (ppr name) ]
