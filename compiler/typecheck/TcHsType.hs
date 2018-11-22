{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcMonoType]{Typechecking user-specified @MonoTypes@}
-}

{-# LANGUAGE CPP, TupleSections, MultiWayIf, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module TcHsType (
        -- Type signatures
        kcHsSigType, tcClassSigType,
        tcHsSigType, tcHsSigWcType,
        tcHsPartialSigType,
        funsSigCtxt, addSigCtxt, pprSigCtxt,

        tcHsClsInstType,
        tcHsDeriv, tcDerivStrategy,
        tcHsTypeApp,
        UserTypeCtxt(..),
        tcImplicitTKBndrs, tcImplicitQTKBndrs,
        tcExplicitTKBndrs,
        kcExplicitTKBndrs, kcImplicitTKBndrs,

                -- Type checking type and class decls
        kcLookupTcTyCon, kcTyClTyVars, tcTyClTyVars,
        tcDataKindSig,

          -- tyvars
        scopeTyVars, scopeTyVars2,

        -- Kind-checking types
        -- No kind generalisation, no checkValidType
        kcLHsQTyVars,
        tcWildCardBinders,
        tcHsLiftedType,   tcHsOpenType,
        tcHsLiftedTypeNC, tcHsOpenTypeNC,
        tcLHsType, tcLHsTypeUnsaturated, tcCheckLHsType,
        tcHsMbContext, tcHsContext, tcLHsPredType, tcInferApps,
        failIfEmitsConstraints,
        solveEqualities, -- useful re-export

        typeLevelMode, kindLevelMode,

        kindGeneralize, checkExpectedKindX, instantiateTyUntilN,
        reportFloatingKvs,

        -- Sort-checking kinds
        tcLHsKindSig, badKindSig,

        -- Zonking and promoting
        zonkPromoteType,

        -- Pattern type signatures
        tcHsPatSigType, tcPatSig,

        -- Error messages
        funAppCtxt, addTyConFlavCtxt
   ) where

#include "HsVersions.h"

import GhcPrelude

import HsSyn
import TcRnMonad
import TcEvidence
import TcEnv
import TcMType
import TcValidity
import TcUnify
import TcIface
import TcSimplify
import TcHsSyn
import TcErrors ( reportAllUnsolved )
import TcType
import Inst   ( tcInstTyBinders, tcInstTyBinder )
import TyCoRep( TyCoBinder(..), TyBinder )  -- Used in tcDataKindSig
import Type
import Coercion
import RdrName( lookupLocalRdrOcc )
import Var
import VarSet
import TyCon
import ConLike
import DataCon
import Class
import Name
import NameSet
import VarEnv
import TysWiredIn
import BasicTypes
import SrcLoc
import Constants ( mAX_CTUPLE_SIZE )
import ErrUtils( MsgDoc )
import Unique
import Util
import UniqSupply
import Outputable
import FastString
import PrelNames hiding ( wildCardName )
import qualified GHC.LanguageExtensions as LangExt

import Maybes
import Data.List ( find, mapAccumR )
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
with TcValidity.checkValidType.

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

kcHsSigType :: SkolemInfo -> [Located Name] -> LHsSigType GhcRn -> TcM ()
kcHsSigType skol_info names (HsIB { hsib_body = hs_ty
                                  , hsib_ext = sig_vars })
  = addSigCtxt (funsSigCtxt names) hs_ty $
    discardResult $
    tcImplicitTKBndrs skol_info sig_vars $
    tc_lhs_type typeLevelMode hs_ty liftedTypeKind
kcHsSigType  _ _ (XHsImplicitBndrs _) = panic "kcHsSigType"

tcClassSigType :: SkolemInfo -> [Located Name] -> LHsSigType GhcRn -> TcM Type
-- Does not do validity checking
tcClassSigType skol_info names sig_ty
  = addSigCtxt (funsSigCtxt names) (hsSigType sig_ty) $
    tc_hs_sig_type_and_gen skol_info sig_ty liftedTypeKind

tcHsSigType :: UserTypeCtxt -> LHsSigType GhcRn -> TcM Type
-- Does validity checking
-- See Note [Recipe for checking a signature]
tcHsSigType ctxt sig_ty
  = addSigCtxt ctxt (hsSigType sig_ty) $
    do { traceTc "tcHsSigType {" (ppr sig_ty)
       ; kind <- case expectedKindInCtxt ctxt of
                    AnythingKind -> newMetaKindVar
                    TheKind k    -> return k
                    OpenKind     -> newOpenTypeKind
              -- The kind is checked by checkValidType, and isn't necessarily
              -- of kind * in a Template Haskell quote eg [t| Maybe |]

          -- Generalise here: see Note [Kind generalisation]
       ; ty <- tc_hs_sig_type_and_gen skol_info sig_ty kind
       ; ty <- zonkTcType ty

       ; checkValidType ctxt ty
       ; traceTc "end tcHsSigType }" (ppr ty)
       ; return ty }
  where
    skol_info = SigTypeSkol ctxt

tc_hs_sig_type_and_gen :: SkolemInfo -> LHsSigType GhcRn -> Kind -> TcM Type
-- Kind-checks/desugars an 'LHsSigType',
--   solve equalities,
--   and then kind-generalizes.
-- This will never emit constraints, as it uses solveEqualities interally.
-- No validity checking or zonking
tc_hs_sig_type_and_gen skol_info (HsIB { hsib_ext = sig_vars
                                       , hsib_body = hs_ty }) kind
  = do { ((tkvs, ty), wanted) <- captureConstraints $
                                 tcImplicitTKBndrs skol_info sig_vars $
                                 tc_lhs_type typeLevelMode hs_ty kind
         -- Any remaining variables (unsolved in the solveLocalEqualities
         -- in the tcImplicitTKBndrs) should be in the global tyvars,
         -- and therefore won't be quantified over

       ; let ty1 = mkSpecForAllTys tkvs ty
       ; kvs <- kindGeneralizeLocal wanted ty1
       ; emitConstraints wanted -- we still need to solve these
       ; return (mkInvForAllTys kvs ty1) }

tc_hs_sig_type_and_gen _ (XHsImplicitBndrs _) _ = panic "tc_hs_sig_type_and_gen"

-----------------
tcHsDeriv :: LHsSigType GhcRn -> TcM ([TyVar], (Class, [Type], [Kind]))
-- Like tcHsSigType, but for the ...deriving( C t1 ty2 ) clause
-- Returns the C, [ty1, ty2, and the kinds of C's remaining arguments
-- E.g.    class C (a::*) (b::k->k)
--         data T a b = ... deriving( C Int )
--    returns ([k], C, [k, Int], [k->k])
-- Return values are fully zonked
tcHsDeriv hs_ty
  = do { cls_kind <- newMetaKindVar
                    -- always safe to kind-generalize, because there
                    -- can be no covars in an outer scope
       ; ty <- checkNoErrs $
                 -- avoid redundant error report with "illegal deriving", below
               tc_hs_sig_type_and_gen (SigTypeSkol DerivClauseCtxt) hs_ty cls_kind
       ; cls_kind <- zonkTcTypeToType cls_kind
       ; ty <- zonkTcTypeToType ty
       ; let (tvs, pred) = splitForAllTys ty
       ; let (args, _) = splitFunTys cls_kind
       ; case getClassPredTys_maybe pred of
           Just (cls, tys) -> return (tvs, (cls, tys, args))
           Nothing -> failWithTc (text "Illegal deriving item" <+> quotes (ppr hs_ty)) }

-- | Typecheck something within the context of a deriving strategy.
-- This is of particular importance when the deriving strategy is @via@.
-- For instance:
--
-- @
-- deriving via (S a) instance C (T a)
-- @
--
-- We need to typecheck @S a@, and moreover, we need to extend the tyvar
-- environment with @a@ before typechecking @C (T a)@, since @S a@ quantified
-- the type variable @a@.
tcDerivStrategy
  :: forall a.
     UserTypeCtxt
  -> Maybe (DerivStrategy GhcRn) -- ^ The deriving strategy
  -> TcM ([TyVar], a) -- ^ The thing to typecheck within the context of the
                      -- deriving strategy, which might quantify some type
                      -- variables of its own.
  -> TcM (Maybe (DerivStrategy GhcTc), [TyVar], a)
     -- ^ The typechecked deriving strategy, all quantified tyvars, and
     -- the payload of the typechecked thing.
tcDerivStrategy user_ctxt mds thing_inside
  = case mds of
      Nothing -> boring_case Nothing
      Just ds -> do (ds', tvs, thing) <- tc_deriv_strategy ds
                    pure (Just ds', tvs, thing)
  where
    tc_deriv_strategy :: DerivStrategy GhcRn
                      -> TcM (DerivStrategy GhcTc, [TyVar], a)
    tc_deriv_strategy StockStrategy    = boring_case StockStrategy
    tc_deriv_strategy AnyclassStrategy = boring_case AnyclassStrategy
    tc_deriv_strategy NewtypeStrategy  = boring_case NewtypeStrategy
    tc_deriv_strategy (ViaStrategy ty) = do
      cls_kind <- newMetaKindVar
      ty' <- checkNoErrs $
             tc_hs_sig_type_and_gen (SigTypeSkol user_ctxt) ty cls_kind
      ty' <- zonkTcTypeToType ty'
      let (via_tvs, via_pred) = splitForAllTys ty'
      tcExtendTyVarEnv via_tvs $ do
        (thing_tvs, thing) <- thing_inside
        pure (ViaStrategy via_pred, via_tvs ++ thing_tvs, thing)

    boring_case :: mds -> TcM (mds, [TyVar], a)
    boring_case mds = do
      (thing_tvs, thing) <- thing_inside
      pure (mds, thing_tvs, thing)

tcHsClsInstType :: UserTypeCtxt    -- InstDeclCtxt or SpecInstCtxt
                -> LHsSigType GhcRn
                -> TcM ([TyVar], ThetaType, Class, [Type])
-- Like tcHsSigType, but for a class instance declaration
tcHsClsInstType user_ctxt hs_inst_ty
  = setSrcSpan (getLoc (hsSigType hs_inst_ty)) $
    {- We want to fail here if the tc_hs_sig_type_and_gen emits constraints.
       First off, we know we'll never solve the constraints, as classes are
       always at top level, and their constraints do not inform the kind checking
       of method types. So failing isn't wrong. Yet, the reason we do it is
       to avoid the validity checker from seeing unsolved coercion holes in
       types. Much better just to report the kind error directly. -}
    do { inst_ty <- failIfEmitsConstraints $
                    tc_hs_sig_type_and_gen (SigTypeSkol user_ctxt) hs_inst_ty constraintKind
       ; inst_ty <- zonkTcTypeToType inst_ty
       ; checkValidInstance user_ctxt hs_inst_ty inst_ty }

----------------------------------------------
-- | Type-check a visible type application
tcHsTypeApp :: LHsWcType GhcRn -> Kind -> TcM Type
-- See Note [Recipe for checking a signature] in TcHsType
tcHsTypeApp wc_ty kind
  | HsWC { hswc_ext = sig_wcs, hswc_body = hs_ty } <- wc_ty
  = do { ty <- solveLocalEqualities $
               -- We are looking at a user-written type, very like a
               -- signature so we want to solve its equalities right now
               tcWildCardBinders sig_wcs $ \ _ ->
               tcCheckLHsType hs_ty kind
       -- We must promote here. Ex:
       --   f :: forall a. a
       --   g = f @(forall b. Proxy b -> ()) @Int ...
       -- After when processing the @Int, we'll have to check its kind
       -- against the as-yet-unknown kind of b. This check causes an assertion
       -- failure if we don't promote.
       ; ty <- zonkPromoteType ty
       ; checkValidType TypeAppCtxt ty
       ; return ty }
        -- NB: we don't call emitWildcardHoleConstraints here, because
        -- we want any holes in visible type applications to be used
        -- without fuss. No errors, warnings, extensions, etc.
tcHsTypeApp (XHsWildCardBndrs _) _ = panic "tcHsTypeApp"

{-
************************************************************************
*                                                                      *
            The main kind checker: no validity checks here
*                                                                      *
************************************************************************

        First a couple of simple wrappers for kcHsType
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
tcCheckLHsType :: LHsType GhcRn -> Kind -> TcM TcType
tcCheckLHsType hs_ty exp_kind
  = addTypeCtxt hs_ty $
    tc_lhs_type typeLevelMode hs_ty exp_kind

tcLHsType :: LHsType GhcRn -> TcM (TcType, TcKind)
-- Called from outside: set the context
tcLHsType ty = addTypeCtxt ty (tc_infer_lhs_type typeLevelMode ty)

-- Like tcLHsType, but use it in a context where type synonyms and type families
-- do not need to be saturated, like in a GHCi :kind call
tcLHsTypeUnsaturated :: LHsType GhcRn -> TcM (TcType, TcKind)
tcLHsTypeUnsaturated ty = addTypeCtxt ty (tc_infer_lhs_type mode ty)
  where
    mode = allowUnsaturated typeLevelMode

{-
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
data TcTyMode
  = TcTyMode { mode_level :: TypeOrKind
             , mode_unsat :: Bool        -- True <=> allow unsaturated type families
             }
 -- The mode_unsat field is solely so that type families/synonyms can be unsaturated
 -- in GHCi :kind calls

typeLevelMode :: TcTyMode
typeLevelMode = TcTyMode { mode_level = TypeLevel, mode_unsat = False }

kindLevelMode :: TcTyMode
kindLevelMode = TcTyMode { mode_level = KindLevel, mode_unsat = False }

allowUnsaturated :: TcTyMode -> TcTyMode
allowUnsaturated mode = mode { mode_unsat = True }

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
again. This is done in calls to tcInstTyBinders.

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

Note [The tcType invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
(IT1) If    tc_ty = tc_hs_type hs_ty exp_kind
      then  typeKind tc_ty = exp_kind
without any zonking needed.  The reason for this is that in
tcInferApps we see (F ty), and we kind-check 'ty' with an
expected-kind coming from F.  Then, to make the resulting application
well kinded --- see Note [The well-kinded type invariant] in TcType ---
we need the kind-checked 'ty' to have exactly the kind that F expects,
with no funny zonking nonsense in between.

The tcType invariant also applies to checkExpectedKind:

(IT2) if
        (tc_ty, _, _) = checkExpectedKind ty act_ki exp_ki
      then
        typeKind tc_ty = exp_ki

These other invariants are all necessary, too, as these functions
are used within tc_hs_type:

(IT3) If (ty, ki) <- tc_infer_hs_type ..., then typeKind ty == ki.

(IT4) If (ty, ki) <- tc_infer_hs_type ..., then zonk ki == ki.
      (In other words, the result kind of tc_infer_hs_type is zonked.)

(IT5) If (ty, ki) <- tcTyVar ..., then typeKind ty == ki.

(IT6) If (ty, ki) <- tcTyVar ..., then zonk ki == ki.
      (In other words, the result kind of tcTyVar is zonked.)

-}

------------------------------------------
-- | Check and desugar a type, returning the core type and its
-- possibly-polymorphic kind. Much like 'tcInferRho' at the expression
-- level.
tc_infer_lhs_type :: TcTyMode -> LHsType GhcRn -> TcM (TcType, TcKind)
tc_infer_lhs_type mode (L span ty)
  = setSrcSpan span $
    do { (ty', kind) <- tc_infer_hs_type mode ty
       ; return (ty', kind) }

-- | Infer the kind of a type and desugar. This is the "up" type-checker,
-- as described in Note [Bidirectional type checking]
tc_infer_hs_type :: TcTyMode -> HsType GhcRn -> TcM (TcType, TcKind)
tc_infer_hs_type mode (HsParTy _ t)          = tc_infer_lhs_type mode t
tc_infer_hs_type mode (HsTyVar _ _ (L _ tv)) = tcTyVar mode tv

tc_infer_hs_type mode (HsAppTy _ ty1 ty2)
  = do { let (hs_fun_ty, hs_arg_tys) = splitHsAppTys ty1 [ty2]
       ; (fun_ty, fun_kind) <- tc_infer_lhs_type mode hs_fun_ty
           -- NB: (IT4) of Note [The tcType invariant] ensures that fun_kind is zonked
       ; tcTyApps mode hs_fun_ty fun_ty fun_kind hs_arg_tys }

tc_infer_hs_type mode (HsOpTy _ lhs lhs_op@(L _ hs_op) rhs)
  | not (hs_op `hasKey` funTyConKey)
  = do { (op, op_kind) <- tcTyVar mode hs_op
       ; tcTyApps mode (noLoc $ HsTyVar noExt NotPromoted lhs_op) op op_kind
                       [lhs, rhs] }

tc_infer_hs_type mode (HsKindSig _ ty sig)
  = do { sig' <- tcLHsKindSig KindSigCtxt sig
                 -- We must typecheck the kind signature, and solve all
                 -- its equalities etc; from this point on we may do
                 -- things like instantiate its foralls, so it needs
                 -- to be fully determined (Trac #14904)
       ; traceTc "tc_infer_hs_type:sig" (ppr ty $$ ppr sig')
       ; ty' <- tc_lhs_type mode ty sig'
       ; return (ty', sig') }

-- HsSpliced is an annotation produced by 'RnSplice.rnSpliceType' to communicate
-- the splice location to the typechecker. Here we skip over it in order to have
-- the same kind inferred for a given expression whether it was produced from
-- splices or not.
--
-- See Note [Delaying modFinalizers in untyped splices].
tc_infer_hs_type mode (HsSpliceTy _ (HsSpliced _ _ (HsSplicedTy ty)))
  = tc_infer_hs_type mode ty

tc_infer_hs_type mode (HsDocTy _ ty _) = tc_infer_lhs_type mode ty
tc_infer_hs_type _    (XHsType (NHsCoreTy ty))
  = do { ty <- zonkTcType ty  -- (IT3) and (IT4) of Note [The tcType invariant]
       ; return (ty, typeKind ty) }
tc_infer_hs_type mode other_ty
  = do { kv <- newMetaKindVar
       ; ty' <- tc_hs_type mode other_ty kv
       ; return (ty', kv) }

------------------------------------------
tc_lhs_type :: TcTyMode -> LHsType GhcRn -> TcKind -> TcM TcType
tc_lhs_type mode (L span ty) exp_kind
  = setSrcSpan span $
    tc_hs_type mode ty exp_kind

------------------------------------------
tc_fun_type :: TcTyMode -> LHsType GhcRn -> LHsType GhcRn -> TcKind
            -> TcM TcType
tc_fun_type mode ty1 ty2 exp_kind = case mode_level mode of
  TypeLevel ->
    do { arg_k <- newOpenTypeKind
       ; res_k <- newOpenTypeKind
       ; ty1' <- tc_lhs_type mode ty1 arg_k
       ; ty2' <- tc_lhs_type mode ty2 res_k
       ; checkExpectedKind (HsFunTy noExt ty1 ty2) (mkFunTy ty1' ty2')
                           liftedTypeKind exp_kind }
  KindLevel ->  -- no representation polymorphism in kinds. yet.
    do { ty1' <- tc_lhs_type mode ty1 liftedTypeKind
       ; ty2' <- tc_lhs_type mode ty2 liftedTypeKind
       ; checkExpectedKind (HsFunTy noExt ty1 ty2) (mkFunTy ty1' ty2')
                           liftedTypeKind exp_kind }

------------------------------------------
tc_hs_type :: TcTyMode -> HsType GhcRn -> TcKind -> TcM TcType
-- See Note [The tcType invariant]
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

-- HsSpliced is an annotation produced by 'RnSplice.rnSpliceType'.
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
tc_hs_type mode forall@(HsForAllTy { hst_bndrs = hs_tvs, hst_body = ty }) exp_kind
  = do { (tvs', ty') <- tcExplicitTKBndrs (ForAllSkol (ppr forall)) hs_tvs $
                        tc_lhs_type mode ty exp_kind
    -- Do not kind-generalise here!  See Note [Kind generalisation]
    -- Why exp_kind?  See Note [Body kind of HsForAllTy]
       ; let bndrs      = mkTyVarBinders Specified tvs'
       ; return (mkForAllTys bndrs ty') }

tc_hs_type mode (HsQualTy { hst_ctxt = ctxt, hst_body = ty }) exp_kind
  | null (unLoc ctxt)
  = tc_lhs_type mode ty exp_kind

  | otherwise
  = do { ctxt' <- tc_hs_context mode ctxt

         -- See Note [Body kind of a HsQualTy]
       ; ty' <- if tcIsConstraintKind exp_kind
                then tc_lhs_type mode ty constraintKind
                else do { ek <- newOpenTypeKind
                                -- The body kind (result of the function)
                                -- can be TYPE r, for any r, hence newOpenTypeKind
                        ; ty' <- tc_lhs_type mode ty ek
                        ; checkExpectedKind (unLoc ty) ty' liftedTypeKind exp_kind }

       ; return (mkPhiTy ctxt' ty') }

--------- Lists, arrays, and tuples
tc_hs_type mode rn_ty@(HsListTy _ elt_ty) exp_kind
  = do { tau_ty <- tc_lhs_type mode elt_ty liftedTypeKind
       ; checkWiredInTyCon listTyCon
       ; checkExpectedKind rn_ty (mkListTy tau_ty) liftedTypeKind exp_kind }

-- See Note [Distinguishing tuple kinds] in HsTypes
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
           -- confusing if we give them a shared kind.  Eg Trac #7410
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
       ; let arg_reps = map getRuntimeRepFromKind arg_kinds
             arg_tys  = arg_reps ++ tau_tys
       ; checkExpectedKind rn_ty
                           (mkTyConApp (sumTyCon arity) arg_tys)
                           (unboxedSumKind arg_reps)
                           exp_kind
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
tc_hs_type mode ty@(HsTyVar {})   ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsAppTy {})   ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsOpTy {})    ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsKindSig {}) ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(XHsType (NHsCoreTy{})) ek = tc_infer_hs_type_ek mode ty ek

tc_hs_type _ (HsWildCardTy wc) exp_kind
  = do { wc_ty <- tcWildCardOcc wc exp_kind
       ; return (mkNakedCastTy wc_ty (mkTcNomReflCo exp_kind))
         -- Take care here! Even though the coercion is Refl,
         -- we still need it to establish Note [The tcType invariant]
       }

tcWildCardOcc :: HsWildCardInfo -> Kind -> TcM TcType
tcWildCardOcc wc_info exp_kind
  = do { wc_tv <- tcLookupTyVar (wildCardName wc_info)
          -- The wildcard's kind should be an un-filled-in meta tyvar
       ; checkExpectedKind (HsWildCardTy wc_info) (mkTyVarTy wc_tv)
                           (tyVarKind wc_tv) exp_kind }

---------------------------
-- | Call 'tc_infer_hs_type' and check its result against an expected kind.
tc_infer_hs_type_ek :: HasDebugCallStack => TcTyMode -> HsType GhcRn -> TcKind -> TcM TcType
tc_infer_hs_type_ek mode hs_ty ek
  = do { (ty, k) <- tc_infer_hs_type mode hs_ty
       ; checkExpectedKind hs_ty ty k ek }

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
           BoxedTuple      -> return (nOfThem arity liftedTypeKind)
           UnboxedTuple    -> mapM (\_ -> newOpenTypeKind) tys
           ConstraintTuple -> return (nOfThem arity constraintKind)
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
finish_tuple rn_ty tup_sort tau_tys tau_kinds exp_kind
  = do { traceTc "finish_tuple" (ppr res_kind $$ ppr tau_kinds $$ ppr exp_kind)
       ; let arg_tys  = case tup_sort of
                   -- See also Note [Unboxed tuple RuntimeRep vars] in TyCon
                 UnboxedTuple    -> tau_reps ++ tau_tys
                 BoxedTuple      -> tau_tys
                 ConstraintTuple -> tau_tys
       ; tycon <- case tup_sort of
           ConstraintTuple
             | arity > mAX_CTUPLE_SIZE
                         -> failWith (bigConstraintTuple arity)
             | otherwise -> tcLookupTyCon (cTupleTyConName arity)
           BoxedTuple    -> do { let tc = tupleTyCon Boxed arity
                               ; checkWiredInTyCon tc
                               ; return tc }
           UnboxedTuple  -> return (tupleTyCon Unboxed arity)
       ; checkExpectedKind rn_ty (mkTyConApp tycon arg_tys) res_kind exp_kind }
  where
    arity = length tau_tys
    tau_reps = map getRuntimeRepFromKind tau_kinds
    res_kind = case tup_sort of
                 UnboxedTuple    -> unboxedTupleKind tau_reps
                 BoxedTuple      -> liftedTypeKind
                 ConstraintTuple -> constraintKind

bigConstraintTuple :: Arity -> MsgDoc
bigConstraintTuple arity
  = hang (text "Constraint tuple arity too large:" <+> int arity
          <+> parens (text "max arity =" <+> int mAX_CTUPLE_SIZE))
       2 (text "Instead, use a nested tuple")

---------------------------
-- | Apply a type of a given kind to a list of arguments. This instantiates
-- invisible parameters as necessary. Always consumes all the arguments,
-- using matchExpectedFunKind as necessary.
-- This takes an optional @VarEnv Kind@ which maps kind variables to kinds.
-- These kinds should be used to instantiate invisible kind variables;
-- they come from an enclosing class for an associated type/data family.
tcInferApps :: TcTyMode
            -> Maybe (VarEnv Kind)  -- ^ Possibly, kind info (see above)
            -> LHsType GhcRn        -- ^ Function (for printing only)
            -> TcType               -- ^ Function
            -> TcKind               -- ^ Function kind (zonked)
            -> [LHsType GhcRn]      -- ^ Args
            -> TcM (TcType, [TcType], TcKind) -- ^ (f args, args, result kind)
-- Precondition: typeKind fun_ty = fun_ki
--    Reason: we will return a type application like (fun_ty arg1 ... argn),
--            and that type must be well-kinded
--            See Note [The tcType invariant]
-- Postcondition: Result kind is zonked.
tcInferApps mode mb_kind_info orig_hs_ty fun_ty fun_ki orig_hs_args
  = do { traceTc "tcInferApps {" (ppr orig_hs_ty $$ ppr orig_hs_args $$ ppr fun_ki)
       ; (f_args, args, res_k) <- go 1 [] empty_subst fun_ty orig_ki_binders orig_inner_ki orig_hs_args
       ; traceTc "tcInferApps }" empty
       ; res_k <- zonkTcType res_k  -- nec'y to uphold (IT4) of Note [The tcType invariant]
       ; return (f_args, args, res_k) }
  where
    empty_subst                      = mkEmptyTCvSubst $ mkInScopeSet $
                                       tyCoVarsOfType fun_ki
    (orig_ki_binders, orig_inner_ki) = tcSplitPiTys fun_ki

    go :: Int             -- the # of the next argument
       -> [TcType]        -- already type-checked args, in reverse order
       -> TCvSubst        -- instantiating substitution
       -> TcType          -- function applied to some args
       -> [TyBinder]      -- binders in function kind (both vis. and invis.)
       -> TcKind          -- function kind body (not a Pi-type)
       -> [LHsType GhcRn] -- un-type-checked args
       -> TcM (TcType, [TcType], TcKind)  -- same as overall return type

      -- no user-written args left. We're done!
    go _ acc_args subst fun ki_binders inner_ki []
      = return ( fun
               , reverse acc_args
               , nakedSubstTy subst $ mkPiTys ki_binders inner_ki)
                 -- nakedSubstTy: see Note [The well-kinded type invariant]

      -- The function's kind has a binder. Is it visible or invisible?
    go n acc_args subst fun (ki_binder:ki_binders) inner_ki
       all_args@(arg:args)
      | isInvisibleBinder ki_binder
        -- It's invisible. Instantiate.
      = do { traceTc "tcInferApps (invis)" (ppr ki_binder $$ ppr subst)
           ; (subst', arg') <- tcInstTyBinder mb_kind_info subst ki_binder
           ; go n (arg' : acc_args) subst' (mkNakedAppTy fun arg')
                ki_binders inner_ki all_args }

      | otherwise
        -- It's visible. Check the next user-written argument
      = do { traceTc "tcInferApps (vis)" (vcat [ ppr ki_binder, ppr arg
                                               , ppr (tyBinderType ki_binder)
                                               , ppr subst ])
           ; let exp_kind = nakedSubstTy subst $ tyBinderType ki_binder
                            -- nakedSubstTy: see Note [The well-kinded type invariant]
           ; arg' <- addErrCtxt (funAppCtxt orig_hs_ty arg n) $
                     tc_lhs_type mode arg exp_kind
           ; traceTc "tcInferApps (vis 1)" (ppr exp_kind)
           ; let subst' = extendTvSubstBinderAndInScope subst ki_binder arg'
           ; go (n+1) (arg' : acc_args) subst'
                (mkNakedAppTy fun arg') -- See Note [The well-kinded type invariant]
                ki_binders inner_ki args }

       -- We've run out of known binders in the functions's kind.
    go n acc_args subst fun [] inner_ki all_args
      | not (null new_ki_binders)
         -- But, after substituting, we have more binders.
      = go n acc_args zapped_subst fun new_ki_binders new_inner_ki all_args

      | otherwise
         -- Even after substituting, still no binders. Use matchExpectedFunKind
      = do { traceTc "tcInferApps (no binder)" (ppr new_inner_ki $$ ppr zapped_subst)
           ; (co, arg_k, res_k) <- matchExpectedFunKind hs_ty substed_inner_ki
           ; let new_in_scope = tyCoVarsOfTypes [arg_k, res_k]
                 subst'       = zapped_subst `extendTCvInScopeSet` new_in_scope
           ; go n acc_args subst'
                (fun `mkNakedCastTy` co)  -- See Note [The well-kinded type invariant]
                [mkAnonBinder arg_k]
                res_k all_args }
      where
        substed_inner_ki               = substTy subst inner_ki
        (new_ki_binders, new_inner_ki) = tcSplitPiTys substed_inner_ki
        zapped_subst                   = zapTCvSubst subst
        hs_ty = mkHsAppTys orig_hs_ty (take (n-1) orig_hs_args)

-- | Applies a type to a list of arguments.
-- Always consumes all the arguments, using 'matchExpectedFunKind' as
-- necessary. If you wish to apply a type to a list of HsTypes, this is
-- your function.
-- Used for type-checking types only.
tcTyApps :: TcTyMode
         -> LHsType GhcRn        -- ^ Function (for printing only)
         -> TcType               -- ^ Function
         -> TcKind               -- ^ Function kind (zonked)
         -> [LHsType GhcRn]      -- ^ Args
         -> TcM (TcType, TcKind) -- ^ (f args, result kind)   result kind is zonked
-- Precondition: see precondition for tcInferApps
tcTyApps mode orig_hs_ty fun_ty fun_ki args
  = do { (ty', _args, ki') <- tcInferApps mode Nothing orig_hs_ty fun_ty fun_ki args
       ; return (ty' `mkNakedCastTy` mkNomReflCo ki', ki') }
          -- The mkNakedCastTy is for (IT3) of Note [The tcType invariant]

--------------------------
-- Like checkExpectedKindX, but returns only the final type; convenient wrapper
-- Obeys Note [The tcType invariant]
checkExpectedKind :: HasDebugCallStack
                  => HsType GhcRn   -- type we're checking (for printing)
                  -> TcType         -- type we're checking
                  -> TcKind         -- the known kind of that type
                  -> TcKind         -- the expected kind
                  -> TcM TcType
checkExpectedKind hs_ty ty act exp
  = fstOf3 <$> checkExpectedKindX Nothing (ppr hs_ty) ty act exp

checkExpectedKindX :: HasDebugCallStack
                   => Maybe (VarEnv Kind)  -- Possibly, instantiations for kind vars
                   -> SDoc                 -- HsType whose kind we're checking
                   -> TcType               -- the type whose kind we're checking
                   -> TcKind               -- the known kind of that type, k
                   -> TcKind               -- the expected kind, exp_kind
                   -> TcM (TcType, [TcType], TcCoercionN)
    -- (the new args, the coercion)
-- Instantiate a kind (if necessary) and then call unifyType
--      (checkExpectedKind ty act_kind exp_kind)
-- checks that the actual kind act_kind is compatible
--      with the expected kind exp_kind
checkExpectedKindX mb_kind_env pp_hs_ty ty act_kind exp_kind
 = do { -- We need to make sure that both kinds have the same number of implicit
        -- foralls out front. If the actual kind has more, instantiate accordingly.
        -- Otherwise, just pass the type & kind through: the errors are caught
        -- in unifyType.
        let (exp_bndrs, _) = splitPiTysInvisible exp_kind
            n_exp          = length exp_bndrs
      ; (new_args, act_kind') <- instantiateTyUntilN mb_kind_env n_exp act_kind

      ; let origin = TypeEqOrigin { uo_actual   = act_kind'
                                  , uo_expected = exp_kind
                                  , uo_thing    = Just pp_hs_ty
                                  , uo_visible  = True } -- the hs_ty is visible
            ty' = mkNakedAppTys ty new_args

      ; traceTc "checkExpectedKind" $
        vcat [ pp_hs_ty
             , text "act_kind:" <+> ppr act_kind
             , text "act_kind':" <+> ppr act_kind'
             , text "exp_kind:" <+> ppr exp_kind ]

      ; if act_kind' `tcEqType` exp_kind
        then return (ty', new_args, mkTcNomReflCo exp_kind)  -- This is very common
        else do { co_k <- uType KindLevel origin act_kind' exp_kind
                ; traceTc "checkExpectedKind" (vcat [ ppr act_kind
                                                    , ppr exp_kind
                                                    , ppr co_k ])
                ; let result_ty = ty' `mkNakedCastTy` co_k
                      -- See Note [The tcType invariant]
                ; return (result_ty, new_args, co_k) } }

-- | Instantiate @n@ invisible arguments to a type. If @n <= 0@, no instantiation
-- occurs. If @n@ is too big, then all available invisible arguments are instantiated.
-- (In other words, this function is very forgiving about bad values of @n@.)
-- Why zonk the result? So that tcTyVar can obey (IT6) of Note [The tcType invariant]
instantiateTyN :: Maybe (VarEnv Kind)              -- ^ Predetermined instantiations
                                                   -- (for assoc. type patterns)
               -> Int                              -- ^ @n@
               -> [TyBinder] -> TcKind             -- ^ its kind (zonked)
               -> TcM ([TcType], TcKind)   -- ^ The inst'ed type, new args, kind (zonked)
instantiateTyN mb_kind_env n bndrs inner_ki
  | n <= 0
  = return ([], ki)

  | otherwise
  = do { (subst, inst_args) <- tcInstTyBinders empty_subst mb_kind_env inst_bndrs
       ; let rebuilt_ki = mkPiTys leftover_bndrs inner_ki
       ; ki' <- zonkTcType (substTy subst rebuilt_ki)
       ; traceTc "instantiateTyN" (vcat [ ppr ki
                                        , ppr n
                                        , ppr subst
                                        , ppr rebuilt_ki
                                        , ppr ki' ])
       ; return (inst_args, ki') }
  where
     -- NB: splitAt is forgiving with invalid numbers
     (inst_bndrs, leftover_bndrs) = splitAt n bndrs
     ki          = mkPiTys bndrs inner_ki
     empty_subst = mkEmptyTCvSubst (mkInScopeSet (tyCoVarsOfType ki))

-- | Instantiate a type to have at most @n@ invisible arguments.
instantiateTyUntilN :: Maybe (VarEnv Kind)   -- ^ Possibly, instantiations for vars
                    -> Int         -- ^ @n@
                    -> TcKind      -- ^ its kind
                    -> TcM ([TcType], TcKind)   -- ^ The new args, final kind
instantiateTyUntilN mb_kind_env n ki
  = let (bndrs, inner_ki) = splitPiTysInvisible ki
        num_to_inst       = length bndrs - n
    in
    instantiateTyN mb_kind_env num_to_inst bndrs inner_ki

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
-- in TcTyClsDecls
tcTyVar mode name         -- Could be a tyvar, a tycon, or a datacon
  = do { traceTc "lk1" (ppr name)
       ; thing <- tcLookup name
       ; case thing of
           ATyVar _ tv -> -- Important: zonk before returning
                          -- We may have the application ((a::kappa) b)
                          -- where kappa is already unified to (k1 -> k2)
                          -- Then we want to see that arrow.  Best done
                          -- here because we are also maintaining
                          -- Note [The tcType invariant], so we don't just
                          -- want to zonk the kind, leaving the TyVar
                          -- un-zonked  (Trac #14873)
                          do { ty <- zonkTcTyVar tv
                             ; return (ty, typeKind ty) }

           ATcTyCon tc_tc -> do { -- See Note [GADT kind self-reference]
                                  unless
                                    (isTypeLevel (mode_level mode))
                                    (promotionErr name TyConPE)
                                ; check_tc tc_tc
                                ; handle_tyfams tc_tc }

           AGlobal (ATyCon tc)
             -> do { check_tc tc
                   ; handle_tyfams tc }

           AGlobal (AConLike (RealDataCon dc))
             -> do { data_kinds <- xoptM LangExt.DataKinds
                   ; unless (data_kinds || specialPromotedDc dc) $
                       promotionErr name NoDataKindsDC
                   ; when (isFamInstTyCon (dataConTyCon dc)) $
                       -- see Trac #15245
                       promotionErr name FamDataConPE
                   ; let (_, _, _, theta, _, _) = dataConFullSig dc
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

    -- if we are type-checking a type family tycon, we must instantiate
    -- any invisible arguments right away. Otherwise, we get #11246
    handle_tyfams :: TyCon     -- the tycon to instantiate
                  -> TcM (TcType, TcKind)
    handle_tyfams tc
      | mightBeUnsaturatedTyCon tc || mode_unsat mode
                                         -- This is where mode_unsat is used
      = do { tc_kind <- zonkTcType (tyConKind tc)   -- (IT6) of Note [The tcType invariant]
           ; traceTc "tcTyVar2a" (ppr tc $$ ppr tc_kind)
           ; return (mkTyConApp tc [] `mkNakedCastTy` mkNomReflCo tc_kind, tc_kind) }
              -- the mkNakedCastTy ensures (IT5) of Note [The tcType invariant]

      | otherwise
      = do { tc_kind <- zonkTcType (tyConKind tc)
           ; let (tc_kind_bndrs, tc_inner_ki) = splitPiTysInvisible tc_kind
           ; (tc_args, kind) <- instantiateTyN Nothing (length (tyConBinders tc))
                                               tc_kind_bndrs tc_inner_ki
           ; let is_saturated = tc_args `lengthAtLeast` tyConArity tc
                 tc_ty
                   | is_saturated = mkTyConApp tc tc_args `mkNakedCastTy` mkNomReflCo kind
                      -- mkNakedCastTy is for (IT5) of Note [The tcType invariant]
                   | otherwise    = mkTyConApp tc tc_args
                      -- if the tycon isn't yet saturated, then we don't want mkNakedCastTy,
                      -- because that means we'll have an unsaturated type family
                      -- We don't need it anyway, because we can be sure that the
                      -- type family kind will accept further arguments (because it is
                      -- not yet saturated)
           ; traceTc "tcTyVar2b" (vcat [ ppr tc <+> dcolon <+> ppr tc_kind
                                       , ppr kind ])
           ; return (tc_ty, kind) }

    -- We cannot promote a data constructor with a context that contains
    -- constraints other than equalities, so error if we find one.
    -- See Note [Constraints handled in types] in Inst.
    dc_theta_illegal_constraint :: ThetaType -> Maybe PredType
    dc_theta_illegal_constraint = find go
      where
        go :: PredType -> Bool
        go pred | Just tc <- tyConAppTyCon_maybe pred
                = not $  tc `hasKey` eqTyConKey
                      || tc `hasKey` heqTyConKey
                | otherwise = True

{-
Note [GADT kind self-reference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A promoted type cannot be used in the body of that type's declaration.
Trac #11554 shows this example, which made GHC loop:

  import Data.Kind
  data P (x :: k) = Q
  data A :: Type where
    B :: forall (a :: A). P a -> A

In order to check the constructor B, we need to have the promoted type A, but in
order to get that promoted type, B must first be checked. To prevent looping, a
TyConPE promotion error is given when tcTyVar checks an ATcTyCon in kind mode.
Any ATcTyCon is a TyCon being defined in the current recursive group (see data
type decl for TcTyThing), and all such TyCons are illegal in kinds.

Trac #11962 proposes checking the head of a data declaration separately from
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
[a] doens't have kind '*', and one saying that we need a Constraint to
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
the Check the arguments again to give good error messages
in eg. `(Maybe, Maybe)`

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
        as in TcHsSyn.
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

Note [Dependent LHsQTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We track (in the renamer) which explicitly bound variables in a
LHsQTyVars are manifestly dependent; only precisely these variables
may be used within the LHsQTyVars. We must do this so that kcLHsQTyVars
can produce the right TyConBinders, and tell Anon vs. Required.

Example   data T k1 (a:k1) (b:k2) c
               = MkT (Proxy a) (Proxy b) (Proxy c)

Here
  (a:k1),(b:k2),(c:k3)
       are Anon     (explicitly specified as a binder, not used
                     in the kind of any other binder
  k1   is Required  (explicitly specifed as a binder, but used
                     in the kind of another binder i.e. dependently)
  k2   is Specified (not explicitly bound, but used in the kind
                     of another binder)
  k3   in Inferred  (not lexically in scope at all, but inferred
                     by kind inference)
and
  T :: forall {k3} k1. forall k3 -> k1 -> k2 -> k3 -> *

See Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility]
in TyCoRep.

kcLHsQTyVars uses the hsq_dependent field to decide whether
k1, a, b, c should be Required or Anon.

Earlier, thought it would work simply to do a free-variable check
during kcLHsQTyVars, but this is bogus, because there may be
unsolved equalities about. And we don't want to eagerly solve the
equalities, because we may get further information after
kcLHsQTyVars is called.  (Recall that kcLHsQTyVars is called
only from getInitialKind.)
This is what implements the rule that all variables intended to be
dependent must be manifestly so.

Sidenote: It's quite possible that later, we'll consider (t -> s)
as a degenerate case of some (pi (x :: t) -> s) and then this will
all get more permissive.

Note [Kind generalisation and TyVarTvs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T (a :: k1) x = MkT (S a ())
  data S (b :: k2) y = MkS (T b ())

While we are doing kind inference for the mutually-recursive S,T,
we will end up unifying k1 and k2 together. So they can't be skolems.
We therefore make them TyVarTvs, which can unify with type variables,
but not with general types.  All this is very similar at the level
of terms: see Note [Quantified variables in partial type signatures]
in TcBinds.

There are some wrinkles

* We always want to kind-generalise over TyVarTvs, and /not/ default
  them to Type.  Another way to say this is: a TyVarTv should /never/
  stand for a type, even via defaulting. Hence the check in
  TcSimplify.defaultTyVarTcS, and TcMType.defaultTyVar.  Here's
  another example (Trac #14555):
     data Exp :: [TYPE rep] -> TYPE rep -> Type where
        Lam :: Exp (a:xs) b -> Exp xs (a -> b)
  We want to kind-generalise over the 'rep' variable.
  Trac #14563 is another example.

* Consider Trac #11203
    data SameKind :: k -> k -> *
    data Q (a :: k1) (b :: k2) c = MkQ (SameKind a b)
  Here we will unify k1 with k2, but this time doing so is an error,
  because k1 and k2 are bound in the same declaration.

  We sort this out using findDupTyVarTvs, in TcHsType.tcTyClTyVars; very much
  as we do with partial type signatures in mk_psig_qtvs in
  TcBinds.chooseInferredQuantifiers

* Even the Required arguments should be made into TyVarTvs, not skolems.
  Consider

    data T k (a :: k)

  Here, k is a Required, dependent variable. For uniformity, it is helpful
  to have k be a TyVarTv, in parallel with other dependent variables.
  (This is key in the call to quantifyTyVars in kcTyClGroup, where quantifyTyVars
  expects not to see unknown skolems.)

Note [Keeping scoped variables in order: Explicit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the user writes `forall a b c. blah`, we bring a, b, and c into
scope and then check blah. In the process of checking blah, we might
learn the kinds of a, b, and c, and these kinds might indicate that
b depends on c, and thus that we should reject the user-written type.

One approach to doing this would be to bring each of a, b, and c into
scope, one at a time, creating an implication constraint and
bumping the TcLevel for each one. This would work, because the kind
of, say, b would be untouchable when c is in scope (and the constraint
couldn't float out because c blocks it). However, it leads to terrible
error messages, complaining about skolem escape. While it is indeed
a problem of skolem escape, we can do better.

Instead, our approach is to bring the block of variables into scope
all at once, creating one implication constraint for the lot. The
user-written variables are skolems in the implication constraint. In
TcSimplify.setImplicationStatus, we check to make sure that the ordering
is correct, choosing ImplicationStatus IC_BadTelescope if they aren't.
Then, in TcErrors, we report if there is a bad telescope. This way,
we can report a suggested ordering to the user if there is a problem.

Note [Keeping scoped variables in order: Implicit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
 3. Zonk.
 4. Promote tyvars and/or kind-generalize.
 5. Zonk.
 6. Check validity.

There may be some surprises in here:

Step 2 is necessary for two reasons: most signatures also bring
implicitly quantified variables into scope, and solving is necessary
to get these in the right order (see Note [Keeping scoped variables in
order: Implicit]). Additionally, solving is necessary in order to
kind-generalize correctly.

In Step 4, we have to deal with the fact that metatyvars generated
in the type may have a bumped TcLevel, because explicit foralls
raise the TcLevel. To avoid these variables from ever being visible
in the surrounding context, we must obey the following dictum:

  Every metavariable in a type must either be
    (A) promoted
    (B) generalized, or
    (C) zapped to Any

If a variable is generalized, then it becomes a skolem and no longer
has a proper TcLevel. (I'm ignoring the TcLevel on a skolem here, as
it's not really in play here.) On the other hand, if it is not
generalized (because we're not generalizing the construct -- e.g., pattern
sig -- or because the metavars are constrained -- see kindGeneralizeLocal)
we need to promote to maintain (MetaTvInv) of Note [TcLevel and untouchable type variables]
in TcType.

For more about (C), see Note [Naughty quantification candidates] in TcMType.

After promoting/generalizing, we need to zonk *again* because both
promoting and generalizing fill in metavariables.

To avoid the double-zonk, we do two things:
 1. When we're not generalizing:
    zonkPromoteType and friends zonk and promote at the same time.
    Accordingly, the function does steps 3-5 all at once, preventing
    the need for multiple traversals.

 2. When we are generalizing:
    kindGeneralize does not require a zonked type -- it zonks as it
    gathers free variables. So this way effectively sidesteps step 3.

-}

tcWildCardBinders :: [Name]
                  -> ([(Name, TcTyVar)] -> TcM a)
                  -> TcM a
tcWildCardBinders wc_names thing_inside
  = do { wcs <- mapM newWildTyVar wc_names
       ; let wc_prs = wc_names `zip` wcs
       ; tcExtendNameTyVarEnv wc_prs $
         thing_inside wc_prs }

-- | Kind-check a 'LHsQTyVars'. If the decl under consideration has a complete,
-- user-supplied kind signature (CUSK), generalise the result.
-- Used in 'getInitialKind' (for tycon kinds and other kinds)
-- and in kind-checking (but not for tycon kinds, which are checked with
-- tcTyClDecls). See Note [CUSKs: complete user-supplied kind signatures]
-- in HsDecls.
--
-- This function does not do telescope checking.
kcLHsQTyVars :: Name              -- ^ of the thing being checked
             -> TyConFlavour      -- ^ What sort of 'TyCon' is being checked
             -> Bool              -- ^ True <=> the decl being checked has a CUSK
             -> LHsQTyVars GhcRn
             -> TcM Kind          -- ^ The result kind
             -> TcM TcTyCon       -- ^ A suitably-kinded TcTyCon
kcLHsQTyVars name flav cusk
  user_tyvars@(HsQTvs { hsq_ext = HsQTvsRn { hsq_implicit = kv_ns
                                           , hsq_dependent = dep_names }
                      , hsq_explicit = hs_tvs }) thing_inside
  | cusk
    -- See note [Required, Specified, and Inferred for types] in TcTyClsDecls
  = addTyConFlavCtxt name flav $
    do { (scoped_kvs, (tc_tvs, res_kind))
           <- solveEqualities                    $
              tcImplicitQTKBndrs skol_info kv_ns $
              kcLHsQTyVarBndrs cusk open_fam skol_info hs_tvs thing_inside

       ; let class_tc_binders
               | Just class_tc <- tyConFlavourAssoc_maybe flav
               = tyConBinders class_tc  -- class has a CUSK, so these are zonked
                                       -- and fully settled
               | otherwise
               = []

             class_tv_set = mkVarSet (binderVars class_tc_binders)
             local_specified = filterOut (`elemVarSet` class_tv_set) scoped_kvs
               -- NB: local_specified are guaranteed to be in a well-scoped
               -- order because of tcImplicitQTKBndrs

         -- NB: candidateQTyVarsOfType is OK with unzonked input
       ; candidates <- candidateQTyVarsOfType class_tv_set $
                       mkSpecForAllTys local_specified $
                       mkSpecForAllTys tc_tvs $
                       res_kind
               -- The type above is a bit wrong, in that we're using foralls for all
               -- the tc_tvs, even those that aren't dependent. This is OK, though,
               -- because we're building the type only to extract the variables to
               -- quantify. We use mk_tc_binder below to get this right.

       ; local_inferred <- quantifyTyVars class_tv_set candidates

       ; local_specified <- mapM zonkTyCoVarKind local_specified
       ; tc_tvs          <- mapM zonkTyCoVarKind tc_tvs
       ; res_kind        <- zonkTcType res_kind

       ; let dep_tv_set = tyCoVarsOfTypes (res_kind : map tyVarKind tc_tvs)
             local_tcbs = concat [ mkNamedTyConBinders Inferred local_inferred
                                 , mkNamedTyConBinders Specified local_specified
                                 , map (mkRequiredTyConBinder dep_tv_set) tc_tvs ]

             free_class_tv_set = tyCoVarsOfTypes (res_kind : map binderType local_tcbs)
                                 `delVarSetList` map binderVar local_tcbs

             used_class_tcbs = filter ((`elemVarSet` free_class_tv_set) . binderVar)
                                      class_tc_binders

              -- Suppose we have class C k where type F (x :: k). We can't have
              -- k *required* in F, so it becomes Specified
             to_invis_tcb tcb
               | Required <- tyConBinderArgFlag tcb
               = mkNamedTyConBinder Specified (binderVar tcb)
               | otherwise
               = tcb

             used_class_tcbs_invis = map to_invis_tcb used_class_tcbs

             all_tcbs = used_class_tcbs_invis ++ local_tcbs

         -- If the ordering from
         -- Note [Required, Specified, and Inferred for types] in TcTyClsDecls
         -- doesn't work, we catch it here, before an error cascade
       ; checkValidTelescope all_tcbs (ppr user_tyvars)

          -- If any of the all_kvs aren't actually mentioned in a binder's
          -- kind (or the return kind), then we're in the CUSK case from
          -- Note [Free-floating kind vars]
       ; let all_kvs = concat [ map binderVar used_class_tcbs_invis
                              , local_inferred
                              , local_specified ]

             all_mentioned_tvs = dep_tv_set `unionVarSet`
                                 tyCoVarsOfTypes (map tyVarKind all_kvs)

             unmentioned_kvs   = filterOut (`elemVarSet` all_mentioned_tvs) all_kvs
       ; reportFloatingKvs name flav (map binderVar all_tcbs) unmentioned_kvs

       ; let all_tv_prs = mkTyVarNamePairs (scoped_kvs ++ tc_tvs)
             tycon = mkTcTyCon name (ppr user_tyvars) all_tcbs res_kind
                               all_tv_prs True {- it is generalised -} flav

       ; traceTc "kcLHsQTyVars: cusk" $
         vcat [ text "name" <+> ppr name
              , text "kv_ns" <+> ppr kv_ns
              , text "hs_tvs" <+> ppr hs_tvs
              , text "dep_names" <+> ppr dep_names
              , text "scoped_kvs" <+> ppr scoped_kvs
              , text "tc_tvs" <+> ppr tc_tvs
              , text "res_kind" <+> ppr res_kind
              , text "all_tcbs" <+> ppr all_tcbs
              , text "mkTyConKind all_tcbs res_kind"
                <+> ppr (mkTyConKind all_tcbs res_kind)
              , text "all_tv_prs" <+> ppr all_tv_prs ]

       ; return tycon }

  | otherwise
  = do { (scoped_kvs, (tc_tvs, res_kind))
           -- Why kcImplicitTKBndrs which uses newTyVarTyVar?
           -- See Note [Kind generalisation and TyVarTvs]
           <- kcImplicitTKBndrs kv_ns $
              kcLHsQTyVarBndrs cusk open_fam skol_info hs_tvs thing_inside

       ; let   -- NB: Don't add scoped_kvs to tyConTyVars, because they
               -- might unify with kind vars in other types in a mutually
               -- recursive group. See Note [Kind generalisation and TyVarTvs]
             tc_binders = zipWith mk_tc_binder hs_tvs tc_tvs
               -- Also, note that tc_binders has the tyvars from only the
               -- user-written tyvarbinders. See S1 in Note [How TcTyCons work]
               -- in TcTyClsDecls
             tycon = mkTcTyCon name (ppr user_tyvars) tc_binders res_kind
                               (mkTyVarNamePairs (scoped_kvs ++ tc_tvs))
                               False -- not yet generalised
                               flav

       ; traceTc "kcLHsQTyVars: not-cusk" $
         vcat [ ppr name, ppr kv_ns, ppr hs_tvs, ppr dep_names
              , ppr tc_tvs, ppr (mkTyConKind tc_binders res_kind) ]
       ; return tycon }
  where
    open_fam = tcFlavourIsOpen flav
    skol_info = TyConSkol flav name

    mk_tc_binder :: LHsTyVarBndr GhcRn -> TyVar -> TyConBinder
    -- See Note [Dependent LHsQTyVars]
    mk_tc_binder hs_tv tv
       | hsLTyVarName hs_tv `elemNameSet` dep_names
       = mkNamedTyConBinder Required tv
       | otherwise
       = mkAnonTyConBinder tv

kcLHsQTyVars _ _ _ (XLHsQTyVars _) _ = panic "kcLHsQTyVars"

kcLHsQTyVarBndrs :: Bool   -- True <=> bump the TcLevel when bringing vars into scope
                 -> Bool   -- True <=> Default un-annotated tyvar
                           --          binders to kind *
                 -> SkolemInfo
                 -> [LHsTyVarBndr GhcRn]
                 -> TcM r
                 -> TcM ([TyVar], r)
-- There may be dependency between the explicit "ty" vars.
-- So, we have to handle them one at a time.
kcLHsQTyVarBndrs _ _ _ [] thing
  = do { stuff <- thing; return ([], stuff) }

kcLHsQTyVarBndrs cusk open_fam skol_info (L _ hs_tv : hs_tvs) thing
  = do { tv_pair@(tv, _) <- kc_hs_tv hs_tv
               -- NB: Bring all tvs into scope, even non-dependent ones,
               -- as they're needed in type synonyms, data constructors, etc.

       ; (tvs, stuff) <- bind_unless_scoped tv_pair $
                         kcLHsQTyVarBndrs cusk open_fam skol_info hs_tvs $
                         thing

       ; return ( tv : tvs, stuff ) }
  where
    -- | Bind the tyvar in the env't unless the bool is True
    bind_unless_scoped :: (TcTyVar, Bool) -> TcM a -> TcM a
    bind_unless_scoped (_, True)   thing_inside = thing_inside
    bind_unless_scoped (tv, False) thing_inside
      | cusk      = scopeTyVars skol_info [tv] thing_inside
      | otherwise = tcExtendTyVarEnv      [tv] thing_inside
         -- These variables haven't settled down yet, so we don't want to bump
         -- the TcLevel. If we do, then we'll have metavars of too high a level
         -- floating about. Changing this causes many, many failures in the
         -- `dependent` testsuite directory.

    kc_hs_tv :: HsTyVarBndr GhcRn -> TcM (TcTyVar, Bool)
      -- Special handling for the case where the binder is already in scope
      -- See Note [Associated type tyvar names] in Class and
      --     Note [TyVar binders for associated decls] in HsDecls
    kc_hs_tv (UserTyVar _ (L _ name))
      = do { mb_tv <- tcLookupLcl_maybe name
           ; case mb_tv of  -- See Note [TyVar binders for associated decls]
                Just (ATyVar _ tv) -> return (tv, True)
                _ -> do { kind <- if open_fam
                                  then return liftedTypeKind
                                  else newMetaKindVar
                                  -- Open type/data families default their variables
                                  -- variables to kind *.  But don't default in-scope
                                  -- class tyvars, of course
                        ; tv <- new_tv name kind
                        ; return (tv, False) } }

    kc_hs_tv (KindedTyVar _ lname@(L _ name) lhs_kind)
      = do { kind <- tcLHsKindSig (TyVarBndrKindCtxt name) lhs_kind
           ; mb_tv <- tcLookupLcl_maybe name
           ; case mb_tv of
               Just (ATyVar _ tv)
                 -> do { discardResult $
                           unifyKind (Just (HsTyVar noExt NotPromoted lname))
                                     kind (tyVarKind tv)
                       ; return (tv, True) }
               _ -> do { tv <- new_tv name kind
                       ; return (tv, False) } }

    kc_hs_tv (XTyVarBndr{}) = panic "kc_hs_tv"


    new_tv :: Name -> Kind -> TcM TcTyVar
    new_tv
      | cusk      = newSkolemTyVar
      | otherwise = newTyVarTyVar
          -- Third wrinkle in Note [Kind generalisation and TyVarTvs]

{- Note [Kind-checking tyvar binders for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When kind-checking the type-variable binders for associated
   data/newtype decls
   family decls
we behave specially for type variables that are already in scope;
that is, bound by the enclosing class decl.  This is done in
kcLHsQTyVarBndrs:
  * The use of tcImplicitQTKBndrs
  * The tcLookupLocal_maybe code in kc_hs_tv

See Note [Associated type tyvar names] in Class and
    Note [TyVar binders for associated decls] in HsDecls

We must do the same for family instance decls, where the in-scope
variables may be bound by the enclosing class instance decl.
Hence the use of tcImplicitQTKBndrs in tcFamTyPats.

Note [Kind variable ordering for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should be the kind of `T` in the following example? (#15591)

  class C (a :: Type) where
    type T (x :: f a)

As per Note [Ordering of implicit variables] in RnTypes, we want to quantify
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

In order to make this distinction, we need to know (in kcLHsQTyVars) which
type variables have been bound by the parent class (if there is one). With
the class-bound variables in hand, we can ensure that we always quantify
these first.
-}


--------------------------------------
-- Implicit binders
--------------------------------------

-- | Bring implicitly quantified type/kind variables into scope during
-- kind checking. Uses TyVarTvs, as per Note [Use TyVarTvs in kind-checking pass]
-- in TcTyClsDecls.
kcImplicitTKBndrs :: [Name]     -- of the vars
                  -> TcM a
                  -> TcM ([TcTyVar], a)  -- returns the tyvars created
                                         -- these are *not* dependency ordered
kcImplicitTKBndrs var_ns thing_inside
  -- NB: Just use tyvars that are in scope, if any. Otherwise, we
  -- get #15711, where GHC forgets that a variable used in an associated
  -- type is the same as the one used in the enclosing class
  = do { tkvs_pairs <- mapM (newFlexiKindedQTyVar newTyVarTyVar) var_ns
       ; let tkvs_to_scope = [ tkv | (tkv, True) <- tkvs_pairs ]
       ; result <- tcExtendTyVarEnv tkvs_to_scope thing_inside
       ; return (map fst tkvs_pairs, result) }

tcImplicitTKBndrs, tcImplicitTKBndrsSig, tcImplicitQTKBndrs
  :: SkolemInfo
  -> [Name]
  -> TcM a
  -> TcM ([TcTyVar], a)
tcImplicitTKBndrs    = tcImplicitTKBndrsX newFlexiKindedSkolemTyVar
tcImplicitTKBndrsSig = tcImplicitTKBndrsX newFlexiKindedTyVarTyVar
tcImplicitQTKBndrs   = tcImplicitTKBndrsX
                         (\nm -> fst <$> newFlexiKindedQTyVar newSkolemTyVar nm)

tcImplicitTKBndrsX :: (Name -> TcM TcTyVar) -- new_tv function
                   -> SkolemInfo
                   -> [Name]
                   -> TcM a
                   -> TcM ([TcTyVar], a)   -- these tyvars are dependency-ordered
-- * Guarantees to call solveLocalEqualities to unify
--   all constraints from thing_inside.
--
-- * Returned TcTyVars have the supplied HsTyVarBndrs,
--   but may be in different order to the original [Name]
--   (because of sorting to respect dependency)
--
-- * Returned TcTyVars have zonked kinds
--   See Note [Keeping scoped variables in order: Implicit]
tcImplicitTKBndrsX new_tv skol_info tv_names thing_inside
  | null tv_names -- Short cut for the common case where there
                  -- are no implicit type variables to bind
  = do { result <- solveLocalEqualities thing_inside
       ; return ([], result) }

  | otherwise
  = do { (skol_tvs, result)
           <- solveLocalEqualities $
              checkTvConstraints skol_info Nothing $
              do { tkvs <- mapM new_tv tv_names
                 ; result <- tcExtendTyVarEnv tkvs thing_inside
                 ; return (tkvs, result) }

       ; skol_tvs <- mapM zonkTcTyCoVarBndr skol_tvs
          -- use zonkTcTyCoVarBndr because a skol_tv might be a TyVarTv

          -- do a stable topological sort, following
          -- Note [Ordering of implicit variables] in RnTypes
       ; let final_tvs = scopedSort skol_tvs
       ; traceTc "tcImplicitTKBndrs" (ppr tv_names $$ ppr final_tvs)
       ; return (final_tvs, result) }

newFlexiKindedQTyVar :: (Name -> Kind -> TcM TyVar) -> Name -> TcM (TcTyVar, Bool)
-- Make a new tyvar for an implicit binder in a type/class/type
-- instance declaration, with a flexi-kind
-- But check for in-scope-ness, and if so return that instead
-- Returns True as second return value iff this created a real new tyvar
newFlexiKindedQTyVar mk_tv name
  = do { mb_tv <- tcLookupLcl_maybe name
       ; case mb_tv of
           Just (ATyVar _ tv) -> return (tv, False)
           _ -> (, True) <$> newFlexiKindedTyVar mk_tv name }

newFlexiKindedTyVar :: (Name -> Kind -> TcM TyVar) -> Name -> TcM TyVar
newFlexiKindedTyVar new_tv name
  = do { kind <- newMetaKindVar
       ; new_tv name kind }

newFlexiKindedSkolemTyVar :: Name -> TcM TyVar
newFlexiKindedSkolemTyVar = newFlexiKindedTyVar newSkolemTyVar

newFlexiKindedTyVarTyVar :: Name -> TcM TyVar
newFlexiKindedTyVarTyVar = newFlexiKindedTyVar newTyVarTyVar

--------------------------------------
-- Explicit binders
--------------------------------------

-- | Used during the "kind-checking" pass in TcTyClsDecls only,
-- and even then only for data-con declarations.
-- See Note [Use TyVarTvs in kind-checking pass] in TcTyClsDecls
kcExplicitTKBndrs :: [LHsTyVarBndr GhcRn]
                  -> TcM a
                  -> TcM a
kcExplicitTKBndrs [] thing_inside = thing_inside
kcExplicitTKBndrs (L _ hs_tv : hs_tvs) thing_inside
  = do { tv <- tcHsTyVarBndr newTyVarTyVar hs_tv
       ; tcExtendTyVarEnv [tv] $
         kcExplicitTKBndrs hs_tvs thing_inside }

tcExplicitTKBndrs :: SkolemInfo
                  -> [LHsTyVarBndr GhcRn]
                  -> TcM a
                  -> TcM ([TcTyVar], a)
tcExplicitTKBndrs skol_info hs_tvs thing_inside
-- Used for the forall'd binders in type signatures of various kinds:
--     - function signatures
--     - data con signatures in GADT-style decls
--     - pattern synonym signatures
--     - expression type signatures
--
-- Specifically NOT used for the binders of a data type
-- or type family decl. So the forall'd variables always /shadow/
-- anything already in scope, and the complications of
-- tcHsQTyVarName to not apply.
--
-- This function brings into scope a telescope of binders as written by
-- the user. At first blush, it would then seem that we should bring
-- them into scope one at a time, bumping the TcLevel each time.
-- (Recall that we bump the level to prevent skolem escape from happening.)
-- However, this leads to terrible error messages, because we end up
-- failing to unify with some `k0`. Better would be to allow type inference
-- to work, potentially creating a skolem-escape problem, and then to
-- notice that the telescope is out of order. That's what we do here,
-- following the logic of tcImplicitTKBndrsX.
-- See also Note [Keeping scoped variables in order: Explicit]
--
-- No cloning: returned TyVars have the same Name as the incoming LHsTyVarBndrs
  | null hs_tvs  -- Short cut that avoids creating an implication
                 -- constraint in the common case where none is needed
  = do { result <- thing_inside
       ; return ([], result) }

  | otherwise
  = do { (skol_tvs, result) <- checkTvConstraints skol_info (Just doc) $
                               bind_tvbs hs_tvs

       ; traceTc "tcExplicitTKBndrs" $
           vcat [ text "Hs vars:" <+> ppr hs_tvs
                , text "tvs:" <+> pprTyVars skol_tvs ]

       ; return (skol_tvs, result) }

  where
    bind_tvbs [] = do { result <- thing_inside
                      ; return ([], result) }
    bind_tvbs (L _ tvb : tvbs)
      = do { tv <- tcHsTyVarBndr newSkolemTyVar tvb
           ; tcExtendTyVarEnv [tv] $
        do { (tvs, result) <- bind_tvbs tvbs
           ; return (tv : tvs, result) }}

    doc = sep (map ppr hs_tvs)

-----------------
tcHsTyVarBndr :: (Name -> Kind -> TcM TyVar)
              -> HsTyVarBndr GhcRn -> TcM TcTyVar
-- Return a TcTyVar, built using the provided function
-- Typically the Kind inside the HsTyVarBndr will be a tyvar
-- with a mutable kind in it.
--
-- Returned TcTyVar has the same name; no cloning
tcHsTyVarBndr new_tv (UserTyVar _ (L _ tv_nm))
  = newFlexiKindedTyVar new_tv tv_nm
tcHsTyVarBndr new_tv (KindedTyVar _ (L _ tv_nm) lhs_kind)
  = do { kind <- tcLHsKindSig (TyVarBndrKindCtxt tv_nm) lhs_kind
       ; new_tv tv_nm kind }
tcHsTyVarBndr _ (XTyVarBndr _) = panic "tcHsTyVarBndr"

-----------------
newWildTyVar :: Name -> TcM TcTyVar
-- ^ New unification variable for a wildcard
newWildTyVar _name
  = do { kind <- newMetaKindVar
       ; uniq <- newUnique
       ; details <- newMetaDetails TauTv
       ; let name = mkSysTvName uniq (fsLit "w")
             tyvar = (mkTcTyVar name kind details)
       ; traceTc "newWildTyVar" (ppr tyvar)
       ; return tyvar }

--------------------------
-- Bringing tyvars into scope
--------------------------

-- | Bring tyvars into scope, wrapping the thing_inside in an implication
-- constraint. The implication constraint is necessary to provide SkolemInfo
-- for the tyvars and to ensure that no unification variables made outside
-- the scope of these tyvars (i.e. lower TcLevel) unify with the locally-scoped
-- tyvars (i.e. higher TcLevel).
--
-- INVARIANT: The thing_inside must check only types, never terms.
--
-- Use this (not tcExtendTyVarEnv) wherever you expect a Λ or ∀ in Core.
-- Use tcExtendTyVarEnv otherwise.
scopeTyVars :: SkolemInfo -> [TcTyVar] -> TcM a -> TcM a
scopeTyVars skol_info tvs = scopeTyVars2 skol_info [(tyVarName tv, tv) | tv <- tvs]

-- | Like 'scopeTyVars', but allows you to specify different scoped names
-- than the Names stored within the tyvars.
scopeTyVars2 :: SkolemInfo -> [(Name, TcTyVar)] -> TcM a -> TcM a
scopeTyVars2 skol_info prs thing_inside
  = fmap snd $ -- discard the TcEvBinds, which will always be empty
    checkConstraints skol_info (map snd prs) [{- no EvVars -}] $
    tcExtendNameTyVarEnv prs $
    thing_inside

------------------
kindGeneralize :: TcType -> TcM [KindVar]
-- Quantify the free kind variables of a kind or type
-- In the latter case the type is closed, so it has no free
-- type variables.  So in both cases, all the free vars are kind vars
-- Input needn't be zonked.
-- NB: You must call solveEqualities or solveLocalEqualities before
-- kind generalization
kindGeneralize = kindGeneralizeLocal emptyWC

-- | This variant of 'kindGeneralize' refuses to generalize over any
-- variables free in the given WantedConstraints. Instead, it promotes
-- these variables into an outer TcLevel. See also
-- Note [Promoting unification variables] in TcSimplify
kindGeneralizeLocal :: WantedConstraints -> TcType -> TcM [KindVar]
kindGeneralizeLocal wanted kind_or_type
  = do {
       -- This bit is very much like decideMonoTyVars in TcSimplify,
       -- but constraints are so much simpler in kinds, it is much
       -- easier here. (In particular, we never quantify over a
       -- constraint in a type.)
       ; constrained <- zonkTyCoVarsAndFV (tyCoVarsOfWC wanted)
       ; (_, constrained) <- promoteTyVarSet constrained

       ; gbl_tvs <- tcGetGlobalTyCoVars -- Already zonked
       ; let mono_tvs = gbl_tvs `unionVarSet` constrained

         -- use the "Kind" variant here, as any types we see
         -- here will already have all type variables quantified;
         -- thus, every free variable is really a kv, never a tv.
       ; dvs <- candidateQTyVarsOfKind mono_tvs kind_or_type

       ; traceTc "kindGeneralizeLocal" (vcat [ ppr wanted
                                             , ppr kind_or_type
                                             , ppr constrained
                                             , ppr mono_tvs
                                             , ppr dvs ])

       ; quantifyTyVars mono_tvs dvs }

{-
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

Note [Free-floating kind vars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data T = MkT (forall (a :: k). Proxy a)
  -- from test ghci/scripts/T7873

This is not an existential datatype, but a higher-rank one (the forall
to the right of MkT). Also consider

  data S a = MkS (Proxy (a :: k))

According to the rules around implicitly-bound kind variables, in both
cases those k's scope over the whole declaration. The renamer grabs
it and adds it to the hsq_implicits field of the HsQTyVars of the
tycon. So it must be in scope during type-checking, but we want to
reject T while accepting S.

Why reject T? Because the kind variable isn't fixed by anything. For
a variable like k to be implicit, it needs to be mentioned in the kind
of a tycon tyvar. But it isn't.

Why accept S? Because kind inference tells us that a has kind k, so it's
all OK.

Our approach depends on whether or not the datatype has a CUSK.

Non-CUSK: In the first pass (kcTyClTyVars) we just bring
k into scope. In the second pass (tcTyClTyVars),
we check to make sure that k has been unified with some other variable
(or generalized over, making k into a skolem). If it hasn't been, then
it must be a free-floating kind var. Error.

CUSK: When we determine the tycon's final, never-to-be-changed kind
in kcLHsQTyVars, we check to make sure all implicitly-bound kind
vars are indeed mentioned in a kind somewhere. If not, error.

We also perform free-floating kind var analysis for type family instances
(see #13985). Here is an interesting example:

    type family   T :: k
    type instance T = (Nothing :: Maybe a)

Upon a cursory glance, it may appear that the kind variable `a` is
free-floating above, since there are no (visible) LHS patterns in `T`. However,
there is an *invisible* pattern due to the return kind, so inside of GHC, the
instance looks closer to this:

    type family T @k :: k
    type instance T @(Maybe a) = (Nothing :: Maybe a)

Here, we can see that `a` really is bound by a LHS type pattern, so `a` is in
fact not free-floating. Contrast that with this example:

    type instance T = Proxy (Nothing :: Maybe a)

This would looks like this inside of GHC:

    type instance T @(*) = Proxy (Nothing :: Maybe a)

So this time, `a` is neither bound by a visible nor invisible type pattern on
the LHS, so it would be reported as free-floating.

Finally, here's one more brain-teaser (from #9574). In the example below:

    class Funct f where
      type Codomain f :: *
    instance Funct ('KProxy :: KProxy o) where
      type Codomain 'KProxy = NatTr (Proxy :: o -> *)

As it turns out, `o` is not free-floating in this example. That is because `o`
bound by the kind signature of the LHS type pattern 'KProxy. To make this more
obvious, one can also write the instance like so:

    instance Funct ('KProxy :: KProxy o) where
      type Codomain ('KProxy :: KProxy o) = NatTr (Proxy :: o -> *)

-}

--------------------
-- getInitialKind has made a suitably-shaped kind for the type or class
-- Look it up in the local environment. This is used only for tycons
-- that we're currently type-checking, so we're sure to find a TcTyCon.
kcLookupTcTyCon :: Name -> TcM TcTyCon
kcLookupTcTyCon nm
  = do { tc_ty_thing <- tcLookup nm
       ; return $ case tc_ty_thing of
           ATcTyCon tc -> tc
           _           -> pprPanic "kcLookupTcTyCon" (ppr tc_ty_thing) }

-----------------------
-- | Bring tycon tyvars into scope. This is used during the "kind-checking"
-- pass in TcTyClsDecls. (Never in getInitialKind, never in the
-- "type-checking"/desugaring pass.)
-- Never emits constraints, though the thing_inside might.
kcTyClTyVars :: Name -> TcM a -> TcM a
kcTyClTyVars tycon_name thing_inside
  -- See Note [Use TyVarTvs in kind-checking pass] in TcTyClsDecls
  = do { tycon <- kcLookupTcTyCon tycon_name
       ; tcExtendNameTyVarEnv (tcTyConScopedTyVars tycon) $ thing_inside }

tcTyClTyVars :: Name
             -> ([TyConBinder] -> Kind -> TcM a) -> TcM a
-- ^ Used for the type variables of a type or class decl
-- on the second full pass (type-checking/desugaring) in TcTyClDecls.
-- This is *not* used in the initial-kind run, nor in the "kind-checking" pass.
-- Accordingly, everything passed to the continuation is fully zonked.
--
-- (tcTyClTyVars T [a,b] thing_inside)
--   where T : forall k1 k2 (a:k1 -> *) (b:k1). k2 -> *
--   calls thing_inside with arguments
--      [k1,k2,a,b] [k1:*, k2:*, Anon (k1 -> *), Anon k1] (k2 -> *)
--   having also extended the type environment with bindings
--   for k1,k2,a,b
--
-- Never emits constraints.
--
-- The LHsTyVarBndrs is always user-written, and the full, generalised
-- kind of the tycon is available in the local env.
tcTyClTyVars tycon_name thing_inside
  = do { tycon <- kcLookupTcTyCon tycon_name

       -- Do checks on scoped tyvars
       -- See Note [Free-floating kind vars]
       ; let flav = tyConFlavour tycon
             scoped_prs = tcTyConScopedTyVars tycon
             scoped_tvs = map snd scoped_prs
             still_sig_tvs = filter isTyVarTyVar scoped_tvs

       ; mapM_ report_sig_tv_err (findDupTyVarTvs scoped_prs)

       ; checkNoErrs $ reportFloatingKvs tycon_name flav
                                         scoped_tvs still_sig_tvs

       ; let res_kind   = tyConResKind tycon
             binders    = correct_binders (tyConBinders tycon) res_kind
       ; traceTc "tcTyClTyVars" (ppr tycon_name <+> ppr binders)
       ; scopeTyVars2 (TyConSkol flav tycon_name) scoped_prs $
         thing_inside binders res_kind }
  where
    report_sig_tv_err (n1, n2)
      = setSrcSpan (getSrcSpan n2) $
        addErrTc (text "Couldn't match" <+> quotes (ppr n1)
                        <+> text "with" <+> quotes (ppr n2))

    -- Given some TyConBinders and a TyCon's result kind, make sure that the
    -- correct any wrong Named/Anon choices. For example, consider
    --   type Syn k = forall (a :: k). Proxy a
    -- At first, it looks like k should be named -- after all, it appears on the RHS.
    -- However, the correct kind for Syn is (* -> *).
    -- (Why? Because k is the kind of a type, so k's kind is *. And the RHS also has
    -- kind *.) See also #13963.
    correct_binders :: [TyConBinder] -> Kind -> [TyConBinder]
    correct_binders binders kind
      = binders'
      where
        (_, binders') = mapAccumR go (tyCoVarsOfType kind) binders

        go :: TyCoVarSet -> TyConBinder -> (TyCoVarSet, TyConBinder)
        go fvs binder
          | isNamedTyConBinder binder
          , not (tv `elemVarSet` fvs)
          = (new_fvs, mkAnonTyConBinder tv)

          | not (isNamedTyConBinder binder)
          , tv `elemVarSet` fvs
          = (new_fvs, mkNamedTyConBinder Required tv)
             -- always Required, because it was anonymous (i.e. visible) previously

          | otherwise
          = (new_fvs, binder)

          where
            tv      = binderVar binder
            new_fvs = fvs `delVarSet` tv `unionVarSet` tyCoVarsOfType (tyVarKind tv)

-----------------------------------
tcDataKindSig :: [TyConBinder]
              -> Kind
              -> TcM ([TyConBinder], Kind)
-- GADT decls can have a (perhaps partial) kind signature
--      e.g.  data T a :: * -> * -> * where ...
-- This function makes up suitable (kinded) TyConBinders for the
-- argument kinds.  E.g. in this case it might return
--   ([b::*, c::*], *)
-- Never emits constraints.
-- It's a little trickier than you might think: see
-- Note [TyConBinders for the result kind signature of a data type]
tcDataKindSig tc_bndrs kind
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

          Just (Anon arg, kind')
            -> go loc occs' uniqs' subst' (tcb : acc) kind'
            where
              arg'   = substTy subst arg
              tv     = mkTyVar (mkInternalName uniq occ loc) arg'
              subst' = extendTCvInScope subst tv
              tcb    = Bndr tv AnonTCB
              (uniq:uniqs') = uniqs
              (occ:occs')   = occs

          Just (Named (Bndr tv vis), kind')
            -> go loc occs uniqs subst' (tcb : acc) kind'
            where
              (subst', tv') = substTyVarBndr subst tv
              tcb = Bndr tv' (NamedTCB vis)

badKindSig :: Bool -> Kind -> SDoc
badKindSig check_for_type kind
 = hang (sep [ text "Kind signature on data type declaration has non-*"
             , (if check_for_type then empty else text "and non-variable") <+>
               text "return kind" ])
        2 (ppr kind)

{- Note [TyConBinders for the result kind signature of a data type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given
  data T (a::*) :: * -> forall k. k -> *
we want to generate the extra TyConBinders for T, so we finally get
  (a::*) (b::*) (k::*) (c::k)
The function tcDataKindSig generates these extra TyConBinders from
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
this can happen: see Trac #14515.

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
(NB: the tidying happens in the conversion to IfaceSyn, which happens
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
         , [Name]             -- Original tyvar names, in correspondence with ...
         , [TcTyVar]          -- ... Implicitly and explicitly bound type variables
         , TcThetaType        -- Theta part
         , TcType )           -- Tau part
-- See Note [Recipe for checking a signature]
tcHsPartialSigType ctxt sig_ty
  | HsWC { hswc_ext  = sig_wcs,         hswc_body = ib_ty } <- sig_ty
  , HsIB { hsib_ext = implicit_hs_tvs
         , hsib_body = hs_ty } <- ib_ty
  , (explicit_hs_tvs, L _ hs_ctxt, hs_tau) <- splitLHsSigmaTy hs_ty
  = addSigCtxt ctxt hs_ty $
    do { (implicit_tvs, (explicit_tvs, (wcs, wcx, theta, tau)))
            <- tcWildCardBinders sig_wcs $ \ wcs ->
               tcImplicitTKBndrsSig skol_info implicit_hs_tvs      $
               tcExplicitTKBndrs    skol_info explicit_hs_tvs      $
               do {   -- Instantiate the type-class context; but if there
                      -- is an extra-constraints wildcard, just discard it here
                    (theta, wcx) <- tcPartialContext hs_ctxt

                  ; tau <- tcHsOpenType hs_tau

                  ; return (wcs, wcx, theta, tau) }

         -- We must return these separately, because all the zonking below
         -- might change the name of a TyVarTv. This, in turn, causes trouble
         -- in partial type signatures that bind scoped type variables, as
         -- we bring the wrong name into scope in the function body.
         -- Test case: partial-sigs/should_compile/LocalDefinitionBug
       ; let tv_names = map tyVarName (implicit_tvs ++ explicit_tvs)

       -- Spit out the wildcards (including the extra-constraints one)
       -- as "hole" constraints, so that they'll be reported if necessary
       -- See Note [Extra-constraint holes in partial type signatures]
       ; emitWildCardHoleConstraints wcs

         -- The TyVarTvs created above will sometimes have too high a TcLevel
         -- (note that they are generated *after* bumping the level in
         -- the tc{Im,Ex}plicitTKBndrsSig functions. Bumping the level
         -- is still important here, because the kinds of these variables
         -- do indeed need to have the higher level, so they can unify
         -- with other local type variables. But, now that we've type-checked
         -- everything (and solved equalities in the tcImplicit call)
         -- we need to promote the TyVarTvs so we don't violate the TcLevel
         -- invariant
       ; all_tvs <- mapM zonkPromoteTyCoVarBndr (implicit_tvs ++ explicit_tvs)
            -- zonkPromoteTyCoVarBndr deals well with TyVarTvs

       ; theta   <- mapM zonkPromoteType theta
       ; tau     <- zonkPromoteType tau

       ; checkValidType ctxt (mkSpecForAllTys all_tvs $ mkPhiTy theta tau)

       ; traceTc "tcHsPartialSigType" (ppr all_tvs)
       ; return (wcs, wcx, tv_names, all_tvs, theta, tau) }
  where
    skol_info   = SigTypeSkol ctxt
tcHsPartialSigType _ (HsWC _ (XHsImplicitBndrs _)) = panic "tcHsPartialSigType"
tcHsPartialSigType _ (XHsWildCardBndrs _) = panic "tcHsPartialSigType"

tcPartialContext :: HsContext GhcRn -> TcM (TcThetaType, Maybe TcType)
tcPartialContext hs_theta
  | Just (hs_theta1, hs_ctxt_last) <- snocView hs_theta
  , L _ (HsWildCardTy wc) <- ignoreParens hs_ctxt_last
  = do { wc_tv_ty <- tcWildCardOcc wc constraintKind
       ; theta <- mapM tcLHsPredType hs_theta1
       ; return (theta, Just wc_tv_ty) }
  | otherwise
  = do { theta <- mapM tcLHsPredType hs_theta
       ; return (theta, Nothing) }

{- Note [Extra-constraint holes in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f :: (_) => a -> a
  f x = ...

* The renamer makes a wildcard name for the "_", and puts it in
  the hswc_wcs field.

* Then, in tcHsPartialSigType, we make a new hole TcTyVar, in
  tcWildCardBinders.

* TcBinds.chooseInferredQuantifiers fills in that hole TcTyVar
  with the inferred constraints, e.g. (Eq a, Show a)

* TcErrors.mkHoleError finally reports the error.

An annoying difficulty happens if there are more than 62 inferred
constraints. Then we need to fill in the TcTyVar with (say) a 70-tuple.
Where do we find the TyCon?  For good reasons we only have constraint
tuples up to 62 (see Note [How tuples work] in TysWiredIn).  So how
can we make a 70-tuple?  This was the root cause of Trac #14217.

It's incredibly tiresome, because we only need this type to fill
in the hole, to communicate to the error reporting machinery.  Nothing
more.  So I use a HACK:

* I make an /ordinary/ tuple of the constraints, in
  TcBinds.chooseInferredQuantifiers. This is ill-kinded because
  ordinary tuples can't contain constraints, but it works fine. And for
  ordinary tuples we don't have the same limit as for constraint
  tuples (which need selectors and an assocated class).

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
               -> LHsSigWcType GhcRn          -- The type signature
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [(Name, TcTyVar)]     -- The new bit of type environment, binding
                                              -- the scoped type variables
                      , TcType)       -- The type
-- Used for type-checking type signatures in
-- (a) patterns           e.g  f (x::Int) = e
-- (b) RULE forall bndrs  e.g. forall (x::Int). f x = x
--
-- This may emit constraints
-- See Note [Recipe for checking a signature]
tcHsPatSigType ctxt sig_ty
  | HsWC { hswc_ext = sig_wcs,   hswc_body = ib_ty } <- sig_ty
  , HsIB { hsib_ext = sig_vars
         , hsib_body = hs_ty } <- ib_ty
  = addSigCtxt ctxt hs_ty $
    do { sig_tkvs <- mapM new_implicit_tv sig_vars
       ; (wcs, sig_ty)
            <- tcWildCardBinders sig_wcs  $ \ wcs ->
               tcExtendTyVarEnv sig_tkvs                           $
               do { sig_ty <- tcHsOpenType hs_ty
                  ; return (wcs, sig_ty) }

        ; emitWildCardHoleConstraints wcs

          -- sig_ty might have tyvars that are at a higher TcLevel (if hs_ty
          -- contains a forall). Promote these.
          -- Ex: f (x :: forall a. Proxy a -> ()) = ... x ...
          -- When we instantiate x, we have to compare the kind of the argument
          -- to a's kind, which will be a metavariable.
        ; sig_ty <- zonkPromoteType sig_ty
        ; checkValidType ctxt sig_ty

        ; let tv_pairs = mkTyVarNamePairs sig_tkvs

        ; traceTc "tcHsPatSigType" (ppr sig_vars)
        ; return (wcs, tv_pairs, sig_ty) }
  where
    new_implicit_tv name = do { kind <- newMetaKindVar
                              ; new_tv name kind }

    new_tv = case ctxt of
               RuleSigCtxt {} -> newSkolemTyVar
               _              -> newTauTyVar
      -- See Note [Pattern signature binders]


tcHsPatSigType _ (HsWC _ (XHsImplicitBndrs _)) = panic "tcHsPatSigType"
tcHsPatSigType _ (XHsWildCardBndrs _)          = panic "tcHsPatSigType"

tcPatSig :: Bool                    -- True <=> pattern binding
         -> LHsSigWcType GhcRn
         -> ExpSigmaType
         -> TcM (TcType,            -- The type to use for "inside" the signature
                 [(Name,TcTyVar)],  -- The new bit of type environment, binding
                                    -- the scoped type variables
                 [(Name,TcTyVar)],  -- The wildcards
                 HsWrapper)         -- Coercion due to unification with actual ty
                                    -- Of shape:  res_ty ~ sig_ty
tcPatSig in_pat_bind sig res_ty
 = do  { (sig_wcs, sig_tvs, sig_ty) <- tcHsPatSigType PatSigCtxt sig
        -- sig_tvs are the type variables free in 'sig',
        -- and not already in scope. These are the ones
        -- that should be brought into scope

        ; if null sig_tvs then do {
                -- Just do the subsumption check and return
                  wrap <- addErrCtxtM (mk_msg sig_ty) $
                          tcSubTypeET PatSigOrigin PatSigCtxt res_ty sig_ty
                ; return (sig_ty, [], sig_wcs, wrap)
        } else do
                -- Type signature binds at least one scoped type variable

                -- A pattern binding cannot bind scoped type variables
                -- It is more convenient to make the test here
                -- than in the renamer
        { when in_pat_bind (addErr (patBindSigErr sig_tvs))

                -- Check that all newly-in-scope tyvars are in fact
                -- constrained by the pattern.  This catches tiresome
                -- cases like
                --      type T a = Int
                --      f :: Int -> Int
                --      f (x :: T a) = ...
                -- Here 'a' doesn't get a binding.  Sigh
        ; let bad_tvs = filterOut (`elemVarSet` exactTyCoVarsOfType sig_ty)
                                  (tyCoVarsOfTypeList sig_ty)
        ; checkTc (null bad_tvs) (badPatTyVarTvs sig_ty bad_tvs)

        -- Now do a subsumption check of the pattern signature against res_ty
        ; wrap <- addErrCtxtM (mk_msg sig_ty) $
                  tcSubTypeET PatSigOrigin PatSigCtxt res_ty sig_ty

        -- Phew!
        ; return (sig_ty, sig_tvs, sig_wcs, wrap)
        } }
  where
    mk_msg sig_ty tidy_env
       = do { (tidy_env, sig_ty) <- zonkTidyTcType tidy_env sig_ty
            ; res_ty <- readExpType res_ty   -- should be filled in by now
            ; (tidy_env, res_ty) <- zonkTidyTcType tidy_env res_ty
            ; let msg = vcat [ hang (text "When checking that the pattern signature:")
                                  4 (ppr sig_ty)
                             , nest 2 (hang (text "fits the type of its context:")
                                          2 (ppr res_ty)) ]
            ; return (tidy_env, msg) }

patBindSigErr :: [(Name,TcTyVar)] -> SDoc
patBindSigErr sig_tvs
  = hang (text "You cannot bind scoped type variable" <> plural sig_tvs
          <+> pprQuotedList (map fst sig_tvs))
       2 (text "in a pattern binding signature")

{- Note [Pattern signature binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Type variables in the type environment] in TcRnTypes.
Consider

  data T where
    MkT :: forall a. a -> (a -> Int) -> T

  f :: T -> ...
  f (MkT x (f :: b -> c)) = <blah>

Here
 * The pattern (MkT p1 p2) creates a *skolem* type variable 'a_sk',
   It must be a skolem so that that it retains its identity, and
   TcErrors.getSkolemInfo can thereby find the binding site for the skolem.

 * The type signature pattern (f :: b -> c) makes freshs meta-tyvars
   beta and gamma (TauTvs), and binds "b" :-> beta, "c" :-> gamma in the
   environment

 * Then unification makes beta := a_sk, gamma := Int
   That's why we must make beta and gamma a MetaTv,
   not a SkolemTv, so that it can unify to a_sk (or Int, respectively).

 * Finally, in '<blah>' we have the envt "b" :-> beta, "c" :-> gamma,
   so we return the pairs ("b" :-> beta, "c" :-> gamma) from tcHsPatSigType,

Another example (Trac #13881):
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
       ; let check rn_ty (ty, act_kind) = checkExpectedKind (unLoc rn_ty) ty act_kind kind
       ; tys' <- zipWithM check rn_tys act_kinds
       ; return (tys', kind) }

{-
************************************************************************
*                                                                      *
    Promotion
*                                                                      *
************************************************************************
-}

-- | Whenever a type is about to be added to the environment, it's necessary
-- to make sure that any free meta-tyvars in the type are promoted to the
-- current TcLevel. (They might be at a higher level due to the level-bumping
-- in tcExplicitTKBndrs, for example.) This function both zonks *and*
-- promotes. Why at the same time? See Note [Recipe for checking a signature]
zonkPromoteType :: TcType -> TcM TcType
zonkPromoteType = mapType zonkPromoteMapper ()

-- cf. TcMType.zonkTcTypeMapper
zonkPromoteMapper :: TyCoMapper () TcM
zonkPromoteMapper = TyCoMapper { tcm_smart    = True
                               , tcm_tyvar    = const zonkPromoteTcTyVar
                               , tcm_covar    = const covar
                               , tcm_hole     = const hole
                               , tcm_tycobinder = const tybinder
                               , tcm_tycon    = return }
  where
    covar cv
      = mkCoVarCo <$> zonkPromoteTyCoVarKind cv

    hole :: CoercionHole -> TcM Coercion
    hole h
      = do { contents <- unpackCoercionHole_maybe h
           ; case contents of
               Just co -> do { co <- zonkPromoteCoercion co
                             ; checkCoercionHole cv co }
               Nothing -> do { cv' <- zonkPromoteTyCoVarKind cv
                             ; return $ mkHoleCo (setCoHoleCoVar h cv') } }
      where
        cv = coHoleCoVar h

    tybinder :: TyVar -> ArgFlag -> TcM ((), TyVar)
    tybinder tv _flag = ((), ) <$> zonkPromoteTyCoVarKind tv

zonkPromoteTcTyVar :: TyCoVar -> TcM TcType
zonkPromoteTcTyVar tv
  | isMetaTyVar tv
  = do { let ref = metaTyVarRef tv
       ; contents <- readTcRef ref
       ; case contents of
           Flexi -> do { (_, promoted_tv) <- promoteTyVar tv
                       ; mkTyVarTy <$> zonkPromoteTyCoVarKind promoted_tv }
           Indirect ty -> zonkPromoteType ty }

  | isTcTyVar tv && isSkolemTyVar tv  -- NB: isSkolemTyVar says "True" to pure TyVars
  = do { tc_lvl <- getTcLevel
       ; mkTyVarTy <$> zonkPromoteTyCoVarKind (promoteSkolem tc_lvl tv) }

  | otherwise
  = mkTyVarTy <$> zonkPromoteTyCoVarKind tv

zonkPromoteTyCoVarKind :: TyCoVar -> TcM TyCoVar
zonkPromoteTyCoVarKind = updateTyVarKindM zonkPromoteType

zonkPromoteTyCoVarBndr :: TyCoVar -> TcM TyCoVar
zonkPromoteTyCoVarBndr tv
  | isTyVarTyVar tv
  = tcGetTyVar "zonkPromoteTyCoVarBndr TyVarTv" <$> zonkPromoteTcTyVar tv

  | isTcTyVar tv && isSkolemTyVar tv
  = do { tc_lvl <- getTcLevel
       ; zonkPromoteTyCoVarKind (promoteSkolem tc_lvl tv) }

  | otherwise
  = zonkPromoteTyCoVarKind tv

zonkPromoteCoercion :: Coercion -> TcM Coercion
zonkPromoteCoercion = mapCoercion zonkPromoteMapper ()

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
-- See  Note [Recipe for checking a signature] in TcHsType
-- Result is zonked
  = do { kind <- solveLocalEqualities $
                 tc_lhs_kind kindLevelMode hs_kind
       ; traceTc "tcLHsKindSig" (ppr hs_kind $$ ppr kind)
       -- No generalization, so we must promote
       ; kind <- zonkPromoteType kind
         -- This zonk is very important in the case of higher rank kinds
         -- E.g. Trac #13879    f :: forall (p :: forall z (y::z). <blah>).
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
               PatSynExPE     -> sep [ text "the existential variables of a pattern synonym"
                                     , text "signature do not scope over the pattern" ]
               _ -> text "it is defined and used in the same recursive group"

{-
************************************************************************
*                                                                      *
                Scoped type variables
*                                                                      *
************************************************************************
-}

badPatTyVarTvs :: TcType -> [TyVar] -> SDoc
badPatTyVarTvs sig_ty bad_tvs
  = vcat [ fsep [text "The type variable" <> plural bad_tvs,
                 quotes (pprWithCommas ppr bad_tvs),
                 text "should be bound by the pattern signature" <+> quotes (ppr sig_ty),
                 text "but are actually discarded by a type synonym" ]
         , text "To fix this, expand the type synonym"
         , text "[Note: I hope to lift this restriction in due course]" ]

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
  = hang (hsep [ text "In the", speakNth arg_no, ptext (sLit "argument of"),
                    quotes (ppr fun) <> text ", namely"])
       2 (quotes (ppr arg))

-- See Note [Free-floating kind vars]
reportFloatingKvs :: Name         -- of the tycon
                  -> TyConFlavour -- What sort of TyCon it is
                  -> [TcTyVar]    -- all tyvars, not necessarily zonked
                  -> [TcTyVar]    -- floating tyvars
                  -> TcM ()
reportFloatingKvs tycon_name flav all_tvs bad_tvs
  = unless (null bad_tvs) $  -- don't bother zonking if there's no error
    do { all_tvs <- mapM zonkTcTyVarToTyVar all_tvs
       ; bad_tvs <- mapM zonkTcTyVarToTyVar bad_tvs
       ; let (tidy_env, tidy_all_tvs) = tidyOpenTyCoVars emptyTidyEnv all_tvs
             tidy_bad_tvs             = map (tidyTyCoVarOcc tidy_env) bad_tvs
       ; mapM_ (report tidy_all_tvs) tidy_bad_tvs }
  where
    report tidy_all_tvs tidy_bad_tv
      = addErr $
        vcat [ text "Kind variable" <+> quotes (ppr tidy_bad_tv) <+>
               text "is implicitly bound in" <+> ppr flav
             , quotes (ppr tycon_name) <> comma <+>
               text "but does not appear as the kind of any"
             , text "of its type variables. Perhaps you meant"
             , text "to bind it explicitly somewhere?"
             , ppWhen (not (null tidy_all_tvs)) $
                 hang (text "Type variables with inferred kinds:")
                 2 (ppr_tv_bndrs tidy_all_tvs) ]

    ppr_tv_bndrs tvs = sep (map pp_tv tvs)
    pp_tv tv         = parens (ppr tv <+> dcolon <+> ppr (tyVarKind tv))

-- | If the inner action emits constraints, reports them as errors and fails;
-- otherwise, propagates the return value. Useful as a wrapper around
-- 'tcImplicitTKBndrs', which uses solveLocalEqualities, when there won't be
-- another chance to solve constraints
failIfEmitsConstraints :: TcM a -> TcM a
failIfEmitsConstraints thing_inside
  = do { (res, lie) <- captureConstraints thing_inside
       ; checkNoErrs $ reportAllUnsolved lie
       ; return res
       }

-- | Add a "In the data declaration for T" or some such.
addTyConFlavCtxt :: Name -> TyConFlavour -> TcM a -> TcM a
addTyConFlavCtxt name flav
  = addErrCtxt $ hsep [ text "In the", ppr flav
                      , text "declaration for", quotes (ppr name) ]
