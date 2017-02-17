{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcMonoType]{Typechecking user-specified @MonoTypes@}
-}

{-# LANGUAGE CPP, TupleSections, MultiWayIf #-}

module TcHsType (
        -- Type signatures
        kcHsSigType, tcClassSigType,
        tcHsSigType, tcHsSigWcType,
        tcHsPartialSigType,
        funsSigCtxt, addSigCtxt, pprSigCtxt,

        tcHsClsInstType,
        tcHsDeriv, tcHsVectInst,
        tcHsTypeApp,
        UserTypeCtxt(..),
        tcImplicitTKBndrs, tcImplicitTKBndrsType, tcExplicitTKBndrs,

                -- Type checking type and class decls
        kcLookupTcTyCon, kcTyClTyVars, tcTyClTyVars,
        tcDataKindSig,

        -- Kind-checking types
        -- No kind generalisation, no checkValidType
        tcWildCardBinders,
        kcHsTyVarBndrs,
        tcHsLiftedType,   tcHsOpenType,
        tcHsLiftedTypeNC, tcHsOpenTypeNC,
        tcLHsType, tcCheckLHsType,
        tcHsContext, tcLHsPredType, tcInferApps, tcInferArgs,
        solveEqualities, -- useful re-export

        kindGeneralize,

        -- Sort-checking kinds
        tcLHsKind,

        -- Pattern type signatures
        tcHsPatSigType, tcPatSig, funAppCtxt
   ) where

#include "HsVersions.h"

import HsSyn
import TcRnMonad
import TcEvidence
import TcEnv
import TcMType
import TcValidity
import TcUnify
import TcIface
import TcSimplify ( solveEqualities )
import TcType
import TcHsSyn( zonkSigType )
import Inst   ( tcInstBinders, tcInstBindersX, tcInstBinderX )
import Type
import Kind
import RdrName( lookupLocalRdrOcc )
import Var
import VarSet
import TyCon
import ConLike
import DataCon
import Class
import Name
import NameEnv
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
import Data.List ( partition, zipWith4 )
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
This is because some type-checking is done in a mutually-recursive knot, and
if we look too closely at the tycons, we'll loop. This is why we always must
use mkNakedTyConApp and mkNakedAppTys, etc., which never look at a tycon.
The mkNamed... functions don't uphold Type invariants, but zonkTcTypeToType
will repair this for us. Note that zonkTcType *is* safe within a knot, and
can be done repeatedly with no ill effect: it just squeezes out metavariables.

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

- Ambiguity checks involve functional dependencies, and it's easier to wait
  until knots have been resolved before poking into them

Also, in a mutually recursive group of types, we can't look at the TyCon until we've
finished building the loop.  So to keep things simple, we postpone most validity
checking until step (3).

Knot tying
~~~~~~~~~~
During step (1) we might fault in a TyCon defined in another module, and it might
(via a loop) refer back to a TyCon defined in this module. So when we tie a big
knot around type declarations with ARecThing, so that the fault-in code can get
the TyCon being defined.

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

addSigCtxt :: UserTypeCtxt -> LHsType Name -> TcM a -> TcM a
addSigCtxt ctxt hs_ty thing_inside
  = setSrcSpan (getLoc hs_ty) $
    addErrCtxt (pprSigCtxt ctxt hs_ty) $
    thing_inside

pprSigCtxt :: UserTypeCtxt -> LHsType Name -> SDoc
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

tcHsSigWcType :: UserTypeCtxt -> LHsSigWcType Name -> TcM Type
-- This one is used when we have a LHsSigWcType, but in
-- a place where wildards aren't allowed. The renamer has
-- already checked this, so we can simply ignore it.
tcHsSigWcType ctxt sig_ty = tcHsSigType ctxt (dropWildCards sig_ty)

kcHsSigType :: [Located Name] -> LHsSigType Name -> TcM ()
kcHsSigType names (HsIB { hsib_body = hs_ty
                        , hsib_vars = sig_vars })
  = addSigCtxt (funsSigCtxt names) hs_ty $
    discardResult $
    tcImplicitTKBndrsType sig_vars $
    tc_lhs_type typeLevelMode hs_ty liftedTypeKind

tcClassSigType :: [Located Name] -> LHsSigType Name -> TcM Type
-- Does not do validity checking; this must be done outside
-- the recursive class declaration "knot"
tcClassSigType names sig_ty
  = addSigCtxt (funsSigCtxt names) (hsSigType sig_ty) $
    do { ty <- tc_hs_sig_type sig_ty liftedTypeKind
       ; kindGeneralizeType ty }

tcHsSigType :: UserTypeCtxt -> LHsSigType Name -> TcM Type
-- Does validity checking
tcHsSigType ctxt sig_ty
  = addSigCtxt ctxt (hsSigType sig_ty) $
    do { kind <- case expectedKindInCtxt ctxt of
                    AnythingKind -> newMetaKindVar
                    TheKind k    -> return k
                    OpenKind     -> newOpenTypeKind
              -- The kind is checked by checkValidType, and isn't necessarily
              -- of kind * in a Template Haskell quote eg [t| Maybe |]

       ; ty <- tc_hs_sig_type sig_ty kind

          -- Generalise here: see Note [Kind generalisation]
       ; do_kind_gen <- decideKindGeneralisationPlan ty
       ; ty <- if do_kind_gen
               then kindGeneralizeType ty
               else zonkTcType ty

       ; checkValidType ctxt ty
       ; return ty }

tc_hs_sig_type :: LHsSigType Name -> Kind -> TcM Type
-- Does not do validity checking or zonking
tc_hs_sig_type (HsIB { hsib_body = hs_ty
                     , hsib_vars = sig_vars }) kind
  = do { (tkvs, ty) <- solveEqualities $
                       tcImplicitTKBndrsType sig_vars $
                       tc_lhs_type typeLevelMode hs_ty kind
       ; return (mkSpecForAllTys tkvs ty) }

-----------------
tcHsDeriv :: LHsSigType Name -> TcM ([TyVar], Class, [Type], [Kind])
-- Like tcHsSigType, but for the ...deriving( C t1 ty2 ) clause
-- Returns the C, [ty1, ty2, and the kinds of C's remaining arguments
-- E.g.    class C (a::*) (b::k->k)
--         data T a b = ... deriving( C Int )
--    returns ([k], C, [k, Int], [k->k])
tcHsDeriv hs_ty
  = do { cls_kind <- newMetaKindVar
                    -- always safe to kind-generalize, because there
                    -- can be no covars in an outer scope
       ; ty <- checkNoErrs $
                 -- avoid redundant error report with "illegal deriving", below
               tc_hs_sig_type hs_ty cls_kind
       ; ty <- kindGeneralizeType ty  -- also zonks
       ; cls_kind <- zonkTcType cls_kind
       ; let (tvs, pred) = splitForAllTys ty
       ; let (args, _) = splitFunTys cls_kind
       ; case getClassPredTys_maybe pred of
           Just (cls, tys) -> return (tvs, cls, tys, args)
           Nothing -> failWithTc (text "Illegal deriving item" <+> quotes (ppr hs_ty)) }

tcHsClsInstType :: UserTypeCtxt    -- InstDeclCtxt or SpecInstCtxt
                -> LHsSigType Name
                -> TcM ([TyVar], ThetaType, Class, [Type])
-- Like tcHsSigType, but for a class instance declaration
tcHsClsInstType user_ctxt hs_inst_ty
  = setSrcSpan (getLoc (hsSigType hs_inst_ty)) $
    do { inst_ty <- tc_hs_sig_type hs_inst_ty constraintKind
       ; inst_ty <- kindGeneralizeType inst_ty
       ; checkValidInstance user_ctxt hs_inst_ty inst_ty }

-- Used for 'VECTORISE [SCALAR] instance' declarations
tcHsVectInst :: LHsSigType Name -> TcM (Class, [Type])
tcHsVectInst ty
  | Just (L _ cls_name, tys) <- hsTyGetAppHead_maybe (hsSigType ty)
    -- Ignoring the binders looks pretty dodgy to me
  = do { (cls, cls_kind) <- tcClass cls_name
       ; (applied_class, _res_kind)
           <- tcInferApps typeLevelMode cls_name (mkClassPred cls []) cls_kind tys
       ; case tcSplitTyConApp_maybe applied_class of
           Just (_tc, args) -> ASSERT( _tc == classTyCon cls )
                               return (cls, args)
           _ -> failWithTc (text "Too many arguments passed to" <+> ppr cls_name) }
  | otherwise
  = failWithTc $ text "Malformed instance type"

----------------------------------------------
-- | Type-check a visible type application
tcHsTypeApp :: LHsWcType Name -> Kind -> TcM Type
tcHsTypeApp wc_ty kind
  | HsWC { hswc_wcs = sig_wcs, hswc_body = hs_ty } <- wc_ty
  = do { ty <- solveEqualities $
               tcWildCardBindersX newWildTyVar sig_wcs $ \ _ ->
               tcCheckLHsType hs_ty kind
       ; ty <- zonkTcType ty
       ; checkValidType TypeAppCtxt ty
       ; return ty }
        -- NB: we don't call emitWildcardHoleConstraints here, because
        -- we want any holes in visible type applications to be used
        -- without fuss. No errors, warnings, extensions, etc.

{-
************************************************************************
*                                                                      *
            The main kind checker: no validity checks here
%*                                                                      *
%************************************************************************

        First a couple of simple wrappers for kcHsType
-}

---------------------------
tcHsOpenType, tcHsLiftedType,
  tcHsOpenTypeNC, tcHsLiftedTypeNC :: LHsType Name -> TcM TcType
-- Used for type signatures
-- Do not do validity checking
tcHsOpenType ty   = addTypeCtxt ty $ tcHsOpenTypeNC ty
tcHsLiftedType ty = addTypeCtxt ty $ tcHsLiftedTypeNC ty

tcHsOpenTypeNC   ty = do { ek <- newOpenTypeKind
                         ; tc_lhs_type typeLevelMode ty ek }
tcHsLiftedTypeNC ty = tc_lhs_type typeLevelMode ty liftedTypeKind

-- Like tcHsType, but takes an expected kind
tcCheckLHsType :: LHsType Name -> Kind -> TcM Type
tcCheckLHsType hs_ty exp_kind
  = addTypeCtxt hs_ty $
    tc_lhs_type typeLevelMode hs_ty exp_kind

tcLHsType :: LHsType Name -> TcM (TcType, TcKind)
-- Called from outside: set the context
tcLHsType ty = addTypeCtxt ty (tc_infer_lhs_type typeLevelMode ty)

---------------------------
-- | Should we generalise the kind of this type signature?
-- We *should* generalise if the type is mentions no scoped type variables
-- or if NoMonoLocalBinds is set. Otherwise, nope.
decideKindGeneralisationPlan :: Type -> TcM Bool
decideKindGeneralisationPlan ty
  = do { mono_locals <- xoptM LangExt.MonoLocalBinds
       ; in_scope <- getInLocalScope
       ; let fvs        = tyCoVarsOfTypeList ty
             should_gen = not mono_locals || all (not . in_scope . getName) fvs
       ; traceTc "decideKindGeneralisationPlan"
           (vcat [ text "type:" <+> ppr ty
                 , text "ftvs:" <+> ppr fvs
                 , text "should gen?" <+> ppr should_gen ])
       ; return should_gen }

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
newtype TcTyMode
  = TcTyMode { mode_level :: TypeOrKind  -- True <=> type, False <=> kind
             }

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
again. This is done in calls to tcInstBinders and tcInstBindersX.

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
tc_infer_lhs_type :: TcTyMode -> LHsType Name -> TcM (TcType, TcKind)
tc_infer_lhs_type mode (L span ty)
  = setSrcSpan span $
    do { (ty', kind) <- tc_infer_hs_type mode ty
       ; return (ty', kind) }

-- | Infer the kind of a type and desugar. This is the "up" type-checker,
-- as described in Note [Bidirectional type checking]
tc_infer_hs_type :: TcTyMode -> HsType Name -> TcM (TcType, TcKind)
tc_infer_hs_type mode (HsTyVar _ (L _ tv)) = tcTyVar mode $ unEmb tv
tc_infer_hs_type mode (HsAppTy ty1 ty2)
  = do { let (fun_ty, arg_tys) = splitHsAppTys ty1 [ty2]
       ; (fun_ty', fun_kind) <- tc_infer_lhs_type mode fun_ty
       ; fun_kind' <- zonkTcType fun_kind
       ; tcInferApps mode fun_ty fun_ty' fun_kind' arg_tys }
tc_infer_hs_type mode (HsParTy t)     = tc_infer_lhs_type mode t
tc_infer_hs_type mode (HsOpTy lhs (L _ op) rhs)
  | not (op `hasKey` funTyConKey)
  = do { (op', op_kind) <- tcTyVar mode op
       ; op_kind' <- zonkTcType op_kind
       ; tcInferApps mode op op' op_kind' [lhs, rhs] }
tc_infer_hs_type mode (HsKindSig ty sig)
  = do { sig' <- tc_lhs_kind (kindLevel mode) sig
       ; ty' <- tc_lhs_type mode ty sig'
       ; return (ty', sig') }
-- HsSpliced is an annotation produced by 'RnSplice.rnSpliceType' to communicate
-- the splice location to the typechecker. Here we skip over it in order to have
-- the same kind inferred for a given expression whether it was produced from
-- splices or not.
--
-- See Note [Delaying modFinalizers in untyped splices].
tc_infer_hs_type mode (HsSpliceTy (HsSpliced _ (HsSplicedTy ty)) _)
  = tc_infer_hs_type mode ty
tc_infer_hs_type mode (HsDocTy ty _) = tc_infer_lhs_type mode ty
tc_infer_hs_type _    (HsCoreTy ty)  = return (ty, typeKind ty)
tc_infer_hs_type mode other_ty
  = do { kv <- newMetaKindVar
       ; ty' <- tc_hs_type mode other_ty kv
       ; return (ty', kv) }

------------------------------------------
tc_lhs_type :: TcTyMode -> LHsType Name -> TcKind -> TcM TcType
tc_lhs_type mode (L span ty) exp_kind
  = setSrcSpan span $
    do { ty' <- tc_hs_type mode ty exp_kind
       ; return ty' }

------------------------------------------
tc_fun_type :: TcTyMode -> LHsType Name -> LHsType Name -> TcKind -> TcM TcType
tc_fun_type mode ty1 ty2 exp_kind = case mode_level mode of
  TypeLevel ->
    do { arg_k <- newOpenTypeKind
       ; res_k <- newOpenTypeKind
       ; ty1' <- tc_lhs_type mode ty1 arg_k
       ; ty2' <- tc_lhs_type mode ty2 res_k
       ; checkExpectedKind (mkFunTy ty1' ty2') liftedTypeKind exp_kind }
  KindLevel ->  -- no representation polymorphism in kinds. yet.
    do { ty1' <- tc_lhs_type mode ty1 liftedTypeKind
       ; ty2' <- tc_lhs_type mode ty2 liftedTypeKind
       ; checkExpectedKind (mkFunTy ty1' ty2') liftedTypeKind exp_kind }

------------------------------------------
-- See also Note [Bidirectional type checking]
tc_hs_type :: TcTyMode -> HsType Name -> TcKind -> TcM TcType
tc_hs_type mode (HsParTy ty)   exp_kind = tc_lhs_type mode ty exp_kind
tc_hs_type mode (HsDocTy ty _) exp_kind = tc_lhs_type mode ty exp_kind
tc_hs_type _ ty@(HsBangTy {}) _
    -- While top-level bangs at this point are eliminated (eg !(Maybe Int)),
    -- other kinds of bangs are not (eg ((!Maybe) Int)). These kinds of
    -- bangs are invalid, so fail. (#7210)
    = failWithTc (text "Unexpected strictness annotation:" <+> ppr ty)
tc_hs_type _ ty@(HsRecTy _)      _
      -- Record types (which only show up temporarily in constructor
      -- signatures) should have been removed by now
    = failWithTc (text "Record syntax is illegal here:" <+> ppr ty)

-- HsSpliced is an annotation produced by 'RnSplice.rnSpliceType'.
-- Here we get rid of it and add the finalizers to the global environment
-- while capturing the local environment.
--
-- See Note [Delaying modFinalizers in untyped splices].
tc_hs_type mode (HsSpliceTy (HsSpliced mod_finalizers (HsSplicedTy ty))
                            _
                )
           exp_kind
  = do addModFinalizersWithLclEnv mod_finalizers
       tc_hs_type mode ty exp_kind

-- This should never happen; type splices are expanded by the renamer
tc_hs_type _ ty@(HsSpliceTy {}) _exp_kind
  = failWithTc (text "Unexpected type splice:" <+> ppr ty)

---------- Functions and applications
tc_hs_type mode (HsFunTy ty1 ty2) exp_kind
  = tc_fun_type mode ty1 ty2 exp_kind

tc_hs_type mode (HsOpTy ty1 (L _ op) ty2) exp_kind
  | op `hasKey` funTyConKey
  = tc_fun_type mode ty1 ty2 exp_kind

--------- Foralls
tc_hs_type mode (HsForAllTy { hst_bndrs = hs_tvs, hst_body = ty }) exp_kind
  = fmap fst $
    tcExplicitTKBndrs hs_tvs $ \ tvs' ->
    -- Do not kind-generalise here!  See Note [Kind generalisation]
    -- Why exp_kind?  See Note [Body kind of HsForAllTy]
    do { ty' <- tc_lhs_type mode ty exp_kind
       ; let bound_vars = allBoundVariables ty'
             bndrs      = mkTyVarBinders Specified tvs'
       ; return (mkForAllTys bndrs ty', bound_vars) }

tc_hs_type mode (HsQualTy { hst_ctxt = ctxt, hst_body = ty }) exp_kind
  | null (unLoc ctxt)
  = tc_lhs_type mode ty exp_kind

  | otherwise
  = do { ctxt' <- tc_hs_context mode ctxt

         -- See Note [Body kind of a HsQualTy]
       ; ty' <- if isConstraintKind exp_kind
                then tc_lhs_type mode ty constraintKind
                else do { ek <- newOpenTypeKind
                                -- The body kind (result of the function)
                                -- can be * or #, hence newOpenTypeKind
                        ; ty <- tc_lhs_type mode ty ek
                        ; checkExpectedKind ty liftedTypeKind exp_kind }

       ; return (mkPhiTy ctxt' ty') }

--------- Lists, arrays, and tuples
tc_hs_type mode (HsListTy elt_ty) exp_kind
  = do { tau_ty <- tc_lhs_type mode elt_ty liftedTypeKind
       ; checkWiredInTyCon listTyCon
       ; checkExpectedKind (mkListTy tau_ty) liftedTypeKind exp_kind }

tc_hs_type mode (HsPArrTy elt_ty) exp_kind
  = do { MASSERT( isTypeLevel (mode_level mode) )
       ; tau_ty <- tc_lhs_type mode elt_ty liftedTypeKind
       ; checkWiredInTyCon parrTyCon
       ; checkExpectedKind (mkPArrTy tau_ty) liftedTypeKind exp_kind }

-- See Note [Distinguishing tuple kinds] in HsTypes
-- See Note [Inferring tuple kinds]
tc_hs_type mode (HsTupleTy HsBoxedOrConstraintTuple hs_tys) exp_kind
     -- (NB: not zonking before looking at exp_k, to avoid left-right bias)
  | Just tup_sort <- tupKindSort_maybe exp_kind
  = traceTc "tc_hs_type tuple" (ppr hs_tys) >>
    tc_tuple mode tup_sort hs_tys exp_kind
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
                            checkExpectedKind ty kind arg_kind
                          | ((L loc _),ty,kind) <- zip3 hs_tys tys kinds ]

       ; finish_tuple tup_sort tys' (map (const arg_kind) tys') exp_kind }


tc_hs_type mode (HsTupleTy hs_tup_sort tys) exp_kind
  = tc_tuple mode tup_sort tys exp_kind
  where
    tup_sort = case hs_tup_sort of  -- Fourth case dealt with above
                  HsUnboxedTuple    -> UnboxedTuple
                  HsBoxedTuple      -> BoxedTuple
                  HsConstraintTuple -> ConstraintTuple
                  _                 -> panic "tc_hs_type HsTupleTy"

tc_hs_type mode (HsSumTy hs_tys) exp_kind
  = do { let arity = length hs_tys
       ; arg_kinds <- mapM (\_ -> newOpenTypeKind) hs_tys
       ; tau_tys   <- zipWithM (tc_lhs_type mode) hs_tys arg_kinds
       ; let arg_reps = map (getRuntimeRepFromKind "tc_hs_type HsSumTy") arg_kinds
             arg_tys  = arg_reps ++ tau_tys
       ; checkExpectedKind (mkTyConApp (sumTyCon arity) arg_tys)
                           (unboxedSumKind arg_reps)
                           exp_kind
       }

--------- Promoted lists and tuples
tc_hs_type mode (HsExplicitListTy _ _k tys) exp_kind
  = do { tks <- mapM (tc_infer_lhs_type mode) tys
       ; (taus', kind) <- unifyKinds tks
       ; let ty = (foldr (mk_cons kind) (mk_nil kind) taus')
       ; checkExpectedKind ty (mkListTy kind) exp_kind }
  where
    mk_cons k a b = mkTyConApp (promoteDataCon consDataCon) [k, a, b]
    mk_nil  k     = mkTyConApp (promoteDataCon nilDataCon) [k]

tc_hs_type mode (HsExplicitTupleTy _ tys) exp_kind
  -- using newMetaKindVar means that we force instantiations of any polykinded
  -- types. At first, I just used tc_infer_lhs_type, but that led to #11255.
  = do { ks   <- replicateM arity newMetaKindVar
       ; taus <- zipWithM (tc_lhs_type mode) tys ks
       ; let kind_con   = tupleTyCon           Boxed arity
             ty_con     = promotedTupleDataCon Boxed arity
             tup_k      = mkTyConApp kind_con ks
       ; checkExpectedKind (mkTyConApp ty_con (ks ++ taus)) tup_k exp_kind }
  where
    arity = length tys

--------- Constraint types
tc_hs_type mode (HsIParamTy (L _ n) ty) exp_kind
  = do { MASSERT( isTypeLevel (mode_level mode) )
       ; ty' <- tc_lhs_type mode ty liftedTypeKind
       ; let n' = mkStrLitTy $ hsIPNameFS n
       ; ipClass <- tcLookupClass ipClassName
       ; checkExpectedKind (mkClassPred ipClass [n',ty'])
           constraintKind exp_kind }

tc_hs_type mode (HsEqTy ty1 ty2) exp_kind
  = do { (ty1', kind1) <- tc_infer_lhs_type mode ty1
       ; (ty2', kind2) <- tc_infer_lhs_type mode ty2
       ; ty2'' <- checkExpectedKind ty2' kind2 kind1
       ; eq_tc <- tcLookupTyCon eqTyConName
       ; let ty' = mkNakedTyConApp eq_tc [kind1, ty1', ty2'']
       ; checkExpectedKind ty' constraintKind exp_kind }

--------- Literals
tc_hs_type _ (HsTyLit (HsNumTy _ n)) exp_kind
  = do { checkWiredInTyCon typeNatKindCon
       ; checkExpectedKind (mkNumLitTy n) typeNatKind exp_kind }

tc_hs_type _ (HsTyLit (HsStrTy _ s)) exp_kind
  = do { checkWiredInTyCon typeSymbolKindCon
       ; checkExpectedKind (mkStrLitTy s) typeSymbolKind exp_kind }

--------- Potentially kind-polymorphic types: call the "up" checker
-- See Note [Future-proofing the type checker]
tc_hs_type mode ty@(HsTyVar {})   ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsAppTy {})   ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsOpTy {})    ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsKindSig {}) ek = tc_infer_hs_type_ek mode ty ek
tc_hs_type mode ty@(HsCoreTy {})  ek = tc_infer_hs_type_ek mode ty ek

tc_hs_type _ (HsWildCardTy wc) exp_kind
  = do { wc_tv <- tcWildCardOcc wc exp_kind
       ; return (mkTyVarTy wc_tv) }

-- disposed of by renamer
tc_hs_type _ ty@(HsAppsTy {}) _
  = pprPanic "tc_hs_tyep HsAppsTy" (ppr ty)

tcWildCardOcc :: HsWildCardInfo Name -> Kind -> TcM TcTyVar
tcWildCardOcc wc_info exp_kind
  = do { wc_tv <- tcLookupTyVar (wildCardName wc_info)
          -- The wildcard's kind should be an un-filled-in meta tyvar
       ; let Just wc_kind_var = tcGetTyVar_maybe (tyVarKind wc_tv)
       ; writeMetaTyVar wc_kind_var exp_kind
       ; return wc_tv }

---------------------------
-- | Call 'tc_infer_hs_type' and check its result against an expected kind.
tc_infer_hs_type_ek :: TcTyMode -> HsType Name -> TcKind -> TcM TcType
tc_infer_hs_type_ek mode ty ek
  = do { (ty', k) <- tc_infer_hs_type mode ty
       ; checkExpectedKind ty' k ek }

---------------------------
tupKindSort_maybe :: TcKind -> Maybe TupleSort
tupKindSort_maybe k
  | Just (k', _) <- splitCastTy_maybe k = tupKindSort_maybe k'
  | Just k'      <- coreView k          = tupKindSort_maybe k'
  | isConstraintKind k = Just ConstraintTuple
  | isLiftedTypeKind k = Just BoxedTuple
  | otherwise          = Nothing

tc_tuple :: TcTyMode -> TupleSort -> [LHsType Name] -> TcKind -> TcM TcType
tc_tuple mode tup_sort tys exp_kind
  = do { arg_kinds <- case tup_sort of
           BoxedTuple      -> return (nOfThem arity liftedTypeKind)
           UnboxedTuple    -> mapM (\_ -> newOpenTypeKind) tys
           ConstraintTuple -> return (nOfThem arity constraintKind)
       ; tau_tys <- zipWithM (tc_lhs_type mode) tys arg_kinds
       ; finish_tuple tup_sort tau_tys arg_kinds exp_kind }
  where
    arity   = length tys

finish_tuple :: TupleSort
             -> [TcType]    -- ^ argument types
             -> [TcKind]    -- ^ of these kinds
             -> TcKind      -- ^ expected kind of the whole tuple
             -> TcM TcType
finish_tuple tup_sort tau_tys tau_kinds exp_kind
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
       ; checkExpectedKind (mkTyConApp tycon arg_tys) res_kind exp_kind }
  where
    arity = length tau_tys
    tau_reps = map (getRuntimeRepFromKind "finish_tuple") tau_kinds
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
-- invisible parameters as necessary. However, it does *not* necessarily
-- apply all the arguments, if the kind runs out of binders.
-- Never calls 'matchExpectedFunKind'; when the kind runs out of binders,
-- this stops processing.
-- This takes an optional @VarEnv Kind@ which maps kind variables to kinds.
-- These kinds should be used to instantiate invisible kind variables;
-- they come from an enclosing class for an associated type/data family.
-- This version will instantiate all invisible arguments left over after
-- the visible ones. Used only when typechecking type/data family patterns
-- (where we need to instantiate all remaining invisible parameters; for
-- example, consider @type family F :: k where F = Int; F = Maybe@. We
-- need to instantiate the @k@.)
tcInferArgs :: Outputable fun
            => fun                      -- ^ the function
            -> [TyConBinder]            -- ^ function kind's binders
            -> Maybe (VarEnv Kind)      -- ^ possibly, kind info (see above)
            -> [LHsType Name]           -- ^ args
            -> TcM (TCvSubst, [TyBinder], [TcType], [LHsType Name], Int)
               -- ^ (instantiating subst, un-insted leftover binders,
               --   typechecked args, untypechecked args, n)
tcInferArgs fun tc_binders mb_kind_info args
  = do { let binders = tyConBindersTyBinders tc_binders  -- UGH!
       ; (subst, leftover_binders, args', leftovers, n)
           <- tc_infer_args typeLevelMode fun binders mb_kind_info args 1
        -- now, we need to instantiate any remaining invisible arguments
       ; let (invis_bndrs, other_binders) = break isVisibleBinder leftover_binders
       ; (subst', invis_args)
           <- tcInstBindersX subst mb_kind_info invis_bndrs
       ; return ( subst'
                , other_binders
                , args' `chkAppend` invis_args
                , leftovers, n ) }

-- | See comments for 'tcInferArgs'. But this version does not instantiate
-- any remaining invisible arguments.
tc_infer_args :: Outputable fun
              => TcTyMode
              -> fun                      -- ^ the function
              -> [TyBinder]               -- ^ function kind's binders (zonked)
              -> Maybe (VarEnv Kind)      -- ^ possibly, kind info (see above)
              -> [LHsType Name]           -- ^ args
              -> Int                      -- ^ number to start arg counter at
              -> TcM (TCvSubst, [TyBinder], [TcType], [LHsType Name], Int)
tc_infer_args mode orig_ty binders mb_kind_info orig_args n0
  = go emptyTCvSubst binders orig_args n0 []
  where
    go subst binders []   n acc
      = return ( subst, binders, reverse acc, [], n )
    -- when we call this when checking type family patterns, we really
    -- do want to instantiate all invisible arguments. During other
    -- typechecking, we don't.

    go subst (binder:binders) all_args@(arg:args) n acc
      | isInvisibleBinder binder
      = do { traceTc "tc_infer_args (invis)" (ppr binder)
           ; (subst', arg') <- tcInstBinderX mb_kind_info subst binder
           ; go subst' binders all_args n (arg' : acc) }

      | otherwise
      = do { traceTc "tc_infer_args (vis)" (ppr binder $$ ppr arg)
           ; arg' <- addErrCtxt (funAppCtxt orig_ty arg n) $
                     tc_lhs_type mode arg (substTyUnchecked subst $
                                           tyBinderType binder)
           ; let subst' = extendTvSubstBinder subst binder arg'
           ; go subst' binders args (n+1) (arg' : acc) }

    go subst [] all_args n acc
      = return (subst, [], reverse acc, all_args, n)

-- | Applies a type to a list of arguments.
-- Always consumes all the arguments, using 'matchExpectedFunKind' as
-- necessary. If you wish to apply a type to a list of HsTypes, this is
-- your function.
-- Used for type-checking types only.
tcInferApps :: Outputable fun
            => TcTyMode
            -> fun                  -- ^ Function (for printing only)
            -> TcType               -- ^ Function (could be knot-tied)
            -> TcKind               -- ^ Function kind (zonked)
            -> [LHsType Name]       -- ^ Args
            -> TcM (TcType, TcKind) -- ^ (f args, result kind)
tcInferApps mode orig_ty ty ki args = go ty ki args 1
  where
    go fun fun_kind []   _ = return (fun, fun_kind)
    go fun fun_kind args n
      | let (binders, res_kind) = splitPiTys fun_kind
      , not (null binders)
      = do { (subst, leftover_binders, args', leftover_args, n')
                <- tc_infer_args mode orig_ty binders Nothing args n
           ; let fun_kind' = substTyUnchecked subst $
                             mkPiTys leftover_binders res_kind
           ; go (mkNakedAppTys fun args') fun_kind' leftover_args n' }

    go fun fun_kind all_args@(arg:args) n
      = do { (co, arg_k, res_k) <- matchExpectedFunKind (length all_args)
                                                        fun fun_kind
           ; arg' <- addErrCtxt (funAppCtxt orig_ty arg n) $
                     tc_lhs_type mode arg arg_k
           ; go (mkNakedAppTy (fun `mkNakedCastTy` co) arg')
                res_k args (n+1) }

--------------------------
checkExpectedKind :: TcType               -- the type whose kind we're checking
                  -> TcKind               -- the known kind of that type, k
                  -> TcKind               -- the expected kind, exp_kind
                  -> TcM TcType    -- a possibly-inst'ed, casted type :: exp_kind
-- Instantiate a kind (if necessary) and then call unifyType
--      (checkExpectedKind ty act_kind exp_kind)
-- checks that the actual kind act_kind is compatible
--      with the expected kind exp_kind
checkExpectedKind ty act_kind exp_kind
 = do { (ty', act_kind') <- instantiate ty act_kind exp_kind
      ; let origin = TypeEqOrigin { uo_actual   = act_kind'
                                  , uo_expected = exp_kind
                                  , uo_thing    = Just $ mkTypeErrorThing ty'
                                  }
      ; co_k <- uType origin KindLevel act_kind' exp_kind
      ; traceTc "checkExpectedKind" (vcat [ ppr act_kind
                                          , ppr exp_kind
                                          , ppr co_k ])
      ; let result_ty = ty' `mkNakedCastTy` co_k
      ; return result_ty }
  where
    -- we need to make sure that both kinds have the same number of implicit
    -- foralls out front. If the actual kind has more, instantiate accordingly.
    -- Otherwise, just pass the type & kind through -- the errors are caught
    -- in unifyType.
    instantiate :: TcType    -- the type
                -> TcKind    -- of this kind
                -> TcKind   -- but expected to be of this one
                -> TcM ( TcType   -- the inst'ed type
                       , TcKind ) -- its new kind
    instantiate ty act_ki exp_ki
      = let (exp_bndrs, _) = splitPiTysInvisible exp_ki in
        instantiateTyN (length exp_bndrs) ty act_ki

-- | Instantiate a type to have at most @n@ invisible arguments.
instantiateTyN :: Int    -- ^ @n@
               -> TcType -- ^ the type
               -> TcKind -- ^ its kind
               -> TcM (TcType, TcKind)   -- ^ The inst'ed type with kind
instantiateTyN n ty ki
  = let (bndrs, inner_ki)            = splitPiTysInvisible ki
        num_to_inst                  = length bndrs - n
           -- NB: splitAt is forgiving with invalid numbers
        (inst_bndrs, leftover_bndrs) = splitAt num_to_inst bndrs
    in
    if num_to_inst <= 0 then return (ty, ki) else
    do { (subst, inst_args) <- tcInstBinders inst_bndrs
       ; let rebuilt_ki = mkPiTys leftover_bndrs inner_ki
             ki'        = substTy subst rebuilt_ki
       ; return (mkNakedAppTys ty inst_args, ki') }

---------------------------
tcHsContext :: LHsContext Name -> TcM [PredType]
tcHsContext = tc_hs_context typeLevelMode

tcLHsPredType :: LHsType Name -> TcM PredType
tcLHsPredType = tc_lhs_pred typeLevelMode

tc_hs_context :: TcTyMode -> LHsContext Name -> TcM [PredType]
tc_hs_context mode ctxt = mapM (tc_lhs_pred mode) (unLoc ctxt)

tc_lhs_pred :: TcTyMode -> LHsType Name -> TcM PredType
tc_lhs_pred mode pred = tc_lhs_type mode pred constraintKind

---------------------------
tcTyVar :: TcTyMode -> Name -> TcM (TcType, TcKind)
-- See Note [Type checking recursive type and class declarations]
-- in TcTyClsDecls
tcTyVar mode name         -- Could be a tyvar, a tycon, or a datacon
  = do { traceTc "lk1" (ppr name)
       ; thing <- tcLookup name
       ; case thing of
           ATyVar _ tv -> return (mkTyVarTy tv, tyVarKind tv)

           ATcTyCon tc_tc -> do { -- See Note [GADT kind self-reference]
                                  unless
                                    (isTypeLevel (mode_level mode))
                                    (promotionErr name TyConPE)
                                ; check_tc tc_tc
                                ; tc <- get_loopy_tc name tc_tc
                                ; handle_tyfams tc tc_tc }
                             -- mkNakedTyConApp: see Note [Type-checking inside the knot]
                 -- NB: we really should check if we're at the kind level
                 -- and if the tycon is promotable if -XNoTypeInType is set.
                 -- But this is a terribly large amount of work! Not worth it.

           AGlobal (ATyCon tc)
             -> do { check_tc tc
                   ; handle_tyfams tc tc }

           AGlobal (AConLike (RealDataCon dc))
             -> do { data_kinds <- xoptM LangExt.DataKinds
                   ; unless (data_kinds || specialPromotedDc dc) $
                       promotionErr name NoDataKindsDC
                   ; type_in_type <- xoptM LangExt.TypeInType
                   ; unless ( type_in_type ||
                              ( isTypeLevel (mode_level mode) &&
                                isLegacyPromotableDataCon dc ) ||
                              ( isKindLevel (mode_level mode) &&
                                specialPromotedDc dc ) ) $
                       promotionErr name NoTypeInTypeDC
                   ; let tc = promoteDataCon dc
                   ; return (mkNakedTyConApp tc [], tyConKind tc) }

           APromotionErr err -> promotionErr name err

           _  -> wrongThingErr "type" thing name }
  where
    check_tc :: TyCon -> TcM ()
    check_tc tc = do { type_in_type <- xoptM LangExt.TypeInType
                     ; data_kinds   <- xoptM LangExt.DataKinds
                     ; unless (isTypeLevel (mode_level mode) ||
                               data_kinds ||
                               isKindTyCon tc) $
                       promotionErr name NoDataKindsTC
                     ; unless (isTypeLevel (mode_level mode) ||
                               type_in_type ||
                               isLegacyPromotableTyCon tc) $
                       promotionErr name NoTypeInTypeTC }

    -- if we are type-checking a type family tycon, we must instantiate
    -- any invisible arguments right away. Otherwise, we get #11246
    handle_tyfams :: TyCon   -- the tycon to instantiate (might be loopy)
                  -> TyCon   -- a non-loopy version of the tycon
                  -> TcM (TcType, TcKind)
    handle_tyfams tc tc_tc
      | mightBeUnsaturatedTyCon tc_tc
      = do { traceTc "tcTyVar2a" (ppr tc_tc $$ ppr tc_kind)
           ; return (ty, tc_kind) }

      | otherwise
      = do { (tc_ty, kind) <- instantiateTyN 0 ty tc_kind
           -- tc and tc_ty must not be traced here, because that would
           -- force the evaluation of a potentially knot-tied variable (tc),
           -- and the typechecker would hang, as per #11708
           ; traceTc "tcTyVar2b" (vcat [ ppr tc_tc <+> dcolon <+> ppr tc_kind
                                       , ppr kind ])
           ; return (tc_ty, kind) }
      where
        ty      = mkNakedTyConApp tc []
        tc_kind = tyConKind tc_tc

    get_loopy_tc :: Name -> TyCon -> TcM TyCon
    -- Return the knot-tied global TyCon if there is one
    -- Otherwise the local TcTyCon; we must be doing kind checking
    -- but we still want to return a TyCon of some sort to use in
    -- error messages
    get_loopy_tc name tc_tc
      = do { env <- getGblEnv
           ; case lookupNameEnv (tcg_type_env env) name of
                Just (ATyCon tc) -> return tc
                _                -> do { traceTc "lk1 (loopy)" (ppr name)
                                       ; return tc_tc } }

tcClass :: Name -> TcM (Class, TcKind)
tcClass cls     -- Must be a class
  = do { thing <- tcLookup cls
       ; case thing of
           ATcTyCon tc -> return (aThingErr "tcClass" cls, tyConKind tc)
           AGlobal (ATyCon tc)
             | Just cls <- tyConClass_maybe tc
             -> return (cls, tyConKind tc)
           _ -> wrongThingErr "class" thing cls }


aThingErr :: String -> Name -> b
-- The type checker for types is sometimes called simply to
-- do *kind* checking; and in that case it ignores the type
-- returned. Which is a good thing since it may not be available yet!
aThingErr str x = pprPanic "AThing evaluated unexpectedly" (text str <+> ppr x)

{-
Note [Type-checking inside the knot]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are checking the argument types of a data constructor.  We
must zonk the types before making the DataCon, because once built we
can't change it.  So we must traverse the type.

BUT the parent TyCon is knot-tied, so we can't look at it yet.

So we must be careful not to use "smart constructors" for types that
look at the TyCon or Class involved.

  * Hence the use of mkNakedXXX functions. These do *not* enforce
    the invariants (for example that we use (FunTy s t) rather
    than (TyConApp (->) [s,t])).

  * The zonking functions establish invariants (even zonkTcType, a change from
    previous behaviour). So we must never inspect the result of a
    zonk that might mention a knot-tied TyCon. This is generally OK
    because we zonk *kinds* while kind-checking types. And the TyCons
    in kinds shouldn't be knot-tied, because they come from a previous
    mutually recursive group.

  * TcHsSyn.zonkTcTypeToType also can safely check/establish
    invariants.

This is horribly delicate.  I hate it.  A good example of how
delicate it is can be seen in Trac #7903.

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
unboxed tuple inside a for-all (via CPR analyis; see
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

addTypeCtxt :: LHsType Name -> TcM a -> TcM a
        -- Wrap a context around only if we want to show that contexts.
        -- Omit invisble ones and ones user's won't grok
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

Note [Scope-check inferred kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  data SameKind :: k -> k -> *
  foo :: forall a (b :: Proxy a) (c :: Proxy d). SameKind b c

d has no binding site. So it gets bound implicitly, at the top. The
problem is that d's kind mentions `a`. So it's all ill-scoped.

The way we check for this is to gather all variables *bound* in a
type variable's scope. The type variable's kind should not mention
any of these variables. That is, d's kind can't mention a, b, or c.
We can't just check to make sure that d's kind is in scope, because
we might be about to kindGeneralize.

A little messy, but it works.

Note [Dependent LHsQTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We track (in the renamer) which explicitly bound variables in a
LHsQTyVars are manifestly dependent; only precisely these variables
may be used within the LHsQTyVars. We must do this so that kcHsTyVarBndrs
can produce the right TyConBinders, and tell Anon vs. Named. Earlier,
I thought it would work simply to do a free-variable check during
kcHsTyVarBndrs, but this is bogus, because there may be unsolved
equalities about. And we don't want to eagerly solve the equalities,
because we may get further information after kcHsTyVarBndrs is called.
(Recall that kcHsTyVarBndrs is usually called from getInitialKind.
The only other case is in kcConDecl.) This is what implements the rule
that all variables intended to be dependent must be manifestly so.

Sidenote: It's quite possible that later, we'll consider (t -> s)
as a degenerate case of some (pi (x :: t) -> s) and then this will
all get more permissive.

-}

tcWildCardBinders :: [Name]
                  -> ([(Name, TcTyVar)] -> TcM a)
                  -> TcM a
tcWildCardBinders = tcWildCardBindersX new_tv
  where
    new_tv name = do { kind <- newMetaKindVar
                     ; newSkolemTyVar name kind }

tcWildCardBindersX :: (Name -> TcM TcTyVar)
                   -> [Name]
                   -> ([(Name, TcTyVar)] -> TcM a)
                   -> TcM a
tcWildCardBindersX new_wc wc_names thing_inside
  = do { wcs <- mapM new_wc wc_names
       ; let wc_prs = wc_names `zip` wcs
       ; tcExtendTyVarEnv2 wc_prs $
         thing_inside wc_prs }

-- | Kind-check a 'LHsQTyVars'. If the decl under consideration has a complete,
-- user-supplied kind signature (CUSK), generalise the result.
-- Used in 'getInitialKind' (for tycon kinds and other kinds)
-- and in kind-checking (but not for tycon kinds, which are checked with
-- tcTyClDecls). See also Note [Complete user-supplied kind signatures] in
-- HsDecls.
--
-- This function does not do telescope checking.
kcHsTyVarBndrs :: Name    -- ^ of the thing being checked
               -> Bool    -- ^ True <=> the TyCon being kind-checked can be unsaturated
               -> Bool    -- ^ True <=> the decl being checked has a CUSK
               -> Bool    -- ^ True <=> the decl is an open type/data family
               -> Bool    -- ^ True <=> all the hsq_implicit are *kind* vars
                          -- (will give these kind * if -XNoTypeInType)
               -> LHsQTyVars Name
               -> TcM (Kind, r)     -- ^ The result kind, possibly with other info
               -> TcM (TcTyCon, r)  -- ^ A suitably-kinded TcTyCon
kcHsTyVarBndrs name unsat cusk open_fam all_kind_vars
  (HsQTvs { hsq_implicit = kv_ns, hsq_explicit = hs_tvs
          , hsq_dependent = dep_names }) thing_inside
  | cusk
  = do { kv_kinds <- mk_kv_kinds
       ; lvl <- getTcLevel
       ; let scoped_kvs = zipWith (mk_skolem_tv lvl) kv_ns kv_kinds
       ; tcExtendTyVarEnv2 (kv_ns `zip` scoped_kvs) $
    do { (tc_binders, res_kind, stuff) <- solveEqualities $
                                          bind_telescope hs_tvs thing_inside

           -- Now, because we're in a CUSK, quantify over the mentioned
           -- kind vars, in dependency order.
       ; tc_binders  <- mapM zonkTcTyVarBinder tc_binders
       ; res_kind <- zonkTcType res_kind
       ; let tc_tvs = binderVars tc_binders
             qkvs   = tyCoVarsOfTypeWellScoped (mkTyConKind tc_binders res_kind)
                   -- the visibility of tvs doesn't matter here; we just
                   -- want the free variables not to include the tvs

          -- If there are any meta-tvs left, the user has
          -- lied about having a CUSK. Error.
       ; let (meta_tvs, good_tvs) = partition isMetaTyVar qkvs
       ; when (not (null meta_tvs)) $
         report_non_cusk_tvs (qkvs ++ tc_tvs)

          -- If any of the scoped_kvs aren't actually mentioned in a binder's
          -- kind (or the return kind), then we're in the CUSK case from
          -- Note [Free-floating kind vars]
       ; let all_tc_tvs        = good_tvs ++ tc_tvs
             all_mentioned_tvs = mapUnionVarSet (tyCoVarsOfType . tyVarKind)
                                                all_tc_tvs
                                 `unionVarSet` tyCoVarsOfType res_kind
             unmentioned_kvs   = filterOut (`elemVarSet` all_mentioned_tvs)
                                           scoped_kvs
       ; reportFloatingKvs name all_tc_tvs unmentioned_kvs

       ; let final_binders = map (mkNamedTyConBinder Specified) good_tvs
                            ++ tc_binders
             tycon = mkTcTyCon name final_binders res_kind
                               unsat (scoped_kvs ++ tc_tvs)
                           -- the tvs contain the binders already
                           -- in scope from an enclosing class, but
                           -- re-adding tvs to the env't doesn't cause
                           -- harm
       ; return (tycon, stuff) }}

  | otherwise
  = do { kv_kinds <- mk_kv_kinds
       ; scoped_kvs <- zipWithM newSigTyVar kv_ns kv_kinds
                     -- the names must line up in splitTelescopeTvs
       ; (binders, res_kind, stuff)
           <- tcExtendTyVarEnv2 (kv_ns `zip` scoped_kvs) $
              bind_telescope hs_tvs thing_inside
       ; let   -- NB: Don't add scoped_kvs to tyConTyVars, because they
               -- must remain lined up with the binders
             tycon = mkTcTyCon name binders res_kind unsat
                               (scoped_kvs ++ binderVars binders)
       ; return (tycon, stuff) }
  where
      -- if -XNoTypeInType and we know all the implicits are kind vars,
      -- just give the kind *. This prevents test
      -- dependent/should_fail/KindLevelsB from compiling, as it should
    mk_kv_kinds :: TcM [Kind]
    mk_kv_kinds = do { typeintype <- xoptM LangExt.TypeInType
                     ; if not typeintype && all_kind_vars
                       then return (map (const liftedTypeKind) kv_ns)
                       else mapM (const newMetaKindVar) kv_ns }

      -- there may be dependency between the explicit "ty" vars. So, we have
      -- to handle them one at a time.
    bind_telescope :: [LHsTyVarBndr Name]
                   -> TcM (Kind, r)
                   -> TcM ([TyConBinder], TcKind, r)
    bind_telescope [] thing
      = do { (res_kind, stuff) <- thing
           ; return ([], res_kind, stuff) }
    bind_telescope (L _ hs_tv : hs_tvs) thing
      = do { tv_pair@(tv, _) <- kc_hs_tv hs_tv
               -- NB: Bring all tvs into scope, even non-dependent ones,
               -- as they're needed in type synonyms, data constructors, etc.
           ; (binders, res_kind, stuff) <- bind_unless_scoped tv_pair $
                                           bind_telescope hs_tvs $
                                           thing
                  -- See Note [Dependent LHsQTyVars]
           ; let new_binder | hsTyVarName hs_tv `elemNameSet` dep_names
                            = mkNamedTyConBinder Required tv
                            | otherwise
                            = mkAnonTyConBinder tv
           ; return ( new_binder : binders
                    , res_kind, stuff ) }

    -- | Bind the tyvar in the env't unless the bool is True
    bind_unless_scoped :: (TcTyVar, Bool) -> TcM a -> TcM a
    bind_unless_scoped (_, True)   thing_inside = thing_inside
    bind_unless_scoped (tv, False) thing_inside
      = tcExtendTyVarEnv [tv] thing_inside

    kc_hs_tv :: HsTyVarBndr Name -> TcM (TcTyVar, Bool)
    kc_hs_tv (UserTyVar (L _ name))
      = do { tv_pair@(tv, scoped) <- tcHsTyVarName Nothing name

              -- Open type/data families default their variables to kind *.
           ; when (open_fam && not scoped) $ -- (don't default class tyvars)
             discardResult $ unifyKind (Just (mkTyVarTy tv)) liftedTypeKind
                                                             (tyVarKind tv)

           ; return tv_pair }

    kc_hs_tv (KindedTyVar (L _ name) lhs_kind)
      = do { kind <- tcLHsKind lhs_kind
           ; tcHsTyVarName (Just kind) name }

    report_non_cusk_tvs all_tvs
      = do { all_tvs <- mapM zonkTyCoVarKind all_tvs
           ; let (_, tidy_tvs)         = tidyOpenTyCoVars emptyTidyEnv all_tvs
                 (meta_tvs, other_tvs) = partition isMetaTyVar tidy_tvs

           ; addErr $
             vcat [ text "You have written a *complete user-suppled kind signature*,"
                  , hang (text "but the following variable" <> plural meta_tvs <+>
                          isOrAre meta_tvs <+> text "undetermined:")
                       2 (vcat (map pp_tv meta_tvs))
                  , text "Perhaps add a kind signature."
                  , hang (text "Inferred kinds of user-written variables:")
                       2 (vcat (map pp_tv other_tvs)) ] }
      where
        pp_tv tv = ppr tv <+> dcolon <+> ppr (tyVarKind tv)


tcImplicitTKBndrs :: [Name]
                  -> TcM (a, TyVarSet)   -- vars are bound somewhere in the scope
                                         -- see Note [Scope-check inferred kinds]
                  -> TcM ([TcTyVar], a)
tcImplicitTKBndrs = tcImplicitTKBndrsX (tcHsTyVarName Nothing)

-- | Convenient specialization
tcImplicitTKBndrsType :: [Name]
                      -> TcM Type
                      -> TcM ([TcTyVar], Type)
tcImplicitTKBndrsType var_ns thing_inside
  = tcImplicitTKBndrs var_ns $
    do { res_ty <- thing_inside
       ; return (res_ty, allBoundVariables res_ty) }

-- this more general variant is needed in tcHsPatSigType.
-- See Note [Pattern signature binders]
tcImplicitTKBndrsX :: (Name -> TcM (TcTyVar, Bool))  -- new_tv function
                   -> [Name]
                   -> TcM (a, TyVarSet)
                   -> TcM ([TcTyVar], a)
-- Returned TcTyVars have the supplied Names
--   i.e. no cloning of fresh names
tcImplicitTKBndrsX new_tv var_ns thing_inside
  = do { tkvs_pairs <- mapM new_tv var_ns
       ; let must_scope_tkvs = [ tkv | (tkv, False) <- tkvs_pairs ]
             tkvs            = map fst tkvs_pairs
       ; (result, bound_tvs) <- tcExtendTyVarEnv must_scope_tkvs $
                                thing_inside

         -- it's possible that we guessed the ordering of variables
         -- wrongly. Adjust.
       ; tkvs <- mapM zonkTyCoVarKind tkvs
       ; let extra = text "NB: Implicitly-bound variables always come" <+>
                     text "before other ones."
       ; checkValidInferredKinds tkvs bound_tvs extra

       ; let final_tvs = toposortTyVars tkvs
       ; traceTc "tcImplicitTKBndrs" (ppr var_ns $$ ppr final_tvs)

       ; return (final_tvs, result) }

tcExplicitTKBndrs :: [LHsTyVarBndr Name]
                  -> ([TyVar] -> TcM (a, TyVarSet))
                        -- ^ Thing inside returns the set of variables bound
                        -- in the scope. See Note [Scope-check inferred kinds]
                  -> TcM (a, TyVarSet)  -- ^ returns augmented bound vars
-- No cloning: returned TyVars have the same Name as the incoming LHsTyVarBndrs
tcExplicitTKBndrs orig_hs_tvs thing_inside
  = tcExplicitTKBndrsX newSkolemTyVar orig_hs_tvs thing_inside

tcExplicitTKBndrsX :: (Name -> Kind -> TcM TyVar)
                   -> [LHsTyVarBndr Name]
                   -> ([TyVar] -> TcM (a, TyVarSet))
                        -- ^ Thing inside returns the set of variables bound
                        -- in the scope. See Note [Scope-check inferred kinds]
                   -> TcM (a, TyVarSet)  -- ^ returns augmented bound vars
tcExplicitTKBndrsX new_tv orig_hs_tvs thing_inside
  = go orig_hs_tvs $ \ tvs ->
    do { (result, bound_tvs) <- thing_inside tvs

         -- Issue an error if the ordering is bogus.
         -- See Note [Bad telescopes] in TcValidity.
       ; tvs <- checkZonkValidTelescope (interppSP orig_hs_tvs) tvs empty
       ; checkValidInferredKinds tvs bound_tvs empty

       ; traceTc "tcExplicitTKBndrs" $
           vcat [ text "Hs vars:" <+> ppr orig_hs_tvs
                , text "tvs:" <+> sep (map pprTyVar tvs) ]

       ; return (result, bound_tvs `unionVarSet` mkVarSet tvs)
       }
  where
    go [] thing = thing []
    go (L _ hs_tv : hs_tvs) thing
      = do { tv <- tcHsTyVarBndr new_tv hs_tv
           ; tcExtendTyVarEnv [tv] $
             go hs_tvs $ \ tvs ->
             thing (tv : tvs) }

tcHsTyVarBndr :: (Name -> Kind -> TcM TyVar)
              -> HsTyVarBndr Name -> TcM TcTyVar
-- Return a SkolemTv TcTyVar, initialised with a kind variable.
-- Typically the Kind inside the HsTyVarBndr will be a tyvar
-- with a mutable kind in it.
-- NB: These variables must not be in scope. This function
-- is not appropriate for use with associated types, for example.
--
-- Returned TcTyVar has the same name; no cloning
--
-- See also Note [Associated type tyvar names] in Class
--
tcHsTyVarBndr new_tv (UserTyVar (L _ name))
  = do { kind <- newMetaKindVar
       ; new_tv name kind }

tcHsTyVarBndr new_tv (KindedTyVar (L _ name) kind)
  = do { kind <- tcLHsKind kind
       ; new_tv name kind }

newWildTyVar :: Name -> TcM TcTyVar
-- ^ New unification variable for a wildcard
newWildTyVar _name
  = do { kind <- newMetaKindVar
       ; uniq <- newUnique
       ; details <- newMetaDetails TauTv
       ; let name = mkSysTvName uniq (fsLit "w")
       ; return (mkTcTyVar name kind details) }

-- | Produce a tyvar of the given name (with the kind provided, or
-- otherwise a meta-var kind). If
-- the name is already in scope, return the scoped variable, checking
-- to make sure the known kind matches any kind provided. The
-- second return value says whether the variable is in scope (True)
-- or not (False). (Use this for associated types, for example.)
tcHsTyVarName :: Maybe Kind -> Name -> TcM (TcTyVar, Bool)
tcHsTyVarName m_kind name
  = do { mb_tv <- tcLookupLcl_maybe name
       ; case mb_tv of
           Just (ATyVar _ tv)
             -> do { whenIsJust m_kind $ \ kind ->
                     discardResult $
                     unifyKind (Just (mkTyVarTy tv)) kind (tyVarKind tv)
                   ; return (tv, True) }
           _ -> do { kind <- case m_kind of
                               Just kind -> return kind
                               Nothing   -> newMetaKindVar
                   ; tv <- newSkolemTyVar name kind
                   ; return (tv, False) }}

-- makes a new skolem tv
newSkolemTyVar :: Name -> Kind -> TcM TcTyVar
newSkolemTyVar name kind = do { lvl <- getTcLevel
                              ; return (mk_skolem_tv lvl name kind) }

mk_skolem_tv :: TcLevel -> Name -> Kind -> TcTyVar
mk_skolem_tv lvl n k = mkTcTyVar n k (SkolemTv lvl False)

------------------
kindGeneralizeType :: Type -> TcM Type
-- Result is zonked
kindGeneralizeType ty
  = do { kvs <- kindGeneralize ty
       ; ty <- zonkSigType (mkInvForAllTys kvs ty)
       ; return ty  }

kindGeneralize :: TcType -> TcM [KindVar]
-- Quantify the free kind variables of a kind or type
-- In the latter case the type is closed, so it has no free
-- type variables.  So in both cases, all the free vars are kind vars
kindGeneralize kind_or_type
  = do { kvs <- zonkTcTypeAndFV kind_or_type
       ; let dvs = DV { dv_kvs = kvs, dv_tvs = emptyDVarSet }
       ; gbl_tvs <- tcGetGlobalTyCoVars -- Already zonked
       ; quantifyZonkedTyVars gbl_tvs dvs }

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

This is not an existential datatype, but a higher-rank one. Note that
the forall to the right of MkT. Also consider

  data S a = MkS (Proxy (a :: k))

According to the rules around implicitly-bound kind variables, those
k's scope over the whole declarations. The renamer grabs it and adds it
to the hsq_implicits field of the HsQTyVars of the tycon. So it must
be in scope during type-checking, but we want to reject T while accepting
S.

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
in kcHsTyVarBndrs, we check to make sure all implicitly-bound kind
vars are indeed mentioned in a kind somewhere. If not, error.

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
  = do { tycon <- kcLookupTcTyCon tycon_name
       ; tcExtendTyVarEnv (tcTyConScopedTyVars tycon) $ thing_inside }

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

       ; let scoped_tvs = tcTyConScopedTyVars tycon
               -- these are all zonked:
             binders    = tyConBinders tycon
             res_kind   = tyConResKind tycon

          -- See Note [Free-floating kind vars]
       ; zonked_scoped_tvs <- mapM zonkTcTyVarToTyVar scoped_tvs
       ; let still_sig_tvs = filter isSigTyVar zonked_scoped_tvs
       ; checkNoErrs $ reportFloatingKvs tycon_name
                                         zonked_scoped_tvs still_sig_tvs

          -- Add the *unzonked* tyvars to the env't, because those
          -- are the ones mentioned in the source.
       ; tcExtendTyVarEnv scoped_tvs $
         thing_inside binders res_kind }

-----------------------------------
tcDataKindSig :: Kind -> TcM ([TyConBinder], Kind)
-- GADT decls can have a (perhaps partial) kind signature
--      e.g.  data T :: * -> * -> * where ...
-- This function makes up suitable (kinded) type variables for
-- the argument kinds, and checks that the result kind is indeed *.
-- We use it also to make up argument type variables for for data instances.
-- Never emits constraints.
-- Returns the new TyVars, the extracted TyBinders, and the new, reduced
-- result kind (which should always be Type or a synonym thereof)
tcDataKindSig kind
  = do  { checkTc (isLiftedTypeKind res_kind) (badKindSig kind)
        ; span <- getSrcSpanM
        ; us   <- newUniqueSupply
        ; rdr_env <- getLocalRdrEnv
        ; let uniqs = uniqsFromSupply us
              occs  = [ occ | str <- allNameStrings
                            , let occ = mkOccName tvName str
                            , isNothing (lookupLocalRdrOcc rdr_env occ) ]
                 -- Note [Avoid name clashes for associated data types]

    -- NB: Use the tv from a binder if there is one. Otherwise,
    -- we end up inventing a new Unique for it, and any other tv
    -- that mentions the first ends up with the wrong kind.
              extra_bndrs = zipWith4 mkTyBinderTyConBinder
                              tv_bndrs (repeat span) uniqs occs

        ; return (extra_bndrs, res_kind) }
  where
    (tv_bndrs, res_kind) = splitPiTys kind

badKindSig :: Kind -> SDoc
badKindSig kind
 = hang (text "Kind signature on data type declaration has non-* return kind")
        2 (ppr kind)

{-
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
             Partial signatures and pattern signatures
*                                                                      *
************************************************************************


Note [Solving equalities in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat a partial type signature as a "shape constraint" to impose on
the term:
  * We make no attempt to kind-generalise it
  * We instantiate the explicit and implicit foralls with SigTvs
  * We instantiate the wildcards with meta tyvars

We /do/ call solveEqualities, and then zonk to propage the result of
solveEqualities, mainly so that functions like matchExpectedFunTys will
be able to decompose the type, and hence higher-rank signatures will
work. Ugh!  For example
   f :: (forall a. a->a) -> _
   f x = (x True, x 'c')

-}

tcHsPartialSigType
  :: UserTypeCtxt
  -> LHsSigWcType Name        -- The type signature
  -> TcM ( [(Name, TcTyVar)]  -- Wildcards
         , Maybe TcTyVar      -- Extra-constraints wildcard
         , [TcTyVar]          -- Implicitly and explicitly bound type variables
         , TcThetaType        -- Theta part
         , TcType )           -- Tau part
tcHsPartialSigType ctxt sig_ty
  | HsWC { hswc_wcs  = sig_wcs,         hswc_body = ib_ty } <- sig_ty
  , HsIB { hsib_vars = implicit_hs_tvs, hsib_body = hs_ty } <- ib_ty
  , (explicit_hs_tvs, L _ hs_ctxt, hs_tau) <- splitLHsSigmaTy hs_ty
  = addSigCtxt ctxt hs_ty $
    do { (implicit_tvs, (wcs, wcx, explicit_tvs, theta, tau))
            <- -- See Note [Solving equalities in partial type signatures]
               solveEqualities $
               tcWildCardBindersX newWildTyVar sig_wcs        $ \ wcs ->
               tcImplicitTKBndrsX new_implicit_tv implicit_hs_tvs $
               tcExplicitTKBndrsX newSigTyVar explicit_hs_tvs $ \ explicit_tvs ->
               do {   -- Instantiate the type-class context; but if there
                      -- is an extra-constraints wildcard, just discard it here
                    (theta, wcx) <- tcPartialContext hs_ctxt

                  ; tau <- tcHsOpenType hs_tau

                  ; let bound_tvs = unionVarSets [ allBoundVariables tau
                                                 , mkVarSet explicit_tvs
                                                 , mkVarSet (map snd wcs) ]

                  ; return ( (wcs, wcx, explicit_tvs, theta, tau)
                           , bound_tvs) }

        ; emitWildCardHoleConstraints wcs

        -- See Note [Solving equalities in partial type signatures]
        ; all_tvs <- mapM (updateTyVarKindM zonkTcType)
                          (implicit_tvs ++ explicit_tvs)
        ; theta   <- mapM zonkTcType theta
        ; tau     <- zonkTcType tau
        ; checkValidType ctxt (mkSpecForAllTys all_tvs $ mkPhiTy theta tau)

        ; traceTc "tcHsPartialSigType" (ppr all_tvs)
        ; return (wcs, wcx, all_tvs, theta, tau) }
  where
    new_implicit_tv name = do { kind <- newMetaKindVar
                              ; tv <- newSigTyVar name kind
                              ; return (tv, False) }

tcPartialContext :: HsContext Name -> TcM (TcThetaType, Maybe TcTyVar)
tcPartialContext hs_theta
  | Just (hs_theta1, hs_ctxt_last) <- snocView hs_theta
  , L _ (HsWildCardTy wc) <- ignoreParens hs_ctxt_last
  = do { wc_tv <- tcWildCardOcc wc constraintKind
       ; theta <- mapM tcLHsPredType hs_theta1
       ; return (theta, Just wc_tv) }
  | otherwise
  = do { theta <- mapM tcLHsPredType hs_theta
       ; return (theta, Nothing) }

tcHsPatSigType :: UserTypeCtxt
               -> LHsSigWcType Name           -- The type signature
               -> TcM ( [(Name, TcTyVar)]     -- Wildcards
                      , [TcTyVar]     -- The new bit of type environment, binding
                                      -- the scoped type variables
                      , TcType)       -- The type
-- Used for type-checking type signatures in
-- (a) patterns           e.g  f (x::Int) = e
-- (b) RULE forall bndrs  e.g. forall (x::Int). f x = x
--
-- This may emit constraints

tcHsPatSigType ctxt sig_ty
  | HsWC { hswc_wcs = sig_wcs,   hswc_body = ib_ty } <- sig_ty
  , HsIB { hsib_vars = sig_vars, hsib_body = hs_ty } <- ib_ty
  = addSigCtxt ctxt hs_ty $
    do { (implicit_tvs, (wcs, sig_ty))
            <- -- See Note [Solving equalities in partial type signatures]
               solveEqualities $
               tcWildCardBindersX newWildTyVar    sig_wcs  $ \ wcs ->
               tcImplicitTKBndrsX new_implicit_tv sig_vars $
               do { sig_ty <- tcHsOpenType hs_ty
                  ; return ((wcs, sig_ty), allBoundVariables sig_ty) }

        ; emitWildCardHoleConstraints wcs

        ; sig_ty <- zonkTcType sig_ty
        ; checkValidType ctxt sig_ty

        ; traceTc "tcHsPatSigType" (ppr sig_vars)
        ; return (wcs, implicit_tvs, sig_ty) }
  where
    new_implicit_tv name = do { kind <- newMetaKindVar
                              ; tv <- new_tv name kind
                              ; return (tv, False) }
       -- "False" means that these tyvars aren't yet in scope
    new_tv = case ctxt of
               RuleSigCtxt {} -> newSkolemTyVar
               _              -> newSigTyVar
      -- See Note [Pattern signature binders]
      -- See Note [Unifying SigTvs]


tcPatSig :: Bool                    -- True <=> pattern binding
         -> LHsSigWcType Name
         -> ExpSigmaType
         -> TcM (TcType,            -- The type to use for "inside" the signature
                 [TcTyVar],         -- The new bit of type environment, binding
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
        ; let bad_tvs = [ tv | tv <- sig_tvs
                             , not (tv `elemVarSet` exactTyCoVarsOfType sig_ty) ]
        ; checkTc (null bad_tvs) (badPatSigTvs sig_ty bad_tvs)

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

patBindSigErr :: [TcTyVar] -> SDoc
patBindSigErr sig_tvs
  = hang (text "You cannot bind scoped type variable" <> plural sig_tvs
          <+> pprQuotedList sig_tvs)
       2 (text "in a pattern binding signature")

{-
Note [Pattern signature binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data T = forall a. T a (a->Int)
   f (T x (f :: a->Int) = blah)

Here
 * The pattern (T p1 p2) creates a *skolem* type variable 'a_sk',
   It must be a skolem so that that it retains its identity, and
   TcErrors.getSkolemInfo can thereby find the binding site for the skolem.

 * The type signature pattern (f :: a->Int) binds "a" -> a_sig in the envt

 * Then unification makes a_sig := a_sk

That's why we must make a_sig a MetaTv (albeit a SigTv),
not a SkolemTv, so that it can unify to a_sk.

For RULE binders, though, things are a bit different (yuk).
  RULE "foo" forall (x::a) (y::[a]).  f x y = ...
Here this really is the binding site of the type variable so we'd like
to use a skolem, so that we get a complaint if we unify two of them
together.

Note [Unifying SigTvs]
~~~~~~~~~~~~~~~~~~~~~~
ALAS we have no decent way of avoiding two SigTvs getting unified.
Consider
  f (x::(a,b)) (y::c)) = [fst x, y]
Here we'd really like to complain that 'a' and 'c' are unified. But
for the reasons above we can't make a,b,c into skolems, so they
are just SigTvs that can unify.  And indeed, this would be ok,
  f x (y::c) = case x of
                 (x1 :: a1, True) -> [x,y]
                 (x1 :: a2, False) -> [x,y,y]
Here the type of x's first component is called 'a1' in one branch and
'a2' in the other.  We could try insisting on the same OccName, but
they definitely won't have the sane lexical Name.

I think we could solve this by recording in a SigTv a list of all the
in-scope variables that it should not unify with, but it's fiddly.


************************************************************************
*                                                                      *
        Checking kinds
*                                                                      *
************************************************************************

-}

unifyKinds :: [(TcType, TcKind)] -> TcM ([TcType], TcKind)
unifyKinds act_kinds
  = do { kind <- newMetaKindVar
       ; let check (ty, act_kind) = checkExpectedKind ty act_kind kind
       ; tys' <- mapM check act_kinds
       ; return (tys', kind) }

{-
************************************************************************
*                                                                      *
        Sort checking kinds
*                                                                      *
************************************************************************

tcLHsKind converts a user-written kind to an internal, sort-checked kind.
It does sort checking and desugaring at the same time, in one single pass.
-}

tcLHsKind :: LHsKind Name -> TcM Kind
tcLHsKind = tc_lhs_kind kindLevelMode

tc_lhs_kind :: TcTyMode -> LHsKind Name -> TcM Kind
tc_lhs_kind mode k
  = addErrCtxt (text "In the kind" <+> quotes (ppr k)) $
    tc_lhs_type (kindLevel mode) k liftedTypeKind

promotionErr :: Name -> PromotionErr -> TcM a
promotionErr name err
  = failWithTc (hang (pprPECategory err <+> quotes (ppr name) <+> text "cannot be used here")
                   2 (parens reason))
  where
    reason = case err of
               FamDataConPE   -> text "it comes from a data family instance"
               NoDataKindsTC  -> text "Perhaps you intended to use DataKinds"
               NoDataKindsDC  -> text "Perhaps you intended to use DataKinds"
               NoTypeInTypeTC -> text "Perhaps you intended to use TypeInType"
               NoTypeInTypeDC -> text "Perhaps you intended to use TypeInType"
               PatSynPE       -> text "Pattern synonyms cannot be promoted"
               _ -> text "it is defined and used in the same recursive group"

{-
************************************************************************
*                                                                      *
                Scoped type variables
*                                                                      *
************************************************************************
-}

badPatSigTvs :: TcType -> [TyVar] -> SDoc
badPatSigTvs sig_ty bad_tvs
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
reportFloatingKvs :: Name        -- of the tycon
                  -> [TcTyVar]   -- all tyvars, not necessarily zonked
                  -> [TcTyVar]   -- floating tyvars
                  -> TcM ()
reportFloatingKvs tycon_name all_tvs bad_tvs
  = unless (null bad_tvs) $  -- don't bother zonking if there's no error
    do { all_tvs <- mapM zonkTcTyVarToTyVar all_tvs
       ; bad_tvs <- mapM zonkTcTyVarToTyVar bad_tvs
       ; let (tidy_env, tidy_all_tvs) = tidyOpenTyCoVars emptyTidyEnv all_tvs
             tidy_bad_tvs             = map (tidyTyVarOcc tidy_env) bad_tvs
       ; typeintype <- xoptM LangExt.TypeInType
       ; mapM_ (report typeintype tidy_all_tvs) tidy_bad_tvs }
  where
    report typeintype tidy_all_tvs tidy_bad_tv
      = addErr $
        vcat [ text "Kind variable" <+> quotes (ppr tidy_bad_tv) <+>
               text "is implicitly bound in datatype"
             , quotes (ppr tycon_name) <> comma <+>
               text "but does not appear as the kind of any"
             , text "of its type variables. Perhaps you meant"
             , text "to bind it" <+> ppWhen (not typeintype)
                                            (text "(with TypeInType)") <+>
                                 text "explicitly somewhere?"
             , ppWhen (not (null tidy_all_tvs)) $
                 hang (text "Type variables with inferred kinds:")
                 2 (ppr_tv_bndrs tidy_all_tvs) ]

    ppr_tv_bndrs tvs = sep (map pp_tv tvs)
    pp_tv tv         = parens (ppr tv <+> dcolon <+> ppr (tyVarKind tv))
