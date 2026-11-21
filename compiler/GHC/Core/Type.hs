-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998
--
-- Type - public interface

{-# LANGUAGE PatternSynonyms, ViewPatterns, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Main functions for manipulating types and type-related things
module GHC.Core.Type (
        -- Note some of this is just re-exports from TyCon..

        -- * Main data types representing Types
        -- $type_classification

        -- $representation_types
        Type, ForAllTyFlag(..), FunTyFlag(..),
        Specificity(..),
        KindOrType, PredType, ThetaType, FRRType,
        Var, TyVar, isTyVar, TyCoVar, PiTyBinder, ForAllTyBinder, TyVarBinder,
        Mult, Scaled,
        KnotTied, RuntimeRepType,

        -- ** Constructing and deconstructing types
        mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, repGetTyVar_maybe,
        getCastedTyVar_maybe, tyVarKind, varType,

        mkAppTy, mkAppTys, splitAppTy, splitAppTys, splitAppTysNoView,
        splitAppTy_maybe, splitAppTyNoView_maybe, tcSplitAppTyNoView_maybe,

        mkFunTy, mkVisFunTy,
        mkVisFunTyMany, mkVisFunTysMany,
        mkVisFunTyManyM, mkVisFunTyManyU, mkVisFunTysManyM, mkVisFunTysManyU,
        mkScaledFunTys,
        mkScaledFunTysM, mkScaledFunTysU,
        mkInvisFunTy, mkInvisFunTys,
        tcMkVisFunTy, tcMkScaledFunTys, tcMkInvisFunTy,
        tcMkScaledFunTysM, tcMkScaledFunTysU,
        splitFunTy, splitFunTy_maybe, splitVisibleFunTy_maybe,
        splitFunTys, funResultTy, funArgTy,
        funTyConAppTy_maybe, funTyFlagTyCon,
        tyConAppFunTy_maybe, tyConAppFunCo_maybe,
        mkFunctionType, mkScaledFunctionTys, mkScaledFunctionTysM, mkScaledFunctionTysU, chooseFunTyFlag,

        mkTyConApp, mkTyConTy,
        tyConAppTyCon_maybe, tyConAppTyConPicky_maybe,
        tyConAppArgs_maybe, tyConAppTyCon, tyConAppArgs,

        splitTyConApp_maybe, splitTyConAppNoView_maybe, splitTyConApp,
        tcSplitTyConApp, tcSplitTyConApp_maybe,

        mkForAllTy, mkForAllTys, mkInvisForAllTys, mkTyCoInvForAllTys,
        mkSpecForAllTy, mkSpecForAllTys,
        mkVisForAllTys, mkTyCoForAllTy, mkTyCoForAllTys, mkTyCoInvForAllTy,
        mkInfForAllTy, mkInfForAllTys,
        splitForAllTyCoVars, splitForAllTyVars,
        splitForAllReqTyBinders, splitForAllInvisTyBinders,
        splitForAllForAllTyBinders, splitForAllForAllTyBinder_maybe,
        splitForAllTyCoVar_maybe, splitForAllTyCoVar,
        splitForAllTyVar_maybe, splitForAllCoVar_maybe,
        splitPiTy_maybe, splitPiTy, splitPiTys, splitUninstantiatedValArgTys,
        getRuntimeArgTys,
        mkTyConBindersPreferAnon,
        mkPiTy, mkPiTys, DepFunTyFlag(..), mkDepFunTy, mkDepFunTyWithVis, isBoolIndexId,
        mkPiTysM, mkPiTysU,
        isNativeIndexId, isNativeIndexType, isNativeClosedIndexType, nativeIndexDataConTyArgs,
        isIntPrimFamilyPatBinder,
        isNativeIndexFunctionId, nativeIndexFunctionTys_maybe,
        nativeIndexFunctionTypeTys_maybe,
        isNativeOpaqueValueId, isNativeOpaqueValueType, isNativeClassDictionaryId,
        isNativeIndexDomainConversion,
        isNativeRetainedBinder, isNativeRetainedBinderType,
        isNativeRetainedField, isNativeRetainedFieldType,
        piResultTy, piResultTys,
        applyTysX, dropForAlls,
        mkFamilyTyConApp,
        buildSynTyCon,

        mkNumLitTy, isNumLitTy,
        mkStrLitTy, isStrLitTy,
        mkCharLitTy, isCharLitTy,
        isLitTy,

        getRuntimeRep, splitRuntimeRep_maybe, kindRep_maybe, kindRep,
        getLevity, levityType_maybe,

        mkCastTy, mkCoercionTy, splitCastTy_maybe,

        ErrorMsgType, pprUserTypeErrorTy,

        coAxNthLHS,
        stripCoercionTy,

        splitInvisPiTys, splitInvisPiTysN, invisibleBndrCount,
        filterOutInvisibleTypes, filterOutInferredTypes,
        partitionInvisibleTypes, partitionInvisibles,
        tyConForAllTyFlags, appTyForAllTyFlags,

        -- ** Analyzing types
        TyCoMapper(..), mapTyCo,
        TyCoFolder(..), foldTyCo, noView,

        -- (Newtypes)
        newTyConInstRhs,

        -- ** Binders
        mkForAllTyBinder, mkForAllTyBinders,
        mkTyVarBinder, mkTyVarBinders,
        tyVarSpecToBinders,
        isAnonPiTyBinder,
        binderVar, binderVars, binderType, binderFlag, binderFlags,
        piTyBinderType, namedPiTyBinder_maybe,
        anonPiTyBinderType_maybe,
        isVisibleForAllTyFlag, isInvisibleForAllTyFlag, isVisiblePiTyBinder,
        isInvisiblePiTyBinder, isNamedPiTyBinder,
        tyConBindersPiTyBinders,

        -- ** Predicates on types
        isTyVarTy, isFunTy, isCoercionTy,
        isCoercionTy_maybe, isForAllTy,
        isForAllTy_ty, isForAllTy_co,
        isForAllTy_invis_ty,
        isPiTy, isTauTy, isFamFreeTy, isUnsaturatedUserFamily,
        isAtomicTy,

        isValidJoinPointType, joinPointResultType,
        tyConAppNeedsKindSig,

        -- * Space-saving construction
        mkTYPEapp, mkTYPEapp_maybe,
        mkCONSTRAINTapp, mkCONSTRAINTapp_maybe,
        mkBoxedRepApp_maybe, mkTupleRepApp_maybe,
        typeOrConstraintKind, liftedTypeOrConstraintKind,

        -- *** Levity and boxity
        sORTKind_maybe, typeTypeOrConstraint,
        typeLevity, typeLevity_maybe, tyConIsTYPEorCONSTRAINT,
        isLiftedTypeKind, isUnliftedTypeKind, pickyIsLiftedTypeKind,
        isLiftedRuntimeRep, isUnliftedRuntimeRep, runtimeRepLevity_maybe,
        isBoxedRuntimeRep,
        isLiftedLevity, isUnliftedLevity,
        isUnliftedType, isBoxedType, isUnboxedTupleType, isUnboxedSumType,
        kindBoxedRepLevity_maybe,
        mightBeLiftedType, mightBeUnliftedType,
        definitelyLiftedType, definitelyUnliftedType,
        isAlgType, isDataFamilyApp, isSatTyFamApp,
        isPrimitiveType, isStrictType, isTerminatingType,
        isLevityTy, isLevityVar,
        isRuntimeRepTy, isRuntimeRepVar, isRuntimeRepKindedTy,
        dropRuntimeRepArgs,

        -- * Multiplicity

        isMultiplicityTy, isMultiplicityVar,
        Matchability, isMatchableFunTy, defaultForAllMatchability,
        unrestricted, linear, tymult,
        mkScaled, irrelevantMult, scaledSet,
        pattern OneTy, pattern ManyTy,
        isOneTy, isManyTy,
        isLinearType,

        -- * Main data types representing Kinds
        Kind,

        -- ** Finding the kind of a type
        typeKind, typeHasFixedRuntimeRep,
        tcIsLiftedTypeKind,
        isConstraintKind, isConstraintLikeKind, returnsConstraintKind,
        tcIsBoxedTypeKind, isTypeLikeKind,

        -- ** Common Kind
        liftedTypeKind, unliftedTypeKind,

        -- * Type free variables
        tyCoVarsOfType, tyCoVarsOfTypes, tyCoVarsOfTypeDSet,
        coVarsOfType, coVarsOfTypes,

        anyFreeVarsOfType, anyFreeVarsOfTypes,
        noFreeVarsOfType,
        expandTypeSynonyms, expandSynTyConApp_maybe,
        typeSize, occCheckExpand,

        -- ** Closing over kinds
        closeOverKinds,

        -- * Forcing evaluation of types
        seqType, seqTypes, seqCo, seqCos, seqMCo,

        -- * Other views onto Types
        coreView, coreFullView, rewriterView, synAppCanReduce,

        tyConsOfType,

        -- * Main type substitution data types
        TvSubstEnv,     -- Representation widely visible
        IdSubstEnv,
        Subst(..),    -- Representation visible to a few friends

        -- ** Manipulating type substitutions
        emptyTvSubstEnv, emptySubst, mkEmptySubst,

        mkTCvSubst, zipTvSubst, mkTvSubstPrs,
        zipTCvSubst,
        notElemSubst,
        getTvSubstEnv,
        zapSubst, substInScopeSet, setInScope, getSubstRangeTyCoFVs,
        extendSubstInScope, extendSubstInScopeList, extendSubstInScopeSet,
        extendTCvSubst, extendCvSubst,
        extendTvSubst, extendTvSubstList, extendTvSubstAndInScope,
        extendTCvSubstList,
        extendTvSubstWithClone,
        extendTCvSubstWithClone,
        isInScope, composeTCvSubst, zipTyEnv, zipCoEnv,
        isEmptySubst, unionSubst, isEmptyTCvSubst,

        -- ** Performing substitution on types and kinds
        substTy, substTys, substScaledTy, substScaledTys, substTyWith, substTysWith, substTheta,
        substTyAddInScope,
        substTyUnchecked, substTysUnchecked, substScaledTyUnchecked, substScaledTysUnchecked,
        substThetaUnchecked, substTyWithUnchecked,
        substCo, substCoWithInScope,
        substTyVarBndr, substTyVarBndrs, substTyVar, substTyVars,
        substVarBndr, substVarBndrs,
        substTyCoBndr, substTyVarToTyVar,
        cloneTyVarBndr, cloneTyVarBndrs, lookupTyVar,

        -- * Kinds
        isTYPEorCONSTRAINT,
        isConcreteType,
        isFixedRuntimeRepKind
    ) where

import GHC.Prelude

import GHC.Types.Basic

-- We import the representation and primitive functions from GHC.Core.TyCo.Rep.
-- Many things are reexported, but not the representation!

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Make
import GHC.Core.TyCo.Subst
import GHC.Core.TyCo.FVs
import {-# SOURCE #-} GHC.Core.TyCo.Compare ( eqType, eqTypes )
import GHC.Core.TyCon
import {-# SOURCE #-} GHC.Core.DataCon
   ( DataCon, dataConTyCon, dataConFullSig, dataConRepArgTys, dataConStupidTheta
   , dataConWrapId_maybe, dataConRepArgBinders, dataConHasPromotedRetained )
import GHC.Core.Coercion.Axiom

import {-# SOURCE #-} GHC.Core.Coercion
   ( isReflexiveCo
   , coercionRKind, coercionType
   , topNormaliseNewType_maybe
   )

-- friends:
import GHC.Types.Var
import {-# SOURCE #-} GHC.Types.Id.Info (isJoinIdDetails)
import GHC.Types.Var.Env
import GHC.Types.Var.Set

import GHC.Builtin.WiredIn.Prim

import {-# SOURCE #-} GHC.Builtin.WiredIn.Types
   ( charTy, naturalTy
   , typeSymbolKind, liftedTypeKind, unliftedTypeKind
   , manyDataConTy, oneDataConTy, matchableDataConTy, unmatchableDataConTy
   , liftedRepTy )

import GHC.Types.Name( Name, hasKnownKey )
import GHC.Builtin.KnownKeys

import {-# SOURCE #-} GHC.Tc.Utils.TcType ( isConcreteTyVar )

-- others
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Monad.StrictReader

import GHC.Data.FastString
import GHC.Data.Maybe   ( orElse, isJust )

-- $type_classification
-- #type_classification#
--
-- Types are any, but at least one, of:
--
-- [Boxed]              Iff its representation is a pointer to an object on the
--                      GC'd heap. Operationally, heap objects can be entered as
--                      a means of evaluation.
--
-- [Lifted]             Iff it has bottom as an element: An instance of a
--                      lifted type might diverge when evaluated.
--                      GHC Haskell's unboxed types are unlifted.
--                      An unboxed, but lifted type is not very useful.
--                      (Example: A byte-represented type, where evaluating 0xff
--                      computes the 12345678th collatz number modulo 0xff.)
--                      Only lifted types may be unified with a type variable.
--
-- [Algebraic]          Iff it is a type with one or more constructors, whether
--                      declared with @data@ or @newtype@.
--                      An algebraic type is one that can be deconstructed
--                      with a case expression. There are algebraic types that
--                      are not lifted types, like unlifted data types or
--                      unboxed tuples.
--
-- [Data]               Iff it is a type declared with @data@, or a boxed tuple.
--                      There are also /unlifted/ data types.
--
-- [Primitive]          Iff it is a built-in type that can't be expressed in Haskell.
--
-- [Unlifted]           Anything that isn't lifted is considered unlifted.
--
-- Currently, all primitive types are unlifted, but that's not necessarily
-- the case: for example, @Int@ could be primitive.
--
-- Some primitive types are unboxed, such as @Int#@, whereas some are boxed
-- but unlifted (such as @ByteArray#@).  The only primitive types that we
-- classify as algebraic are the unboxed tuples.
--
-- Some examples of type classifications that may make this a bit clearer are:
--
-- @
-- Type          primitive       boxed           lifted          algebraic
-- -----------------------------------------------------------------------------
-- Int#          Yes             No              No              No
-- ByteArray#    Yes             Yes             No              No
-- (\# a, b \#)  Yes             No              No              Yes
-- (\# a | b \#) Yes             No              No              Yes
-- (  a, b  )    No              Yes             Yes             Yes
-- [a]           No              Yes             Yes             Yes
-- @

-- $representation_types
-- A /source type/ is a type that is a separate type as far as the type checker is
-- concerned, but which has a more low-level representation as far as Core-to-Core
-- passes and the rest of the back end is concerned.
--
-- You don't normally have to worry about this, as the utility functions in
-- this module will automatically convert a source into a representation type
-- if they are spotted, to the best of its abilities. If you don't want this
-- to happen, use the equivalent functions from the "TcType" module.

{-
************************************************************************
*                                                                      *
                Type representation
*                                                                      *
************************************************************************
-}

rewriterView :: Type -> Maybe Type
-- Unwrap a type synonym only when either:
--   The type synonym is forgetful, or
--   the type synonym mentions a type family in its expansion
-- See Note [Rewriting synonyms]
{-# INLINE rewriterView #-}
rewriterView (TyConApp tc tys)
  | isTypeSynonymTyCon tc
  , isForgetfulSynTyCon tc || synAppCanReduce tc tys
  = expandSynTyConApp_maybe tc tys
rewriterView _other
  = Nothing

-- | Can expanding a type synonym expose a saturated type family?
-- See Note [Rewriting synonyms] in GHC.Tc.Solver.Rewrite.
synAppCanReduce :: TyCon -> [Type] -> Bool
synAppCanReduce tc args =
  not (isFamFreeTyCon tc) || case splitAt (tyConArity tc) args of
    (_, []) -> False
    (prefix, extra) -> appliesFamily (piResultTys (tyConKind tc) prefix) extra
  where
    appliesFamily _ [] = False
    appliesFamily kind (arg : rest) =
      not (isMatchableFunTy kind) || appliesFamily (piResultTy kind arg) rest

coreView :: Type -> Maybe Type
-- ^ This function strips off the /top layer only/ of a type synonym
-- application (if any) its underlying representation type.
-- Returns 'Nothing' if there is nothing to look through.
--
-- This function does not look through type family applications.
--
-- By being non-recursive and inlined, this case analysis gets efficiently
-- joined onto the case analysis that the caller is already doing
coreView (TyConApp tc tys) = expandSynTyConApp_maybe tc tys
coreView _                 = Nothing
-- See Note [Inlining coreView].
{-# INLINE coreView #-}

coreFullView, core_full_view :: Type -> Type
-- ^ Iterates 'coreView' until there is no more to synonym to expand.
-- NB: coreFullView is non-recursive and can be inlined;
--     core_full_view is the recursive one
-- See Note [Inlining coreView].
coreFullView ty@(TyConApp tc _)
  | isTypeSynonymTyCon tc = core_full_view ty
coreFullView ty = ty
{-# INLINE coreFullView #-}

core_full_view ty
  | Just ty' <- coreView ty = core_full_view ty'
  | otherwise               = ty

-----------------------------------------------
-- | @expandSynTyConApp_maybe tc tys@ expands the RHS of type synonym @tc@
-- instantiated at arguments @tys@, or returns 'Nothing' if @tc@ is not a
-- synonym.
expandSynTyConApp_maybe :: TyCon -> [Type] -> Maybe Type
{-# INLINE expandSynTyConApp_maybe #-}
-- This INLINE will inline the call to expandSynTyConApp_maybe in coreView,
-- which will eliminate the allocation Just/Nothing in the result
-- Don't be tempted to make `expand_syn` (which is NOINLINE) return the
-- Just/Nothing, else you'll increase allocation
expandSynTyConApp_maybe tc arg_tys
  | Just (tvs, rhs) <- synTyConDefn_maybe tc
  , arg_tys `saturates` tyConArity tc
  = Just $! (expand_syn tvs rhs arg_tys)
    -- Why strict application? Because every client of this function will evaluat
    -- that (expand_syn ...) thunk, so it's more efficient not to build a thunk.
    -- Mind you, this function is always INLINEd, so the client context is probably
    -- enough to avoid thunk construction and so the $! is just belt-and-braces.
  | otherwise
  = Nothing

saturates :: [Type] -> Arity -> Bool
saturates _       0 = True
saturates []      _ = False
saturates (_:tys) n = assert( n >= 0 ) $ saturates tys (n-1)
                       -- Arities are always positive; the assertion just checks
                       -- that, to avoid an infinite loop in the bad case

-- | A helper for 'expandSynTyConApp_maybe' to avoid inlining this cold path
-- into call-sites.
--
-- Precondition: the call is saturated or over-saturated;
--               i.e. length tvs <= length arg_tys
expand_syn :: [TyVar]  -- ^ the variables bound by the synonym
           -> Type     -- ^ the RHS of the synonym
           -> [Type]   -- ^ the type arguments the synonym is instantiated at.
           -> Type
{-# NOINLINE expand_syn #-} -- We never want to inline this cold-path.

expand_syn tvs rhs arg_tys
  -- No substitution necessary if either tvs or tys is empty
  -- This is both more efficient, and steers clear of an infinite
  -- loop; see Note [Care using synonyms to compress types]
  | null arg_tys  = assert (null tvs) rhs
  | null tvs      = mkAppTys rhs arg_tys
  | otherwise     = go empty_subst tvs arg_tys
  where
    empty_subst = mkEmptySubst in_scope
    in_scope = mkInScopeSet $ tyCoVarsOfTypes $ arg_tys
      -- The free vars of 'rhs' should all be bound by 'tenv',
      -- so we only need the free vars of tys
      -- See also Note [The substitution invariant] in GHC.Core.TyCo.Subst.

    go subst [] tys
      | null tys  = rhs'  -- Exactly Saturated
      | otherwise = mkAppTys rhs' tys
          -- Its important to use mkAppTys, rather than (foldl AppTy),
          -- because the function part might well return a
          -- partially-applied type constructor; indeed, usually will!
      where
        rhs' = substTy subst rhs

    go subst (tv:tvs) (ty:tys) = go (extendTvSubst subst tv ty) tvs tys

    go _ (_:_) [] = pprPanic "expand_syn" (ppr tvs $$ ppr rhs $$ ppr arg_tys)
                   -- Under-saturated, precondition failed

{- Note [Inlining coreView]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is very common to have a function

  f :: Type -> ...
  f ty | Just ty' <- coreView ty = f ty'
  f (TyVarTy ...) = ...
  f ...           = ...

If f is not otherwise recursive, the initial call to coreView
causes f to become recursive, which kills the possibility of
inlining. Instead, for non-recursive functions, we prefer to
use coreFullView, which guarantees to unwrap top-level type
synonyms. It can be inlined and is efficient and non-allocating
in its fast path. For this to really be fast, all calls made
on its fast path must also be inlined, linked back to this Note.
-}


{- *********************************************************************
*                                                                      *
                expandTypeSynonyms
*                                                                      *
********************************************************************* -}

expandTypeSynonyms :: Type -> Type
-- ^ Expand out all type synonyms.  Actually, it'd suffice to expand out
-- just the ones that discard type variables (e.g.  type Funny a = Int)
-- But we don't know which those are currently, so we just expand all.
--
-- 'expandTypeSynonyms' only expands out type synonyms mentioned in the type,
-- not in the kinds of any TyCon or TyVar mentioned in the type.
--
-- Keep this synchronized with 'synonymTyConsOfType'
expandTypeSynonyms ty
  = expandTypeSynonymsX empty_subst ty
  where
    empty_subst = mkEmptySubst $ mkInScopeSet (tyCoVarsOfType ty)

expandTypeSynonymsX :: Subst -> Type -> Type
expandTypeSynonymsX
  = case mapTyCo expandTypeSynonymMapper of
      (exp_ty, _, _, _) -> runSubstM exp_ty

expandTypeSynonymMapper :: TyCoMapper SubstM
-- Just like substitution, but treat TyConApp specially
expandTypeSynonymMapper
  = substTyCoMapper { tcm_tcapp_ty = tcapp_ty, tcm_tycobinder = tcv_bndr }
 where
   tcapp_ty :: Type -> TyCon -> [Type] -> SubstM Type
   tcapp_ty ty tc tys'
      | ExpandsSyn tenv rhs tys'' <- expandSynTyCon_maybe tc tys'
      = StrictReaderT $ \subst ->
        let in_scope    = substInScopeSet subst
            local_subst = mkTvSubst in_scope (mkVarEnv tenv)
            -- NB: tys' are already expanded, so this works
            --     even in the nested case (#11665)
            --        type T a b = a -> b
            --        type S x y = T y x
        in return (mkAppTys (expandTypeSynonymsX local_subst rhs) tys'')

      | null tys'   -- Avoid allocation in this very
      = return ty   -- common case (E.g. Int, LiftedRep etc)

      | otherwise
      = return (TyConApp tc tys')
        -- Do not use mkTyConApp, lest it re-introduce type synonyms!
        -- See Note [Using synonyms to compress types] which describes
        -- the logic for introducing type synonyms.  But here we are
        -- supposed to be getting /rid/ of type synonyms!

   tcv_bndr :: TyCoVar -> ForAllTyFlag -> (TyCoVar -> SubstM r) -> SubstM r
   tcv_bndr tcv _vis k
     = StrictReaderT $ \subst ->
       let !(subst', tcv') = substVarBndrUsing expandTypeSynonymsX subst tcv
                             -- Expand synonyms in the kind of the binder
       in runStrictReaderT (k tcv') subst'

{- Notes on type synonyms
~~~~~~~~~~~~~~~~~~~~~~~~~
The various "split" functions (splitFunTy, splitRhoTy, splitForAllTy) try
to return type synonyms wherever possible. Thus

        type Foo a = a -> a

we want
        splitFunTys (a -> Foo a) = ([a], Foo a)
not                                ([a], a -> a)

The reason is that we then get better (shorter) type signatures in
interfaces.  Notably this plays a role in tcTySigs in GHC.Tc.Gen.Bind.
-}

{- *********************************************************************
*                                                                      *
                Random functions (todo: organise)
*                                                                      *
********************************************************************* -}

-- | An INLINE helper for function such as 'kindRep_maybe' below.
--
-- @isTyConKeyApp_maybe key ty@ returns @Just tys@ iff
-- the type @ty = T tys@, where T's unique = key
-- key must not be `fUNTyConKey`; to test for functions, use `splitFunTy_maybe`.
-- Thanks to this fact, we don't have to pattern match on `FunTy` here.
isTyConKeyApp_maybe :: Unique -> Type -> Maybe [Type]
isTyConKeyApp_maybe key ty
  | TyConApp tc args <- coreFullView ty
  , tc `hasKey` key
  = Just args
  | otherwise
  = Nothing
{-# INLINE isTyConKeyApp_maybe #-}

-- | Extract the RuntimeRep classifier of a type from its kind. For example,
-- @kindRep * = LiftedRep@; Panics if this is not possible.
-- Treats * and Constraint as the same
kindRep :: HasDebugCallStack => Kind -> RuntimeRepType
kindRep k = case kindRep_maybe k of
              Just r  -> r
              Nothing -> pprPanic "kindRep" (ppr k)

-- | Given a kind (TYPE rr) or (CONSTRAINT rr), extract its RuntimeRep classifier rr.
-- For example, @kindRep_maybe * = Just LiftedRep@
-- Returns 'Nothing' if the kind is not of form (TYPE rr)
kindRep_maybe :: HasDebugCallStack => Kind -> Maybe RuntimeRepType
kindRep_maybe kind
  | Just (_, rep) <- sORTKind_maybe kind = Just rep
  | otherwise                            = Nothing

-- | Returns True if the argument is (lifted) Type or Constraint
-- See Note [TYPE and CONSTRAINT] in GHC.Builtin.WiredIn.Prim
isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind kind
  = case kindRep_maybe kind of
      Just rep -> isLiftedRuntimeRep rep
      Nothing  -> False

-- | Returns True if the kind classifies unlifted types (like 'Int#') and False
-- otherwise. Note that this returns False for representation-polymorphic
-- kinds, which may be specialized to a kind that classifies unlifted types.
isUnliftedTypeKind :: Kind -> Bool
isUnliftedTypeKind kind
  = case kindRep_maybe kind of
      Just rep -> isUnliftedRuntimeRep rep
      Nothing  -> False

pickyIsLiftedTypeKind :: Kind -> Bool
-- Checks whether the kind is literally
--      TYPE LiftedRep
-- or   TYPE ('BoxedRep 'Lifted)
-- or   Type
-- without expanding type synonyms or anything
-- Used only when deciding whether to suppress the ":: *" in
-- (a :: *) when printing kinded type variables
-- See Note [Suppressing * kinds] in GHC.Core.TyCo.Ppr
pickyIsLiftedTypeKind kind
  | TyConApp tc [arg] <- kind
  , tc `hasKey` tYPETyConKey
  , TyConApp rr_tc rr_args <- arg = case rr_args of
      [] -> rr_tc `hasKey` liftedRepTyConKey
      [rr_arg]
        | rr_tc `hasKey` boxedRepDataConKey
        , TyConApp lev [] <- rr_arg
        , lev `hasKey` liftedDataConKey -> True
      _ -> False
  | TyConApp tc [] <- kind
  , tc `hasKey` liftedTypeKindTyConKey = True
  | otherwise                          = False

-- | Check whether a kind is of the form `TYPE (BoxedRep Lifted)`
-- or `TYPE (BoxedRep Unlifted)`.
--
-- Returns:
--
--  - `Just Lifted` for `TYPE (BoxedRep Lifted)` and `Type`,
--  - `Just Unlifted` for `TYPE (BoxedRep Unlifted)` and `UnliftedType`,
--  - `Nothing` for anything else, e.g. `TYPE IntRep`, `TYPE (BoxedRep l)`, etc.
kindBoxedRepLevity_maybe :: Type -> Maybe Levity
kindBoxedRepLevity_maybe ty
  | Just rep <- kindRep_maybe ty
  , isBoxedRuntimeRep rep
  = runtimeRepLevity_maybe rep
  | otherwise
  = Nothing

-- | Check whether a type of kind 'RuntimeRep' is lifted.
--
-- 'isLiftedRuntimeRep' is:
--
--  * True of @LiftedRep :: RuntimeRep@
--  * False of type variables, type family applications,
--    and of other reps such as @IntRep :: RuntimeRep@.
isLiftedRuntimeRep :: RuntimeRepType -> Bool
isLiftedRuntimeRep rep
  = runtimeRepLevity_maybe rep == Just Lifted

-- | Check whether a type of kind 'RuntimeRep' is unlifted.
--
--  * True of definitely unlifted 'RuntimeRep's such as
--    'UnliftedRep', 'IntRep', 'FloatRep', ...
--  * False of 'LiftedRep',
--  * False for type variables and type family applications.
isUnliftedRuntimeRep :: RuntimeRepType -> Bool
isUnliftedRuntimeRep rep =
  runtimeRepLevity_maybe rep == Just Unlifted

-- | An INLINE helper for functions such as 'isLiftedLevity' and 'isUnliftedLevity'.
--
-- Checks whether the type is a nullary 'TyCon' application,
-- for a 'TyCon' with the given 'Unique'.
isNullaryTyConKeyApp :: Unique -> Type -> Bool
isNullaryTyConKeyApp key ty
  | Just args <- isTyConKeyApp_maybe key ty
  = assert (null args) True
  | otherwise
  = False
{-# INLINE isNullaryTyConKeyApp #-}

isLiftedLevity :: Type -> Bool
isLiftedLevity = isNullaryTyConKeyApp liftedDataConKey

isUnliftedLevity :: Type -> Bool
isUnliftedLevity = isNullaryTyConKeyApp unliftedDataConKey

-- | Is this the type 'Levity'?
isLevityTy :: Type -> Bool
isLevityTy = isNullaryTyConKeyApp levityTyConKey

-- | Is this the type 'RuntimeRep'?
isRuntimeRepTy :: Type -> Bool
isRuntimeRepTy = isNullaryTyConKeyApp runtimeRepTyConKey

-- | Is a tyvar of type 'RuntimeRep'?
isRuntimeRepVar :: TyVar -> Bool
isRuntimeRepVar = isRuntimeRepTy . tyVarKind

-- | Is a tyvar of type 'Levity'?
isLevityVar :: TyVar -> Bool
isLevityVar = isLevityTy . tyVarKind

-- | Is this the type 'Multiplicity'?
isMultiplicityTy :: Type -> Bool
isMultiplicityTy  = isNullaryTyConKeyApp multiplicityTyConKey

-- | Is a tyvar of type 'Multiplicity'?
isMultiplicityVar :: TyVar -> Bool
isMultiplicityVar = isMultiplicityTy . tyVarKind

--------------------------------------------
--  Splitting RuntimeRep
--------------------------------------------

-- | (splitRuntimeRep_maybe rr) takes a Type rr :: RuntimeRep, and
--   returns the (TyCon,[Type]) for the RuntimeRep, if possible, where
--   the TyCon is one of the promoted DataCons of RuntimeRep.
-- Remember: the unique on TyCon that is a a promoted DataCon is the
--           same as the unique on the DataCon
--           See Note [Promoted data constructors] in GHC.Core.TyCon
-- May not be possible if `rr` is a type variable or type
--   family application
splitRuntimeRep_maybe :: RuntimeRepType -> Maybe (TyCon, [Type])
splitRuntimeRep_maybe rep
  | TyConApp rr_tc args <- coreFullView rep
  , isPromotedDataCon rr_tc
    -- isPromotedDataCon: be careful of type families (F tys) :: RuntimeRep,
  = Just (rr_tc, args)
  | otherwise
  = Nothing

-- | See 'isBoxedRuntimeRep_maybe'.
isBoxedRuntimeRep :: RuntimeRepType -> Bool
isBoxedRuntimeRep rep = isJust (isBoxedRuntimeRep_maybe rep)

-- | `isBoxedRuntimeRep_maybe (rep :: RuntimeRep)` returns `Just lev` if `rep`
-- expands to `Boxed lev` and returns `Nothing` otherwise.
--
-- Types with this runtime rep are represented by pointers on the GC'd heap.
isBoxedRuntimeRep_maybe :: RuntimeRepType -> Maybe LevityType
isBoxedRuntimeRep_maybe rep
  | Just (rr_tc, args) <- splitRuntimeRep_maybe rep
  , rr_tc `hasKey` boxedRepDataConKey
  , [lev] <- args
  = Just lev
  | otherwise
  = Nothing

-- | Check whether a type (usually of kind 'RuntimeRep') is lifted, unlifted,
--   or unknown.  Returns Nothing if the type isn't of kind 'RuntimeRep'.
--
-- `runtimeRepLevity_maybe rr` returns:
--
--   * `Just Lifted` if `rr` is `LiftedRep :: RuntimeRep`
--   * `Just Unlifted` if `rr` is definitely unlifted, e.g. `IntRep`
--   * `Nothing` if not known (e.g. it's a type variable or a type family application).
runtimeRepLevity_maybe :: RuntimeRepType -> Maybe Levity
runtimeRepLevity_maybe rep
  | Just (rr_tc, args) <- splitRuntimeRep_maybe rep
  =       -- NB: args might be non-empty e.g. TupleRep [r1, .., rn]
    if (rr_tc `hasKey` boxedRepDataConKey)
    then case args of
            [lev] -> levityType_maybe lev
            _     -> Nothing  -- Type isn't of kind RuntimeRep
                     -- The latter case happens via the call to isLiftedRuntimeRep
                     -- in GHC.Tc.Errors.Ppr.pprMismatchMsg (#22742)
    else Just Unlifted
        -- Avoid searching all the unlifted RuntimeRep type cons
        -- In the RuntimeRep data type, only LiftedRep is lifted
  | otherwise
  = Nothing

--------------------------------------------
--  Splitting Levity
--------------------------------------------

-- | `levityType_maybe` takes a Type of kind Levity, and returns its levity
-- May not be possible for a type variable or type family application
levityType_maybe :: LevityType -> Maybe Levity
levityType_maybe lev
  | TyConApp lev_tc args <- coreFullView lev
  = if | lev_tc `hasKey` liftedDataConKey   -> assert( null args) $ Just Lifted
       | lev_tc `hasKey` unliftedDataConKey -> assert( null args) $ Just Unlifted
       | otherwise                          -> Nothing
  | otherwise
  = Nothing


{- *********************************************************************
*                                                                      *
                      TyVarTy
*                                                                      *
********************************************************************* -}

-- | Attempts to obtain the type variable underlying a 'Type', and panics with the
-- given message if this is not a type variable type. See also 'getTyVar_maybe'
getTyVar :: HasDebugCallStack => Type -> TyVar
getTyVar ty = case getTyVar_maybe ty of
                    Just tv -> tv
                    Nothing -> pprPanic "getTyVar" (ppr ty)

-- | Attempts to obtain the type variable underlying a 'Type'
getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe = repGetTyVar_maybe . coreFullView

-- | Attempts to obtain the type variable underlying a 'Type', without
-- any expansion
repGetTyVar_maybe :: Type -> Maybe TyVar
repGetTyVar_maybe (TyVarTy tv) = Just tv
repGetTyVar_maybe _            = Nothing

isTyVarTy :: Type -> Bool
isTyVarTy ty = isJust (getTyVar_maybe ty)

-- | If the type is a tyvar, possibly under a cast, returns it, along
-- with the coercion. Thus, the co is :: kind tv ~N kind ty
getCastedTyVar_maybe :: Type -> Maybe (TyVar, CoercionN)
getCastedTyVar_maybe ty = case coreFullView ty of
  CastTy (TyVarTy tv) co -> Just (tv, co)
  TyVarTy tv             -> Just (tv, mkReflCo Nominal (tyVarKind tv))
  _                      -> Nothing


substTyVarToTyVar :: HasDebugCallStack => Subst -> TyVar -> TyVar
-- Apply the substitution, expecting the result to be a TyVarTy
substTyVarToTyVar (Subst _ _ tenv _) tv
  = assert (isTyVar tv) $
    case lookupVarEnv tenv tv of
      Just ty -> case getTyVar_maybe ty of
                    Just tv -> tv
                    Nothing -> pprPanic "substTyVarToTyVar" (ppr tv $$ ppr ty)
      Nothing -> tv


{- *********************************************************************
*                                                                      *
                      AppTy
*                                                                      *
********************************************************************* -}

splitAppTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Attempt to take a type application apart, whether it is a
-- function, type constructor, or plain type application. Note
-- that type family applications are NEVER unsaturated by this!
splitAppTy_maybe = splitAppTyNoView_maybe . coreFullView

splitAppTy :: Type -> (Type, Type)
-- ^ Attempts to take a type application apart, as in 'splitAppTy_maybe',
-- and panics if this is not possible
splitAppTy ty = splitAppTy_maybe ty `orElse` pprPanic "splitAppTy" (ppr ty)

-------------
splitAppTyNoView_maybe :: HasDebugCallStack => Type -> Maybe (Type,Type)
-- ^ Does the AppTy split as in 'splitAppTy_maybe', but assumes that
-- any 'coreView' stuff is already done.
splitAppTyNoView_maybe (AppTy ty1 ty2)
  = Just (ty1, ty2)

splitAppTyNoView_maybe (FunTy af ma w ty1 ty2)
  | Just (tc, tys)   <- funTyConAppTy_maybe af ma w ty1 ty2
  , Just (tys', ty') <- snocView tys
  = Just (TyConApp tc tys', ty')

splitAppTyNoView_maybe (TyConApp tc tys)
  | not (tyConMustBeSaturated tc) || tys `lengthExceeds` tyConArity tc
  , Just (tys', ty') <- snocView tys
  = Just (TyConApp tc tys', ty')    -- Never create unsaturated type family apps!

splitAppTyNoView_maybe _other = Nothing

tcSplitAppTyNoView_maybe :: Type -> Maybe (Type,Type)
-- ^ Just like 'splitAppTyNoView_maybe', but does not split @(c => t)@.
--
-- See Note [Decomposing fat arrow c=>t]
tcSplitAppTyNoView_maybe ty
  | FunTy { ft_af = af } <- ty
  , not (isVisibleFunArg af)  -- See Note [Decomposing fat arrow c=>t]
  = Nothing
  | otherwise
  = splitAppTyNoView_maybe ty

-------------
splitAppTys :: HasDebugCallStack => Type -> (Type, [Type])
-- ^ Recursively splits a type as far as is possible, leaving a residual
-- type being applied to and the type arguments applied to it. Never fails,
-- even if that means returning an empty list of type applications.
splitAppTys ty = split ty ty []
  where
    split orig_ty ty args | Just ty' <- coreView ty = split orig_ty ty' args
    split _       (AppTy ty arg)        args = split ty ty (arg:args)
    split _       (TyConApp tc tc_args) args
      = let -- keep type families saturated
            n | tyConMustBeSaturated tc = tyConArity tc
              | otherwise               = 0
            (tc_args1, tc_args2) = splitAt n tc_args
        in
        (TyConApp tc tc_args1, tc_args2 ++ args)
    split _   (FunTy af ma w ty1 ty2) args
      | Just (tc,tys) <- funTyConAppTy_maybe af ma w ty1 ty2
      = assert (null args )
        (TyConApp tc [], tys)

    split orig_ty _ args  = (orig_ty, args)

-- | Like 'splitAppTys', but doesn't look through type synonyms
splitAppTysNoView :: HasDebugCallStack => Type -> (Type, [Type])
splitAppTysNoView ty = split ty []
  where
    split (AppTy ty arg) args = split ty (arg:args)
    split (TyConApp tc tc_args) args
      = let n | tyConMustBeSaturated tc = tyConArity tc
              | otherwise               = 0
            (tc_args1, tc_args2) = splitAt n tc_args
        in
        (TyConApp tc tc_args1, tc_args2 ++ args)
    split (FunTy af ma w ty1 ty2) args
      | Just (tc, tys) <- funTyConAppTy_maybe af ma w ty1 ty2
      = assert (null args )
        (TyConApp tc [], tys)

    split ty args = (ty, args)


{- *********************************************************************
*                                                                      *
                      LitTy
*                                                                      *
********************************************************************* -}

-- | Is this a numeric literal. We also look through type synonyms.
isNumLitTy :: Type -> Maybe Integer
isNumLitTy ty
  | LitTy (NumTyLit n) <- coreFullView ty = Just n
  | otherwise                             = Nothing

-- | Is this a symbol literal. We also look through type synonyms.
isStrLitTy :: Type -> Maybe FastString
isStrLitTy ty
  | LitTy (StrTyLit s) <- coreFullView ty = Just s
  | otherwise                             = Nothing

-- | Is this a char literal? We also look through type synonyms.
isCharLitTy :: Type -> Maybe Char
isCharLitTy ty
  | LitTy (CharTyLit s) <- coreFullView ty = Just s
  | otherwise                              = Nothing


-- | Is this a type literal (symbol, numeric, or char)?
isLitTy :: Type -> Maybe TyLit
isLitTy ty
  | LitTy l <- coreFullView ty = Just l
  | otherwise                  = Nothing

-- | A type of kind 'ErrorMessage' (from the 'GHC.TypeError' module).
type ErrorMsgType = Type

-- | Render a type corresponding to a user type error into a SDoc.
pprUserTypeErrorTy :: ErrorMsgType -> SDoc
pprUserTypeErrorTy ty =
  case splitTyConApp_maybe ty of

    -- Text "Something"
    Just (tc,[txt])
      | tc `hasKnownKey` typeErrorTextDataConKey
      , Just str <- isStrLitTy txt -> ftext str

    -- ShowType t
    Just (tc,[_k,t])
      | tc `hasKnownKey` typeErrorShowTypeDataConKey -> ppr t

    -- t1 :<>: t2
    Just (tc,[t1,t2])
      | tc `hasKnownKey` typeErrorAppendDataConKey ->
        pprUserTypeErrorTy t1 <> pprUserTypeErrorTy t2

    -- t1 :$$: t2
    Just (tc,[t1,t2])
      | tc `hasKnownKey` typeErrorVAppendDataConKey ->
        pprUserTypeErrorTy t1 $$ pprUserTypeErrorTy t2

    -- An unevaluated type function
    _ -> ppr ty

{- *********************************************************************
*                                                                      *
                      FunTy
*                                                                      *
********************************************************************* -}

{- Note [Representation of function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functions (e.g. Int -> Char) can be thought of as being applications
of funTyCon (known in Haskell surface syntax as (->)), (note that
`RuntimeRep' quantifiers are left inferred)

    (->) :: forall {r1 :: RuntimeRep} {r2 :: RuntimeRep}
                   (a :: TYPE r1) (b :: TYPE r2).
            a -> b -> Type

However, for efficiency's sake we represent saturated applications of (->)
with FunTy. For instance, the type,

    (->) r1 r2 a b

is equivalent to,

    FunTy (Anon a) b

Note how the RuntimeReps are implied in the FunTy representation. For this
reason we must be careful when reconstructing the TyConApp representation (see,
for instance, splitTyConApp_maybe).

In the compiler we maintain the invariant that all saturated applications of
(->) are represented with FunTy.

See #11714.
-}

-----------------------------------------------
funTyConAppTy_maybe :: FunTyFlag -> Matchability -> Type -> Type -> Type
                    -> Maybe (TyCon, [Type])
-- ^ Given the components of a FunTy
-- figure out the corresponding TyConApp.
funTyConAppTy_maybe af ma mult arg res
  | isFUNArg af || eqType ma unmatchableDataConTy
  , Just arg_rep <- getRuntimeRep_maybe arg
  , Just res_rep <- getRuntimeRep_maybe res
  -- If you're changing the lines below, you'll probably want to adapt the
  -- `fUNTyCon` case of GHC.Core.Unify.unify_ty correspondingly.
  , let args | isFUNArg af = [ma, mult, arg_rep, res_rep, arg, res]
             | otherwise   = [      arg_rep, res_rep, arg, res]
  = Just $ (funTyFlagTyCon af, args)
  | otherwise
  = Nothing

mkFunctionType :: HasDebugCallStack => Matchability -> Mult -> Type -> Type -> Type
-- ^ This one works out the FunTyFlag from the argument type
-- See GHC.Types.Var Note [FunTyFlag]
mkFunctionType ma mult arg_ty res_ty
 = FunTy { ft_af = af, ft_ma = ma, ft_arg = arg_ty, ft_res = res_ty
         , ft_mult = assertPpr mult_ok (ppr [mult, arg_ty, res_ty]) $
                     mult }
  where
    af = chooseFunTyFlag arg_ty res_ty
    mult_ok = isVisibleFunArg af || isManyTy mult

mkScaledFunctionTys :: [(Matchability, Scaled Type)] -> Type -> Type
-- ^ Like mkFunctionType, compute the FunTyFlag from the arguments
mkScaledFunctionTys arg_tys res_ty
  = foldr mk res_ty arg_tys
  where
    mk (ma, Scaled mult arg_ty) res_ty
      = mkFunTy (chooseFunTyFlag arg_ty res_ty)
                ma mult arg_ty res_ty

-- | Build matchable scaled arrows.
mkScaledFunctionTysM :: [Scaled Type] -> Type -> Type
mkScaledFunctionTysM args = mkScaledFunctionTys [(matchableDataConTy,arg) | arg <- args]

-- | Build ordinary runtime scaled arrows.
mkScaledFunctionTysU :: [Scaled Type] -> Type -> Type
mkScaledFunctionTysU args = mkScaledFunctionTys [(unmatchableDataConTy,arg) | arg <- args]

chooseFunTyFlag :: HasDebugCallStack => Type -> Type -> FunTyFlag
-- ^ See GHC.Types.Var Note [FunTyFlag]
chooseFunTyFlag arg_ty res_ty
  = mkFunTyFlag (typeTypeOrConstraint arg_ty) (typeTypeOrConstraint res_ty)

splitFunTy :: Type -> (Matchability, Mult, Type, Type)
-- ^ Extract matchability, multiplicity, argument and result.
splitFunTy ty = case splitFunTy_maybe ty of
                   Just (_af, ma, mult, arg, res) -> (ma,mult,arg,res)
                   Nothing -> pprPanic "splitFunTy" (ppr ty)

{-# INLINE splitFunTy_maybe #-}
splitFunTy_maybe :: Type -> Maybe (FunTyFlag, Matchability, Mult, Type, Type)
-- ^ Extract all semantic arrow components.
splitFunTy_maybe ty
  | FunTy af ma w arg res <- coreFullView ty = Just (af, ma, w, arg, res)
  | otherwise = Nothing

{-# INLINE splitVisibleFunTy_maybe #-}
splitVisibleFunTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Inspect visible argument/result syntax without an inverse equality rule.
splitVisibleFunTy_maybe ty
  | FunTy af _ma _ arg res <- coreFullView ty
  , isVisibleFunArg af = Just (arg,res)
  | otherwise = Nothing

splitFunTys :: Type -> ([Matchability], [Scaled Type], Type)
splitFunTys ty = split [] [] ty ty
  where
    split mas args _ (FunTy _ ma w arg res)
      = split (ma:mas) (Scaled w arg:args) res res
    split mas args orig_ty ty | Just ty' <- coreView ty
      = split mas args orig_ty ty'
    split mas args orig_ty _ = (reverse mas,reverse args,orig_ty)

-- | Check the consumed kind binder for nominal inverse application evidence.
-- See Note [Ordinary arrow matchability] in GHC.Core.TyCo.Rep.
isMatchableFunTy :: Type -> Bool
isMatchableFunTy ki = case coreFullView ki of
    FunTy { ft_ma = ma } -> eqType ma matchableDataConTy
    ForAllTy ma _ _ -> eqType ma matchableDataConTy
    _ -> False

funResultTy :: HasDebugCallStack => Type -> Type
-- ^ Extract the function result type and panic if that is not possible
funResultTy ty
  | FunTy { ft_res = res } <- coreFullView ty = res
  | otherwise                                 = pprPanic "funResultTy" (ppr ty)

funArgTy :: HasDebugCallStack => Type -> Type
-- ^ Extract the function argument type and panic if that is not possible
funArgTy ty
  | FunTy { ft_arg = arg } <- coreFullView ty = arg
  | otherwise                                 = pprPanic "funArgTy" (ppr ty)

-- ^ Just like 'piResultTys' but for a single argument
-- Try not to iterate 'piResultTy', because it's inefficient to substitute
-- one variable at a time; instead use 'piResultTys"
piResultTy :: HasDebugCallStack => Type -> Type ->  Type
piResultTy ty arg = case piResultTy_maybe ty arg of
                      Just res -> res
                      Nothing  -> pprPanic "piResultTy" (ppr ty $$ ppr arg)

piResultTy_maybe :: Type -> Type -> Maybe Type
piResultTy_maybe ty arg = case coreFullView ty of
  FunTy { ft_res = res } -> Just res

  ForAllTy _ (Bndr tv _) res
    -- See Note [Source and target substitution scopes] in GHC.Core.TyCo.Subst.
    -> let target_subst = mkEmptySubst $ mkInScopeSet $ tyCoVarsOfType arg
           empty_subst = extendSubstInScopeSetFromSource target_subst
                           (tyCoVarsOfType ty)
       in Just (substTy (extendTCvSubst empty_subst tv arg) res)

  _ -> Nothing

-- | (piResultTys f_ty [ty1, .., tyn]) gives the type of (f ty1 .. tyn)
--   where f :: f_ty
-- 'piResultTys' is interesting because:
--      1. 'f_ty' may have more for-alls than there are args
--      2. Less obviously, it may have fewer for-alls
-- For case 2. think of:
--   piResultTys (forall a.a) [forall b.b, Int]
-- This really can happen, but only (I think) in situations involving
-- undefined.  For example:
--       undefined :: forall a. a
-- Term: undefined @(forall b. b->b) @Int
-- This term should have type (Int -> Int), but notice that
-- there are more type args than foralls in 'undefined's type.

-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint

-- This is a heavily used function (e.g. from typeKind),
-- so we pay attention to efficiency, especially in the special case
-- where there are no for-alls so we are just dropping arrows from
-- a function type/kind.
piResultTys :: HasDebugCallStack => Type -> [Type] -> Type
piResultTys ty [] = ty
piResultTys ty orig_args@(arg:args)
  | FunTy { ft_res = res } <- ty
  = piResultTys res args

  | ForAllTy _ (Bndr tcv _) res <- ty
  = -- Both type and coercion variables
    go (extendTCvSubst init_subst tcv arg) res args

  | Just ty' <- coreView ty
  = piResultTys ty' orig_args

  | otherwise
  = pprPanic "piResultTys1" (ppr ty $$ ppr orig_args)
  where
    -- See Note [Source and target substitution scopes] in GHC.Core.TyCo.Subst.
    init_subst = extendSubstInScopeSetFromSource target_subst (tyCoVarsOfType ty)
    target_subst = mkEmptySubst $ mkInScopeSet (tyCoVarsOfTypes orig_args)

    go :: Subst -> Type -> [Type] -> Type
    go subst ty [] = substTyUnchecked subst ty

    go subst ty all_args@(arg:args)
      | FunTy { ft_res = res } <- ty
      = go subst res args

      | ForAllTy _ (Bndr tv _) res <- ty
      = go (extendTCvSubst subst tv arg) res args

      | Just ty' <- coreView ty
      = go subst ty' all_args

      | not (isEmptyTCvSubst subst)  -- See Note [Care with kind instantiation]
      = go init_subst
          (substTy subst ty)
          all_args

      | otherwise
      = -- We have not run out of arguments, but the function doesn't
        -- have the right kind to apply to them; so panic.
        -- Without the explicit isEmptyVarEnv test, an ill-kinded type
        -- would give an infinite loop, which is very unhelpful
        -- c.f. #15473
        pprPanic "piResultTys2" (ppr ty $$ ppr orig_args $$ ppr all_args)

applyTysX :: HasDebugCallStack => [TyVar] -> Type -> [Type] -> Type
-- applyTysX beta-reduces (/\tvs. body_ty) arg_tys
-- Assumes that (/\tvs. body_ty) is closed
applyTysX tvs body_ty arg_tys
  = assertPpr (tvs `leLength` arg_tys) pp_stuff $
    assertPpr (tyCoVarsOfType body_ty `subVarSet` mkVarSet tvs) pp_stuff $
    mkAppTys (substTyWith tvs arg_tys_prefix body_ty)
             arg_tys_rest
  where
    pp_stuff = vcat [ppr tvs, ppr body_ty, ppr arg_tys]
    (arg_tys_prefix, arg_tys_rest) = splitAtList tvs arg_tys


{- Note [Care with kind instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  T :: forall k. k
and we are finding the kind of
  T (forall b. b -> b) * Int
Then
  T (forall b. b->b) :: k[ k :-> forall b. b->b]
                     :: forall b. b -> b
So
  T (forall b. b->b) * :: (b -> b)[ b :-> *]
                       :: * -> *

In other words we must instantiate the forall!

Similarly (#15428)
   S :: forall k f. k -> f k
and we are finding the kind of
   S * (* ->) Int Bool
We have
   S * (* ->) :: (k -> f k)[ k :-> *, f :-> (* ->)]
              :: * -> * -> *
So again we must instantiate.

The same thing happens in GHC.CoreToIface.toIfaceAppArgsX.
-}


{- *********************************************************************
*                                                                      *
                      TyConApp
*                                                                      *
********************************************************************* -}

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

-- | Retrieve the tycon heading this type, if there is one. Does /not/
-- look through synonyms.
tyConAppTyConPicky_maybe :: Type -> Maybe TyCon
tyConAppTyConPicky_maybe (TyConApp tc _)        = Just tc
tyConAppTyConPicky_maybe (FunTy { ft_af = af }) = Just (funTyFlagTyCon af)
tyConAppTyConPicky_maybe _                      = Nothing


-- | The same as @fst . splitTyConApp@
-- We can short-cut the FunTy case
{-# INLINE tyConAppTyCon_maybe #-}
tyConAppTyCon_maybe :: Type -> Maybe TyCon
tyConAppTyCon_maybe ty = case coreFullView ty of
  TyConApp tc _        -> Just tc
  FunTy { ft_af = af } -> Just (funTyFlagTyCon af)
  _                    -> Nothing

tyConAppTyCon :: HasDebugCallStack => Type -> TyCon
tyConAppTyCon ty = tyConAppTyCon_maybe ty `orElse` pprPanic "tyConAppTyCon" (ppr ty)

-- | The same as @snd . splitTyConApp@
tyConAppArgs_maybe :: Type -> Maybe [Type]
tyConAppArgs_maybe ty = case splitTyConApp_maybe ty of
                          Just (_, tys) -> Just tys
                          Nothing       -> Nothing

tyConAppArgs :: HasDebugCallStack => Type -> [Type]
tyConAppArgs ty = tyConAppArgs_maybe ty `orElse` pprPanic "tyConAppArgs" (ppr ty)

-- | Attempts to tease a type apart into a type constructor and the application
-- of a number of arguments to that constructor. Panics if that is not possible.
-- See also 'splitTyConApp_maybe'
splitTyConApp :: Type -> (TyCon, [Type])
splitTyConApp ty = splitTyConApp_maybe ty `orElse` pprPanic "splitTyConApp" (ppr ty)

-- | Attempts to tease a type apart into a type constructor and the application
-- of a number of arguments to that constructor
splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe ty = splitTyConAppNoView_maybe (coreFullView ty)

splitTyConAppNoView_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
-- Same as splitTyConApp_maybe but without looking through synonyms
splitTyConAppNoView_maybe ty
  = case ty of
      FunTy { ft_af = af, ft_ma = ma, ft_mult = w, ft_arg = arg, ft_res = res}
                      -> funTyConAppTy_maybe af ma w arg res
      TyConApp tc tys -> Just (tc, tys)
      _               -> Nothing

-- | tcSplitTyConApp_maybe splits a type constructor application into
-- its type constructor and applied types.
--
-- Differs from splitTyConApp_maybe in that it does *not* split types
-- headed with (=>), as that's not a TyCon in the type-checker.
--
-- Note that this may fail (in funTyConAppTy_maybe) in the case
-- of a 'FunTy' with an argument of unknown kind 'FunTy'
-- (e.g. `FunTy (a :: k) Int`, since the kind of @a@ isn't of
-- the form `TYPE rep`.  This isn't usually a problem but may
-- be temporarily the case during canonicalization:
--     see Note [Decomposing FunTy] in GHC.Tc.Solver.Equality
--     and Note [The Purely Kinded Type Invariant (PKTI)] in GHC.Tc.Gen.HsType,
--         Wrinkle around FunTy
--
-- Consequently, you may need to zonk your type before
-- using this function.
tcSplitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
-- Defined here to avoid module loops between Unify and TcType.
tcSplitTyConApp_maybe ty
  = case coreFullView ty of
      FunTy { ft_af = af, ft_ma = ma, ft_mult = w, ft_arg = arg, ft_res = res}
                      | isVisibleFunArg af    -- Visible args only
                        -- See Note [Decomposing fat arrow c=>t]
                      -> funTyConAppTy_maybe af ma w arg res
      TyConApp tc tys -> Just (tc, tys)
      _               -> Nothing

tcSplitTyConApp :: Type -> (TyCon, [Type])
tcSplitTyConApp ty
  = tcSplitTyConApp_maybe ty `orElse` pprPanic "tcSplitTyConApp" (ppr ty)

---------------------------
newTyConInstRhs :: TyCon -> [Type] -> Type
-- ^ Unwrap one 'layer' of newtype on a type constructor and its
-- arguments, using an eta-reduced version of the @newtype@ if possible.
-- This requires tys to have at least @newTyConInstArity tycon@ elements.
newTyConInstRhs tycon tys
    = assertPpr (tvs `leLength` tys) (ppr tycon $$ ppr tys $$ ppr tvs) $
      applyTysX tvs rhs tys
  where
    (tvs, rhs) = newTyConEtadRhs tycon


{- *********************************************************************
*                                                                      *
                      CastTy
*                                                                      *
********************************************************************* -}

splitCastTy_maybe :: Type -> Maybe (Type, Coercion)
splitCastTy_maybe ty
  | CastTy ty' co <- coreFullView ty = Just (ty', co)
  | otherwise                        = Nothing

-- | Make a 'CastTy'. The Coercion must be nominal. Checks the
-- Coercion for reflexivity, dropping it if it's reflexive.
-- See @Note [Respecting definitional equality]@ in "GHC.Core.TyCo.Rep"
mkCastTy :: Type -> Coercion -> Type
mkCastTy orig_ty co | isReflexiveCo co = orig_ty  -- (EQ2) from the Note
-- NB: Do the slow check here. This is important to keep the splitXXX
-- functions working properly. Otherwise, we may end up with something
-- like (((->) |> something_reflexive_but_not_obviously_so) biz baz)
-- fails under splitFunTy_maybe. This happened with the cheaper check
-- in test dependent/should_compile/dynamic-paper.
mkCastTy orig_ty co = mk_cast_ty orig_ty co

-- | Like 'mkCastTy', but avoids checking the coercion for reflexivity,
-- as that can be expensive.
mk_cast_ty :: Type -> Coercion -> Type
mk_cast_ty orig_ty co = go orig_ty
  where
    go :: Type -> Type
    -- See Note [Using coreView in mk_cast_ty]
    go ty | Just ty' <- coreView ty = go ty'

    go (CastTy ty co1)
      -- (EQ3) from the Note
      = mkCastTy ty (co1 `mkTransCo` co)
          -- call mkCastTy again for the reflexivity check

    go (ForAllTy ma (Bndr tv vis) inner_ty)
      -- (EQ4) from the Note
      -- See Note [Weird typing rule for ForAllTy] in GHC.Core.TyCo.Rep.
      | isTyVar tv
      , let fvs = tyCoVarsOfCo co
      = -- have to make sure that pushing the co in doesn't capture the bound var!
        if tv `elemVarSet` fvs
        then let empty_subst = mkEmptySubst (mkInScopeSet fvs)
                 (subst, tv') = substVarBndr empty_subst tv
             in ForAllTy ma (Bndr tv' vis) (substTy subst inner_ty `mk_cast_ty` co)
        else ForAllTy ma (Bndr tv vis) (inner_ty `mk_cast_ty` co)

    go _ = CastTy orig_ty co -- NB: orig_ty: preserve synonyms if possible

{-
Note [Using coreView in mk_cast_ty]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariants (EQ3) and (EQ4) of Note [Respecting definitional equality] in
GHC.Core.TyCo.Rep must apply regardless of type synonyms. For instance,
consider this example (#19742):

   type EqSameNat = () |> co
   useNatEq :: EqSameNat |> sym co

(Those casts aren't visible in the user-source code, of course; see #19742 for
what the user might write.)

The type `EqSameNat |> sym co` looks as if it satisfies (EQ3), as it has no
nested casts, but if we expand EqSameNat, we see that it doesn't.
And then Bad Things happen.

The solution is easy: just use `coreView` when establishing (EQ3) and (EQ4) in
`mk_cast_ty`.
-}

{- *********************************************************************
*                                                                      *
                      CoercionTy
*                                                                      *
********************************************************************* -}

isCoercionTy :: Type -> Bool
isCoercionTy (CoercionTy _) = True
isCoercionTy _              = False

isCoercionTy_maybe :: Type -> Maybe Coercion
isCoercionTy_maybe (CoercionTy co) = Just co
isCoercionTy_maybe _               = Nothing

stripCoercionTy :: Type -> Coercion
stripCoercionTy (CoercionTy co) = co
stripCoercionTy ty              = pprPanic "stripCoercionTy" (ppr ty)


{- *********************************************************************
*                                                                      *
                      ForAllTy
*                                                                      *
********************************************************************* -}

tyConBindersPiTyBinders :: [TyConBinder] -> [PiTyBinder]
-- Return the tyConBinders in PiTyBinder form
tyConBindersPiTyBinders = map to_tyb
  where
    to_tyb (Bndr tv (NamedTCB vis)) = Named (Bndr tv vis)
    to_tyb (Bndr tv AnonTCB)        = Anon (tymult (varType tv)) FTF_T_T

-- | Select the concrete default of a newly introduced source forall.
defaultForAllMatchability :: TypeOrKind -> Matchability
defaultForAllMatchability TypeLevel = unmatchableDataConTy
defaultForAllMatchability KindLevel = matchableDataConTy

-- | Take a ForAllTy apart, returning annotated binders and the result type.
splitForAllForAllTyBinders :: Type -> ([(Matchability, ForAllTyBinder)], Type)
splitForAllForAllTyBinders ty = split ty ty []
  where
    split _ (ForAllTy ma b res) bs                = split res res ((ma,b):bs)
    split orig_ty ty bs | Just ty' <- coreView ty = split orig_ty ty' bs
    split orig_ty _                bs             = (reverse bs, orig_ty)
{-# INLINE splitForAllForAllTyBinders #-}

-- | Take a ForAllTy apart, returning the list of tycovars and the result type.
-- This always succeeds, even if it returns only an empty list. Note that the
-- result type returned may have free variables that were bound by a forall.
splitForAllTyCoVars :: Type -> ([(Matchability, TyCoVar)], Type)
splitForAllTyCoVars ty = split ty ty []
  where
    split _       (ForAllTy ma (Bndr tv _) ty) tvs = split ty ty ((ma,tv):tvs)
    split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
    split orig_ty _                            tvs = (reverse tvs, orig_ty)

-- | Like 'splitForAllTyCoVars', but split only for tyvars.
-- This always succeeds, even if it returns only an empty list. Note that the
-- result type returned may have free variables that were bound by a forall.
splitForAllTyVars :: Type -> ([(Matchability, TyVar)], Type)
splitForAllTyVars ty = split ty ty []
  where
    split _ (ForAllTy ma (Bndr tv _) ty) tvs | isTyVar tv = split ty ty ((ma,tv):tvs)
    split orig_ty ty tvs | Just ty' <- coreView ty     = split orig_ty ty' tvs
    split orig_ty _                   tvs              = (reverse tvs, orig_ty)

-- | Like 'splitForAllTyCoVars', but only splits 'ForAllTy's with 'Required' type
-- variable binders. Furthermore, each returned tyvar is annotated with '()'.
splitForAllReqTyBinders :: Type -> ([(Matchability, ReqTyBinder)], Type)
splitForAllReqTyBinders ty = split ty ty []
  where
    split _ (ForAllTy ma (Bndr tv Required) ty) tvs = split ty ty ((ma,Bndr tv ()):tvs)
    split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
    split orig_ty _                   tvs          = (reverse tvs, orig_ty)

-- | Like 'splitForAllTyCoVars', but only splits 'ForAllTy's with 'Invisible' type
-- variable binders. Furthermore, each returned tyvar is annotated with its
-- 'Specificity'.
splitForAllInvisTyBinders :: Type -> ([(Matchability, InvisTyBinder)], Type)
splitForAllInvisTyBinders ty = split ty ty []
  where
    split _ (ForAllTy ma (Bndr tv (Invisible spec)) ty) tvs = split ty ty ((ma,Bndr tv spec):tvs)
    split orig_ty ty tvs | Just ty' <- coreView ty       = split orig_ty ty' tvs
    split orig_ty _                   tvs                = (reverse tvs, orig_ty)

-- | Checks whether this is a proper forall (with a named binder)
isForAllTy :: Type -> Bool
isForAllTy ty
  | ForAllTy {} <- coreFullView ty = True
  | otherwise                      = False

-- | Like `isForAllTy`, but returns True only if it is a tyvar binder
isForAllTy_ty :: Type -> Bool
isForAllTy_ty ty
  | ForAllTy _ (Bndr tv _) _ <- coreFullView ty
  , isTyVar tv
  = True

  | otherwise = False

-- | Like `isForAllTy`, but returns True only if it is an inferred tyvar binder
isForAllTy_invis_ty :: Type -> Bool
isForAllTy_invis_ty  ty
  | ForAllTy _ (Bndr tv (Invisible InferredSpec)) _ <- coreFullView ty
  , isTyVar tv
  = True

  | otherwise = False

-- | Like `isForAllTy`, but returns True only if it is a covar binder
isForAllTy_co :: Type -> Bool
isForAllTy_co ty
  | ForAllTy _ (Bndr tv _) _ <- coreFullView ty
  , isCoVar tv
  = True

  | otherwise = False

-- | Is this type a @FunTy@ or a @ForAllTy@, after expanding type synonyms?
--
-- Does not look through newtypes nor reduce type family applications.
isPiTy :: Type -> Bool
isPiTy ty = case coreFullView ty of
  ForAllTy {} -> True
  FunTy {}    -> True
  _           -> False

-- | Is this type a function type (e.g. @X -> Y@ or @X => Y@),
-- after expanding type synonyms?
--
-- Note:
--
--  - Does not look through newtypes, so 'isFunTy' is 'False' for @IO ()@,
--    despite the latter being operationally a function (after expanding newtypes).
--  - Does not reduce type family applications, so 'isFunTy' is 'False' for
--    @Id (Int -> Bool)@ (where 'Id' is the identity type family).
--  - Does not consider @ForAllTy@ to be a function type (even when the argument
--    is a required argument), nor does it look through foralls.
--    Hence both 'isFunTy' is 'False' for both @forall b. Show b => b -> b@
--    and for @forall a -> Int@.
--
-- See also 'mightBeFunTy', when you want to know whether something could
-- be a function (taking a value argument) at runtime.
isFunTy :: Type -> Bool
isFunTy ty
  | FunTy {} <- coreFullView ty = True
  | otherwise                   = False

-- | Take a forall type apart, or panics if that is not possible.
splitForAllTyCoVar :: Type -> (Matchability, TyCoVar, Type)
splitForAllTyCoVar ty
  | Just answer <- splitForAllTyCoVar_maybe ty = answer
  | otherwise                                  = pprPanic "splitForAllTyCoVar" (ppr ty)

-- | Drops all ForAllTys
dropForAlls :: Type -> Type
dropForAlls ty = go ty
  where
    go (ForAllTy _ _ res)            = go res
    go ty | Just ty' <- coreView ty = go ty'
    go res                         = res

-- | Attempts to take a ForAllTy apart, returning the full ForAllTyBinder
splitForAllForAllTyBinder_maybe :: Type -> Maybe (Matchability, ForAllTyBinder, Type)
splitForAllForAllTyBinder_maybe ty
  | ForAllTy ma bndr inner_ty <- coreFullView ty = Just (ma, bndr, inner_ty)
  | otherwise                                 = Nothing


-- | Attempts to take a ForAllTy apart, returning the Var
splitForAllTyCoVar_maybe :: Type -> Maybe (Matchability, TyCoVar, Type)
splitForAllTyCoVar_maybe ty
  | ForAllTy ma (Bndr tv _) inner_ty <- coreFullView ty = Just (ma, tv, inner_ty)
  | otherwise                                        = Nothing

-- | Attempts to take a ForAllTy apart, but only if the binder is a TyVar
splitForAllTyVar_maybe :: Type -> Maybe (Matchability, TyVar, Type)
splitForAllTyVar_maybe ty
  | ForAllTy ma (Bndr tv _) inner_ty <- coreFullView ty
  , isTyVar tv
  = Just (ma, tv, inner_ty)

  | otherwise = Nothing

-- | Like 'splitForAllTyCoVar_maybe', but only returns Just if it is a covar binder.
splitForAllCoVar_maybe :: Type -> Maybe (Matchability, CoVar, Type)
splitForAllCoVar_maybe ty
  | ForAllTy ma (Bndr tv _) inner_ty <- coreFullView ty
  , isCoVar tv
  = Just (ma, tv, inner_ty)

  | otherwise = Nothing

-- | Attempts to take a forall type apart; works with proper foralls and
-- functions
{-# INLINE splitPiTy_maybe #-}  -- callers will immediately deconstruct
splitPiTy_maybe :: Type -> Maybe (Matchability, PiTyBinder, Type)
splitPiTy_maybe ty = case coreFullView ty of
  ForAllTy ma bndr ty -> Just (ma, Named bndr, ty)
  FunTy { ft_af = af, ft_ma = ma, ft_mult = w, ft_arg = arg, ft_res = res }
    -> Just (ma,Anon (mkScaled w arg) af,res)
  _ -> Nothing

-- | Split one binder and preserve its matchability.
splitPiTy :: Type -> (Matchability, PiTyBinder, Type)
splitPiTy ty
  | Just answer <- splitPiTy_maybe ty = answer
  | otherwise = pprPanic "splitPiTy" (ppr ty)

-- | Split a telescope with one matchability for each binder.
splitPiTys :: Type -> ([Matchability], [PiTyBinder], Type)
splitPiTys ty = split ty ty [] []
  where
    split _ (ForAllTy ma b res) mas bs = split res res (ma:mas) (Named b:bs)
    split _ (FunTy { ft_af = af, ft_ma = ma, ft_mult = w, ft_arg = arg, ft_res = res }) mas bs
      = split res res (ma:mas) (Anon (Scaled w arg) af:bs)
    split orig_ty ty mas bs | Just ty' <- coreView ty = split orig_ty ty' mas bs
    split orig_ty _ mas bs = (reverse mas,reverse bs,orig_ty)

splitUninstantiatedValArgTys :: Type -> [Type]
-- This function takes a type apart, returning all its /value/
-- argument types (including (forall (co::t1~t2).t)
-- doing no instantiation at all.
-- E.g. splitUinstantiatedValArgTys (forall a. a -> Int -> T a)
--         returns [a, Int]
-- It's a pretty specialised function, hence its long name
splitUninstantiatedValArgTys ty = go ty
  where
    go (ForAllTy _ (Bndr b _) res) | isCoVar b  = varType b : go res
                                 | otherwise  = go res
    go (FunTy { ft_arg = arg, ft_res = res }) = arg : go res
    go ty | Just ty' <- coreView ty = go ty'
    go _ = []

-- | Extracts a list of run-time arguments from a function type,
-- looking through newtypes to the right of arrows.
--
-- Examples:
--
-- @
--    newtype Identity a = I a
--
--    getRuntimeArgTys (Int -> Bool -> Double) == [(Int, FTF_T_T), (Bool, FTF_T_T)]
--    getRuntimeArgTys (Identity Int -> Bool -> Double) == [(Identity Int, FTF_T_T), (Bool, FTF_T_T)]
--    getRuntimeArgTys (Int -> Identity (Bool -> Identity Double)) == [(Int, FTF_T_T), (Bool, FTF_T_T)]
--    getRuntimeArgTys (forall a. Show a => Identity a -> a -> Int -> Bool)
--             == [(Show a, FTF_C_T), (Identity a, FTF_T_T),(a, FTF_T_T),(Int, FTF_T_T)]
-- @
--
-- Note that, in the last case, the returned types might mention an out-of-scope
-- type variable. This function is used only when we really care about the /kinds/
-- of the returned types, so this is OK.
--
-- **Warning**: this function can return an infinite list. For example:
--
-- @
--   newtype N a = MkN (a -> N a)
--   getRuntimeArgTys (N a) == repeat (a, FTF_T_T)
-- @
getRuntimeArgTys :: Type -> [(Scaled Type, FunTyFlag)]
getRuntimeArgTys = go
  where
    go :: Type -> [(Scaled Type, FunTyFlag)]
    go (ForAllTy _ _ res)
      = go res
    go (FunTy { ft_mult = w, ft_arg = arg, ft_res = res, ft_af = af })
      = (Scaled w arg, af) : go res
    go ty
      | Just ty' <- coreView ty
      = go ty'
      | Just (_,ty') <- topNormaliseNewType_maybe ty
      = go ty'
      | otherwise
      = []

invisibleBndrCount :: Type -> Int
-- Returns the number of leading invisible forall'd binders in the type
-- Includes invisible predicate arguments; e.g. for
--    e.g.  forall {k}. (k ~ *) => k -> k
-- returns 2 not 1
invisibleBndrCount ty = case splitInvisPiTys ty of (_, bs, _) -> length bs

-- | Like 'splitPiTys', but returns only *invisible* binders, including constraints.
-- Stops at the first visible binder.
splitInvisPiTys :: Type -> ([Matchability], [PiTyBinder], Type)
splitInvisPiTys ty = split ty ty [] []
   where
    split _ (ForAllTy ma b res) mas bs
      | Bndr _ vis <- b
      , isInvisibleForAllTyFlag vis = split res res (ma:mas) (Named b:bs)
    split _ (FunTy { ft_af = af, ft_ma = ma, ft_mult = mult, ft_arg = arg, ft_res = res }) mas bs
      | isInvisibleFunArg af = split res res (ma:mas) (Anon (mkScaled mult arg) af:bs)
    split orig_ty ty mas bs
      | Just ty' <- coreView ty = split orig_ty ty' mas bs
    split orig_ty _ mas bs = (reverse mas, reverse bs, orig_ty)

splitInvisPiTysN :: Int -> Type -> ([Matchability], [PiTyBinder], Type)
-- ^ Same as 'splitInvisPiTys', but stop when
--   - you have found @n@ 'PiTyBinder's,
--   - or you run out of invisible binders
splitInvisPiTysN n ty = split n ty ty [] []
   where
    split n orig_ty ty mas bs
      | n == 0 = (reverse mas, reverse bs, orig_ty)
      | Just ty' <- coreView ty = split n orig_ty ty' mas bs
      | ForAllTy ma b res <- ty
      , Bndr _ vis <- b
      , isInvisibleForAllTyFlag vis = split (n-1) res res (ma:mas) (Named b:bs)
      | FunTy { ft_af = af, ft_ma = ma, ft_mult = mult, ft_arg = arg, ft_res = res } <- ty
      , isInvisibleFunArg af = split (n-1) res res (ma:mas) (Anon (Scaled mult arg) af:bs)
      | otherwise = (reverse mas, reverse bs, orig_ty)

-- | Given a 'TyCon' and a list of argument types, filter out any invisible
-- (i.e., 'Inferred' or 'Specified') arguments.
filterOutInvisibleTypes :: TyCon -> [Type] -> [Type]
filterOutInvisibleTypes tc tys = snd $ partitionInvisibleTypes tc tys

-- | Given a 'TyCon' and a list of argument types, filter out any 'Inferred'
-- arguments.
filterOutInferredTypes :: TyCon -> [Type] -> [Type]
filterOutInferredTypes tc tys =
  filterByList (map (not . isInferredForAllTyFlag) $ tyConForAllTyFlags tc tys) tys

-- | Given a 'TyCon' and a list of argument types, partition the arguments
-- into:
--
-- 1. 'Inferred' or 'Specified' (i.e., invisible) arguments and
--
-- 2. 'Required' (i.e., visible) arguments
partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])
partitionInvisibleTypes tc tys =
  partitionByList (map isInvisibleForAllTyFlag $ tyConForAllTyFlags tc tys) tys

-- | Given a list of things paired with their visibilities, partition the
-- things into (invisible things, visible things).
partitionInvisibles :: [(a, ForAllTyFlag)] -> ([a], [a])
partitionInvisibles = partitionWith pick_invis
  where
    pick_invis :: (a, ForAllTyFlag) -> Either a a
    pick_invis (thing, vis) | isInvisibleForAllTyFlag vis = Left thing
                            | otherwise              = Right thing

-- | Given a 'TyCon' and a list of argument types to which the 'TyCon' is
-- applied, determine each argument's visibility
-- ('Inferred', 'Specified', or 'Required').
--
-- Wrinkle: consider the following scenario:
--
-- > T :: forall k. k -> k
-- > tyConForAllTyFlags T [forall m. m -> m -> m, S, R, Q]
--
-- After substituting, we get
--
-- > T (forall m. m -> m -> m) :: (forall m. m -> m -> m) -> forall n. n -> n -> n
--
-- Thus, the first argument is invisible, @S@ is visible, @R@ is invisible again,
-- and @Q@ is visible.
tyConForAllTyFlags :: TyCon -> [Type] -> [ForAllTyFlag]
tyConForAllTyFlags tc = fun_kind_arg_flags (tyConKind tc)

-- | Given a 'Type' and a list of argument types to which the 'Type' is
-- applied, determine each argument's visibility
-- ('Inferred', 'Specified', or 'Required').
--
-- Most of the time, the arguments will be 'Required', but not always. Consider
-- @f :: forall a. a -> Type@. In @f Type Bool@, the first argument (@Type@) is
-- 'Specified' and the second argument (@Bool@) is 'Required'. It is precisely
-- this sort of higher-rank situation in which 'appTyForAllTyFlags' comes in handy,
-- since @f Type Bool@ would be represented in Core using 'AppTy's.
-- (See also #15792).
appTyForAllTyFlags :: Type -> [Type] -> [ForAllTyFlag]
appTyForAllTyFlags ty = fun_kind_arg_flags (typeKind ty)

-- | Given a function kind and a list of argument types (where each argument's
-- kind aligns with the corresponding position in the argument kind), determine
-- each argument's visibility ('Inferred', 'Specified', or 'Required').
fun_kind_arg_flags :: Kind -> [Type] -> [ForAllTyFlag]
fun_kind_arg_flags = go emptySubst
  where
    go subst ki arg_tys
      | Just ki' <- coreView ki = go subst ki' arg_tys
    go _ _ [] = []
    go subst (ForAllTy _ (Bndr tv argf) res_ki) (arg_ty:arg_tys)
      = argf : go subst' res_ki arg_tys
      where
        subst' = extendTvSubst subst tv arg_ty
    go subst (TyVarTy tv) arg_tys
      | Just ki <- lookupTyVar subst tv = go subst ki arg_tys
    -- This FunTy case is important to handle kinds with nested foralls, such
    -- as this kind (inspired by #16518):
    --
    --   forall {k1} k2. k1 -> k2 -> forall k3. k3 -> Type
    --
    -- Here, we want to get the following ForAllTyFlags:
    --
    -- [Inferred,   Specified, Required, Required, Specified, Required]
    -- forall {k1}. forall k2. k1 ->     k2 ->     forall k3. k3 ->     Type
    go subst (FunTy{ft_af = af, ft_res = res_ki}) (_:arg_tys)
      = argf : go subst res_ki arg_tys
      where
        argf | isVisibleFunArg af = Required
             | otherwise          = Inferred
    go _ _ arg_tys = map (const Required) arg_tys
                        -- something is ill-kinded. But this can happen
                        -- when printing errors. Assume everything is Required.

-- @isTauTy@ tests if a type has no foralls or (=>)
isTauTy :: Type -> Bool
isTauTy ty | Just ty' <- coreView ty = isTauTy ty'
isTauTy (TyVarTy _)       = True
isTauTy (ClassAppTy _ inst dict operand kind) = all isTauTy [inst, dict, operand, kind]
isTauTy DepDictTy{} = False
isTauTy (TermAppTy _ arg) = isTauTy arg
isTauTy (TermVarTy _)     = True
isTauTy ty@(ExistsTy _ binder body)
  = isTauTy (varType binder) && isTauTy body
    && noFreeVarsOfType ty && isFamFreeTy ty
    -- See Note [Closed existential packages in type inference].
isTauTy (DepFunTy {})     = False
isTauTy (LitTy {})        = True
isTauTy (TyConApp tc tys) = all isTauTy tys && isTauTyCon tc
isTauTy (AppTy a b)       = isTauTy a && isTauTy b
isTauTy (FunTy { ft_af = af, ft_ma = ma, ft_mult = w, ft_arg = a, ft_res = b })
 | isInvisibleFunArg af   = False                               -- e.g., Eq a => b
 | otherwise              = isTauTy ma && isTauTy w && isTauTy a && isTauTy b -- e.g., a -> b
isTauTy (ForAllTy {})     = False
isTauTy (CastTy ty _)     = isTauTy ty
isTauTy (CoercionTy _)    = False  -- Not sure about this

{- Note [Closed existential packages in type inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A first-class existential package does not instantiate its bound witness
when a polymorphic function returns the package. It is safe to assign
a result metavariable to a closed package with a tau payload. The binder
is not a forall, and the payload stays inside its scope.

This first implementation only admits closed, family-free packages. A
slow unification check can return reflexivity for the whole type.
There are no free variables to promote or check for escape. The solver
does not rewrite the payload or use a forall coercion for an existential.
Continue to reject polymorphic payloads on the ordinary tau path.
-}

isAtomicTy :: Type -> Bool
-- True if the type is just a single token, and can be printed compactly
-- Used when deciding how to lay out type error messages; see the
-- call in GHC.Tc.Errors
isAtomicTy (TyVarTy {})    = True
isAtomicTy ClassAppTy{} = False
isAtomicTy DepDictTy{} = False
isAtomicTy (TermAppTy {})  = False
isAtomicTy (TermVarTy {})  = True
isAtomicTy (LitTy {})      = True
isAtomicTy (TyConApp _ []) = True

isAtomicTy ty | isLiftedTypeKind ty = True
   -- 'Type' prints compactly as *
   -- See GHC.Iface.Type.ppr_kind_type

isAtomicTy _ = False

{-
************************************************************************
*                                                                      *
\subsection{Type families}
*                                                                      *
************************************************************************
-}

mkFamilyTyConApp :: TyCon -> [Type] -> Type
-- ^ Given a family instance TyCon and its arg types, return the
-- corresponding family type.  E.g:
--
-- > data family T a
-- > data instance T (Maybe b) = MkT b
--
-- Where the instance tycon is :RTL, so:
--
-- > mkFamilyTyConApp :RTL Int  =  T (Maybe Int)
mkFamilyTyConApp tc tys
  | Just (fam_tc, fam_tys) <- tyConDataFamInst_maybe tc
  , let tvs = tyConTyVars tc
        fam_subst = assertPpr (tvs `equalLength` tys) (ppr tc <+> ppr tys) $
                    zipTvSubst tvs tys
  = mkTyConApp fam_tc (substTys fam_subst fam_tys)
  | otherwise
  = mkTyConApp tc tys

-- | Get the type on the LHS of a coercion induced by a type/data
-- family instance.
coAxNthLHS :: CoAxiom br -> Int -> Type
coAxNthLHS ax ind =
  mkTyConApp (coAxiomTyCon ax) (coAxBranchLHS (coAxiomNthBranch ax ind))

-- | A partial user family retains only known unmatchable type binders.
-- See Note [Partial type family applications].
isUnsaturatedUserFamily :: TyCon -> [Type] -> Bool
isUnsaturatedUserFamily tc args
  | user_family (tyConFlavour tc)
  , Nothing <- builtInClosedTyFamTyCon_maybe tc
  , let missing = tyConArity tc - length args
  , missing > 0 =
      unmatchable missing (piResultTys (tyConKind tc) args)
  | otherwise = False
  where
    -- Include checked TcTyCon headers in the initial kind-checking pass.
    -- Their real family TyCons only exist after this mutually recursive group.
    user_family ClosedTypeFamilyFlavour = True
    user_family (OpenFamilyFlavour IAmType _) = True
    user_family _ = False

    unmatchable 0 _ = True
    unmatchable n kind
      | ForAllTy ma (Bndr tv _) result <- coreFullView kind
      , isTyVar tv, ma `eqType` unmatchableDataConTy =
          unmatchable (n - 1) result
      | FunTy{ft_af = FTF_T_T, ft_ma = ma, ft_mult = mult, ft_res = result} <- coreFullView kind
      , isManyTy mult, ma `eqType` unmatchableDataConTy =
          unmatchable (n - 1) result
      | otherwise = False

{- Note [Partial type family applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each Flip xs can pass Flip as a function. The family axiom for Flip
expects an argument. Until the application reaches its arity, rewrite
its arguments by congruence and leave its head intact. Only a saturated
application can be a TyFamLHS or enter the family reduction cache.
Data family representation axioms have their own eta-reduced arity.

Check each missing binder in the instantiated kind, including its
matchability. A partial family can retain an invisible type binder
when its receiver already expects the entire polymorphic kind. It can
also retain a required type binder, as in a family with kind
forall k -> @U k %U -> Type. These binders quantify types.
Equality of applications at U gives no inverse for the
arguments. In particular, failure to split two partial applications
does not establish their apartness. The source validity check controls
which modules can write a partial family. Core Lint must also accept
a legal partial application obtained from an imported interface.
See proposal #242 and the R23 card in docs/dependent-haskell/roadmap.
-}

isFamFreeTy :: Type -> Bool
isFamFreeTy ty | Just ty' <- coreView ty = isFamFreeTy ty'
isFamFreeTy (TyVarTy _)       = True
isFamFreeTy (ClassAppTy _ inst dict operand kind) = all isFamFreeTy [inst, dict, operand, kind]
isFamFreeTy (DepDictTy b ty) = isFamFreeTy (varType b) && isFamFreeTy ty
isFamFreeTy (TermAppTy _ arg) = isFamFreeTy arg
isFamFreeTy (TermVarTy _)     = True
isFamFreeTy (ExistsTy _ b ty) = isFamFreeTy (varType b) && isFamFreeTy ty
isFamFreeTy (DepFunTy _ b ty) = isFamFreeTy (varType b) && isFamFreeTy ty
isFamFreeTy (LitTy {})        = True
isFamFreeTy (TyConApp tc tys) = all isFamFreeTy tys && isFamFreeTyCon tc
isFamFreeTy (AppTy a b)       = isMatchableFunTy (typeKind a) && isFamFreeTy a && isFamFreeTy b
isFamFreeTy (FunTy _ ma w a b) = isFamFreeTy ma && isFamFreeTy w && isFamFreeTy a && isFamFreeTy b
isFamFreeTy (ForAllTy ma _ ty)  = isFamFreeTy ma && isFamFreeTy ty
isFamFreeTy (CastTy ty _)     = isFamFreeTy ty
isFamFreeTy (CoercionTy _)    = False  -- Not sure about this

-- | Check whether a type is a data family type
isDataFamilyApp :: Type -> Bool
isDataFamilyApp ty = case tyConAppTyCon_maybe ty of
                           Just tc -> isDataFamilyTyCon tc
                           _       -> False

isSatTyFamApp :: Type -> Maybe (TyCon, [Type])
-- Return the argument if we have a saturated type family application
-- Why saturated?  See (ATF4) in Note [Apartness and type families]
isSatTyFamApp (TyConApp tc tys)
  |  isTypeFamilyTyCon tc
  && tys `lengthIs` tyConArity tc
  = Just (tc, tys)
isSatTyFamApp _ = Nothing

buildSynTyCon :: Name
              -> KnotTied Kind
              -> [KnotTied TyConBinder]
              -> Kind   -- ^ /result/ kind
              -> [Role] -> KnotTied Type -> TyCon
-- This function is here because here is where we have
--   isFamFree and isTauTy
buildSynTyCon name kind binders res_kind roles rhs
  = mkSynonymTyCon name kind binders res_kind roles rhs
                   is_tau is_fam_free is_forgetful is_concrete
  where
    qtvs         = mkVarSet (map binderVar binders)
    is_tau       = isTauTy rhs
    is_fam_free  = isFamFreeTy rhs
    is_concrete  = isConcreteTypeWith qtvs rhs
    is_forgetful = not (qtvs `subVarSet` expanded_rhs_tyvars)

    expanded_rhs_tyvars = tyCoVarsOfType (expandTypeSynonyms rhs)
       -- See Note [Forgetful type synonyms] in GHC.Core.TyCon
       -- To find out if this TyCon is forgetful, expand the synonyms in its RHS
       -- and check that all of the binders are free in the expanded type.
       -- We really only need to expand the /forgetful/ synonyms on the RHS,
       -- but we don't currently have a function to do that.
       -- Failing to expand the RHS led to #25094, e.g.
       --    type Bucket a b c = Key (a,b,c)
       --    type Key x = Any
       -- Here Bucket is definitely forgetful!

{-
************************************************************************
*                                                                      *
\subsection{Liftedness}
*                                                                      *
************************************************************************
-}

-- | Tries to compute the 'Levity' of the given type. Returns either
-- a definite 'Levity', or 'Nothing' if we aren't sure (e.g. the
-- type is representation-polymorphic).
--
-- Panics if the kind does not have the shape @TYPE r@.
typeLevity_maybe :: HasDebugCallStack => Type -> Maybe Levity
typeLevity_maybe ty = runtimeRepLevity_maybe (getRuntimeRep ty)

typeLevity :: HasDebugCallStack => Type -> Levity
typeLevity ty = case typeLevity_maybe ty of
                   Just lev -> lev
                   Nothing  -> pprPanic "typeLevity" (ppr ty)

-- | Is the given type definitely unlifted?
-- See "Type#type_classification" for what an unlifted type is.
--
-- Panics on representation-polymorphic types; See 'mightBeUnliftedType' for
-- a more approximate predicate that behaves better in the presence of
-- representation polymorphism.
isUnliftedType :: HasDebugCallStack => Type -> Bool
        -- isUnliftedType returns True for forall'd unlifted types:
        --      x :: forall a. Int#
        -- I found bindings like these were getting floated to the top level.
isUnliftedType ty =
  case typeLevity_maybe ty of
    Just Lifted   -> False
    Just Unlifted -> True
    Nothing       -> pprPanic "isUnliftedType" (ppr ty <+> dcolon <+> ppr (typeKind ty))

-- | Returns:
--
-- * 'False' if the type is /guaranteed/ unlifted or
-- * 'True' if it lifted, OR we aren't sure
--    (e.g. in a representation-polymorphic case)
mightBeLiftedType :: Type -> Bool
mightBeLiftedType = mightBeLifted . typeLevity_maybe

definitelyLiftedType :: Type -> Bool
definitelyLiftedType = not . mightBeUnliftedType

-- | Returns:
--
-- * 'False' if the type is /guaranteed/ lifted or
-- * 'True' if it is unlifted, OR we aren't sure
--    (e.g. in a representation-polymorphic case)
mightBeUnliftedType :: Type -> Bool
mightBeUnliftedType = mightBeUnlifted . typeLevity_maybe

definitelyUnliftedType :: Type -> Bool
definitelyUnliftedType = not . mightBeLiftedType

-- | See "Type#type_classification" for what a boxed type is.
-- Panics on representation-polymorphic types; See 'mightBeUnliftedType' for
-- a more approximate predicate that behaves better in the presence of
-- representation polymorphism.
isBoxedType :: Type -> Bool
isBoxedType ty = isBoxedRuntimeRep (getRuntimeRep ty)

-- | Is this a type of kind RuntimeRep? (e.g. LiftedRep)
isRuntimeRepKindedTy :: Type -> Bool
isRuntimeRepKindedTy = isRuntimeRepTy . typeKind

-- | Drops prefix of RuntimeRep constructors in 'TyConApp's. Useful for e.g.
-- dropping 'LiftedRep arguments of unboxed tuple TyCon applications:
--
--   dropRuntimeRepArgs [ 'LiftedRep, 'IntRep
--                      , String, Int# ] == [String, Int#]
--
dropRuntimeRepArgs :: [Type] -> [Type]
dropRuntimeRepArgs = dropWhile isRuntimeRepKindedTy

-- | Extract the RuntimeRep classifier of a type. For instance,
-- @getRuntimeRep_maybe Int = Just LiftedRep@. Returns 'Nothing' if this is not
-- possible.
getRuntimeRep_maybe :: HasDebugCallStack
                    => Type -> Maybe RuntimeRepType
getRuntimeRep_maybe = kindRep_maybe . typeKind

-- | Extract the RuntimeRep classifier of a type. For instance,
-- @getRuntimeRep_maybe Int = LiftedRep@. Panics if this is not possible.
getRuntimeRep :: HasDebugCallStack => Type -> RuntimeRepType
getRuntimeRep ty
  = case getRuntimeRep_maybe ty of
      Just r  -> r
      Nothing -> pprPanic "getRuntimeRep" (ppr ty <+> dcolon <+> ppr (typeKind ty))

-- | Extract the 'Levity' of a type. For example, @getLevity_maybe Int = Just Lifted@,
-- @getLevity (Array# Int) = Just Unlifted@, @getLevity Float# = Nothing@.
--
-- Returns 'Nothing' if this is not possible. Does not look through type family applications.
getLevity_maybe :: HasDebugCallStack => Type -> Maybe Type
getLevity_maybe ty
  | Just rep <- getRuntimeRep_maybe ty
  -- Directly matching on TyConApp after expanding type synonyms
  -- saves allocations compared to `splitTyConApp_maybe`. See #22254.
  -- Given that this is a pretty hot function we make use of the fact
  -- and use isTyConKeyApp_maybe instead.
  , Just [lev] <- isTyConKeyApp_maybe boxedRepDataConKey rep
  = Just lev
  | otherwise
  = Nothing

-- | Extract the 'Levity' of a type. For example, @getLevity Int = Lifted@,
-- or @getLevity (Array# Int) = Unlifted@.
--
-- Panics if this is not possible. Does not look through type family applications.
getLevity :: HasDebugCallStack => Type -> Type
getLevity ty
  | Just lev <- getLevity_maybe ty
  = lev
  | otherwise
  = pprPanic "getLevity" (ppr ty <+> dcolon <+> ppr (typeKind ty))

isUnboxedTupleType :: Type -> Bool
isUnboxedTupleType ty
  = tyConAppTyCon (getRuntimeRep ty) `hasKey` tupleRepDataConKey
  -- NB: Do not use typePrimRep, as that can't tell the difference between
  -- unboxed tuples and unboxed sums


isUnboxedSumType :: Type -> Bool
isUnboxedSumType ty
  = tyConAppTyCon (getRuntimeRep ty) `hasKey` sumRepDataConKey

-- | See "Type#type_classification" for what an algebraic type is.
-- Should only be applied to /types/, as opposed to e.g. partially
-- saturated type constructors
isAlgType :: Type -> Bool
isAlgType ty
  = case splitTyConApp_maybe ty of
      Just (tc, ty_args) -> assert (ty_args `lengthIs` tyConArity tc )
                            isAlgTyCon tc
      _other             -> False

-- | Computes whether an argument (or let right hand side) should
-- be computed strictly or lazily, based only on its type.
-- Currently, it's just 'isUnliftedType'.
-- Panics on representation-polymorphic types.
isStrictType :: HasDebugCallStack => Type -> Bool
isStrictType = isUnliftedType

isTerminatingType :: HasDebugCallStack => Type -> Bool
-- ^ True <=> a term of this type cannot be bottom
-- This identifies the types described by
--    Note [NON-BOTTOM-DICTS invariant] in GHC.Core
-- NB: unlifted types are not terminating types!
--     e.g. you can write a term (loop 1)::Int# that diverges.
isTerminatingType ty = case tyConAppTyCon_maybe ty of
    Just tc -> isTerminatingTyCon tc
    _       -> False

isPrimitiveType :: Type -> Bool
-- ^ Returns true of types that are opaque to Haskell.
isPrimitiveType ty = case splitTyConApp_maybe ty of
                        Just (tc, ty_args) -> assert (ty_args `lengthIs` tyConArity tc )
                                              isPrimTyCon tc
                        _                  -> False

{-
************************************************************************
*                                                                      *
\subsection{Join points}
*                                                                      *
************************************************************************
-}

-- | Determine whether a type could be the type of a join point of given total
-- arity, according to the polymorphism rule. A join point cannot be polymorphic
-- in its return type, since given
--   join j @a @b x y z = e1 in e2,
-- the types of e1 and e2 must be the same, and a and b are not in scope for e2.
-- (See Note [The polymorphism rule of join points] in "GHC.Core".) Returns False
-- also if the type simply doesn't have enough arguments.
--
-- Note that we need to know how many arguments (type *and* value) the putative
-- join point takes; for instance, if
--   j :: forall a. a -> Int
-- then j could be a binary join point returning an Int, but it could *not* be a
-- unary join point returning a -> Int.
--
-- See Note [Retained arguments of join points].
-- TODO: See Note [Excess polymorphism and join points]
isValidJoinPointType :: JoinArity -> Type -> Bool
isValidJoinPointType arity ty = isJust (joinPointResultType arity ty)

-- | Return the type shared by all jumps after consuming the join arguments.
-- See Note [Retained arguments of join points].
joinPointResultType :: JoinArity -> Type -> Maybe Type
joinPointResultType arity ty = valid_under emptyVarSet arity ty
  where
    valid_under tvs arity ty
      | arity == 0
      = if tvs `disjointVarSet` tyCoVarsOfType ty then Just ty else Nothing
      | Just (_, t, ty') <- splitForAllTyCoVar_maybe ty
      = valid_under (tvs `extendVarSet` t) (arity-1) ty'
      | DepFunTy _ index body <- coreFullView ty
      = valid_under (tvs `extendVarSet` index) (arity-1) body
      | Just (_, ma, _, _, res_ty) <- splitFunTy_maybe ty
      , eqType ma unmatchableDataConTy
        -- See Note [Ordinary arrow metadata and eta] in GHC.Core.Opt.Arity.
      = valid_under tvs (arity-1) res_ty
      | otherwise
      = Nothing

{- Note [Retained arguments of join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A join point can take a runtime index. Later arguments may depend on it.
For example, an alternative of Stored can produce a join of type

  foreach (b :: Bool) -> Value b -> Stored -> String

Count b as one value argument. Check the return type after consuming all
join arguments. It must mention none of the locally bound type variables
or runtime indices, since the jump returns to the caller's context.
For example, a join that returns Value b after consuming b is invalid.
-}

{- Note [Excess polymorphism and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In principle, if a function would be a join point except that it fails
the polymorphism rule (see Note [The polymorphism rule of join points] in
GHC.Core), it can still be made a join point with some effort. This is because
all tail calls must return the same type (they return to the same context!), and
thus if the return type depends on an argument, that argument must always be the
same.

For instance, consider:

  let f :: forall a. a -> Char -> [a]
      f @a x c = ... f @a y 'a' ...
  in ... f @Int 1 'b' ... f @Int 2 'c' ...

(where the calls are tail calls). `f` fails the polymorphism rule because its
return type is [a], where [a] is bound. But since the type argument is always
'Int', we can rewrite it as:

  let f' :: Int -> Char -> [Int]
      f' x c = ... f' y 'a' ...
  in ... f' 1 'b' ... f 2 'c' ...

and now we can make f' a join point:

  join f' :: Int -> Char -> [Int]
       f' x c = ... jump f' y 'a' ...
  in ... jump f' 1 'b' ... jump f' 2 'c' ...

It's not clear that this comes up often, however. TODO: Measure how often and
add this analysis if necessary.  See #14620.
-}

{- *********************************************************************
*                                                                      *
             Sequencing on types and coercions
*                                                                      *
********************************************************************* -}

{- Note [`seq` on types and coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We `seq` on types and coercions via `TyCoFolder`.

Note that (non-obviously) we will actually `seq` on some mostly-unchanging
fields, including:

* `TyCon` in `TyConApp` and `TyConAppCo`
* `CoAxiom` in `AxiomCo`
* `Role` in `FunCo`, `GRefl` etc
* `CoSel` in `SelCo`
* `LeftOrRight` in `LRCo`

because the `TyCoFolder` applies the (polymorphic) `tcf_lit` to them.
-}

seqType  :: Type -> SeqResult
seqTypes :: [Type] -> SeqResult
seqCo    :: Coercion -> SeqResult
seqCos   :: [Coercion] -> SeqResult
(seqType, seqTypes, seqCo, seqCos)
  = foldTyCo folder
  where
    folder :: TyCoFolder SeqResult
    folder = TyCoFolder { tcf_view       = noView
                        , tcf_tyvar      = seqUnit
                        , tcf_termvar    = seqUnit
                        , tcf_termbinder = \_ x -> x
                        , tcf_covar      = seqUnit
                        , tcf_lit        = seqUnit
                        , tcf_hole       = \h -> seqUnit (coHoleCoVar h)
                        , tcf_tycobinder = \ _tcv x -> x
                          -- TyCoFolder already traverses the kind of tcv,
                          -- so we don't need to look at it here
                        }

seqMCo :: MCoercion -> SeqResult
seqMCo MRefl    = mempty
seqMCo (MCo co) = seqCo co

{-
************************************************************************
*                                                                      *
        The kind of a type
*                                                                      *
************************************************************************

Note [Kinding rules for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here are the key kinding rules for types

          torc1 is TYPE or CONSTRAINT
          torc2 is TYPE or CONSTRAINT
          t1 : torc1 rep1
          t2 : torc2 rep2
   (FUN)  ----------------
          t1 -> t2 : torc2 LiftedRep
          -- In fact the arrow varies with torc1/torc2
          -- See Note [Function type constructors and FunTy]
          -- in GHC.Builtin.WiredIn.Prim

          torc is TYPE or CONSTRAINT
          ty : body_torc rep
          ki : Type
          `a` is a type variable
          `a` is not free in rep
(FORALL1) -----------------------
          forall (a::ki). ty : body_torc rep

          torc is TYPE or CONSTRAINT
          ty : body_torc rep
          `c` is a coercion variable
          `c` is not free in rep
          `c` is free in ty       -- Surprise 1!
(FORALL2) -------------------------
          forall (cv::k1 ~#{N,R} k2). ty : body_torc LiftedRep
                                           -- Surprise 2!

Note that:
* (FORALL1) rejects (forall (a::Maybe). blah)

* (FORALL2) Surprise 1:
  See GHC.Core.TyCo.Rep Note [Unused coercion variable in ForAllTy]

* (FORALL2) Surprise 2: coercion abstractions are not erased, so
  this must be LiftedRep, just like (FUN).  (FORALL2) is just a
  dependent form of (FUN).


Note [Phantom type variables in kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  type K (r :: RuntimeRep) = Type   -- Note 'r' is unused
  data T r :: K r                   -- T :: forall r -> K r
  foo :: forall r. T r

The body of the forall in foo's type has kind (K r), and
normally it would make no sense to have
   forall r. (ty :: K r)
because the kind of the forall would escape the binding
of 'r'.  But in this case it's fine because (K r) expands
to Type, so we explicitly /permit/ the type
   forall r. T r

To accommodate such a type, in typeKind (forall a.ty) we use
occCheckExpand to expand any type synonyms in the kind of 'ty'
to eliminate 'a'.  See kinding rule (FORALL) in
Note [Kinding rules for types]


See also
 * GHC.Core.Type.occCheckExpand
 * GHC.Core.Utils.coreAltsType
 * GHC.Tc.Validity.checkEscapingKind
all of which grapple with the same problem.

See #14939.
-}

-----------------------------
-- | Test whether a lifted algebraic type has a supported index schema.
-- See Note [Algebraic runtime index domains].
isNativeIndexType :: Type -> Bool
isNativeIndexType = nativeIndexType True

-- | Check a closed structural domain without opaque or retained fields.
isNativeClosedIndexType :: Type -> Bool
isNativeClosedIndexType = nativeIndexType False

-- | Check a structural schema or a closed function-free endpoint.
-- See Note [Function fields in native index domains].
nativeIndexType :: Bool -> Type -> Bool
nativeIndexType structural_fields = go []
  where
    go path ty
      | TyConApp tc args <- coreFullView ty
      , Just previous <- lookup tc path
      = eqTypes args previous
      | TyConApp tc args <- coreFullView ty
      , isVanillaAlgTyCon tc, isBoxedDataTyCon tc
      , not (isTypeDataTyCon tc), tyConArity tc == length args
      , tcIsLiftedTypeKind (typeKind ty)
      , null (tyConStupidTheta tc)
      , let cons = tyConDataCons tc
            package_root = structural_fields && null path && isSingleton cons
      , (all (tcIsLiftedTypeKind . tyVarKind) (tyConTyVars tc) && all argument args)
         || (package_root && any dataConHasPromotedRetained cons
             && and (zipWith (package_argument (tyConTyVars tc) args) (tyConTyVars tc) args))
      , Just fields <- concat <$> traverse (con_fields package_root ty args) cons
      = all (field ((tc, args) : path)) fields
      | otherwise = False

    field path (parameter, ty)
      | structural_fields
      , parameter
      = isNativeOpaqueValueType ty
      | structural_fields
      , isJust (nativeIndexFunctionTypeTys_maybe ty)
      = True
      | otherwise
      = go path ty

    argument arg
      | structural_fields = isNativeOpaqueValueType arg
      | otherwise = noFreeVarsOfType arg && isTauTy arg
                    && isNativeOpaqueValueType arg

    -- A checked root package alone can use an ordinary higher-kinded
    -- parameter. Its domain may name an earlier type parameter.
    package_argument parameters actuals tv arg
      | tcIsLiftedTypeKind (tyVarKind tv) = argument arg
      | Just (_, _, _, domain, range) <- splitFunTy_maybe (tyVarKind tv)
      , range `eqType` liftedTypeKind
      , isNativeClosedIndexType domain || maybe False (`elem` parameters) (getTyVar_maybe domain)
      = typeKind arg `eqType` substTyWith parameters actuals (tyVarKind tv)
      | otherwise = False

    con_fields package_root domain args con
      | package_root
      -- A witness declared at a type parameter is opaque after instantiation.
      -- See Note [Abstract witness domains].
      , dataConHasPromotedRetained con
      , (univs, [], [], [], _, res) <- dataConFullSig con
      , univs `equalLength` args
      , substTyWith univs args res `eqType` domain
      , null (dataConStupidTheta con)
      , Nothing <- dataConWrapId_maybe con
      , [witness, payload] <- rep_args
      = let field_type = substTyWith univs args . scaledThing
        in Just [(is_parameter (scaledThing witness), field_type witness), (True, field_type payload)]
      | null (dataConRepArgBinders con)
      , (univs, [], [], [], orig_args, res) <- dataConFullSig con
      , univs `equalLength` args
      , all (tcIsLiftedTypeKind . tyVarKind) univs
      , null (dataConStupidTheta con)
      , Nothing <- dataConWrapId_maybe con
      , substTyWith univs args res `eqType` domain
      , orig_args `equalLength` rep_args
      , and (zipWith same_field (instantiate orig_args) (instantiate rep_args))
      = Just (zipWith field_origin orig_args (instantiate rep_args))
      | otherwise = Nothing
      where
        rep_args = dataConRepArgTys con
        (univs, _, _, _, _, _) = dataConFullSig con
        instantiate = map (mapScaledType (substTyWith univs args))
        field_origin (Scaled _ orig) (Scaled _ ty)
          = (is_parameter orig, ty)
        is_parameter orig = case coreFullView orig of
          TyVarTy tv -> tv `elem` univs
          _         -> False

    same_field (Scaled orig_mult orig) (Scaled rep_mult rep)
      | closed_mult orig_mult, closed_mult rep_mult
      = orig `eqType` rep
      | otherwise = False

    closed_mult ty = case coreFullView ty of
      OneTy -> True
      ManyTy -> True
      _ -> False

-- | Recover the ordinary universal arguments of a supported index constructor.
-- See Note [Algebraic runtime index domains].
nativeIndexDataConTyArgs :: Type -> DataCon -> Maybe [Type]
nativeIndexDataConTyArgs domain con
  | isNativeIndexType domain
  , TyConApp tc args <- coreFullView domain
  , tc == dataConTyCon con
  = Just args
  | otherwise = Nothing

-- | Return the independently closed argument and result domains of a native head.
-- See Note [Opaque native call indices] in GHC.Core.TyCo.Rep.
nativeIndexFunctionTys_maybe :: Id -> Maybe (Type, Type)
nativeIndexFunctionTys_maybe f
  | isLocalId f, not (isCoVar f), isManyTy (idMult f)
  , not (isJoinIdDetails (idDetails f))
  , noFreeVarsOfType (idMult f)
  = nativeIndexFunctionTypeTys_maybe (varType f)
  | otherwise = Nothing

-- | Check the closed ordinary function type shared by heads and retained binders.
-- See Note [Function-valued retained binders].
nativeIndexFunctionTypeTys_maybe :: Type -> Maybe (Type, Type)
nativeIndexFunctionTypeTys_maybe ty
  | noFreeVarsOfType ty
  , FunTy { ft_af = af, ft_ma = ma, ft_mult = mult, ft_arg = arg, ft_res = res }
      <- coreFullView ty
  , af == FTF_T_T, eqType ma unmatchableDataConTy, isManyTy mult
  , noFreeVarsOfType mult
  , isNativeClosedIndexType arg, isNativeClosedIndexType res
  = Just (arg, res)
  | otherwise = Nothing

-- | Test whether an identifier is an eligible opaque native call head.
isNativeIndexFunctionId :: Id -> Bool
isNativeIndexFunctionId = isJust . nativeIndexFunctionTys_maybe

{- Note [Int# family pattern binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A closed type family equation can match the field of promoted I#. The
equation binds an erased Int# index only where its pattern has the form
'I# x. The binder cannot occur in the right-hand side. These rules preserve
the match at a known constructor application and leave a neutral Int stuck.

The axiom binds the variable without introducing a ForAllTy. The ordinary
FORALL1 rule and runtime index readback keep their existing scope. See
GHC.Tc.Gen.HsType for source binding and GHC.Core.Lint for axiom checking.
-}

-- | Check the scope of a raw field variable in a closed family axiom.
isIntPrimFamilyPatBinder :: [Type] -> Type -> TyVar -> Bool
isIntPrimFamilyPatBinder pats rhs var
  = isTyVar var && varType var `eqType` intPrimTy
    && any int_pattern pats
    && all (\pat -> int_pattern pat || not (occurs_in pat)) pats
    && not (occurs_in rhs)
  where
    occurs_in ty = var `elemVarSet` tyCoVarsOfType ty
    int_pattern (TyConApp tc [TyVarTy field]) = tc `hasKey` intDataConKey && field == var
    int_pattern _ = False

-- | Test whether a type can classify an opaque runtime value index.
-- See Note [Opaque runtime index values].
isNativeOpaqueValueType :: Type -> Bool
isNativeOpaqueValueType = tcIsLiftedTypeKind . typeKind

-- | Test the metadata of an opaque local unrestricted runtime value.
-- The caller must check its actual lexical binding.
-- See Note [Opaque runtime index values].
isNativeOpaqueValueId :: Id -> Bool
isNativeOpaqueValueId v
  = isLocalId v && not (isCoVar v)
    && not (isJoinIdDetails (idDetails v))
    && isManyTy (idMult v) && isNativeOpaqueValueType (varType v)

-- | A local unrestricted class dictionary eligible for a dependent binder.
isNativeClassDictionaryId :: Id -> Bool
isNativeClassDictionaryId v
  = isLocalId v && not (isCoVar v) && not (isJoinIdDetails (idDetails v))
    && isManyTy (idMult v)
    && case coreFullView (varType v) of
         TyConApp tc _ -> isJust (tyConClass_maybe tc)
         _ -> False

-- | Check nominal index-conversion endpoints without checking the evidence.
-- Eligible native function heads retain their complete fixed type.
-- See Note [Opaque runtime index values].
isNativeIndexDomainConversion :: Type -> Type -> Bool
isNativeIndexDomainConversion source target
  = isNativeOpaqueValueType source && isNativeOpaqueValueType target
    && (not (fixed_function source || fixed_function target) || source `eqType` target)
  where
    fixed_function = isJust . nativeIndexFunctionTypeTys_maybe

{- Note [Opaque runtime index values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An opaque index records the identity of an actual local Many value Id. Its
type must have kind Type, not Constraint or an unlifted kind. Keep its complete
type, Name and Unique. The type can contain outer erased variables and earlier
runtime indices. The caller must check that scope and the binding's actual
metadata. Do not inspect the value's RHS or its constructors.

Opaque identity does not require a structural domain. A parameter field of
List a stays opaque when a becomes Int, IO Int or Vec Int n. The list tail
still has the structural domain List a. Derive that distinction from the
original constructor telescope. See Note [Algebraic runtime index domains].

Opaque identity does not grant native call permission. Keep the independently
closed function policy in nativeIndexFunctionTypeTys_maybe. A nominal value
conversion can change an opaque domain. If either endpoint has an eligible
native function type, require the same complete type at both endpoints.
Other function values can remain opaque fields. The conversion predicate
checks endpoints only; its caller must retain nominal evidence and direction.

Core retained value binders and occurrences use the opaque Id judgment.
Source foreach roots use isNativeRetainedBinder instead. See
Note [Opaque algebraic retained binders] and Note [Abstract witness domains].
Both checks leave scope and nominal domain equality to their callers.
-}

-- | Test whether a local unrestricted identifier has a supported index domain.
isNativeIndexId :: Id -> Bool
isNativeIndexId v
  = isNativeOpaqueValueId v && isNativeIndexType (varType v)

-- | Test whether an Id can bind a source value index or a fixed call head.
-- See Note [Opaque algebraic retained binders].
isNativeRetainedBinder :: Id -> Bool
isNativeRetainedBinder v
  = (isNativeOpaqueValueId v && isNativeAlgebraicOrAbstractDomain (varType v))
    || isNativeIndexFunctionId v

-- | Test a source retained-root domain without constructing a temporary Id.
-- See Note [Opaque algebraic retained binders].
isNativeRetainedBinderType :: Type -> Bool
isNativeRetainedBinderType ty
  = (isNativeOpaqueValueType ty && isNativeAlgebraicOrAbstractDomain ty)
    || isJust (nativeIndexFunctionTypeTys_maybe ty)

-- | Check the Id stored in a retained constructor field telescope.
-- See Note [Opaque algebraic retained binders].
isNativeRetainedField :: Id -> Bool
isNativeRetainedField v
  = isNativeIndexId v || isNativeIndexFunctionId v
    || (isTyVarTy (varType v) && isNativeOpaqueValueId v)

-- | Check the domain of a retained constructor field before making its Id.
-- See Note [Opaque algebraic retained binders].
isNativeRetainedFieldType :: Type -> Bool
isNativeRetainedFieldType ty
  = isNativeIndexType ty || isJust (nativeIndexFunctionTypeTys_maybe ty)
    || (isTyVarTy ty && isNativeOpaqueValueType ty)

-- | Test the head of an opaque value domain after expanding synonyms.
-- The caller checks the full domain's lifted kind.
isNativeAlgebraicOrAbstractDomain :: Type -> Bool
isNativeAlgebraicOrAbstractDomain ty = case coreFullView ty of
  TyVarTy {} -> True
  TyConApp tc args
    | isDataFamilyTyCon tc -> tyConArity tc <= length args
    | otherwise -> isVanillaAlgTyCon tc && isBoxedDataTyCon tc && tyConArity tc == length args
  _ -> False

{- Note [Opaque algebraic retained binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained binder at a lifted type parameter can already instantiate that
parameter to Vec a n. Allow the same opaque binder when the signature names
that domain directly:

  vectorIdentity :: foreach (xs :: Vec a n) -> xs :~: xs

The result mentions the local Id. Binding that Id uses the domain's type
and requires no traversal of its constructors or their equalities. Use
the saturated, lifted boxed data head. A synonym can expand to such a head.
The same rule applies to a saturated data family application of kind Type.
The binder uses the public type without selecting its representation.
A data family can have additional arguments after its declared arity.

The binder does not reify constructors as indices. Constructor
reification and structural matching still require isNativeIndexType.
Function calls still use the fixed head check in
Note [Function-valued retained binders].

A named constructor field remains subject to the structural domain policy.
Its checked telescope can also supply the fields of a promoted retained
constructor. Keep the existing abstract witness and fixed function cases.
That policy rejects a structural cycle through an unsupported retained
telescope. The value-binder check only uses the outer lifted data head.
Use the field predicate in source and interface constructor checking and
when building DataCon metadata.

Agda's checkPiTelescope checks the domain and extends the context for the
result. See docs/dependent-haskell/REFERENCES.md#agda-provenance. The GHC
binder preserves the runtime argument and its laziness. Two distinct
binders at the same domain still name distinct values.
-}

{- Note [Abstract witness domains]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the constructor

  Pair :: foreach (x :: a) -> p x -> DPair a p

the type parameter a has kind Type. The second field refers to the
identity of x. It needs neither the constructors of a nor an equation
for x. Check the witness as an unrestricted local Id at this domain.
Keep the existing checks for constructor refinement and for call heads.

When matching Pair at a concrete type, the declaration has already
established which field binds x. For example, matching a DPair Int p
binds a local Int witness. Check the instantiated field as an opaque Id.
Requiring Int to be a structural index domain here would prevent an
ordinary client from opening that package.
-}

{- Note [Function fields in native index domains]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An algebraic index can contain an eligible opaque function field.
For example, Maybe (Bool -> Bool) has a Nothing alternative and a Just field
with a closed ordinary U/Many arrow. Keep all other algebraic-domain checks.
The function remains a runtime Id. Its TermVarTy records identity without
evaluating the function or comparing its behavior.

Function arguments and results keep the old closed function-free algebraic
policy. nativeIndexType False checks every instantiated field structurally.
It requires closed monotype parameters and disables opaque parameter fields.
Thus List Int can be a structural root without becoming a native function
endpoint. Do not call the broader public
predicate from nativeIndexFunctionTypeTys_maybe: a domain such as

  data Loop = Loop (Loop -> Bool)

would then restart eligibility checking through its function argument.
The function-free endpoint check rejects Loop without this recursive restart.
This restriction is separate from regular algebraic recursion.

Use isNativeOpaqueValueId for TermVarTy occurrences. isNativeIndexId still
checks algebraic-root identifiers. The source retained-binder predicate also
checks local U/Many function heads and their complete fixed types. See
Note [Native function-value application] in GHC.Core.TyCo.Make for application
and forward evidence.
-}

{- Note [Function-valued retained binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A source foreach root can bind an opaque algebraic or abstract value,
or an eligible opaque call head. Core retained value binders use the
opaque Id judgment. See Note [Opaque runtime index values].
The function head has a closed ordinary visible Many arrow A -> B. Both A and
B are independently supported scalar domains. The Id is local, unrestricted,
and not a join point. Keep the head policy separate from structural schemas.

A bound function can occur in the head slot of TermAppTy or TermAppCo, or as
a TermVarTy stored in an algebraic index. Application of that TermVarTy uses
the same opaque TermAppTy form. It is not a matchable function. Applying its
retained binder requires an eligible local Var with exactly the same type.
Use the existing head-only Id substitution. Do not unfold that Var or transport
the function domain through a coercion. DepFunCo keeps eligible call-head domains
unchanged. Value-domain conversion uses nominal evidence; see
Note [Retained dependent function coercions] in GHC.Core.TyCo.Rep.

The binder scopes over calls in the result and its proofs. Existing runtime
binder renaming masks outer substitutions and avoids capture in both places.
The function argument remains a lazy runtime argument. A type occurrence does
not force it or add a runtime use. No function equality or inverse call rule
follows from this binder policy.

See the pinned Agda context and neutral-call comparison in
docs/dependent-haskell/REFERENCES.md, under
head-only-function-valued-retained-binders-prior-art.
-}

{- Note [Algebraic runtime index domains]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A structural runtime index has a lifted algebraic data type. Inspect ordinary
TyCons and DataCons; Nat has no wired identity. Its ordinary universal arguments
must have kind Type. They can contain outer erased variables and earlier
runtime indices. The caller checks their full types and scope.

Check the original constructor telescope before instantiating its universals.
A field that expands to one of those universal variables is an opaque parameter
field. Require only a lifted value type after instantiation. Thus the cons head
stays opaque at Int or Vec Int n. Do not inspect Int's unlifted field or Vec's
constructors. The cons tail List a is a structural field. A nested Maybe a
uses Maybe's own constructor telescope. Keep structural traversal for fixed
concrete fields. A datatype with a fixed Int field need not be a structural
root for its values to occur opaquely in a list. No stored flag is needed.

Other fields retain the existing structural or eligible function-leaf policy.
Check the instantiated original and representation fields for exact agreement.
The strict function endpoint mode disables parameter opacity and still requires
closed monotypes. See Note [Function fields in native index domains].

Track full datatype applications along the active path. A return to the same
TyCon must have nominally equal arguments. Use ordinary eqType, which requires
a SOURCE import because type comparison uses this module. Separate siblings can
use different instances. This uniform-recursion rule admits List Bool and
mutually recursive uniform domains. It rejects growing arguments and parameter
permutations. It does not assume finite or total runtime values.

Reject existential variables, constructor constraints, datatype
contexts, newtypes, type data, and wrappers. Inspect retained field metadata
before the full signature. A recursive interface field can otherwise force the
same domain thunk which is already being checked.

A structural root may have one constructor with a checked promoted witness
telescope. Require a closed ordinary structural type for its witness, as in
Note [Promoting one retained witness] in GHC.Core.DataCon. This check excludes
retained fields when inspecting the witness domain itself. The dependent
payload is opaque to structural inspection, since the promoted telescope
checks its exact kind. Do not admit retained constructors in nested
structural fields or in strict function endpoints.

Original and representation fields must have the same complete domain. Thus a
promoted constructor's arguments match
its worker's runtime fields. In particular, do not promote an unpacked payload as
if it were the original boxed field. Constructor field multiplicities may be One
or Many; the retained identifier itself must be unrestricted.

Agda's constructor indices and refined field context are the precedent. The
schema and worker restrictions bound this implementation; they are not
Agda language restrictions. Haskell fields can be partial or cyclic. Do not use
termination or finite-tree apartness to compare opaque runtime indices.
See the Agda Nat/Vec provenance in docs/dependent-haskell/REFERENCES.md.
-}

-- | Test whether an identifier can occur as a retained Boolean index.
isBoolIndexId :: Id -> Bool
isBoolIndexId v
  | isLocalId v, not (isCoVar v)
  , TyConApp tc [] <- coreFullView (varType v)
  , tc `hasKey` boolTyConKey
  , ManyTy <- coreFullView (idMult v)
  = True
  | otherwise = False

typeKind :: HasDebugCallStack => Type -> Kind
-- No need to expand synonyms
typeKind (TyConApp tc tys)      = piResultTys (tyConKind tc) tys
typeKind (LitTy l)              = typeLiteralKind l
typeKind (FunTy { ft_af = af }) = liftedTypeOrConstraintKind (funTyFlagResultTypeOrConstraint af)
typeKind (TyVarTy tyvar)        = tyVarKind tyvar
typeKind (TermAppTy f _)        = case nativeIndexFunctionTys_maybe f of
  Just (_, res) -> res
  Nothing -> pprPanic "typeKind: invalid native function" (ppr f)
typeKind (TermVarTy v)          = varType v
typeKind (ExistsTy {})           = liftedTypeKind
typeKind (DepFunTy {})          = liftedTypeKind
typeKind (DepDictTy {})         = liftedTypeKind
typeKind (ClassAppTy _ _ _ _ kind) = kind
typeKind (CastTy _ty co)        = coercionRKind co
typeKind (CoercionTy co)        = coercionType co

typeKind (AppTy fun arg)
  = go fun [arg]
  where
    -- Accumulate the type arguments, so we can call piResultTys,
    -- rather than a succession of calls to piResultTy (which is
    -- asymptotically costly as the number of arguments increases)
    go (AppTy fun arg) args = go fun (arg:args)
    go fun             args = piResultTys (typeKind fun) args

typeKind ty@(ForAllTy {})
  = assertPpr (not (null tcvs)) (ppr ty) $
       -- If tcvs is empty somehow we'll get an infinite loop!
    case occCheckExpand tcvs body_kind of
      -- We must make sure tvs do not occur in kind,
      -- as they would be out of scope!
      -- See Note [Phantom type variables in kinds]
      Nothing -> pprPanic "typeKind"
                  (ppr ty $$ ppr tcvs $$ ppr body <+> dcolon <+> ppr body_kind)

      Just k' | all isTyVar tcvs -> k'                     -- Rule (FORALL1)
              | otherwise        -> lifted_kind_from_body  -- Rule (FORALL2)
  where
    (annotated_tcvs, body) = splitForAllTyCoVars ty
    tcvs = map snd annotated_tcvs  -- Important: splits both TyVar and CoVar binders
    body_kind    = typeKind body

    lifted_kind_from_body  -- Implements (FORALL2)
      = case sORTKind_maybe body_kind of
          Just (torc, _) -> liftedTypeOrConstraintKind torc
          Nothing        -> pprPanic "typeKind" (ppr body_kind)


---------------------------------------------

sORTKind_maybe :: Kind -> Maybe (TypeOrConstraint, Type)
-- Sees if the argument is of form (TYPE rep) or (CONSTRAINT rep)
-- and if so returns which, and the runtime rep
--
-- This is a "hot" function.  Do not call splitTyConApp_maybe here,
-- to avoid the faff with FunTy
sORTKind_maybe (TyConApp tc tys)
  -- First, short-cuts for Type and Constraint that do no allocation
  | tc_uniq == liftedTypeKindTyConKey = assert( null tys ) $ Just (TypeLike,       liftedRepTy)
  | tc_uniq == constraintKindTyConKey = assert( null tys ) $ Just (ConstraintLike, liftedRepTy)
  | tc_uniq == tYPETyConKey           = get_rep TypeLike
  | tc_uniq == cONSTRAINTTyConKey     = get_rep ConstraintLike
  | Just ty' <- expandSynTyConApp_maybe tc tys = sORTKind_maybe ty'
  where
    !tc_uniq = tyConUnique tc
     -- This bang on tc_uniq is important.  It means that sORTKind_maybe starts
     -- by evaluating tc_uniq, and then ends up with a single case with a 4-way branch

    get_rep torc = case tys of
                     (rep:_reps) -> assert (null _reps) $ Just (torc, rep)
                     []          -> Nothing

sORTKind_maybe _ = Nothing

typeTypeOrConstraint :: HasDebugCallStack => Type -> TypeOrConstraint
-- ^ Returns whether the given type is 'TypeLike' or 'ConstraintLike'.
--
-- Equivalent to calling 'sORTKind_maybe', but faster in the 'FunTy' case.
--
-- __Precondition__: expects a type that classifies values.
typeTypeOrConstraint ty
   = case coreFullView ty of
       FunTy { ft_af = af } -> funTyFlagResultTypeOrConstraint af
       ty' | Just (torc, _) <- sORTKind_maybe (typeKind ty')
          -> torc
          | otherwise
          -> pprPanic "typeOrConstraint" (ppr ty <+> dcolon <+> ppr (typeKind ty))

-- | Does this classify a type allowed to have values? Responds True to things
-- like *, TYPE Lifted, TYPE IntRep, TYPE v, Constraint.
isTYPEorCONSTRAINT :: Kind -> Bool
-- ^ True of a kind `TYPE _` or `CONSTRAINT _`
isTYPEorCONSTRAINT k = isJust (sORTKind_maybe k)

tyConIsTYPEorCONSTRAINT :: TyCon -> Bool
tyConIsTYPEorCONSTRAINT tc
  = tc_uniq == tYPETyConKey || tc_uniq == cONSTRAINTTyConKey
  where
    !tc_uniq = tyConUnique tc

isConstraintLikeKind :: Kind -> Bool
-- True of (CONSTRAINT _)
isConstraintLikeKind kind
  = case sORTKind_maybe kind of
      Just (ConstraintLike, _) -> True
      _                        -> False

isConstraintKind :: Kind -> Bool
-- True of (CONSTRAINT LiftedRep)
isConstraintKind kind
  = case sORTKind_maybe kind of
      Just (ConstraintLike, rep) -> isLiftedRuntimeRep rep
      _                          -> False

tcIsLiftedTypeKind :: Kind -> Bool
-- ^ Is this kind equivalent to 'Type' i.e. TYPE LiftedRep?
tcIsLiftedTypeKind kind
  | Just (TypeLike, rep) <- sORTKind_maybe kind
  = isLiftedRuntimeRep rep
  | otherwise
  = False

tcIsBoxedTypeKind :: Kind -> Bool
-- ^ Is this kind equivalent to @TYPE (BoxedRep l)@ for some @l :: Levity@?
tcIsBoxedTypeKind kind
  | Just (TypeLike, rep) <- sORTKind_maybe kind
  = isBoxedRuntimeRep rep
  | otherwise
  = False

-- | Is this kind equivalent to @TYPE r@ (for some unknown r)?
--
-- This considers 'Constraint' to be distinct from @*@.
isTypeLikeKind :: Kind -> Bool
isTypeLikeKind kind
  = case sORTKind_maybe kind of
      Just (TypeLike, _) -> True
      _                  -> False

returnsConstraintKind :: Kind -> Bool
-- True <=> the Kind ultimately returns a Constraint
--   E.g.  * -> Constraint
--         forall k. k -> Constraint
returnsConstraintKind kind
  | Just kind' <- coreView kind = returnsConstraintKind kind'
returnsConstraintKind (ForAllTy _ _ ty)         = returnsConstraintKind ty
returnsConstraintKind (FunTy { ft_res = ty }) = returnsConstraintKind ty
returnsConstraintKind kind                    = isConstraintLikeKind kind

--------------------------
typeLiteralKind :: TyLit -> Kind
typeLiteralKind (NumTyLit {}) = naturalTy
typeLiteralKind (IntPrimTyLit {}) = intPrimTy
typeLiteralKind (StrTyLit {}) = typeSymbolKind
typeLiteralKind (CharTyLit {}) = charTy

-- | Returns True if a type has a syntactically fixed runtime rep,
-- as per Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete.
--
-- This function is equivalent to `isFixedRuntimeRepKind . typeKind`
-- but much faster.
--
-- __Precondition:__ The type has kind @('TYPE' blah)@
typeHasFixedRuntimeRep :: HasDebugCallStack => Type -> Bool
typeHasFixedRuntimeRep = go
  where
    go (TyConApp tc _)
      | tcHasFixedRuntimeRep tc = True
    go (FunTy {})               = True
    go (LitTy {})               = True
    go (ForAllTy _ _ ty)          = go ty
    go ty                       = isFixedRuntimeRepKind (typeKind ty)

-- | Checks that a kind of the form 'Type', 'Constraint'
-- or @'TYPE r@ is concrete. See 'isConcreteType'.
--
-- __Precondition:__ The type has kind `TYPE blah` or `CONSTRAINT blah`
isFixedRuntimeRepKind :: HasDebugCallStack => Kind -> Bool
isFixedRuntimeRepKind k
  = assertPpr (isTYPEorCONSTRAINT k) (ppr k) $
    -- the isLiftedTypeKind check is necessary b/c of Constraint
    isConcreteType k

-- | Tests whether the given type is concrete, i.e. it
-- whether it consists only of concrete type constructors,
-- concrete type variables, and applications.
--
-- See Note [Concrete types] in GHC.Tc.Utils.Concrete.
isConcreteType :: Type -> Bool
isConcreteType = isConcreteTypeWith emptyVarSet

-- | Like 'isConcreteType', but allows passing in a set of 'TyVar's that
-- should be considered concrete.
--
-- See Note [Concrete types] in GHC.Tc.Utils.Concrete.
isConcreteTypeWith :: TyVarSet -> Type -> Bool
-- This version, with a 'TyVarSet' argument, supports 'mkSynonymTyCon',
-- which needs to test the RHS for concreteness, under the assumption that
-- the binders are instantiated to concrete types
isConcreteTypeWith conc_tvs = go
  where
    go (TyVarTy tv)        = isConcreteTyVar tv || tv `elemVarSet` conc_tvs
    go (ClassAppTy {})     = False
    go (DepDictTy {})      = False
    go (TermAppTy {})      = False
    go (TermVarTy _)       = False
    go (DepFunTy {})       = False
    go (AppTy ty1 ty2)     = go ty1 && go ty2
    go (TyConApp tc tys)   = go_tc tc tys
    go ForAllTy{}          = False
    go (FunTy _ ma w t1 t2) = go ma && go w
                           && go (typeKind t1) && go t1
                           && go (typeKind t2) && go t2
    go LitTy{}             = True
    go CastTy{}            = False
    go CoercionTy{}        = False

    go_tc :: TyCon -> [Type] -> Bool
    go_tc tc tys
      | isForgetfulSynTyCon tc  -- E.g. type S a = Int
                                -- Then (S x) is concrete even if x isn't
      , Just ty' <- expandSynTyConApp_maybe tc tys
      = go ty'

      -- Apart from forgetful synonyms, isConcreteTyCon
      -- is enough; no need to expand.  This is good for e.g
      --      type LiftedRep = BoxedRep Lifted
      | isConcreteTyCon tc
      = all go tys

      | otherwise  -- E.g. type families
      = False

{-
%************************************************************************
%*                                                                      *
         Pretty-printing
%*                                                                      *
%************************************************************************

Most pretty-printing is either in GHC.Core.TyCo.Rep or GHC.Iface.Type.

-}

-- | Does a 'TyCon' (that is applied to some number of arguments) need to be
-- ascribed with an explicit kind signature to resolve ambiguity if rendered as
-- a source-syntax type?
-- (See @Note [When does a tycon application need an explicit kind signature?]@
-- for a full explanation of what this function checks for.)
tyConAppNeedsKindSig
  :: Bool  -- ^ Should specified binders count towards injective positions in
           --   the kind of the TyCon? (If you're using visible kind
           --   applications, then you want True here.)
  -> TyCon
  -> Int   -- ^ The number of args the 'TyCon' is applied to.
  -> Bool  -- ^ Does @T t_1 ... t_n@ need a kind signature? (Where @n@ is the
           --   number of arguments)
tyConAppNeedsKindSig spec_inj_pos tc n_args
  | LT <- listLengthCmp tc_binders n_args
  = False
  | otherwise
  = let (dropped_binders, remaining_binders)
          = splitAt n_args tc_binders
        result_kind  = mkTyConKind remaining_binders tc_res_kind
        result_vars  = tyCoVarsOfType result_kind
        dropped_vars = mapUnionVarSet injective_vars_of_binder dropped_binders

    in not (subVarSet result_vars dropped_vars)
  where
    tc_binders  = tyConBinders tc
    tc_res_kind = tyConResKind tc

    -- Returns the variables that would be fixed by knowing a TyConBinder. See
    -- Note [When does a tycon application need an explicit kind signature?]
    -- for a more detailed explanation of what this function does.
    injective_vars_of_binder :: TyConBinder -> VarSet
    injective_vars_of_binder (Bndr tv vis) =
      case vis of
        AnonTCB        -> injectiveVarsOfType False -- conservative choice
                                              (varType tv)
        NamedTCB argf  | source_of_injectivity argf
                       -> unitVarSet tv `unionVarSet`
                          injectiveVarsOfType False (varType tv)
        _              -> emptyVarSet

    source_of_injectivity Required  = True
    source_of_injectivity Specified = spec_inj_pos
    source_of_injectivity Inferred  = False

{-
Note [When does a tycon application need an explicit kind signature?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a couple of places in GHC where we convert Core Types into forms that
more closely resemble user-written syntax. These include:

1. Template Haskell Type reification (see, for instance, GHC.Tc.Gen.Splice.reify_tc_app)
2. Converting Types to LHsTypes (such as in Haddock.Convert in haddock)

This conversion presents a challenge: how do we ensure that the resulting type
has enough kind information so as not to be ambiguous? To better motivate this
question, consider the following Core type:

  -- Foo :: Type -> Type
  type Foo = Proxy Type

There is nothing ambiguous about the RHS of Foo in Core. But if we were to,
say, reify it into a TH Type, then it's tempting to just drop the invisible
Type argument and simply return `Proxy`. But now we've lost crucial kind
information: we don't know if we're dealing with `Proxy Type` or `Proxy Bool`
or `Proxy Int` or something else! We've inadvertently introduced ambiguity.

Unlike in other situations in GHC, we can't just turn on
-fprint-explicit-kinds, as we need to produce something which has the same
structure as a source-syntax type. Moreover, we can't rely on visible kind
application, since the first kind argument to Proxy is inferred, not specified.
Our solution is to annotate certain tycons with their kinds whenever they
appear in applied form in order to resolve the ambiguity. For instance, we
would reify the RHS of Foo like so:

  type Foo = (Proxy :: Type -> Type)

We need to devise an algorithm that determines precisely which tycons need
these explicit kind signatures. We certainly don't want to annotate _every_
tycon with a kind signature, or else we might end up with horribly bloated
types like the following:

  (Either :: Type -> Type -> Type) (Int :: Type) (Char :: Type)

We only want to annotate tycons that absolutely require kind signatures in
order to resolve some sort of ambiguity, and nothing more.

Suppose we have a tycon application (T ty_1 ... ty_n). Why might this type
require a kind signature? It might require it when we need to fill in any of
T's omitted arguments. By "omitted argument", we mean one that is dropped when
reifying ty_1 ... ty_n. Sometimes, the omitted arguments are inferred and
specified arguments (e.g., TH reification in GHC.Tc.Gen.Splice), and sometimes the
omitted arguments are only the inferred ones (e.g., in situations where
specified arguments are reified through visible kind application).
Regardless, the key idea is that _some_ arguments are going to be omitted after
reification, and the only mechanism we have at our disposal for filling them in
is through explicit kind signatures.

What do we mean by "fill in"? Let's consider this small example:

  T :: forall {k}. Type -> (k -> Type) -> k

Moreover, we have this application of T:

  T @{j} Int aty

When we reify this type, we omit the inferred argument @{j}. Is it fixed by the
other (non-inferred) arguments? Yes! If we know the kind of (aty :: blah), then
we'll generate an equality constraint (kappa -> Type) and, assuming we can
solve it, that will fix `kappa`. (Here, `kappa` is the unification variable
that we instantiate `k` with.)

Therefore, for any application of a tycon T to some arguments, the Question We
Must Answer is:

* Given the first n arguments of T, do the kinds of the non-omitted arguments
  fill in the omitted arguments?

(This is still a bit hand-wavy, but we'll refine this question incrementally
as we explain more of the machinery underlying this process.)

Answering this question is precisely the role that the `injectiveVarsOfType`
and `injective_vars_of_binder` functions exist to serve. If an omitted argument
`a` appears in the set returned by `injectiveVarsOfType ty`, then knowing
`ty` determines (i.e., fills in) `a`. (More on `injective_vars_of_binder` in a
bit.)

More formally, if
`a` is in `injectiveVarsOfType ty`
and  S1(ty) ~ S2(ty),
then S1(a)  ~ S2(a),
where S1 and S2 are arbitrary substitutions.

For example, is `F` is a non-injective type family, then

  injectiveVarsOfType(Either c (Maybe (a, F b c))) = {a, c}

Now that we know what this function does, here is a second attempt at the
Question We Must Answer:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. Do the injective
  variables of these binders fill in the remainder of T's kind?

Alright, we're getting closer. Next, we need to clarify what the injective
variables of a tycon binder are. This the role that the
`injective_vars_of_binder` function serves. Here is what this function does for
each form of tycon binder:

* Anonymous binders are injective positions. For example, in the promoted data
  constructor '(:):

    '(:) :: forall a. a -> [a] -> [a]

  The second and third tyvar binders (of kinds `a` and `[a]`) are both
  anonymous, so if we had '(:) 'True '[], then the kinds of 'True and
  '[] would contribute to the kind of '(:) 'True '[]. Therefore,
  injective_vars_of_binder(_ :: a) = injectiveVarsOfType(a) = {a}.
  (Similarly, injective_vars_of_binder(_ :: [a]) = {a}.)
* Named binders:
  - Inferred binders are never injective positions. For example, in this data
    type:

      data Proxy a
      Proxy :: forall {k}. k -> Type

    If we had Proxy 'True, then the kind of 'True would not contribute to the
    kind of Proxy 'True. Therefore,
    injective_vars_of_binder(forall {k}. ...) = {}.
  - Required binders are injective positions. For example, in this data type:

      data Wurble k (a :: k) :: k
      Wurble :: forall k -> k -> k

  The first tyvar binder (of kind `forall k`) has required visibility, so if
  we had Wurble (Maybe a) Nothing, then the kind of Maybe a would
  contribute to the kind of Wurble (Maybe a) Nothing. Hence,
  injective_vars_of_binder(forall a -> ...) = {a}.
  - Specified binders /might/ be injective positions, depending on how you
    approach things. Continuing the '(:) example:

      '(:) :: forall a. a -> [a] -> [a]

    Normally, the (forall a. ...) tyvar binder wouldn't contribute to the kind
    of '(:) 'True '[], since it's not explicitly instantiated by the user. But
    if visible kind application is enabled, then this is possible, since the
    user can write '(:) @Bool 'True '[]. (In that case,
    injective_vars_of_binder(forall a. ...) = {a}.)

    There are some situations where using visible kind application is appropriate
    and others where it is not (e.g., TH
    reification), so the `injective_vars_of_binder` function is parameterized by
    a Bool which decides if specified binders should be counted towards
    injective positions or not.

Now that we've defined injective_vars_of_binder, we can refine the Question We
Must Answer once more:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. For each such binder
  b_i, take the union of all injective_vars_of_binder(b_i). Is this set a
  superset of the free variables of the remainder of T's kind?

If the answer to this question is "no", then (T ty_1 ... ty_n) needs an
explicit kind signature, since T's kind has kind variables leftover that
aren't fixed by the non-omitted arguments.

One last sticking point: what does "the remainder of T's kind" mean? You might
be tempted to think that it corresponds to all of the arguments in the kind of
T that would normally be instantiated by omitted arguments. But this isn't
quite right, strictly speaking. Consider the following (silly) example:

  S :: forall {k}. Type -> Type

And suppose we have this application of S:

  S Int Bool

The Int argument would be omitted, and
injective_vars_of_binder(_ :: Type) = {}. This is not a superset of {k}, which
might suggest that (S Bool) needs an explicit kind signature. But
(S Bool :: Type) doesn't actually fix `k`! This is because the kind signature
only affects the /result/ of the application, not all of the individual
arguments. So adding a kind signature here won't make a difference. Therefore,
the fourth (and final) iteration of the Question We Must Answer is:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. For each such binder
  b_i, take the union of all injective_vars_of_binder(b_i). Is this set a
  superset of the free variables of the kind of (T ty_1 ... ty_n)?

Phew, that was a lot of work!

How can be sure that this is correct? That is, how can we be sure that in the
event that we leave off a kind annotation, that one could infer the kind of the
tycon application from its arguments? It's essentially a proof by induction: if
we can infer the kinds of every subtree of a type, then the whole tycon
application will have an inferrable kind--unless, of course, the remainder of
the tycon application's kind has uninstantiated kind variables.

What happens if T is oversaturated? That is, if T's kind has fewer than n
arguments, in the case that the concrete application instantiates a result
kind variable with an arrow kind? If we run out of arguments, we do not attach
a kind annotation. This should be a rare case, indeed. Here is an example:

   data T1 :: k1 -> k2 -> *
   data T2 :: k1 -> k2 -> *

   type family G (a :: k) :: k
   type instance G T1 = T2

   type instance F Char = (G T1 Bool :: (* -> *) -> *)   -- F from above

Here G's kind is (forall k. k -> k), and the desugared RHS of that last
instance of F is (G (* -> (* -> *) -> *) (T1 * (* -> *)) Bool). According to
the algorithm above, there are 3 arguments to G so we should peel off 3
arguments in G's kind. But G's kind has only two arguments. This is the
rare special case, and we choose not to annotate the application of G with
a kind signature. After all, we needn't do this, since that instance would
be reified as:

   type instance F Char = G (T1 :: * -> (* -> *) -> *) Bool

So the kind of G isn't ambiguous anymore due to the explicit kind annotation
on its argument. See #8953 and test th/T8953.
-}

{-
************************************************************************
*                                                                      *
        Multiplicities
*                                                                      *
************************************************************************

These functions would prefer to be in GHC.Core.Multiplicity, but
they some are used elsewhere in this module, and wanted to bring
their friends here with them.
-}

unrestricted, linear, tymult :: a -> Scaled a

-- | Scale a payload by Many
unrestricted = Scaled ManyTy

-- | Scale a payload by One
linear = Scaled OneTy

-- | Scale a payload by Many; used for type arguments in core
tymult = Scaled ManyTy

irrelevantMult :: Scaled a -> a
irrelevantMult = scaledThing

mkScaled :: Mult -> a -> Scaled a
mkScaled = Scaled

scaledSet :: Scaled a -> b -> Scaled b
scaledSet (Scaled m _) b = Scaled m b

pattern OneTy :: Mult
pattern OneTy <- (isOneTy -> True)
  where OneTy = oneDataConTy

pattern ManyTy :: Mult
pattern ManyTy <- (isManyTy -> True)
  where ManyTy = manyDataConTy

isManyTy :: Mult -> Bool
isManyTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` manyDataConKey
isManyTy _ = False

isOneTy :: Mult -> Bool
isOneTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` oneDataConKey
isOneTy _ = False

isLinearType :: Type -> Bool
-- ^ @isLinear t@ returns @True@ of a if @t@ is a type of (curried) function
-- where at least one argument is linear (or otherwise non-unrestricted). We use
-- this function to check whether it is safe to eta reduce an Id in CorePrep. It
-- is always safe to return 'True', because 'True' deactivates the optimisation.
isLinearType ty = case ty of
                      FunTy _ _ ManyTy _ res -> isLinearType res
                      FunTy _ _ _ _ _      -> True
                      ForAllTy _ _ res       -> isLinearType res
                      _ -> False
