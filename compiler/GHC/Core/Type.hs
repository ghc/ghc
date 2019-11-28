-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998
--
-- Type - public interface

{-# LANGUAGE CPP, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Main functions for manipulating types and type-related things
module GHC.Core.Type (
        -- Note some of this is just re-exports from TyCon..

        -- * Main data types representing Types
        -- $type_classification

        -- $representation_types
        TyThing(..), Type, ArgFlag(..), AnonArgFlag(..), ForallVisFlag(..),
        KindOrType, PredType, ThetaType,
        Var, TyVar, isTyVar, TyCoVar, TyCoBinder, TyCoVarBinder, TyVarBinder,
        KnotTied,

        -- ** Constructing and deconstructing types
        mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, repGetTyVar_maybe,
        getCastedTyVar_maybe, tyVarKind, varType,

        mkAppTy, mkAppTys, splitAppTy, splitAppTys, repSplitAppTys,
        splitAppTy_maybe, repSplitAppTy_maybe, tcRepSplitAppTy_maybe,

        mkVisFunTy, mkInvisFunTy, mkVisFunTys, mkInvisFunTys,
        splitFunTy, splitFunTy_maybe,
        splitFunTys, funResultTy, funArgTy,

        mkTyConApp, mkTyConTy,
        tyConAppTyCon_maybe, tyConAppTyConPicky_maybe,
        tyConAppArgs_maybe, tyConAppTyCon, tyConAppArgs,
        splitTyConApp_maybe, splitTyConApp, tyConAppArgN,
        tcSplitTyConApp_maybe,
        splitListTyConApp_maybe,
        repSplitTyConApp_maybe,

        mkForAllTy, mkForAllTys, mkTyCoInvForAllTys,
        mkSpecForAllTy, mkSpecForAllTys,
        mkVisForAllTys, mkTyCoInvForAllTy,
        mkInvForAllTy, mkInvForAllTys,
        splitForAllTys, splitForAllTysSameVis,
        splitForAllVarBndrs,
        splitForAllTy_maybe, splitForAllTy,
        splitForAllTy_ty_maybe, splitForAllTy_co_maybe,
        splitPiTy_maybe, splitPiTy, splitPiTys,
        mkTyConBindersPreferAnon,
        mkPiTy, mkPiTys,
        mkLamType, mkLamTypes,
        piResultTy, piResultTys,
        applyTysX, dropForAlls,
        mkFamilyTyConApp,
        buildSynTyCon,

        mkNumLitTy, isNumLitTy,
        mkStrLitTy, isStrLitTy,
        isLitTy,

        isPredTy,

        getRuntimeRep_maybe, kindRep_maybe, kindRep,

        mkCastTy, mkCoercionTy, splitCastTy_maybe,
        discardCast,

        userTypeError_maybe, pprUserTypeErrorTy,

        coAxNthLHS,
        stripCoercionTy,

        splitPiTysInvisible, splitPiTysInvisibleN,
        invisibleTyBndrCount,
        filterOutInvisibleTypes, filterOutInferredTypes,
        partitionInvisibleTypes, partitionInvisibles,
        tyConArgFlags, appTyArgFlags,
        synTyConResKind,

        modifyJoinResTy, setJoinResTy,

        -- ** Analyzing types
        TyCoMapper(..), mapTyCo, mapTyCoX,
        TyCoFolder(..), foldTyCo,

        -- (Newtypes)
        newTyConInstRhs,

        -- ** Binders
        sameVis,
        mkTyCoVarBinder, mkTyCoVarBinders,
        mkTyVarBinders,
        mkAnonBinder,
        isAnonTyCoBinder,
        binderVar, binderVars, binderType, binderArgFlag,
        tyCoBinderType, tyCoBinderVar_maybe,
        tyBinderType,
        binderRelevantType_maybe,
        isVisibleArgFlag, isInvisibleArgFlag, isVisibleBinder,
        isInvisibleBinder, isNamedBinder,
        tyConBindersTyCoBinders,

        -- ** Common type constructors
        funTyCon,

        -- ** Predicates on types
        isTyVarTy, isFunTy, isCoercionTy,
        isCoercionTy_maybe, isForAllTy,
        isForAllTy_ty, isForAllTy_co,
        isPiTy, isTauTy, isFamFreeTy,
        isCoVarType,

        isValidJoinPointType,
        tyConAppNeedsKindSig,

        -- *** Levity and boxity
        isLiftedType_maybe,
        isLiftedTypeKind, isUnliftedTypeKind,
        isLiftedRuntimeRep, isUnliftedRuntimeRep,
        isUnliftedType, mightBeUnliftedType, isUnboxedTupleType, isUnboxedSumType,
        isAlgType, isDataFamilyAppType,
        isPrimitiveType, isStrictType,
        isRuntimeRepTy, isRuntimeRepVar, isRuntimeRepKindedTy,
        dropRuntimeRepArgs,
        getRuntimeRep,

        -- * Main data types representing Kinds
        Kind,

        -- ** Finding the kind of a type
        typeKind, tcTypeKind, isTypeLevPoly, resultIsLevPoly,
        tcIsLiftedTypeKind, tcIsConstraintKind, tcReturnsConstraintKind,
        tcIsRuntimeTypeKind,

        -- ** Common Kind
        liftedTypeKind,

        -- * Type free variables
        tyCoFVsOfType, tyCoFVsBndr, tyCoFVsVarBndr, tyCoFVsVarBndrs,
        tyCoVarsOfType, tyCoVarsOfTypes,
        tyCoVarsOfTypeDSet,
        coVarsOfType,
        coVarsOfTypes,

        noFreeVarsOfType,
        splitVisVarsOfType, splitVisVarsOfTypes,
        expandTypeSynonyms,
        typeSize, occCheckExpand,

        -- ** Closing over kinds
        closeOverKindsDSet, closeOverKindsList,
        closeOverKinds,

        -- * Well-scoped lists of variables
        scopedSort, tyCoVarsOfTypeWellScoped,
        tyCoVarsOfTypesWellScoped,

        -- * Type comparison
        eqType, eqTypeX, eqTypes, nonDetCmpType, nonDetCmpTypes, nonDetCmpTypeX,
        nonDetCmpTypesX, nonDetCmpTc,
        eqVarBndrs,

        -- * Forcing evaluation of types
        seqType, seqTypes,

        -- * Other views onto Types
        coreView, tcView,

        tyConsOfType,

        -- * Main type substitution data types
        TvSubstEnv,     -- Representation widely visible
        TCvSubst(..),    -- Representation visible to a few friends

        -- ** Manipulating type substitutions
        emptyTvSubstEnv, emptyTCvSubst, mkEmptyTCvSubst,

        mkTCvSubst, zipTvSubst, mkTvSubstPrs,
        zipTCvSubst,
        notElemTCvSubst,
        getTvSubstEnv, setTvSubstEnv,
        zapTCvSubst, getTCvInScope, getTCvSubstRangeFVs,
        extendTCvInScope, extendTCvInScopeList, extendTCvInScopeSet,
        extendTCvSubst, extendCvSubst,
        extendTvSubst, extendTvSubstBinderAndInScope,
        extendTvSubstList, extendTvSubstAndInScope,
        extendTCvSubstList,
        extendTvSubstWithClone,
        extendTCvSubstWithClone,
        isInScope, composeTCvSubstEnv, composeTCvSubst, zipTyEnv, zipCoEnv,
        isEmptyTCvSubst, unionTCvSubst,

        -- ** Performing substitution on types and kinds
        substTy, substTys, substTyWith, substTysWith, substTheta,
        substTyAddInScope,
        substTyUnchecked, substTysUnchecked, substThetaUnchecked,
        substTyWithUnchecked,
        substCoUnchecked, substCoWithUnchecked,
        substTyVarBndr, substTyVarBndrs, substTyVar, substTyVars,
        substVarBndr, substVarBndrs,
        cloneTyVarBndr, cloneTyVarBndrs, lookupTyVar,

        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyOpenKind,
        tidyVarBndr, tidyVarBndrs, tidyFreeTyCoVars,
        tidyOpenTyCoVar, tidyOpenTyCoVars,
        tidyTyCoVarOcc,
        tidyTopType,
        tidyKind,
        tidyTyCoVarBinder, tidyTyCoVarBinders,

        -- * Kinds
        isConstraintKindCon,
        classifiesTypeWithValues,
        isKindLevPoly
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Types.Basic

-- We import the representation and primitive functions from GHC.Core.TyCo.Rep.
-- Many things are reexported, but not the representation!

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Subst
import GHC.Core.TyCo.Tidy
import GHC.Core.TyCo.FVs

-- friends:
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Unique.Set

import GHC.Core.TyCon
import TysPrim
import {-# SOURCE #-} TysWiredIn ( listTyCon, typeNatKind
                                 , typeSymbolKind, liftedTypeKind
                                 , liftedTypeKindTyCon
                                 , constraintKind )
import GHC.Types.Name( Name )
import PrelNames
import GHC.Core.Coercion.Axiom
import {-# SOURCE #-} GHC.Core.Coercion
   ( mkNomReflCo, mkGReflCo, mkReflCo
   , mkTyConAppCo, mkAppCo, mkCoVarCo, mkAxiomRuleCo
   , mkForAllCo, mkFunCo, mkAxiomInstCo, mkUnivCo
   , mkSymCo, mkTransCo, mkNthCo, mkLRCo, mkInstCo
   , mkKindCo, mkSubCo, mkFunCo, mkAxiomInstCo
   , decomposePiCos, coercionKind, coercionLKind
   , coercionRKind, coercionType
   , isReflexiveCo, seqCo )

-- others
import Util
import FV
import Outputable
import FastString
import Pair
import ListSetOps
import GHC.Types.Unique ( nonDetCmpUnique )

import Maybes           ( orElse )
import Data.Maybe       ( isJust )
import Control.Monad    ( guard )

-- $type_classification
-- #type_classification#
--
-- Types are one of:
--
-- [Unboxed]            Iff its representation is other than a pointer
--                      Unboxed types are also unlifted.
--
-- [Lifted]             Iff it has bottom as an element.
--                      Closures always have lifted types: i.e. any
--                      let-bound identifier in Core must have a lifted
--                      type. Operationally, a lifted object is one that
--                      can be entered.
--                      Only lifted types may be unified with a type variable.
--
-- [Algebraic]          Iff it is a type with one or more constructors, whether
--                      declared with @data@ or @newtype@.
--                      An algebraic type is one that can be deconstructed
--                      with a case expression. This is /not/ the same as
--                      lifted types, because we also include unboxed
--                      tuples in this classification.
--
-- [Data]               Iff it is a type declared with @data@, or a boxed tuple.
--
-- [Primitive]          Iff it is a built-in type that can't be expressed in Haskell.
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

Note [coreView vs tcView]
~~~~~~~~~~~~~~~~~~~~~~~~~
So far as the typechecker is concerned, 'Constraint' and 'TYPE
LiftedRep' are distinct kinds.

But in Core these two are treated as identical.

We implement this by making 'coreView' convert 'Constraint' to 'TYPE
LiftedRep' on the fly.  The function tcView (used in the type checker)
does not do this.

See also #11715, which tracks removing this inconsistency.

-}

-- | Gives the typechecker view of a type. This unwraps synonyms but
-- leaves 'Constraint' alone. c.f. coreView, which turns Constraint into
-- TYPE LiftedRep. Returns Nothing if no unwrapping happens.
-- See also Note [coreView vs tcView]
{-# INLINE tcView #-}
tcView :: Type -> Maybe Type
tcView (TyConApp tc tys) | Just (tenv, rhs, tys') <- expandSynTyCon_maybe tc tys
  = Just (mkAppTys (substTy (mkTvSubstPrs tenv) rhs) tys')
               -- The free vars of 'rhs' should all be bound by 'tenv', so it's
               -- ok to use 'substTy' here.
               -- See also Note [The substitution invariant] in GHC.Core.TyCo.Subst.
               -- Its important to use mkAppTys, rather than (foldl AppTy),
               -- because the function part might well return a
               -- partially-applied type constructor; indeed, usually will!
tcView _ = Nothing

{-# INLINE coreView #-}
coreView :: Type -> Maybe Type
-- ^ This function Strips off the /top layer only/ of a type synonym
-- application (if any) its underlying representation type.
-- Returns Nothing if there is nothing to look through.
-- This function considers 'Constraint' to be a synonym of @TYPE LiftedRep@.
--
-- By being non-recursive and inlined, this case analysis gets efficiently
-- joined onto the case analysis that the caller is already doing
coreView ty@(TyConApp tc tys)
  | Just (tenv, rhs, tys') <- expandSynTyCon_maybe tc tys
  = Just (mkAppTys (substTy (mkTvSubstPrs tenv) rhs) tys')
    -- This equation is exactly like tcView

  -- At the Core level, Constraint = Type
  -- See Note [coreView vs tcView]
  | isConstraintKindCon tc
  = ASSERT2( null tys, ppr ty )
    Just liftedTypeKind

coreView _ = Nothing

-----------------------------------------------
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
  = go (mkEmptyTCvSubst in_scope) ty
  where
    in_scope = mkInScopeSet (tyCoVarsOfType ty)

    go subst (TyConApp tc tys)
      | Just (tenv, rhs, tys') <- expandSynTyCon_maybe tc expanded_tys
      = let subst' = mkTvSubst in_scope (mkVarEnv tenv)
            -- Make a fresh substitution; rhs has nothing to
            -- do with anything that has happened so far
            -- NB: if you make changes here, be sure to build an
            --     /idempotent/ substitution, even in the nested case
            --        type T a b = a -> b
            --        type S x y = T y x
            -- (#11665)
        in  mkAppTys (go subst' rhs) tys'
      | otherwise
      = TyConApp tc expanded_tys
      where
        expanded_tys = (map (go subst) tys)

    go _     (LitTy l)     = LitTy l
    go subst (TyVarTy tv)  = substTyVar subst tv
    go subst (AppTy t1 t2) = mkAppTy (go subst t1) (go subst t2)
    go subst ty@(FunTy _ arg res)
      = ty { ft_arg = go subst arg, ft_res = go subst res }
    go subst (ForAllTy (Bndr tv vis) t)
      = let (subst', tv') = substVarBndrUsing go subst tv in
        ForAllTy (Bndr tv' vis) (go subst' t)
    go subst (CastTy ty co)  = mkCastTy (go subst ty) (go_co subst co)
    go subst (CoercionTy co) = mkCoercionTy (go_co subst co)

    go_mco _     MRefl    = MRefl
    go_mco subst (MCo co) = MCo (go_co subst co)

    go_co subst (Refl ty)
      = mkNomReflCo (go subst ty)
    go_co subst (GRefl r ty mco)
      = mkGReflCo r (go subst ty) (go_mco subst mco)
       -- NB: coercions are always expanded upon creation
    go_co subst (TyConAppCo r tc args)
      = mkTyConAppCo r tc (map (go_co subst) args)
    go_co subst (AppCo co arg)
      = mkAppCo (go_co subst co) (go_co subst arg)
    go_co subst (ForAllCo tv kind_co co)
      = let (subst', tv', kind_co') = go_cobndr subst tv kind_co in
        mkForAllCo tv' kind_co' (go_co subst' co)
    go_co subst (FunCo r co1 co2)
      = mkFunCo r (go_co subst co1) (go_co subst co2)
    go_co subst (CoVarCo cv)
      = substCoVar subst cv
    go_co subst (AxiomInstCo ax ind args)
      = mkAxiomInstCo ax ind (map (go_co subst) args)
    go_co subst (UnivCo p r t1 t2)
      = mkUnivCo (go_prov subst p) r (go subst t1) (go subst t2)
    go_co subst (SymCo co)
      = mkSymCo (go_co subst co)
    go_co subst (TransCo co1 co2)
      = mkTransCo (go_co subst co1) (go_co subst co2)
    go_co subst (NthCo r n co)
      = mkNthCo r n (go_co subst co)
    go_co subst (LRCo lr co)
      = mkLRCo lr (go_co subst co)
    go_co subst (InstCo co arg)
      = mkInstCo (go_co subst co) (go_co subst arg)
    go_co subst (KindCo co)
      = mkKindCo (go_co subst co)
    go_co subst (SubCo co)
      = mkSubCo (go_co subst co)
    go_co subst (AxiomRuleCo ax cs)
      = AxiomRuleCo ax (map (go_co subst) cs)
    go_co _ (HoleCo h)
      = pprPanic "expandTypeSynonyms hit a hole" (ppr h)

    go_prov subst (PhantomProv co)    = PhantomProv (go_co subst co)
    go_prov subst (ProofIrrelProv co) = ProofIrrelProv (go_co subst co)
    go_prov _     p@(PluginProv _)    = p

      -- the "False" and "const" are to accommodate the type of
      -- substForAllCoBndrUsing, which is general enough to
      -- handle coercion optimization (which sometimes swaps the
      -- order of a coercion)
    go_cobndr subst = substForAllCoBndrUsing False (go_co subst) subst


-- | Extract the RuntimeRep classifier of a type from its kind. For example,
-- @kindRep * = LiftedRep@; Panics if this is not possible.
-- Treats * and Constraint as the same
kindRep :: HasDebugCallStack => Kind -> Type
kindRep k = case kindRep_maybe k of
              Just r  -> r
              Nothing -> pprPanic "kindRep" (ppr k)

-- | Given a kind (TYPE rr), extract its RuntimeRep classifier rr.
-- For example, @kindRep_maybe * = Just LiftedRep@
-- Returns 'Nothing' if the kind is not of form (TYPE rr)
-- Treats * and Constraint as the same
kindRep_maybe :: HasDebugCallStack => Kind -> Maybe Type
kindRep_maybe kind
  | Just kind' <- coreView kind = kindRep_maybe kind'
  | TyConApp tc [arg] <- kind
  , tc `hasKey` tYPETyConKey    = Just arg
  | otherwise                   = Nothing

-- | This version considers Constraint to be the same as *. Returns True
-- if the argument is equivalent to Type/Constraint and False otherwise.
-- See Note [Kind Constraint and kind Type]
isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind kind
  = case kindRep_maybe kind of
      Just rep -> isLiftedRuntimeRep rep
      Nothing  -> False

isLiftedRuntimeRep :: Type -> Bool
-- isLiftedRuntimeRep is true of LiftedRep :: RuntimeRep
-- False of type variables (a :: RuntimeRep)
--   and of other reps e.g. (IntRep :: RuntimeRep)
isLiftedRuntimeRep rep
  | Just rep' <- coreView rep          = isLiftedRuntimeRep rep'
  | TyConApp rr_tc args <- rep
  , rr_tc `hasKey` liftedRepDataConKey = ASSERT( null args ) True
  | otherwise                          = False

-- | Returns True if the kind classifies unlifted types and False otherwise.
-- Note that this returns False for levity-polymorphic kinds, which may
-- be specialized to a kind that classifies unlifted types.
isUnliftedTypeKind :: Kind -> Bool
isUnliftedTypeKind kind
  = case kindRep_maybe kind of
      Just rep -> isUnliftedRuntimeRep rep
      Nothing  -> False

isUnliftedRuntimeRep :: Type -> Bool
-- True of definitely-unlifted RuntimeReps
-- False of           (LiftedRep :: RuntimeRep)
--   and of variables (a :: RuntimeRep)
isUnliftedRuntimeRep rep
  | Just rep' <- coreView rep = isUnliftedRuntimeRep rep'
  | TyConApp rr_tc _ <- rep   -- NB: args might be non-empty
                              --     e.g. TupleRep [r1, .., rn]
  = isPromotedDataCon rr_tc && not (rr_tc `hasKey` liftedRepDataConKey)
        -- Avoid searching all the unlifted RuntimeRep type cons
        -- In the RuntimeRep data type, only LiftedRep is lifted
        -- But be careful of type families (F tys) :: RuntimeRep
  | otherwise {- Variables, applications -}
  = False

-- | Is this the type 'RuntimeRep'?
isRuntimeRepTy :: Type -> Bool
isRuntimeRepTy ty | Just ty' <- coreView ty = isRuntimeRepTy ty'
isRuntimeRepTy (TyConApp tc args)
  | tc `hasKey` runtimeRepTyConKey = ASSERT( null args ) True
isRuntimeRepTy _ = False

-- | Is a tyvar of type 'RuntimeRep'?
isRuntimeRepVar :: TyVar -> Bool
isRuntimeRepVar = isRuntimeRepTy . tyVarKind


{- *********************************************************************
*                                                                      *
               mapType
*                                                                      *
************************************************************************

These functions do a map-like operation over types, performing some operation
on all variables and binding sites. Primarily used for zonking.

Note [Efficiency for ForAllCo case of mapTyCoX]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As noted in Note [Forall coercions] in GHC.Core.TyCo.Rep, a ForAllCo is a bit redundant.
It stores a TyCoVar and a Coercion, where the kind of the TyCoVar always matches
the left-hand kind of the coercion. This is convenient lots of the time, but
not when mapping a function over a coercion.

The problem is that tcm_tybinder will affect the TyCoVar's kind and
mapCoercion will affect the Coercion, and we hope that the results will be
the same. Even if they are the same (which should generally happen with
correct algorithms), then there is an efficiency issue. In particular,
this problem seems to make what should be a linear algorithm into a potentially
exponential one. But it's only going to be bad in the case where there's
lots of foralls in the kinds of other foralls. Like this:

  forall a : (forall b : (forall c : ...). ...). ...

This construction seems unlikely. So we'll do the inefficient, easy way
for now.

Note [Specialising mappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
These INLINE pragmas are indispensable. mapTyCo and mapTyCoX are used
to implement zonking, and it's vital that they get specialised to the TcM
monad and the particular mapper in use.

Even specialising to the monad alone made a 20% allocation difference
in perf/compiler/T5030.

See Note [Specialising foldType] in TyCoRep for more details of this
idiom.
-}

-- | This describes how a "map" operation over a type/coercion should behave
data TyCoMapper env m
  = TyCoMapper
      { tcm_tyvar :: env -> TyVar -> m Type
      , tcm_covar :: env -> CoVar -> m Coercion
      , tcm_hole  :: env -> CoercionHole -> m Coercion
          -- ^ What to do with coercion holes.
          -- See Note [Coercion holes] in GHC.Core.TyCo.Rep.

      , tcm_tycobinder :: env -> TyCoVar -> ArgFlag -> m (env, TyCoVar)
          -- ^ The returned env is used in the extended scope

      , tcm_tycon :: TyCon -> m TyCon
          -- ^ This is used only for TcTyCons
          -- a) To zonk TcTyCons
          -- b) To turn TcTyCons into TyCons.
          --    See Note [Type checking recursive type and class declarations]
          --    in TcTyClsDecls
      }

{-# INLINE mapTyCo #-}  -- See Note [Specialising mappers]
mapTyCo :: Monad m => TyCoMapper () m
         -> ( Type       -> m Type
            , [Type]     -> m [Type]
            , Coercion   -> m Coercion
            , [Coercion] -> m[Coercion])
mapTyCo mapper
  = case mapTyCoX mapper of
     (go_ty, go_tys, go_co, go_cos)
        -> (go_ty (), go_tys (), go_co (), go_cos ())

{-# INLINE mapTyCoX #-}  -- See Note [Specialising mappers]
mapTyCoX :: Monad m => TyCoMapper env m
         -> ( env -> Type       -> m Type
            , env -> [Type]     -> m [Type]
            , env -> Coercion   -> m Coercion
            , env -> [Coercion] -> m[Coercion])
mapTyCoX (TyCoMapper { tcm_tyvar = tyvar
                     , tcm_tycobinder = tycobinder
                     , tcm_tycon = tycon
                     , tcm_covar = covar
                     , tcm_hole = cohole })
  = (go_ty, go_tys, go_co, go_cos)
  where
    go_tys _   []       = return []
    go_tys env (ty:tys) = (:) <$> go_ty env ty <*> go_tys env tys

    go_ty env (TyVarTy tv)    = tyvar env tv
    go_ty env (AppTy t1 t2)   = mkAppTy <$> go_ty env t1 <*> go_ty env t2
    go_ty _   ty@(LitTy {})   = return ty
    go_ty env (CastTy ty co)  = mkCastTy <$> go_ty env ty <*> go_co env co
    go_ty env (CoercionTy co) = CoercionTy <$> go_co env co

    go_ty env ty@(FunTy _ arg res)
      = do { arg' <- go_ty env arg; res' <- go_ty env res
           ; return (ty { ft_arg = arg', ft_res = res' }) }

    go_ty env ty@(TyConApp tc tys)
      | isTcTyCon tc
      = do { tc' <- tycon tc
           ; mkTyConApp tc' <$> go_tys env tys }

      -- Not a TcTyCon
      | null tys    -- Avoid allocation in this very
      = return ty   -- common case (E.g. Int, LiftedRep etc)

      | otherwise
      = mkTyConApp tc <$> go_tys env tys

    go_ty env (ForAllTy (Bndr tv vis) inner)
      = do { (env', tv') <- tycobinder env tv vis
           ; inner' <- go_ty env' inner
           ; return $ ForAllTy (Bndr tv' vis) inner' }

    go_cos _   []       = return []
    go_cos env (co:cos) = (:) <$> go_co env co <*> go_cos env cos

    go_mco _   MRefl    = return MRefl
    go_mco env (MCo co) = MCo <$> (go_co env co)

    go_co env (Refl ty)           = Refl <$> go_ty env ty
    go_co env (GRefl r ty mco)    = mkGReflCo r <$> go_ty env ty <*> go_mco env mco
    go_co env (AppCo c1 c2)       = mkAppCo <$> go_co env c1 <*> go_co env c2
    go_co env (FunCo r c1 c2)     = mkFunCo r <$> go_co env c1 <*> go_co env c2
    go_co env (CoVarCo cv)        = covar env cv
    go_co env (HoleCo hole)       = cohole env hole
    go_co env (UnivCo p r t1 t2)  = mkUnivCo <$> go_prov env p <*> pure r
                                    <*> go_ty env t1 <*> go_ty env t2
    go_co env (SymCo co)          = mkSymCo <$> go_co env co
    go_co env (TransCo c1 c2)     = mkTransCo <$> go_co env c1 <*> go_co env c2
    go_co env (AxiomRuleCo r cos) = AxiomRuleCo r <$> go_cos env cos
    go_co env (NthCo r i co)      = mkNthCo r i <$> go_co env co
    go_co env (LRCo lr co)        = mkLRCo lr <$> go_co env co
    go_co env (InstCo co arg)     = mkInstCo <$> go_co env co <*> go_co env arg
    go_co env (KindCo co)         = mkKindCo <$> go_co env co
    go_co env (SubCo co)          = mkSubCo <$> go_co env co
    go_co env (AxiomInstCo ax i cos) = mkAxiomInstCo ax i <$> go_cos env cos
    go_co env co@(TyConAppCo r tc cos)
      | isTcTyCon tc
      = do { tc' <- tycon tc
           ; mkTyConAppCo r tc' <$> go_cos env cos }

      -- Not a TcTyCon
      | null cos    -- Avoid allocation in this very
      = return co   -- common case (E.g. Int, LiftedRep etc)

      | otherwise
      = mkTyConAppCo r tc <$> go_cos env cos
    go_co env (ForAllCo tv kind_co co)
      = do { kind_co' <- go_co env kind_co
           ; (env', tv') <- tycobinder env tv Inferred
           ; co' <- go_co env' co
           ; return $ mkForAllCo tv' kind_co' co' }
        -- See Note [Efficiency for ForAllCo case of mapTyCoX]

    go_prov env (PhantomProv co)    = PhantomProv <$> go_co env co
    go_prov env (ProofIrrelProv co) = ProofIrrelProv <$> go_co env co
    go_prov _   p@(PluginProv _)    = return p


{-
************************************************************************
*                                                                      *
\subsection{Constructor-specific functions}
*                                                                      *
************************************************************************


---------------------------------------------------------------------
                                TyVarTy
                                ~~~~~~~
-}

-- | Attempts to obtain the type variable underlying a 'Type', and panics with the
-- given message if this is not a type variable type. See also 'getTyVar_maybe'
getTyVar :: String -> Type -> TyVar
getTyVar msg ty = case getTyVar_maybe ty of
                    Just tv -> tv
                    Nothing -> panic ("getTyVar: " ++ msg)

isTyVarTy :: Type -> Bool
isTyVarTy ty = isJust (getTyVar_maybe ty)

-- | Attempts to obtain the type variable underlying a 'Type'
getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe ty | Just ty' <- coreView ty = getTyVar_maybe ty'
                  | otherwise               = repGetTyVar_maybe ty

-- | If the type is a tyvar, possibly under a cast, returns it, along
-- with the coercion. Thus, the co is :: kind tv ~N kind ty
getCastedTyVar_maybe :: Type -> Maybe (TyVar, CoercionN)
getCastedTyVar_maybe ty | Just ty' <- coreView ty = getCastedTyVar_maybe ty'
getCastedTyVar_maybe (CastTy (TyVarTy tv) co)     = Just (tv, co)
getCastedTyVar_maybe (TyVarTy tv)
  = Just (tv, mkReflCo Nominal (tyVarKind tv))
getCastedTyVar_maybe _                            = Nothing

-- | Attempts to obtain the type variable underlying a 'Type', without
-- any expansion
repGetTyVar_maybe :: Type -> Maybe TyVar
repGetTyVar_maybe (TyVarTy tv) = Just tv
repGetTyVar_maybe _            = Nothing

{-
---------------------------------------------------------------------
                                AppTy
                                ~~~~~
We need to be pretty careful with AppTy to make sure we obey the
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

Note [Decomposing fat arrow c=>t]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Can we unify (a b) with (Eq a => ty)?   If we do so, we end up with
a partial application like ((=>) Eq a) which doesn't make sense in
source Haskell.  In contrast, we *can* unify (a b) with (t1 -> t2).
Here's an example (#9858) of how you might do it:
   i :: (Typeable a, Typeable b) => Proxy (a b) -> TypeRep
   i p = typeRep p

   j = i (Proxy :: Proxy (Eq Int => Int))
The type (Proxy (Eq Int => Int)) is only accepted with -XImpredicativeTypes,
but suppose we want that.  But then in the call to 'i', we end
up decomposing (Eq Int => Int), and we definitely don't want that.

This really only applies to the type checker; in Core, '=>' and '->'
are the same, as are 'Constraint' and '*'.  But for now I've put
the test in repSplitAppTy_maybe, which applies throughout, because
the other calls to splitAppTy are in GHC.Core.Unify, which is also used by
the type checker (e.g. when matching type-function equations).

-}

-- | Applies a type to another, as in e.g. @k a@
mkAppTy :: Type -> Type -> Type
  -- See Note [Respecting definitional equality], invariant (EQ1).
mkAppTy (CastTy fun_ty co) arg_ty
  | ([arg_co], res_co) <- decomposePiCos co (coercionKind co) [arg_ty]
  = (fun_ty `mkAppTy` (arg_ty `mkCastTy` arg_co)) `mkCastTy` res_co

mkAppTy (TyConApp tc tys) ty2 = mkTyConApp tc (tys ++ [ty2])
mkAppTy ty1               ty2 = AppTy ty1 ty2
        -- Note that the TyConApp could be an
        -- under-saturated type synonym.  GHC allows that; e.g.
        --      type Foo k = k a -> k a
        --      type Id x = x
        --      foo :: Foo Id -> Foo Id
        --
        -- Here Id is partially applied in the type sig for Foo,
        -- but once the type synonyms are expanded all is well
        --
        -- Moreover in TcHsTypes.tcInferApps we build up a type
        --   (T t1 t2 t3) one argument at a type, thus forming
        --   (T t1), (T t1 t2), etc

mkAppTys :: Type -> [Type] -> Type
mkAppTys ty1                []   = ty1
mkAppTys (CastTy fun_ty co) arg_tys  -- much more efficient then nested mkAppTy
                                     -- Why do this? See (EQ1) of
                                     -- Note [Respecting definitional equality]
                                     -- in GHC.Core.TyCo.Rep
  = foldl' AppTy ((mkAppTys fun_ty casted_arg_tys) `mkCastTy` res_co) leftovers
  where
    (arg_cos, res_co) = decomposePiCos co (coercionKind co) arg_tys
    (args_to_cast, leftovers) = splitAtList arg_cos arg_tys
    casted_arg_tys = zipWith mkCastTy args_to_cast arg_cos
mkAppTys (TyConApp tc tys1) tys2 = mkTyConApp tc (tys1 ++ tys2)
mkAppTys ty1                tys2 = foldl' AppTy ty1 tys2

-------------
splitAppTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Attempt to take a type application apart, whether it is a
-- function, type constructor, or plain type application. Note
-- that type family applications are NEVER unsaturated by this!
splitAppTy_maybe ty | Just ty' <- coreView ty
                    = splitAppTy_maybe ty'
splitAppTy_maybe ty = repSplitAppTy_maybe ty

-------------
repSplitAppTy_maybe :: HasDebugCallStack => Type -> Maybe (Type,Type)
-- ^ Does the AppTy split as in 'splitAppTy_maybe', but assumes that
-- any Core view stuff is already done
repSplitAppTy_maybe (FunTy _ ty1 ty2)
  = Just (TyConApp funTyCon [rep1, rep2, ty1], ty2)
  where
    rep1 = getRuntimeRep ty1
    rep2 = getRuntimeRep ty2

repSplitAppTy_maybe (AppTy ty1 ty2)
  = Just (ty1, ty2)

repSplitAppTy_maybe (TyConApp tc tys)
  | not (mustBeSaturated tc) || tys `lengthExceeds` tyConArity tc
  , Just (tys', ty') <- snocView tys
  = Just (TyConApp tc tys', ty')    -- Never create unsaturated type family apps!

repSplitAppTy_maybe _other = Nothing

-- This one doesn't break apart (c => t).
-- See Note [Decomposing fat arrow c=>t]
-- Defined here to avoid module loops between Unify and TcType.
tcRepSplitAppTy_maybe :: Type -> Maybe (Type,Type)
-- ^ Does the AppTy split as in 'tcSplitAppTy_maybe', but assumes that
-- any coreView stuff is already done. Refuses to look through (c => t)
tcRepSplitAppTy_maybe (FunTy { ft_af = af, ft_arg = ty1, ft_res = ty2 })
  | InvisArg <- af
  = Nothing  -- See Note [Decomposing fat arrow c=>t]

  | otherwise
  = Just (TyConApp funTyCon [rep1, rep2, ty1], ty2)
  where
    rep1 = getRuntimeRep ty1
    rep2 = getRuntimeRep ty2

tcRepSplitAppTy_maybe (AppTy ty1 ty2)    = Just (ty1, ty2)
tcRepSplitAppTy_maybe (TyConApp tc tys)
  | not (mustBeSaturated tc) || tys `lengthExceeds` tyConArity tc
  , Just (tys', ty') <- snocView tys
  = Just (TyConApp tc tys', ty')    -- Never create unsaturated type family apps!
tcRepSplitAppTy_maybe _other = Nothing

-------------
splitAppTy :: Type -> (Type, Type)
-- ^ Attempts to take a type application apart, as in 'splitAppTy_maybe',
-- and panics if this is not possible
splitAppTy ty = case splitAppTy_maybe ty of
                Just pr -> pr
                Nothing -> panic "splitAppTy"

-------------
splitAppTys :: Type -> (Type, [Type])
-- ^ Recursively splits a type as far as is possible, leaving a residual
-- type being applied to and the type arguments applied to it. Never fails,
-- even if that means returning an empty list of type applications.
splitAppTys ty = split ty ty []
  where
    split orig_ty ty args | Just ty' <- coreView ty = split orig_ty ty' args
    split _       (AppTy ty arg)        args = split ty ty (arg:args)
    split _       (TyConApp tc tc_args) args
      = let -- keep type families saturated
            n | mustBeSaturated tc = tyConArity tc
              | otherwise          = 0
            (tc_args1, tc_args2) = splitAt n tc_args
        in
        (TyConApp tc tc_args1, tc_args2 ++ args)
    split _   (FunTy _ ty1 ty2) args
      = ASSERT( null args )
        (TyConApp funTyCon [], [rep1, rep2, ty1, ty2])
      where
        rep1 = getRuntimeRep ty1
        rep2 = getRuntimeRep ty2

    split orig_ty _                     args  = (orig_ty, args)

-- | Like 'splitAppTys', but doesn't look through type synonyms
repSplitAppTys :: HasDebugCallStack => Type -> (Type, [Type])
repSplitAppTys ty = split ty []
  where
    split (AppTy ty arg) args = split ty (arg:args)
    split (TyConApp tc tc_args) args
      = let n | mustBeSaturated tc = tyConArity tc
              | otherwise          = 0
            (tc_args1, tc_args2) = splitAt n tc_args
        in
        (TyConApp tc tc_args1, tc_args2 ++ args)
    split (FunTy _ ty1 ty2) args
      = ASSERT( null args )
        (TyConApp funTyCon [], [rep1, rep2, ty1, ty2])
      where
        rep1 = getRuntimeRep ty1
        rep2 = getRuntimeRep ty2

    split ty args = (ty, args)

{-
                      LitTy
                      ~~~~~
-}

mkNumLitTy :: Integer -> Type
mkNumLitTy n = LitTy (NumTyLit n)

-- | Is this a numeric literal. We also look through type synonyms.
isNumLitTy :: Type -> Maybe Integer
isNumLitTy ty | Just ty1 <- coreView ty = isNumLitTy ty1
isNumLitTy (LitTy (NumTyLit n)) = Just n
isNumLitTy _                    = Nothing

mkStrLitTy :: FastString -> Type
mkStrLitTy s = LitTy (StrTyLit s)

-- | Is this a symbol literal. We also look through type synonyms.
isStrLitTy :: Type -> Maybe FastString
isStrLitTy ty | Just ty1 <- coreView ty = isStrLitTy ty1
isStrLitTy (LitTy (StrTyLit s)) = Just s
isStrLitTy _                    = Nothing

-- | Is this a type literal (symbol or numeric).
isLitTy :: Type -> Maybe TyLit
isLitTy ty | Just ty1 <- coreView ty = isLitTy ty1
isLitTy (LitTy l)                    = Just l
isLitTy _                            = Nothing

-- | Is this type a custom user error?
-- If so, give us the kind and the error message.
userTypeError_maybe :: Type -> Maybe Type
userTypeError_maybe t
  = do { (tc, _kind : msg : _) <- splitTyConApp_maybe t
          -- There may be more than 2 arguments, if the type error is
          -- used as a type constructor (e.g. at kind `Type -> Type`).

       ; guard (tyConName tc == errorMessageTypeErrorFamName)
       ; return msg }

-- | Render a type corresponding to a user type error into a SDoc.
pprUserTypeErrorTy :: Type -> SDoc
pprUserTypeErrorTy ty =
  case splitTyConApp_maybe ty of

    -- Text "Something"
    Just (tc,[txt])
      | tyConName tc == typeErrorTextDataConName
      , Just str <- isStrLitTy txt -> ftext str

    -- ShowType t
    Just (tc,[_k,t])
      | tyConName tc == typeErrorShowTypeDataConName -> ppr t

    -- t1 :<>: t2
    Just (tc,[t1,t2])
      | tyConName tc == typeErrorAppendDataConName ->
        pprUserTypeErrorTy t1 <> pprUserTypeErrorTy t2

    -- t1 :$$: t2
    Just (tc,[t1,t2])
      | tyConName tc == typeErrorVAppendDataConName ->
        pprUserTypeErrorTy t1 $$ pprUserTypeErrorTy t2

    -- An unevaluated type function
    _ -> ppr ty




{-
---------------------------------------------------------------------
                                FunTy
                                ~~~~~

Note [Representation of function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions (e.g. Int -> Char) can be thought of as being applications
of funTyCon (known in Haskell surface syntax as (->)),

    (->) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                   (a :: TYPE r1) (b :: TYPE r2).
            a -> b -> Type

However, for efficiency's sake we represent saturated applications of (->)
with FunTy. For instance, the type,

    (->) r1 r2 a b

is equivalent to,

    FunTy (Anon a) b

Note how the RuntimeReps are implied in the FunTy representation. For this
reason we must be careful when recontructing the TyConApp representation (see,
for instance, splitTyConApp_maybe).

In the compiler we maintain the invariant that all saturated applications of
(->) are represented with FunTy.

See #11714.
-}

splitFunTy :: Type -> (Type, Type)
-- ^ Attempts to extract the argument and result types from a type, and
-- panics if that is not possible. See also 'splitFunTy_maybe'
splitFunTy ty | Just ty' <- coreView ty = splitFunTy ty'
splitFunTy (FunTy _ arg res) = (arg, res)
splitFunTy other             = pprPanic "splitFunTy" (ppr other)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Attempts to extract the argument and result types from a type
splitFunTy_maybe ty | Just ty' <- coreView ty = splitFunTy_maybe ty'
splitFunTy_maybe (FunTy _ arg res) = Just (arg, res)
splitFunTy_maybe _                 = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty ty | Just ty' <- coreView ty = split args orig_ty ty'
    split args _       (FunTy _ arg res) = split (arg:args) res res
    split args orig_ty _                 = (reverse args, orig_ty)

funResultTy :: Type -> Type
-- ^ Extract the function result type and panic if that is not possible
funResultTy ty | Just ty' <- coreView ty = funResultTy ty'
funResultTy (FunTy { ft_res = res }) = res
funResultTy ty                       = pprPanic "funResultTy" (ppr ty)

funArgTy :: Type -> Type
-- ^ Extract the function argument type and panic if that is not possible
funArgTy ty | Just ty' <- coreView ty = funArgTy ty'
funArgTy (FunTy { ft_arg = arg })    = arg
funArgTy ty                           = pprPanic "funArgTy" (ppr ty)

-- ^ Just like 'piResultTys' but for a single argument
-- Try not to iterate 'piResultTy', because it's inefficient to substitute
-- one variable at a time; instead use 'piResultTys"
piResultTy :: HasDebugCallStack => Type -> Type ->  Type
piResultTy ty arg = case piResultTy_maybe ty arg of
                      Just res -> res
                      Nothing  -> pprPanic "piResultTy" (ppr ty $$ ppr arg)

piResultTy_maybe :: Type -> Type -> Maybe Type
-- We don't need a 'tc' version, because
-- this function behaves the same for Type and Constraint
piResultTy_maybe ty arg
  | Just ty' <- coreView ty = piResultTy_maybe ty' arg

  | FunTy { ft_res = res } <- ty
  = Just res

  | ForAllTy (Bndr tv _) res <- ty
  = let empty_subst = mkEmptyTCvSubst $ mkInScopeSet $
                      tyCoVarsOfTypes [arg,res]
    in Just (substTy (extendTCvSubst empty_subst tv arg) res)

  | otherwise
  = Nothing

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
  | Just ty' <- coreView ty
  = piResultTys ty' orig_args

  | FunTy { ft_res = res } <- ty
  = piResultTys res args

  | ForAllTy (Bndr tv _) res <- ty
  = go (extendTCvSubst init_subst tv arg) res args

  | otherwise
  = pprPanic "piResultTys1" (ppr ty $$ ppr orig_args)
  where
    init_subst = mkEmptyTCvSubst $ mkInScopeSet (tyCoVarsOfTypes (ty:orig_args))

    go :: TCvSubst -> Type -> [Type] -> Type
    go subst ty [] = substTyUnchecked subst ty

    go subst ty all_args@(arg:args)
      | Just ty' <- coreView ty
      = go subst ty' all_args

      | FunTy { ft_res = res } <- ty
      = go subst res args

      | ForAllTy (Bndr tv _) res <- ty
      = go (extendTCvSubst subst tv arg) res args

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

applyTysX :: [TyVar] -> Type -> [Type] -> Type
-- applyTyxX beta-reduces (/\tvs. body_ty) arg_tys
-- Assumes that (/\tvs. body_ty) is closed
applyTysX tvs body_ty arg_tys
  = ASSERT2( arg_tys `lengthAtLeast` n_tvs, pp_stuff )
    ASSERT2( tyCoVarsOfType body_ty `subVarSet` mkVarSet tvs, pp_stuff )
    mkAppTys (substTyWith tvs (take n_tvs arg_tys) body_ty)
             (drop n_tvs arg_tys)
  where
    pp_stuff = vcat [ppr tvs, ppr body_ty, ppr arg_tys]
    n_tvs = length tvs



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

--------------------------------------
Note [mkTyConApp and Type]

Whilst benchmarking it was observed in #17292 that GHC allocated a lot
of `TyConApp` constructors. Upon further inspection a large number of these
TyConApp constructors were all duplicates of `Type` applied to no arguments.

```
(From a sample of 100000 TyConApp closures)
0x45f3523    - 28732 - `Type`
0x420b840702 - 9629  - generic type constructors
0x42055b7e46 - 9596
0x420559b582 - 9511
0x420bb15a1e - 9509
0x420b86c6ba - 9501
0x42055bac1e - 9496
0x45e68fd    - 538 - `TYPE ...`
```

Therefore in `mkTyConApp` we have a special case for `Type` to ensure that
only one `TyConApp 'Type []` closure is allocated during the course of
compilation. In order to avoid a potentially expensive series of checks in
`mkTyConApp` only this egregious case is special cased at the moment.


---------------------------------------------------------------------
                                TyConApp
                                ~~~~~~~~
-}

-- | A key function: builds a 'TyConApp' or 'FunTy' as appropriate to
-- its arguments.  Applies its arguments to the constructor from left to right.
mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tycon tys
  | isFunTyCon tycon
  , [_rep1,_rep2,ty1,ty2] <- tys
  -- The FunTyCon (->) is always a visible one
  = FunTy { ft_af = VisArg, ft_arg = ty1, ft_res = ty2 }
  -- Note [mkTyConApp and Type]
  | tycon == liftedTypeKindTyCon
  = ASSERT2( null tys, ppr tycon $$ ppr tys )
    liftedTypeKindTyConApp
  | otherwise
  = TyConApp tycon tys

-- This is a single, global definition of the type `Type`
-- Defined here so it is only allocated once.
-- See Note [mkTyConApp and Type]
liftedTypeKindTyConApp :: Type
liftedTypeKindTyConApp = TyConApp liftedTypeKindTyCon []

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

-- | Retrieve the tycon heading this type, if there is one. Does /not/
-- look through synonyms.
tyConAppTyConPicky_maybe :: Type -> Maybe TyCon
tyConAppTyConPicky_maybe (TyConApp tc _) = Just tc
tyConAppTyConPicky_maybe (FunTy {})      = Just funTyCon
tyConAppTyConPicky_maybe _               = Nothing


-- | The same as @fst . splitTyConApp@
tyConAppTyCon_maybe :: Type -> Maybe TyCon
tyConAppTyCon_maybe ty | Just ty' <- coreView ty = tyConAppTyCon_maybe ty'
tyConAppTyCon_maybe (TyConApp tc _) = Just tc
tyConAppTyCon_maybe (FunTy {})      = Just funTyCon
tyConAppTyCon_maybe _               = Nothing

tyConAppTyCon :: Type -> TyCon
tyConAppTyCon ty = tyConAppTyCon_maybe ty `orElse` pprPanic "tyConAppTyCon" (ppr ty)

-- | The same as @snd . splitTyConApp@
tyConAppArgs_maybe :: Type -> Maybe [Type]
tyConAppArgs_maybe ty | Just ty' <- coreView ty = tyConAppArgs_maybe ty'
tyConAppArgs_maybe (TyConApp _ tys) = Just tys
tyConAppArgs_maybe (FunTy _ arg res)
  | Just rep1 <- getRuntimeRep_maybe arg
  , Just rep2 <- getRuntimeRep_maybe res
  = Just [rep1, rep2, arg, res]
tyConAppArgs_maybe _  = Nothing

tyConAppArgs :: Type -> [Type]
tyConAppArgs ty = tyConAppArgs_maybe ty `orElse` pprPanic "tyConAppArgs" (ppr ty)

tyConAppArgN :: Int -> Type -> Type
-- Executing Nth
tyConAppArgN n ty
  = case tyConAppArgs_maybe ty of
      Just tys -> tys `getNth` n
      Nothing  -> pprPanic "tyConAppArgN" (ppr n <+> ppr ty)

-- | Attempts to tease a type apart into a type constructor and the application
-- of a number of arguments to that constructor. Panics if that is not possible.
-- See also 'splitTyConApp_maybe'
splitTyConApp :: Type -> (TyCon, [Type])
splitTyConApp ty = case splitTyConApp_maybe ty of
                   Just stuff -> stuff
                   Nothing    -> pprPanic "splitTyConApp" (ppr ty)

-- | Attempts to tease a type apart into a type constructor and the application
-- of a number of arguments to that constructor
splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe ty | Just ty' <- coreView ty = splitTyConApp_maybe ty'
splitTyConApp_maybe ty                           = repSplitTyConApp_maybe ty

-- | Split a type constructor application into its type constructor and
-- applied types. Note that this may fail in the case of a 'FunTy' with an
-- argument of unknown kind 'FunTy' (e.g. @FunTy (a :: k) Int@. since the kind
-- of @a@ isn't of the form @TYPE rep@). Consequently, you may need to zonk your
-- type before using this function.
--
-- If you only need the 'TyCon', consider using 'tcTyConAppTyCon_maybe'.
tcSplitTyConApp_maybe :: HasCallStack => Type -> Maybe (TyCon, [Type])
-- Defined here to avoid module loops between Unify and TcType.
tcSplitTyConApp_maybe ty | Just ty' <- tcView ty = tcSplitTyConApp_maybe ty'
tcSplitTyConApp_maybe ty                         = repSplitTyConApp_maybe ty

-------------------
repSplitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
-- ^ Like 'splitTyConApp_maybe', but doesn't look through synonyms. This
-- assumes the synonyms have already been dealt with.
--
-- Moreover, for a FunTy, it only succeeds if the argument types
-- have enough info to extract the runtime-rep arguments that
-- the funTyCon requires.  This will usually be true;
-- but may be temporarily false during canonicalization:
--     see Note [FunTy and decomposing tycon applications] in TcCanonical
--
repSplitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
repSplitTyConApp_maybe (FunTy _ arg res)
  | Just arg_rep <- getRuntimeRep_maybe arg
  , Just res_rep <- getRuntimeRep_maybe res
  = Just (funTyCon, [arg_rep, res_rep, arg, res])
repSplitTyConApp_maybe _ = Nothing

-------------------
-- | Attempts to tease a list type apart and gives the type of the elements if
-- successful (looks through type synonyms)
splitListTyConApp_maybe :: Type -> Maybe Type
splitListTyConApp_maybe ty = case splitTyConApp_maybe ty of
  Just (tc,[e]) | tc == listTyCon -> Just e
  _other                          -> Nothing

newTyConInstRhs :: TyCon -> [Type] -> Type
-- ^ Unwrap one 'layer' of newtype on a type constructor and its
-- arguments, using an eta-reduced version of the @newtype@ if possible.
-- This requires tys to have at least @newTyConInstArity tycon@ elements.
newTyConInstRhs tycon tys
    = ASSERT2( tvs `leLength` tys, ppr tycon $$ ppr tys $$ ppr tvs )
      applyTysX tvs rhs tys
  where
    (tvs, rhs) = newTyConEtadRhs tycon

{-
---------------------------------------------------------------------
                           CastTy
                           ~~~~~~
A casted type has its *kind* casted into something new.
-}

splitCastTy_maybe :: Type -> Maybe (Type, Coercion)
splitCastTy_maybe ty | Just ty' <- coreView ty = splitCastTy_maybe ty'
splitCastTy_maybe (CastTy ty co)               = Just (ty, co)
splitCastTy_maybe _                            = Nothing

-- | Make a 'CastTy'. The Coercion must be nominal. Checks the
-- Coercion for reflexivity, dropping it if it's reflexive.
-- See Note [Respecting definitional equality] in GHC.Core.TyCo.Rep
mkCastTy :: Type -> Coercion -> Type
mkCastTy ty co | isReflexiveCo co = ty  -- (EQ2) from the Note
-- NB: Do the slow check here. This is important to keep the splitXXX
-- functions working properly. Otherwise, we may end up with something
-- like (((->) |> something_reflexive_but_not_obviously_so) biz baz)
-- fails under splitFunTy_maybe. This happened with the cheaper check
-- in test dependent/should_compile/dynamic-paper.

mkCastTy (CastTy ty co1) co2
  -- (EQ3) from the Note
  = mkCastTy ty (co1 `mkTransCo` co2)
      -- call mkCastTy again for the reflexivity check

mkCastTy (ForAllTy (Bndr tv vis) inner_ty) co
  -- (EQ4) from the Note
  -- See Note [Weird typing rule for ForAllTy] in GHC.Core.TyCo.Rep.
  | isTyVar tv
  , let fvs = tyCoVarsOfCo co
  = -- have to make sure that pushing the co in doesn't capture the bound var!
    if tv `elemVarSet` fvs
    then let empty_subst = mkEmptyTCvSubst (mkInScopeSet fvs)
             (subst, tv') = substVarBndr empty_subst tv
         in ForAllTy (Bndr tv' vis) (substTy subst inner_ty `mkCastTy` co)
    else ForAllTy (Bndr tv vis) (inner_ty `mkCastTy` co)

mkCastTy ty co = CastTy ty co

tyConBindersTyCoBinders :: [TyConBinder] -> [TyCoBinder]
-- Return the tyConBinders in TyCoBinder form
tyConBindersTyCoBinders = map to_tyb
  where
    to_tyb (Bndr tv (NamedTCB vis)) = Named (Bndr tv vis)
    to_tyb (Bndr tv (AnonTCB af))   = Anon af (varType tv)

-- | Drop the cast on a type, if any. If there is no
-- cast, just return the original type. This is rarely what
-- you want. The CastTy data constructor (in GHC.Core.TyCo.Rep) has the
-- invariant that another CastTy is not inside. See the
-- data constructor for a full description of this invariant.
-- Since CastTy cannot be nested, the result of discardCast
-- cannot be a CastTy.
discardCast :: Type -> Type
discardCast (CastTy ty _) = ASSERT(not (isCastTy ty)) ty
  where
  isCastTy CastTy{} = True
  isCastTy _        = False
discardCast ty            = ty


{-
--------------------------------------------------------------------
                            CoercionTy
                            ~~~~~~~~~~
CoercionTy allows us to inject coercions into types. A CoercionTy
should appear only in the right-hand side of an application.
-}

mkCoercionTy :: Coercion -> Type
mkCoercionTy = CoercionTy

isCoercionTy :: Type -> Bool
isCoercionTy (CoercionTy _) = True
isCoercionTy _              = False

isCoercionTy_maybe :: Type -> Maybe Coercion
isCoercionTy_maybe (CoercionTy co) = Just co
isCoercionTy_maybe _               = Nothing

stripCoercionTy :: Type -> Coercion
stripCoercionTy (CoercionTy co) = co
stripCoercionTy ty              = pprPanic "stripCoercionTy" (ppr ty)

{-
---------------------------------------------------------------------
                                SynTy
                                ~~~~~

Notes on type synonyms
~~~~~~~~~~~~~~~~~~~~~~
The various "split" functions (splitFunTy, splitRhoTy, splitForAllTy) try
to return type synonyms wherever possible. Thus

        type Foo a = a -> a

we want
        splitFunTys (a -> Foo a) = ([a], Foo a)
not                                ([a], a -> a)

The reason is that we then get better (shorter) type signatures in
interfaces.  Notably this plays a role in tcTySigs in TcBinds.hs.


---------------------------------------------------------------------
                                ForAllTy
                                ~~~~~~~~
-}

-- | Make a dependent forall over an 'Inferred' variable
mkTyCoInvForAllTy :: TyCoVar -> Type -> Type
mkTyCoInvForAllTy tv ty
  | isCoVar tv
  , not (tv `elemVarSet` tyCoVarsOfType ty)
  = mkVisFunTy (varType tv) ty
  | otherwise
  = ForAllTy (Bndr tv Inferred) ty

-- | Like 'mkTyCoInvForAllTy', but tv should be a tyvar
mkInvForAllTy :: TyVar -> Type -> Type
mkInvForAllTy tv ty = ASSERT( isTyVar tv )
                      ForAllTy (Bndr tv Inferred) ty

-- | Like 'mkForAllTys', but assumes all variables are dependent and
-- 'Inferred', a common case
mkTyCoInvForAllTys :: [TyCoVar] -> Type -> Type
mkTyCoInvForAllTys tvs ty = foldr mkTyCoInvForAllTy ty tvs

-- | Like 'mkTyCoInvForAllTys', but tvs should be a list of tyvar
mkInvForAllTys :: [TyVar] -> Type -> Type
mkInvForAllTys tvs ty = foldr mkInvForAllTy ty tvs

-- | Like 'mkForAllTy', but assumes the variable is dependent and 'Specified',
-- a common case
mkSpecForAllTy :: TyVar -> Type -> Type
mkSpecForAllTy tv ty = ASSERT( isTyVar tv )
                       -- covar is always Inferred, so input should be tyvar
                       ForAllTy (Bndr tv Specified) ty

-- | Like 'mkForAllTys', but assumes all variables are dependent and
-- 'Specified', a common case
mkSpecForAllTys :: [TyVar] -> Type -> Type
mkSpecForAllTys tvs ty = foldr mkSpecForAllTy ty tvs

-- | Like mkForAllTys, but assumes all variables are dependent and visible
mkVisForAllTys :: [TyVar] -> Type -> Type
mkVisForAllTys tvs = ASSERT( all isTyVar tvs )
                     -- covar is always Inferred, so all inputs should be tyvar
                     mkForAllTys [ Bndr tv Required | tv <- tvs ]

mkLamType  :: Var -> Type -> Type
-- ^ Makes a @(->)@ type or an implicit forall type, depending
-- on whether it is given a type variable or a term variable.
-- This is used, for example, when producing the type of a lambda.
-- Always uses Inferred binders.
mkLamTypes :: [Var] -> Type -> Type
-- ^ 'mkLamType' for multiple type or value arguments

mkLamType v body_ty
   | isTyVar v
   = ForAllTy (Bndr v Inferred) body_ty

   | isCoVar v
   , v `elemVarSet` tyCoVarsOfType body_ty
   = ForAllTy (Bndr v Required) body_ty

   | isPredTy arg_ty  -- See Note [mkLamType: dictionary arguments]
   = mkInvisFunTy arg_ty body_ty

   | otherwise
   = mkVisFunTy arg_ty body_ty
   where
     arg_ty = varType v

mkLamTypes vs ty = foldr mkLamType ty vs

{- Note [mkLamType: dictionary arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have (\ (d :: Ord a). blah), we want to give it type
           (Ord a => blah_ty)
with a fat arrow; that is, using mkInvisFunTy, not mkVisFunTy.

Why? After all, we are in Core, where (=>) and (->) behave the same.
Yes, but the /specialiser/ does treat dictionary arguments specially.
Suppose we do w/w on 'foo' in module A, thus (#11272, #6056)
   foo :: Ord a => Int -> blah
   foo a d x = case x of I# x' -> $wfoo @a d x'

   $wfoo :: Ord a => Int# -> blah

Now in module B we see (foo @Int dOrdInt).  The specialiser will
specialise this to $sfoo, where
   $sfoo :: Int -> blah
   $sfoo x = case x of I# x' -> $wfoo @Int dOrdInt x'

Now we /must/ also specialise $wfoo!  But it wasn't user-written,
and has a type built with mkLamTypes.

Conclusion: the easiest thing is to make mkLamType build
            (c => ty)
when the argument is a predicate type.  See GHC.Core.TyCo.Rep
Note [Types for coercions, predicates, and evidence]
-}

-- | Given a list of type-level vars and the free vars of a result kind,
-- makes TyCoBinders, preferring anonymous binders
-- if the variable is, in fact, not dependent.
-- e.g.    mkTyConBindersPreferAnon [(k:*),(b:k),(c:k)] (k->k)
-- We want (k:*) Named, (b:k) Anon, (c:k) Anon
--
-- All non-coercion binders are /visible/.
mkTyConBindersPreferAnon :: [TyVar]      -- ^ binders
                         -> TyCoVarSet   -- ^ free variables of result
                         -> [TyConBinder]
mkTyConBindersPreferAnon vars inner_tkvs = ASSERT( all isTyVar vars)
                                           fst (go vars)
  where
    go :: [TyVar] -> ([TyConBinder], VarSet) -- also returns the free vars
    go [] = ([], inner_tkvs)
    go (v:vs) | v `elemVarSet` fvs
              = ( Bndr v (NamedTCB Required) : binders
                , fvs `delVarSet` v `unionVarSet` kind_vars )
              | otherwise
              = ( Bndr v (AnonTCB VisArg) : binders
                , fvs `unionVarSet` kind_vars )
      where
        (binders, fvs) = go vs
        kind_vars      = tyCoVarsOfType $ tyVarKind v

-- | Take a ForAllTy apart, returning the list of tycovars and the result type.
-- This always succeeds, even if it returns only an empty list. Note that the
-- result type returned may have free variables that were bound by a forall.
splitForAllTys :: Type -> ([TyCoVar], Type)
splitForAllTys ty = split ty ty []
  where
    split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
    split _       (ForAllTy (Bndr tv _) ty)    tvs = split ty ty (tv:tvs)
    split orig_ty _                            tvs = (reverse tvs, orig_ty)

-- | Like 'splitForAllTys', but only splits a 'ForAllTy' if
-- @'sameVis' argf supplied_argf@ is 'True', where @argf@ is the visibility
-- of the @ForAllTy@'s binder and @supplied_argf@ is the visibility provided
-- as an argument to this function.
splitForAllTysSameVis :: ArgFlag -> Type -> ([TyCoVar], Type)
splitForAllTysSameVis supplied_argf ty = split ty ty []
  where
    split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
    split _       (ForAllTy (Bndr tv argf) ty) tvs
      | argf `sameVis` supplied_argf               = split ty ty (tv:tvs)
    split orig_ty _                            tvs = (reverse tvs, orig_ty)

-- | Like splitForAllTys, but split only for tyvars.
-- This always succeeds, even if it returns only an empty list. Note that the
-- result type returned may have free variables that were bound by a forall.
splitTyVarForAllTys :: Type -> ([TyVar], Type)
splitTyVarForAllTys ty = split ty ty []
  where
    split orig_ty ty tvs | Just ty' <- coreView ty     = split orig_ty ty' tvs
    split _ (ForAllTy (Bndr tv _) ty) tvs | isTyVar tv = split ty ty (tv:tvs)
    split orig_ty _                   tvs              = (reverse tvs, orig_ty)

-- | Checks whether this is a proper forall (with a named binder)
isForAllTy :: Type -> Bool
isForAllTy ty | Just ty' <- coreView ty = isForAllTy ty'
isForAllTy (ForAllTy {}) = True
isForAllTy _             = False

-- | Like `isForAllTy`, but returns True only if it is a tyvar binder
isForAllTy_ty :: Type -> Bool
isForAllTy_ty ty | Just ty' <- coreView ty = isForAllTy_ty ty'
isForAllTy_ty (ForAllTy (Bndr tv _) _) | isTyVar tv = True
isForAllTy_ty _             = False

-- | Like `isForAllTy`, but returns True only if it is a covar binder
isForAllTy_co :: Type -> Bool
isForAllTy_co ty | Just ty' <- coreView ty = isForAllTy_co ty'
isForAllTy_co (ForAllTy (Bndr tv _) _) | isCoVar tv = True
isForAllTy_co _             = False

-- | Is this a function or forall?
isPiTy :: Type -> Bool
isPiTy ty | Just ty' <- coreView ty = isPiTy ty'
isPiTy (ForAllTy {}) = True
isPiTy (FunTy {})    = True
isPiTy _             = False

-- | Is this a function?
isFunTy :: Type -> Bool
isFunTy ty | Just ty' <- coreView ty = isFunTy ty'
isFunTy (FunTy {}) = True
isFunTy _          = False

-- | Take a forall type apart, or panics if that is not possible.
splitForAllTy :: Type -> (TyCoVar, Type)
splitForAllTy ty
  | Just answer <- splitForAllTy_maybe ty = answer
  | otherwise                             = pprPanic "splitForAllTy" (ppr ty)

-- | Drops all ForAllTys
dropForAlls :: Type -> Type
dropForAlls ty = go ty
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy _ res)            = go res
    go res                         = res

-- | Attempts to take a forall type apart, but only if it's a proper forall,
-- with a named binder
splitForAllTy_maybe :: Type -> Maybe (TyCoVar, Type)
splitForAllTy_maybe ty = go ty
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy (Bndr tv _) ty)    = Just (tv, ty)
    go _                            = Nothing

-- | Like splitForAllTy_maybe, but only returns Just if it is a tyvar binder.
splitForAllTy_ty_maybe :: Type -> Maybe (TyCoVar, Type)
splitForAllTy_ty_maybe ty = go ty
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy (Bndr tv _) ty) | isTyVar tv = Just (tv, ty)
    go _                            = Nothing

-- | Like splitForAllTy_maybe, but only returns Just if it is a covar binder.
splitForAllTy_co_maybe :: Type -> Maybe (TyCoVar, Type)
splitForAllTy_co_maybe ty = go ty
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy (Bndr tv _) ty) | isCoVar tv = Just (tv, ty)
    go _                            = Nothing

-- | Attempts to take a forall type apart; works with proper foralls and
-- functions
splitPiTy_maybe :: Type -> Maybe (TyCoBinder, Type)
splitPiTy_maybe ty = go ty
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy bndr ty) = Just (Named bndr, ty)
    go (FunTy { ft_af = af, ft_arg = arg, ft_res = res})
                          = Just (Anon af arg, res)
    go _                  = Nothing

-- | Takes a forall type apart, or panics
splitPiTy :: Type -> (TyCoBinder, Type)
splitPiTy ty
  | Just answer <- splitPiTy_maybe ty = answer
  | otherwise                         = pprPanic "splitPiTy" (ppr ty)

-- | Split off all TyCoBinders to a type, splitting both proper foralls
-- and functions
splitPiTys :: Type -> ([TyCoBinder], Type)
splitPiTys ty = split ty ty []
  where
    split orig_ty ty bs | Just ty' <- coreView ty = split orig_ty ty' bs
    split _       (ForAllTy b res) bs = split res res (Named b  : bs)
    split _       (FunTy { ft_af = af, ft_arg = arg, ft_res = res }) bs
                                      = split res res (Anon af arg : bs)
    split orig_ty _                bs = (reverse bs, orig_ty)

-- | Like 'splitPiTys' but split off only /named/ binders
--   and returns TyCoVarBinders rather than TyCoBinders
splitForAllVarBndrs :: Type -> ([TyCoVarBinder], Type)
splitForAllVarBndrs ty = split ty ty []
  where
    split orig_ty ty bs | Just ty' <- coreView ty = split orig_ty ty' bs
    split _       (ForAllTy b res) bs = split res res (b:bs)
    split orig_ty _                bs = (reverse bs, orig_ty)
{-# INLINE splitForAllVarBndrs #-}

invisibleTyBndrCount :: Type -> Int
-- Returns the number of leading invisible forall'd binders in the type
-- Includes invisible predicate arguments; e.g. for
--    e.g.  forall {k}. (k ~ *) => k -> k
-- returns 2 not 1
invisibleTyBndrCount ty = length (fst (splitPiTysInvisible ty))

-- Like splitPiTys, but returns only *invisible* binders, including constraints
-- Stops at the first visible binder
splitPiTysInvisible :: Type -> ([TyCoBinder], Type)
splitPiTysInvisible ty = split ty ty []
   where
    split orig_ty ty bs
      | Just ty' <- coreView ty  = split orig_ty ty' bs
    split _ (ForAllTy b res) bs
      | Bndr _ vis <- b
      , isInvisibleArgFlag vis   = split res res (Named b  : bs)
    split _ (FunTy { ft_af = InvisArg, ft_arg = arg, ft_res = res })  bs
                                 = split res res (Anon InvisArg arg : bs)
    split orig_ty _          bs  = (reverse bs, orig_ty)

splitPiTysInvisibleN :: Int -> Type -> ([TyCoBinder], Type)
-- Same as splitPiTysInvisible, but stop when
--   - you have found 'n' TyCoBinders,
--   - or you run out of invisible binders
splitPiTysInvisibleN n ty = split n ty ty []
   where
    split n orig_ty ty bs
      | n == 0                  = (reverse bs, orig_ty)
      | Just ty' <- coreView ty = split n orig_ty ty' bs
      | ForAllTy b res <- ty
      , Bndr _ vis <- b
      , isInvisibleArgFlag vis  = split (n-1) res res (Named b  : bs)
      | FunTy { ft_af = InvisArg, ft_arg = arg, ft_res = res } <- ty
                                = split (n-1) res res (Anon InvisArg arg : bs)
      | otherwise               = (reverse bs, orig_ty)

-- | Given a 'TyCon' and a list of argument types, filter out any invisible
-- (i.e., 'Inferred' or 'Specified') arguments.
filterOutInvisibleTypes :: TyCon -> [Type] -> [Type]
filterOutInvisibleTypes tc tys = snd $ partitionInvisibleTypes tc tys

-- | Given a 'TyCon' and a list of argument types, filter out any 'Inferred'
-- arguments.
filterOutInferredTypes :: TyCon -> [Type] -> [Type]
filterOutInferredTypes tc tys =
  filterByList (map (/= Inferred) $ tyConArgFlags tc tys) tys

-- | Given a 'TyCon' and a list of argument types, partition the arguments
-- into:
--
-- 1. 'Inferred' or 'Specified' (i.e., invisible) arguments and
--
-- 2. 'Required' (i.e., visible) arguments
partitionInvisibleTypes :: TyCon -> [Type] -> ([Type], [Type])
partitionInvisibleTypes tc tys =
  partitionByList (map isInvisibleArgFlag $ tyConArgFlags tc tys) tys

-- | Given a list of things paired with their visibilities, partition the
-- things into (invisible things, visible things).
partitionInvisibles :: [(a, ArgFlag)] -> ([a], [a])
partitionInvisibles = partitionWith pick_invis
  where
    pick_invis :: (a, ArgFlag) -> Either a a
    pick_invis (thing, vis) | isInvisibleArgFlag vis = Left thing
                            | otherwise              = Right thing

-- | Given a 'TyCon' and a list of argument types to which the 'TyCon' is
-- applied, determine each argument's visibility
-- ('Inferred', 'Specified', or 'Required').
--
-- Wrinkle: consider the following scenario:
--
-- > T :: forall k. k -> k
-- > tyConArgFlags T [forall m. m -> m -> m, S, R, Q]
--
-- After substituting, we get
--
-- > T (forall m. m -> m -> m) :: (forall m. m -> m -> m) -> forall n. n -> n -> n
--
-- Thus, the first argument is invisible, @S@ is visible, @R@ is invisible again,
-- and @Q@ is visible.
tyConArgFlags :: TyCon -> [Type] -> [ArgFlag]
tyConArgFlags tc = fun_kind_arg_flags (tyConKind tc)

-- | Given a 'Type' and a list of argument types to which the 'Type' is
-- applied, determine each argument's visibility
-- ('Inferred', 'Specified', or 'Required').
--
-- Most of the time, the arguments will be 'Required', but not always. Consider
-- @f :: forall a. a -> Type@. In @f Type Bool@, the first argument (@Type@) is
-- 'Specified' and the second argument (@Bool@) is 'Required'. It is precisely
-- this sort of higher-rank situation in which 'appTyArgFlags' comes in handy,
-- since @f Type Bool@ would be represented in Core using 'AppTy's.
-- (See also #15792).
appTyArgFlags :: Type -> [Type] -> [ArgFlag]
appTyArgFlags ty = fun_kind_arg_flags (typeKind ty)

-- | Given a function kind and a list of argument types (where each argument's
-- kind aligns with the corresponding position in the argument kind), determine
-- each argument's visibility ('Inferred', 'Specified', or 'Required').
fun_kind_arg_flags :: Kind -> [Type] -> [ArgFlag]
fun_kind_arg_flags = go emptyTCvSubst
  where
    go subst ki arg_tys
      | Just ki' <- coreView ki = go subst ki' arg_tys
    go _ _ [] = []
    go subst (ForAllTy (Bndr tv argf) res_ki) (arg_ty:arg_tys)
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
    -- Here, we want to get the following ArgFlags:
    --
    -- [Inferred,   Specified, Required, Required, Specified, Required]
    -- forall {k1}. forall k2. k1 ->     k2 ->     forall k3. k3 ->     Type
    go subst (FunTy{ft_af = af, ft_res = res_ki}) (_:arg_tys)
      = argf : go subst res_ki arg_tys
      where
        argf = case af of
                 VisArg   -> Required
                 InvisArg -> Inferred
    go _ _ arg_tys = map (const Required) arg_tys
                        -- something is ill-kinded. But this can happen
                        -- when printing errors. Assume everything is Required.

-- @isTauTy@ tests if a type has no foralls
isTauTy :: Type -> Bool
isTauTy ty | Just ty' <- coreView ty = isTauTy ty'
isTauTy (TyVarTy _)           = True
isTauTy (LitTy {})            = True
isTauTy (TyConApp tc tys)     = all isTauTy tys && isTauTyCon tc
isTauTy (AppTy a b)           = isTauTy a && isTauTy b
isTauTy (FunTy _ a b)         = isTauTy a && isTauTy b
isTauTy (ForAllTy {})         = False
isTauTy (CastTy ty _)         = isTauTy ty
isTauTy (CoercionTy _)        = False  -- Not sure about this

{-
%************************************************************************
%*                                                                      *
   TyCoBinders
%*                                                                      *
%************************************************************************
-}

-- | Make an anonymous binder
mkAnonBinder :: AnonArgFlag -> Type -> TyCoBinder
mkAnonBinder = Anon

-- | Does this binder bind a variable that is /not/ erased? Returns
-- 'True' for anonymous binders.
isAnonTyCoBinder :: TyCoBinder -> Bool
isAnonTyCoBinder (Named {}) = False
isAnonTyCoBinder (Anon {})  = True

tyCoBinderVar_maybe :: TyCoBinder -> Maybe TyCoVar
tyCoBinderVar_maybe (Named tv) = Just $ binderVar tv
tyCoBinderVar_maybe _          = Nothing

tyCoBinderType :: TyCoBinder -> Type
tyCoBinderType (Named tvb) = binderType tvb
tyCoBinderType (Anon _ ty) = ty

tyBinderType :: TyBinder -> Type
tyBinderType (Named (Bndr tv _))
  = ASSERT( isTyVar tv )
    tyVarKind tv
tyBinderType (Anon _ ty)   = ty

-- | Extract a relevant type, if there is one.
binderRelevantType_maybe :: TyCoBinder -> Maybe Type
binderRelevantType_maybe (Named {})  = Nothing
binderRelevantType_maybe (Anon _ ty) = Just ty

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
  | Just (fam_tc, fam_tys) <- tyConFamInst_maybe tc
  , let tvs = tyConTyVars tc
        fam_subst = ASSERT2( tvs `equalLength` tys, ppr tc <+> ppr tys )
                    zipTvSubst tvs tys
  = mkTyConApp fam_tc (substTys fam_subst fam_tys)
  | otherwise
  = mkTyConApp tc tys

-- | Get the type on the LHS of a coercion induced by a type/data
-- family instance.
coAxNthLHS :: CoAxiom br -> Int -> Type
coAxNthLHS ax ind =
  mkTyConApp (coAxiomTyCon ax) (coAxBranchLHS (coAxiomNthBranch ax ind))

isFamFreeTy :: Type -> Bool
isFamFreeTy ty | Just ty' <- coreView ty = isFamFreeTy ty'
isFamFreeTy (TyVarTy _)       = True
isFamFreeTy (LitTy {})        = True
isFamFreeTy (TyConApp tc tys) = all isFamFreeTy tys && isFamFreeTyCon tc
isFamFreeTy (AppTy a b)       = isFamFreeTy a && isFamFreeTy b
isFamFreeTy (FunTy _ a b)     = isFamFreeTy a && isFamFreeTy b
isFamFreeTy (ForAllTy _ ty)   = isFamFreeTy ty
isFamFreeTy (CastTy ty _)     = isFamFreeTy ty
isFamFreeTy (CoercionTy _)    = False  -- Not sure about this

-- | Does this type classify a core (unlifted) Coercion?
-- At either role nominal or representational
--    (t1 ~# t2) or (t1 ~R# t2)
-- See Note [Types for coercions, predicates, and evidence] in GHC.Core.TyCo.Rep
isCoVarType :: Type -> Bool
  -- ToDo: should we check saturation?
isCoVarType ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey
  | otherwise
  = False

buildSynTyCon :: Name -> [KnotTied TyConBinder] -> Kind   -- ^ /result/ kind
              -> [Role] -> KnotTied Type -> TyCon
-- This function is here beucase here is where we have
--   isFamFree and isTauTy
buildSynTyCon name binders res_kind roles rhs
  = mkSynonymTyCon name binders res_kind roles rhs is_tau is_fam_free
  where
    is_tau      = isTauTy rhs
    is_fam_free = isFamFreeTy rhs

{-
************************************************************************
*                                                                      *
\subsection{Liftedness}
*                                                                      *
************************************************************************
-}

-- | Returns Just True if this type is surely lifted, Just False
-- if it is surely unlifted, Nothing if we can't be sure (i.e., it is
-- levity polymorphic), and panics if the kind does not have the shape
-- TYPE r.
isLiftedType_maybe :: HasDebugCallStack => Type -> Maybe Bool
isLiftedType_maybe ty = go (getRuntimeRep ty)
  where
    go rr | Just rr' <- coreView rr = go rr'
          | isLiftedRuntimeRep rr  = Just True
          | TyConApp {} <- rr      = Just False  -- Everything else is unlifted
          | otherwise              = Nothing     -- levity polymorphic

-- | See "Type#type_classification" for what an unlifted type is.
-- Panics on levity polymorphic types; See 'mightBeUnliftedType' for
-- a more approximate predicate that behaves better in the presence of
-- levity polymorphism.
isUnliftedType :: HasDebugCallStack => Type -> Bool
        -- isUnliftedType returns True for forall'd unlifted types:
        --      x :: forall a. Int#
        -- I found bindings like these were getting floated to the top level.
        -- They are pretty bogus types, mind you.  It would be better never to
        -- construct them
isUnliftedType ty
  = not (isLiftedType_maybe ty `orElse`
         pprPanic "isUnliftedType" (ppr ty <+> dcolon <+> ppr (typeKind ty)))

-- | Returns:
--
-- * 'False' if the type is /guaranteed/ lifted or
-- * 'True' if it is unlifted, OR we aren't sure (e.g. in a levity-polymorphic case)
mightBeUnliftedType :: Type -> Bool
mightBeUnliftedType ty
  = case isLiftedType_maybe ty of
      Just is_lifted -> not is_lifted
      Nothing -> True

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
-- @getRuntimeRep_maybe Int = LiftedRep@. Returns 'Nothing' if this is not
-- possible.
getRuntimeRep_maybe :: HasDebugCallStack
                    => Type -> Maybe Type
getRuntimeRep_maybe = kindRep_maybe . typeKind

-- | Extract the RuntimeRep classifier of a type. For instance,
-- @getRuntimeRep_maybe Int = LiftedRep@. Panics if this is not possible.
getRuntimeRep :: HasDebugCallStack => Type -> Type
getRuntimeRep ty
  = case getRuntimeRep_maybe ty of
      Just r  -> r
      Nothing -> pprPanic "getRuntimeRep" (ppr ty <+> dcolon <+> ppr (typeKind ty))

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
      Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
                            isAlgTyCon tc
      _other             -> False

-- | Check whether a type is a data family type
isDataFamilyAppType :: Type -> Bool
isDataFamilyAppType ty = case tyConAppTyCon_maybe ty of
                           Just tc -> isDataFamilyTyCon tc
                           _       -> False

-- | Computes whether an argument (or let right hand side) should
-- be computed strictly or lazily, based only on its type.
-- Currently, it's just 'isUnliftedType'. Panics on levity-polymorphic types.
isStrictType :: HasDebugCallStack => Type -> Bool
isStrictType = isUnliftedType

isPrimitiveType :: Type -> Bool
-- ^ Returns true of types that are opaque to Haskell.
isPrimitiveType ty = case splitTyConApp_maybe ty of
                        Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
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
-- (See Note [The polymorphism rule of join points] in GHC.Core.) Returns False
-- also if the type simply doesn't have enough arguments.
--
-- Note that we need to know how many arguments (type *and* value) the putative
-- join point takes; for instance, if
--   j :: forall a. a -> Int
-- then j could be a binary join point returning an Int, but it could *not* be a
-- unary join point returning a -> Int.
--
-- TODO: See Note [Excess polymorphism and join points]
isValidJoinPointType :: JoinArity -> Type -> Bool
isValidJoinPointType arity ty
  = valid_under emptyVarSet arity ty
  where
    valid_under tvs arity ty
      | arity == 0
      = tvs `disjointVarSet` tyCoVarsOfType ty
      | Just (t, ty') <- splitForAllTy_maybe ty
      = valid_under (tvs `extendVarSet` t) (arity-1) ty'
      | Just (_, res_ty) <- splitFunTy_maybe ty
      = valid_under tvs (arity-1) res_ty
      | otherwise
      = False

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


************************************************************************
*                                                                      *
\subsection{Sequencing on types}
*                                                                      *
************************************************************************
-}

seqType :: Type -> ()
seqType (LitTy n)                   = n `seq` ()
seqType (TyVarTy tv)                = tv `seq` ()
seqType (AppTy t1 t2)               = seqType t1 `seq` seqType t2
seqType (FunTy _ t1 t2)             = seqType t1 `seq` seqType t2
seqType (TyConApp tc tys)           = tc `seq` seqTypes tys
seqType (ForAllTy (Bndr tv _) ty)   = seqType (varType tv) `seq` seqType ty
seqType (CastTy ty co)              = seqType ty `seq` seqCo co
seqType (CoercionTy co)             = seqCo co

seqTypes :: [Type] -> ()
seqTypes []       = ()
seqTypes (ty:tys) = seqType ty `seq` seqTypes tys

{-
************************************************************************
*                                                                      *
                Comparison for types
        (We don't use instances so that we know where it happens)
*                                                                      *
************************************************************************

Note [Equality on AppTys]
~~~~~~~~~~~~~~~~~~~~~~~~~
In our cast-ignoring equality, we want to say that the following two
are equal:

  (Maybe |> co) (Int |> co')   ~?       Maybe Int

But the left is an AppTy while the right is a TyConApp. The solution is
to use repSplitAppTy_maybe to break up the TyConApp into its pieces and
then continue. Easy to do, but also easy to forget to do.

-}

eqType :: Type -> Type -> Bool
-- ^ Type equality on source types. Does not look through @newtypes@ or
-- 'PredType's, but it does look through type synonyms.
-- This first checks that the kinds of the types are equal and then
-- checks whether the types are equal, ignoring casts and coercions.
-- (The kind check is a recursive call, but since all kinds have type
-- @Type@, there is no need to check the types of kinds.)
-- See also Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep.
eqType t1 t2 = isEqual $ nonDetCmpType t1 t2
  -- It's OK to use nonDetCmpType here and eqType is deterministic,
  -- nonDetCmpType does equality deterministically

-- | Compare types with respect to a (presumably) non-empty 'RnEnv2'.
eqTypeX :: RnEnv2 -> Type -> Type -> Bool
eqTypeX env t1 t2 = isEqual $ nonDetCmpTypeX env t1 t2
  -- It's OK to use nonDetCmpType here and eqTypeX is deterministic,
  -- nonDetCmpTypeX does equality deterministically

-- | Type equality on lists of types, looking through type synonyms
-- but not newtypes.
eqTypes :: [Type] -> [Type] -> Bool
eqTypes tys1 tys2 = isEqual $ nonDetCmpTypes tys1 tys2
  -- It's OK to use nonDetCmpType here and eqTypes is deterministic,
  -- nonDetCmpTypes does equality deterministically

eqVarBndrs :: RnEnv2 -> [Var] -> [Var] -> Maybe RnEnv2
-- Check that the var lists are the same length
-- and have matching kinds; if so, extend the RnEnv2
-- Returns Nothing if they don't match
eqVarBndrs env [] []
 = Just env
eqVarBndrs env (tv1:tvs1) (tv2:tvs2)
 | eqTypeX env (varType tv1) (varType tv2)
 = eqVarBndrs (rnBndr2 env tv1 tv2) tvs1 tvs2
eqVarBndrs _ _ _= Nothing

-- Now here comes the real worker

{-
Note [nonDetCmpType nondeterminism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nonDetCmpType is implemented in terms of nonDetCmpTypeX. nonDetCmpTypeX
uses nonDetCmpTc which compares TyCons by their Unique value. Using Uniques for
ordering leads to nondeterminism. We hit the same problem in the TyVarTy case,
comparing type variables is nondeterministic, note the call to nonDetCmpVar in
nonDetCmpTypeX.
See Note [Unique Determinism] for more details.
-}

nonDetCmpType :: Type -> Type -> Ordering
nonDetCmpType t1 t2
  -- we know k1 and k2 have the same kind, because they both have kind *.
  = nonDetCmpTypeX rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfTypes [t1, t2]))

nonDetCmpTypes :: [Type] -> [Type] -> Ordering
nonDetCmpTypes ts1 ts2 = nonDetCmpTypesX rn_env ts1 ts2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfTypes (ts1 ++ ts2)))

-- | An ordering relation between two 'Type's (known below as @t1 :: k1@
-- and @t2 :: k2@)
data TypeOrdering = TLT  -- ^ @t1 < t2@
                  | TEQ  -- ^ @t1 ~ t2@ and there are no casts in either,
                         -- therefore we can conclude @k1 ~ k2@
                  | TEQX -- ^ @t1 ~ t2@ yet one of the types contains a cast so
                         -- they may differ in kind.
                  | TGT  -- ^ @t1 > t2@
                  deriving (Eq, Ord, Enum, Bounded)

nonDetCmpTypeX :: RnEnv2 -> Type -> Type -> Ordering  -- Main workhorse
    -- See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep
nonDetCmpTypeX env orig_t1 orig_t2 =
    case go env orig_t1 orig_t2 of
      -- If there are casts then we also need to do a comparison of the kinds of
      -- the types being compared
      TEQX          -> toOrdering $ go env k1 k2
      ty_ordering   -> toOrdering ty_ordering
  where
    k1 = typeKind orig_t1
    k2 = typeKind orig_t2

    toOrdering :: TypeOrdering -> Ordering
    toOrdering TLT  = LT
    toOrdering TEQ  = EQ
    toOrdering TEQX = EQ
    toOrdering TGT  = GT

    liftOrdering :: Ordering -> TypeOrdering
    liftOrdering LT = TLT
    liftOrdering EQ = TEQ
    liftOrdering GT = TGT

    thenCmpTy :: TypeOrdering -> TypeOrdering -> TypeOrdering
    thenCmpTy TEQ  rel  = rel
    thenCmpTy TEQX rel  = hasCast rel
    thenCmpTy rel  _    = rel

    hasCast :: TypeOrdering -> TypeOrdering
    hasCast TEQ = TEQX
    hasCast rel = rel

    -- Returns both the resulting ordering relation between the two types
    -- and whether either contains a cast.
    go :: RnEnv2 -> Type -> Type -> TypeOrdering
    go env t1 t2
      | Just t1' <- coreView t1 = go env t1' t2
      | Just t2' <- coreView t2 = go env t1 t2'

    go env (TyVarTy tv1)       (TyVarTy tv2)
      = liftOrdering $ rnOccL env tv1 `nonDetCmpVar` rnOccR env tv2
    go env (ForAllTy (Bndr tv1 _) t1) (ForAllTy (Bndr tv2 _) t2)
      = go env (varType tv1) (varType tv2)
        `thenCmpTy` go (rnBndr2 env tv1 tv2) t1 t2
        -- See Note [Equality on AppTys]
    go env (AppTy s1 t1) ty2
      | Just (s2, t2) <- repSplitAppTy_maybe ty2
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env ty1 (AppTy s2 t2)
      | Just (s1, t1) <- repSplitAppTy_maybe ty1
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env (FunTy _ s1 t1) (FunTy _ s2 t2)
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      = liftOrdering (tc1 `nonDetCmpTc` tc2) `thenCmpTy` gos env tys1 tys2
    go _   (LitTy l1)          (LitTy l2)          = liftOrdering (compare l1 l2)
    go env (CastTy t1 _)       t2                  = hasCast $ go env t1 t2
    go env t1                  (CastTy t2 _)       = hasCast $ go env t1 t2

    go _   (CoercionTy {})     (CoercionTy {})     = TEQ

        -- Deal with the rest: TyVarTy < CoercionTy < AppTy < LitTy < TyConApp < ForAllTy
    go _ ty1 ty2
      = liftOrdering $ (get_rank ty1) `compare` (get_rank ty2)
      where get_rank :: Type -> Int
            get_rank (CastTy {})
              = pprPanic "nonDetCmpTypeX.get_rank" (ppr [ty1,ty2])
            get_rank (TyVarTy {})    = 0
            get_rank (CoercionTy {}) = 1
            get_rank (AppTy {})      = 3
            get_rank (LitTy {})      = 4
            get_rank (TyConApp {})   = 5
            get_rank (FunTy {})      = 6
            get_rank (ForAllTy {})   = 7

    gos :: RnEnv2 -> [Type] -> [Type] -> TypeOrdering
    gos _   []         []         = TEQ
    gos _   []         _          = TLT
    gos _   _          []         = TGT
    gos env (ty1:tys1) (ty2:tys2) = go env ty1 ty2 `thenCmpTy` gos env tys1 tys2

-------------
nonDetCmpTypesX :: RnEnv2 -> [Type] -> [Type] -> Ordering
nonDetCmpTypesX _   []        []        = EQ
nonDetCmpTypesX env (t1:tys1) (t2:tys2) = nonDetCmpTypeX env t1 t2
                                          `thenCmp`
                                          nonDetCmpTypesX env tys1 tys2
nonDetCmpTypesX _   []        _         = LT
nonDetCmpTypesX _   _         []        = GT

-------------
-- | Compare two 'TyCon's. NB: This should /never/ see 'Constraint' (as
-- recognized by Kind.isConstraintKindCon) which is considered a synonym for
-- 'Type' in Core.
-- See Note [Kind Constraint and kind Type] in Kind.
-- See Note [nonDetCmpType nondeterminism]
nonDetCmpTc :: TyCon -> TyCon -> Ordering
nonDetCmpTc tc1 tc2
  = ASSERT( not (isConstraintKindCon tc1) && not (isConstraintKindCon tc2) )
    u1 `nonDetCmpUnique` u2
  where
    u1  = tyConUnique tc1
    u2  = tyConUnique tc2

{-
************************************************************************
*                                                                      *
        The kind of a type
*                                                                      *
************************************************************************

Note [typeKind vs tcTypeKind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have two functions to get the kind of a type

  * typeKind   ignores  the distinction between Constraint and *
  * tcTypeKind respects the distinction between Constraint and *

tcTypeKind is used by the type inference engine, for which Constraint
and * are different; after that we use typeKind.

See also Note [coreView vs tcView]

Note [Kinding rules for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In typeKind we consider Constraint and (TYPE LiftedRep) to be identical.
We then have

         t1 : TYPE rep1
         t2 : TYPE rep2
   (FUN) ----------------
         t1 -> t2 : Type

         ty : TYPE rep
         `a` is not free in rep
(FORALL) -----------------------
         forall a. ty : TYPE rep

In tcTypeKind we consider Constraint and (TYPE LiftedRep) to be distinct:

          t1 : TYPE rep1
          t2 : TYPE rep2
    (FUN) ----------------
          t1 -> t2 : Type

          t1 : Constraint
          t2 : TYPE rep
  (PRED1) ----------------
          t1 => t2 : Type

          t1 : Constraint
          t2 : Constraint
  (PRED2) ---------------------
          t1 => t2 : Constraint

          ty : TYPE rep
          `a` is not free in rep
(FORALL1) -----------------------
          forall a. ty : TYPE rep

          ty : Constraint
(FORALL2) -------------------------
          forall a. ty : Constraint

Note that:
* The only way we distinguish '->' from '=>' is by the fact
  that the argument is a PredTy.  Both are FunTys

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
of 'r'.  But in this case it's fine because (K r) exapands
to Type, so we expliclity /permit/ the type
   forall r. T r

To accommodate such a type, in typeKind (forall a.ty) we use
occCheckExpand to expand any type synonyms in the kind of 'ty'
to eliminate 'a'.  See kinding rule (FORALL) in
Note [Kinding rules for types]

And in TcValidity.checkEscapingKind, we use also use
occCheckExpand, for the same reason.
-}

-----------------------------
typeKind :: HasDebugCallStack => Type -> Kind
-- No need to expand synonyms
typeKind (TyConApp tc tys) = piResultTys (tyConKind tc) tys
typeKind (LitTy l)         = typeLiteralKind l
typeKind (FunTy {})        = liftedTypeKind
typeKind (TyVarTy tyvar)   = tyVarKind tyvar
typeKind (CastTy _ty co)   = coercionRKind co
typeKind (CoercionTy co)   = coercionType co

typeKind (AppTy fun arg)
  = go fun [arg]
  where
    -- Accumulate the type arguments, so we can call piResultTys,
    -- rather than a succession of calls to piResultTy (which is
    -- asymptotically costly as the number of arguments increases)
    go (AppTy fun arg) args = go fun (arg:args)
    go fun             args = piResultTys (typeKind fun) args

typeKind ty@(ForAllTy {})
  = case occCheckExpand tvs body_kind of
      -- We must make sure tv does not occur in kind
      -- As it is already out of scope!
      -- See Note [Phantom type variables in kinds]
      Just k' -> k'
      Nothing -> pprPanic "typeKind"
                  (ppr ty $$ ppr tvs $$ ppr body <+> dcolon <+> ppr body_kind)
  where
    (tvs, body) = splitTyVarForAllTys ty
    body_kind   = typeKind body

---------------------------------------------
-- Utilities to be used in GHC.Core.Unify,
-- which uses "tc" functions
---------------------------------------------

tcTypeKind :: HasDebugCallStack => Type -> Kind
-- No need to expand synonyms
tcTypeKind (TyConApp tc tys) = piResultTys (tyConKind tc) tys
tcTypeKind (LitTy l)         = typeLiteralKind l
tcTypeKind (TyVarTy tyvar)   = tyVarKind tyvar
tcTypeKind (CastTy _ty co)   = coercionRKind co
tcTypeKind (CoercionTy co)   = coercionType co

tcTypeKind (FunTy { ft_af = af, ft_res = res })
  | InvisArg <- af
  , tcIsConstraintKind (tcTypeKind res)
  = constraintKind     -- Eq a => Ord a         :: Constraint
  | otherwise          -- Eq a => a -> a        :: TYPE LiftedRep
  = liftedTypeKind     -- Eq a => Array# Int    :: Type LiftedRep (not TYPE PtrRep)

tcTypeKind (AppTy fun arg)
  = go fun [arg]
  where
    -- Accumulate the type arguments, so we can call piResultTys,
    -- rather than a succession of calls to piResultTy (which is
    -- asymptotically costly as the number of arguments increases)
    go (AppTy fun arg) args = go fun (arg:args)
    go fun             args = piResultTys (tcTypeKind fun) args

tcTypeKind ty@(ForAllTy {})
  | tcIsConstraintKind body_kind
  = constraintKind

  | otherwise
  = case occCheckExpand tvs body_kind of
      -- We must make sure tv does not occur in kind
      -- As it is already out of scope!
      -- See Note [Phantom type variables in kinds]
      Just k' -> k'
      Nothing -> pprPanic "tcTypeKind"
                  (ppr ty $$ ppr tvs $$ ppr body <+> dcolon <+> ppr body_kind)
  where
    (tvs, body) = splitTyVarForAllTys ty
    body_kind = tcTypeKind body


isPredTy :: HasDebugCallStack => Type -> Bool
-- See Note [Types for coercions, predicates, and evidence] in GHC.Core.TyCo.Rep
isPredTy ty = tcIsConstraintKind (tcTypeKind ty)

-- tcIsConstraintKind stuff only makes sense in the typechecker
-- After that Constraint = Type
-- See Note [coreView vs tcView]
-- Defined here because it is used in isPredTy and tcRepSplitAppTy_maybe (sigh)
tcIsConstraintKind :: Kind -> Bool
tcIsConstraintKind ty
  | Just (tc, args) <- tcSplitTyConApp_maybe ty    -- Note: tcSplit here
  , isConstraintKindCon tc
  = ASSERT2( null args, ppr ty ) True

  | otherwise
  = False

-- | Is this kind equivalent to @*@?
--
-- This considers 'Constraint' to be distinct from @*@. For a version that
-- treats them as the same type, see 'isLiftedTypeKind'.
tcIsLiftedTypeKind :: Kind -> Bool
tcIsLiftedTypeKind ty
  | Just (tc, [arg]) <- tcSplitTyConApp_maybe ty    -- Note: tcSplit here
  , tc `hasKey` tYPETyConKey
  = isLiftedRuntimeRep arg
  | otherwise
  = False

-- | Is this kind equivalent to @TYPE r@ (for some unknown r)?
--
-- This considers 'Constraint' to be distinct from @*@.
tcIsRuntimeTypeKind :: Kind -> Bool
tcIsRuntimeTypeKind ty
  | Just (tc, _) <- tcSplitTyConApp_maybe ty    -- Note: tcSplit here
  , tc `hasKey` tYPETyConKey
  = True
  | otherwise
  = False

tcReturnsConstraintKind :: Kind -> Bool
-- True <=> the Kind ultimately returns a Constraint
--   E.g.  * -> Constraint
--         forall k. k -> Constraint
tcReturnsConstraintKind kind
  | Just kind' <- tcView kind = tcReturnsConstraintKind kind'
tcReturnsConstraintKind (ForAllTy _ ty)         = tcReturnsConstraintKind ty
tcReturnsConstraintKind (FunTy { ft_res = ty }) = tcReturnsConstraintKind ty
tcReturnsConstraintKind (TyConApp tc _)         = isConstraintKindCon tc
tcReturnsConstraintKind _                       = False

--------------------------
typeLiteralKind :: TyLit -> Kind
typeLiteralKind (NumTyLit {}) = typeNatKind
typeLiteralKind (StrTyLit {}) = typeSymbolKind

-- | Returns True if a type is levity polymorphic. Should be the same
-- as (isKindLevPoly . typeKind) but much faster.
-- Precondition: The type has kind (TYPE blah)
isTypeLevPoly :: Type -> Bool
isTypeLevPoly = go
  where
    go ty@(TyVarTy {})                           = check_kind ty
    go ty@(AppTy {})                             = check_kind ty
    go ty@(TyConApp tc _) | not (isTcLevPoly tc) = False
                          | otherwise            = check_kind ty
    go (ForAllTy _ ty)                           = go ty
    go (FunTy {})                                = False
    go (LitTy {})                                = False
    go ty@(CastTy {})                            = check_kind ty
    go ty@(CoercionTy {})                        = pprPanic "isTypeLevPoly co" (ppr ty)

    check_kind = isKindLevPoly . typeKind

-- | Looking past all pi-types, is the end result potentially levity polymorphic?
-- Example: True for (forall r (a :: TYPE r). String -> a)
-- Example: False for (forall r1 r2 (a :: TYPE r1) (b :: TYPE r2). a -> b -> Type)
resultIsLevPoly :: Type -> Bool
resultIsLevPoly = isTypeLevPoly . snd . splitPiTys


{- **********************************************************************
*                                                                       *
           Occurs check expansion
%*                                                                      *
%********************************************************************* -}

{- Note [Occurs check expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(occurCheckExpand tv xi) expands synonyms in xi just enough to get rid
of occurrences of tv outside type function arguments, if that is
possible; otherwise, it returns Nothing.

For example, suppose we have
  type F a b = [a]
Then
  occCheckExpand b (F Int b) = Just [Int]
but
  occCheckExpand a (F a Int) = Nothing

We don't promise to do the absolute minimum amount of expanding
necessary, but we try not to do expansions we don't need to.  We
prefer doing inner expansions first.  For example,
  type F a b = (a, Int, a, [a])
  type G b   = Char
We have
  occCheckExpand b (F (G b)) = Just (F Char)
even though we could also expand F to get rid of b.
-}

occCheckExpand :: [Var] -> Type -> Maybe Type
-- See Note [Occurs check expansion]
-- We may have needed to do some type synonym unfolding in order to
-- get rid of the variable (or forall), so we also return the unfolded
-- version of the type, which is guaranteed to be syntactically free
-- of the given type variable.  If the type is already syntactically
-- free of the variable, then the same type is returned.
occCheckExpand vs_to_avoid ty
  | null vs_to_avoid  -- Efficient shortcut
  = Just ty           -- Can happen, eg. GHC.Core.Utils.mkSingleAltCase

  | otherwise
  = go (mkVarSet vs_to_avoid, emptyVarEnv) ty
  where
    go :: (VarSet, VarEnv TyCoVar) -> Type -> Maybe Type
          -- The VarSet is the set of variables we are trying to avoid
          -- The VarEnv carries mappings necessary
          -- because of kind expansion
    go cxt@(as, env) (TyVarTy tv')
      | tv' `elemVarSet` as               = Nothing
      | Just tv'' <- lookupVarEnv env tv' = return (mkTyVarTy tv'')
      | otherwise                         = do { tv'' <- go_var cxt tv'
                                               ; return (mkTyVarTy tv'') }

    go _   ty@(LitTy {}) = return ty
    go cxt (AppTy ty1 ty2) = do { ty1' <- go cxt ty1
                                ; ty2' <- go cxt ty2
                                ; return (mkAppTy ty1' ty2') }
    go cxt ty@(FunTy _ ty1 ty2)
       = do { ty1' <- go cxt ty1
            ; ty2' <- go cxt ty2
            ; return (ty { ft_arg = ty1', ft_res = ty2' }) }
    go cxt@(as, env) (ForAllTy (Bndr tv vis) body_ty)
       = do { ki' <- go cxt (varType tv)
            ; let tv' = setVarType tv ki'
                  env' = extendVarEnv env tv tv'
                  as'  = as `delVarSet` tv
            ; body' <- go (as', env') body_ty
            ; return (ForAllTy (Bndr tv' vis) body') }

    -- For a type constructor application, first try expanding away the
    -- offending variable from the arguments.  If that doesn't work, next
    -- see if the type constructor is a type synonym, and if so, expand
    -- it and try again.
    go cxt ty@(TyConApp tc tys)
      = case mapM (go cxt) tys of
          Just tys' -> return (mkTyConApp tc tys')
          Nothing | Just ty' <- tcView ty -> go cxt ty'
                  | otherwise             -> Nothing
                      -- Failing that, try to expand a synonym

    go cxt (CastTy ty co) =  do { ty' <- go cxt ty
                                ; co' <- go_co cxt co
                                ; return (mkCastTy ty' co') }
    go cxt (CoercionTy co) = do { co' <- go_co cxt co
                                ; return (mkCoercionTy co') }

    ------------------
    go_var cxt v = do { k' <- go cxt (varType v)
                      ; return (setVarType v k') }
           -- Works for TyVar and CoVar
           -- See Note [Occurrence checking: look inside kinds]

    ------------------
    go_mco _   MRefl = return MRefl
    go_mco ctx (MCo co) = MCo <$> go_co ctx co

    ------------------
    go_co cxt (Refl ty)                 = do { ty' <- go cxt ty
                                             ; return (mkNomReflCo ty') }
    go_co cxt (GRefl r ty mco)          = do { mco' <- go_mco cxt mco
                                             ; ty' <- go cxt ty
                                             ; return (mkGReflCo r ty' mco') }
      -- Note: Coercions do not contain type synonyms
    go_co cxt (TyConAppCo r tc args)    = do { args' <- mapM (go_co cxt) args
                                             ; return (mkTyConAppCo r tc args') }
    go_co cxt (AppCo co arg)            = do { co' <- go_co cxt co
                                             ; arg' <- go_co cxt arg
                                             ; return (mkAppCo co' arg') }
    go_co cxt@(as, env) (ForAllCo tv kind_co body_co)
      = do { kind_co' <- go_co cxt kind_co
           ; let tv' = setVarType tv $
                       coercionLKind kind_co'
                 env' = extendVarEnv env tv tv'
                 as'  = as `delVarSet` tv
           ; body' <- go_co (as', env') body_co
           ; return (ForAllCo tv' kind_co' body') }
    go_co cxt (FunCo r co1 co2)         = do { co1' <- go_co cxt co1
                                             ; co2' <- go_co cxt co2
                                             ; return (mkFunCo r co1' co2') }
    go_co cxt@(as,env) (CoVarCo c)
      | c `elemVarSet` as               = Nothing
      | Just c' <- lookupVarEnv env c   = return (mkCoVarCo c')
      | otherwise                       = do { c' <- go_var cxt c
                                             ; return (mkCoVarCo c') }
    go_co cxt (HoleCo h)                = do { c' <- go_var cxt (ch_co_var h)
                                             ; return (HoleCo (h { ch_co_var = c' })) }
    go_co cxt (AxiomInstCo ax ind args) = do { args' <- mapM (go_co cxt) args
                                             ; return (mkAxiomInstCo ax ind args') }
    go_co cxt (UnivCo p r ty1 ty2)      = do { p' <- go_prov cxt p
                                             ; ty1' <- go cxt ty1
                                             ; ty2' <- go cxt ty2
                                             ; return (mkUnivCo p' r ty1' ty2') }
    go_co cxt (SymCo co)                = do { co' <- go_co cxt co
                                             ; return (mkSymCo co') }
    go_co cxt (TransCo co1 co2)         = do { co1' <- go_co cxt co1
                                             ; co2' <- go_co cxt co2
                                             ; return (mkTransCo co1' co2') }
    go_co cxt (NthCo r n co)            = do { co' <- go_co cxt co
                                             ; return (mkNthCo r n co') }
    go_co cxt (LRCo lr co)              = do { co' <- go_co cxt co
                                             ; return (mkLRCo lr co') }
    go_co cxt (InstCo co arg)           = do { co' <- go_co cxt co
                                             ; arg' <- go_co cxt arg
                                             ; return (mkInstCo co' arg') }
    go_co cxt (KindCo co)               = do { co' <- go_co cxt co
                                             ; return (mkKindCo co') }
    go_co cxt (SubCo co)                = do { co' <- go_co cxt co
                                             ; return (mkSubCo co') }
    go_co cxt (AxiomRuleCo ax cs)       = do { cs' <- mapM (go_co cxt) cs
                                             ; return (mkAxiomRuleCo ax cs') }

    ------------------
    go_prov cxt (PhantomProv co)    = PhantomProv <$> go_co cxt co
    go_prov cxt (ProofIrrelProv co) = ProofIrrelProv <$> go_co cxt co
    go_prov _   p@(PluginProv _)    = return p


{-
%************************************************************************
%*                                                                      *
        Miscellaneous functions
%*                                                                      *
%************************************************************************

-}
-- | All type constructors occurring in the type; looking through type
--   synonyms, but not newtypes.
--  When it finds a Class, it returns the class TyCon.
tyConsOfType :: Type -> UniqSet TyCon
tyConsOfType ty
  = go ty
  where
     go :: Type -> UniqSet TyCon  -- The UniqSet does duplicate elim
     go ty | Just ty' <- coreView ty = go ty'
     go (TyVarTy {})                = emptyUniqSet
     go (LitTy {})                  = emptyUniqSet
     go (TyConApp tc tys)           = go_tc tc `unionUniqSets` go_s tys
     go (AppTy a b)                 = go a `unionUniqSets` go b
     go (FunTy _ a b)               = go a `unionUniqSets` go b `unionUniqSets` go_tc funTyCon
     go (ForAllTy (Bndr tv _) ty)   = go ty `unionUniqSets` go (varType tv)
     go (CastTy ty co)              = go ty `unionUniqSets` go_co co
     go (CoercionTy co)             = go_co co

     go_co (Refl ty)               = go ty
     go_co (GRefl _ ty mco)        = go ty `unionUniqSets` go_mco mco
     go_co (TyConAppCo _ tc args)  = go_tc tc `unionUniqSets` go_cos args
     go_co (AppCo co arg)          = go_co co `unionUniqSets` go_co arg
     go_co (ForAllCo _ kind_co co) = go_co kind_co `unionUniqSets` go_co co
     go_co (FunCo _ co1 co2)       = go_co co1 `unionUniqSets` go_co co2
     go_co (AxiomInstCo ax _ args) = go_ax ax `unionUniqSets` go_cos args
     go_co (UnivCo p _ t1 t2)      = go_prov p `unionUniqSets` go t1 `unionUniqSets` go t2
     go_co (CoVarCo {})            = emptyUniqSet
     go_co (HoleCo {})             = emptyUniqSet
     go_co (SymCo co)              = go_co co
     go_co (TransCo co1 co2)       = go_co co1 `unionUniqSets` go_co co2
     go_co (NthCo _ _ co)          = go_co co
     go_co (LRCo _ co)             = go_co co
     go_co (InstCo co arg)         = go_co co `unionUniqSets` go_co arg
     go_co (KindCo co)             = go_co co
     go_co (SubCo co)              = go_co co
     go_co (AxiomRuleCo _ cs)      = go_cos cs

     go_mco MRefl    = emptyUniqSet
     go_mco (MCo co) = go_co co

     go_prov (PhantomProv co)    = go_co co
     go_prov (ProofIrrelProv co) = go_co co
     go_prov (PluginProv _)      = emptyUniqSet
        -- this last case can happen from the tyConsOfType used from
        -- checkTauTvUpdate

     go_s tys     = foldr (unionUniqSets . go)     emptyUniqSet tys
     go_cos cos   = foldr (unionUniqSets . go_co)  emptyUniqSet cos

     go_tc tc = unitUniqSet tc
     go_ax ax = go_tc $ coAxiomTyCon ax

-- | Find the result 'Kind' of a type synonym,
-- after applying it to its 'arity' number of type variables
-- Actually this function works fine on data types too,
-- but they'd always return '*', so we never need to ask
synTyConResKind :: TyCon -> Kind
synTyConResKind tycon = piResultTys (tyConKind tycon) (mkTyVarTys (tyConTyVars tycon))

-- | Retrieve the free variables in this type, splitting them based
-- on whether they are used visibly or invisibly. Invisible ones come
-- first.
splitVisVarsOfType :: Type -> Pair TyCoVarSet
splitVisVarsOfType orig_ty = Pair invis_vars vis_vars
  where
    Pair invis_vars1 vis_vars = go orig_ty
    invis_vars = invis_vars1 `minusVarSet` vis_vars

    go (TyVarTy tv)      = Pair (tyCoVarsOfType $ tyVarKind tv) (unitVarSet tv)
    go (AppTy t1 t2)     = go t1 `mappend` go t2
    go (TyConApp tc tys) = go_tc tc tys
    go (FunTy _ t1 t2)   = go t1 `mappend` go t2
    go (ForAllTy (Bndr tv _) ty)
      = ((`delVarSet` tv) <$> go ty) `mappend`
        (invisible (tyCoVarsOfType $ varType tv))
    go (LitTy {}) = mempty
    go (CastTy ty co) = go ty `mappend` invisible (tyCoVarsOfCo co)
    go (CoercionTy co) = invisible $ tyCoVarsOfCo co

    invisible vs = Pair vs emptyVarSet

    go_tc tc tys = let (invis, vis) = partitionInvisibleTypes tc tys in
                   invisible (tyCoVarsOfTypes invis) `mappend` foldMap go vis

splitVisVarsOfTypes :: [Type] -> Pair TyCoVarSet
splitVisVarsOfTypes = foldMap splitVisVarsOfType

modifyJoinResTy :: Int            -- Number of binders to skip
                -> (Type -> Type) -- Function to apply to result type
                -> Type           -- Type of join point
                -> Type           -- New type
-- INVARIANT: If any of the first n binders are foralls, those tyvars cannot
-- appear in the original result type. See isValidJoinPointType.
modifyJoinResTy orig_ar f orig_ty
  = go orig_ar orig_ty
  where
    go 0 ty = f ty
    go n ty | Just (arg_bndr, res_ty) <- splitPiTy_maybe ty
            = mkPiTy arg_bndr (go (n-1) res_ty)
            | otherwise
            = pprPanic "modifyJoinResTy" (ppr orig_ar <+> ppr orig_ty)

setJoinResTy :: Int  -- Number of binders to skip
             -> Type -- New result type
             -> Type -- Type of join point
             -> Type -- New type
-- INVARIANT: Same as for modifyJoinResTy
setJoinResTy ar new_res_ty ty
  = modifyJoinResTy ar (const new_res_ty) ty

{-
************************************************************************
*                                                                      *
        Functions over Kinds
*                                                                      *
************************************************************************

Note [Kind Constraint and kind Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The kind Constraint is the kind of classes and other type constraints.
The special thing about types of kind Constraint is that
 * They are displayed with double arrow:
     f :: Ord a => a -> a
 * They are implicitly instantiated at call sites; so the type inference
   engine inserts an extra argument of type (Ord a) at every call site
   to f.

However, once type inference is over, there is *no* distinction between
Constraint and Type. Indeed we can have coercions between the two. Consider
   class C a where
     op :: a -> a
For this single-method class we may generate a newtype, which in turn
generates an axiom witnessing
    C a ~ (a -> a)
so on the left we have Constraint, and on the right we have Type.
See #7451.

Bottom line: although 'Type' and 'Constraint' are distinct TyCons, with
distinct uniques, they are treated as equal at all times except
during type inference.
-}

isConstraintKindCon :: TyCon -> Bool
isConstraintKindCon tc = tyConUnique tc == constraintKindTyConKey

-- | Tests whether the given kind (which should look like @TYPE x@)
-- is something other than a constructor tree (that is, constructors at every node).
-- E.g.  True of   TYPE k, TYPE (F Int)
--       False of  TYPE 'LiftedRep
isKindLevPoly :: Kind -> Bool
isKindLevPoly k = ASSERT2( isLiftedTypeKind k || _is_type, ppr k )
                    -- the isLiftedTypeKind check is necessary b/c of Constraint
                  go k
  where
    go ty | Just ty' <- coreView ty = go ty'
    go TyVarTy{}         = True
    go AppTy{}           = True  -- it can't be a TyConApp
    go (TyConApp tc tys) = isFamilyTyCon tc || any go tys
    go ForAllTy{}        = True
    go (FunTy _ t1 t2)   = go t1 || go t2
    go LitTy{}           = False
    go CastTy{}          = True
    go CoercionTy{}      = True

    _is_type = classifiesTypeWithValues k

-----------------------------------------
--              Subkinding
-- The tc variants are used during type-checking, where ConstraintKind
-- is distinct from all other kinds
-- After type-checking (in core), Constraint and liftedTypeKind are
-- indistinguishable

-- | Does this classify a type allowed to have values? Responds True to things
-- like *, #, TYPE Lifted, TYPE v, Constraint.
classifiesTypeWithValues :: Kind -> Bool
-- ^ True of any sub-kind of OpenTypeKind
classifiesTypeWithValues k = isJust (kindRep_maybe k)

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
           --   applications, then you want True here.
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
        dropped_vars = fvVarSet $
                       mapUnionFV injective_vars_of_binder dropped_binders

    in not (subVarSet result_vars dropped_vars)
  where
    tc_binders  = tyConBinders tc
    tc_res_kind = tyConResKind tc

    -- Returns the variables that would be fixed by knowing a TyConBinder. See
    -- Note [When does a tycon application need an explicit kind signature?]
    -- for a more detailed explanation of what this function does.
    injective_vars_of_binder :: TyConBinder -> FV
    injective_vars_of_binder (Bndr tv vis) =
      case vis of
        AnonTCB VisArg -> injectiveVarsOfType False -- conservative choice
                                              (varType tv)
        NamedTCB argf  | source_of_injectivity argf
                       -> unitFV tv `unionFV`
                          injectiveVarsOfType False (varType tv)
        _              -> emptyFV

    source_of_injectivity Required  = True
    source_of_injectivity Specified = spec_inj_pos
    source_of_injectivity Inferred  = False

{-
Note [When does a tycon application need an explicit kind signature?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a couple of places in GHC where we convert Core Types into forms that
more closely resemble user-written syntax. These include:

1. Template Haskell Type reification (see, for instance, TcSplice.reify_tc_app)
2. Converting Types to LHsTypes (in GHC.Hs.Utils.typeToLHsType, or in Haddock)

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
specified arguments (e.g., TH reification in TcSplice), and sometimes the
omitted arguments are only the inferred ones (e.g., in GHC.Hs.Utils.typeToLHsType,
which reifies specified arguments through visible kind application).
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

(This is still a bit hand-wavey, but we'll refine this question incrementally
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
    (e.g., GHC.Hs.Utils.typeToLHsType) and others where it is not (e.g., TH
    reification), so the `injective_vars_of_binder` function is parametrized by
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
