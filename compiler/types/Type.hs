-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998
--
-- Type - public interface

{-# LANGUAGE CPP, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Main functions for manipulating types and type-related things
module Type (
        -- Note some of this is just re-exports from TyCon..

        -- * Main data types representing Types
        -- $type_classification

        -- $representation_types
        TyThing(..), Type, ArgFlag(..), KindOrType, PredType, ThetaType,
        Var, TyVar, isTyVar, TyCoVar, TyBinder, TyVarBinder,

        -- ** Constructing and deconstructing types
        mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, repGetTyVar_maybe,
        getCastedTyVar_maybe, tyVarKind,

        mkAppTy, mkAppTys, splitAppTy, splitAppTys, repSplitAppTys,
        splitAppTy_maybe, repSplitAppTy_maybe, tcRepSplitAppTy_maybe,

        mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe,
        splitFunTys, funResultTy, funArgTy,

        mkTyConApp, mkTyConTy,
        tyConAppTyCon_maybe, tyConAppTyConPicky_maybe,
        tyConAppArgs_maybe, tyConAppTyCon, tyConAppArgs,
        splitTyConApp_maybe, splitTyConApp, tyConAppArgN, nextRole,
        tcRepSplitTyConApp_maybe, tcSplitTyConApp_maybe,
        splitListTyConApp_maybe,
        repSplitTyConApp_maybe,

        mkForAllTy, mkForAllTys, mkInvForAllTys, mkSpecForAllTys,
        mkVisForAllTys, mkInvForAllTy,
        splitForAllTys, splitForAllTyVarBndrs,
        splitForAllTy_maybe, splitForAllTy,
        splitPiTy_maybe, splitPiTy, splitPiTys,
        mkPiTy, mkPiTys, mkTyConBindersPreferAnon,
        mkLamType, mkLamTypes,
        piResultTy, piResultTys,
        applyTysX, dropForAlls,

        mkNumLitTy, isNumLitTy,
        mkStrLitTy, isStrLitTy,

        getRuntimeRep_maybe, getRuntimeRepFromKind_maybe,

        mkCastTy, mkCoercionTy, splitCastTy_maybe,

        userTypeError_maybe, pprUserTypeErrorTy,

        coAxNthLHS,
        stripCoercionTy, splitCoercionType_maybe,

        splitPiTysInvisible, filterOutInvisibleTypes,
        filterOutInvisibleTyVars, partitionInvisibles,
        synTyConResKind,

        modifyJoinResTy, setJoinResTy,

        -- Analyzing types
        TyCoMapper(..), mapType, mapCoercion,

        -- (Newtypes)
        newTyConInstRhs,

        -- Pred types
        mkFamilyTyConApp,
        isDictLikeTy,
        mkPrimEqPred, mkReprPrimEqPred, mkPrimEqPredRole,
        equalityTyCon,
        mkHeteroPrimEqPred, mkHeteroReprPrimEqPred,
        mkClassPred,
        isClassPred, isEqPred, isNomEqPred,
        isIPPred, isIPPred_maybe, isIPTyCon, isIPClass,
        isCTupleClass,

        -- Deconstructing predicate types
        PredTree(..), EqRel(..), eqRelRole, classifyPredType,
        getClassPredTys, getClassPredTys_maybe,
        getEqPredTys, getEqPredTys_maybe, getEqPredRole,
        predTypeEqRel,

        -- ** Binders
        sameVis,
        mkTyVarBinder, mkTyVarBinders,
        mkAnonBinder,
        isAnonTyBinder, isNamedTyBinder,
        binderVar, binderVars, binderKind, binderArgFlag,
        tyBinderType,
        binderRelevantType_maybe, caseBinder,
        isVisibleArgFlag, isInvisibleArgFlag, isVisibleBinder, isInvisibleBinder,
        tyConBindersTyBinders,
        mkTyBinderTyConBinder,

        -- ** Common type constructors
        funTyCon,

        -- ** Predicates on types
        isTyVarTy, isFunTy, isDictTy, isPredTy, isCoercionTy,
        isCoercionTy_maybe, isCoercionType, isForAllTy,
        isPiTy, isTauTy, isFamFreeTy,

        isValidJoinPointType,

        -- (Lifting and boxity)
        isLiftedType_maybe, isUnliftedType, isUnboxedTupleType, isUnboxedSumType,
        isAlgType, isClosedAlgType, isDataFamilyAppType,
        isPrimitiveType, isStrictType,
        isRuntimeRepTy, isRuntimeRepVar, isRuntimeRepKindedTy,
        dropRuntimeRepArgs,
        getRuntimeRep, getRuntimeRepFromKind,

        -- * Main data types representing Kinds
        Kind,

        -- ** Finding the kind of a type
        typeKind, isTypeLevPoly, resultIsLevPoly,

        -- ** Common Kind
        liftedTypeKind,

        -- * Type free variables
        tyCoFVsOfType, tyCoFVsBndr,
        tyCoVarsOfType, tyCoVarsOfTypes,
        tyCoVarsOfTypeDSet,
        coVarsOfType,
        coVarsOfTypes, closeOverKinds, closeOverKindsList,
        noFreeVarsOfType,
        splitVisVarsOfType, splitVisVarsOfTypes,
        expandTypeSynonyms,
        typeSize,

        -- * Well-scoped lists of variables
        dVarSetElemsWellScoped, toposortTyVars, tyCoVarsOfTypeWellScoped,
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
        notElemTCvSubst,
        getTvSubstEnv, setTvSubstEnv,
        zapTCvSubst, getTCvInScope, getTCvSubstRangeFVs,
        extendTCvInScope, extendTCvInScopeList, extendTCvInScopeSet,
        extendTCvSubst, extendCvSubst,
        extendTvSubst, extendTvSubstBinder,
        extendTvSubstList, extendTvSubstAndInScope,
        extendTvSubstWithClone,
        isInScope, composeTCvSubstEnv, composeTCvSubst, zipTyEnv, zipCoEnv,
        isEmptyTCvSubst, unionTCvSubst,

        -- ** Performing substitution on types and kinds
        substTy, substTys, substTyWith, substTysWith, substTheta,
        substTyAddInScope,
        substTyUnchecked, substTysUnchecked, substThetaUnchecked,
        substTyWithUnchecked,
        substCoUnchecked, substCoWithUnchecked,
        substTyVarBndr, substTyVar, substTyVars,
        cloneTyVarBndr, cloneTyVarBndrs, lookupTyVar,

        -- * Pretty-printing
        pprType, pprParendType, pprPrecType,
        pprTypeApp, pprTyThingCategory, pprShortTyThing,
        pprTvBndr, pprTvBndrs, pprForAll, pprUserForAll,
        pprSigmaType, ppSuggestExplicitKinds,
        pprTheta, pprThetaArrowTy, pprClassPred,
        pprKind, pprParendKind, pprSourceTyCon,
        TyPrec(..), maybeParen,
        pprTyVar, pprTyVars, pprPrefixApp, pprArrowChain,

        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyOpenKind,
        tidyTyCoVarBndr, tidyTyCoVarBndrs, tidyFreeTyCoVars,
        tidyOpenTyCoVar, tidyOpenTyCoVars,
        tidyTyVarOcc,
        tidyTopType,
        tidyKind,
        tidyTyVarBinder, tidyTyVarBinders
    ) where

#include "HsVersions.h"

import BasicTypes

-- We import the representation and primitive functions from TyCoRep.
-- Many things are reexported, but not the representation!

import Kind
import TyCoRep

-- friends:
import Var
import VarEnv
import VarSet
import UniqSet

import Class
import TyCon
import TysPrim
import {-# SOURCE #-} TysWiredIn ( listTyCon, typeNatKind
                                 , typeSymbolKind, liftedTypeKind )
import PrelNames
import CoAxiom
import {-# SOURCE #-} Coercion

-- others
import Util
import Outputable
import FastString
import Pair
import ListSetOps
import Digraph
import Unique ( nonDetCmpUnique )
import SrcLoc  ( SrcSpan )
import OccName ( OccName )
import Name    ( mkInternalName )

import Maybes           ( orElse )
import Data.Maybe       ( isJust, mapMaybe )
import Control.Monad    ( guard )
import Control.Arrow    ( first, second )

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
-- if they are spotted, to the best of it's abilities. If you don't want this
-- to happen, use the equivalent functions from the "TcType" module.

{-
************************************************************************
*                                                                      *
                Type representation
*                                                                      *
************************************************************************

Note [coreView vs tcView]
~~~~~~~~~~~~~~~~~~~~~~~~~
So far as the typechecker is concerned, 'Constraint' and 'TYPE LiftedRep' are distinct kinds.

But in Core these two are treated as identical.

We implement this by making 'coreView' convert 'Constraint' to 'TYPE LiftedRep' on the fly.
The function tcView (used in the type checker) does not do this.

See also Trac #11715, which tracks removing this inconsistency.

-}

{-# INLINE coreView #-}
coreView :: Type -> Maybe Type
-- ^ This function Strips off the /top layer only/ of a type synonym
-- application (if any) its underlying representation type.
-- Returns Nothing if there is nothing to look through.
-- This function considers 'Constraint' to be a synonym of @TYPE LiftedRep@.
--
-- By being non-recursive and inlined, this case analysis gets efficiently
-- joined onto the case analysis that the caller is already doing
coreView (TyConApp tc tys) | Just (tenv, rhs, tys') <- expandSynTyCon_maybe tc tys
  = Just (mkAppTys (substTy (mkTvSubstPrs tenv) rhs) tys')
               -- The free vars of 'rhs' should all be bound by 'tenv', so it's
               -- ok to use 'substTy' here.
               -- See also Note [The substitution invariant] in TyCoRep.
               -- Its important to use mkAppTys, rather than (foldl AppTy),
               -- because the function part might well return a
               -- partially-applied type constructor; indeed, usually will!
coreView (TyConApp tc [])
  | isStarKindSynonymTyCon tc
  = Just liftedTypeKind

coreView _ = Nothing

-- | Gives the typechecker view of a type. This unwraps synonyms but
-- leaves 'Constraint' alone. c.f. coreView, which turns Constraint into
-- TYPE LiftedRep. Returns Nothing if no unwrapping happens.
-- See also Note [coreView vs tcView] in Type.
{-# INLINE tcView #-}
tcView :: Type -> Maybe Type
tcView (TyConApp tc tys) | Just (tenv, rhs, tys') <- expandSynTyCon_maybe tc tys
  = Just (mkAppTys (substTy (mkTvSubstPrs tenv) rhs) tys')
               -- The free vars of 'rhs' should all be bound by 'tenv', so it's
               -- ok to use 'substTy' here.
               -- See also Note [The substitution invariant] in TyCoRep.
               -- Its important to use mkAppTys, rather than (foldl AppTy),
               -- because the function part might well return a
               -- partially-applied type constructor; indeed, usually will!
tcView _ = Nothing

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
            -- (Trac #11665)
        in  mkAppTys (go subst' rhs) tys'
      | otherwise
      = TyConApp tc expanded_tys
      where
        expanded_tys = (map (go subst) tys)

    go _     (LitTy l)     = LitTy l
    go subst (TyVarTy tv)  = substTyVar subst tv
    go subst (AppTy t1 t2) = mkAppTy (go subst t1) (go subst t2)
    go subst (FunTy arg res)
      = mkFunTy (go subst arg) (go subst res)
    go subst (ForAllTy (TvBndr tv vis) t)
      = let (subst', tv') = substTyVarBndrCallback go subst tv in
        ForAllTy (TvBndr tv' vis) (go subst' t)
    go subst (CastTy ty co)  = mkCastTy (go subst ty) (go_co subst co)
    go subst (CoercionTy co) = mkCoercionTy (go_co subst co)

    go_co subst (Refl r ty)
      = mkReflCo r (go subst ty)
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
    go_co subst (NthCo n co)
      = mkNthCo n (go_co subst co)
    go_co subst (LRCo lr co)
      = mkLRCo lr (go_co subst co)
    go_co subst (InstCo co arg)
      = mkInstCo (go_co subst co) (go_co subst arg)
    go_co subst (CoherenceCo co1 co2)
      = mkCoherenceCo (go_co subst co1) (go_co subst co2)
    go_co subst (KindCo co)
      = mkKindCo (go_co subst co)
    go_co subst (SubCo co)
      = mkSubCo (go_co subst co)
    go_co subst (AxiomRuleCo ax cs) = AxiomRuleCo ax (map (go_co subst) cs)

    go_prov _     UnsafeCoerceProv    = UnsafeCoerceProv
    go_prov subst (PhantomProv co)    = PhantomProv (go_co subst co)
    go_prov subst (ProofIrrelProv co) = ProofIrrelProv (go_co subst co)
    go_prov _     p@(PluginProv _)    = p
    go_prov _     (HoleProv h)        = pprPanic "expandTypeSynonyms hit a hole" (ppr h)

      -- the "False" and "const" are to accommodate the type of
      -- substForAllCoBndrCallback, which is general enough to
      -- handle coercion optimization (which sometimes swaps the
      -- order of a coercion)
    go_cobndr subst = substForAllCoBndrCallback False (go_co subst) subst

{-
************************************************************************
*                                                                      *
   Analyzing types
*                                                                      *
************************************************************************

These functions do a map-like operation over types, performing some operation
on all variables and binding sites. Primarily used for zonking.

Note [Efficiency for mapCoercion ForAllCo case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As noted in Note [Forall coercions] in TyCoRep, a ForAllCo is a bit redundant.
It stores a TyVar and a Coercion, where the kind of the TyVar always matches
the left-hand kind of the coercion. This is convenient lots of the time, but
not when mapping a function over a coercion.

The problem is that tcm_tybinder will affect the TyVar's kind and
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
These INLINABLE pragmas are indispensable. mapType/mapCoercion are used
to implement zonking, and it's vital that they get specialised to the TcM
monad. This specialisation happens automatically (that is, without a
SPECIALISE pragma) as long as the definitions are INLINABLE. For example,
this one change made a 20% allocation difference in perf/compiler/T5030.

-}

-- | This describes how a "map" operation over a type/coercion should behave
data TyCoMapper env m
  = TyCoMapper
      { tcm_smart :: Bool -- ^ Should the new type be created with smart
                         -- constructors?
      , tcm_tyvar :: env -> TyVar -> m Type
      , tcm_covar :: env -> CoVar -> m Coercion
      , tcm_hole  :: env -> CoercionHole -> Role
                  -> Type -> Type -> m Coercion
          -- ^ What to do with coercion holes. See Note [Coercion holes] in
          -- TyCoRep.

      , tcm_tybinder :: env -> TyVar -> ArgFlag -> m (env, TyVar)
          -- ^ The returned env is used in the extended scope
      }

{-# INLINABLE mapType #-}  -- See Note [Specialising mappers]
mapType :: Monad m => TyCoMapper env m -> env -> Type -> m Type
mapType mapper@(TyCoMapper { tcm_smart = smart, tcm_tyvar = tyvar
                           , tcm_tybinder = tybinder })
        env ty
  = go ty
  where
    go (TyVarTy tv) = tyvar env tv
    go (AppTy t1 t2) = mkappty <$> go t1 <*> go t2
    go t@(TyConApp _ []) = return t  -- avoid allocation in this exceedingly
                                     -- common case (mostly, for *)
    go (TyConApp tc tys) = mktyconapp tc <$> mapM go tys
    go (FunTy arg res)   = FunTy <$> go arg <*> go res
    go (ForAllTy (TvBndr tv vis) inner)
      = do { (env', tv') <- tybinder env tv vis
           ; inner' <- mapType mapper env' inner
           ; return $ ForAllTy (TvBndr tv' vis) inner' }
    go ty@(LitTy {})   = return ty
    go (CastTy ty co)  = mkcastty <$> go ty <*> mapCoercion mapper env co
    go (CoercionTy co) = CoercionTy <$> mapCoercion mapper env co

    (mktyconapp, mkappty, mkcastty)
      | smart     = (mkTyConApp, mkAppTy, mkCastTy)
      | otherwise = (TyConApp,   AppTy,   CastTy)

{-# INLINABLE mapCoercion #-}  -- See Note [Specialising mappers]
mapCoercion :: Monad m
            => TyCoMapper env m -> env -> Coercion -> m Coercion
mapCoercion mapper@(TyCoMapper { tcm_smart = smart, tcm_covar = covar
                               , tcm_hole = cohole, tcm_tybinder = tybinder })
            env co
  = go co
  where
    go (Refl r ty) = Refl r <$> mapType mapper env ty
    go (TyConAppCo r tc args)
      = mktyconappco r tc <$> mapM go args
    go (AppCo c1 c2) = mkappco <$> go c1 <*> go c2
    go (ForAllCo tv kind_co co)
      = do { kind_co' <- go kind_co
           ; (env', tv') <- tybinder env tv Inferred
           ; co' <- mapCoercion mapper env' co
           ; return $ mkforallco tv' kind_co' co' }
        -- See Note [Efficiency for mapCoercion ForAllCo case]
    go (FunCo r c1 c2) = mkFunCo r <$> go c1 <*> go c2
    go (CoVarCo cv) = covar env cv
    go (AxiomInstCo ax i args)
      = mkaxiominstco ax i <$> mapM go args
    go (UnivCo (HoleProv hole) r t1 t2)
      = cohole env hole r t1 t2
    go (UnivCo p r t1 t2)
      = mkunivco <$> go_prov p <*> pure r
                 <*> mapType mapper env t1 <*> mapType mapper env t2
    go (SymCo co) = mksymco <$> go co
    go (TransCo c1 c2) = mktransco <$> go c1 <*> go c2
    go (AxiomRuleCo r cos) = AxiomRuleCo r <$> mapM go cos
    go (NthCo i co)        = mknthco i <$> go co
    go (LRCo lr co)        = mklrco lr <$> go co
    go (InstCo co arg)     = mkinstco <$> go co <*> go arg
    go (CoherenceCo c1 c2) = mkcoherenceco <$> go c1 <*> go c2
    go (KindCo co)         = mkkindco <$> go co
    go (SubCo co)          = mksubco <$> go co

    go_prov UnsafeCoerceProv    = return UnsafeCoerceProv
    go_prov (PhantomProv co)    = PhantomProv <$> go co
    go_prov (ProofIrrelProv co) = ProofIrrelProv <$> go co
    go_prov p@(PluginProv _)    = return p
    go_prov (HoleProv _)        = panic "mapCoercion"

    ( mktyconappco, mkappco, mkaxiominstco, mkunivco
      , mksymco, mktransco, mknthco, mklrco, mkinstco, mkcoherenceco
      , mkkindco, mksubco, mkforallco)
      | smart
      = ( mkTyConAppCo, mkAppCo, mkAxiomInstCo, mkUnivCo
        , mkSymCo, mkTransCo, mkNthCo, mkLRCo, mkInstCo, mkCoherenceCo
        , mkKindCo, mkSubCo, mkForAllCo )
      | otherwise
      = ( TyConAppCo, AppCo, AxiomInstCo, UnivCo
        , SymCo, TransCo, NthCo, LRCo, InstCo, CoherenceCo
        , KindCo, SubCo, ForAllCo )

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
-- with the coercion. Thus, the co is :: kind tv ~N kind type
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
Here's an example (Trac #9858) of how you might do it:
   i :: (Typeable a, Typeable b) => Proxy (a b) -> TypeRep
   i p = typeRep p

   j = i (Proxy :: Proxy (Eq Int => Int))
The type (Proxy (Eq Int => Int)) is only accepted with -XImpredicativeTypes,
but suppose we want that.  But then in the call to 'i', we end
up decomposing (Eq Int => Int), and we definitely don't want that.

This really only applies to the type checker; in Core, '=>' and '->'
are the same, as are 'Constraint' and '*'.  But for now I've put
the test in repSplitAppTy_maybe, which applies throughout, because
the other calls to splitAppTy are in Unify, which is also used by
the type checker (e.g. when matching type-function equations).

-}

-- | Applies a type to another, as in e.g. @k a@
mkAppTy :: Type -> Type -> Type
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

mkAppTys :: Type -> [Type] -> Type
mkAppTys ty1                []   = ty1
mkAppTys (TyConApp tc tys1) tys2 = mkTyConApp tc (tys1 ++ tys2)
mkAppTys ty1                tys2 = foldl AppTy ty1 tys2

-------------
splitAppTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Attempt to take a type application apart, whether it is a
-- function, type constructor, or plain type application. Note
-- that type family applications are NEVER unsaturated by this!
splitAppTy_maybe ty | Just ty' <- coreView ty
                    = splitAppTy_maybe ty'
splitAppTy_maybe ty = repSplitAppTy_maybe ty

-------------
repSplitAppTy_maybe :: Type -> Maybe (Type,Type)
-- ^ Does the AppTy split as in 'splitAppTy_maybe', but assumes that
-- any Core view stuff is already done
repSplitAppTy_maybe (FunTy ty1 ty2)
  | Just rep1 <- getRuntimeRep_maybe ty1
  , Just rep2 <- getRuntimeRep_maybe ty2
  = Just (TyConApp funTyCon [rep1, rep2, ty1], ty2)

  | otherwise
  = pprPanic "repSplitAppTy_maybe" (ppr ty1 $$ ppr ty2)
repSplitAppTy_maybe (AppTy ty1 ty2)
  = Just (ty1, ty2)
repSplitAppTy_maybe (TyConApp tc tys)
  | mightBeUnsaturatedTyCon tc || tys `lengthExceeds` tyConArity tc
  , Just (tys', ty') <- snocView tys
  = Just (TyConApp tc tys', ty')    -- Never create unsaturated type family apps!
repSplitAppTy_maybe _other = Nothing

-- this one doesn't braek apart (c => t).
-- See Note [Decomposing fat arrow c=>t]
-- Defined here to avoid module loops between Unify and TcType.
tcRepSplitAppTy_maybe :: Type -> Maybe (Type,Type)
-- ^ Does the AppTy split as in 'tcSplitAppTy_maybe', but assumes that
-- any coreView stuff is already done. Refuses to look through (c => t)
tcRepSplitAppTy_maybe (FunTy ty1 ty2)
  | isConstraintKind (typeKind ty1)
  = Nothing  -- See Note [Decomposing fat arrow c=>t]

  | Just rep1 <- getRuntimeRep_maybe ty1
  , Just rep2 <- getRuntimeRep_maybe ty2
  = Just (TyConApp funTyCon [rep1, rep2, ty1], ty2)

  | otherwise
  = pprPanic "repSplitAppTy_maybe" (ppr ty1 $$ ppr ty2)
tcRepSplitAppTy_maybe (AppTy ty1 ty2)    = Just (ty1, ty2)
tcRepSplitAppTy_maybe (TyConApp tc tys)
  | mightBeUnsaturatedTyCon tc || tys `lengthExceeds` tyConArity tc
  , Just (tys', ty') <- snocView tys
  = Just (TyConApp tc tys', ty')    -- Never create unsaturated type family apps!
tcRepSplitAppTy_maybe _other = Nothing

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
tcSplitTyConApp_maybe ty                         = tcRepSplitTyConApp_maybe ty

-- | Like 'tcSplitTyConApp_maybe' but doesn't look through type synonyms.
tcRepSplitTyConApp_maybe :: HasCallStack => Type -> Maybe (TyCon, [Type])
-- Defined here to avoid module loops between Unify and TcType.
tcRepSplitTyConApp_maybe (TyConApp tc tys)          = Just (tc, tys)
tcRepSplitTyConApp_maybe (FunTy arg res)
  | Just arg_rep <- getRuntimeRep_maybe arg
  , Just res_rep <- getRuntimeRep_maybe res
  = Just (funTyCon, [arg_rep, res_rep, arg, res])

  | otherwise
  = pprPanic "tcRepSplitTyConApp_maybe" (ppr arg $$ ppr res)
tcRepSplitTyConApp_maybe _                          = Nothing


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
            n | mightBeUnsaturatedTyCon tc = 0
              | otherwise                  = tyConArity tc
            (tc_args1, tc_args2) = splitAt n tc_args
        in
        (TyConApp tc tc_args1, tc_args2 ++ args)
    split _   (FunTy ty1 ty2) args
      | Just rep1 <- getRuntimeRep_maybe ty1
      , Just rep2 <- getRuntimeRep_maybe ty2
      = ASSERT( null args )
        (TyConApp funTyCon [], [rep1, rep2, ty1, ty2])

      | otherwise
      = pprPanic "splitAppTys" (ppr ty1 $$ ppr ty2 $$ ppr args)
    split orig_ty _                     args  = (orig_ty, args)

-- | Like 'splitAppTys', but doesn't look through type synonyms
repSplitAppTys :: Type -> (Type, [Type])
repSplitAppTys ty = split ty []
  where
    split (AppTy ty arg) args = split ty (arg:args)
    split (TyConApp tc tc_args) args
      = let n | mightBeUnsaturatedTyCon tc = 0
              | otherwise                  = tyConArity tc
            (tc_args1, tc_args2) = splitAt n tc_args
        in
        (TyConApp tc tc_args1, tc_args2 ++ args)
    split (FunTy ty1 ty2) args
      | Just rep1 <- getRuntimeRep_maybe ty1
      , Just rep2 <- getRuntimeRep_maybe ty2
      = ASSERT( null args )
        (TyConApp funTyCon [], [rep1, rep2, ty1, ty2])

      | otherwise
      = pprPanic "repSplitAppTys" (ppr ty1 $$ ppr ty2 $$ ppr args)
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

    -- An uneavaluated type function
    _ -> ppr ty




{-
---------------------------------------------------------------------
                                FunTy
                                ~~~~~

Note [Representation of function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions (e.g. Int -> Char) are can be thought of as being applications
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

isFunTy :: Type -> Bool
isFunTy ty = isJust (splitFunTy_maybe ty)

splitFunTy :: Type -> (Type, Type)
-- ^ Attempts to extract the argument and result types from a type, and
-- panics if that is not possible. See also 'splitFunTy_maybe'
splitFunTy ty | Just ty' <- coreView ty = splitFunTy ty'
splitFunTy (FunTy arg res) = (arg, res)
splitFunTy other           = pprPanic "splitFunTy" (ppr other)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Attempts to extract the argument and result types from a type
splitFunTy_maybe ty | Just ty' <- coreView ty = splitFunTy_maybe ty'
splitFunTy_maybe (FunTy arg res) = Just (arg, res)
splitFunTy_maybe _               = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty ty | Just ty' <- coreView ty = split args orig_ty ty'
    split args _       (FunTy arg res) = split (arg:args) res res
    split args orig_ty _               = (reverse args, orig_ty)

funResultTy :: Type -> Type
-- ^ Extract the function result type and panic if that is not possible
funResultTy ty | Just ty' <- coreView ty = funResultTy ty'
funResultTy (FunTy _ res) = res
funResultTy ty            = pprPanic "funResultTy" (ppr ty)

funArgTy :: Type -> Type
-- ^ Extract the function argument type and panic if that is not possible
funArgTy ty | Just ty' <- coreView ty = funArgTy ty'
funArgTy (FunTy arg _res) = arg
funArgTy ty               = pprPanic "funArgTy" (ppr ty)

piResultTy :: Type -> Type ->  Type
piResultTy ty arg = case piResultTy_maybe ty arg of
                      Just res -> res
                      Nothing  -> pprPanic "piResultTy" (ppr ty $$ ppr arg)

piResultTy_maybe :: Type -> Type -> Maybe Type

-- ^ Just like 'piResultTys' but for a single argument
-- Try not to iterate 'piResultTy', because it's inefficient to substitute
-- one variable at a time; instead use 'piResultTys"
piResultTy_maybe ty arg
  | Just ty' <- coreView ty = piResultTy_maybe ty' arg

  | FunTy _ res <- ty
  = Just res

  | ForAllTy (TvBndr tv _) res <- ty
  = let empty_subst = mkEmptyTCvSubst $ mkInScopeSet $
                      tyCoVarsOfTypes [arg,res]
    in Just (substTy (extendTvSubst empty_subst tv arg) res)

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
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs

-- This is a heavily used function (e.g. from typeKind),
-- so we pay attention to efficiency, especially in the special case
-- where there are no for-alls so we are just dropping arrows from
-- a function type/kind.
piResultTys :: Type -> [Type] -> Type
piResultTys ty [] = ty
piResultTys ty orig_args@(arg:args)
  | Just ty' <- coreView ty
  = piResultTys ty' orig_args

  | FunTy _ res <- ty
  = piResultTys res args

  | ForAllTy (TvBndr tv _) res <- ty
  = go (extendVarEnv emptyTvSubstEnv tv arg) res args

  | otherwise
  = pprPanic "piResultTys1" (ppr ty $$ ppr orig_args)
  where
    in_scope = mkInScopeSet (tyCoVarsOfTypes (ty:orig_args))

    go :: TvSubstEnv -> Type -> [Type] -> Type
    go tv_env ty [] = substTy (mkTvSubst in_scope tv_env) ty

    go tv_env ty all_args@(arg:args)
      | Just ty' <- coreView ty
      = go tv_env ty' all_args

      | FunTy _ res <- ty
      = go tv_env res args

      | ForAllTy (TvBndr tv _) res <- ty
      = go (extendVarEnv tv_env tv arg) res args

      | TyVarTy tv <- ty
      , Just ty' <- lookupVarEnv tv_env tv
        -- Deals with piResultTys (forall a. a) [forall b.b, Int]
      = piResultTys ty' all_args

      | otherwise
      = pprPanic "piResultTys2" (ppr ty $$ ppr orig_args $$ ppr all_args)

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

{-
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
  = FunTy ty1 ty2

  | otherwise
  = TyConApp tycon tys

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
tyConAppArgs_maybe (FunTy arg res)
  | Just rep1 <- getRuntimeRep_maybe arg
  , Just rep2 <- getRuntimeRep_maybe res
  = Just [rep1, rep2, arg, res]
tyConAppArgs_maybe _                = Nothing

tyConAppArgs :: Type -> [Type]
tyConAppArgs ty = tyConAppArgs_maybe ty `orElse` pprPanic "tyConAppArgs" (ppr ty)

tyConAppArgN :: Int -> Type -> Type
-- Executing Nth
tyConAppArgN n ty
  = case tyConAppArgs_maybe ty of
      Just tys -> ASSERT2( tys `lengthExceeds` n, ppr n <+> ppr tys ) tys `getNth` n
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

-- | Like 'splitTyConApp_maybe', but doesn't look through synonyms. This
-- assumes the synonyms have already been dealt with.
repSplitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
repSplitTyConApp_maybe (TyConApp tc tys) = Just (tc, tys)
repSplitTyConApp_maybe (FunTy arg res)
  | Just rep1 <- getRuntimeRep_maybe arg
  , Just rep2 <- getRuntimeRep_maybe res
  = Just (funTyCon, [rep1, rep2, arg, res])
  | otherwise
  = pprPanic "repSplitTyConApp_maybe"
             (ppr arg $$ ppr res $$ ppr (typeKind res))
repSplitTyConApp_maybe _ = Nothing

-- | Attempts to tease a list type apart and gives the type of the elements if
-- successful (looks through type synonyms)
splitListTyConApp_maybe :: Type -> Maybe Type
splitListTyConApp_maybe ty = case splitTyConApp_maybe ty of
  Just (tc,[e]) | tc == listTyCon -> Just e
  _other                          -> Nothing

-- | What is the role assigned to the next parameter of this type? Usually,
-- this will be 'Nominal', but if the type is a 'TyConApp', we may be able to
-- do better. The type does *not* have to be well-kinded when applied for this
-- to work!
nextRole :: Type -> Role
nextRole ty
  | Just (tc, tys) <- splitTyConApp_maybe ty
  , let num_tys = length tys
  , num_tys < tyConArity tc
  = tyConRoles tc `getNth` num_tys

  | otherwise
  = Nominal

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

Note [No reflexive casts in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As far as possible, we would like to maintain the following property:

  (*) If (t1 `eqType` t2), then t1 and t2 are treated identically within GHC.

The (*) property is very useful, because we have a tendency to compare two
types to see if they're equal, and then arbitrarily choose one. We don't
want this arbitrary choice to then matter later on. Maintaining (*) means
that every function that looks at a structure of a type must think about
casts. In places where we directly pattern-match, this consideration is
forced by consideration of the CastTy constructor.

But, when we call a splitXXX function, it's easy to ignore the possibility
of casts. In particular, splitTyConApp is used extensively, and we don't
want it to fail on (T a b c |> co). Happily, if we have
  (T a b c |> co) `eqType` (T d e f)
then co must be reflexive. Why? eqType checks that the kinds are equal, as
well as checking that (a `eqType` d), (b `eqType` e), and (c `eqType` f).
By the kind check, we know that (T a b c |> co) and (T d e f) have the same
kind. So the only way that co could be non-reflexive is for (T a b c) to have
a different kind than (T d e f). But because T's kind is closed (all tycon kinds
are closed), the only way for this to happen is that one of the arguments has
to differ, leading to a contradiction. Thus, co is reflexive.

Accordingly, by eliminating reflexive casts, splitTyConApp need not worry
about outermost casts to uphold (*).

Unforunately, that's not the end of the story. Consider comparing
  (T a b c)      =?       (T a b |> (co -> <Type>)) (c |> sym co)
These two types have the same kind (Type), but the left type is a TyConApp
while the right type is not. To handle this case, we will have to implement
some variant of the dreaded KPush algorithm (c.f. CoreOpt.pushCoDataCon).
This stone is left unturned for now, meaning that we don't yet uphold (*).

The other place where (*) will be hard to guarantee is in splitAppTy, because
I (Richard E) can't think of a way to push coercions into AppTys. The good
news here is that splitAppTy is not used all that much, and so all clients of
that function can simply be told to use splitCastTy as well, in order to
uphold (*). This, too, is left undone, for now.

-}

splitCastTy_maybe :: Type -> Maybe (Type, Coercion)
splitCastTy_maybe ty | Just ty' <- coreView ty = splitCastTy_maybe ty'
splitCastTy_maybe (CastTy ty co)               = Just (ty, co)
splitCastTy_maybe _                            = Nothing

-- | Make a 'CastTy'. The Coercion must be nominal. Checks the
-- Coercion for reflexivity, dropping it if it's reflexive.
-- See Note [No reflexive casts in types]
mkCastTy :: Type -> Coercion -> Type
mkCastTy ty co | isReflexiveCo co = ty
-- NB: Do the slow check here. This is important to keep the splitXXX
-- functions working properly. Otherwise, we may end up with something
-- like (((->) |> something_reflexive_but_not_obviously_so) biz baz)
-- fails under splitFunTy_maybe. This happened with the cheaper check
-- in test dependent/should_compile/dynamic-paper.

mkCastTy (CastTy ty co1) co2 = mkCastTy ty (co1 `mkTransCo` co2)
mkCastTy ty co = CastTy ty co

tyConBindersTyBinders :: [TyConBinder] -> [TyBinder]
-- Return the tyConBinders in TyBinder form
tyConBindersTyBinders = map to_tyb
  where
    to_tyb (TvBndr tv (NamedTCB vis)) = Named (TvBndr tv vis)
    to_tyb (TvBndr tv AnonTCB)        = Anon (tyVarKind tv)

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

-- | Make a dependent forall over an Inferred (as opposed to Specified)
-- variable
mkInvForAllTy :: TyVar -> Type -> Type
mkInvForAllTy tv ty = ASSERT( isTyVar tv )
                      ForAllTy (TvBndr tv Inferred) ty

-- | Like mkForAllTys, but assumes all variables are dependent and Inferred,
-- a common case
mkInvForAllTys :: [TyVar] -> Type -> Type
mkInvForAllTys tvs ty = ASSERT( all isTyVar tvs )
                        foldr mkInvForAllTy ty tvs

-- | Like mkForAllTys, but assumes all variables are dependent and specified,
-- a common case
mkSpecForAllTys :: [TyVar] -> Type -> Type
mkSpecForAllTys tvs = ASSERT( all isTyVar tvs )
                     mkForAllTys [ TvBndr tv Specified | tv <- tvs ]

-- | Like mkForAllTys, but assumes all variables are dependent and visible
mkVisForAllTys :: [TyVar] -> Type -> Type
mkVisForAllTys tvs = ASSERT( all isTyVar tvs )
                     mkForAllTys [ TvBndr tv Required | tv <- tvs ]

mkLamType  :: Var -> Type -> Type
-- ^ Makes a @(->)@ type or an implicit forall type, depending
-- on whether it is given a type variable or a term variable.
-- This is used, for example, when producing the type of a lambda.
-- Always uses Inferred binders.
mkLamTypes :: [Var] -> Type -> Type
-- ^ 'mkLamType' for multiple type or value arguments

mkLamType v ty
   | isTyVar v = ForAllTy (TvBndr v Inferred) ty
   | otherwise = FunTy    (varType v)          ty

mkLamTypes vs ty = foldr mkLamType ty vs

-- | Given a list of type-level vars and a result type, makes TyBinders, preferring
-- anonymous binders if the variable is, in fact, not dependent.
-- All binders are /visible/.
mkTyConBindersPreferAnon :: [TyVar] -> Type -> [TyConBinder]
mkTyConBindersPreferAnon vars inner_ty = fst (go vars)
  where
    go :: [TyVar] -> ([TyConBinder], VarSet) -- also returns the free vars
    go [] = ([], tyCoVarsOfType inner_ty)
    go (v:vs) |  v `elemVarSet` fvs
              = ( TvBndr v (NamedTCB Required) : binders
                , fvs `delVarSet` v `unionVarSet` kind_vars )
              | otherwise
              = ( TvBndr v AnonTCB : binders
                , fvs `unionVarSet` kind_vars )
      where
        (binders, fvs) = go vs
        kind_vars      = tyCoVarsOfType $ tyVarKind v

-- | Take a ForAllTy apart, returning the list of tyvars and the result type.
-- This always succeeds, even if it returns only an empty list. Note that the
-- result type returned may have free variables that were bound by a forall.
splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split ty ty []
  where
    split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
    split _       (ForAllTy (TvBndr tv _) ty) tvs = split ty ty (tv:tvs)
    split orig_ty _                           tvs = (reverse tvs, orig_ty)

-- | Like 'splitPiTys' but split off only /named/ binders.
splitForAllTyVarBndrs :: Type -> ([TyVarBinder], Type)
splitForAllTyVarBndrs ty = split ty ty []
  where
    split orig_ty ty bs | Just ty' <- coreView ty = split orig_ty ty' bs
    split _       (ForAllTy b res) bs  = split res res (b:bs)
    split orig_ty _                bs  = (reverse bs, orig_ty)

-- | Checks whether this is a proper forall (with a named binder)
isForAllTy :: Type -> Bool
isForAllTy ty | Just ty' <- coreView ty = isForAllTy ty'
isForAllTy (ForAllTy {}) = True
isForAllTy _             = False

-- | Is this a function or forall?
isPiTy :: Type -> Bool
isPiTy ty | Just ty' <- coreView ty = isForAllTy ty'
isPiTy (ForAllTy {}) = True
isPiTy (FunTy {})    = True
isPiTy _             = False

-- | Take a forall type apart, or panics if that is not possible.
splitForAllTy :: Type -> (TyVar, Type)
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
splitForAllTy_maybe :: Type -> Maybe (TyVar, Type)
splitForAllTy_maybe ty = go ty
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy (TvBndr tv _) ty) = Just (tv, ty)
    go _                           = Nothing

-- | Attempts to take a forall type apart; works with proper foralls and
-- functions
splitPiTy_maybe :: Type -> Maybe (TyBinder, Type)
splitPiTy_maybe ty = go ty
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy bndr ty) = Just (Named bndr, ty)
    go (FunTy arg res)    = Just (Anon arg, res)
    go _                  = Nothing

-- | Takes a forall type apart, or panics
splitPiTy :: Type -> (TyBinder, Type)
splitPiTy ty
  | Just answer <- splitPiTy_maybe ty = answer
  | otherwise                         = pprPanic "splitPiTy" (ppr ty)

-- | Split off all TyBinders to a type, splitting both proper foralls
-- and functions
splitPiTys :: Type -> ([TyBinder], Type)
splitPiTys ty = split ty ty []
  where
    split orig_ty ty bs | Just ty' <- coreView ty = split orig_ty ty' bs
    split _       (ForAllTy b res) bs  = split res res (Named b  : bs)
    split _       (FunTy arg res)  bs  = split res res (Anon arg : bs)
    split orig_ty _                bs  = (reverse bs, orig_ty)

-- Like splitPiTys, but returns only *invisible* binders, including constraints
-- Stops at the first visible binder
splitPiTysInvisible :: Type -> ([TyBinder], Type)
splitPiTysInvisible ty = split ty ty []
   where
    split orig_ty ty bs | Just ty' <- coreView ty = split orig_ty ty' bs
    split _       (ForAllTy b@(TvBndr _ vis) res) bs
      | isInvisibleArgFlag vis         = split res res (Named b  : bs)
    split _       (FunTy arg res)  bs
      | isPredTy arg                   = split res res (Anon arg : bs)
    split orig_ty _                bs  = (reverse bs, orig_ty)

-- | Given a tycon and its arguments, filters out any invisible arguments
filterOutInvisibleTypes :: TyCon -> [Type] -> [Type]
filterOutInvisibleTypes tc tys = snd $ partitionInvisibles tc id tys

-- | Like 'filterOutInvisibles', but works on 'TyVar's
filterOutInvisibleTyVars :: TyCon -> [TyVar] -> [TyVar]
filterOutInvisibleTyVars tc tvs = snd $ partitionInvisibles tc mkTyVarTy tvs

-- | Given a tycon and a list of things (which correspond to arguments),
-- partitions the things into
--      Inferred or Specified ones and
--      Required ones
-- The callback function is necessary for this scenario:
--
-- > T :: forall k. k -> k
-- > partitionInvisibles T [forall m. m -> m -> m, S, R, Q]
--
-- After substituting, we get
--
-- > T (forall m. m -> m -> m) :: (forall m. m -> m -> m) -> forall n. n -> n -> n
--
-- Thus, the first argument is invisible, @S@ is visible, @R@ is invisible again,
-- and @Q@ is visible.
--
-- If you're absolutely sure that your tycon's kind doesn't end in a variable,
-- it's OK if the callback function panics, as that's the only time it's
-- consulted.
partitionInvisibles :: TyCon -> (a -> Type) -> [a] -> ([a], [a])
partitionInvisibles tc get_ty = go emptyTCvSubst (tyConKind tc)
  where
    go _ _ [] = ([], [])
    go subst (ForAllTy (TvBndr tv vis) res_ki) (x:xs)
      | isVisibleArgFlag vis = second (x :) (go subst' res_ki xs)
      | otherwise            = first  (x :) (go subst' res_ki xs)
      where
        subst' = extendTvSubst subst tv (get_ty x)
    go subst (TyVarTy tv) xs
      | Just ki <- lookupTyVar subst tv = go subst ki xs
    go _ _ xs = ([], xs)  -- something is ill-kinded. But this can happen
                          -- when printing errors. Assume everything is visible.

-- @isTauTy@ tests if a type has no foralls
isTauTy :: Type -> Bool
isTauTy ty | Just ty' <- coreView ty = isTauTy ty'
isTauTy (TyVarTy _)           = True
isTauTy (LitTy {})            = True
isTauTy (TyConApp tc tys)     = all isTauTy tys && isTauTyCon tc
isTauTy (AppTy a b)           = isTauTy a && isTauTy b
isTauTy (FunTy a b)           = isTauTy a && isTauTy b
isTauTy (ForAllTy {})         = False
isTauTy (CastTy ty _)         = isTauTy ty
isTauTy (CoercionTy _)        = False  -- Not sure about this

{-
%************************************************************************
%*                                                                      *
   TyBinders
%*                                                                      *
%************************************************************************
-}

-- | Make an anonymous binder
mkAnonBinder :: Type -> TyBinder
mkAnonBinder = Anon

-- | Does this binder bind a variable that is /not/ erased? Returns
-- 'True' for anonymous binders.
isAnonTyBinder :: TyBinder -> Bool
isAnonTyBinder (Named {}) = False
isAnonTyBinder (Anon {})  = True

isNamedTyBinder :: TyBinder -> Bool
isNamedTyBinder (Named {}) = True
isNamedTyBinder (Anon {})  = False

tyBinderType :: TyBinder -> Type
-- Barely used
tyBinderType (Named tvb) = binderKind tvb
tyBinderType (Anon ty)   = ty

-- | Extract a relevant type, if there is one.
binderRelevantType_maybe :: TyBinder -> Maybe Type
binderRelevantType_maybe (Named {}) = Nothing
binderRelevantType_maybe (Anon ty)  = Just ty

-- | Like 'maybe', but for binders.
caseBinder :: TyBinder           -- ^ binder to scrutinize
           -> (TyVarBinder -> a) -- ^ named case
           -> (Type -> a)        -- ^ anonymous case
           -> a
caseBinder (Named v) f _ = f v
caseBinder (Anon t)  _ d = d t

-- | Manufacture a new 'TyConBinder' from a 'TyBinder'. Anonymous
-- 'TyBinder's are still assigned names as 'TyConBinder's, so we need
-- the extra gunk with which to construct a 'Name'. Used when producing
-- tyConTyVars from a datatype kind signature. Defined here to avoid module
-- loops.
mkTyBinderTyConBinder :: TyBinder -> SrcSpan -> Unique -> OccName -> TyConBinder
mkTyBinderTyConBinder (Named (TvBndr tv argf)) _ _ _ = TvBndr tv (NamedTCB argf)
mkTyBinderTyConBinder (Anon kind) loc uniq occ
  = TvBndr (mkTyVar (mkInternalName uniq occ loc) kind) AnonTCB

{-
%************************************************************************
%*                                                                      *
                         Pred
*                                                                      *
************************************************************************

Predicates on PredType

Note [isPredTy complications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You would think that we could define
  isPredTy ty = isConstraintKind (typeKind ty)
But there are a number of complications:

* isPredTy is used when printing types, which can happen in debug
  printing during type checking of not-fully-zonked types.  So it's
  not cool to say isConstraintKind (typeKind ty) because, absent
  zonking, the type might be ill-kinded, and typeKind crashes. Hence the
  rather tiresome story here

* isPredTy must return "True" to *unlifted* coercions, such as (t1 ~# t2)
  and (t1 ~R# t2), which are not of kind Constraint!  Currently they are
  of kind #.

* If we do form the type '(C a => C [a]) => blah', then we'd like to
  print it as such. But that means that isPredTy must return True for
  (C a => C [a]).  Admittedly that type is illegal in Haskell, but we
  want to print it nicely in error messages.
-}

-- | Is the type suitable to classify a given/wanted in the typechecker?
isPredTy :: Type -> Bool
-- See Note [isPredTy complications]
isPredTy ty = go ty []
  where
    go :: Type -> [KindOrType] -> Bool
    go (AppTy ty1 ty2)   args       = go ty1 (ty2 : args)
    go (TyVarTy tv)      args       = go_k (tyVarKind tv) args
    go (TyConApp tc tys) args       = ASSERT( null args )  -- TyConApp invariant
                                      go_tc tc tys
    go (FunTy arg res) []
      | isPredTy arg                = isPredTy res   -- (Eq a => C a)
      | otherwise                   = False          -- (Int -> Bool)
    go (ForAllTy _ ty) []           = go ty []
    go (CastTy _ co) args           = go_k (pSnd (coercionKind co)) args
    go _ _ = False

    go_tc :: TyCon -> [KindOrType] -> Bool
    go_tc tc args
      | tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey
                  = args `lengthIs` 4  -- ~# and ~R# sadly have result kind #
                                       -- not Constraint; but we still want
                                       -- isPredTy to reply True.
      | otherwise = go_k (tyConKind tc) args

    go_k :: Kind -> [KindOrType] -> Bool
    -- True <=> ('k' applied to 'kts') = Constraint
    go_k k [] = isConstraintKind k
    go_k k (arg:args) = case piResultTy_maybe k arg of
                          Just k' -> go_k k' args
                          Nothing -> WARN( True, text "isPredTy" <+> ppr ty )
                                     False
       -- This last case shouldn't happen under most circumstances. It can
       -- occur if we call isPredTy during kind checking, especially if one
       -- of the following happens:
       --
       -- 1. There is actually a kind error.  Example in which this showed up:
       --    polykinds/T11399
       --
       -- 2. A type constructor application appears to be oversaturated. An
       --    example of this occurred in GHC Trac #13187:
       --
       --      {-# LANGUAGE PolyKinds #-}
       --      type Const a b = b
       --      f :: Const Int (,) Bool Char -> Char
       --
       --    We call isPredTy (Const k1 k2 Int (,) Bool Char
       --    where k1,k2 are unification variables that have been
       --    unified to *, and (*->*->*) resp, /but not zonked/.
       --    This shows that isPredTy can report a false negative
       --    if a constraint is similarly oversaturated, but
       --    it's hard to do better than isPredTy currently does without
       --    zonking, so we punt on such cases for now.  This only happens
       --    during debug-printing, so it doesn't matter.

isClassPred, isEqPred, isNomEqPred, isIPPred :: PredType -> Bool
isClassPred ty = case tyConAppTyCon_maybe ty of
    Just tyCon | isClassTyCon tyCon -> True
    _                               -> False
isEqPred ty = case tyConAppTyCon_maybe ty of
    Just tyCon -> tyCon `hasKey` eqPrimTyConKey
               || tyCon `hasKey` eqReprPrimTyConKey
    _          -> False

isNomEqPred ty = case tyConAppTyCon_maybe ty of
    Just tyCon -> tyCon `hasKey` eqPrimTyConKey
    _          -> False

isIPPred ty = case tyConAppTyCon_maybe ty of
    Just tc -> isIPTyCon tc
    _       -> False

isIPTyCon :: TyCon -> Bool
isIPTyCon tc = tc `hasKey` ipClassKey
  -- Class and its corresponding TyCon have the same Unique

isIPClass :: Class -> Bool
isIPClass cls = cls `hasKey` ipClassKey

isCTupleClass :: Class -> Bool
isCTupleClass cls = isTupleTyCon (classTyCon cls)

isIPPred_maybe :: Type -> Maybe (FastString, Type)
isIPPred_maybe ty =
  do (tc,[t1,t2]) <- splitTyConApp_maybe ty
     guard (isIPTyCon tc)
     x <- isStrLitTy t1
     return (x,t2)

{-
Make PredTypes

--------------------- Equality types ---------------------------------
-}

-- | Makes a lifted equality predicate at the given role
mkPrimEqPredRole :: Role -> Type -> Type -> PredType
mkPrimEqPredRole Nominal          = mkPrimEqPred
mkPrimEqPredRole Representational = mkReprPrimEqPred
mkPrimEqPredRole Phantom          = panic "mkPrimEqPredRole phantom"

-- | Creates a primitive type equality predicate.
-- Invariant: the types are not Coercions
mkPrimEqPred :: Type -> Type -> Type
mkPrimEqPred ty1 ty2
  = TyConApp eqPrimTyCon [k1, k2, ty1, ty2]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

-- | Creates a primite type equality predicate with explicit kinds
mkHeteroPrimEqPred :: Kind -> Kind -> Type -> Type -> Type
mkHeteroPrimEqPred k1 k2 ty1 ty2 = TyConApp eqPrimTyCon [k1, k2, ty1, ty2]

-- | Creates a primitive representational type equality predicate
-- with explicit kinds
mkHeteroReprPrimEqPred :: Kind -> Kind -> Type -> Type -> Type
mkHeteroReprPrimEqPred k1 k2 ty1 ty2
  = TyConApp eqReprPrimTyCon [k1, k2, ty1, ty2]

-- | Try to split up a coercion type into the types that it coerces
splitCoercionType_maybe :: Type -> Maybe (Type, Type)
splitCoercionType_maybe ty
  = do { (tc, [_, _, ty1, ty2]) <- splitTyConApp_maybe ty
       ; guard $ tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey
       ; return (ty1, ty2) }

mkReprPrimEqPred :: Type -> Type -> Type
mkReprPrimEqPred ty1  ty2
  = TyConApp eqReprPrimTyCon [k1, k2, ty1, ty2]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

equalityTyCon :: Role -> TyCon
equalityTyCon Nominal          = eqPrimTyCon
equalityTyCon Representational = eqReprPrimTyCon
equalityTyCon Phantom          = eqPhantPrimTyCon

-- --------------------- Dictionary types ---------------------------------

mkClassPred :: Class -> [Type] -> PredType
mkClassPred clas tys = TyConApp (classTyCon clas) tys

isDictTy :: Type -> Bool
isDictTy = isClassPred

isDictLikeTy :: Type -> Bool
-- Note [Dictionary-like types]
isDictLikeTy ty | Just ty' <- coreView ty = isDictLikeTy ty'
isDictLikeTy ty = case splitTyConApp_maybe ty of
        Just (tc, tys) | isClassTyCon tc -> True
                       | isTupleTyCon tc -> all isDictLikeTy tys
        _other                           -> False

{-
Note [Dictionary-like types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Being "dictionary-like" means either a dictionary type or a tuple thereof.
In GHC 6.10 we build implication constraints which construct such tuples,
and if we land up with a binding
    t :: (C [a], Eq [a])
    t = blah
then we want to treat t as cheap under "-fdicts-cheap" for example.
(Implication constraints are normally inlined, but sadly not if the
occurrence is itself inside an INLINE function!  Until we revise the
handling of implication constraints, that is.)  This turned out to
be important in getting good arities in DPH code.  Example:

    class C a
    class D a where { foo :: a -> a }
    instance C a => D (Maybe a) where { foo x = x }

    bar :: (C a, C b) => a -> b -> (Maybe a, Maybe b)
    {-# INLINE bar #-}
    bar x y = (foo (Just x), foo (Just y))

Then 'bar' should jolly well have arity 4 (two dicts, two args), but
we ended up with something like
   bar = __inline_me__ (\d1,d2. let t :: (D (Maybe a), D (Maybe b)) = ...
                                in \x,y. <blah>)

This is all a bit ad-hoc; eg it relies on knowing that implication
constraints build tuples.


Decomposing PredType
-}

-- | A choice of equality relation. This is separate from the type 'Role'
-- because 'Phantom' does not define a (non-trivial) equality relation.
data EqRel = NomEq | ReprEq
  deriving (Eq, Ord)

instance Outputable EqRel where
  ppr NomEq  = text "nominal equality"
  ppr ReprEq = text "representational equality"

eqRelRole :: EqRel -> Role
eqRelRole NomEq  = Nominal
eqRelRole ReprEq = Representational

data PredTree = ClassPred Class [Type]
              | EqPred EqRel Type Type
              | IrredPred PredType

classifyPredType :: PredType -> PredTree
classifyPredType ev_ty = case splitTyConApp_maybe ev_ty of
    Just (tc, [_, _, ty1, ty2])
      | tc `hasKey` eqReprPrimTyConKey    -> EqPred ReprEq ty1 ty2
      | tc `hasKey` eqPrimTyConKey        -> EqPred NomEq ty1 ty2
    Just (tc, tys)
      | Just clas <- tyConClass_maybe tc  -> ClassPred clas tys
    _                                     -> IrredPred ev_ty

getClassPredTys :: PredType -> (Class, [Type])
getClassPredTys ty = case getClassPredTys_maybe ty of
        Just (clas, tys) -> (clas, tys)
        Nothing          -> pprPanic "getClassPredTys" (ppr ty)

getClassPredTys_maybe :: PredType -> Maybe (Class, [Type])
getClassPredTys_maybe ty = case splitTyConApp_maybe ty of
        Just (tc, tys) | Just clas <- tyConClass_maybe tc -> Just (clas, tys)
        _ -> Nothing

getEqPredTys :: PredType -> (Type, Type)
getEqPredTys ty
  = case splitTyConApp_maybe ty of
      Just (tc, [_, _, ty1, ty2])
        |  tc `hasKey` eqPrimTyConKey
        || tc `hasKey` eqReprPrimTyConKey
        -> (ty1, ty2)
      _ -> pprPanic "getEqPredTys" (ppr ty)

getEqPredTys_maybe :: PredType -> Maybe (Role, Type, Type)
getEqPredTys_maybe ty
  = case splitTyConApp_maybe ty of
      Just (tc, [_, _, ty1, ty2])
        | tc `hasKey` eqPrimTyConKey     -> Just (Nominal, ty1, ty2)
        | tc `hasKey` eqReprPrimTyConKey -> Just (Representational, ty1, ty2)
      _ -> Nothing

getEqPredRole :: PredType -> Role
getEqPredRole ty = eqRelRole (predTypeEqRel ty)

-- | Get the equality relation relevant for a pred type.
predTypeEqRel :: PredType -> EqRel
predTypeEqRel ty
  | Just (tc, _) <- splitTyConApp_maybe ty
  , tc `hasKey` eqReprPrimTyConKey
  = ReprEq
  | otherwise
  = NomEq

{-
%************************************************************************
%*                                                                      *
         Well-scoped tyvars
*                                                                      *
************************************************************************
-}

-- | Do a topological sort on a list of tyvars,
--   so that binders occur before occurrences
-- E.g. given  [ a::k, k::*, b::k ]
-- it'll return a well-scoped list [ k::*, a::k, b::k ]
--
-- This is a deterministic sorting operation
-- (that is, doesn't depend on Uniques).
toposortTyVars :: [TyVar] -> [TyVar]
toposortTyVars tvs = reverse $
                     [ node_payload node | node <- topologicalSortG $
                                          graphFromEdgedVerticesOrd nodes ]
  where
    var_ids :: VarEnv Int
    var_ids = mkVarEnv (zip tvs [1..])

    nodes :: [ Node Int TyVar ]
    nodes = [ DigraphNode
                tv
                (lookupVarEnv_NF var_ids tv)
                (mapMaybe (lookupVarEnv var_ids)
                         (tyCoVarsOfTypeList (tyVarKind tv)))
            | tv <- tvs ]

-- | Extract a well-scoped list of variables from a deterministic set of
-- variables. The result is deterministic.
-- NB: There used to exist varSetElemsWellScoped :: VarSet -> [Var] which
-- took a non-deterministic set and produced a non-deterministic
-- well-scoped list. If you care about the list being well-scoped you also
-- most likely care about it being in deterministic order.
dVarSetElemsWellScoped :: DVarSet -> [Var]
dVarSetElemsWellScoped = toposortTyVars . dVarSetElems

-- | Get the free vars of a type in scoped order
tyCoVarsOfTypeWellScoped :: Type -> [TyVar]
tyCoVarsOfTypeWellScoped = toposortTyVars . tyCoVarsOfTypeList

-- | Get the free vars of types in scoped order
tyCoVarsOfTypesWellScoped :: [Type] -> [TyVar]
tyCoVarsOfTypesWellScoped = toposortTyVars . tyCoVarsOfTypesList

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

-- | Pretty prints a 'TyCon', using the family instance in case of a
-- representation tycon.  For example:
--
-- > data T [a] = ...
--
-- In that case we want to print @T [a]@, where @T@ is the family 'TyCon'
pprSourceTyCon :: TyCon -> SDoc
pprSourceTyCon tycon
  | Just (fam_tc, tys) <- tyConFamInst_maybe tycon
  = ppr $ fam_tc `TyConApp` tys        -- can't be FunTyCon
  | otherwise
  = ppr tycon

-- @isTauTy@ tests if a type has no foralls
isFamFreeTy :: Type -> Bool
isFamFreeTy ty | Just ty' <- coreView ty = isFamFreeTy ty'
isFamFreeTy (TyVarTy _)       = True
isFamFreeTy (LitTy {})        = True
isFamFreeTy (TyConApp tc tys) = all isFamFreeTy tys && isFamFreeTyCon tc
isFamFreeTy (AppTy a b)       = isFamFreeTy a && isFamFreeTy b
isFamFreeTy (FunTy a b)       = isFamFreeTy a && isFamFreeTy b
isFamFreeTy (ForAllTy _ ty)   = isFamFreeTy ty
isFamFreeTy (CastTy ty _)     = isFamFreeTy ty
isFamFreeTy (CoercionTy _)    = False  -- Not sure about this

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
isLiftedType_maybe ty = go (getRuntimeRep "isLiftedType_maybe" ty)
  where
    go rr | Just rr' <- coreView rr = go rr'
    go (TyConApp lifted_rep [])
      | lifted_rep `hasKey` liftedRepDataConKey = Just True
    go (TyConApp {}) = Just False -- everything else is unlifted
    go _             = Nothing    -- levity polymorphic

-- | See "Type#type_classification" for what an unlifted type is.
-- Panics on levity polymorphic types.
isUnliftedType :: HasDebugCallStack => Type -> Bool
        -- isUnliftedType returns True for forall'd unlifted types:
        --      x :: forall a. Int#
        -- I found bindings like these were getting floated to the top level.
        -- They are pretty bogus types, mind you.  It would be better never to
        -- construct them
isUnliftedType ty
  = not (isLiftedType_maybe ty `orElse`
         pprPanic "isUnliftedType" (ppr ty <+> dcolon <+> ppr (typeKind ty)))

-- | Extract the RuntimeRep classifier of a type. For instance,
-- @getRuntimeRep_maybe Int = LiftedRep@. Returns 'Nothing' if this is not
-- possible.
getRuntimeRep_maybe :: HasDebugCallStack
                    => Type -> Maybe Type
getRuntimeRep_maybe = getRuntimeRepFromKind_maybe . typeKind

-- | Extract the RuntimeRep classifier of a type. For instance,
-- @getRuntimeRep_maybe Int = LiftedRep@. Panics if this is not possible.
getRuntimeRep :: HasDebugCallStack
              => String   -- ^ Printed in case of an error
              -> Type -> Type
getRuntimeRep err ty =
    case getRuntimeRep_maybe ty of
      Just r  -> r
      Nothing -> pprPanic "getRuntimeRep"
                          (text err $$ ppr ty <+> dcolon <+> ppr (typeKind ty))

-- | Extract the RuntimeRep classifier of a type from its kind. For example,
-- @getRuntimeRepFromKind * = LiftedRep@; Panics if this is not possible.
getRuntimeRepFromKind :: HasDebugCallStack
                      => String -> Type -> Type
getRuntimeRepFromKind err k =
    case getRuntimeRepFromKind_maybe k of
      Just r  -> r
      Nothing -> pprPanic "getRuntimeRepFromKind"
                          (text err $$ ppr k <+> dcolon <+> ppr (typeKind k))

-- | Extract the RuntimeRep classifier of a type from its kind. For example,
-- @getRuntimeRepFromKind * = LiftedRep@; Returns 'Nothing' if this is not
-- possible.
getRuntimeRepFromKind_maybe :: HasDebugCallStack
                            => Type -> Maybe Type
getRuntimeRepFromKind_maybe = go
  where
    go k | Just k' <- coreView k = go k'
    go k
      | Just (_tc, [arg]) <- splitTyConApp_maybe k
      = ASSERT2( _tc `hasKey` tYPETyConKey, ppr k )
        Just arg
    go _ = Nothing

isUnboxedTupleType :: Type -> Bool
isUnboxedTupleType ty
  = tyConAppTyCon (getRuntimeRep "isUnboxedTupleType" ty) `hasKey` tupleRepDataConKey
  -- NB: Do not use typePrimRep, as that can't tell the difference between
  -- unboxed tuples and unboxed sums


isUnboxedSumType :: Type -> Bool
isUnboxedSumType ty
  = tyConAppTyCon (getRuntimeRep "isUnboxedSumType" ty) `hasKey` sumRepDataConKey

-- | See "Type#type_classification" for what an algebraic type is.
-- Should only be applied to /types/, as opposed to e.g. partially
-- saturated type constructors
isAlgType :: Type -> Bool
isAlgType ty
  = case splitTyConApp_maybe ty of
      Just (tc, ty_args) -> ASSERT( ty_args `lengthIs` tyConArity tc )
                            isAlgTyCon tc
      _other             -> False

-- | See "Type#type_classification" for what an algebraic type is.
-- Should only be applied to /types/, as opposed to e.g. partially
-- saturated type constructors. Closed type constructors are those
-- with a fixed right hand side, as opposed to e.g. associated types
isClosedAlgType :: Type -> Bool
isClosedAlgType ty
  = case splitTyConApp_maybe ty of
      Just (tc, ty_args) | isAlgTyCon tc && not (isFamilyTyCon tc)
             -> ASSERT2( ty_args `lengthIs` tyConArity tc, ppr ty ) True
      _other -> False

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
-- (See Note [The polymorphism rule of join points] in CoreSyn.) Returns False
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
      = isEmptyVarSet (tvs `intersectVarSet` tyCoVarsOfType ty)
      | Just (t, ty') <- splitForAllTy_maybe ty
      = valid_under (tvs `extendVarSet` t) (arity-1) ty'
      | Just (_, res_ty) <- splitFunTy_maybe ty
      = valid_under tvs (arity-1) res_ty
      | otherwise
      = False

{-
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
seqType (FunTy t1 t2)               = seqType t1 `seq` seqType t2
seqType (TyConApp tc tys)           = tc `seq` seqTypes tys
seqType (ForAllTy (TvBndr tv _) ty) = seqType (tyVarKind tv) `seq` seqType ty
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
-- See also Note [Non-trivial definitional equality] in TyCoRep.
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
 | eqTypeX env (tyVarKind tv1) (tyVarKind tv2)
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
    -- See Note [Non-trivial definitional equality] in TyCoRep
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
    go env (ForAllTy (TvBndr tv1 _) t1) (ForAllTy (TvBndr tv2 _) t2)
      = go env (tyVarKind tv1) (tyVarKind tv2)
        `thenCmpTy` go (rnBndr2 env tv1 tv2) t1 t2
        -- See Note [Equality on AppTys]
    go env (AppTy s1 t1) ty2
      | Just (s2, t2) <- repSplitAppTy_maybe ty2
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env ty1 (AppTy s2 t2)
      | Just (s1, t1) <- repSplitAppTy_maybe ty1
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env (FunTy s1 t1) (FunTy s2 t2)
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
                                      `thenCmp` nonDetCmpTypesX env tys1 tys2
nonDetCmpTypesX _   []        _         = LT
nonDetCmpTypesX _   _         []        = GT

-------------
-- | Compare two 'TyCon's. NB: This should /never/ see the "star synonyms",
-- as recognized by Kind.isStarKindSynonymTyCon. See Note
-- [Kind Constraint and kind *] in Kind.
-- See Note [nonDetCmpType nondeterminism]
nonDetCmpTc :: TyCon -> TyCon -> Ordering
nonDetCmpTc tc1 tc2
  = ASSERT( not (isStarKindSynonymTyCon tc1) && not (isStarKindSynonymTyCon tc2) )
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
-}

typeKind :: Type -> Kind
typeKind (TyConApp tc tys)     = piResultTys (tyConKind tc) tys
typeKind (AppTy fun arg)       = piResultTy (typeKind fun) arg
typeKind (LitTy l)             = typeLiteralKind l
typeKind (FunTy {})            = liftedTypeKind
typeKind (ForAllTy _ ty)       = typeKind ty
typeKind (TyVarTy tyvar)       = tyVarKind tyvar
typeKind (CastTy _ty co)       = pSnd $ coercionKind co
typeKind (CoercionTy co)       = coercionType co

typeLiteralKind :: TyLit -> Kind
typeLiteralKind l =
  case l of
    NumTyLit _ -> typeNatKind
    StrTyLit _ -> typeSymbolKind

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
     go (FunTy a b)                 = go a `unionUniqSets` go b `unionUniqSets` go_tc funTyCon
     go (ForAllTy (TvBndr tv _) ty) = go ty `unionUniqSets` go (tyVarKind tv)
     go (CastTy ty co)              = go ty `unionUniqSets` go_co co
     go (CoercionTy co)             = go_co co

     go_co (Refl _ ty)             = go ty
     go_co (TyConAppCo _ tc args)  = go_tc tc `unionUniqSets` go_cos args
     go_co (AppCo co arg)          = go_co co `unionUniqSets` go_co arg
     go_co (ForAllCo _ kind_co co) = go_co kind_co `unionUniqSets` go_co co
     go_co (FunCo _ co1 co2)       = go_co co1 `unionUniqSets` go_co co2
     go_co (AxiomInstCo ax _ args) = go_ax ax `unionUniqSets` go_cos args
     go_co (UnivCo p _ t1 t2)      = go_prov p `unionUniqSets` go t1 `unionUniqSets` go t2
     go_co (CoVarCo {})            = emptyUniqSet
     go_co (SymCo co)              = go_co co
     go_co (TransCo co1 co2)       = go_co co1 `unionUniqSets` go_co co2
     go_co (NthCo _ co)            = go_co co
     go_co (LRCo _ co)             = go_co co
     go_co (InstCo co arg)         = go_co co `unionUniqSets` go_co arg
     go_co (CoherenceCo co1 co2)   = go_co co1 `unionUniqSets` go_co co2
     go_co (KindCo co)             = go_co co
     go_co (SubCo co)              = go_co co
     go_co (AxiomRuleCo _ cs)      = go_cos cs

     go_prov UnsafeCoerceProv    = emptyUniqSet
     go_prov (PhantomProv co)    = go_co co
     go_prov (ProofIrrelProv co) = go_co co
     go_prov (PluginProv _)      = emptyUniqSet
     go_prov (HoleProv _)        = emptyUniqSet
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
    go (FunTy t1 t2)     = go t1 `mappend` go t2
    go (ForAllTy (TvBndr tv _) ty)
      = ((`delVarSet` tv) <$> go ty) `mappend`
        (invisible (tyCoVarsOfType $ tyVarKind tv))
    go (LitTy {}) = mempty
    go (CastTy ty co) = go ty `mappend` invisible (tyCoVarsOfCo co)
    go (CoercionTy co) = invisible $ tyCoVarsOfCo co

    invisible vs = Pair vs emptyVarSet

    go_tc tc tys = let (invis, vis) = partitionInvisibles tc id tys in
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
