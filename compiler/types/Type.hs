-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998
--
-- Type - public interface

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Main functions for manipulating types and type-related things
module Type (
        -- Note some of this is just re-exports from TyCon..

        -- * Main data types representing Types
        -- $type_classification

        -- $representation_types
        TyThing(..), Type, VisibilityFlag(..), KindOrType, PredType, ThetaType,
        Var, TyVar, isTyVar, TyCoVar, TyBinder,

        -- ** Constructing and deconstructing types
        mkTyVarTy, mkTyVarTys, getTyVar, getTyVar_maybe, repGetTyVar_maybe,
        getCastedTyVar_maybe, tyVarKind,

        mkAppTy, mkAppTys, splitAppTy, splitAppTys, repSplitAppTys,
        splitAppTy_maybe, repSplitAppTy_maybe, tcRepSplitAppTy_maybe,

        mkFunTy, mkFunTys, splitFunTy, splitFunTy_maybe,
        splitFunTys, splitFunTysN,
        funResultTy, funArgTy,

        mkTyConApp, mkTyConTy,
        tyConAppTyCon_maybe, tyConAppTyConPicky_maybe,
        tyConAppArgs_maybe, tyConAppTyCon, tyConAppArgs,
        splitTyConApp_maybe, splitTyConApp, tyConAppArgN, nextRole,
        splitListTyConApp_maybe,
        repSplitTyConApp_maybe,

        mkForAllTy, mkForAllTys, mkInvForAllTys, mkSpecForAllTys,
        mkVisForAllTys,
        mkNamedForAllTy,
        splitForAllTy_maybe, splitForAllTys, splitForAllTy,
        splitPiTy_maybe, splitPiTys, splitPiTy,
        splitNamedPiTys,
        mkPiType, mkPiTypes, mkTyBindersPreferAnon,
        piResultTy, piResultTys,
        applyTysX, dropForAlls,

        mkNumLitTy, isNumLitTy,
        mkStrLitTy, isStrLitTy,

        mkCastTy, mkCoercionTy, splitCastTy_maybe,

        userTypeError_maybe, pprUserTypeErrorTy,

        coAxNthLHS,
        stripCoercionTy, splitCoercionType_maybe,

        splitPiTysInvisible, filterOutInvisibleTypes,
        filterOutInvisibleTyVars, partitionInvisibles,
        synTyConResKind,

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
        mkNamedBinder, mkNamedBinders,
        mkAnonBinder, isNamedBinder, isAnonBinder,
        isIdLikeBinder, binderVisibility, binderVar_maybe,
        binderVar, binderRelevantType_maybe, caseBinder,
        partitionBinders, partitionBindersIntoBinders,
        binderType, isVisibleBinder, isInvisibleBinder,

        -- ** Common type constructors
        funTyCon,

        -- ** Predicates on types
        allDistinctTyVars,
        isTyVarTy, isFunTy, isDictTy, isPredTy, isVoidTy, isCoercionTy,
        isCoercionTy_maybe, isCoercionType, isForAllTy,
        isPiTy,

        -- (Lifting and boxity)
        isUnliftedType, isUnboxedTupleType, isAlgType, isClosedAlgType,
        isPrimitiveType, isStrictType,
        isRuntimeRepTy, isRuntimeRepVar, isRuntimeRepKindedTy,
        dropRuntimeRepArgs,
        getRuntimeRep, getRuntimeRepFromKind,

        -- * Main data types representing Kinds
        Kind,

        -- ** Finding the kind of a type
        typeKind,

        -- ** Common Kind
        liftedTypeKind,

        -- * Type free variables
        tyCoVarsOfType, tyCoVarsOfTypes, tyCoVarsOfTypeAcc,
        tyCoVarsOfTypeDSet,
        coVarsOfType,
        coVarsOfTypes, closeOverKinds,
        splitDepVarsOfType, splitDepVarsOfTypes,
        splitVisVarsOfType, splitVisVarsOfTypes,
        expandTypeSynonyms,
        typeSize,

        -- * Well-scoped lists of variables
        varSetElemsWellScoped, toposortTyVars, tyCoVarsOfTypeWellScoped,
        tyCoVarsOfTypesWellScoped,

        -- * Type comparison
        eqType, eqTypeX, eqTypes, cmpType, cmpTypes, cmpTypeX, cmpTypesX, cmpTc,
        eqVarBndrs,

        -- * Forcing evaluation of types
        seqType, seqTypes,

        -- * Other views onto Types
        coreView, coreViewOneStarKind,

        UnaryType, RepType(..), flattenRepType, repType,
        tyConsOfType,

        -- * Type representation for the code generator
        typePrimRep, typeRepArity, kindPrimRep, tyConPrimRep,

        -- * Main type substitution data types
        TvSubstEnv,     -- Representation widely visible
        TCvSubst(..),    -- Representation visible to a few friends

        -- ** Manipulating type substitutions
        emptyTvSubstEnv, emptyTCvSubst, mkEmptyTCvSubst,

        mkTCvSubst, zipTvSubst, mkTvSubstPrs,
        notElemTCvSubst,
        getTvSubstEnv, setTvSubstEnv,
        zapTCvSubst, getTCvInScope,
        extendTCvInScope, extendTCvInScopeList, extendTCvInScopeSet,
        extendTCvSubst, extendCvSubst,
        extendTvSubst, extendTvSubstList, extendTvSubstAndInScope,
        isInScope, composeTCvSubstEnv, composeTCvSubst, zipTyEnv, zipCoEnv,
        isEmptyTCvSubst, unionTCvSubst,

        -- ** Performing substitution on types and kinds
        substTy, substTys, substTyWith, substTysWith, substTheta,
        substTyAddInScope,
        substTyUnchecked, substTysUnchecked, substThetaUnchecked,
        substTyWithBindersUnchecked, substTyWithUnchecked,
        substCoUnchecked, substCoWithUnchecked,
        substTyVarBndr, substTyVar, substTyVars,
        cloneTyVarBndr, cloneTyVarBndrs, lookupTyVar,

        -- * Pretty-printing
        pprType, pprParendType, pprTypeApp, pprTyThingCategory, pprTyThing,
        pprTvBndr, pprTvBndrs, pprForAll, pprForAllImplicit, pprUserForAll,
        pprSigmaType,
        pprTheta, pprThetaArrowTy, pprClassPred,
        pprKind, pprParendKind, pprSourceTyCon,
        TyPrec(..), maybeParen,
        pprTyVar, pprTcAppTy, pprPrefixApp, pprArrowChain,

        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyOpenKind,
        tidyTyCoVarBndr, tidyTyCoVarBndrs, tidyFreeTyCoVars,
        tidyOpenTyCoVar, tidyOpenTyCoVars,
        tidyTyVarOcc,
        tidyTopType,
        tidyKind,
        tidyTyBinder, tidyTyBinders
    ) where

#include "HsVersions.h"

-- We import the representation and primitive functions from TyCoRep.
-- Many things are reexported, but not the representation!

import Kind
import TyCoRep

-- friends:
import Var
import VarEnv
import VarSet
import NameEnv

import Class
import TyCon
import TysPrim
import {-# SOURCE #-} TysWiredIn ( listTyCon, typeNatKind
                                 , typeSymbolKind, liftedTypeKind )
import PrelNames
import CoAxiom
import {-# SOURCE #-} Coercion

-- others
import BasicTypes       ( Arity, RepArity )
import Util
import Outputable
import FastString
import Pair
import ListSetOps
import Digraph

import Maybes           ( orElse )
import Data.Maybe       ( isJust, mapMaybe )
import Control.Monad    ( guard )
import Control.Arrow    ( first, second )

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ( Applicative, (<*>), (<$>), pure )
import Data.Monoid         ( Monoid(..) )
import Data.Foldable       ( foldMap )
#endif

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
-- Type         primitive       boxed           lifted          algebraic
-- -----------------------------------------------------------------------------
-- Int#         Yes             No              No              No
-- ByteArray#   Yes             Yes             No              No
-- (\# a, b \#)   Yes             No              No              Yes
-- (  a, b  )   No              Yes             Yes             Yes
-- [a]          No              Yes             Yes             Yes
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
-}

{-# INLINE coreView #-}
coreView :: Type -> Maybe Type
-- ^ This function Strips off the /top layer only/ of a type synonym
-- application (if any) its underlying representation type.
-- Returns Nothing if there is nothing to look through.
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
coreView _ = Nothing

-- | Like 'coreView', but it also "expands" @Constraint@ to become
-- @TYPE PtrRepLifted@.
{-# INLINE coreViewOneStarKind #-}
coreViewOneStarKind :: Type -> Maybe Type
coreViewOneStarKind = go Nothing
  where
    go _ t | Just t' <- coreView t                    = go (Just t') t'
    go _ (TyConApp tc []) | isStarKindSynonymTyCon tc = go (Just t') t'
      where t' = liftedTypeKind
    go res _ = res

-----------------------------------------------
expandTypeSynonyms :: Type -> Type
-- ^ Expand out all type synonyms.  Actually, it'd suffice to expand out
-- just the ones that discard type variables (e.g.  type Funny a = Int)
-- But we don't know which those are currently, so we just expand all.
--
-- 'expandTypeSynonyms' only expands out type synonyms mentioned in the type,
-- not in the kinds of any TyCon or TyVar mentioned in the type.
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
    go subst (ForAllTy (Anon arg) res)
      = mkFunTy (go subst arg) (go subst res)
    go subst (ForAllTy (Named tv vis) t)
      = let (subst', tv') = substTyVarBndrCallback go subst tv in
        ForAllTy (Named tv' vis) (go subst' t)
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

      , tcm_tybinder :: env -> TyVar -> VisibilityFlag -> m (env, TyVar)
          -- ^ The returned env is used in the extended scope
      }

{-# INLINABLE mapType #-}  -- See Note [Specialising mappers]
mapType :: (Applicative m, Monad m) => TyCoMapper env m -> env -> Type -> m Type
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
    go (ForAllTy (Anon arg) res) = mkfunty <$> go arg <*> go res
    go (ForAllTy (Named tv vis) inner)
      = do { (env', tv') <- tybinder env tv vis
           ; inner' <- mapType mapper env' inner
           ; return $ ForAllTy (Named tv' vis) inner' }
    go ty@(LitTy {}) = return ty
    go (CastTy ty co) = mkcastty <$> go ty <*> mapCoercion mapper env co
    go (CoercionTy co) = CoercionTy <$> mapCoercion mapper env co

    (mktyconapp, mkappty, mkcastty, mkfunty)
      | smart     = (mkTyConApp, mkAppTy, mkCastTy, mkFunTy)
      | otherwise = (TyConApp,   AppTy,   CastTy,   ForAllTy . Anon)

{-# INLINABLE mapCoercion #-}  -- See Note [Specialising mappers]
mapCoercion :: (Applicative m, Monad m)
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
           ; (env', tv') <- tybinder env tv Invisible
           ; co' <- mapCoercion mapper env' co
           ; return $ mkforallco tv' kind_co' co' }
        -- See Note [Efficiency for mapCoercion ForAllCo case]
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
-- with the coercion. Thus, the co is :: kind tv ~R kind type
getCastedTyVar_maybe :: Type -> Maybe (TyVar, Coercion)
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

allDistinctTyVars :: [KindOrType] -> Bool
allDistinctTyVars tkvs = go emptyVarSet tkvs
  where
    go _      [] = True
    go so_far (ty : tys)
       = case getTyVar_maybe ty of
             Nothing -> False
             Just tv | tv `elemVarSet` so_far -> False
                     | otherwise -> go (so_far `extendVarSet` tv) tys

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
source Haskell.  In constrast, we *can* unify (a b) with (t1 -> t2).
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
repSplitAppTy_maybe (ForAllTy (Anon ty1) ty2)
                                      = Just (TyConApp funTyCon [ty1], ty2)
repSplitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
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
tcRepSplitAppTy_maybe (ForAllTy (Anon ty1) ty2)
  | isConstraintKind (typeKind ty1)     = Nothing  -- See Note [Decomposing fat arrow c=>t]
  | otherwise                           = Just (TyConApp funTyCon [ty1], ty2)
tcRepSplitAppTy_maybe (AppTy ty1 ty2)   = Just (ty1, ty2)
tcRepSplitAppTy_maybe (TyConApp tc tys)
  | mightBeUnsaturatedTyCon tc || tys `lengthExceeds` tyConArity tc
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
            n | mightBeUnsaturatedTyCon tc = 0
              | otherwise                  = tyConArity tc
            (tc_args1, tc_args2) = splitAt n tc_args
        in
        (TyConApp tc tc_args1, tc_args2 ++ args)
    split _   (ForAllTy (Anon ty1) ty2) args = ASSERT( null args )
                                               (TyConApp funTyCon [], [ty1,ty2])
    split orig_ty _                     args = (orig_ty, args)

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
    split (ForAllTy (Anon ty1) ty2) args = ASSERT( null args )
                                           (TyConApp funTyCon [], [ty1, ty2])
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

Function types are represented with (ForAllTy (Anon ...) ...)
-}

isFunTy :: Type -> Bool
isFunTy ty = isJust (splitFunTy_maybe ty)

splitFunTy :: Type -> (Type, Type)
-- ^ Attempts to extract the argument and result types from a type, and
-- panics if that is not possible. See also 'splitFunTy_maybe'
splitFunTy ty | Just ty' <- coreView ty = splitFunTy ty'
splitFunTy (ForAllTy (Anon arg) res)    = (arg, res)
splitFunTy other                        = pprPanic "splitFunTy" (ppr other)

splitFunTy_maybe :: Type -> Maybe (Type, Type)
-- ^ Attempts to extract the argument and result types from a type
splitFunTy_maybe ty | Just ty' <- coreView ty = splitFunTy_maybe ty'
splitFunTy_maybe (ForAllTy (Anon arg) res) = Just (arg, res)
splitFunTy_maybe _                         = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args orig_ty ty | Just ty' <- coreView ty = split args orig_ty ty'
    split args _       (ForAllTy (Anon arg) res) = split (arg:args) res res
    split args orig_ty _                         = (reverse args, orig_ty)

splitFunTysN :: Int -> Type -> ([Type], Type)
-- ^ Split off exactly the given number argument types, and panics if that is not possible
splitFunTysN 0 ty = ([], ty)
splitFunTysN n ty = ASSERT2( isFunTy ty, int n <+> ppr ty )
                    case splitFunTy ty of { (arg, res) ->
                    case splitFunTysN (n-1) res of { (args, res) ->
                    (arg:args, res) }}

funResultTy :: Type -> Type
-- ^ Extract the function result type and panic if that is not possible
funResultTy ty | Just ty' <- coreView ty = funResultTy ty'
funResultTy (ForAllTy (Anon {}) res)     = res
funResultTy ty                           = pprPanic "funResultTy" (ppr ty)

funArgTy :: Type -> Type
-- ^ Extract the function argument type and panic if that is not possible
funArgTy ty | Just ty' <- coreView ty = funArgTy ty'
funArgTy (ForAllTy (Anon arg) _res) = arg
funArgTy ty                         = pprPanic "funArgTy" (ppr ty)

piResultTy :: Type -> Type -> Type
-- ^ Just like 'piResultTys' but for a single argument
-- Try not to iterate 'piResultTy', because it's inefficient to substitute
-- one variable at a time; instead use 'piResultTys"
piResultTy ty arg
  | Just ty' <- coreView ty = piResultTy ty' arg

  | ForAllTy bndr res <- ty
  = case bndr of
      Anon {}    -> res
      Named tv _ -> substTy (extendTvSubst empty_subst tv arg) res
        where
          empty_subst = mkEmptyTCvSubst $ mkInScopeSet $
                        tyCoVarsOfTypes [arg,res]
  | otherwise
  = pprPanic "piResultTy" (ppr ty $$ ppr arg)

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

  | ForAllTy bndr res <- ty
  = case bndr of
      Anon {}    -> piResultTys res args
      Named tv _ -> go (extendVarEnv emptyTvSubstEnv tv arg) res args

  | otherwise
  = pprPanic "piResultTys1" (ppr ty $$ ppr orig_args)
  where
    go :: TvSubstEnv -> Type -> [Type] -> Type
    go tv_env ty [] = substTy (mkTvSubst in_scope tv_env) ty
      where
        in_scope = mkInScopeSet (tyCoVarsOfTypes (ty:orig_args))

    go tv_env ty all_args@(arg:args)
      | Just ty' <- coreView ty
      = go tv_env ty' all_args

      | ForAllTy bndr res <- ty
      = case bndr of
          Anon _     -> go tv_env res args
          Named tv _ -> go (extendVarEnv tv_env tv arg) res args

      | TyVarTy tv <- ty
      , Just ty' <- lookupVarEnv tv_env tv
        -- Deals with piResultTys (forall a. a) [forall b.b, Int]
      = piResultTys ty' all_args

      | otherwise
      = pprPanic "piResultTys2" (ppr ty $$ ppr orig_args $$ ppr all_args)

{-
---------------------------------------------------------------------
                                TyConApp
                                ~~~~~~~~
-}

-- | A key function: builds a 'TyConApp' or 'FunTy' as appropriate to
-- its arguments.  Applies its arguments to the constructor from left to right.
mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tycon tys
  | isFunTyCon tycon, [ty1,ty2] <- tys
  = ForAllTy (Anon ty1) ty2

  | otherwise
  = TyConApp tycon tys

-- splitTyConApp "looks through" synonyms, because they don't
-- mean a distinct type, but all other type-constructor applications
-- including functions are returned as Just ..

-- | Retrieve the tycon heading this type, if there is one. Does /not/
-- look through synonyms.
tyConAppTyConPicky_maybe :: Type -> Maybe TyCon
tyConAppTyConPicky_maybe (TyConApp tc _)       = Just tc
tyConAppTyConPicky_maybe (ForAllTy (Anon _) _) = Just funTyCon
tyConAppTyConPicky_maybe _                     = Nothing


-- | The same as @fst . splitTyConApp@
tyConAppTyCon_maybe :: Type -> Maybe TyCon
tyConAppTyCon_maybe ty | Just ty' <- coreView ty = tyConAppTyCon_maybe ty'
tyConAppTyCon_maybe (TyConApp tc _)       = Just tc
tyConAppTyCon_maybe (ForAllTy (Anon _) _) = Just funTyCon
tyConAppTyCon_maybe _                     = Nothing

tyConAppTyCon :: Type -> TyCon
tyConAppTyCon ty = tyConAppTyCon_maybe ty `orElse` pprPanic "tyConAppTyCon" (ppr ty)

-- | The same as @snd . splitTyConApp@
tyConAppArgs_maybe :: Type -> Maybe [Type]
tyConAppArgs_maybe ty | Just ty' <- coreView ty = tyConAppArgs_maybe ty'
tyConAppArgs_maybe (TyConApp _ tys)          = Just tys
tyConAppArgs_maybe (ForAllTy (Anon arg) res) = Just [arg,res]
tyConAppArgs_maybe _                         = Nothing

tyConAppArgs :: Type -> [Type]
tyConAppArgs ty = tyConAppArgs_maybe ty `orElse` pprPanic "tyConAppArgs" (ppr ty)

tyConAppArgN :: Int -> Type -> Type
-- Executing Nth
tyConAppArgN n ty
  = case tyConAppArgs_maybe ty of
      Just tys -> ASSERT2( n < length tys, ppr n <+> ppr tys ) tys `getNth` n
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
splitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
splitTyConApp_maybe ty | Just ty' <- coreView ty = splitTyConApp_maybe ty'
splitTyConApp_maybe ty                           = repSplitTyConApp_maybe ty

-- | Like 'splitTyConApp_maybe', but doesn't look through synonyms. This
-- assumes the synonyms have already been dealt with.
repSplitTyConApp_maybe :: Type -> Maybe (TyCon, [Type])
repSplitTyConApp_maybe (TyConApp tc tys)         = Just (tc, tys)
repSplitTyConApp_maybe (ForAllTy (Anon arg) res) = Just (funTyCon, [arg,res])
repSplitTyConApp_maybe _                         = Nothing

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

Note [Weird typing rule for ForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here is the (truncated) typing rule for the dependent ForAllTy:

inner : kind
------------------------------------
ForAllTy (Named tv vis) inner : kind

Note that neither the inner type nor for ForAllTy itself have to have
kind *! But, it means that we should push any kind casts through the
ForAllTy. The only trouble is avoiding capture.

-}

splitCastTy_maybe :: Type -> Maybe (Type, Coercion)
splitCastTy_maybe ty | Just ty' <- coreView ty = splitCastTy_maybe ty'
splitCastTy_maybe (CastTy ty co)               = Just (ty, co)
splitCastTy_maybe _                            = Nothing

-- | Make a 'CastTy'. The Coercion must be nominal. This function looks
-- at the entire structure of the type and coercion in an attempt to
-- maintain representation invariance (that is, any two types that are `eqType`
-- look the same). Be very wary of calling this in a loop.
mkCastTy :: Type -> Coercion -> Type
-- Running example:
--   T :: forall k1. k1 -> forall k2. k2 -> Bool -> Maybe k1 -> *
--   co :: * ~R X    (maybe X is a newtype around *)
--   ty = T Nat 3 Symbol "foo" True (Just 2)
--
-- We wish to "push" the cast down as far as possible. See also
-- Note [Pushing down casts] in TyCoRep. Here is where we end
-- up:
--
--   (T Nat 3 Symbol |> <Symbol> -> <Bool> -> <Maybe Nat> -> co)
--      "foo" True (Just 2)
--
mkCastTy ty co | isReflexiveCo co = ty
-- NB: Do the slow check here. This is important to keep the splitXXX
-- functions working properly. Otherwise, we may end up with something
-- like (((->) |> something_reflexive_but_not_obviously_so) biz baz)
-- fails under splitFunTy_maybe. This happened with the cheaper check
-- in test dependent/should_compile/dynamic-paper.

mkCastTy (CastTy ty co1) co2 = mkCastTy ty (co1 `mkTransCo` co2)
-- See Note [Weird typing rule for ForAllTy]
mkCastTy outer_ty@(ForAllTy (Named tv vis) inner_ty) co
  = -- have to make sure that pushing the co in doesn't capture the bound var
    let fvs = tyCoVarsOfCo co `unionVarSet` tyCoVarsOfType outer_ty
        empty_subst = mkEmptyTCvSubst (mkInScopeSet fvs)
        (subst, tv') = substTyVarBndr empty_subst tv
    in
    ForAllTy (Named tv' vis) (substTy subst inner_ty `mkCastTy` co)
mkCastTy ty co = -- NB: don't check if the coercion "from" type matches here;
                 -- there may be unzonked variables about
                 let result = split_apps [] ty co in
                 ASSERT2( CastTy ty co `eqType` result
                        , ppr ty <+> dcolon <+> ppr (typeKind ty) $$
                          ppr co <+> dcolon <+> ppr (coercionKind co) $$
                          ppr result <+> dcolon <+> ppr (typeKind result) )
                 result
  where
    -- split_apps breaks apart any type applications, so we can see how far down
    -- to push the cast
    split_apps args (AppTy t1 t2) co
      = split_apps (t2:args) t1 co
    split_apps args (TyConApp tc tc_args) co
      | mightBeUnsaturatedTyCon tc
      = affix_co (tyConBinders tc) (mkTyConTy tc) (tc_args `chkAppend` args) co
      | otherwise -- not decomposable... but it may still be oversaturated
      = let (non_decomp_args, decomp_args) = splitAt (tyConArity tc) tc_args
            saturated_tc = mkTyConApp tc non_decomp_args
        in
        affix_co (fst $ splitPiTys $ typeKind saturated_tc)
                 saturated_tc (decomp_args `chkAppend` args) co

    split_apps args (ForAllTy (Anon arg) res) co
      = affix_co (tyConBinders funTyCon) (mkTyConTy funTyCon)
                 (arg : res : args) co
    split_apps args ty co
      = affix_co (fst $ splitPiTys $ typeKind ty)
                 ty args co

    -- having broken everything apart, this figures out the point at which there
    -- are no more dependent quantifications, and puts the cast there
    affix_co _ ty [] co = no_double_casts ty co
    affix_co bndrs ty args co
      -- if kind contains any dependent quantifications, we can't push.
      -- apply arguments until it doesn't
      = let (no_dep_bndrs, some_dep_bndrs) = spanEnd isAnonBinder bndrs
            (some_dep_args, rest_args) = splitAtList some_dep_bndrs args
            dep_subst = zipTyBinderSubst some_dep_bndrs some_dep_args
            used_no_dep_bndrs = takeList rest_args no_dep_bndrs
            rest_arg_tys = substTys dep_subst (map binderType used_no_dep_bndrs)
            co' = mkFunCos Nominal
                           (map (mkReflCo Nominal) rest_arg_tys)
                           co
        in
        ((ty `mkAppTys` some_dep_args) `no_double_casts` co') `mkAppTys` rest_args

    no_double_casts (CastTy ty co1) co2 = CastTy ty (co1 `mkTransCo` co2)
    no_double_casts ty              co  = CastTy ty co

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


                Representation types
                ~~~~~~~~~~~~~~~~~~~~

Note [Nullary unboxed tuple]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We represent the nullary unboxed tuple as the unary (but void) type
Void#.  The reason for this is that the ReprArity is never
less than the Arity (as it would otherwise be for a function type like
(# #) -> Int).

As a result, ReprArity is always strictly positive if Arity is. This
is important because it allows us to distinguish at runtime between a
thunk and a function takes a nullary unboxed tuple as an argument!
-}

type UnaryType = Type

data RepType = UbxTupleRep [UnaryType] -- INVARIANT: never an empty list (see Note [Nullary unboxed tuple])
             | UnaryRep UnaryType

instance Outputable RepType where
  ppr (UbxTupleRep tys) = text "UbxTupleRep" <+> ppr tys
  ppr (UnaryRep ty)     = text "UnaryRep"    <+> ppr ty

flattenRepType :: RepType -> [UnaryType]
flattenRepType (UbxTupleRep tys) = tys
flattenRepType (UnaryRep ty)     = [ty]

-- | Looks through:
--
--      1. For-alls
--      2. Synonyms
--      3. Predicates
--      4. All newtypes, including recursive ones, but not newtype families
--      5. Casts
--
-- It's useful in the back end of the compiler.
repType :: Type -> RepType
repType ty
  = go initRecTc ty
  where
    go :: RecTcChecker -> Type -> RepType
    go rec_nts ty                       -- Expand predicates and synonyms
      | Just ty' <- coreView ty
      = go rec_nts ty'

    go rec_nts (ForAllTy (Named {}) ty2)  -- Drop type foralls
      = go rec_nts ty2

    go rec_nts (TyConApp tc tys)        -- Expand newtypes
      | isNewTyCon tc
      , tys `lengthAtLeast` tyConArity tc
      , Just rec_nts' <- checkRecTc rec_nts tc   -- See Note [Expanding newtypes] in TyCon
      = go rec_nts' (newTyConInstRhs tc tys)

      | isUnboxedTupleTyCon tc
      = if null tys
         then UnaryRep voidPrimTy -- See Note [Nullary unboxed tuple]
         else UbxTupleRep (concatMap (flattenRepType . go rec_nts) non_rr_tys)
      where
          -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
        non_rr_tys = dropRuntimeRepArgs tys

    go rec_nts (CastTy ty _)
      = go rec_nts ty

    go _ ty@(CoercionTy _)
      = pprPanic "repType" (ppr ty)

    go _ ty = UnaryRep ty

-- ToDo: this could be moved to the code generator, using splitTyConApp instead
-- of inspecting the type directly.

-- | Discovers the primitive representation of a more abstract 'UnaryType'
typePrimRep :: UnaryType -> PrimRep
typePrimRep ty = kindPrimRep (typeKind ty)

-- | Find the primitive representation of a 'TyCon'. Defined here to
-- avoid module loops. Call this only on unlifted tycons.
tyConPrimRep :: TyCon -> PrimRep
tyConPrimRep tc = kindPrimRep res_kind
  where
    res_kind = tyConResKind tc

-- | Take a kind (of shape @TYPE rr@) and produce the 'PrimRep' of values
-- of types of this kind.
kindPrimRep :: Kind -> PrimRep
kindPrimRep ki | Just ki' <- coreViewOneStarKind ki = kindPrimRep ki'
kindPrimRep (TyConApp typ [runtime_rep])
  = ASSERT( typ `hasKey` tYPETyConKey )
    go runtime_rep
  where
    go rr | Just rr' <- coreView rr = go rr'
    go (TyConApp rr_dc args)
      | RuntimeRep fun <- tyConRuntimeRepInfo rr_dc
      = fun args
    go rr = pprPanic "kindPrimRep.go" (ppr rr)
kindPrimRep ki = WARN( True
                     , text "kindPrimRep defaulting to PtrRep on" <+> ppr ki )
                 PtrRep  -- this can happen legitimately for, e.g., Any

typeRepArity :: Arity -> Type -> RepArity
typeRepArity 0 _ = 0
typeRepArity n ty = case repType ty of
  UnaryRep (ForAllTy bndr ty) -> length (flattenRepType (repType (binderType bndr))) + typeRepArity (n - 1) ty
  _                           -> pprPanic "typeRepArity: arity greater than type can handle" (ppr (n, ty, repType ty))

isVoidTy :: Type -> Bool
-- True if the type has zero width
isVoidTy ty = case repType ty of
                UnaryRep (TyConApp tc _) -> isUnliftedTyCon tc &&
                                            isVoidRep (tyConPrimRep tc)
                _                        -> False

{-
Note [AppTy rep]
~~~~~~~~~~~~~~~~
Types of the form 'f a' must be of kind *, not #, so we are guaranteed
that they are represented by pointers.  The reason is that f must have
kind (kk -> kk) and kk cannot be unlifted; see Note [The kind invariant]
in TyCoRep.

---------------------------------------------------------------------
                                ForAllTy
                                ~~~~~~~~
-}

mkForAllTy :: TyBinder -> Type -> Type
mkForAllTy = ForAllTy

-- | Make a dependent forall.
mkNamedForAllTy :: TyVar -> VisibilityFlag -> Type -> Type
mkNamedForAllTy tv vis = ASSERT( isTyVar tv )
                         ForAllTy (Named tv vis)

-- | Like mkForAllTys, but assumes all variables are dependent and invisible,
-- a common case
mkInvForAllTys :: [TyVar] -> Type -> Type
mkInvForAllTys tvs = ASSERT( all isTyVar tvs )
                     mkForAllTys (map (flip Named Invisible) tvs)

-- | Like mkForAllTys, but assumes all variables are dependent and specified,
-- a common case
mkSpecForAllTys :: [TyVar] -> Type -> Type
mkSpecForAllTys tvs = ASSERT( all isTyVar tvs )
                      mkForAllTys (map (flip Named Specified) tvs)

-- | Like mkForAllTys, but assumes all variables are dependent and visible
mkVisForAllTys :: [TyVar] -> Type -> Type
mkVisForAllTys tvs = ASSERT( all isTyVar tvs )
                     mkForAllTys (map (flip Named Visible) tvs)

mkPiType  :: Var -> Type -> Type
-- ^ Makes a @(->)@ type or an implicit forall type, depending
-- on whether it is given a type variable or a term variable.
-- This is used, for example, when producing the type of a lambda.
-- Always uses Invisible binders.
mkPiTypes :: [Var] -> Type -> Type
-- ^ 'mkPiType' for multiple type or value arguments

mkPiType v ty
   | isTyVar v = mkForAllTy (Named v Invisible) ty
   | otherwise = mkForAllTy (Anon (varType v)) ty

mkPiTypes vs ty = foldr mkPiType ty vs

-- | Given a list of type-level vars and a result type, makes TyBinders, preferring
-- anonymous binders if the variable is, in fact, not dependent.
-- All binders are /visible/.
mkTyBindersPreferAnon :: [TyVar] -> Type -> [TyBinder]
mkTyBindersPreferAnon vars inner_ty = fst $ go vars inner_ty
  where
    go :: [TyVar] -> Type -> ([TyBinder], VarSet) -- also returns the free vars
    go [] ty = ([], tyCoVarsOfType ty)
    go (v:vs) ty |  v `elemVarSet` fvs
                 = ( Named v Visible : binders
                   , fvs `delVarSet` v `unionVarSet` kind_vars )
                 | otherwise
                 = ( Anon (tyVarKind v) : binders
                   , fvs `unionVarSet` kind_vars )
      where
        (binders, fvs) = go vs ty
        kind_vars      = tyCoVarsOfType $ tyVarKind v

-- | Take a ForAllTy apart, returning the list of tyvars and the result type.
-- This always succeeds, even if it returns only an empty list. Note that the
-- result type returned may have free variables that were bound by a forall.
splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split ty ty []
  where
    split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
    split _       (ForAllTy (Named tv _) ty) tvs = split ty ty (tv:tvs)
    split orig_ty _                          tvs = (reverse tvs, orig_ty)

-- | Split off all TyBinders to a type, splitting both proper foralls
-- and functions
splitPiTys :: Type -> ([TyBinder], Type)
splitPiTys ty = split ty ty []
  where
    split orig_ty ty bs | Just ty' <- coreView ty = split orig_ty ty' bs
    split _       (ForAllTy b res) bs  = split res res (b:bs)
    split orig_ty _                bs  = (reverse bs, orig_ty)

-- | Like 'splitPiTys' but split off only /named/ binders.
splitNamedPiTys :: Type -> ([TyBinder], Type)
splitNamedPiTys ty = split ty ty []
  where
    split orig_ty ty bs | Just ty' <- coreView ty = split orig_ty ty' bs
    split _       (ForAllTy b@(Named {}) res) bs  = split res res (b:bs)
    split orig_ty _                           bs  = (reverse bs, orig_ty)

-- | Checks whether this is a proper forall (with a named binder)
isForAllTy :: Type -> Bool
isForAllTy (ForAllTy (Named {}) _) = True
isForAllTy _                       = False

-- | Is this a function or forall?
isPiTy :: Type -> Bool
isPiTy (ForAllTy {}) = True
isPiTy _             = False

-- | Take a forall type apart, or panics if that is not possible.
splitForAllTy :: Type -> (TyVar, Type)
splitForAllTy ty
  | Just answer <- splitForAllTy_maybe ty = answer
  | otherwise                             = pprPanic "splitForAllTy" (ppr ty)

-- | Attempts to take a forall type apart, but only if it's a proper forall,
-- with a named binder
splitForAllTy_maybe :: Type -> Maybe (TyVar, Type)
splitForAllTy_maybe ty = splitFAT_m ty
  where
    splitFAT_m ty | Just ty' <- coreView ty = splitFAT_m ty'
    splitFAT_m (ForAllTy (Named tv _) ty) = Just (tv, ty)
    splitFAT_m _                          = Nothing

-- | Attempts to take a forall type apart; works with proper foralls and
-- functions
splitPiTy_maybe :: Type -> Maybe (TyBinder, Type)
splitPiTy_maybe ty = go ty
  where
    go ty | Just ty' <- coreView ty = go ty'
    go (ForAllTy bndr ty) = Just (bndr, ty)
    go _                  = Nothing

-- | Takes a forall type apart, or panics
splitPiTy :: Type -> (TyBinder, Type)
splitPiTy ty
  | Just answer <- splitPiTy_maybe ty = answer
  | otherwise                         = pprPanic "splitPiTy" (ppr ty)

-- | Drops all non-anonymous ForAllTys
dropForAlls :: Type -> Type
dropForAlls ty | Just ty' <- coreView ty = dropForAlls ty'
               | otherwise = go ty
  where
    go (ForAllTy (Named {}) res) = go res
    go res                       = res

-- | Given a tycon and its arguments, filters out any invisible arguments
filterOutInvisibleTypes :: TyCon -> [Type] -> [Type]
filterOutInvisibleTypes tc tys = snd $ partitionInvisibles tc id tys

-- | Like 'filterOutInvisibles', but works on 'TyVar's
filterOutInvisibleTyVars :: TyCon -> [TyVar] -> [TyVar]
filterOutInvisibleTyVars tc tvs = snd $ partitionInvisibles tc mkTyVarTy tvs

-- | Given a tycon and a list of things (which correspond to arguments),
-- partitions the things into the invisible ones and the visible ones.
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
    go subst (ForAllTy bndr res_ki) (x:xs)
      | isVisibleBinder bndr = second (x :) (go subst' res_ki xs)
      | otherwise            = first  (x :) (go subst' res_ki xs)
      where
        subst' = extendTvSubstBinder subst bndr (get_ty x)
    go subst (TyVarTy tv) xs
      | Just ki <- lookupTyVar subst tv = go subst ki xs
    go _ _ xs = ([], xs)  -- something is ill-kinded. But this can happen
                          -- when printing errors. Assume everything is visible.

-- like splitPiTys, but returns only *invisible* binders, including constraints
splitPiTysInvisible :: Type -> ([TyBinder], Type)
splitPiTysInvisible ty = split ty ty []
   where
     split orig_ty ty bndrs
       | Just ty' <- coreView ty = split orig_ty ty' bndrs
     split _       (ForAllTy bndr ty) bndrs
       |  isInvisibleBinder bndr
       = split ty ty (bndr:bndrs)

     split orig_ty _ bndrs
       = (reverse bndrs, orig_ty)

applyTysX :: [TyVar] -> Type -> [Type] -> Type
-- applyTyxX beta-reduces (/\tvs. body_ty) arg_tys
-- Assumes that (/\tvs. body_ty) is closed
applyTysX tvs body_ty arg_tys
  = ASSERT2( length arg_tys >= n_tvs, pp_stuff )
    ASSERT2( tyCoVarsOfType body_ty `subVarSet` mkVarSet tvs, pp_stuff )
    mkAppTys (substTyWith tvs (take n_tvs arg_tys) body_ty)
             (drop n_tvs arg_tys)
  where
    pp_stuff = vcat [ppr tvs, ppr body_ty, ppr arg_tys]
    n_tvs = length tvs

{-
%************************************************************************
%*                                                                      *
   TyBinders
%*                                                                      *
%************************************************************************
-}

-- | Make a named binder
mkNamedBinder :: VisibilityFlag -> Var -> TyBinder
mkNamedBinder vis var = Named var vis

-- | Make many named binders
mkNamedBinders :: VisibilityFlag -> [TyVar] -> [TyBinder]
mkNamedBinders vis = map (mkNamedBinder vis)

-- | Make an anonymous binder
mkAnonBinder :: Type -> TyBinder
mkAnonBinder = Anon

-- | Does this binder bind a variable that is /not/ erased? Returns
-- 'True' for anonymous binders.
isIdLikeBinder :: TyBinder -> Bool
isIdLikeBinder (Named {}) = False
isIdLikeBinder (Anon {})  = True

-- | Does this type, when used to the left of an arrow, require
-- a visible argument? This checks to see if the kind of the type
-- is constraint.
isVisibleType :: Type -> Bool
isVisibleType = not . isPredTy

binderVisibility :: TyBinder -> VisibilityFlag
binderVisibility (Named _ vis) = vis
binderVisibility (Anon ty)
  | isVisibleType ty = Visible
  | otherwise        = Invisible

-- | Extract a bound variable in a binder, if any
binderVar_maybe :: TyBinder -> Maybe Var
binderVar_maybe (Named v _) = Just v
binderVar_maybe (Anon {})   = Nothing

-- | Extract a bound variable in a binder, or panics
binderVar :: String   -- ^ printed if there is a panic
          -> TyBinder -> Var
binderVar _ (Named v _) = v
binderVar e (Anon t)    = pprPanic ("binderVar (" ++ e ++ ")") (ppr t)

-- | Extract a relevant type, if there is one.
binderRelevantType_maybe :: TyBinder -> Maybe Type
binderRelevantType_maybe (Named {}) = Nothing
binderRelevantType_maybe (Anon ty)  = Just ty

-- | Like 'maybe', but for binders.
caseBinder :: TyBinder       -- ^ binder to scrutinize
           -> (TyVar -> a) -- ^ named case
           -> (Type -> a)  -- ^ anonymous case
           -> a
caseBinder (Named v _) f _ = f v
caseBinder (Anon t) _ d    = d t

-- | Break apart a list of binders into tyvars and anonymous types.
partitionBinders :: [TyBinder] -> ([TyVar], [Type])
partitionBinders = partitionWith named_or_anon
  where
    named_or_anon bndr = caseBinder bndr Left Right

-- | Break apart a list of binders into a list of named binders and
-- a list of anonymous types.
partitionBindersIntoBinders :: [TyBinder] -> ([TyBinder], [Type])
partitionBindersIntoBinders = partitionWith named_or_anon
  where
    named_or_anon bndr = caseBinder bndr (\_ -> Left bndr) Right

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
    go (ForAllTy (Anon arg) res) []
      | isPredTy arg                = isPredTy res   -- (Eq a => C a)
      | otherwise                   = False          -- (Int -> Bool)
    go (ForAllTy (Named {}) ty) []  = go ty []
    go _ _ = False

    go_tc :: TyCon -> [KindOrType] -> Bool
    go_tc tc args
      | tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey
                  = length args == 4  -- ~# and ~R# sadly have result kind #
                                      -- not Contraint; but we still want
                                      -- isPredTy to reply True.
      | otherwise = go_k (tyConKind tc) args

    go_k :: Kind -> [KindOrType] -> Bool
    -- True <=> ('k' applied to 'kts') = Constraint
    go_k k args = isConstraintKind (piResultTys k args)

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
                   Size
*                                                                      *
************************************************************************
-}

-- NB: This function does not respect `eqType`, in that two types that
-- are `eqType` may return different sizes. This is OK, because this
-- function is used only in reporting, not decision-making.
typeSize :: Type -> Int
typeSize (LitTy {})       = 1
typeSize (TyVarTy {})     = 1
typeSize (AppTy t1 t2)    = typeSize t1 + typeSize t2
typeSize (ForAllTy b t)   = typeSize (binderType b) + typeSize t
typeSize (TyConApp _ ts)  = 1 + sum (map typeSize ts)
typeSize (CastTy ty co)   = typeSize ty + coercionSize co
typeSize (CoercionTy co)  = coercionSize co

{-
%************************************************************************
%*                                                                      *
         Well-scoped tyvars
*                                                                      *
************************************************************************
-}

-- | Do a topological sort on a list of tyvars. This is a deterministic
-- sorting operation (that is, doesn't depend on Uniques).
toposortTyVars :: [TyVar] -> [TyVar]
toposortTyVars tvs = reverse $
                     [ tv | (tv, _, _) <- topologicalSortG $
                                          graphFromEdgedVertices nodes ]
  where
    var_ids :: VarEnv Int
    var_ids = mkVarEnv (zip tvs [1..])

    nodes = [ ( tv
              , lookupVarEnv_NF var_ids tv
              , mapMaybe (lookupVarEnv var_ids)
                         (tyCoVarsOfTypeList (tyVarKind tv)) )
            | tv <- tvs ]

-- | Extract a well-scoped list of variables from a set of variables.
varSetElemsWellScoped :: VarSet -> [Var]
varSetElemsWellScoped = toposortTyVars . varSetElems

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
        fam_subst = ASSERT2( length tvs == length tys, ppr tc <+> ppr tys )
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

{-
************************************************************************
*                                                                      *
\subsection{Liftedness}
*                                                                      *
************************************************************************
-}

-- | See "Type#type_classification" for what an unlifted type is
isUnliftedType :: Type -> Bool
        -- isUnliftedType returns True for forall'd unlifted types:
        --      x :: forall a. Int#
        -- I found bindings like these were getting floated to the top level.
        -- They are pretty bogus types, mind you.  It would be better never to
        -- construct them

isUnliftedType ty | Just ty' <- coreView ty = isUnliftedType ty'
isUnliftedType (ForAllTy (Named {}) ty) = isUnliftedType ty
isUnliftedType (TyConApp tc _)          = isUnliftedTyCon tc
isUnliftedType _                        = False

-- | Extract the RuntimeRep classifier of a type. Panics if this is not possible.
getRuntimeRep :: String   -- ^ Printed in case of an error
              -> Type -> Type
getRuntimeRep err ty = getRuntimeRepFromKind err (typeKind ty)

-- | Extract the RuntimeRep classifier of a type from its kind.
-- For example, getRuntimeRepFromKind * = PtrRepLifted;
--              getRuntimeRepFromKind # = PtrRepUnlifted.
-- Panics if this is not possible.
getRuntimeRepFromKind :: String  -- ^ Printed in case of an error
                      -> Type -> Type
getRuntimeRepFromKind err = go
  where
    go k | Just k' <- coreViewOneStarKind k = go k'
    go k
      | Just (tc, [arg]) <- splitTyConApp_maybe k
      , tc `hasKey` tYPETyConKey
      = arg
    go k = pprPanic "getRuntimeRep" (text err $$
                                     ppr k <+> dcolon <+> ppr (typeKind k))

isUnboxedTupleType :: Type -> Bool
isUnboxedTupleType ty = case tyConAppTyCon_maybe ty of
                           Just tc -> isUnboxedTupleTyCon tc
                           _       -> False

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

-- | Computes whether an argument (or let right hand side) should
-- be computed strictly or lazily, based only on its type.
-- Currently, it's just 'isUnliftedType'.

isStrictType :: Type -> Bool
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
\subsection{Sequencing on types}
*                                                                      *
************************************************************************
-}

seqType :: Type -> ()
seqType (LitTy n)            = n `seq` ()
seqType (TyVarTy tv)         = tv `seq` ()
seqType (AppTy t1 t2)        = seqType t1 `seq` seqType t2
seqType (TyConApp tc tys)    = tc `seq` seqTypes tys
seqType (ForAllTy bndr ty)   = seqType (binderType bndr) `seq` seqType ty
seqType (CastTy ty co)       = seqType ty `seq` seqCo co
seqType (CoercionTy co)      = seqCo co

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
eqType t1 t2 = isEqual $ cmpType t1 t2

-- | Compare types with respect to a (presumably) non-empty 'RnEnv2'.
eqTypeX :: RnEnv2 -> Type -> Type -> Bool
eqTypeX env t1 t2 = isEqual $ cmpTypeX env t1 t2

-- | Type equality on lists of types, looking through type synonyms
-- but not newtypes.
eqTypes :: [Type] -> [Type] -> Bool
eqTypes tys1 tys2 = isEqual $ cmpTypes tys1 tys2

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

cmpType :: Type -> Type -> Ordering
cmpType t1 t2
  -- we know k1 and k2 have the same kind, because they both have kind *.
  = cmpTypeX rn_env t1 t2
  where
    rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfTypes [t1, t2]))

cmpTypes :: [Type] -> [Type] -> Ordering
cmpTypes ts1 ts2 = cmpTypesX rn_env ts1 ts2
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

cmpTypeX :: RnEnv2 -> Type -> Type -> Ordering  -- Main workhorse
    -- See Note [Non-trivial definitional equality] in TyCoRep
cmpTypeX env orig_t1 orig_t2 =
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
      | Just t1' <- coreViewOneStarKind t1 = go env t1' t2
      | Just t2' <- coreViewOneStarKind t2 = go env t1 t2'

    go env (TyVarTy tv1)       (TyVarTy tv2)
      = liftOrdering $ rnOccL env tv1 `compare` rnOccR env tv2
    go env (ForAllTy (Named tv1 _) t1) (ForAllTy (Named tv2 _) t2)
      = go env (tyVarKind tv1) (tyVarKind tv2)
        `thenCmpTy` go (rnBndr2 env tv1 tv2) t1 t2
        -- See Note [Equality on AppTys]
    go env (AppTy s1 t1) ty2
      | Just (s2, t2) <- repSplitAppTy_maybe ty2
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env ty1 (AppTy s2 t2)
      | Just (s1, t1) <- repSplitAppTy_maybe ty1
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env (ForAllTy (Anon s1) t1) (ForAllTy (Anon s2) t2)
      = go env s1 s2 `thenCmpTy` go env t1 t2
    go env (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      = liftOrdering (tc1 `cmpTc` tc2) `thenCmpTy` gos env tys1 tys2
    go _   (LitTy l1)          (LitTy l2)          = liftOrdering (compare l1 l2)
    go env (CastTy t1 _)       t2                  = hasCast $ go env t1 t2
    go env t1                  (CastTy t2 _)       = hasCast $ go env t1 t2
    go _   (CoercionTy {})     (CoercionTy {})     = TEQ

        -- Deal with the rest: TyVarTy < CoercionTy < AppTy < LitTy < TyConApp < ForAllTy
    go _ ty1 ty2
      = liftOrdering $ (get_rank ty1) `compare` (get_rank ty2)
      where get_rank :: Type -> Int
            get_rank (CastTy {})
              = pprPanic "cmpTypeX.get_rank" (ppr [ty1,ty2])
            get_rank (TyVarTy {})            = 0
            get_rank (CoercionTy {})         = 1
            get_rank (AppTy {})              = 3
            get_rank (LitTy {})              = 4
            get_rank (TyConApp {})           = 5
            get_rank (ForAllTy (Anon {}) _)  = 6
            get_rank (ForAllTy (Named {}) _) = 7

    gos :: RnEnv2 -> [Type] -> [Type] -> TypeOrdering
    gos _   []         []         = TEQ
    gos _   []         _          = TLT
    gos _   _          []         = TGT
    gos env (ty1:tys1) (ty2:tys2) = go env ty1 ty2 `thenCmpTy` gos env tys1 tys2

-------------
cmpTypesX :: RnEnv2 -> [Type] -> [Type] -> Ordering
cmpTypesX _   []        []        = EQ
cmpTypesX env (t1:tys1) (t2:tys2) = cmpTypeX env t1 t2 `thenCmp` cmpTypesX env tys1 tys2
cmpTypesX _   []        _         = LT
cmpTypesX _   _         []        = GT

-------------
-- | Compare two 'TyCon's. NB: This should /never/ see the "star synonyms",
-- as recognized by Kind.isStarKindSynonymTyCon. See Note
-- [Kind Constraint and kind *] in Kind.
cmpTc :: TyCon -> TyCon -> Ordering
cmpTc tc1 tc2
  = ASSERT( not (isStarKindSynonymTyCon tc1) && not (isStarKindSynonymTyCon tc2) )
    u1 `compare` u2
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
typeKind (ForAllTy (Anon _) _) = liftedTypeKind
typeKind (ForAllTy _ ty)       = typeKind ty
typeKind (TyVarTy tyvar)       = tyVarKind tyvar
typeKind (CastTy _ty co)       = pSnd $ coercionKind co
typeKind (CoercionTy co)       = coercionType co

typeLiteralKind :: TyLit -> Kind
typeLiteralKind l =
  case l of
    NumTyLit _ -> typeNatKind
    StrTyLit _ -> typeSymbolKind

-- | Print a tyvar with its kind
pprTyVar :: TyVar -> SDoc
pprTyVar tv = ppr tv <+> dcolon <+> ppr (tyVarKind tv)

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
tyConsOfType :: Type -> NameEnv TyCon
tyConsOfType ty
  = go ty
  where
     go :: Type -> NameEnv TyCon  -- The NameEnv does duplicate elim
     go ty | Just ty' <- coreView ty = go ty'
     go (TyVarTy {})               = emptyNameEnv
     go (LitTy {})                 = emptyNameEnv
     go (TyConApp tc tys)          = go_tc tc `plusNameEnv` go_s tys
     go (AppTy a b)                = go a `plusNameEnv` go b
     go (ForAllTy (Anon a) b)      = go a `plusNameEnv` go b `plusNameEnv` go_tc funTyCon
     go (ForAllTy (Named tv _) ty) = go ty `plusNameEnv` go (tyVarKind tv)
     go (CastTy ty co)             = go ty `plusNameEnv` go_co co
     go (CoercionTy co)            = go_co co

     go_co (Refl _ ty)             = go ty
     go_co (TyConAppCo _ tc args)  = go_tc tc `plusNameEnv` go_cos args
     go_co (AppCo co arg)          = go_co co `plusNameEnv` go_co arg
     go_co (ForAllCo _ kind_co co) = go_co kind_co `plusNameEnv` go_co co
     go_co (CoVarCo {})            = emptyNameEnv
     go_co (AxiomInstCo ax _ args) = go_ax ax `plusNameEnv` go_cos args
     go_co (UnivCo p _ t1 t2)      = go_prov p `plusNameEnv` go t1 `plusNameEnv` go t2
     go_co (SymCo co)              = go_co co
     go_co (TransCo co1 co2)       = go_co co1 `plusNameEnv` go_co co2
     go_co (NthCo _ co)            = go_co co
     go_co (LRCo _ co)             = go_co co
     go_co (InstCo co arg)         = go_co co `plusNameEnv` go_co arg
     go_co (CoherenceCo co1 co2)   = go_co co1 `plusNameEnv` go_co co2
     go_co (KindCo co)             = go_co co
     go_co (SubCo co)              = go_co co
     go_co (AxiomRuleCo _ cs)      = go_cos cs

     go_prov UnsafeCoerceProv    = emptyNameEnv
     go_prov (PhantomProv co)    = go_co co
     go_prov (ProofIrrelProv co) = go_co co
     go_prov (PluginProv _)      = emptyNameEnv
     go_prov (HoleProv _)        = emptyNameEnv
        -- this last case can happen from the tyConsOfType used from
        -- checkTauTvUpdate

     go_s tys     = foldr (plusNameEnv . go)     emptyNameEnv tys
     go_cos cos   = foldr (plusNameEnv . go_co)  emptyNameEnv cos

     go_tc tc = unitNameEnv (tyConName tc) tc
     go_ax ax = go_tc $ coAxiomTyCon ax

-- | Find the result 'Kind' of a type synonym,
-- after applying it to its 'arity' number of type variables
-- Actually this function works fine on data types too,
-- but they'd always return '*', so we never need to ask
synTyConResKind :: TyCon -> Kind
synTyConResKind tycon = piResultTys (tyConKind tycon) (mkTyVarTys (tyConTyVars tycon))

-- | Retrieve the free variables in this type, splitting them based
-- on whether the variable was used in a dependent context.
-- (This isn't the most precise analysis, because
-- it's used in the typechecking knot. It might list some dependent
-- variables as also non-dependent.)
splitDepVarsOfType :: Type -> Pair TyCoVarSet
splitDepVarsOfType ty = Pair dep_vars final_nondep_vars
  where
    Pair dep_vars nondep_vars = split_dep_vars ty
    final_nondep_vars = nondep_vars `minusVarSet` dep_vars

-- | Like 'splitDepVarsOfType', but over a list of types
splitDepVarsOfTypes :: [Type] -> Pair TyCoVarSet
splitDepVarsOfTypes tys = Pair dep_vars final_nondep_vars
  where
    Pair dep_vars nondep_vars = foldMap split_dep_vars tys
    final_nondep_vars = nondep_vars `minusVarSet` dep_vars

-- | Worker for 'splitDepVarsOfType'. This might output the same var
-- in both sets, if it's used in both a type and a kind.
split_dep_vars :: Type -> Pair TyCoVarSet
split_dep_vars = go
  where
    go (TyVarTy tv)              = Pair (tyCoVarsOfType $ tyVarKind tv)
                                        (unitVarSet tv)
    go (AppTy t1 t2)             = go t1 `mappend` go t2
    go (TyConApp _ tys)          = foldMap go tys
    go (ForAllTy (Anon arg) res) = go arg `mappend` go res
    go (ForAllTy (Named tv _) ty)
      = let Pair kvs tvs = go ty in
        Pair (kvs `delVarSet` tv `unionVarSet` tyCoVarsOfType (tyVarKind tv))
             (tvs `delVarSet` tv)
    go (LitTy {})                = mempty
    go (CastTy ty co)            = go ty `mappend` Pair (tyCoVarsOfCo co)
                                                        emptyVarSet
    go (CoercionTy co)           = go_co co

    go_co co = let Pair ty1 ty2 = coercionKind co in
               go ty1 `mappend` go ty2  -- NB: the Pairs separate along different
                                        -- dimensions here. Be careful!

-- | Retrieve the free variables in this type, splitting them based
-- on whether they are used visibly or invisibly. Invisible ones come
-- first.
splitVisVarsOfType :: Type -> Pair TyCoVarSet
splitVisVarsOfType orig_ty = Pair invis_vars vis_vars
  where
    Pair invis_vars1 vis_vars = go orig_ty
    invis_vars = invis_vars1 `minusVarSet` vis_vars

    go (TyVarTy tv)  = Pair (tyCoVarsOfType $ tyVarKind tv) (unitVarSet tv)
    go (AppTy t1 t2) = go t1 `mappend` go t2
    go (TyConApp tc tys) = go_tc tc tys
    go (ForAllTy (Anon t1) t2) = go t1 `mappend` go t2
    go (ForAllTy (Named tv _) ty)
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
