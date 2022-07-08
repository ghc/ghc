
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Types used in the typechecker
--
-- This module provides the Type interface for front-end parts of the
-- compiler.  These parts
--
-- * treat "source types" as opaque:
--         newtypes, and predicates are meaningful.
-- * look through usage types
--
module GHC.Tc.Utils.TcType (
  --------------------------------
  -- Types
  TcType, TcSigmaType, TcTypeFRR, TcSigmaTypeFRR,
  TcRhoType, TcTauType, TcPredType, TcThetaType,
  TcTyVar, TcTyVarSet, TcDTyVarSet, TcTyCoVarSet, TcDTyCoVarSet,
  TcKind, TcCoVar, TcTyCoVar, TcTyVarBinder, TcInvisTVBinder, TcReqTVBinder,
  TcTyCon, MonoTcTyCon, PolyTcTyCon, TcTyConBinder, KnotTied,

  ExpType(..), InferResult(..),
  ExpTypeFRR, ExpSigmaType, ExpSigmaTypeFRR,
  ExpRhoType,
  mkCheckExpType,

  SyntaxOpType(..), synKnownType, mkSynFunTys,

  -- TcLevel
  TcLevel(..), topTcLevel, pushTcLevel, isTopTcLevel,
  strictlyDeeperThan, deeperThanOrSame, sameDepthAs,
  tcTypeLevel, tcTyVarLevel, maxTcLevel,
  --------------------------------
  -- MetaDetails
  TcTyVarDetails(..), pprTcTyVarDetails, vanillaSkolemTvUnk,
  MetaDetails(Flexi, Indirect), MetaInfo(..), skolemSkolInfo,
  isImmutableTyVar, isSkolemTyVar, isMetaTyVar,  isMetaTyVarTy, isTyVarTy,
  tcIsTcTyVar, isTyVarTyVar, isOverlappableTyVar,  isTyConableTyVar,
  ConcreteTvOrigin(..), isConcreteTyVar_maybe, isConcreteTyVar,
  isConcreteTyVarTy, isConcreteTyVarTy_maybe,
  isAmbiguousTyVar, isCycleBreakerTyVar, metaTyVarRef, metaTyVarInfo,
  isFlexi, isIndirect, isRuntimeUnkSkol,
  metaTyVarTcLevel, setMetaTyVarTcLevel, metaTyVarTcLevel_maybe,
  isTouchableMetaTyVar, isPromotableMetaTyVar,
  findDupTyVarTvs, mkTyVarNamePairs,

  --------------------------------
  -- Builders
  mkPhiTy, mkInfSigmaTy, mkSpecSigmaTy, mkSigmaTy,
  mkTcAppTy, mkTcAppTys, mkTcCastTy,

  --------------------------------
  -- Splitters
  -- These are important because they do not look through newtypes
  getTyVar,
  tcSplitForAllTyVarBinder_maybe,
  tcSplitForAllTyVars, tcSplitForAllInvisTyVars, tcSplitSomeForAllTyVars,
  tcSplitForAllReqTVBinders, tcSplitForAllInvisTVBinders,
  tcSplitPiTys, tcSplitPiTy_maybe, tcSplitForAllTyVarBinders,
  tcSplitPhiTy, tcSplitPredFunTy_maybe,
  tcSplitFunTy_maybe, tcSplitFunTys, tcFunArgTy, tcFunResultTy, tcFunResultTyN,
  tcSplitFunTysN,
  tcSplitTyConApp, tcSplitTyConApp_maybe,
  tcTyConAppTyCon, tcTyConAppTyCon_maybe, tcTyConAppArgs,
  tcSplitAppTy_maybe, tcSplitAppTy, tcSplitAppTys, tcRepSplitAppTy_maybe,
  tcRepGetNumAppTys,
  tcGetCastedTyVar_maybe, tcGetTyVar_maybe, tcGetTyVar,
  tcSplitSigmaTy, tcSplitNestedSigmaTys,

  ---------------------------------
  -- Predicates.
  -- Again, newtypes are opaque
  eqType, eqTypes, nonDetCmpType, nonDetCmpTypes, eqTypeX,
  pickyEqType, tcEqType, tcEqKind, tcEqTypeNoKindCheck, tcEqTypeVis,
  tcEqTyConApps,
  isSigmaTy, isRhoTy, isRhoExpTy, isOverloadedTy,
  isFloatingPrimTy, isDoubleTy, isFloatTy, isIntTy, isWordTy, isStringTy,
  isIntegerTy, isNaturalTy,
  isBoolTy, isUnitTy, isCharTy,
  isTauTy, isTauTyCon, tcIsTyVarTy, tcIsForAllTy,
  isPredTy, isTyVarClassPred,
  checkValidClsArgs, hasTyVarHead,
  isRigidTy,

  ---------------------------------
  -- Misc type manipulators

  deNoteType,
  orphNamesOfType, orphNamesOfCo,
  orphNamesOfTypes, orphNamesOfCoCon,
  getDFunTyKey, evVarPred,
  ambigTkvsOfTy,

  ---------------------------------
  -- Predicate types
  mkMinimalBySCs, transSuperClasses,
  pickCapturedPreds,
  immSuperClasses, boxEqPred,
  isImprovementPred,

  -- * Finding type instances
  tcTyFamInsts, tcTyFamInstsAndVis, tcTyConAppTyFamInstsAndVis, isTyFamFree,

  -- * Finding "exact" (non-dead) type variables
  exactTyCoVarsOfType, exactTyCoVarsOfTypes,
  anyRewritableTyVar, anyRewritableTyFamApp,

  ---------------------------------
  -- Foreign import and export
  IllegalForeignTypeReason(..),
  TypeCannotBeMarshaledReason(..),
  isFFIArgumentTy,     -- :: DynFlags -> Safety -> Type -> Bool
  isFFIImportResultTy, -- :: DynFlags -> Type -> Bool
  isFFIExportResultTy, -- :: Type -> Bool
  isFFIExternalTy,     -- :: Type -> Bool
  isFFIDynTy,          -- :: Type -> Type -> Bool
  isFFIPrimArgumentTy, -- :: DynFlags -> Type -> Bool
  isFFIPrimResultTy,   -- :: DynFlags -> Type -> Bool
  isFFILabelTy,        -- :: Type -> Bool
  isFunPtrTy,          -- :: Type -> Bool
  tcSplitIOType_maybe, -- :: Type -> Maybe Type

  --------------------------------
  -- Reexported from Kind
  Kind, tcTypeKind,
  liftedTypeKind,
  constraintKind,
  isLiftedTypeKind, isUnliftedTypeKind, classifiesTypeWithValues,

  --------------------------------
  -- Reexported from Type
  Type, PredType, ThetaType, TyCoBinder,
  ArgFlag(..), AnonArgFlag(..),

  mkForAllTy, mkForAllTys, mkInvisForAllTys, mkTyCoInvForAllTys,
  mkSpecForAllTys, mkTyCoInvForAllTy,
  mkInfForAllTy, mkInfForAllTys,
  mkVisFunTy, mkVisFunTys, mkInvisFunTy, mkInvisFunTyMany,
  mkVisFunTyMany, mkVisFunTysMany, mkInvisFunTysMany,
  mkTyConApp, mkAppTy, mkAppTys,
  mkTyConTy, mkTyVarTy, mkTyVarTys,
  mkTyCoVarTy, mkTyCoVarTys,

  isClassPred, isEqPrimPred, isIPLikePred, isEqPred, isEqPredClass,
  mkClassPred,
  tcSplitDFunTy, tcSplitDFunHead, tcSplitMethodTy,
  isRuntimeRepVar, isFixedRuntimeRepKind,
  isVisibleBinder, isInvisibleBinder,

  -- Type substitutions
  TCvSubst(..),         -- Representation visible to a few friends
  TvSubstEnv, emptyTCvSubst, mkEmptyTCvSubst,
  zipTvSubst,
  mkTvSubstPrs, notElemTCvSubst, unionTCvSubst,
  getTvSubstEnv, setTvSubstEnv, getTCvInScope, extendTCvInScope,
  extendTCvInScopeList, extendTCvInScopeSet, extendTvSubstAndInScope,
  Type.lookupTyVar, Type.extendTCvSubst, Type.substTyVarBndr,
  Type.extendTvSubst,
  isInScope, mkTCvSubst, mkTvSubst, zipTyEnv, zipCoEnv,
  Type.substTy, substTys, substScaledTys, substTyWith, substTyWithCoVars,
  substTyAddInScope,
  substTyUnchecked, substTysUnchecked, substScaledTyUnchecked,
  substThetaUnchecked,
  substTyWithUnchecked,
  substCoUnchecked, substCoWithUnchecked,
  substTheta,

  isUnliftedType,       -- Source types are always lifted
  isUnboxedTupleType,   -- Ditto
  isPrimitiveType,

  tcView, coreView,

  tyCoVarsOfType, tyCoVarsOfTypes, closeOverKinds,
  tyCoFVsOfType, tyCoFVsOfTypes,
  tyCoVarsOfTypeDSet, tyCoVarsOfTypesDSet, closeOverKindsDSet,
  tyCoVarsOfTypeList, tyCoVarsOfTypesList,
  noFreeVarsOfType,

  --------------------------------
  pprKind, pprParendKind, pprSigmaType,
  pprType, pprParendType, pprTypeApp,
  pprTheta, pprParendTheta, pprThetaArrowTy, pprClassPred,
  pprTCvBndr, pprTCvBndrs,

  TypeSize, sizeType, sizeTypes, scopedSort,

  ---------------------------------
  -- argument visibility
  tcTyConVisibilities, isNextTyConArgVisible, isNextArgVisible

  ) where

-- friends:
import GHC.Prelude

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Subst ( mkTvSubst, substTyWithCoVars )
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr
import GHC.Core.Class
import GHC.Types.Var
import GHC.Types.ForeignCall
import GHC.Types.Var.Set
import GHC.Core.Coercion
import GHC.Core.Type as Type
import GHC.Core.Predicate
import GHC.Types.RepType
import GHC.Core.TyCon

import {-# SOURCE #-} GHC.Tc.Types.Origin
  ( SkolemInfo, unkSkol
  , FixedRuntimeRepOrigin, FixedRuntimeRepContext )

-- others:
import GHC.Driver.Session
import GHC.Core.FVs
import GHC.Types.Name as Name
            -- We use this to make dictionaries for type literals.
            -- Perhaps there's a better way to do this?
import GHC.Types.Name.Set
import GHC.Types.Var.Env
import GHC.Builtin.Names
import GHC.Builtin.Types ( coercibleClass, eqClass, heqClass, unitTyCon, unitTyConKey
                         , listTyCon, constraintKind )
import GHC.Types.Basic
import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Data.List.SetOps ( getNth, findDupsEq )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Error( Validity'(..) )
import qualified GHC.LanguageExtensions as LangExt

import Data.IORef
import Data.List.NonEmpty( NonEmpty(..) )
import Data.List ( partition )


{-
************************************************************************
*                                                                      *
              Types
*                                                                      *
************************************************************************

The type checker divides the generic Type world into the
following more structured beasts:

sigma ::= forall tyvars. phi
        -- A sigma type is a qualified type
        --
        -- Note that even if 'tyvars' is empty, theta
        -- may not be: e.g.   (?x::Int) => Int

        -- Note that 'sigma' is in prenex form:
        -- all the foralls are at the front.
        -- A 'phi' type has no foralls to the right of
        -- an arrow

phi :: theta => rho

rho ::= sigma -> rho
     |  tau

-- A 'tau' type has no quantification anywhere
-- Note that the args of a type constructor must be taus
tau ::= tyvar
     |  tycon tau_1 .. tau_n
     |  tau_1 tau_2
     |  tau_1 -> tau_2

-- In all cases, a (saturated) type synonym application is legal,
-- provided it expands to the required form.

Note [TcTyVars and TyVars in the typechecker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker uses a lot of type variables with special properties,
notably being a unification variable with a mutable reference.  These
use the 'TcTyVar' variant of Var.Var.

Note, though, that a /bound/ type variable can (and probably should)
be a TyVar.  E.g
    forall a. a -> a
Here 'a' is really just a deBruijn-number; it certainly does not have
a significant TcLevel (as every TcTyVar does).  So a forall-bound type
variable should be TyVars; and hence a TyVar can appear free in a TcType.

The type checker and constraint solver can also encounter /free/ type
variables that use the 'TyVar' variant of Var.Var, for a couple of
reasons:

  - When typechecking a class decl, say
       class C (a :: k) where
          foo :: T a -> Int
    We have first kind-check the header; fix k and (a:k) to be
    TyVars, bring 'k' and 'a' into scope, and kind check the
    signature for 'foo'.  In doing so we call solveEqualities to
    solve any kind equalities in foo's signature.  So the solver
    may see free occurrences of 'k'.

    See calls to tcExtendTyVarEnv for other places that ordinary
    TyVars are bought into scope, and hence may show up in the types
    and kinds generated by GHC.Tc.Gen.HsType.

  - The pattern-match overlap checker calls the constraint solver,
    long after TcTyVars have been zonked away

It's convenient to simply treat these TyVars as skolem constants,
which of course they are.  We give them a level number of "outermost",
so they behave as global constants.  Specifically:

* Var.tcTyVarDetails succeeds on a TyVar, returning
  vanillaSkolemTv, as well as on a TcTyVar.

* tcIsTcTyVar returns True for both TyVar and TcTyVar variants
  of Var.Var.  The "tc" prefix means "a type variable that can be
  encountered by the typechecker".

This is a bit of a change from an earlier era when we remoselessly
insisted on real TcTyVars in the type checker.  But that seems
unnecessary (for skolems, TyVars are fine) and it's now very hard
to guarantee, with the advent of kind equalities.

Note [Coercion variables in free variable lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are several places in the GHC codebase where functions like
tyCoVarsOfType, tyCoVarsOfCt, et al. are used to compute the free type
variables of a type. The "Co" part of these functions' names shouldn't be
dismissed, as it is entirely possible that they will include coercion variables
in addition to type variables! As a result, there are some places in GHC.Tc.Utils.TcType
where we must take care to check that a variable is a _type_ variable (using
isTyVar) before calling tcTyVarDetails--a partial function that is not defined
for coercion variables--on the variable. Failing to do so led to
GHC #12785.
-}

-- See Note [TcTyVars and TyVars in the typechecker]
type TcCoVar = CoVar    -- Used only during type inference
type TcType = Type      -- A TcType can have mutable type variables
type TcTyCoVar = Var    -- Either a TcTyVar or a CoVar

-- | A type which has a syntactically fixed RuntimeRep as per
-- Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete.
type TcTypeFRR = TcType
  -- TODO: consider making this a newtype.

type TcTyVarBinder     = TyVarBinder
type TcInvisTVBinder   = InvisTVBinder
type TcReqTVBinder     = ReqTVBinder

-- See Note [TcTyCon, MonoTcTyCon, and PolyTcTyCon]
type TcTyCon       = TyCon
type MonoTcTyCon   = TcTyCon
type PolyTcTyCon   = TcTyCon
type TcTyConBinder = TyConBinder -- With skolem TcTyVars

-- These types do not have boxy type variables in them
type TcPredType     = PredType
type TcThetaType    = ThetaType
type TcSigmaType    = TcType

-- | A 'TcSigmaTypeFRR' is a 'TcSigmaType' which has a syntactically
--  fixed 'RuntimeRep' in the sense of Note [Fixed RuntimeRep]
-- in GHC.Tc.Utils.Concrete.
--
-- In particular, this means that:
--
-- - 'GHC.Types.RepType.typePrimRep' does not panic,
-- - 'GHC.Core.typeLevity_maybe' does not return 'Nothing'.
--
-- This property is important in functions such as 'matchExpectedFunTys', where
-- we want to provide argument types which have a known runtime representation.
-- See Note [Return arguments with a fixed RuntimeRep.
type TcSigmaTypeFRR = TcSigmaType
    -- TODO: consider making this a newtype.

type TcRhoType      = TcType  -- Note [TcRhoType]
type TcTauType      = TcType
type TcKind         = Kind
type TcTyVarSet     = TyVarSet
type TcTyCoVarSet   = TyCoVarSet
type TcDTyVarSet    = DTyVarSet
type TcDTyCoVarSet  = DTyCoVarSet

{- Note [TcTyCon, MonoTcTyCon, and PolyTcTyCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [How TcTyCons work] in GHC.Tc.TyCl

Invariants:

* TcTyCon: a TyCon built with the TcTyCon constructor

* TcTyConBinder: a TyConBinder with a TcTyVar inside (not a TyVar)

* TcTyCons contain TcTyVars

* MonoTcTyCon:
  - Flag tcTyConIsPoly = False

  - tyConScopedTyVars is important; maps a Name to a TyVarTv unification variable
    The order is important: Specified then Required variables.   E.g. in
        data T a (b :: k) = ...
    the order will be [k, a, b].

    NB: There are no Inferred binders in tyConScopedTyVars; 'a' may
    also be poly-kinded, but that kind variable will be added by
    generaliseTcTyCon, in the passage to a PolyTcTyCon.

  - tyConBinders are irrelevant; we just use tcTyConScopedTyVars
    Well not /quite/ irrelevant: its length gives the number of Required binders,
    and so allows up to distinguish between the Specified and Required elements of
    tyConScopedTyVars.

* PolyTcTyCon:
  - Flag tcTyConIsPoly = True; this is used only to short-cut zonking

  - tyConBinders are still TcTyConBinders, but they are /skolem/ TcTyVars,
    with fixed kinds: no unification variables here

    tyConBinders includes the Inferred binders if any

    tyConBinders uses the Names from the original, renamed program.

  - tcTyConScopedTyVars is irrelevant: just use (binderVars tyConBinders)
    All the types have been swizzled back to use the original Names
    See Note [tyConBinders and lexical scoping] in GHC.Core.TyCon

-}

{- *********************************************************************
*                                                                      *
          ExpType: an "expected type" in the type checker
*                                                                      *
********************************************************************* -}

-- | An expected type to check against during type-checking.
-- See Note [ExpType] in "GHC.Tc.Utils.TcMType", where you'll also find manipulators.
data ExpType = Check TcType
             | Infer !InferResult

data InferResult
  = IR { ir_uniq :: Unique
          -- ^ This 'Unique' is for debugging only

       , ir_lvl  :: TcLevel
         -- ^ See Note [TcLevel of ExpType] in GHC.Tc.Utils.TcMType

       , ir_frr  :: Maybe FixedRuntimeRepContext
         -- ^ See Note [FixedRuntimeRep context in ExpType] in GHC.Tc.Utils.TcMType

       , ir_ref  :: IORef (Maybe TcType) }
         -- ^ The type that fills in this hole should be a @Type@,
         -- that is, its kind should be @TYPE rr@ for some @rr :: RuntimeRep@.
         --
         -- Additionally, if the 'ir_frr' field is @Just frr_orig@ then
         -- @rr@ must be concrete, in the sense of Note [Concrete types]
         -- in GHC.Tc.Utils.Concrete.

type ExpSigmaType    = ExpType

-- | An 'ExpType' which has a fixed RuntimeRep.
--
-- For a 'Check' 'ExpType', the stored 'TcType' must have
-- a fixed RuntimeRep. For an 'Infer' 'ExpType', the 'ir_frr'
-- field must be of the form @Just frr_orig@.
type ExpTypeFRR      = ExpType

-- | Like 'TcSigmaTypeFRR', but for an expected type.
--
-- See 'ExpTypeFRR'.
type ExpSigmaTypeFRR = ExpTypeFRR
  -- TODO: consider making this a newtype.

type ExpRhoType      = ExpType

instance Outputable ExpType where
  ppr (Check ty) = text "Check" <> braces (ppr ty)
  ppr (Infer ir) = ppr ir

instance Outputable InferResult where
  ppr (IR { ir_uniq = u, ir_lvl = lvl, ir_frr = mb_frr })
    = text "Infer" <> mb_frr_text <> braces (ppr u <> comma <> ppr lvl)
    where
      mb_frr_text = case mb_frr of
        Just _  -> text "FRR"
        Nothing -> empty

-- | Make an 'ExpType' suitable for checking.
mkCheckExpType :: TcType -> ExpType
mkCheckExpType = Check


{- *********************************************************************
*                                                                      *
          SyntaxOpType
*                                                                      *
********************************************************************* -}

-- | What to expect for an argument to a rebindable-syntax operator.
-- Quite like 'Type', but allows for holes to be filled in by tcSyntaxOp.
-- The callback called from tcSyntaxOp gets a list of types; the meaning
-- of these types is determined by a left-to-right depth-first traversal
-- of the 'SyntaxOpType' tree. So if you pass in
--
-- > SynAny `SynFun` (SynList `SynFun` SynType Int) `SynFun` SynAny
--
-- you'll get three types back: one for the first 'SynAny', the /element/
-- type of the list, and one for the last 'SynAny'. You don't get anything
-- for the 'SynType', because you've said positively that it should be an
-- Int, and so it shall be.
--
-- You'll also get three multiplicities back: one for each function arrow. See
-- also Note [Linear types] in Multiplicity.
--
-- This is defined here to avoid defining it in "GHC.Tc.Gen.Expr" boot file.
data SyntaxOpType
  = SynAny     -- ^ Any type
  | SynRho     -- ^ A rho type, skolemised or instantiated as appropriate
  | SynList    -- ^ A list type. You get back the element type of the list
  | SynFun SyntaxOpType SyntaxOpType
               -- ^ A function.
  | SynType ExpType   -- ^ A known type.
infixr 0 `SynFun`

-- | Like 'SynType' but accepts a regular TcType
synKnownType :: TcType -> SyntaxOpType
synKnownType = SynType . mkCheckExpType

-- | Like 'mkFunTys' but for 'SyntaxOpType'
mkSynFunTys :: [SyntaxOpType] -> ExpType -> SyntaxOpType
mkSynFunTys arg_tys res_ty = foldr SynFun (SynType res_ty) arg_tys


{-
Note [TcRhoType]
~~~~~~~~~~~~~~~~
A TcRhoType has no foralls or contexts at the top
  NO     forall a. a ->  Int
  NO     Eq a => a -> a
  YES    a -> a
  YES    (forall a. a->a) -> Int
  YES    Int -> forall a. a -> Int


************************************************************************
*                                                                      *
        TyVarDetails, MetaDetails, MetaInfo
*                                                                      *
************************************************************************

TyVarDetails gives extra info about type variables, used during type
checking.  It's attached to mutable type variables only.
It's knot-tied back to "GHC.Types.Var".  There is no reason in principle
why "GHC.Types.Var" shouldn't actually have the definition, but it "belongs" here.

Note [TyVars and TcTyVars during type checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Var type has constructors TyVar and TcTyVar.  They are used
as follows:

* TcTyVar: used /only/ during type checking.  Should never appear
  afterwards.  May contain a mutable field, in the MetaTv case.

* TyVar: is never seen by the constraint solver, except locally
  inside a type like (forall a. [a] ->[a]), where 'a' is a TyVar.
  We instantiate these with TcTyVars before exposing the type
  to the constraint solver.

I have swithered about the latter invariant, excluding TyVars from the
constraint solver.  It's not strictly essential, and indeed
(historically but still there) Var.tcTyVarDetails returns
vanillaSkolemTv for a TyVar.

But ultimately I want to seeparate Type from TcType, and in that case
we would need to enforce the separation.
-}

-- A TyVarDetails is inside a TyVar
-- See Note [TyVars and TcTyVars]
data TcTyVarDetails
  = SkolemTv      -- A skolem
       SkolemInfo
       TcLevel    -- Level of the implication that binds it
                  -- See GHC.Tc.Utils.Unify Note [Deeper level on the left] for
                  --     how this level number is used
       Bool       -- True <=> this skolem type variable can be overlapped
                  --          when looking up instances
                  -- See Note [Binding when looking up instances] in GHC.Core.InstEnv

  | RuntimeUnk    -- Stands for an as-yet-unknown type in the GHCi
                  -- interactive context

  | MetaTv { mtv_info  :: MetaInfo
           , mtv_ref   :: IORef MetaDetails
           , mtv_tclvl :: TcLevel }  -- See Note [TcLevel invariants]

vanillaSkolemTvUnk :: HasCallStack => TcTyVarDetails
vanillaSkolemTvUnk = SkolemTv unkSkol topTcLevel False

instance Outputable TcTyVarDetails where
  ppr = pprTcTyVarDetails

pprTcTyVarDetails :: TcTyVarDetails -> SDoc
-- For debugging
pprTcTyVarDetails (RuntimeUnk {})      = text "rt"
pprTcTyVarDetails (SkolemTv _sk lvl True)  = text "ssk" <> colon <> ppr lvl
pprTcTyVarDetails (SkolemTv _sk lvl False) = text "sk"  <> colon <> ppr lvl
pprTcTyVarDetails (MetaTv { mtv_info = info, mtv_tclvl = tclvl })
  = ppr info <> colon <> ppr tclvl

-----------------------------
data MetaDetails
  = Flexi  -- Flexi type variables unify to become Indirects
  | Indirect TcType

-- | What restrictions are on this metavariable around unification?
-- These are checked in GHC.Tc.Utils.Unify.startSolvingByUnification.
data MetaInfo
   = TauTv         -- ^ This MetaTv is an ordinary unification variable
                   -- A TauTv is always filled in with a tau-type, which
                   -- never contains any ForAlls.

   | TyVarTv       -- ^ A variant of TauTv, except that it should not be
                   --   unified with a type, only with a type variable
                   -- See Note [TyVarTv] in GHC.Tc.Utils.TcMType

   | RuntimeUnkTv  -- ^ A unification variable used in the GHCi debugger.
                   -- It /is/ allowed to unify with a polytype, unlike TauTv

   | CycleBreakerTv  -- Used to fix occurs-check problems in Givens
                     -- See Note [Type equality cycles] in
                     -- GHC.Tc.Solver.Canonical

   | ConcreteTv ConcreteTvOrigin
        -- ^ A unification variable that can only be unified
        -- with a concrete type, in the sense of
        -- Note [Concrete types] in GHC.Tc.Utils.Concrete.
        -- See Note [ConcreteTv] in GHC.Tc.Utils.Concrete.
        -- See also Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete
        -- for an overview of how this works in context.

instance Outputable MetaDetails where
  ppr Flexi         = text "Flexi"
  ppr (Indirect ty) = text "Indirect" <+> ppr ty

instance Outputable MetaInfo where
  ppr TauTv           = text "tau"
  ppr TyVarTv         = text "tyv"
  ppr RuntimeUnkTv    = text "rutv"
  ppr CycleBreakerTv  = text "cbv"
  ppr (ConcreteTv {}) = text "conc"

-- | What caused us to create a 'ConcreteTv' metavariable?
-- See Note [ConcreteTv] in GHC.Tc.Utils.Concrete.
data ConcreteTvOrigin
   -- | A 'ConcreteTv' used to enforce the representation-polymorphism invariants.
   --
   -- See 'FixedRuntimeRepOrigin' for more information.
  = ConcreteFRR FixedRuntimeRepOrigin

{- *********************************************************************
*                                                                      *
                Untouchable type variables
*                                                                      *
********************************************************************* -}

newtype TcLevel = TcLevel Int deriving( Eq, Ord )
  -- See Note [TcLevel invariants] for what this Int is
  -- See also Note [TcLevel assignment]

{-
Note [TcLevel invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~
* Each unification variable (MetaTv)
  and skolem (SkolemTv)
  and each Implication
  has a level number (of type TcLevel)

* INVARIANTS.  In a tree of Implications,

    (ImplicInv) The level number (ic_tclvl) of an Implication is
                STRICTLY GREATER THAN that of its parent

    (SkolInv)   The level number of the skolems (ic_skols) of an
                Implication is equal to the level of the implication
                itself (ic_tclvl)

    (GivenInv)  The level number of a unification variable appearing
                in the 'ic_given' of an implication I should be
                STRICTLY LESS THAN the ic_tclvl of I
                See Note [GivenInv]

    (WantedInv) The level number of a unification variable appearing
                in the 'ic_wanted' of an implication I should be
                LESS THAN OR EQUAL TO the ic_tclvl of I
                See Note [WantedInv]

The level of a MetaTyVar also governs its untouchability.  See
Note [Unification preconditions] in GHC.Tc.Utils.Unify.

Note [TcLevel assignment]
~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange the TcLevels like this

   0   Top level
   1   First-level implication constraints
   2   Second-level implication constraints
   ...etc...

Note [GivenInv]
~~~~~~~~~~~~~~~
Invariant (GivenInv) is not essential, but it is easy to guarantee, and
it is a useful extra piece of structure.  It ensures that the Givens of
an implication don't change because of unifications /at the same level/
caused by Wanteds.  (Wanteds can also cause unifications at an outer
level, but that will iterate the entire implication; see GHC.Tc.Solver.Monad
Note [The Unification Level Flag].)

Givens can certainly contain meta-tyvars from /outer/ levels.  E.g.
   data T a where
     MkT :: Eq a => a -> MkT a

   f x = case x of MkT y -> y && True

Then we'll infer (x :: T alpha[1]).  The Givens from the implication
arising from the pattern match will look like this:

   forall[2] . Eq alpha[1] => (alpha[1] ~ Bool)

But if we unify alpha (which in this case we will), we'll iterate
the entire implication via Note [The Unification Level Flag] in
GHC.Tc.Solver.Monad.  That isn't true of unifications at the /ambient/
level.

It would be entirely possible to weaken (GivenInv), to LESS THAN OR
EQUAL TO, but we'd need to think carefully about
  - kick-out for Givens
  - GHC.Tc.Solver.Monad.isOuterTyVar
But in fact (GivenInv) is automatically true, so we're adhering to
it for now.  See #18929.

* If a tyvar tv has level n, then the levels of all variables free
  in tv's kind are <= n. Consequence: if tv is untouchable, so are
  all variables in tv's kind.

Note [WantedInv]
~~~~~~~~~~~~~~~~
Why is WantedInv important?  Consider this implication, where
the constraint (C alpha[3]) disobeys WantedInv:

   forall[2] a. blah => (C alpha[3])
                        (forall[3] b. alpha[3] ~ b)

We can unify alpha:=b in the inner implication, because 'alpha' is
touchable; but then 'b' has excaped its scope into the outer implication.
-}

maxTcLevel :: TcLevel -> TcLevel -> TcLevel
maxTcLevel (TcLevel a) (TcLevel b) = TcLevel (a `max` b)

topTcLevel :: TcLevel
-- See Note [TcLevel assignment]
topTcLevel = TcLevel 0   -- 0 = outermost level

isTopTcLevel :: TcLevel -> Bool
isTopTcLevel (TcLevel 0) = True
isTopTcLevel _           = False

pushTcLevel :: TcLevel -> TcLevel
-- See Note [TcLevel assignment]
pushTcLevel (TcLevel us) = TcLevel (us + 1)

strictlyDeeperThan :: TcLevel -> TcLevel -> Bool
strictlyDeeperThan (TcLevel tv_tclvl) (TcLevel ctxt_tclvl)
  = tv_tclvl > ctxt_tclvl

deeperThanOrSame :: TcLevel -> TcLevel -> Bool
deeperThanOrSame (TcLevel tv_tclvl) (TcLevel ctxt_tclvl)
  = tv_tclvl >= ctxt_tclvl

sameDepthAs :: TcLevel -> TcLevel -> Bool
sameDepthAs (TcLevel ctxt_tclvl) (TcLevel tv_tclvl)
  = ctxt_tclvl == tv_tclvl   -- NB: invariant ctxt_tclvl >= tv_tclvl
                             --     So <= would be equivalent

checkTcLevelInvariant :: TcLevel -> TcLevel -> Bool
-- Checks (WantedInv) from Note [TcLevel invariants]
checkTcLevelInvariant (TcLevel ctxt_tclvl) (TcLevel tv_tclvl)
  = ctxt_tclvl >= tv_tclvl

-- Returns topTcLevel for non-TcTyVars
tcTyVarLevel :: TcTyVar -> TcLevel
tcTyVarLevel tv
  = case tcTyVarDetails tv of
          MetaTv { mtv_tclvl = tv_lvl } -> tv_lvl
          SkolemTv _ tv_lvl _           -> tv_lvl
          RuntimeUnk                    -> topTcLevel


tcTypeLevel :: TcType -> TcLevel
-- Max level of any free var of the type
tcTypeLevel ty
  = nonDetStrictFoldDVarSet add topTcLevel (tyCoVarsOfTypeDSet ty)
    -- It's safe to use a non-deterministic fold because `maxTcLevel` is
    -- commutative.
  where
    add v lvl
      | isTcTyVar v = lvl `maxTcLevel` tcTyVarLevel v
      | otherwise = lvl

instance Outputable TcLevel where
  ppr (TcLevel us) = ppr us

{- *********************************************************************
*                                                                      *
    Finding type family instances
*                                                                      *
************************************************************************
-}

-- | Finds outermost type-family applications occurring in a type,
-- after expanding synonyms.  In the list (F, tys) that is returned
-- we guarantee that tys matches F's arity.  For example, given
--    type family F a :: * -> *    (arity 1)
-- calling tcTyFamInsts on (Maybe (F Int Bool) will return
--     (F, [Int]), not (F, [Int,Bool])
--
-- This is important for its use in deciding termination of type
-- instances (see #11581).  E.g.
--    type instance G [Int] = ...(F Int \<big type>)...
-- we don't need to take \<big type> into account when asking if
-- the calls on the RHS are smaller than the LHS
tcTyFamInsts :: Type -> [(TyCon, [Type])]
tcTyFamInsts = map (\(_,b,c) -> (b,c)) . tcTyFamInstsAndVis

-- | Like 'tcTyFamInsts', except that the output records whether the
-- type family and its arguments occur as an /invisible/ argument in
-- some type application. This information is useful because it helps GHC know
-- when to turn on @-fprint-explicit-kinds@ during error reporting so that
-- users can actually see the type family being mentioned.
--
-- As an example, consider:
--
-- @
-- class C a
-- data T (a :: k)
-- type family F a :: k
-- instance C (T @(F Int) (F Bool))
-- @
--
-- There are two occurrences of the type family `F` in that `C` instance, so
-- @'tcTyFamInstsAndVis' (C (T \@(F Int) (F Bool)))@ will return:
--
-- @
-- [ ('True',  F, [Int])
-- , ('False', F, [Bool]) ]
-- @
--
-- @F Int@ is paired with 'True' since it appears as an /invisible/ argument
-- to @C@, whereas @F Bool@ is paired with 'False' since it appears an a
-- /visible/ argument to @C@.
--
-- See also @Note [Kind arguments in error messages]@ in "GHC.Tc.Errors".
tcTyFamInstsAndVis :: Type -> [(Bool, TyCon, [Type])]
tcTyFamInstsAndVis = tcTyFamInstsAndVisX False

tcTyFamInstsAndVisX
  :: Bool -- ^ Is this an invisible argument to some type application?
  -> Type -> [(Bool, TyCon, [Type])]
tcTyFamInstsAndVisX = go
  where
    go is_invis_arg ty
      | Just exp_ty <- tcView ty       = go is_invis_arg exp_ty
    go _ (TyVarTy _)                   = []
    go is_invis_arg (TyConApp tc tys)
      | isTypeFamilyTyCon tc
      = [(is_invis_arg, tc, take (tyConArity tc) tys)]
      | otherwise
      = tcTyConAppTyFamInstsAndVisX is_invis_arg tc tys
    go _            (LitTy {})         = []
    go is_invis_arg (ForAllTy bndr ty) = go is_invis_arg (binderType bndr)
                                         ++ go is_invis_arg ty
    go is_invis_arg (FunTy _ w ty1 ty2)  = go is_invis_arg w
                                         ++ go is_invis_arg ty1
                                         ++ go is_invis_arg ty2
    go is_invis_arg ty@(AppTy _ _)     =
      let (ty_head, ty_args) = splitAppTys ty
          ty_arg_flags       = appTyArgFlags ty_head ty_args
      in go is_invis_arg ty_head
         ++ concat (zipWith (\flag -> go (isInvisibleArgFlag flag))
                            ty_arg_flags ty_args)
    go is_invis_arg (CastTy ty _)      = go is_invis_arg ty
    go _            (CoercionTy _)     = [] -- don't count tyfams in coercions,
                                            -- as they never get normalized,
                                            -- anyway

-- | In an application of a 'TyCon' to some arguments, find the outermost
-- occurrences of type family applications within the arguments. This function
-- will not consider the 'TyCon' itself when checking for type family
-- applications.
--
-- See 'tcTyFamInstsAndVis' for more details on how this works (as this
-- function is called inside of 'tcTyFamInstsAndVis').
tcTyConAppTyFamInstsAndVis :: TyCon -> [Type] -> [(Bool, TyCon, [Type])]
tcTyConAppTyFamInstsAndVis = tcTyConAppTyFamInstsAndVisX False

tcTyConAppTyFamInstsAndVisX
  :: Bool -- ^ Is this an invisible argument to some type application?
  -> TyCon -> [Type] -> [(Bool, TyCon, [Type])]
tcTyConAppTyFamInstsAndVisX is_invis_arg tc tys =
  let (invis_tys, vis_tys) = partitionInvisibleTypes tc tys
  in concat $ map (tcTyFamInstsAndVisX True)         invis_tys
           ++ map (tcTyFamInstsAndVisX is_invis_arg) vis_tys

isTyFamFree :: Type -> Bool
-- ^ Check that a type does not contain any type family applications.
isTyFamFree = null . tcTyFamInsts

any_rewritable :: EqRel   -- Ambient role
               -> (EqRel -> TcTyVar -> Bool)           -- check tyvar
               -> (EqRel -> TyCon -> [TcType] -> Bool) -- check type family
               -> (TyCon -> Bool)                      -- expand type synonym?
               -> TcType -> Bool
-- Checks every tyvar and tyconapp (not including FunTys) within a type,
-- ORing the results of the predicates above together
-- Do not look inside casts and coercions
-- See Note [anyRewritableTyVar must be role-aware]
--
-- This looks like it should use foldTyCo, but that function is
-- role-agnostic, and this one must be role-aware. We could make
-- foldTyCon role-aware, but that may slow down more common usages.
--
-- See Note [Rewritable] in GHC.Tc.Solver.InertSet for a specification for this function.
{-# INLINE any_rewritable #-} -- this allows specialization of predicates
any_rewritable role tv_pred tc_pred should_expand
  = go role emptyVarSet
  where
    go_tv rl bvs tv | tv `elemVarSet` bvs = False
                    | otherwise           = tv_pred rl tv

    go rl bvs ty@(TyConApp tc tys)
      | isTypeSynonymTyCon tc
      , should_expand tc
      , Just ty' <- tcView ty   -- should always match
      = go rl bvs ty'

      | tc_pred rl tc tys
      = True

      | otherwise
      = go_tc rl bvs tc tys

    go rl bvs (TyVarTy tv)       = go_tv rl bvs tv
    go _ _     (LitTy {})        = False
    go rl bvs (AppTy fun arg)    = go rl bvs fun || go NomEq bvs arg
    go rl bvs (FunTy _ w arg res)  = go NomEq bvs arg_rep || go NomEq bvs res_rep ||
                                     go rl bvs arg || go rl bvs res || go NomEq bvs w
      where arg_rep = getRuntimeRep arg -- forgetting these causes #17024
            res_rep = getRuntimeRep res
    go rl bvs (ForAllTy tv ty)   = go rl (bvs `extendVarSet` binderVar tv) ty
    go rl bvs (CastTy ty _)      = go rl bvs ty
    go _  _   (CoercionTy _)     = False

    go_tc NomEq  bvs _  tys = any (go NomEq bvs) tys
    go_tc ReprEq bvs tc tys = any (go_arg bvs)
                              (tyConRolesRepresentational tc `zip` tys)

    go_arg bvs (Nominal,          ty) = go NomEq  bvs ty
    go_arg bvs (Representational, ty) = go ReprEq bvs ty
    go_arg _   (Phantom,          _)  = False  -- We never rewrite with phantoms

anyRewritableTyVar :: EqRel    -- Ambient role
                   -> (EqRel -> TcTyVar -> Bool)  -- check tyvar
                   -> TcType -> Bool
-- See Note [Rewritable] in GHC.Tc.Solver.InertSet for a specification for this function.
anyRewritableTyVar role pred
  = any_rewritable role pred
      (\ _ _ _ -> False) -- no special check for tyconapps
                         -- (this False is ORed with other results, so it
                         --  really means "do nothing special"; the arguments
                         --   are still inspected)
      (\ _ -> False)     -- don't expand synonyms
    -- NB: No need to expand synonyms, because we can find
    -- all free variables of a synonym by looking at its
    -- arguments

anyRewritableTyFamApp :: EqRel   -- Ambient role
                      -> (EqRel -> TyCon -> [TcType] -> Bool) -- check tyconapp
                          -- should return True only for type family applications
                      -> TcType -> Bool
  -- always ignores casts & coercions
-- See Note [Rewritable] in GHC.Tc.Solver.InertSet for a specification for this function.
anyRewritableTyFamApp role check_tyconapp
  = any_rewritable role (\ _ _ -> False) check_tyconapp (not . isFamFreeTyCon)

{- Note [anyRewritableTyVar must be role-aware]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
anyRewritableTyVar is used during kick-out from the inert set,
to decide if, given a new equality (a ~ ty), we should kick out
a constraint C.  Rather than gather free variables and see if 'a'
is among them, we instead pass in a predicate; this is just efficiency.

Moreover, consider
  work item:   [G] a ~R f b
  inert item:  [G] b ~R f a
We use anyRewritableTyVar to decide whether to kick out the inert item,
on the grounds that the work item might rewrite it. Well, 'a' is certainly
free in [G] b ~R f a.  But because the role of a type variable ('f' in
this case) is nominal, the work item can't actually rewrite the inert item.
Moreover, if we were to kick out the inert item the exact same situation
would re-occur and we end up with an infinite loop in which each kicks
out the other (#14363).

-}

{- *********************************************************************
*                                                                      *
          The "exact" free variables of a type
*                                                                      *
********************************************************************* -}

{- Note [Silly type synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  type T a = Int
What are the free tyvars of (T x)?  Empty, of course!

exactTyCoVarsOfType is used by the type checker to figure out exactly
which type variables are mentioned in a type.  It only matters
occasionally -- see the calls to exactTyCoVarsOfType.

We place this function here in GHC.Tc.Utils.TcType, not in GHC.Core.TyCo.FVs,
because we want to "see" tcView (efficiency issue only).
-}

exactTyCoVarsOfType  :: Type   -> TyCoVarSet
exactTyCoVarsOfTypes :: [Type] -> TyCoVarSet
-- Find the free type variables (of any kind)
-- but *expand* type synonyms.  See Note [Silly type synonym] above.

exactTyCoVarsOfType  ty  = runTyCoVars (exact_ty ty)
exactTyCoVarsOfTypes tys = runTyCoVars (exact_tys tys)

exact_ty  :: Type       -> Endo TyCoVarSet
exact_tys :: [Type]     -> Endo TyCoVarSet
(exact_ty, exact_tys, _, _) = foldTyCo exactTcvFolder emptyVarSet

exactTcvFolder :: TyCoFolder TyCoVarSet (Endo TyCoVarSet)
exactTcvFolder = deepTcvFolder { tcf_view = tcView }
                 -- This is the key line

{-
************************************************************************
*                                                                      *
                Predicates
*                                                                      *
************************************************************************
-}

tcIsTcTyVar :: TcTyVar -> Bool
-- See Note [TcTyVars and TyVars in the typechecker]
tcIsTcTyVar tv = isTyVar tv

isPromotableMetaTyVar :: TcTyVar -> Bool
-- True is this is a meta-tyvar that can be
-- promoted to an outer level
isPromotableMetaTyVar tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  , MetaTv { mtv_info = info } <- tcTyVarDetails tv
  = isTouchableInfo info   -- Can't promote cycle breakers
  | otherwise
  = False

isTouchableMetaTyVar :: TcLevel -> TcTyVar -> Bool
isTouchableMetaTyVar ctxt_tclvl tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  , MetaTv { mtv_tclvl = tv_tclvl, mtv_info = info } <- tcTyVarDetails tv
  , isTouchableInfo info
  = assertPpr (checkTcLevelInvariant ctxt_tclvl tv_tclvl)
              (ppr tv $$ ppr tv_tclvl $$ ppr ctxt_tclvl) $
    tv_tclvl `sameDepthAs` ctxt_tclvl

  | otherwise = False

isImmutableTyVar :: TyVar -> Bool
isImmutableTyVar tv = isSkolemTyVar tv

isTyConableTyVar, isSkolemTyVar, isOverlappableTyVar,
  isMetaTyVar, isAmbiguousTyVar, isCycleBreakerTyVar :: TcTyVar -> Bool

isTyConableTyVar tv
        -- True of a meta-type variable that can be filled in
        -- with a type constructor application; in particular,
        -- not a TyVarTv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = case tcTyVarDetails tv of
        MetaTv { mtv_info = TyVarTv } -> False
        _                             -> True
  | otherwise = True

isSkolemTyVar tv
  = assertPpr (tcIsTcTyVar tv) (ppr tv) $
    case tcTyVarDetails tv of
        MetaTv {} -> False
        _other    -> True

skolemSkolInfo :: TcTyVar -> SkolemInfo
skolemSkolInfo tv
  = assert (isSkolemTyVar tv) $
    case tcTyVarDetails tv of
      SkolemTv skol_info _ _ -> skol_info
      RuntimeUnk -> panic "RuntimeUnk"
      MetaTv {} -> panic "skolemSkolInfo"


isOverlappableTyVar tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = case tcTyVarDetails tv of
        SkolemTv _ _ overlappable -> overlappable
        _                       -> False
  | otherwise = False

isMetaTyVar tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = case tcTyVarDetails tv of
        MetaTv {} -> True
        _         -> False
  | otherwise = False

-- isAmbiguousTyVar is used only when reporting type errors
-- It picks out variables that are unbound, namely meta
-- type variables and the RuntimeUnk variables created by
-- GHC.Runtime.Heap.Inspect.zonkRTTIType.  These are "ambiguous" in
-- the sense that they stand for an as-yet-unknown type
isAmbiguousTyVar tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = case tcTyVarDetails tv of
        MetaTv {}     -> True
        RuntimeUnk {} -> True
        _             -> False
  | otherwise = False

isCycleBreakerTyVar tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  , MetaTv { mtv_info = CycleBreakerTv } <- tcTyVarDetails tv
  = True

  | otherwise
  = False

-- | Is this type variable a concrete type variable, i.e.
-- it is a metavariable with 'ConcreteTv' 'MetaInfo'?
--
-- Returns the 'ConcreteTvOrigin' stored in the type variable
-- if so, or 'Nothing' otherwise.
isConcreteTyVar_maybe :: TcTyVar -> Maybe ConcreteTvOrigin
isConcreteTyVar_maybe tv
  | isTcTyVar tv
  , MetaTv { mtv_info = ConcreteTv conc_orig } <- tcTyVarDetails tv
  = Just conc_orig
  | otherwise
  = Nothing

-- | Is this type variable a concrete type variable, i.e.
-- it is a metavariable with 'ConcreteTv' 'MetaInfo'?
isConcreteTyVar :: TcTyVar -> Bool
isConcreteTyVar = isJust . isConcreteTyVar_maybe

-- | Is this type concrete type variable, i.e.
-- a metavariable with 'ConcreteTv' 'MetaInfo'?
isConcreteTyVarTy :: TcType -> Bool
isConcreteTyVarTy = isJust . isConcreteTyVarTy_maybe

-- | Is this type a concrete type variable? If so, return
-- the associated 'TcTyVar' and 'ConcreteTvOrigin'.
isConcreteTyVarTy_maybe :: TcType -> Maybe (TcTyVar, ConcreteTvOrigin)
isConcreteTyVarTy_maybe (TyVarTy tv) = (tv, ) <$> isConcreteTyVar_maybe tv
isConcreteTyVarTy_maybe _            = Nothing

isMetaTyVarTy :: TcType -> Bool
isMetaTyVarTy (TyVarTy tv) = isMetaTyVar tv
isMetaTyVarTy _            = False

metaTyVarInfo :: TcTyVar -> MetaInfo
metaTyVarInfo tv
  = case tcTyVarDetails tv of
      MetaTv { mtv_info = info } -> info
      _ -> pprPanic "metaTyVarInfo" (ppr tv)

isTouchableInfo :: MetaInfo -> Bool
isTouchableInfo info
  | CycleBreakerTv <- info = False
  | otherwise              = True

metaTyVarTcLevel :: TcTyVar -> TcLevel
metaTyVarTcLevel tv
  = case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tclvl } -> tclvl
      _ -> pprPanic "metaTyVarTcLevel" (ppr tv)

metaTyVarTcLevel_maybe :: TcTyVar -> Maybe TcLevel
metaTyVarTcLevel_maybe tv
  = case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tclvl } -> Just tclvl
      _                            -> Nothing

metaTyVarRef :: TyVar -> IORef MetaDetails
metaTyVarRef tv
  = case tcTyVarDetails tv of
        MetaTv { mtv_ref = ref } -> ref
        _ -> pprPanic "metaTyVarRef" (ppr tv)

setMetaTyVarTcLevel :: TcTyVar -> TcLevel -> TcTyVar
setMetaTyVarTcLevel tv tclvl
  = case tcTyVarDetails tv of
      details@(MetaTv {}) -> setTcTyVarDetails tv (details { mtv_tclvl = tclvl })
      _ -> pprPanic "metaTyVarTcLevel" (ppr tv)

isTyVarTyVar :: Var -> Bool
isTyVarTyVar tv
  = case tcTyVarDetails tv of
        MetaTv { mtv_info = TyVarTv } -> True
        _                             -> False

isFlexi, isIndirect :: MetaDetails -> Bool
isFlexi Flexi = True
isFlexi _     = False

isIndirect (Indirect _) = True
isIndirect _            = False

isRuntimeUnkSkol :: TyVar -> Bool
-- Called only in GHC.Tc.Errors; see Note [Runtime skolems] there
isRuntimeUnkSkol x
  | RuntimeUnk <- tcTyVarDetails x = True
  | otherwise                      = False

mkTyVarNamePairs :: [TyVar] -> [(Name,TyVar)]
-- Just pair each TyVar with its own name
mkTyVarNamePairs tvs = [(tyVarName tv, tv) | tv <- tvs]

findDupTyVarTvs :: [(Name,TcTyVar)] -> [(Name,Name)]
-- If we have [...(x1,tv)...(x2,tv)...]
-- return (x1,x2) in the result list
findDupTyVarTvs prs
  = concatMap mk_result_prs $
    findDupsEq eq_snd prs
  where
    eq_snd (_,tv1) (_,tv2) = tv1 == tv2
    mk_result_prs ((n1,_) :| xs) = map (\(n2,_) -> (n1,n2)) xs

-- | Returns the (kind, type) variables in a type that are
-- as-yet-unknown: metavariables and RuntimeUnks
ambigTkvsOfTy :: TcType -> ([Var],[Var])
ambigTkvsOfTy ty
  = partition (`elemVarSet` dep_tkv_set) ambig_tkvs
  where
    tkvs        = tyCoVarsOfTypeList ty
    ambig_tkvs  = filter isAmbiguousTyVar tkvs
    dep_tkv_set = tyCoVarsOfTypes (map tyVarKind tkvs)

{-
************************************************************************
*                                                                      *
   Tau, sigma and rho
*                                                                      *
************************************************************************
-}

mkSigmaTy :: [TyCoVarBinder] -> [PredType] -> Type -> Type
mkSigmaTy bndrs theta tau = mkForAllTys bndrs (mkPhiTy theta tau)

-- | Make a sigma ty where all type variables are 'Inferred'. That is,
-- they cannot be used with visible type application.
mkInfSigmaTy :: [TyCoVar] -> [PredType] -> Type -> Type
mkInfSigmaTy tyvars theta ty = mkSigmaTy (mkTyCoVarBinders Inferred tyvars) theta ty

-- | Make a sigma ty where all type variables are "specified". That is,
-- they can be used with visible type application
mkSpecSigmaTy :: [TyVar] -> [PredType] -> Type -> Type
mkSpecSigmaTy tyvars preds ty = mkSigmaTy (mkTyCoVarBinders Specified tyvars) preds ty

mkPhiTy :: [PredType] -> Type -> Type
mkPhiTy = mkInvisFunTysMany

---------------
getDFunTyKey :: Type -> OccName -- Get some string from a type, to be used to
                                -- construct a dictionary function name
getDFunTyKey ty | Just ty' <- coreView ty = getDFunTyKey ty'
getDFunTyKey (TyVarTy tv)            = getOccName tv
getDFunTyKey (TyConApp tc _)         = getOccName tc
getDFunTyKey (LitTy x)               = getDFunTyLitKey x
getDFunTyKey (AppTy fun _)           = getDFunTyKey fun
getDFunTyKey (FunTy {})              = getOccName funTyCon
getDFunTyKey (ForAllTy _ t)          = getDFunTyKey t
getDFunTyKey (CastTy ty _)           = getDFunTyKey ty
getDFunTyKey t@(CoercionTy _)        = pprPanic "getDFunTyKey" (ppr t)

getDFunTyLitKey :: TyLit -> OccName
getDFunTyLitKey (NumTyLit n) = mkOccName Name.varName (show n)
getDFunTyLitKey (StrTyLit n) = mkOccName Name.varName (show n)  -- hm
getDFunTyLitKey (CharTyLit n) = mkOccName Name.varName (show n)

{- *********************************************************************
*                                                                      *
           Building types
*                                                                      *
********************************************************************* -}

-- ToDo: I think we need Tc versions of these
-- Reason: mkCastTy checks isReflexiveCastTy, which checks
--         for equality; and that has a different answer
--         depending on whether or not Type = Constraint

mkTcAppTys :: Type -> [Type] -> Type
mkTcAppTys = mkAppTys

mkTcAppTy :: Type -> Type -> Type
mkTcAppTy = mkAppTy

mkTcCastTy :: Type -> Coercion -> Type
mkTcCastTy = mkCastTy   -- Do we need a tc version of mkCastTy?

{-
************************************************************************
*                                                                      *
   Expanding and splitting
*                                                                      *
************************************************************************

These tcSplit functions are like their non-Tc analogues, but
        *) they do not look through newtypes

However, they are non-monadic and do not follow through mutable type
variables.  It's up to you to make sure this doesn't matter.
-}

-- | Splits a forall type into a list of 'TyBinder's and the inner type.
-- Always succeeds, even if it returns an empty list.
tcSplitPiTys :: Type -> ([TyBinder], Type)
tcSplitPiTys ty
  = assert (all isTyBinder (fst sty) ) sty
  where sty = splitPiTys ty

-- | Splits a type into a TyBinder and a body, if possible. Panics otherwise
tcSplitPiTy_maybe :: Type -> Maybe (TyBinder, Type)
tcSplitPiTy_maybe ty
  = assert (isMaybeTyBinder sty ) sty
  where
    sty = splitPiTy_maybe ty
    isMaybeTyBinder (Just (t,_)) = isTyBinder t
    isMaybeTyBinder _            = True

tcSplitForAllTyVarBinder_maybe :: Type -> Maybe (TyVarBinder, Type)
tcSplitForAllTyVarBinder_maybe ty | Just ty' <- tcView ty = tcSplitForAllTyVarBinder_maybe ty'
tcSplitForAllTyVarBinder_maybe (ForAllTy tv ty) = assert (isTyVarBinder tv ) Just (tv, ty)
tcSplitForAllTyVarBinder_maybe _                = Nothing

-- | Like 'tcSplitPiTys', but splits off only named binders,
-- returning just the tyvars.
tcSplitForAllTyVars :: Type -> ([TyVar], Type)
tcSplitForAllTyVars ty
  = assert (all isTyVar (fst sty) ) sty
  where sty = splitForAllTyCoVars ty

-- | Like 'tcSplitForAllTyVars', but only splits 'ForAllTy's with 'Invisible'
-- type variable binders.
tcSplitForAllInvisTyVars :: Type -> ([TyVar], Type)
tcSplitForAllInvisTyVars ty = tcSplitSomeForAllTyVars isInvisibleArgFlag ty

-- | Like 'tcSplitForAllTyVars', but only splits a 'ForAllTy' if @argf_pred argf@
-- is 'True', where @argf@ is the visibility of the @ForAllTy@'s binder and
-- @argf_pred@ is a predicate over visibilities provided as an argument to this
-- function.
tcSplitSomeForAllTyVars :: (ArgFlag -> Bool) -> Type -> ([TyVar], Type)
tcSplitSomeForAllTyVars argf_pred ty
  = split ty ty []
  where
    split _ (ForAllTy (Bndr tv argf) ty) tvs
      | argf_pred argf                             = split ty ty (tv:tvs)
    split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
    split orig_ty _                            tvs = (reverse tvs, orig_ty)

-- | Like 'tcSplitForAllTyVars', but only splits 'ForAllTy's with 'Required' type
-- variable binders. All split tyvars are annotated with '()'.
tcSplitForAllReqTVBinders :: Type -> ([TcReqTVBinder], Type)
tcSplitForAllReqTVBinders ty = assert (all (isTyVar . binderVar) (fst sty) ) sty
  where sty = splitForAllReqTVBinders ty

-- | Like 'tcSplitForAllTyVars', but only splits 'ForAllTy's with 'Invisible' type
-- variable binders. All split tyvars are annotated with their 'Specificity'.
tcSplitForAllInvisTVBinders :: Type -> ([TcInvisTVBinder], Type)
tcSplitForAllInvisTVBinders ty = assert (all (isTyVar . binderVar) (fst sty) ) sty
  where sty = splitForAllInvisTVBinders ty

-- | Like 'tcSplitForAllTyVars', but splits off only named binders.
tcSplitForAllTyVarBinders :: Type -> ([TyVarBinder], Type)
tcSplitForAllTyVarBinders ty = assert (all isTyVarBinder (fst sty)) sty
  where sty = splitForAllTyCoVarBinders ty

-- | Is this a ForAllTy with a named binder?
tcIsForAllTy :: Type -> Bool
tcIsForAllTy ty | Just ty' <- tcView ty = tcIsForAllTy ty'
tcIsForAllTy (ForAllTy {}) = True
tcIsForAllTy _             = False

tcSplitPredFunTy_maybe :: Type -> Maybe (PredType, Type)
-- Split off the first predicate argument from a type
tcSplitPredFunTy_maybe ty
  | Just ty' <- tcView ty = tcSplitPredFunTy_maybe ty'
tcSplitPredFunTy_maybe (FunTy { ft_af = InvisArg
                              , ft_arg = arg, ft_res = res })
  = Just (arg, res)
tcSplitPredFunTy_maybe _
  = Nothing

tcSplitPhiTy :: Type -> (ThetaType, Type)
tcSplitPhiTy ty
  = split ty []
  where
    split ty ts
      = case tcSplitPredFunTy_maybe ty of
          Just (pred, ty) -> split ty (pred:ts)
          Nothing         -> (reverse ts, ty)

-- | Split a sigma type into its parts. This only splits /invisible/ type
-- variable binders, as these are the only forms of binder that the typechecker
-- will implicitly instantiate.
tcSplitSigmaTy :: Type -> ([TyVar], ThetaType, Type)
tcSplitSigmaTy ty = case tcSplitForAllInvisTyVars ty of
                        (tvs, rho) -> case tcSplitPhiTy rho of
                                        (theta, tau) -> (tvs, theta, tau)

-- | Split a sigma type into its parts, going underneath as many arrows
-- and foralls as possible. See Note [tcSplitNestedSigmaTys]
tcSplitNestedSigmaTys :: Type -> ([TyVar], ThetaType, Type)
-- See Note [tcSplitNestedSigmaTys]
-- NB: This is basically a pure version of deeplyInstantiate (from Unify) that
--     doesn't compute an HsWrapper.
tcSplitNestedSigmaTys ty
    -- If there's a forall, split it apart and try splitting the rho type
    -- underneath it.
  | (arg_tys, body_ty)   <- tcSplitFunTys ty
  , (tvs1, theta1, rho1) <- tcSplitSigmaTy body_ty
  , not (null tvs1 && null theta1)
  = let (tvs2, theta2, rho2) = tcSplitNestedSigmaTys rho1
    in (tvs1 ++ tvs2, theta1 ++ theta2, mkVisFunTys arg_tys rho2)

    -- If there's no forall, we're done.
  | otherwise = ([], [], ty)

{- Note [tcSplitNestedSigmaTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcSplitNestedSigmaTys splits out all the /nested/ foralls and constraints,
including under function arrows.  E.g. given this type synonym:
  type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

then
  tcSplitNestedSigmaTys (forall s t a b. C s t a b => Int -> Traversal s t a b)

will return
  ( [s,t,a,b,f]
  , [C s t a b, Applicative f]
  , Int -> (a -> f b) -> s -> f t)@.

This function is used in these places:
* Improving error messages in GHC.Tc.Gen.Head.addFunResCtxt
* Validity checking for default methods: GHC.Tc.TyCl.checkValidClass
* A couple of calls in the GHCi debugger: GHC.Runtime.Heap.Inspect

In other words, just in validity checking and error messages; hence
no wrappers or evidence generation.

Notice that tcSplitNestedSigmaTys even looks under function arrows;
doing so is the Right Thing even with simple subsumption, not just
with deep subsumption.
-}

-----------------------
tcTyConAppTyCon :: Type -> TyCon
tcTyConAppTyCon ty
  = case tcTyConAppTyCon_maybe ty of
      Just tc -> tc
      Nothing -> pprPanic "tcTyConAppTyCon" (pprType ty)

-- | Like 'tcRepSplitTyConApp_maybe', but only returns the 'TyCon'.
tcTyConAppTyCon_maybe :: Type -> Maybe TyCon
tcTyConAppTyCon_maybe ty
  | Just ty' <- tcView ty = tcTyConAppTyCon_maybe ty'
tcTyConAppTyCon_maybe (TyConApp tc _)
  = Just tc
tcTyConAppTyCon_maybe (FunTy { ft_af = VisArg })
  = Just funTyCon  -- (=>) is /not/ a TyCon in its own right
                   -- C.f. tcRepSplitAppTy_maybe
tcTyConAppTyCon_maybe _
  = Nothing

tcTyConAppArgs :: Type -> [Type]
tcTyConAppArgs ty = case tcSplitTyConApp_maybe ty of
                        Just (_, args) -> args
                        Nothing        -> pprPanic "tcTyConAppArgs" (pprType ty)

tcSplitTyConApp :: Type -> (TyCon, [Type])
tcSplitTyConApp ty = case tcSplitTyConApp_maybe ty of
                        Just stuff -> stuff
                        Nothing    -> pprPanic "tcSplitTyConApp" (pprType ty)

-----------------------
tcSplitFunTys :: Type -> ([Scaled Type], Type)
tcSplitFunTys ty = case tcSplitFunTy_maybe ty of
                        Nothing        -> ([], ty)
                        Just (arg,res) -> (arg:args, res')
                                       where
                                          (args,res') = tcSplitFunTys res

tcSplitFunTy_maybe :: Type -> Maybe (Scaled Type, Type)
tcSplitFunTy_maybe ty
  | Just ty' <- tcView ty = tcSplitFunTy_maybe ty'
tcSplitFunTy_maybe (FunTy { ft_af = af, ft_mult = w, ft_arg = arg, ft_res = res })
  | VisArg <- af = Just (Scaled w arg, res)
tcSplitFunTy_maybe _ = Nothing
        -- Note the VisArg guard
        -- Consider     (?x::Int) => Bool
        -- We don't want to treat this as a function type!
        -- A concrete example is test tc230:
        --      f :: () -> (?p :: ()) => () -> ()
        --
        --      g = f () ()

tcSplitFunTysN :: Arity                      -- n: Number of desired args
               -> TcRhoType
               -> Either Arity               -- Number of missing arrows
                        ([Scaled TcSigmaType],-- Arg types (always N types)
                         TcSigmaType)        -- The rest of the type
-- ^ Split off exactly the specified number argument types
-- Returns
--  (Left m) if there are 'm' missing arrows in the type
--  (Right (tys,res)) if the type looks like t1 -> ... -> tn -> res
tcSplitFunTysN n ty
 | n == 0
 = Right ([], ty)
 | Just (arg,res) <- tcSplitFunTy_maybe ty
 = case tcSplitFunTysN (n-1) res of
     Left m            -> Left m
     Right (args,body) -> Right (arg:args, body)
 | otherwise
 = Left n

tcSplitFunTy :: Type -> (Scaled Type, Type)
tcSplitFunTy  ty = expectJust "tcSplitFunTy" (tcSplitFunTy_maybe ty)

tcFunArgTy :: Type -> Scaled Type
tcFunArgTy    ty = fst (tcSplitFunTy ty)

tcFunResultTy :: Type -> Type
tcFunResultTy ty = snd (tcSplitFunTy ty)

-- | Strips off n *visible* arguments and returns the resulting type
tcFunResultTyN :: HasDebugCallStack => Arity -> Type -> Type
tcFunResultTyN n ty
  | Right (_, res_ty) <- tcSplitFunTysN n ty
  = res_ty
  | otherwise
  = pprPanic "tcFunResultTyN" (ppr n <+> ppr ty)

-----------------------
tcSplitAppTy_maybe :: Type -> Maybe (Type, Type)
tcSplitAppTy_maybe ty | Just ty' <- tcView ty = tcSplitAppTy_maybe ty'
tcSplitAppTy_maybe ty = tcRepSplitAppTy_maybe ty

tcSplitAppTy :: Type -> (Type, Type)
tcSplitAppTy ty = case tcSplitAppTy_maybe ty of
                    Just stuff -> stuff
                    Nothing    -> pprPanic "tcSplitAppTy" (pprType ty)

tcSplitAppTys :: Type -> (Type, [Type])
tcSplitAppTys ty
  = go ty []
  where
    go ty args = case tcSplitAppTy_maybe ty of
                   Just (ty', arg) -> go ty' (arg:args)
                   Nothing         -> (ty,args)

-- | Returns the number of arguments in the given type, without
-- looking through synonyms. This is used only for error reporting.
-- We don't look through synonyms because of #11313.
tcRepGetNumAppTys :: Type -> Arity
tcRepGetNumAppTys = length . snd . repSplitAppTys

-----------------------
-- | If the type is a tyvar, possibly under a cast, returns it, along
-- with the coercion. Thus, the co is :: kind tv ~N kind type
tcGetCastedTyVar_maybe :: Type -> Maybe (TyVar, CoercionN)
tcGetCastedTyVar_maybe ty | Just ty' <- tcView ty = tcGetCastedTyVar_maybe ty'
tcGetCastedTyVar_maybe (CastTy (TyVarTy tv) co) = Just (tv, co)
tcGetCastedTyVar_maybe (TyVarTy tv)             = Just (tv, mkNomReflCo (tyVarKind tv))
tcGetCastedTyVar_maybe _                        = Nothing

tcGetTyVar_maybe :: Type -> Maybe TyVar
tcGetTyVar_maybe ty | Just ty' <- tcView ty = tcGetTyVar_maybe ty'
tcGetTyVar_maybe (TyVarTy tv)   = Just tv
tcGetTyVar_maybe _              = Nothing

tcGetTyVar :: String -> Type -> TyVar
tcGetTyVar msg ty
  = case tcGetTyVar_maybe ty of
     Just tv -> tv
     Nothing -> pprPanic msg (ppr ty)

tcIsTyVarTy :: Type -> Bool
tcIsTyVarTy ty | Just ty' <- tcView ty = tcIsTyVarTy ty'
tcIsTyVarTy (CastTy ty _) = tcIsTyVarTy ty  -- look through casts, as
                                            -- this is only used for
                                            -- e.g., FlexibleContexts
tcIsTyVarTy (TyVarTy _)   = True
tcIsTyVarTy _             = False

-----------------------
tcSplitDFunTy :: Type -> ([TyVar], [Type], Class, [Type])
-- Split the type of a dictionary function
-- We don't use tcSplitSigmaTy,  because a DFun may (with NDP)
-- have non-Pred arguments, such as
--     df :: forall m. (forall b. Eq b => Eq (m b)) -> C m
--
-- Also NB splitFunTys, not tcSplitFunTys;
-- the latter specifically stops at PredTy arguments,
-- and we don't want to do that here
tcSplitDFunTy ty
  = case tcSplitForAllInvisTyVars ty of { (tvs, rho)    ->
    case splitFunTys rho             of { (theta, tau)  ->
    case tcSplitDFunHead tau         of { (clas, tys)   ->
    (tvs, map scaledThing theta, clas, tys) }}}

tcSplitDFunHead :: Type -> (Class, [Type])
tcSplitDFunHead = getClassPredTys

tcSplitMethodTy :: Type -> ([TyVar], PredType, Type)
-- A class method (selector) always has a type like
--   forall as. C as => blah
-- So if the class looks like
--   class C a where
--     op :: forall b. (Eq a, Ix b) => a -> b
-- the class method type looks like
--  op :: forall a. C a => forall b. (Eq a, Ix b) => a -> b
--
-- tcSplitMethodTy just peels off the outer forall and
-- that first predicate
tcSplitMethodTy ty
  | (sel_tyvars,sel_rho) <- tcSplitForAllInvisTyVars ty
  , Just (first_pred, local_meth_ty) <- tcSplitPredFunTy_maybe sel_rho
  = (sel_tyvars, first_pred, local_meth_ty)
  | otherwise
  = pprPanic "tcSplitMethodTy" (ppr ty)


{- *********************************************************************
*                                                                      *
            Type equalities
*                                                                      *
********************************************************************* -}

tcEqKind :: HasDebugCallStack => TcKind -> TcKind -> Bool
tcEqKind = tcEqType

tcEqType :: HasDebugCallStack => TcType -> TcType -> Bool
-- ^ tcEqType implements typechecker equality, as described in
-- @Note [Typechecker equality vs definitional equality]@.
tcEqType ty1 ty2
  =  tcEqTypeNoSyns ki1 ki2
  && tcEqTypeNoSyns ty1 ty2
  where
    ki1 = tcTypeKind ty1
    ki2 = tcTypeKind ty2

-- | Just like 'tcEqType', but will return True for types of different kinds
-- as long as their non-coercion structure is identical.
tcEqTypeNoKindCheck :: TcType -> TcType -> Bool
tcEqTypeNoKindCheck ty1 ty2
  = tcEqTypeNoSyns ty1 ty2

-- | Check whether two TyConApps are the same; if the number of arguments
-- are different, just checks the common prefix of arguments.
tcEqTyConApps :: TyCon -> [Type] -> TyCon -> [Type] -> Bool
tcEqTyConApps tc1 args1 tc2 args2
  = tc1 == tc2 &&
    and (zipWith tcEqTypeNoKindCheck args1 args2)
    -- No kind check necessary: if both arguments are well typed, then
    -- any difference in the kinds of later arguments would show up
    -- as differences in earlier (dependent) arguments

{-
Note [Specialising tc_eq_type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type equality predicates in TcType are hit pretty hard during typechecking.
Consequently we take pains to ensure that these paths are compiled to
efficient, minimally-allocating code.

To this end we place an INLINE on tc_eq_type, ensuring that it is inlined into
its publicly-visible interfaces (e.g. tcEqType). In addition to eliminating
some dynamic branches, this allows the simplifier to eliminate the closure
allocations that would otherwise be necessary to capture the two boolean "mode"
flags. This reduces allocations by a good fraction of a percent when compiling
Cabal.

See #19226.
-}

-- | Type equality comparing both visible and invisible arguments and expanding
-- type synonyms.
tcEqTypeNoSyns :: TcType -> TcType -> Bool
tcEqTypeNoSyns ta tb = tc_eq_type False False ta tb

-- | Like 'tcEqType', but returns True if the /visible/ part of the types
-- are equal, even if they are really unequal (in the invisible bits)
tcEqTypeVis :: TcType -> TcType -> Bool
tcEqTypeVis ty1 ty2 = tc_eq_type False True ty1 ty2

-- | Like 'pickyEqTypeVis', but returns a Bool for convenience
pickyEqType :: TcType -> TcType -> Bool
-- Check when two types _look_ the same, _including_ synonyms.
-- So (pickyEqType String [Char]) returns False
-- This ignores kinds and coercions, because this is used only for printing.
pickyEqType ty1 ty2 = tc_eq_type True False ty1 ty2

-- | Real worker for 'tcEqType'. No kind check!
tc_eq_type :: Bool          -- ^ True <=> do not expand type synonyms
           -> Bool          -- ^ True <=> compare visible args only
           -> Type -> Type
           -> Bool
-- Flags False, False is the usual setting for tc_eq_type
-- See Note [Computing equality on types] in Type
tc_eq_type keep_syns vis_only orig_ty1 orig_ty2
  = go orig_env orig_ty1 orig_ty2
  where
    go :: RnEnv2 -> Type -> Type -> Bool
    -- See Note [Comparing nullary type synonyms] in GHC.Core.Type.
    go _   (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = True

    go env t1 t2 | not keep_syns, Just t1' <- tcView t1 = go env t1' t2
    go env t1 t2 | not keep_syns, Just t2' <- tcView t2 = go env t1 t2'

    go env (TyVarTy tv1) (TyVarTy tv2)
      = rnOccL env tv1 == rnOccR env tv2

    go _   (LitTy lit1) (LitTy lit2)
      = lit1 == lit2

    go env (ForAllTy (Bndr tv1 vis1) ty1)
           (ForAllTy (Bndr tv2 vis2) ty2)
      =  vis1 `sameVis` vis2
           -- See Note [ForAllTy and typechecker equality] in
           -- GHC.Tc.Solver.Canonical for why we use `sameVis` here
      && (vis_only || go env (varType tv1) (varType tv2))
      && go (rnBndr2 env tv1 tv2) ty1 ty2

    -- Make sure we handle all FunTy cases since falling through to the
    -- AppTy case means that tcRepSplitAppTy_maybe may see an unzonked
    -- kind variable, which causes things to blow up.
    -- See Note [Equality on FunTys] in GHC.Core.TyCo.Rep: we must check
    -- kinds here
    go env (FunTy _ w1 arg1 res1) (FunTy _ w2 arg2 res2)
      = kinds_eq && go env arg1 arg2 && go env res1 res2 && go env w1 w2
      where
        kinds_eq | vis_only  = True
                 | otherwise = go env (typeKind arg1) (typeKind arg2) &&
                               go env (typeKind res1) (typeKind res2)

      -- See Note [Equality on AppTys] in GHC.Core.Type
    go env (AppTy s1 t1)        ty2
      | Just (s2, t2) <- tcRepSplitAppTy_maybe ty2
      = go env s1 s2 && go env t1 t2
    go env ty1                  (AppTy s2 t2)
      | Just (s1, t1) <- tcRepSplitAppTy_maybe ty1
      = go env s1 s2 && go env t1 t2

    go env (TyConApp tc1 ts1)   (TyConApp tc2 ts2)
      = tc1 == tc2 && gos env (tc_vis tc1) ts1 ts2

    go env (CastTy t1 _)   t2              = go env t1 t2
    go env t1              (CastTy t2 _)   = go env t1 t2
    go _   (CoercionTy {}) (CoercionTy {}) = True

    go _ _ _ = False

    gos _   _         []       []      = True
    gos env (ig:igs) (t1:ts1) (t2:ts2) = (ig || go env t1 t2)
                                      && gos env igs ts1 ts2
    gos _ _ _ _ = False

    tc_vis :: TyCon -> [Bool]  -- True for the fields we should ignore
    tc_vis tc | vis_only  = inviss ++ repeat False    -- Ignore invisibles
              | otherwise = repeat False              -- Ignore nothing
       -- The repeat False is necessary because tycons
       -- can legitimately be oversaturated
      where
        bndrs = tyConBinders tc
        inviss  = map isInvisibleTyConBinder bndrs

    orig_env = mkRnEnv2 $ mkInScopeSet $ tyCoVarsOfTypes [orig_ty1, orig_ty2]

{-# INLINE tc_eq_type #-} -- See Note [Specialising tc_eq_type].

{- Note [Typechecker equality vs definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC has two notions of equality over Core types:

* Definitional equality, as implemented by GHC.Core.Type.eqType.
  See Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep.
* Typechecker equality, as implemented by tcEqType (in GHC.Tc.Utils.TcType).
  GHC.Tc.Solver.Canonical.canEqNC also respects typechecker equality.

Typechecker equality implies definitional equality: if two types are equal
according to typechecker equality, then they are also equal according to
definitional equality. The converse is not always true, as typechecker equality
is more finer-grained than definitional equality in two places:

* Unlike definitional equality, which equates Type and Constraint, typechecker
  treats them as distinct types. See Note [Kind Constraint and kind Type] in
  GHC.Core.Type.
* Unlike definitional equality, which does not care about the ArgFlag of a
  ForAllTy, typechecker equality treats Required type variable binders as
  distinct from Invisible type variable binders.
  See Note [ForAllTy and typechecker equality] in GHC.Tc.Solver.Canonical.
-}

{- *********************************************************************
*                                                                      *
                       Predicate types
*                                                                      *
************************************************************************

Deconstructors and tests on predicate types

Note [Kind polymorphic type classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    class C f where...   -- C :: forall k. k -> Constraint
    g :: forall (f::*). C f => f -> f

Here the (C f) in the signature is really (C * f), and we
don't want to complain that the * isn't a type variable!
-}

isTyVarClassPred :: PredType -> Bool
isTyVarClassPred ty = case getClassPredTys_maybe ty of
    Just (_, tys) -> all isTyVarTy tys
    _             -> False

-------------------------
checkValidClsArgs :: Bool -> Class -> [KindOrType] -> Bool
-- If the Bool is True (flexible contexts), return True (i.e. ok)
-- Otherwise, check that the type (not kind) args are all headed by a tyvar
--   E.g. (Eq a) accepted, (Eq (f a)) accepted, but (Eq Int) rejected
-- This function is here rather than in GHC.Tc.Validity because it is
-- called from GHC.Tc.Solver, which itself is imported by GHC.Tc.Validity
checkValidClsArgs flexible_contexts cls kts
  | flexible_contexts = True
  | otherwise         = all hasTyVarHead tys
  where
    tys = filterOutInvisibleTypes (classTyCon cls) kts

hasTyVarHead :: Type -> Bool
-- Returns true of (a t1 .. tn), where 'a' is a type variable
hasTyVarHead ty                 -- Haskell 98 allows predicates of form
  | tcIsTyVarTy ty = True       --      C (a ty1 .. tyn)
  | otherwise                   -- where a is a type variable
  = case tcSplitAppTy_maybe ty of
       Just (ty, _) -> hasTyVarHead ty
       Nothing      -> False

evVarPred :: EvVar -> PredType
evVarPred var = varType var
  -- Historical note: I used to have an ASSERT here,
  -- checking (isEvVarType (varType var)).  But with something like
  --   f :: c => _ -> _
  -- we end up with (c :: kappa), and (kappa ~ Constraint).  Until
  -- we solve and zonk (which there is no particular reason to do for
  -- partial signatures, (isEvVarType kappa) will return False. But
  -- nothing is wrong.  So I just removed the ASSERT.

---------------------------
boxEqPred :: EqRel -> Type -> Type -> Maybe (Class, [Type])
-- Given (t1 ~# t2) or (t1 ~R# t2) return the boxed version
--       (t1 ~ t2)  or (t1 `Coercible` t2)
boxEqPred eq_rel ty1 ty2
  = case eq_rel of
      NomEq  | homo_kind -> Just (eqClass,        [k1,     ty1, ty2])
             | otherwise -> Just (heqClass,       [k1, k2, ty1, ty2])
      ReprEq | homo_kind -> Just (coercibleClass, [k1,     ty1, ty2])
             | otherwise -> Nothing -- Sigh: we do not have hererogeneous Coercible
                                    --       so we can't abstract over it
                                    -- Nothing fundamental: we could add it
 where
   k1 = tcTypeKind ty1
   k2 = tcTypeKind ty2
   homo_kind = k1 `tcEqType` k2

pickCapturedPreds
  :: TyVarSet           -- Quantifying over these
  -> TcThetaType        -- Proposed constraints to quantify
  -> TcThetaType        -- A subset that we can actually quantify
-- A simpler version of pickQuantifiablePreds, used to winnow down
-- the inferred constraints of a group of bindings, into those for
-- one particular identifier
pickCapturedPreds qtvs theta
  = filter captured theta
  where
    captured pred = isIPLikePred pred || (tyCoVarsOfType pred `intersectsVarSet` qtvs)


-- Superclasses

type PredWithSCs a = (PredType, [PredType], a)

mkMinimalBySCs :: forall a. (a -> PredType) -> [a] -> [a]
-- Remove predicates that
--
--   - are the same as another predicate
--
--   - can be deduced from another by superclasses,
--
--   - are a reflexive equality (e.g  * ~ *)
--     (see Note [Remove redundant provided dicts] in GHC.Tc.TyCl.PatSyn)
--
-- The result is a subset of the input.
-- The 'a' is just paired up with the PredType;
--   typically it might be a dictionary Id
mkMinimalBySCs get_pred xs = go preds_with_scs []
 where
   preds_with_scs :: [PredWithSCs a]
   preds_with_scs = [ (pred, implicants pred, x)
                    | x <- xs
                    , let pred = get_pred x ]

   go :: [PredWithSCs a]   -- Work list
      -> [PredWithSCs a]   -- Accumulating result
      -> [a]
   go [] min_preds
     = reverse (map thdOf3 min_preds)
       -- The 'reverse' isn't strictly necessary, but it
       -- means that the results are returned in the same
       -- order as the input, which is generally saner
   go (work_item@(p,_,_) : work_list) min_preds
     | EqPred _ t1 t2 <- classifyPredType p
     , t1 `tcEqType` t2   -- See GHC.Tc.TyCl.PatSyn
                          -- Note [Remove redundant provided dicts]
     = go work_list min_preds
     | p `in_cloud` work_list || p `in_cloud` min_preds
       -- Why look at work-list too?  Suppose work_item is Eq a,
       -- and work-list contains Ord a
     = go work_list min_preds
     | otherwise
     = go work_list (work_item : min_preds)

   in_cloud :: PredType -> [PredWithSCs a] -> Bool
   in_cloud p ps = or [ p `tcEqType` p' | (_, scs, _) <- ps, p' <- scs ]

   implicants pred
     = pred : eq_extras pred ++ transSuperClasses pred

   -- Combine (a ~ b) and (b ~ a); no need to have both in one context
   -- These can arise when dealing with partial type signatures (e.g. T14715)
   eq_extras pred
     = case classifyPredType pred of
         EqPred r t1 t2               -> [mkPrimEqPredRole (eqRelRole r) t2 t1]
         ClassPred cls [k1,k2,t1,t2]
           | cls `hasKey` heqTyConKey -> [mkClassPred cls [k2, k1, t2, t1]]
         ClassPred cls [k,t1,t2]
           | cls `hasKey` eqTyConKey  -> [mkClassPred cls [k, t2, t1]]
         _ -> []

transSuperClasses :: PredType -> [PredType]
-- (transSuperClasses p) returns (p's superclasses) not including p
-- Stop if you encounter the same class again
-- See Note [Expanding superclasses]
transSuperClasses p
  = go emptyNameSet p
  where
    go :: NameSet -> PredType -> [PredType]
    go rec_clss p
       | ClassPred cls tys <- classifyPredType p
       , let cls_nm = className cls
       , not (cls_nm `elemNameSet` rec_clss)
       , let rec_clss' | isCTupleClass cls = rec_clss
                       | otherwise         = rec_clss `extendNameSet` cls_nm
       = [ p' | sc <- immSuperClasses cls tys
              , p'  <- sc : go rec_clss' sc ]
       | otherwise
       = []

immSuperClasses :: Class -> [Type] -> [PredType]
immSuperClasses cls tys
  = substTheta (zipTvSubst tyvars tys) sc_theta
  where
    (tyvars,sc_theta,_,_) = classBigSig cls

isImprovementPred :: PredType -> Bool
-- Either it's an equality, or has some functional dependency
isImprovementPred ty
  = case classifyPredType ty of
      EqPred NomEq t1 t2 -> not (t1 `tcEqType` t2)
      EqPred ReprEq _ _  -> False
      ClassPred cls _    -> classHasFds cls
      IrredPred {}       -> True -- Might have equalities after reduction?
      ForAllPred {}      -> False

{- Note [Expanding superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we expand superclasses, we use the following algorithm:

transSuperClasses( C tys ) returns the transitive superclasses
                           of (C tys), not including C itself

For example
  class C a b => D a b
  class D b a => C a b

Then
  transSuperClasses( Ord ty )  = [Eq ty]
  transSuperClasses( C ta tb ) = [D tb ta, C tb ta]

Notice that in the recursive-superclass case we include C again at
the end of the chain.  One could exclude C in this case, but
the code is more awkward and there seems no good reason to do so.
(However C.f. GHC.Tc.Solver.Canonical.mk_strict_superclasses, which /does/
appear to do so.)

The algorithm is expand( so_far, pred ):

 1. If pred is not a class constraint, return empty set
       Otherwise pred = C ts
 2. If C is in so_far, return empty set (breaks loops)
 3. Find the immediate superclasses constraints of (C ts)
 4. For each such sc_pred, return (sc_pred : expand( so_far+C, D ss )

Notice that

 * With normal Haskell-98 classes, the loop-detector will never bite,
   so we'll get all the superclasses.

 * We need the loop-breaker in case we have UndecidableSuperClasses on

 * Since there is only a finite number of distinct classes, expansion
   must terminate.

 * The loop breaking is a bit conservative. Notably, a tuple class
   could contain many times without threatening termination:
      (Eq a, (Ord a, Ix a))
   And this is try of any class that we can statically guarantee
   as non-recursive (in some sense).  For now, we just make a special
   case for tuples.  Something better would be cool.

See also GHC.Tc.TyCl.Utils.checkClassCycles.

************************************************************************
*                                                                      *
      Classifying types
*                                                                      *
************************************************************************
-}

isSigmaTy :: TcType -> Bool
-- isSigmaTy returns true of any qualified type.  It doesn't
-- *necessarily* have any foralls.  E.g
--        f :: (?x::Int) => Int -> Int
isSigmaTy ty | Just ty' <- tcView ty = isSigmaTy ty'
isSigmaTy (ForAllTy {})                = True
isSigmaTy (FunTy { ft_af = InvisArg }) = True
isSigmaTy _                            = False

isRhoTy :: TcType -> Bool   -- True of TcRhoTypes; see Note [TcRhoType]
isRhoTy ty | Just ty' <- tcView ty = isRhoTy ty'
isRhoTy (ForAllTy {})                = False
isRhoTy (FunTy { ft_af = InvisArg }) = False
isRhoTy _                            = True

-- | Like 'isRhoTy', but also says 'True' for 'Infer' types
isRhoExpTy :: ExpType -> Bool
isRhoExpTy (Check ty) = isRhoTy ty
isRhoExpTy (Infer {}) = True

isOverloadedTy :: Type -> Bool
-- Yes for a type of a function that might require evidence-passing
-- Used only by bindLocalMethods
isOverloadedTy ty | Just ty' <- tcView ty = isOverloadedTy ty'
isOverloadedTy (ForAllTy _  ty)             = isOverloadedTy ty
isOverloadedTy (FunTy { ft_af = InvisArg }) = True
isOverloadedTy _                            = False

isFloatTy, isDoubleTy,
    isFloatPrimTy, isDoublePrimTy,
    isIntegerTy, isNaturalTy,
    isIntTy, isWordTy, isBoolTy,
    isUnitTy, isCharTy, isAnyTy :: Type -> Bool
isFloatTy      = is_tc floatTyConKey
isDoubleTy     = is_tc doubleTyConKey
isFloatPrimTy  = is_tc floatPrimTyConKey
isDoublePrimTy = is_tc doublePrimTyConKey
isIntegerTy    = is_tc integerTyConKey
isNaturalTy    = is_tc naturalTyConKey
isIntTy        = is_tc intTyConKey
isWordTy       = is_tc wordTyConKey
isBoolTy       = is_tc boolTyConKey
isUnitTy       = is_tc unitTyConKey
isCharTy       = is_tc charTyConKey
isAnyTy        = is_tc anyTyConKey

-- | Is the type inhabited by machine floating-point numbers?
--
-- Used to check that we don't use floating-point literal patterns
-- in Core.
--
-- See #9238 and Note [Rules for floating-point comparisons]
-- in GHC.Core.Opt.ConstantFold.
isFloatingPrimTy :: Type -> Bool
isFloatingPrimTy ty = isFloatPrimTy ty || isDoublePrimTy ty

-- | Is a type 'String'?
isStringTy :: Type -> Bool
isStringTy ty
  = case tcSplitTyConApp_maybe ty of
      Just (tc, [arg_ty]) -> tc == listTyCon && isCharTy arg_ty
      _                   -> False

is_tc :: Unique -> Type -> Bool
-- Newtypes are opaque to this
is_tc uniq ty = case tcSplitTyConApp_maybe ty of
                        Just (tc, _) -> uniq == getUnique tc
                        Nothing      -> False

isRigidTy :: TcType -> Bool
isRigidTy ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isGenerativeTyCon tc Nominal
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | isForAllTy ty                           = True
  | otherwise                               = False

{-
************************************************************************
*                                                                      *
   Misc
*                                                                      *
************************************************************************

Note [Visible type application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC implements a generalisation of the algorithm described in the
"Visible Type Application" paper (available from
http://www.cis.upenn.edu/~sweirich/publications.html). A key part
of that algorithm is to distinguish user-specified variables from inferred
variables. For example, the following should typecheck:

  f :: forall a b. a -> b -> b
  f = const id

  g = const id

  x = f @Int @Bool 5 False
  y = g 5 @Bool False

The idea is that we wish to allow visible type application when we are
instantiating a specified, fixed variable. In practice, specified, fixed
variables are either written in a type signature (or
annotation), OR are imported from another module. (We could do better here,
for example by doing SCC analysis on parts of a module and considering any
type from outside one's SCC to be fully specified, but this is very confusing to
users. The simple rule above is much more straightforward and predictable.)

So, both of f's quantified variables are specified and may be instantiated.
But g has no type signature, so only id's variable is specified (because id
is imported). We write the type of g as forall {a}. a -> forall b. b -> b.
Note that the a is in braces, meaning it cannot be instantiated with
visible type application.

Tracking specified vs. inferred variables is done conveniently by a field
in TyBinder.

-}

deNoteType :: Type -> Type
-- Remove all *outermost* type synonyms and other notes
deNoteType ty | Just ty' <- coreView ty = deNoteType ty'
deNoteType ty = ty

{-
Find the free tycons and classes of a type.  This is used in the front
end of the compiler.
-}

{-
************************************************************************
*                                                                      *
   External types
*                                                                      *
************************************************************************

The compiler's foreign function interface supports the passing of a
restricted set of types as arguments and results (the restricting factor
being the )
-}

tcSplitIOType_maybe :: Type -> Maybe (TyCon, Type)
-- (tcSplitIOType_maybe t) returns Just (IO,t',co)
--              if co : t ~ IO t'
--              returns Nothing otherwise
tcSplitIOType_maybe ty
  = case tcSplitTyConApp_maybe ty of
        Just (io_tycon, [io_res_ty])
         | io_tycon `hasKey` ioTyConKey ->
            Just (io_tycon, io_res_ty)
        _ ->
            Nothing

-- | Reason why a type in an FFI signature is invalid
data IllegalForeignTypeReason
  = TypeCannotBeMarshaled !Type TypeCannotBeMarshaledReason
  | ForeignDynNotPtr
      !Type -- ^ Expected type
      !Type -- ^ Actual type
  | SafeHaskellMustBeInIO
  | IOResultExpected
  | UnexpectedNestedForall
  | LinearTypesNotAllowed
  | OneArgExpected
  | AtLeastOneArgExpected

-- | Reason why a type cannot be marshalled through the FFI.
data TypeCannotBeMarshaledReason
  = NotADataType
  | NewtypeDataConNotInScope !(Maybe TyCon)
  | UnliftedFFITypesNeeded
  | NotABoxedMarshalableTyCon
  | ForeignLabelNotAPtr
  | NotSimpleUnliftedType

isFFIArgumentTy :: DynFlags -> Safety -> Type -> Validity' IllegalForeignTypeReason
-- Checks for valid argument type for a 'foreign import'
isFFIArgumentTy dflags safety ty
   = checkRepTyCon (legalOutgoingTyCon dflags safety) ty

isFFIExternalTy :: Type -> Validity' IllegalForeignTypeReason
-- Types that are allowed as arguments of a 'foreign export'
isFFIExternalTy ty = checkRepTyCon legalFEArgTyCon ty

isFFIImportResultTy :: DynFlags -> Type -> Validity' IllegalForeignTypeReason
isFFIImportResultTy dflags ty
  = checkRepTyCon (legalFIResultTyCon dflags) ty

isFFIExportResultTy :: Type -> Validity' IllegalForeignTypeReason
isFFIExportResultTy ty = checkRepTyCon legalFEResultTyCon ty

isFFIDynTy :: Type -> Type -> Validity' IllegalForeignTypeReason
-- The type in a foreign import dynamic must be Ptr, FunPtr, or a newtype of
-- either, and the wrapped function type must be equal to the given type.
-- We assume that all types have been run through normaliseFfiType, so we don't
-- need to worry about expanding newtypes here.
isFFIDynTy expected ty
    -- Note [Foreign import dynamic]
    -- In the example below, expected would be 'CInt -> IO ()', while ty would
    -- be 'FunPtr (CDouble -> IO ())'.
    | Just (tc, [ty']) <- splitTyConApp_maybe ty
    , tyConUnique tc `elem` [ptrTyConKey, funPtrTyConKey]
    , eqType ty' expected
    = IsValid
    | otherwise
    = NotValid (ForeignDynNotPtr expected ty)

isFFILabelTy :: Type -> Validity' IllegalForeignTypeReason
-- The type of a foreign label must be Ptr, FunPtr, or a newtype of either.
isFFILabelTy ty = checkRepTyCon ok ty
  where
    ok tc | tc `hasKey` funPtrTyConKey || tc `hasKey` ptrTyConKey
          = IsValid
          | otherwise
          = NotValid ForeignLabelNotAPtr

isFFIPrimArgumentTy :: DynFlags -> Type -> Validity' IllegalForeignTypeReason
-- Checks for valid argument type for a 'foreign import prim'
-- Currently they must all be simple unlifted types, or the well-known type
-- Any, which can be used to pass the address to a Haskell object on the heap to
-- the foreign function.
isFFIPrimArgumentTy dflags ty
  | isAnyTy ty = IsValid
  | otherwise  = checkRepTyCon (legalFIPrimArgTyCon dflags) ty

isFFIPrimResultTy :: DynFlags -> Type -> Validity' IllegalForeignTypeReason
-- Checks for valid result type for a 'foreign import prim' Currently
-- it must be an unlifted type, including unboxed tuples, unboxed
-- sums, or the well-known type Any.
isFFIPrimResultTy dflags ty
  | isAnyTy ty = IsValid
  | otherwise = checkRepTyCon (legalFIPrimResultTyCon dflags) ty

isFunPtrTy :: Type -> Bool
isFunPtrTy ty
  | Just (tc, [_]) <- splitTyConApp_maybe ty
  = tc `hasKey` funPtrTyConKey
  | otherwise
  = False

-- normaliseFfiType gets run before checkRepTyCon, so we don't
-- need to worry about looking through newtypes or type functions
-- here; that's already been taken care of.
checkRepTyCon
  :: (TyCon -> Validity' TypeCannotBeMarshaledReason)
  -> Type
  -> Validity' IllegalForeignTypeReason
checkRepTyCon check_tc ty
  = fmap (TypeCannotBeMarshaled ty) $ case splitTyConApp_maybe ty of
      Just (tc, tys)
        | isNewTyCon tc -> NotValid (mk_nt_reason tc tys)
        | otherwise     -> check_tc tc
      Nothing -> NotValid NotADataType
  where
    mk_nt_reason tc tys
      | null tys  = NewtypeDataConNotInScope Nothing
      | otherwise = NewtypeDataConNotInScope (Just tc)

{-
Note [Foreign import dynamic]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A dynamic stub must be of the form 'FunPtr ft -> ft' where ft is any foreign
type.  Similarly, a wrapper stub must be of the form 'ft -> IO (FunPtr ft)'.

We use isFFIDynTy to check whether a signature is well-formed. For example,
given a (illegal) declaration like:

foreign import ccall "dynamic"
  foo :: FunPtr (CDouble -> IO ()) -> CInt -> IO ()

isFFIDynTy will compare the 'FunPtr' type 'CDouble -> IO ()' with the curried
result type 'CInt -> IO ()', and return False, as they are not equal.


----------------------------------------------
These chaps do the work; they are not exported
----------------------------------------------
-}

legalFEArgTyCon :: TyCon -> Validity' TypeCannotBeMarshaledReason
legalFEArgTyCon tc
  -- It's illegal to make foreign exports that take unboxed
  -- arguments.  The RTS API currently can't invoke such things.  --SDM 7/2000
  = boxedMarshalableTyCon tc

legalFIResultTyCon :: DynFlags -> TyCon -> Validity' TypeCannotBeMarshaledReason
legalFIResultTyCon dflags tc
  | tc == unitTyCon         = IsValid
  | otherwise               = marshalableTyCon dflags tc

legalFEResultTyCon :: TyCon -> Validity' TypeCannotBeMarshaledReason
legalFEResultTyCon tc
  | tc == unitTyCon         = IsValid
  | otherwise               = boxedMarshalableTyCon tc

legalOutgoingTyCon :: DynFlags -> Safety -> TyCon -> Validity' TypeCannotBeMarshaledReason
-- Checks validity of types going from Haskell -> external world
legalOutgoingTyCon dflags _ tc
  = marshalableTyCon dflags tc

-- Check for marshalability of a primitive type.
-- We exclude lifted types such as RealWorld and TYPE.
-- They can technically appear in types, e.g.
-- f :: RealWorld -> TYPE LiftedRep -> RealWorld
-- f x _ = x
-- but there are no values of type RealWorld or TYPE LiftedRep,
-- so it doesn't make sense to use them in FFI.
marshalablePrimTyCon :: TyCon -> Bool
marshalablePrimTyCon tc = isPrimTyCon tc && not (isLiftedTypeKind (tyConResKind tc))

marshalableTyCon :: DynFlags -> TyCon -> Validity' TypeCannotBeMarshaledReason
marshalableTyCon dflags tc
  | marshalablePrimTyCon tc
  , not (null (tyConPrimRep tc)) -- Note [Marshalling void]
  = validIfUnliftedFFITypes dflags
  | otherwise
  = boxedMarshalableTyCon tc

boxedMarshalableTyCon :: TyCon -> Validity' TypeCannotBeMarshaledReason
boxedMarshalableTyCon tc
   | getUnique tc `elem` [ intTyConKey, int8TyConKey, int16TyConKey
                         , int32TyConKey, int64TyConKey
                         , wordTyConKey, word8TyConKey, word16TyConKey
                         , word32TyConKey, word64TyConKey
                         , floatTyConKey, doubleTyConKey
                         , ptrTyConKey, funPtrTyConKey
                         , charTyConKey
                         , stablePtrTyConKey
                         , boolTyConKey
                         ]
  = IsValid

  | otherwise = NotValid NotABoxedMarshalableTyCon

legalFIPrimArgTyCon :: DynFlags -> TyCon -> Validity' TypeCannotBeMarshaledReason
-- Check args of 'foreign import prim', only allow simple unlifted types.
legalFIPrimArgTyCon dflags tc
  | marshalablePrimTyCon tc
  = validIfUnliftedFFITypes dflags
  | otherwise
  = NotValid NotSimpleUnliftedType

legalFIPrimResultTyCon :: DynFlags -> TyCon -> Validity' TypeCannotBeMarshaledReason
-- Check result type of 'foreign import prim'. Allow simple unlifted
-- types and also unboxed tuple and sum result types.
legalFIPrimResultTyCon dflags tc
  | marshalablePrimTyCon tc
  , not (null (tyConPrimRep tc))   -- Note [Marshalling void]
  = validIfUnliftedFFITypes dflags

  | isUnboxedTupleTyCon tc || isUnboxedSumTyCon tc
  = validIfUnliftedFFITypes dflags

  | otherwise
  = NotValid $ NotSimpleUnliftedType

validIfUnliftedFFITypes :: DynFlags -> Validity' TypeCannotBeMarshaledReason
validIfUnliftedFFITypes dflags
  | xopt LangExt.UnliftedFFITypes dflags =  IsValid
  | otherwise = NotValid UnliftedFFITypesNeeded

{-
Note [Marshalling void]
~~~~~~~~~~~~~~~~~~~~~~~
We don't treat State# (whose PrimRep is VoidRep) as marshalable.
In turn that means you can't write
        foreign import foo :: Int -> State# RealWorld

Reason: the back end falls over with panic "primRepHint:VoidRep";
        and there is no compelling reason to permit it
-}

{-
************************************************************************
*                                                                      *
        The "Paterson size" of a type
*                                                                      *
************************************************************************
-}

{-
Note [Paterson conditions on PredTypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are considering whether *class* constraints terminate
(see Note [Paterson conditions]). Precisely, the Paterson conditions
would have us check that "the constraint has fewer constructors and variables
(taken together and counting repetitions) than the head.".

However, we can be a bit more refined by looking at which kind of constraint
this actually is. There are two main tricks:

 1. It seems like it should be OK not to count the tuple type constructor
    for a PredType like (Show a, Eq a) :: Constraint, since we don't
    count the "implicit" tuple in the ThetaType itself.

    In fact, the Paterson test just checks *each component* of the top level
    ThetaType against the size bound, one at a time. By analogy, it should be
    OK to return the size of the *largest* tuple component as the size of the
    whole tuple.

 2. Once we get into an implicit parameter or equality we
    can't get back to a class constraint, so it's safe
    to say "size 0".  See #4200.

NB: we don't want to detect PredTypes in sizeType (and then call
sizePred on them), or we might get an infinite loop if that PredType
is irreducible. See #5581.
-}

type TypeSize = IntWithInf

sizeType :: Type -> TypeSize
-- Size of a type: the number of variables and constructors
-- Ignore kinds altogether
sizeType = go
  where
    go ty | Just exp_ty <- tcView ty = go exp_ty
    go (TyVarTy {})              = 1
    go (TyConApp tc tys)
      | isTypeFamilyTyCon tc     = infinity  -- Type-family applications can
                                             -- expand to any arbitrary size
      | otherwise                = sizeTypes (filterOutInvisibleTypes tc tys) + 1
                                   -- Why filter out invisible args?  I suppose any
                                   -- size ordering is sound, but why is this better?
                                   -- I came across this when investigating #14010.
    go (LitTy {})                = 1
    go (FunTy _ w arg res)       = go w + go arg + go res + 1
    go (AppTy fun arg)           = go fun + go arg
    go (ForAllTy (Bndr tv vis) ty)
        | isVisibleArgFlag vis   = go (tyVarKind tv) + go ty + 1
        | otherwise              = go ty + 1
    go (CastTy ty _)             = go ty
    go (CoercionTy {})           = 0

sizeTypes :: [Type] -> TypeSize
sizeTypes tys = sum (map sizeType tys)

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-----------------------
-- | For every arg a tycon can take, the returned list says True if the argument
-- is taken visibly, and False otherwise. Ends with an infinite tail of Trues to
-- allow for oversaturation.
tcTyConVisibilities :: TyCon -> [Bool]
tcTyConVisibilities tc = tc_binder_viss ++ tc_return_kind_viss ++ repeat True
  where
    tc_binder_viss      = map isVisibleTyConBinder (tyConBinders tc)
    tc_return_kind_viss = map isVisibleBinder (fst $ tcSplitPiTys (tyConResKind tc))

-- | If the tycon is applied to the types, is the next argument visible?
isNextTyConArgVisible :: TyCon -> [Type] -> Bool
isNextTyConArgVisible tc tys
  = tcTyConVisibilities tc `getNth` length tys

-- | Should this type be applied to a visible argument?
isNextArgVisible :: TcType -> Bool
isNextArgVisible ty
  | Just (bndr, _) <- tcSplitPiTy_maybe ty = isVisibleBinder bndr
  | otherwise                              = True
    -- this second case might happen if, say, we have an unzonked TauTv.
    -- But TauTvs can't range over types that take invisible arguments
