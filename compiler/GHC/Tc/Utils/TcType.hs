{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE MultiWayIf          #-}

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

  ExpType(..), ExpKind, InferResult(..),
  ExpTypeFRR, ExpSigmaType, ExpSigmaTypeFRR,
  ExpRhoType,
  mkCheckExpType,
  checkingExpType_maybe, checkingExpType,

  ExpPatType(..), mkCheckExpFunPatTy, mkInvisExpPatType,
  isVisibleExpPatType, isExpFunPatType,

  SyntaxOpType(..), synKnownType, mkSynFunTys,

  --------------------------------
  -- TcLevel
  TcLevel(..), topTcLevel, pushTcLevel, isTopTcLevel,
  strictlyDeeperThan, deeperThanOrSame, sameDepthAs,
  tcTypeLevel, tcTyVarLevel, maxTcLevel, minTcLevel,

  --------------------------------
  -- MetaDetails
  TcTyVarDetails(..), pprTcTyVarDetails, vanillaSkolemTvUnk,
  MetaDetails(Flexi, Indirect), MetaInfo(..), skolemSkolInfo,
  isImmutableTyVar, isSkolemTyVar, isMetaTyVar,  isMetaTyVarTy, isTyVarTy,
  tcIsTcTyVar, isTyVarTyVar, isOverlappableTyVar,  isTyConableTyVar,
  ConcreteTvOrigin(..), isConcreteTyVar_maybe, isConcreteTyVar,
  isConcreteTyVarTy, isConcreteTyVarTy_maybe, isConcreteInfo,
  ConcreteTyVars, noConcreteTyVars,
  isAmbiguousTyVar, isCycleBreakerTyVar, metaTyVarRef, metaTyVarInfo,
  isFlexi, isIndirect, isRuntimeUnkSkol,
  isQLInstTyVar, isRuntimeUnkTyVar,
  metaTyVarTcLevel, setMetaTyVarTcLevel, metaTyVarTcLevel_maybe,
  isTouchableMetaTyVar, isPromotableMetaTyVar,
  findDupTyVarTvs, mkTyVarNamePairs,

  --------------------------------
  -- Builders
  mkInfSigmaTy, mkSpecSigmaTy, mkSigmaTy, mkPhiTy, tcMkPhiTy,
  tcMkDFunSigmaTy, tcMkDFunPhiTy,

  --------------------------------
  -- Splitters
  getTyVar, getTyVar_maybe, getCastedTyVar_maybe,
  tcSplitForAllTyVarBinder_maybe, tcSplitForAllTyVarsReqTVBindersN,
  tcSplitForAllTyVars, tcSplitForAllInvisTyVars, tcSplitSomeForAllTyVars,
  tcSplitForAllReqTVBinders, tcSplitForAllInvisTVBinders,
  tcSplitPiTys, tcSplitPiTy_maybe, tcSplitForAllTyVarBinders,
  tcSplitPhiTy, tcSplitPredFunTy_maybe,
  tcSplitFunTy_maybe, tcSplitFunTys, tcFunArgTy, tcFunResultTy, tcFunResultTyN,
  tcSplitFunTysN,
  tcSplitTyConApp, tcSplitTyConApp_maybe,
  tcTyConAppTyCon, tcTyConAppTyCon_maybe, tcTyConAppArgs,
  tcSplitAppTy_maybe, tcSplitAppTy, tcSplitAppTys, tcSplitAppTyNoView_maybe,
  tcSplitSigmaTy, tcSplitSigmaTyBndrs, tcSplitNestedSigmaTys, tcSplitIOType_maybe,

  ---------------------------------
  -- Predicates.
  -- Again, newtypes are opaque
  isSigmaTy, isRhoTy, isRhoExpTy, isOverloadedTy,
  isFloatingPrimTy, isDoubleTy, isFloatTy, isIntTy, isWordTy, isStringTy,
  isIntegerTy, isNaturalTy,
  isBoolTy, isUnitTy, isCharTy,
  isTauTy, isTauTyCon, tcIsTyVarTy,
  isPredTy, isTyVarClassPred,
  checkValidClsArgs, hasTyVarHead,
  isRigidTy, anyTy_maybe,


  -- Re-exported from GHC.Core.TyCo.Compare
  -- mainly just for back-compat reasons
  eqType, eqTypes, nonDetCmpType, eqTypeX,
  pickyEqType, tcEqType, tcEqKind, tcEqTypeNoKindCheck, mayLookIdentical,
  tcEqTyConApps, eqForAllVis, eqVarBndrs,

  ---------------------------------
  -- Misc type manipulators

  deNoteType,
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
  anyRewritableTyVar, anyRewritableTyFamApp, UnderFam,

  ---------------------------------
  -- Patersons sizes
  PatersonSize(..), PatersonCondFailure(..),
  PatersonCondFailureContext(..),
  ltPatersonSize,
  pSizeZero, pSizeOne,
  pSizeType, pSizeTypeX, pSizeTypes,
  pSizeClassPred, pSizeClassPredX,
  pSizeTyConApp,
  noMoreTyVars, allDistinctTyVars,
  TypeSize, sizeType, sizeTypes, scopedSort,
  isTerminatingClass, isStuckTypeFamily,

  --------------------------------
  -- Reexported from Kind
  Kind, liftedTypeKind, constraintKind,
  isLiftedTypeKind, isUnliftedTypeKind, isTYPEorCONSTRAINT,

  --------------------------------
  -- Reexported from Type
  Type, PredType, ThetaType, PiTyBinder,
  ForAllTyFlag(..), FunTyFlag(..),

  mkForAllTy, mkForAllTys, mkInvisForAllTys, mkTyCoInvForAllTys,
  mkSpecForAllTys, mkTyCoInvForAllTy,
  mkInfForAllTy, mkInfForAllTys,
  mkVisFunTy, mkVisFunTyMany, mkVisFunTysMany,
  mkScaledFunTys,
  mkInvisFunTy, mkInvisFunTys,
  mkTyConApp, mkAppTy, mkAppTys,
  mkTyConTy, mkTyVarTy, mkTyVarTys,
  mkTyCoVarTy, mkTyCoVarTys,

  isClassPred, isEqPrimPred, isIPLikePred, isEqPred,
  isEqualityClass, mkClassPred,
  tcSplitQuantPredTy, tcSplitDFunTy, tcSplitDFunHead, tcSplitMethodTy,
  isRuntimeRepVar, isFixedRuntimeRepKind,
  isVisiblePiTyBinder, isInvisiblePiTyBinder,

  -- Type substitutions
  Subst(..),         -- Representation visible to a few friends
  TvSubstEnv, emptySubst, mkEmptySubst,
  zipTvSubst,
  mkTvSubstPrs, notElemSubst, unionSubst,
  getTvSubstEnv, getSubstInScope, extendSubstInScope,
  extendSubstInScopeList, extendSubstInScopeSet, extendTvSubstAndInScope,
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

  isUnliftedType,
  isUnboxedTupleType,
  isPrimitiveType,

  coreView,

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

  ---------------------------------
  -- argument visibility
  tyConVisibilities, isNextTyConArgVisible, isNextArgVisible

  ) where

-- friends:
import GHC.Prelude

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Subst ( mkTvSubst, substTyWithCoVars )
import GHC.Core.TyCo.Compare
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr
import GHC.Core.Class
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Core.Coercion
import GHC.Core.Type as Type
import GHC.Core.Predicate
import GHC.Core.TyCon

import {-# SOURCE #-} GHC.Tc.Types.Origin
  ( SkolemInfo, unkSkol
  , FixedRuntimeRepOrigin, FixedRuntimeRepContext )

-- others:
import GHC.Types.Name as Name
            -- We use this to make dictionaries for type literals.
            -- Perhaps there's a better way to do this?
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Builtin.Names
import GHC.Builtin.Types ( coercibleClass, eqClass, heqClass, unitTyConKey
                         , listTyCon, constraintKind )
import GHC.Types.Basic
import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Data.List.SetOps ( getNth, findDupsEq )
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.IORef ( IORef )
import Data.List.NonEmpty( NonEmpty(..) )
import Data.List ( partition, nub, (\\) )

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

This is a bit of a change from an earlier era when we remorselessly
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

-- See Note [TcTyCon, MonoTcTyCon, and PolyTcTyCon] in GHC.Tc.TyCl
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

type ExpRhoType = ExpType
      -- Invariant: if -XDeepSubsumption is on,
      --            and we are checking (i.e. the ExpRhoType is (Check rho)),
      --            then the `rho` is deeply skolemised

-- | Like 'ExpType', but on kind level
type ExpKind = ExpType

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

-- | Returns the expected type when in checking mode.
checkingExpType_maybe :: ExpType -> Maybe TcType
checkingExpType_maybe (Check ty) = Just ty
checkingExpType_maybe (Infer {}) = Nothing

-- | Returns the expected type when in checking mode.
--   Panics if in inference mode.
checkingExpType :: ExpType -> TcType
checkingExpType (Check ty)    = ty
checkingExpType et@(Infer {}) = pprPanic "checkingExpType" (ppr et)

-- Expected type of a pattern in a lambda or a function left-hand side.
data ExpPatType =
    ExpFunPatTy    (Scaled ExpSigmaTypeFRR)   -- the type A of a function A -> B
  | ExpForAllPatTy ForAllTyBinder             -- the binder (a::A) of  forall (a::A) -> B or forall (a :: A). B

mkCheckExpFunPatTy :: Scaled TcType -> ExpPatType
mkCheckExpFunPatTy (Scaled mult ty) = ExpFunPatTy (Scaled mult (mkCheckExpType ty))

mkInvisExpPatType :: InvisTyBinder -> ExpPatType
mkInvisExpPatType (Bndr tv spec) = ExpForAllPatTy (Bndr tv (Invisible spec))

isVisibleExpPatType :: ExpPatType -> Bool
isVisibleExpPatType (ExpForAllPatTy (Bndr _ vis)) = isVisibleForAllTyFlag vis
isVisibleExpPatType (ExpFunPatTy {})              = True

isExpFunPatType :: ExpPatType -> Bool
isExpFunPatType ExpFunPatTy{}    = True
isExpFunPatType ExpForAllPatTy{} = False

instance Outputable ExpPatType where
  ppr (ExpFunPatTy t) = ppr t
  ppr (ExpForAllPatTy tv) = text "forall" <+> ppr tv

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

But ultimately I want to separate Type from TcType, and in that case
we would need to enforce the separation.

Note [Keeping SkolemInfo inside a SkolemTv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A SkolemTv contains a SkolemInfo, which describes the binding side of that
TcTyVar.  This is very convenient to a consumer of a SkolemTv, but it is
a bit awkward for the /producer/.  Why? Because sometimes we can't produce
the SkolemInfo until we have the TcTyVars!

Example: in `GHC.Tc.Utils.Unify.tcSkolemise` we create SkolemTvs whose
`SkolemInfo` is `SigSkol`, whose arguments in turn mention the newly-created
SkolemTvs.  So we a RecrusiveDo idiom, like this:

  rec { (wrap, tv_prs, given, rho_ty) <- skolemise skol_info expected_ty
      ; skol_info <- mkSkolemInfo (SigSkol ctxt expected_ty tv_prs) }

Note that the `skol_info` can't be created until we have the `tv_prs` returned
by `skolemise`. Note also that `skolemise` had better be lazy in `skol_info`.

All uses of this idiom should be flagged with a reference to this Note.
-}

-- A TyVarDetails is inside a TyVar
-- See Note [TyVars and TcTyVars during type checking]
data TcTyVarDetails
  = SkolemTv      -- A skolem
       SkolemInfo -- See Note [Keeping SkolemInfo inside a SkolemTv]
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

vanillaSkolemTvUnk :: HasDebugCallStack => TcTyVarDetails
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
-- These are checked in GHC.Tc.Utils.Unify.checkTopShape
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
                     -- GHC.Tc.Solver.Equality

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

-- | A mapping from skolem type variable 'Name' to concreteness information,
--
-- See Note [Representation-polymorphism checking built-ins] in GHC.Tc.Utils.Concrete.
type ConcreteTyVars = NameEnv ConcreteTvOrigin
-- | The 'Id' has no outer forall'd type variables which must be instantiated
-- to concrete types.
noConcreteTyVars :: ConcreteTyVars
noConcreteTyVars = emptyNameEnv

{- *********************************************************************
*                                                                      *
                Untouchable type variables
*                                                                      *
********************************************************************* -}

data TcLevel = TcLevel {-# UNPACK #-} !Int
             | QLInstVar
  -- See Note [TcLevel invariants] for what this Int is
  -- See also Note [TcLevel assignment]
  -- See also Note [The QLInstVar TcLevel]

{-
Note [TcLevel invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~
* Each unification variable (MetaTv)
  and skolem (SkolemTv)
  and each Implication
  has a level number (of type TcLevel)

* INVARIANT (KindInv) Given a type variable (tv::ki) at at level L,
                      the free vars of `ki` all have level <= L

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

  -- See also Note [The QLInstVar TcLevel]

Note [TcLevel assignment]
~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange the TcLevels like this

   0          Top level
   1          First-level implication constraints
   2          Second-level implication constraints
   ...etc...
   QLInstVar  The level for QuickLook instantiation variables
              See Note [The QLInstVar TcLevel]

Note [The QLInstVar TcLevel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
QuickLook instantiation variables are identified by having a TcLevel
of QLInstVar.  See Note [Quick Look overview] in GHC.Tc.Gen.App.

The QLInstVar level behaves like infinity: it is greater than any
other TcLevel.  See `strictlyDeeperThan` and friends in this module.
That ensures that we never unify an ordinary unification variable
with a QL instantiation variable, e.g.
      alpha[tau:3] := Maybe beta[tau:qlinstvar]
(This is an immediate consequence of our general rule that we never
unify a variable with a type mentioning deeper variables; the skolem
escape check.)

QL instantation variables are eventually turned into ordinary unificaiton
variables; see (QL3) in Note [Quick Look overview].

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
touchable; but then 'b' has escaped its scope into the outer implication.
-}

maxTcLevel :: TcLevel -> TcLevel -> TcLevel
maxTcLevel (TcLevel a) (TcLevel b)
  | a > b      = TcLevel a
  | otherwise  = TcLevel b
maxTcLevel _ _ = QLInstVar

minTcLevel :: TcLevel -> TcLevel -> TcLevel
minTcLevel tcla@(TcLevel a) tclb@(TcLevel b)
  | a < b                              = tcla
  | otherwise                          = tclb
minTcLevel tcla@(TcLevel {}) QLInstVar = tcla
minTcLevel QLInstVar tclb@(TcLevel {}) = tclb
minTcLevel QLInstVar QLInstVar         = QLInstVar

topTcLevel :: TcLevel
-- See Note [TcLevel assignment]
topTcLevel = TcLevel 0   -- 0 = outermost level

isTopTcLevel :: TcLevel -> Bool
isTopTcLevel (TcLevel 0) = True
isTopTcLevel _            = False

pushTcLevel :: TcLevel -> TcLevel
-- See Note [TcLevel assignment]
pushTcLevel (TcLevel us) = TcLevel (us + 1)
pushTcLevel QLInstVar    = QLInstVar

strictlyDeeperThan :: TcLevel -> TcLevel -> Bool
-- See Note [The QLInstVar TcLevel]
strictlyDeeperThan (TcLevel tv_tclvl) (TcLevel ctxt_tclvl)
  = tv_tclvl > ctxt_tclvl
strictlyDeeperThan QLInstVar (TcLevel {})  = True
strictlyDeeperThan _ _                     = False

deeperThanOrSame :: TcLevel -> TcLevel -> Bool
-- See Note [The QLInstVar TcLevel]
deeperThanOrSame (TcLevel tv_tclvl) (TcLevel ctxt_tclvl)
  = tv_tclvl >= ctxt_tclvl
deeperThanOrSame (TcLevel {}) QLInstVar  = False
deeperThanOrSame QLInstVar    _           = True

sameDepthAs :: TcLevel -> TcLevel -> Bool
sameDepthAs (TcLevel ctxt_tclvl) (TcLevel tv_tclvl)
  = ctxt_tclvl == tv_tclvl
    -- NB: invariant ctxt_tclvl >= tv_tclvl
    --     So <= would be equivalent
sameDepthAs QLInstVar QLInstVar = True
sameDepthAs _         _         = False

checkTcLevelInvariant :: TcLevel -> TcLevel -> Bool
-- Checks (WantedInv) from Note [TcLevel invariants]
checkTcLevelInvariant ctxt_tclvl tv_tclvl
  = ctxt_tclvl `deeperThanOrSame` tv_tclvl

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
  ppr (TcLevel n) = ppr n
  ppr QLInstVar   = text "qlinst"

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
-- See also Note [Showing invisible bits of types in error messages]
-- in "GHC.Tc.Errors.Ppr".
tcTyFamInstsAndVis :: Type -> [(Bool, TyCon, [Type])]
tcTyFamInstsAndVis = tcTyFamInstsAndVisX False

tcTyFamInstsAndVisX
  :: Bool -- ^ Is this an invisible argument to some type application?
  -> Type -> [(Bool, TyCon, [Type])]
tcTyFamInstsAndVisX = go
  where
    go is_invis_arg ty
      | Just exp_ty <- coreView ty     = go is_invis_arg exp_ty
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
          ty_arg_flags       = appTyForAllTyFlags ty_head ty_args
      in go is_invis_arg ty_head
         ++ concat (zipWith (\flag -> go (isInvisibleForAllTyFlag flag))
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

type UnderFam = Bool   -- True <=> we are in the argument of a type family application

any_rewritable :: EqRel   -- Ambient role
               -> (UnderFam -> EqRel -> TcTyVar -> Bool)           -- Check tyvar
               -> (UnderFam -> EqRel -> TyCon -> [TcType] -> Bool) -- Check type family application
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
any_rewritable role tv_pred tc_pred ty
  = go False emptyVarSet role ty
  where
    go_tv uf bvs rl tv | tv `elemVarSet` bvs = False
                       | otherwise           = tv_pred uf rl tv

    go :: UnderFam -> VarSet -> EqRel -> TcType -> Bool
    go under_fam bvs rl (TyConApp tc tys)

      -- Expand synonyms, unless (a) we are at Nominal role and (b) the synonym
      -- is type-family-free; then it suffices just to look at the args
      | isTypeSynonymTyCon tc
      , case rl of { NomEq -> not (isFamFreeTyCon tc); ReprEq -> True }
      , Just ty' <- expandSynTyConApp_maybe tc tys
      = go under_fam bvs rl ty'

      -- Check if we are going under a type family application
      | case rl of
           NomEq  -> isTypeFamilyTyCon tc
           ReprEq -> isFamilyTyCon     tc
      = if | tc_pred under_fam rl tc tys -> True
           | otherwise                   -> go_fam under_fam (tyConArity tc) bvs tys

      | otherwise
      = go_tc under_fam bvs rl tc tys

    go uf bvs rl (TyVarTy tv)        = go_tv uf bvs rl tv
    go _  _    _ (LitTy {})          = False
    go uf bvs rl (AppTy fun arg)     = go uf bvs rl fun || go uf bvs NomEq arg
    go uf bvs rl (FunTy _ w arg res) = go uf bvs NomEq arg_rep || go uf bvs NomEq res_rep ||
                                       go uf bvs rl arg || go uf bvs rl res || go uf bvs NomEq w
      where arg_rep = getRuntimeRep arg -- forgetting these causes #17024
            res_rep = getRuntimeRep res
    go uf bvs rl (ForAllTy tv ty)   = go uf (bvs `extendVarSet` binderVar tv) rl ty
    go uf bvs rl (CastTy ty _)      = go uf bvs rl ty
    go _  _   _  (CoercionTy _)     = False

    go_tc :: UnderFam -> VarSet -> EqRel -> TyCon -> [TcType] -> Bool
    go_tc uf bvs NomEq  _  tys = any (go uf bvs NomEq) tys
    go_tc uf bvs ReprEq tc tys = any2 (go_arg uf bvs) tys (tyConRoleListRepresentational tc)

    go_arg uf bvs ty Nominal          = go uf bvs NomEq ty
    go_arg uf bvs ty Representational = go uf bvs ReprEq ty
    go_arg _   _  _  Phantom          = False  -- We never rewrite with phantoms

    -- For a type-family or data-family application (F t1 .. tn), all arguments
    --   have Nominal role (whether in F's arity or, if over-saturated, beyond it)
    -- Switch on under_fam for arguments <= arity
    go_fam uf 0 bvs tys      = any (go uf bvs NomEq) tys   -- Like AppTy
    go_fam _  _ _   []       = False
    go_fam uf n bvs (ty:tys) = go True bvs NomEq ty || go_fam uf (n-1) bvs tys
                               -- True <=> switch on under_fam

anyRewritableTyVar :: EqRel    -- Ambient role
                   -> (UnderFam -> EqRel -> TcTyVar -> Bool)  -- check tyvar
                   -> TcType -> Bool
-- See Note [Rewritable] in GHC.Tc.Solver.InertSet for a specification for this function.
anyRewritableTyVar role check_tv
  = any_rewritable role
      check_tv
      (\ _ _ _ _ -> False) -- No special check for tyconapps
                           -- (this False is ORed with other results,
                           --  so it really means "do nothing special";
                           --  the arguments are still inspected)

anyRewritableTyFamApp :: EqRel   -- Ambient role
                      -> (UnderFam -> EqRel -> TyCon -> [TcType] -> Bool)
                         -- Check a type-family application
                      -> TcType -> Bool
  -- always ignores casts & coercions
-- See Note [Rewritable] in GHC.Tc.Solver.InertSet for a specification for this function.
anyRewritableTyFamApp role check_tyconapp
  = any_rewritable role (\ _ _ _ -> False) check_tyconapp

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
because we want to "see" coreView (efficiency issue only).
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
exactTcvFolder = deepTcvFolder { tcf_view = coreView }
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

isQLInstTyVar :: TcTyVar -> Bool
isQLInstTyVar tv
  = case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = QLInstVar } -> True
      _                                -> False

isRuntimeUnkTyVar :: TcTyVar -> Bool
isRuntimeUnkTyVar tv
  = case tcTyVarDetails tv of
      MetaTv { mtv_info = RuntimeUnkTv } -> True
      _                                  -> False

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

isConcreteInfo :: MetaInfo -> Bool
isConcreteInfo (ConcreteTv {}) = True
isConcreteInfo _               = False

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

-- | Make a sigma ty where all type variables are 'Inferred'. That is,
-- they cannot be used with visible type application.
mkInfSigmaTy :: HasDebugCallStack => [TyCoVar] -> [PredType] -> Type -> Type
mkInfSigmaTy tyvars theta ty = mkSigmaTy (mkForAllTyBinders Inferred tyvars) theta ty

-- | Make a sigma ty where all type variables are "specified". That is,
-- they can be used with visible type application
mkSpecSigmaTy :: HasDebugCallStack => [TyVar] -> [PredType] -> Type -> Type
mkSpecSigmaTy tyvars preds ty = mkSigmaTy (mkForAllTyBinders Specified tyvars) preds ty

mkSigmaTy :: HasDebugCallStack => [ForAllTyBinder] -> [PredType] -> Type -> Type
-- Result is TypeLike
mkSigmaTy bndrs theta tau = mkForAllTys bndrs (mkPhiTy theta tau)

tcMkDFunSigmaTy :: [TyVar] -> ThetaType -> Type -> Type
tcMkDFunSigmaTy tvs theta res_ty
 = mkForAllTys (mkForAllTyBinders Specified tvs) $
   tcMkDFunPhiTy theta res_ty

mkPhiTy :: HasDebugCallStack => [PredType] -> Type -> Type
-- Result type is TypeLike
mkPhiTy = mkInvisFunTys

tcMkPhiTy :: HasDebugCallStack => [PredType] -> Type -> Type
-- Like mkPhiTy, but with no assertion checks; it is called
-- by the type checker and the result kind may not be zonked yet
-- But the result kind is TypeLike
tcMkPhiTy tys ty = foldr (tcMkInvisFunTy TypeLike) ty tys

tcMkDFunPhiTy :: HasDebugCallStack => [PredType] -> Type -> Type
-- Just like tcMkPhiTy, but result type is ConstraintLike
tcMkDFunPhiTy preds res = foldr (tcMkInvisFunTy ConstraintLike) res preds

---------------
getDFunTyKey :: Type -> OccName -- Get some string from a type, to be used to
                                -- construct a dictionary function name
getDFunTyKey ty | Just ty' <- coreView ty = getDFunTyKey ty'
getDFunTyKey (TyVarTy tv)            = getOccName tv
getDFunTyKey (TyConApp tc _)         = getOccName tc
getDFunTyKey (LitTy x)               = getDFunTyLitKey x
getDFunTyKey (AppTy fun _)           = getDFunTyKey fun
getDFunTyKey (FunTy { ft_af = af })  = getOccName (funTyFlagTyCon af)
getDFunTyKey (ForAllTy _ t)          = getDFunTyKey t
getDFunTyKey (CastTy ty _)           = getDFunTyKey ty
getDFunTyKey t@(CoercionTy _)        = pprPanic "getDFunTyKey" (ppr t)

getDFunTyLitKey :: TyLit -> OccName
getDFunTyLitKey (NumTyLit n) = mkOccName Name.varName (show n)
getDFunTyLitKey (StrTyLit n) = mkOccName Name.varName (show n)  -- hm
getDFunTyLitKey (CharTyLit n) = mkOccName Name.varName (show n)

{-
************************************************************************
*                                                                      *
   Expanding and splitting
*                                                                      *
************************************************************************
-}

-- | Splits a forall type into a list of 'PiTyVarBinder's and the inner type.
-- Always succeeds, even if it returns an empty list.
tcSplitPiTys :: Type -> ([PiTyVarBinder], Type)
tcSplitPiTys ty
  = assert (all isTyBinder (fst sty))   -- No CoVar binders here
    sty
  where sty = splitPiTys ty

-- | Splits a type into a PiTyVarBinder and a body, if possible.
tcSplitPiTy_maybe :: Type -> Maybe (PiTyVarBinder, Type)
tcSplitPiTy_maybe ty
  = assert (isMaybeTyBinder sty)  -- No CoVar binders here
    sty
  where
    sty = splitPiTy_maybe ty
    isMaybeTyBinder (Just (t,_)) = isTyBinder t
    isMaybeTyBinder _            = True

tcSplitForAllTyVarBinder_maybe :: Type -> Maybe (TyVarBinder, Type)
tcSplitForAllTyVarBinder_maybe ty | Just ty' <- coreView ty = tcSplitForAllTyVarBinder_maybe ty'
tcSplitForAllTyVarBinder_maybe (ForAllTy tv ty) = assert (isTyVarBinder tv ) Just (tv, ty)
tcSplitForAllTyVarBinder_maybe _                = Nothing

-- | Like 'tcSplitPiTys', but splits off only named binders,
-- returning just the tyvars.
tcSplitForAllTyVars :: Type -> ([TyVar], Type)
tcSplitForAllTyVars ty
  = assert (all isTyVar (fst sty)) sty
  where sty = splitForAllTyCoVars ty

-- | Like 'tcSplitForAllTyVars', but only splits 'ForAllTy's with 'Invisible'
-- type variable binders.
tcSplitForAllInvisTyVars :: Type -> ([TyVar], Type)
tcSplitForAllInvisTyVars ty = tcSplitSomeForAllTyVars isInvisibleForAllTyFlag ty

-- | Like 'tcSplitForAllTyVars', but only splits a 'ForAllTy' if @argf_pred argf@
-- is 'True', where @argf@ is the visibility of the @ForAllTy@'s binder and
-- @argf_pred@ is a predicate over visibilities provided as an argument to this
-- function.
tcSplitSomeForAllTyVars :: (ForAllTyFlag -> Bool) -> Type -> ([TyVar], Type)
tcSplitSomeForAllTyVars argf_pred ty
  = split ty ty []
  where
    split _ (ForAllTy (Bndr tv argf) ty) tvs
      | argf_pred argf                             = split ty ty (tv:tvs)
    split orig_ty ty tvs | Just ty' <- coreView ty = split orig_ty ty' tvs
    split orig_ty _                            tvs = (reverse tvs, orig_ty)

tcSplitForAllTyVarsReqTVBindersN :: Arity -> Type -> (Arity, [ForAllTyBinder], Type)
-- Split off at most N /required/ (aka visible) binders, plus any invisible ones
-- in the way, /and/ any trailing invisible ones
tcSplitForAllTyVarsReqTVBindersN n_req ty
  = split n_req ty ty []
  where
    split n_req _orig_ty (ForAllTy b@(Bndr _ argf) ty) bs
      | isVisibleForAllTyFlag argf, n_req > 0           = split (n_req - 1) ty ty (b:bs)
      | otherwise                                       = split n_req       ty ty (b:bs)
    split n_req orig_ty ty bs | Just ty' <- coreView ty = split n_req orig_ty ty' bs
    split n_req orig_ty _ty bs                          = (n_req, reverse bs, orig_ty)

-- | Like 'tcSplitForAllTyVars', but only splits 'ForAllTy's with 'Required' type
-- variable binders. All split tyvars are annotated with '()'.
tcSplitForAllReqTVBinders :: Type -> ([TcReqTVBinder], Type)
tcSplitForAllReqTVBinders ty = assert (all isTyVarBinder (fst sty) ) sty
  where sty = splitForAllReqTyBinders ty

-- | Like 'tcSplitForAllTyVars', but only splits 'ForAllTy's with 'Invisible' type
-- variable binders. All split tyvars are annotated with their 'Specificity'.
tcSplitForAllInvisTVBinders :: Type -> ([TcInvisTVBinder], Type)
tcSplitForAllInvisTVBinders ty = assert (all (isTyVar . binderVar) (fst sty)) sty
  where sty = splitForAllInvisTyBinders ty

-- | Like 'tcSplitForAllTyVars', but splits off only named binders.
tcSplitForAllTyVarBinders :: Type -> ([TyVarBinder], Type)
tcSplitForAllTyVarBinders ty = assert (all isTyVarBinder (fst sty)) sty
  where sty = splitForAllForAllTyBinders ty

tcSplitPredFunTy_maybe :: Type -> Maybe (PredType, Type)
-- Split off the first predicate argument from a type
tcSplitPredFunTy_maybe ty
  | Just ty' <- coreView ty = tcSplitPredFunTy_maybe ty'
tcSplitPredFunTy_maybe (FunTy { ft_af = af, ft_arg = arg, ft_res = res })
  | isInvisibleFunArg af
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

tcSplitSigmaTyBndrs :: Type -> ([TcInvisTVBinder], ThetaType, Type)
tcSplitSigmaTyBndrs ty = case tcSplitForAllInvisTVBinders ty of
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
    in (tvs1 ++ tvs2, theta1 ++ theta2, mkScaledFunTys arg_tys rho2)

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
tcTyConAppTyCon_maybe ty | Just ty' <- coreView ty = tcTyConAppTyCon_maybe ty'
tcTyConAppTyCon_maybe (TyConApp tc _)              = Just tc
tcTyConAppTyCon_maybe (FunTy { ft_af = af })       = Just (funTyFlagTyCon af)
tcTyConAppTyCon_maybe _                            = Nothing

tcTyConAppArgs :: Type -> [Type]
tcTyConAppArgs ty = case tcSplitTyConApp_maybe ty of
                        Just (_, args) -> args
                        Nothing        -> pprPanic "tcTyConAppArgs" (pprType ty)

-----------------------
tcSplitFunTys :: Type -> ([Scaled Type], Type)
tcSplitFunTys ty = case tcSplitFunTy_maybe ty of
                        Nothing        -> ([], ty)
                        Just (arg,res) -> (arg:args, res')
                                       where
                                          (args,res') = tcSplitFunTys res

tcSplitFunTy_maybe :: Type -> Maybe (Scaled Type, Type)
-- Only splits function (->) and (-=>), not (=>) or (==>)
tcSplitFunTy_maybe ty
  | Just ty' <- coreView ty = tcSplitFunTy_maybe ty'
tcSplitFunTy_maybe (FunTy { ft_af = af, ft_mult = w, ft_arg = arg, ft_res = res })
  | isVisibleFunArg af = Just (Scaled w arg, res)
tcSplitFunTy_maybe _   = Nothing
        -- Note the isVisibleFunArg guard
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
tcFunArgTy ty = fst (tcSplitFunTy ty)

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
tcSplitAppTy_maybe ty | Just ty' <- coreView ty = tcSplitAppTy_maybe ty'
tcSplitAppTy_maybe ty = tcSplitAppTyNoView_maybe ty

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

-----------------------
tcIsTyVarTy :: Type -> Bool
tcIsTyVarTy ty | Just ty' <- coreView ty = tcIsTyVarTy ty'
tcIsTyVarTy (CastTy ty _) = tcIsTyVarTy ty  -- look through casts, as
                                            -- this is only used for
                                            -- e.g., FlexibleContexts
tcIsTyVarTy (TyVarTy _)   = True
tcIsTyVarTy _             = False

-----------------------
tcSplitQuantPredTy :: Type -> ([TyVar], [Type], PredType)
-- Split up the type of a quantified predicate
--    forall tys, theta => head
-- NB splitFunTys, not tcSplitFunTys;
-- the latter specifically stops at PredTy arguments,
-- and we don't want to do that here
tcSplitQuantPredTy ty
  = case tcSplitForAllInvisTyVars ty of { (tvs, rho)    ->
    case splitFunTys rho             of { (theta, head) ->
    (tvs, map scaledThing theta, head) }}

tcSplitDFunTy :: Type -> ([TyVar], [Type], Class, [Type])
-- Split the type of a dictionary function
tcSplitDFunTy ty
  = case tcSplitQuantPredTy ty of { (tvs, theta, head)  ->
    case tcSplitDFunHead head  of { (clas, tys)   ->
    (tvs, theta, clas, tys) }}

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
-- This function is here in GHC.Tc.Utils.TcType, rather than in GHC.Tc.Validity,
-- because it is called from GHC.Tc.Solver, which itself is imported by GHC.Tc.Validity
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
             | otherwise -> Nothing -- Sigh: we do not have heterogeneous Coercible
                                    --       so we can't abstract over it
                                    -- Nothing fundamental: we could add it
 where
   k1 = typeKind ty1
   k2 = typeKind ty2
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
(However C.f. GHC.Tc.Solver.Dict.mk_strict_superclasses, which /does/
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
-- isSigmaTy returns true of any type with /invisible/ quantifiers at the top:
--     forall a. blah
--     Eq a => blah
--     ?x::Int => blah
-- But not
--     forall a -> blah
isSigmaTy (ForAllTy (Bndr _ af) _)     = isInvisibleForAllTyFlag af
isSigmaTy (FunTy { ft_af = af })       = isInvisibleFunArg af
isSigmaTy ty | Just ty' <- coreView ty = isSigmaTy ty'
isSigmaTy _                            = False


isRhoTy :: TcType -> Bool   -- True of TcRhoTypes; see Note [TcRhoType]
isRhoTy ty = not (isSigmaTy ty)

-- | Like 'isRhoTy', but also says 'True' for 'Infer' types
isRhoExpTy :: ExpType -> Bool
isRhoExpTy (Check ty) = isRhoTy ty
isRhoExpTy (Infer {}) = True

isOverloadedTy :: Type -> Bool
-- Yes for a type of a function that might require evidence-passing
-- Used by bindLocalMethods and for -fprof-late-overloaded
isOverloadedTy ty | Just ty' <- coreView ty = isOverloadedTy ty'
isOverloadedTy (ForAllTy _  ty)             = isOverloadedTy ty
isOverloadedTy (FunTy { ft_af = af })       = isInvisibleFunArg af
isOverloadedTy _                            = False

isFloatTy, isDoubleTy,
    isFloatPrimTy, isDoublePrimTy,
    isIntegerTy, isNaturalTy,
    isIntTy, isWordTy, isBoolTy,
    isUnitTy, isCharTy :: Type -> Bool
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

-- | Check whether the type is of the form @Any :: k@,
-- returning the kind @k@.
anyTy_maybe :: Type -> Maybe Kind
anyTy_maybe ty
  | Just (tc, [k]) <- splitTyConApp_maybe ty
  , getUnique tc == anyTyConKey
  = Just k
  | otherwise
  = Nothing

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
in PiTyVarBinder.

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
-- (tcSplitIOType_maybe t) returns Just (IO,t')
--              if t = IO t'
--              returns Nothing otherwise
tcSplitIOType_maybe ty
  = case tcSplitTyConApp_maybe ty of
        Just (io_tycon, [io_res_ty])
         | io_tycon `hasKey` ioTyConKey ->
            Just (io_tycon, io_res_ty)
        _ ->
            Nothing


{-
************************************************************************
*                                                                      *
        Visiblities
*                                                                      *
************************************************************************
-}

-- | For every arg a tycon can take, the returned list says True if the argument
-- is taken visibly, and False otherwise. Ends with an infinite tail of Trues to
-- allow for oversaturation.
tyConVisibilities :: TyCon -> [Bool]
tyConVisibilities tc = tc_binder_viss ++ tc_return_kind_viss ++ repeat True
  where
    tc_binder_viss      = map isVisibleTyConBinder (tyConBinders tc)
    tc_return_kind_viss = map isVisiblePiTyBinder (fst $ tcSplitPiTys (tyConResKind tc))

-- | If the tycon is applied to the types, is the next argument visible?
isNextTyConArgVisible :: TyCon -> [Type] -> Bool
isNextTyConArgVisible tc tys
  = tyConVisibilities tc `getNth` length tys

-- | Should this type be applied to a visible argument?
-- E.g. (s t): is `t` a visible argument of `s`?
isNextArgVisible :: TcType -> Bool
isNextArgVisible ty
  | Just (bndr, _) <- tcSplitPiTy_maybe (typeKind ty) = isVisiblePiTyBinder bndr
  | otherwise                                         = True
    -- this second case might happen if, say, we have an unzonked TauTv.
    -- But TauTvs can't range over types that take invisible arguments

{-
************************************************************************
*                                                                      *
                     Paterson sizes
*                                                                      *
************************************************************************
-}

{- Note [The PatersonSize of a type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The PatersonSize of type is something we can compare, with `ltPatersonSize`,
to determine if the Paterson conditions are satisfied for an instance
declaration.  See Note [Paterson conditions] in GHC.Tc.Validity.

There are some wrinkles

(PS1) Once we get into an implicit parameter or equality we
      can't get back to a class constraint, so it's safe
      to say "size 0".  See #4200.

      We do this with isTerminatingClass

Note [Invisible arguments and termination]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking the ​Paterson conditions for termination an instance
declaration, we check for the number of "constructors and variables"
in the instance head and constraints. Question: Do we look at

 * All the arguments, visible or invisible?
 * Just the visible arguments?

I think both will ensure termination, provided we are consistent.
Currently we are /not/ consistent, which is really a bug.  It's
described in #15177, which contains a number of examples.
The suspicious bits are the calls to filterOutInvisibleTypes.
See also #11833.

Note [Stuck type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~
A type-family application generally has infinite size (PS_TyFam);
see (PC3) in Note [Paterson conditions] in GHC.Tc.Validity.

But a couple of built-in type families have no axioms, and can never
expand into anything else.  They are:

* (TypeError "stuff").  E.g. consider

     type family F a where
       F Int  = Bool
       F Bool = Char
       F _    = TypeError "Bad"

  We don't want to complain about possible non-termination of F, in
  GHC.Tc.Validity.checkFamInstRhs.  cf indexed-types/should_fail/T13271

* (Any @k).

For now we treat them as being size zero, but (#22696) I think we should
actually treat them as big (like any other ype family) because we don't
want to abstract over them in e.g. validDerivPred.

The type-family termination test, in GHC.Tc.Validity.checkFamInstRhs, already
has a separate call to isStuckTypeFamily, so the `F` above will still be accepted.
-}

-- | Why did the Paterson conditions fail; that is, why
-- was the context P not Paterson-smaller than the head H?
--
-- See Note [Paterson conditions] in GHC.Tc.Validity.
data PatersonCondFailure
  -- | Some type variables occur more often in P than in H.
  -- See (PC1) in Note [Paterson conditions] in GHC.Tc.Validity.
  = PCF_TyVar
    [TyVar]  -- ^ the type variables which appear more often in the context
  -- | P is not smaller in size than H.
  -- See (PC2) in Note [Paterson conditions] in GHC.Tc.Validity.
  | PCF_Size
  -- | P contains a type family.
  -- See (PC3) in Note [Paterson conditions] in GHC.Tc.Validity.
  | PCF_TyFam
    TyCon  -- ^ the type constructor of the type family

-- | Indicates whether a Paterson condition failure occurred in an instance declaration or a type family equation.
-- Useful for differentiating context in error messages.
data PatersonCondFailureContext
  = InInstanceDecl
  | InTyFamEquation

--------------------------------------

-- | The Paterson size of a given type, in the sense of
-- Note [Paterson conditions] in GHC.Tc.Validity
--
--   - after expanding synonyms,
--   - ignoring coercions (as they are not user written).
data PatersonSize
  -- | The type mentions a type family, so the size could be anything.
  = PS_TyFam TyCon
  -- | The type does not mention a type family.
  | PS_Vanilla { ps_tvs :: [TyVar]  -- ^ free tyvars, including repetitions;
               , ps_size :: Int     -- ^ number of type constructors and variables
    }
  -- ToDo: ignore invisible arguments?  See Note [Invisible arguments and termination]

instance Outputable PatersonSize where
  ppr (PS_TyFam tc) = text "PS_TyFam" <+> ppr tc
  ppr (PS_Vanilla { ps_tvs = tvs, ps_size = size })
    = text "PS_Vanilla" <> braces (sep [ text "ps_tvs =" <+> ppr tvs <> comma
                                       , text "ps_size =" <+> int size ])

pSizeZero, pSizeOne :: PatersonSize
pSizeZero = PS_Vanilla { ps_tvs = [], ps_size = 0 }
pSizeOne  = PS_Vanilla { ps_tvs = [], ps_size = 1 }

-- | @ltPatersonSize ps1 ps2@ returns:
--
--  - @Nothing@ iff @ps1@ is definitely strictly smaller than @ps2@,
--  - @Just ps_fail@ otherwise; @ps_fail@ says what went wrong.
ltPatersonSize :: PatersonSize
               -> PatersonSize
               -> Maybe PatersonCondFailure
ltPatersonSize (PS_Vanilla { ps_tvs = tvs1, ps_size = s1 })
               (PS_Vanilla { ps_tvs = tvs2, ps_size = s2 })
  | s1 >= s2                                = Just PCF_Size
  | bad_tvs@(_:_) <- noMoreTyVars tvs1 tvs2 = Just (PCF_TyVar bad_tvs)
  | otherwise                               = Nothing -- OK!
ltPatersonSize (PS_TyFam tc) _ = Just (PCF_TyFam tc)
ltPatersonSize _ (PS_TyFam tc) = Just (PCF_TyFam tc)
  -- NB: this last equation is never taken when checking instances, because
  -- type families are disallowed in instance heads.
  --
  -- However, this function is also used in the logic for solving superclass
  -- constraints (see Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance),
  -- in which case we might well hit this case (see e.g. T23171).

noMoreTyVars :: [TyVar]  -- Free vars (with repetitions) of the constraint C
             -> [TyVar]  -- Free vars (with repetitions) of the head H
             -> [TyVar]  -- TyVars that appear more often in C than H;
                         --   no repetitions in this list
noMoreTyVars tvs head_tvs
  = nub (tvs \\ head_tvs)  -- The (\\) is list difference; e.g.
                           --   [a,b,a,a] \\ [a,a] = [b,a]
                           -- So we are counting repetitions

addPSize :: PatersonSize -> PatersonSize -> PatersonSize
addPSize ps1@(PS_TyFam {}) _ = ps1
addPSize _ ps2@(PS_TyFam {}) = ps2
addPSize (PS_Vanilla { ps_tvs = tvs1, ps_size = s1 })
         (PS_Vanilla { ps_tvs = tvs2, ps_size = s2 })
  = PS_Vanilla { ps_tvs = tvs1 ++ tvs2, ps_size = s1 + s2 }
    -- (++) is not very performant, but the types
    -- are user-written and never large

pSizeType :: Type -> PatersonSize
pSizeType = pSizeTypeX emptyVarSet

pSizeTypes :: [Type] -> PatersonSize
pSizeTypes = pSizeTypesX emptyVarSet pSizeZero

-- Paterson size of a type, retaining repetitions, and expanding synonyms
-- This ignores coercions, as coercions aren't user-written
pSizeTypeX :: VarSet -> Type -> PatersonSize
pSizeTypeX bvs ty | Just exp_ty <- coreView ty = pSizeTypeX bvs exp_ty
pSizeTypeX bvs (TyVarTy tv)
  | tv `elemVarSet` bvs                  = pSizeOne
  | otherwise                            = PS_Vanilla { ps_tvs = [tv], ps_size = 1 }
pSizeTypeX _   (LitTy {})                = pSizeOne
pSizeTypeX bvs (TyConApp tc tys)         = pSizeTyConAppX bvs tc tys
pSizeTypeX bvs (AppTy fun arg)           = pSizeTypeX bvs fun `addPSize` pSizeTypeX bvs arg
pSizeTypeX bvs (FunTy _ w arg res)       = pSizeTypeX bvs w `addPSize` pSizeTypeX bvs arg `addPSize`
                                           pSizeTypeX bvs res
pSizeTypeX bvs (ForAllTy (Bndr tv _) ty) = pSizeTypeX bvs (tyVarKind tv) `addPSize`
                                           pSizeTypeX (bvs `extendVarSet` tv) ty
pSizeTypeX bvs (CastTy ty _)             = pSizeTypeX bvs ty
pSizeTypeX _   (CoercionTy {})           = pSizeOne

pSizeTypesX :: VarSet -> PatersonSize -> [Type] -> PatersonSize
pSizeTypesX bvs sz tys = foldr (addPSize . pSizeTypeX bvs) sz tys

pSizeTyConApp :: TyCon -> [Type] -> PatersonSize
pSizeTyConApp = pSizeTyConAppX emptyVarSet

pSizeTyConAppX :: VarSet -> TyCon -> [Type] -> PatersonSize
-- Open question: do we count all args, or just the visible ones?
-- See Note [Invisible arguments and termination]
pSizeTyConAppX bvs tc tys
  | isTypeFamilyTyCon tc = pSizeTyFamApp tc
  | otherwise            = pSizeTypesX bvs pSizeOne tys

pSizeTyFamApp :: TyCon -> PatersonSize
-- See Note [Stuck type families]
pSizeTyFamApp tc
 | isStuckTypeFamily tc = pSizeZero
 | otherwise            = PS_TyFam tc

pSizeClassPred :: Class -> [Type] -> PatersonSize
pSizeClassPred = pSizeClassPredX emptyVarSet

pSizeClassPredX :: VarSet -> Class -> [Type] -> PatersonSize
pSizeClassPredX bvs cls tys
  | isTerminatingClass cls -- See (PS1) in Note [The PatersonSize of a type]
  = pSizeZero
  | otherwise
  = pSizeTypesX bvs pSizeOne $
    filterOutInvisibleTypes (classTyCon cls) tys
    -- filterOutInvisibleTypes Yuk!  See Note [Invisible arguments and termination]

isStuckTypeFamily :: TyCon -> Bool
-- See Note [Stuck type families]
isStuckTypeFamily tc
  =  tc `hasKey` errorMessageTypeErrorFamKey
  || tc `hasKey` anyTyConKey

-- | When this says "True", ignore this class constraint during
-- a termination check
-- See (PS1) in Note [The PatersonSize of a type]
isTerminatingClass :: Class -> Bool
isTerminatingClass cls
  = isIPClass cls    -- Implicit parameter constraints always terminate because
                     -- there are no instances for them --- they are only solved
                     -- by "local instances" in expressions
    || isEqualityClass cls
    || cls `hasKey` typeableClassKey
            -- Typeable constraints are bigger than they appear due
            -- to kind polymorphism, but we can never get instance divergence this way
    || cls `hasKey` unsatisfiableClassNameKey

allDistinctTyVars :: TyVarSet -> [KindOrType] -> Bool
-- (allDistinctTyVars tvs tys) returns True if tys are
-- a) all tyvars
-- b) all distinct
-- c) disjoint from tvs
allDistinctTyVars _    [] = True
allDistinctTyVars tkvs (ty : tys)
  = case getTyVar_maybe ty of
      Nothing -> False
      Just tv | tv `elemVarSet` tkvs -> False
              | otherwise -> allDistinctTyVars (tkvs `extendVarSet` tv) tys

-----------------------
type TypeSize = IntWithInf

sizeType :: Type -> TypeSize
-- Size of a type: the number of variables and constructors
sizeType ty = toTypeSize (pSizeType ty)

sizeTypes :: [Type] -> TypeSize
sizeTypes tys = toTypeSize (foldr (addPSize . pSizeType) pSizeZero tys)

toTypeSize :: PatersonSize -> TypeSize
toTypeSize (PS_TyFam {})                   =  infinity
toTypeSize (PS_Vanilla { ps_size = size }) = mkIntWithInf size
