{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcType]{Types used in the typechecker}

This module provides the Type interface for front-end parts of the
compiler.  These parts

        * treat "source types" as opaque:
                newtypes, and predicates are meaningful.
        * look through usage types

The "tc" prefix is for "TypeChecker", because the type checker
is the principal client.
-}

{-# LANGUAGE CPP, MultiWayIf, FlexibleContexts #-}

module TcType (
  --------------------------------
  -- Types
  TcType, TcSigmaType, TcRhoType, TcTauType, TcPredType, TcThetaType,
  TcTyVar, TcTyVarSet, TcDTyVarSet, TcTyCoVarSet, TcDTyCoVarSet,
  TcKind, TcCoVar, TcTyCoVar, TcTyVarBinder, TcTyCon,

  ExpType(..), InferResult(..), ExpSigmaType, ExpRhoType, mkCheckExpType,

  SyntaxOpType(..), synKnownType, mkSynFunTys,

  -- TcLevel
  TcLevel(..), topTcLevel, pushTcLevel, isTopTcLevel,
  strictlyDeeperThan, sameDepthAs, fmvTcLevel,
  tcTypeLevel, tcTyVarLevel, maxTcLevel,

  --------------------------------
  -- MetaDetails
  UserTypeCtxt(..), pprUserTypeCtxt, isSigMaybe,
  TcTyVarDetails(..), pprTcTyVarDetails, vanillaSkolemTv, superSkolemTv,
  MetaDetails(Flexi, Indirect), MetaInfo(..),
  isImmutableTyVar, isSkolemTyVar, isMetaTyVar,  isMetaTyVarTy, isTyVarTy,
  isSigTyVar, isOverlappableTyVar,  isTyConableTyVar,
  isFskTyVar, isFmvTyVar, isFlattenTyVar,
  isAmbiguousTyVar, metaTyVarRef, metaTyVarInfo,
  isFlexi, isIndirect, isRuntimeUnkSkol,
  metaTyVarTcLevel, setMetaTyVarTcLevel, metaTyVarTcLevel_maybe,
  isTouchableMetaTyVar, isTouchableOrFmv,
  isFloatedTouchableMetaTyVar,

  --------------------------------
  -- Builders
  mkPhiTy, mkInfSigmaTy, mkSpecSigmaTy, mkSigmaTy,
  mkNakedTyConApp, mkNakedAppTys, mkNakedAppTy,
  mkNakedCastTy,

  --------------------------------
  -- Splitters
  -- These are important because they do not look through newtypes
  getTyVar,
  tcSplitForAllTy_maybe,
  tcSplitForAllTys, tcSplitPiTys, tcSplitPiTy_maybe, tcSplitForAllTyVarBndrs,
  tcSplitPhiTy, tcSplitPredFunTy_maybe,
  tcSplitFunTy_maybe, tcSplitFunTys, tcFunArgTy, tcFunResultTy, tcFunResultTyN,
  tcSplitFunTysN,
  tcSplitTyConApp, tcSplitTyConApp_maybe,
  tcRepSplitTyConApp_maybe, tcRepSplitTyConApp_maybe',
  tcTyConAppTyCon, tcTyConAppTyCon_maybe, tcTyConAppArgs,
  tcSplitAppTy_maybe, tcSplitAppTy, tcSplitAppTys, tcRepSplitAppTy_maybe,
  tcRepGetNumAppTys,
  tcGetCastedTyVar_maybe, tcGetTyVar_maybe, tcGetTyVar, nextRole,
  tcSplitSigmaTy, tcSplitNestedSigmaTys, tcDeepSplitSigmaTy_maybe,

  ---------------------------------
  -- Predicates.
  -- Again, newtypes are opaque
  eqType, eqTypes, nonDetCmpType, nonDetCmpTypes, eqTypeX,
  pickyEqType, tcEqType, tcEqKind, tcEqTypeNoKindCheck, tcEqTypeVis,
  isSigmaTy, isRhoTy, isRhoExpTy, isOverloadedTy,
  isFloatingTy, isDoubleTy, isFloatTy, isIntTy, isWordTy, isStringTy,
  isIntegerTy, isBoolTy, isUnitTy, isCharTy, isCallStackTy, isCallStackPred,
  isTauTy, isTauTyCon, tcIsTyVarTy, tcIsForAllTy,
  isPredTy, isTyVarClassPred, isTyVarExposed, isInsolubleOccursCheck,
  checkValidClsArgs, hasTyVarHead,
  isRigidEqPred, isRigidTy,

  ---------------------------------
  -- Misc type manipulators

  deNoteType,
  orphNamesOfType, orphNamesOfCo,
  orphNamesOfTypes, orphNamesOfCoCon,
  getDFunTyKey,
  evVarPred_maybe, evVarPred,

  ---------------------------------
  -- Predicate types
  mkMinimalBySCs, transSuperClasses,
  pickQuantifiablePreds, pickCapturedPreds,
  immSuperClasses,
  isImprovementPred,

  -- * Finding type instances
  tcTyFamInsts,

  -- * Finding "exact" (non-dead) type variables
  exactTyCoVarsOfType, exactTyCoVarsOfTypes,
  candidateQTyVarsOfType, candidateQTyVarsOfTypes, CandidatesQTvs(..),
  anyRewritableTyVar,

  -- * Extracting bound variables
  allBoundVariables, allBoundVariabless,

  ---------------------------------
  -- Foreign import and export
  isFFIArgumentTy,     -- :: DynFlags -> Safety -> Type -> Bool
  isFFIImportResultTy, -- :: DynFlags -> Type -> Bool
  isFFIExportResultTy, -- :: Type -> Bool
  isFFIExternalTy,     -- :: Type -> Bool
  isFFIDynTy,          -- :: Type -> Type -> Bool
  isFFIPrimArgumentTy, -- :: DynFlags -> Type -> Bool
  isFFIPrimResultTy,   -- :: DynFlags -> Type -> Bool
  isFFILabelTy,        -- :: Type -> Bool
  isFFITy,             -- :: Type -> Bool
  isFunPtrTy,          -- :: Type -> Bool
  tcSplitIOType_maybe, -- :: Type -> Maybe Type

  --------------------------------
  -- Rexported from Kind
  Kind, typeKind,
  liftedTypeKind,
  constraintKind,
  isLiftedTypeKind, isUnliftedTypeKind, classifiesTypeWithValues,

  --------------------------------
  -- Rexported from Type
  Type, PredType, ThetaType, TyBinder, ArgFlag(..),

  mkForAllTy, mkForAllTys, mkInvForAllTys, mkSpecForAllTys, mkInvForAllTy,
  mkFunTy, mkFunTys,
  mkTyConApp, mkAppTy, mkAppTys,
  mkTyConTy, mkTyVarTy,
  mkTyVarTys,

  isClassPred, isEqPred, isNomEqPred, isIPPred,
  mkClassPred,
  isDictLikeTy,
  tcSplitDFunTy, tcSplitDFunHead, tcSplitMethodTy,
  isRuntimeRepVar, isKindLevPoly,
  isVisibleBinder, isInvisibleBinder,

  -- Type substitutions
  TCvSubst(..),         -- Representation visible to a few friends
  TvSubstEnv, emptyTCvSubst,
  zipTvSubst,
  mkTvSubstPrs, notElemTCvSubst, unionTCvSubst,
  getTvSubstEnv, setTvSubstEnv, getTCvInScope, extendTCvInScope,
  extendTCvInScopeList, extendTCvInScopeSet, extendTvSubstAndInScope,
  Type.lookupTyVar, Type.extendTCvSubst, Type.substTyVarBndr,
  Type.extendTvSubst,
  isInScope, mkTCvSubst, mkTvSubst, zipTyEnv, zipCoEnv,
  Type.substTy, substTys, substTyWith, substTyWithCoVars,
  substTyAddInScope,
  substTyUnchecked, substTysUnchecked, substThetaUnchecked,
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
  -- Transforming Types to TcTypes
  toTcType,    -- :: Type -> TcType
  toTcTypeBag, -- :: Bag EvVar -> Bag EvVar

  pprKind, pprParendKind, pprSigmaType,
  pprType, pprParendType, pprTypeApp, pprTyThingCategory, tyThingCategory,
  pprTheta, pprParendTheta, pprThetaArrowTy, pprClassPred,
  pprTvBndr, pprTvBndrs,

  TypeSize, sizeType, sizeTypes, toposortTyVars,

  ---------------------------------
  -- argument visibility
  tcTyConVisibilities, isNextTyConArgVisible, isNextArgVisible

  ) where

#include "HsVersions.h"

-- friends:
import Kind
import TyCoRep
import Class
import Var
import ForeignCall
import VarSet
import Coercion
import Type
import RepType
import TyCon

-- others:
import DynFlags
import CoreFVs
import Name -- hiding (varName)
            -- We use this to make dictionaries for type literals.
            -- Perhaps there's a better way to do this?
import NameSet
import VarEnv
import PrelNames
import TysWiredIn( coercibleClass, unitTyCon, unitTyConKey
                 , listTyCon, constraintKind )
import BasicTypes
import Util
import Bag
import Maybes
import ListSetOps ( getNth )
import Outputable
import FastString
import ErrUtils( Validity(..), MsgDoc, isValid )
import FV
import qualified GHC.LanguageExtensions as LangExt

import Data.IORef
import Data.Functor.Identity

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

Note [TcTyVars in the typechecker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker uses a lot of type variables with special properties,
notably being a unification variable with a mutable reference.  These
use the 'TcTyVar' variant of Var.Var.

However, the type checker and constraint solver can encounter type
variables that use the 'TyVar' variant of Var.Var, for a couple of
reasons:

  - When unifying or flattening under (forall a. ty)

  - When typechecking a class decl, say
       class C (a :: k) where
          foo :: T a -> Int
    We have first kind-check the header; fix k and (a:k) to be
    TyVars, bring 'k' and 'a' into scope, and kind check the
    signature for 'foo'.  In doing so we call solveEqualities to
    solve any kind equalities in foo's signature.  So the solver
    may see free occurrences of 'k'.

It's convenient to simply treat these TyVars as skolem constants,
which of course they are.  So

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
in addition to type variables! As a result, there are some places in TcType
where we must take care to check that a variable is a _type_ variable (using
isTyVar) before calling tcTyVarDetails--a partial function that is not defined
for coercion variables--on the variable. Failing to do so led to
GHC Trac #12785.
-}

-- See Note [TcTyVars in the typechecker]
type TcTyVar = TyVar    -- Used only during type inference
type TcCoVar = CoVar    -- Used only during type inference
type TcType = Type      -- A TcType can have mutable type variables
type TcTyCoVar = Var    -- Either a TcTyVar or a CoVar
        -- Invariant on ForAllTy in TcTypes:
        --      forall a. T
        -- a cannot occur inside a MutTyVar in T; that is,
        -- T is "flattened" before quantifying over a

type TcTyVarBinder = TyVarBinder
type TcTyCon       = TyCon   -- these can be the TcTyCon constructor

-- These types do not have boxy type variables in them
type TcPredType     = PredType
type TcThetaType    = ThetaType
type TcSigmaType    = TcType
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
-- See Note [ExpType] in TcMType, where you'll also find manipulators.
data ExpType = Check TcType
             | Infer !InferResult

data InferResult
  = IR { ir_uniq :: Unique  -- For debugging only
       , ir_lvl  :: TcLevel -- See Note [TcLevel of ExpType] in TcMType
       , ir_inst :: Bool    -- True <=> deeply instantiate before returning
                            --           i.e. return a RhoType
                            -- False <=> do not instantiate before returning
                            --           i.e. return a SigmaType
       , ir_ref  :: IORef (Maybe TcType) }
         -- The type that fills in this hole should be a Type,
         -- that is, its kind should be (TYPE rr) for some rr

type ExpSigmaType = ExpType
type ExpRhoType   = ExpType

instance Outputable ExpType where
  ppr (Check ty) = text "Check" <> braces (ppr ty)
  ppr (Infer ir) = ppr ir

instance Outputable InferResult where
  ppr (IR { ir_uniq = u, ir_lvl = lvl
          , ir_inst = inst })
    = text "Infer" <> braces (ppr u <> comma <> ppr lvl <+> ppr inst)

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
-- This is defined here to avoid defining it in TcExpr.hs-boot.
data SyntaxOpType
  = SynAny     -- ^ Any type
  | SynRho     -- ^ A rho type, deeply skolemised or instantiated as appropriate
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
A TcRhoType has no foralls or contexts at the top, or to the right of an arrow
  YES    (forall a. a->a) -> Int
  NO     forall a. a ->  Int
  NO     Eq a => a -> a
  NO     Int -> forall a. a -> Int


************************************************************************
*                                                                      *
        TyVarDetails, MetaDetails, MetaInfo
*                                                                      *
************************************************************************

TyVarDetails gives extra info about type variables, used during type
checking.  It's attached to mutable type variables only.
It's knot-tied back to Var.hs.  There is no reason in principle
why Var.hs shouldn't actually have the definition, but it "belongs" here.

Note [Signature skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
A SigTv is a specialised variant of TauTv, with the following invarints:

    * A SigTv can be unified only with a TyVar,
      not with any other type

    * Its MetaDetails, if filled in, will always be another SigTv
      or a SkolemTv

SigTvs are only distinguished to improve error messages.
Consider this

  f :: forall a. [a] -> Int
  f (x::b : xs) = 3

Here 'b' is a lexically scoped type variable, but it turns out to be
the same as the skolem 'a'.  So we make them both SigTvs, which can unify
with each other.

Similarly consider
  data T (a:k1) = MkT (S a)
  data S (b:k2) = MkS (T b)
When doing kind inference on {S,T} we don't want *skolems* for k1,k2,
because they end up unifying; we want those SigTvs again.

SigTvs are used *only* for pattern type signatures.

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
       TcLevel    -- Level of the implication that binds it
       Bool       -- True <=> this skolem type variable can be overlapped
                  --          when looking up instances
                  -- See Note [Binding when looking up instances] in InstEnv

  | RuntimeUnk    -- Stands for an as-yet-unknown type in the GHCi
                  -- interactive context

  | MetaTv { mtv_info  :: MetaInfo
           , mtv_ref   :: IORef MetaDetails
           , mtv_tclvl :: TcLevel }  -- See Note [TcLevel and untouchable type variables]

vanillaSkolemTv, superSkolemTv :: TcTyVarDetails
-- See Note [Binding when looking up instances] in InstEnv
vanillaSkolemTv = SkolemTv (pushTcLevel topTcLevel) False  -- Might be instantiated
superSkolemTv   = SkolemTv (pushTcLevel topTcLevel) True   -- Treat this as a completely distinct type

-----------------------------
data MetaDetails
  = Flexi  -- Flexi type variables unify to become Indirects
  | Indirect TcType

data MetaInfo
   = TauTv         -- This MetaTv is an ordinary unification variable
                   -- A TauTv is always filled in with a tau-type, which
                   -- never contains any ForAlls.

   | SigTv         -- A variant of TauTv, except that it should not be
                   --   unified with a type, only with a type variable
                   -- See Note [Signature skolems]

   | FlatMetaTv    -- A flatten meta-tyvar
                   -- It is a meta-tyvar, but it is always untouchable, with level 0
                   -- See Note [The flattening story] in TcFlatten

   | FlatSkolTv    -- A flatten skolem tyvar
                   -- Just like FlatMetaTv, but is comletely "owned" by
                   --   its Given CFunEqCan.
                   -- It is filled in /only/ by unflattenGivens
                   -- See Note [The flattening story] in TcFlatten

instance Outputable MetaDetails where
  ppr Flexi         = text "Flexi"
  ppr (Indirect ty) = text "Indirect" <+> ppr ty

pprTcTyVarDetails :: TcTyVarDetails -> SDoc
-- For debugging
pprTcTyVarDetails (RuntimeUnk {})      = text "rt"
pprTcTyVarDetails (SkolemTv lvl True)  = text "ssk" <> colon <> ppr lvl
pprTcTyVarDetails (SkolemTv lvl False) = text "sk"  <> colon <> ppr lvl
pprTcTyVarDetails (MetaTv { mtv_info = info, mtv_tclvl = tclvl })
  = pp_info <> colon <> ppr tclvl
  where
    pp_info = case info of
                TauTv      -> text "tau"
                SigTv      -> text "sig"
                FlatMetaTv -> text "fmv"
                FlatSkolTv -> text "fsk"


{- *********************************************************************
*                                                                      *
          UserTypeCtxt
*                                                                      *
********************************************************************* -}

-------------------------------------
-- UserTypeCtxt describes the origin of the polymorphic type
-- in the places where we need to an expression has that type

data UserTypeCtxt
  = FunSigCtxt      -- Function type signature, when checking the type
                    -- Also used for types in SPECIALISE pragmas
       Name              -- Name of the function
       Bool              -- True <=> report redundant constraints
                            -- This is usually True, but False for
                            --   * Record selectors (not important here)
                            --   * Class and instance methods.  Here
                            --     the code may legitimately be more
                            --     polymorphic than the signature
                            --     generated from the class
                            --     declaration

  | InfSigCtxt Name     -- Inferred type for function
  | ExprSigCtxt         -- Expression type signature
  | TypeAppCtxt         -- Visible type application
  | ConArgCtxt Name     -- Data constructor argument
  | TySynCtxt Name      -- RHS of a type synonym decl
  | PatSynCtxt Name     -- Type sig for a pattern synonym
  | PatSigCtxt          -- Type sig in pattern
                        --   eg  f (x::t) = ...
                        --   or  (x::t, y) = e
  | RuleSigCtxt Name    -- LHS of a RULE forall
                        --    RULE "foo" forall (x :: a -> a). f (Just x) = ...
  | ResSigCtxt          -- Result type sig
                        --      f x :: t = ....
  | ForSigCtxt Name     -- Foreign import or export signature
  | DefaultDeclCtxt     -- Types in a default declaration
  | InstDeclCtxt        -- An instance declaration
  | SpecInstCtxt        -- SPECIALISE instance pragma
  | ThBrackCtxt         -- Template Haskell type brackets [t| ... |]
  | GenSigCtxt          -- Higher-rank or impredicative situations
                        -- e.g. (f e) where f has a higher-rank type
                        -- We might want to elaborate this
  | GhciCtxt            -- GHCi command :kind <type>

  | ClassSCCtxt Name    -- Superclasses of a class
  | SigmaCtxt           -- Theta part of a normal for-all type
                        --      f :: <S> => a -> a
  | DataTyCtxt Name     -- The "stupid theta" part of a data decl
                        --      data <S> => T a = MkT a

{-
-- Notes re TySynCtxt
-- We allow type synonyms that aren't types; e.g.  type List = []
--
-- If the RHS mentions tyvars that aren't in scope, we'll
-- quantify over them:
--      e.g.    type T = a->a
-- will become  type T = forall a. a->a
--
-- With gla-exts that's right, but for H98 we should complain.
-}


pprUserTypeCtxt :: UserTypeCtxt -> SDoc
pprUserTypeCtxt (FunSigCtxt n _)  = text "the type signature for" <+> quotes (ppr n)
pprUserTypeCtxt (InfSigCtxt n)    = text "the inferred type for" <+> quotes (ppr n)
pprUserTypeCtxt (RuleSigCtxt n)   = text "a RULE for" <+> quotes (ppr n)
pprUserTypeCtxt ExprSigCtxt       = text "an expression type signature"
pprUserTypeCtxt TypeAppCtxt       = text "a type argument"
pprUserTypeCtxt (ConArgCtxt c)    = text "the type of the constructor" <+> quotes (ppr c)
pprUserTypeCtxt (TySynCtxt c)     = text "the RHS of the type synonym" <+> quotes (ppr c)
pprUserTypeCtxt ThBrackCtxt       = text "a Template Haskell quotation [t|...|]"
pprUserTypeCtxt PatSigCtxt        = text "a pattern type signature"
pprUserTypeCtxt ResSigCtxt        = text "a result type signature"
pprUserTypeCtxt (ForSigCtxt n)    = text "the foreign declaration for" <+> quotes (ppr n)
pprUserTypeCtxt DefaultDeclCtxt   = text "a type in a `default' declaration"
pprUserTypeCtxt InstDeclCtxt      = text "an instance declaration"
pprUserTypeCtxt SpecInstCtxt      = text "a SPECIALISE instance pragma"
pprUserTypeCtxt GenSigCtxt        = text "a type expected by the context"
pprUserTypeCtxt GhciCtxt          = text "a type in a GHCi command"
pprUserTypeCtxt (ClassSCCtxt c)   = text "the super-classes of class" <+> quotes (ppr c)
pprUserTypeCtxt SigmaCtxt         = text "the context of a polymorphic type"
pprUserTypeCtxt (DataTyCtxt tc)   = text "the context of the data type declaration for" <+> quotes (ppr tc)
pprUserTypeCtxt (PatSynCtxt n)    = text "the signature for pattern synonym" <+> quotes (ppr n)

isSigMaybe :: UserTypeCtxt -> Maybe Name
isSigMaybe (FunSigCtxt n _) = Just n
isSigMaybe (ConArgCtxt n)   = Just n
isSigMaybe (ForSigCtxt n)   = Just n
isSigMaybe (PatSynCtxt n)   = Just n
isSigMaybe _                = Nothing


{- *********************************************************************
*                                                                      *
                Untoucable type variables
*                                                                      *
********************************************************************* -}

newtype TcLevel = TcLevel Int deriving( Eq, Ord )
  -- See Note [TcLevel and untouchable type variables] for what this Int is
  -- See also Note [TcLevel assignment]

{-
Note [TcLevel and untouchable type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Each unification variable (MetaTv)
  and each Implication
  has a level number (of type TcLevel)

* INVARIANTS.  In a tree of Implications,

    (ImplicInv) The level number of an Implication is
                STRICTLY GREATER THAN that of its parent

    (MetaTvInv) The level number of a unification variable is
                LESS THAN OR EQUAL TO that of its parent
                implication

* A unification variable is *touchable* if its level number
  is EQUAL TO that of its immediate parent implication.

* INVARIANT
    (GivenInv)  The free variables of the ic_given of an
                implication are all untouchable; ie their level
                numbers are LESS THAN the ic_tclvl of the implication

Note [Skolem escape prevention]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only unify touchable unification variables.  Because of
(MetaTvInv), there can be no occurrences of the variable further out,
so the unification can't cause the skolems to escape. Example:
     data T = forall a. MkT a (a->Int)
     f x (MkT v f) = length [v,x]
We decide (x::alpha), and generate an implication like
      [1]forall a. (a ~ alpha[0])
But we must not unify alpha:=a, because the skolem would escape.

For the cases where we DO want to unify, we rely on floating the
equality.   Example (with same T)
     g x (MkT v f) = x && True
We decide (x::alpha), and generate an implication like
      [1]forall a. (Bool ~ alpha[0])
We do NOT unify directly, bur rather float out (if the constraint
does not mention 'a') to get
      (Bool ~ alpha[0]) /\ [1]forall a.()
and NOW we can unify alpha.

The same idea of only unifying touchables solves another problem.
Suppose we had
   (F Int ~ uf[0])  /\  [1](forall a. C a => F Int ~ beta[1])
In this example, beta is touchable inside the implication. The
first solveSimpleWanteds step leaves 'uf' un-unified. Then we move inside
the implication where a new constraint
       uf  ~  beta
emerges. If we (wrongly) spontaneously solved it to get uf := beta,
the whole implication disappears but when we pop out again we are left with
(F Int ~ uf) which will be unified by our final zonking stage and
uf will get unified *once more* to (F Int).

Note [TcLevel assignment]
~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange the TcLevels like this

   0   Level for all flatten meta-vars
   1   Top level
   2   First-level implication constraints
   3   Second-level implication constraints
   ...etc...

The flatten meta-vars are all at level 0, just to make them untouchable.
-}

maxTcLevel :: TcLevel -> TcLevel -> TcLevel
maxTcLevel (TcLevel a) (TcLevel b) = TcLevel (a `max` b)

fmvTcLevel :: TcLevel
-- See Note [TcLevel assignment]
fmvTcLevel = TcLevel 0

topTcLevel :: TcLevel
-- See Note [TcLevel assignment]
topTcLevel = TcLevel 1   -- 1 = outermost level

isTopTcLevel :: TcLevel -> Bool
isTopTcLevel (TcLevel 1) = True
isTopTcLevel _           = False

pushTcLevel :: TcLevel -> TcLevel
-- See Note [TcLevel assignment]
pushTcLevel (TcLevel us) = TcLevel (us + 1)

strictlyDeeperThan :: TcLevel -> TcLevel -> Bool
strictlyDeeperThan (TcLevel tv_tclvl) (TcLevel ctxt_tclvl)
  = tv_tclvl > ctxt_tclvl

sameDepthAs :: TcLevel -> TcLevel -> Bool
sameDepthAs (TcLevel ctxt_tclvl) (TcLevel tv_tclvl)
  = ctxt_tclvl == tv_tclvl   -- NB: invariant ctxt_tclvl >= tv_tclvl
                             --     So <= would be equivalent

checkTcLevelInvariant :: TcLevel -> TcLevel -> Bool
-- Checks (MetaTvInv) from Note [TcLevel and untouchable type variables]
checkTcLevelInvariant (TcLevel ctxt_tclvl) (TcLevel tv_tclvl)
  = ctxt_tclvl >= tv_tclvl

tcTyVarLevel :: TcTyVar -> TcLevel
tcTyVarLevel tv
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
          MetaTv { mtv_tclvl = tv_lvl } -> tv_lvl
          SkolemTv tv_lvl _             -> tv_lvl
          RuntimeUnk                    -> topTcLevel

tcTypeLevel :: TcType -> TcLevel
-- Max level of any free var of the type
tcTypeLevel ty
  = foldDVarSet add topTcLevel (tyCoVarsOfTypeDSet ty)
  where
    add v lvl
      | isTcTyVar v = lvl `maxTcLevel` tcTyVarLevel v
      | otherwise   = lvl

instance Outputable TcLevel where
  ppr (TcLevel us) = ppr us

{- *********************************************************************
*                                                                      *
    Finding type family instances
*                                                                      *
************************************************************************
-}

-- | Finds outermost type-family applications occuring in a type,
-- after expanding synonyms.  In the list (F, tys) that is returned
-- we guarantee that tys matches F's arity.  For example, given
--    type family F a :: * -> *    (arity 1)
-- calling tcTyFamInsts on (Maybe (F Int Bool) will return
--     (F, [Int]), not (F, [Int,Bool])
--
-- This is important for its use in deciding termination of type
-- instances (see Trac #11581).  E.g.
--    type instance G [Int] = ...(F Int <big type>)...
-- we don't need to take <big type> into account when asking if
-- the calls on the RHS are smaller than the LHS
tcTyFamInsts :: Type -> [(TyCon, [Type])]
tcTyFamInsts ty
  | Just exp_ty <- tcView ty    = tcTyFamInsts exp_ty
tcTyFamInsts (TyVarTy _)        = []
tcTyFamInsts (TyConApp tc tys)
  | isTypeFamilyTyCon tc        = [(tc, take (tyConArity tc) tys)]
  | otherwise                   = concat (map tcTyFamInsts tys)
tcTyFamInsts (LitTy {})         = []
tcTyFamInsts (ForAllTy bndr ty) = tcTyFamInsts (binderKind bndr)
                                  ++ tcTyFamInsts ty
tcTyFamInsts (FunTy ty1 ty2)    = tcTyFamInsts ty1 ++ tcTyFamInsts ty2
tcTyFamInsts (AppTy ty1 ty2)    = tcTyFamInsts ty1 ++ tcTyFamInsts ty2
tcTyFamInsts (CastTy ty _)      = tcTyFamInsts ty
tcTyFamInsts (CoercionTy _)     = []  -- don't count tyfams in coercions,
                                      -- as they never get normalized, anyway

{-
************************************************************************
*                                                                      *
          The "exact" free variables of a type
*                                                                      *
************************************************************************

Note [Silly type synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  type T a = Int
What are the free tyvars of (T x)?  Empty, of course!
Here's the example that Ralf Laemmel showed me:
  foo :: (forall a. C u a -> C u a) -> u
  mappend :: Monoid u => u -> u -> u

  bar :: Monoid u => u
  bar = foo (\t -> t `mappend` t)
We have to generalise at the arg to f, and we don't
want to capture the constraint (Monad (C u a)) because
it appears to mention a.  Pretty silly, but it was useful to him.

exactTyCoVarsOfType is used by the type checker to figure out exactly
which type variables are mentioned in a type.  It's also used in the
smart-app checking code --- see TcExpr.tcIdApp

On the other hand, consider a *top-level* definition
  f = (\x -> x) :: T a -> T a
If we don't abstract over 'a' it'll get fixed to GHC.Prim.Any, and then
if we have an application like (f "x") we get a confusing error message
involving Any.  So the conclusion is this: when generalising
  - at top level use tyCoVarsOfType
  - in nested bindings use exactTyCoVarsOfType
See Trac #1813 for example.
-}

exactTyCoVarsOfType :: Type -> TyCoVarSet
-- Find the free type variables (of any kind)
-- but *expand* type synonyms.  See Note [Silly type synonym] above.
exactTyCoVarsOfType ty
  = go ty
  where
    go ty | Just ty' <- tcView ty = go ty'  -- This is the key line
    go (TyVarTy tv)         = unitVarSet tv `unionVarSet` go (tyVarKind tv)
    go (TyConApp _ tys)     = exactTyCoVarsOfTypes tys
    go (LitTy {})           = emptyVarSet
    go (AppTy fun arg)      = go fun `unionVarSet` go arg
    go (FunTy arg res)      = go arg `unionVarSet` go res
    go (ForAllTy bndr ty)   = delBinderVar (go ty) bndr `unionVarSet` go (binderKind bndr)
    go (CastTy ty co)       = go ty `unionVarSet` goCo co
    go (CoercionTy co)      = goCo co

    goCo (Refl _ ty)        = go ty
    goCo (TyConAppCo _ _ args)= goCos args
    goCo (AppCo co arg)     = goCo co `unionVarSet` goCo arg
    goCo (ForAllCo tv k_co co)
      = goCo co `delVarSet` tv `unionVarSet` goCo k_co
    goCo (FunCo _ co1 co2)   = goCo co1 `unionVarSet` goCo co2
    goCo (CoVarCo v)         = unitVarSet v `unionVarSet` go (varType v)
    goCo (AxiomInstCo _ _ args) = goCos args
    goCo (UnivCo p _ t1 t2)  = goProv p `unionVarSet` go t1 `unionVarSet` go t2
    goCo (SymCo co)          = goCo co
    goCo (TransCo co1 co2)   = goCo co1 `unionVarSet` goCo co2
    goCo (NthCo _ co)        = goCo co
    goCo (LRCo _ co)         = goCo co
    goCo (InstCo co arg)     = goCo co `unionVarSet` goCo arg
    goCo (CoherenceCo c1 c2) = goCo c1 `unionVarSet` goCo c2
    goCo (KindCo co)         = goCo co
    goCo (SubCo co)          = goCo co
    goCo (AxiomRuleCo _ c)   = goCos c

    goCos cos = foldr (unionVarSet . goCo) emptyVarSet cos

    goProv UnsafeCoerceProv     = emptyVarSet
    goProv (PhantomProv kco)    = goCo kco
    goProv (ProofIrrelProv kco) = goCo kco
    goProv (PluginProv _)       = emptyVarSet
    goProv (HoleProv _)         = emptyVarSet

exactTyCoVarsOfTypes :: [Type] -> TyVarSet
exactTyCoVarsOfTypes tys = mapUnionVarSet exactTyCoVarsOfType tys

anyRewritableTyVar :: Bool -> (TcTyVar -> Bool)
                   -> TcType -> Bool
-- (anyRewritableTyVar ignore_cos pred ty) returns True
--    if the 'pred' returns True of free TyVar in 'ty'
-- Do not look inside casts and coercions if 'ignore_cos' is True
-- See Note [anyRewritableTyVar]
anyRewritableTyVar ignore_cos pred ty
  = go emptyVarSet ty
  where
    go_tv bound tv | tv `elemVarSet` bound = False
                   | otherwise             = pred tv

    go bound (TyVarTy tv)     = go_tv bound tv
    go _     (LitTy {})       = False
    go bound (TyConApp _ tys) = any (go bound) tys
    go bound (AppTy fun arg)  = go bound fun || go bound arg
    go bound (FunTy arg res)  = go bound arg || go bound res
    go bound (ForAllTy tv ty) = go (bound `extendVarSet` binderVar tv) ty
    go bound (CastTy ty co)   = go bound ty || go_co bound co
    go bound (CoercionTy co)  = go_co bound co

    go_co bound co
      | ignore_cos = False
      | otherwise  = anyVarSet (go_tv bound) (tyCoVarsOfCo co)
      -- We don't have an equivalent of anyRewritableTyVar for coercions
      -- (at least not yet) so take the free vars and test them

{- Note [anyRewritableTyVar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
anyRewritableTyVar is used during kick-out from the inert set,
to decide if, given a new equality (a ~ ty), we should kick out
a constraint C.  Rather than gather free variables and see if 'a'
is among them, we instead pass in a predicate; this is just efficiency.
-}

{- *********************************************************************
*                                                                      *
          Bound variables in a type
*                                                                      *
********************************************************************* -}

-- | Find all variables bound anywhere in a type.
-- See also Note [Scope-check inferred kinds] in TcHsType
allBoundVariables :: Type -> TyVarSet
allBoundVariables ty = fvVarSet $ go ty
  where
    go :: Type -> FV
    go (TyVarTy tv)     = go (tyVarKind tv)
    go (TyConApp _ tys) = mapUnionFV go tys
    go (AppTy t1 t2)    = go t1 `unionFV` go t2
    go (FunTy t1 t2)    = go t1 `unionFV` go t2
    go (ForAllTy (TvBndr tv _) t2) = FV.unitFV tv `unionFV`
                                    go (tyVarKind tv) `unionFV` go t2
    go (LitTy {})       = emptyFV
    go (CastTy ty _)    = go ty
    go (CoercionTy {})  = emptyFV
      -- any types mentioned in a coercion should also be mentioned in
      -- a type.

allBoundVariabless :: [Type] -> TyVarSet
allBoundVariabless = mapUnionVarSet allBoundVariables

{- *********************************************************************
*                                                                      *
          Type and kind variables in a type
*                                                                      *
********************************************************************* -}

data CandidatesQTvs  -- See Note [Dependent type variables]
                     -- See Note [CandidatesQTvs determinism]
  = DV { dv_kvs :: DTyCoVarSet  -- "kind" variables (dependent)
       , dv_tvs :: DTyVarSet    -- "type" variables (non-dependent)
         -- A variable may appear in both sets
         -- E.g.   T k (x::k)    The first occurrence of k makes it
         --                      show up in dv_tvs, the second in dv_kvs
         -- See Note [Dependent type variables]
    }

instance Monoid CandidatesQTvs where
   mempty = DV { dv_kvs = emptyDVarSet, dv_tvs = emptyDVarSet }
   mappend (DV { dv_kvs = kv1, dv_tvs = tv1 })
           (DV { dv_kvs = kv2, dv_tvs = tv2 })
          = DV { dv_kvs = kv1 `unionDVarSet` kv2
               , dv_tvs = tv1 `unionDVarSet` tv2}

instance Outputable CandidatesQTvs where
  ppr (DV {dv_kvs = kvs, dv_tvs = tvs })
    = text "DV" <+> braces (sep [ text "dv_kvs =" <+> ppr kvs
                                , text "dv_tvs =" <+> ppr tvs ])

{- Note [Dependent type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Haskell type inference we quantify over type variables; but we only
quantify over /kind/ variables when -XPolyKinds is on.  Without -XPolyKinds
we default the kind variables to *.

So, to support this defaulting, and only for that reason, when
collecting the free vars of a type, prior to quantifying, we must keep
the type and kind variables separate.

But what does that mean in a system where kind variables /are/ type
variables? It's a fairly arbitrary distinction based on how the
variables appear:

  - "Kind variables" appear in the kind of some other free variable
     PLUS any free coercion variables

     These are the ones we default to * if -XPolyKinds is off

  - "Type variables" are all free vars that are not kind variables

E.g.  In the type    T k (a::k)
      'k' is a kind variable, because it occurs in the kind of 'a',
          even though it also appears at "top level" of the type
      'a' is a type variable, because it doesn't

We gather these variables using a CandidatesQTvs record:
  DV { dv_kvs: Variables free in the kind of a free type variable
               or of a forall-bound type variable
     , dv_tvs: Variables sytactically free in the type }

So:  dv_kvs            are the kind variables of the type
     (dv_tvs - dv_kvs) are the type variable of the type

Note that

* A variable can occur in both.
      T k (x::k)    The first occurrence of k makes it
                    show up in dv_tvs, the second in dv_kvs

* We include any coercion variables in the "dependent",
  "kind-variable" set because we never quantify over them.

* Both sets are un-ordered, of course.

* The "kind variables" might depend on each other; e.g
     (k1 :: k2), (k2 :: *)
  The "type variables" do not depend on each other; if
  one did, it'd be classified as a kind variable!

Note [CandidatesQTvs determinism and order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Determinism: when we quantify over type variables we decide the
  order in which they appear in the final type. Because the order of
  type variables in the type can end up in the interface file and
  affects some optimizations like worker-wrapper, we want this order to
  be deterministic.

  To achieve that we use deterministic sets of variables that can be
  converted to lists in a deterministic order. For more information
  about deterministic sets see Note [Deterministic UniqFM] in UniqDFM.

* Order: as well as being deterministic, we use an
  accumulating-parameter style for candidateQTyVarsOfType so that we
  add variables one at a time, left to right.  That means we tend to
  produce the variables in left-to-right order.  This is just to make
  it bit more predicatable for the programmer.
-}

-- | Worker for 'splitDepVarsOfType'. This might output the same var
-- in both sets, if it's used in both a type and a kind.
-- See Note [CandidatesQTvs determinism and order]
-- See Note [Dependent type variables]
candidateQTyVarsOfType :: Type -> CandidatesQTvs
candidateQTyVarsOfType = split_dvs emptyVarSet mempty

split_dvs :: VarSet -> CandidatesQTvs -> Type -> CandidatesQTvs
split_dvs bound dvs ty
  = go dvs ty
  where
    go dv (AppTy t1 t2)    = go (go dv t1) t2
    go dv (TyConApp _ tys) = foldl go dv tys
    go dv (FunTy arg res)  = go (go dv arg) res
    go dv (LitTy {})       = dv
    go dv (CastTy ty co)   = go dv ty `mappend` go_co co
    go dv (CoercionTy co)  = dv `mappend` go_co co

    go dv@(DV { dv_kvs = kvs, dv_tvs = tvs }) (TyVarTy tv)
      | tv `elemVarSet` bound
      = dv
      | otherwise
      = DV { dv_kvs = kvs `unionDVarSet`
                      kill_bound (tyCoVarsOfTypeDSet (tyVarKind tv))
           , dv_tvs = tvs `extendDVarSet` tv }

    go dv (ForAllTy (TvBndr tv _) ty)
      = DV { dv_kvs = kvs `unionDVarSet`
                      kill_bound (tyCoVarsOfTypeDSet (tyVarKind tv))
           , dv_tvs = tvs }
      where
        DV { dv_kvs = kvs, dv_tvs = tvs } = split_dvs (bound `extendVarSet` tv) dv ty

    go_co co = DV { dv_kvs = kill_bound (tyCoVarsOfCoDSet co)
                  , dv_tvs = emptyDVarSet }

    kill_bound free
      | isEmptyVarSet bound = free
      | otherwise           = free `dVarSetMinusVarSet` bound

-- | Like 'splitDepVarsOfType', but over a list of types
candidateQTyVarsOfTypes :: [Type] -> CandidatesQTvs
candidateQTyVarsOfTypes = foldl (split_dvs emptyVarSet) mempty

{-
************************************************************************
*                                                                      *
                Predicates
*                                                                      *
************************************************************************
-}

tcIsTcTyVar :: TcTyVar -> Bool
-- See Note [TcTyVars in the typechecker]
tcIsTcTyVar tv = isTyVar tv

isTouchableOrFmv :: TcLevel -> TcTyVar -> Bool
isTouchableOrFmv ctxt_tclvl tv
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tv_tclvl, mtv_info = info }
        -> ASSERT2( checkTcLevelInvariant ctxt_tclvl tv_tclvl,
                    ppr tv $$ ppr tv_tclvl $$ ppr ctxt_tclvl )
           case info of
             FlatMetaTv -> True
             _          -> tv_tclvl `sameDepthAs` ctxt_tclvl
      _          -> False

isTouchableMetaTyVar :: TcLevel -> TcTyVar -> Bool
isTouchableMetaTyVar ctxt_tclvl tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tv_tclvl }
        -> ASSERT2( checkTcLevelInvariant ctxt_tclvl tv_tclvl,
                    ppr tv $$ ppr tv_tclvl $$ ppr ctxt_tclvl )
           tv_tclvl `sameDepthAs` ctxt_tclvl
      _ -> False
  | otherwise = False

isFloatedTouchableMetaTyVar :: TcLevel -> TcTyVar -> Bool
isFloatedTouchableMetaTyVar ctxt_tclvl tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
      MetaTv { mtv_tclvl = tv_tclvl } -> tv_tclvl `strictlyDeeperThan` ctxt_tclvl
      _ -> False
  | otherwise = False

isImmutableTyVar :: TyVar -> Bool
isImmutableTyVar tv = isSkolemTyVar tv

isTyConableTyVar, isSkolemTyVar, isOverlappableTyVar,
  isMetaTyVar, isAmbiguousTyVar,
  isFmvTyVar, isFskTyVar, isFlattenTyVar :: TcTyVar -> Bool

isTyConableTyVar tv
        -- True of a meta-type variable that can be filled in
        -- with a type constructor application; in particular,
        -- not a SigTv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv { mtv_info = SigTv } -> False
        _                           -> True
  | otherwise = True

isFmvTyVar tv
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv { mtv_info = FlatMetaTv } -> True
        _                                -> False

isFskTyVar tv
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv { mtv_info = FlatSkolTv } -> True
        _                                -> False

-- | True of both given and wanted flatten-skolems (fak and usk)
isFlattenTyVar tv
  = isFmvTyVar tv || isFskTyVar tv

isSkolemTyVar tv
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv {} -> False
        _other    -> True

isOverlappableTyVar tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        SkolemTv _ overlappable -> overlappable
        _                       -> False
  | otherwise = False

isMetaTyVar tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = ASSERT2( tcIsTcTyVar tv, ppr tv )
    case tcTyVarDetails tv of
        MetaTv {} -> True
        _         -> False
  | otherwise = False

-- isAmbiguousTyVar is used only when reporting type errors
-- It picks out variables that are unbound, namely meta
-- type variables and the RuntimUnk variables created by
-- RtClosureInspect.zonkRTTIType.  These are "ambiguous" in
-- the sense that they stand for an as-yet-unknown type
isAmbiguousTyVar tv
  | isTyVar tv -- See Note [Coercion variables in free variable lists]
  = case tcTyVarDetails tv of
        MetaTv {}     -> True
        RuntimeUnk {} -> True
        _             -> False
  | otherwise = False

isMetaTyVarTy :: TcType -> Bool
isMetaTyVarTy (TyVarTy tv) = isMetaTyVar tv
isMetaTyVarTy _            = False

metaTyVarInfo :: TcTyVar -> MetaInfo
metaTyVarInfo tv
  = case tcTyVarDetails tv of
      MetaTv { mtv_info = info } -> info
      _ -> pprPanic "metaTyVarInfo" (ppr tv)

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

isSigTyVar :: Var -> Bool
isSigTyVar tv
  = case tcTyVarDetails tv of
        MetaTv { mtv_info = SigTv } -> True
        _                           -> False

isFlexi, isIndirect :: MetaDetails -> Bool
isFlexi Flexi = True
isFlexi _     = False

isIndirect (Indirect _) = True
isIndirect _            = False

isRuntimeUnkSkol :: TyVar -> Bool
-- Called only in TcErrors; see Note [Runtime skolems] there
isRuntimeUnkSkol x
  | RuntimeUnk <- tcTyVarDetails x = True
  | otherwise                      = False

{-
************************************************************************
*                                                                      *
\subsection{Tau, sigma and rho}
*                                                                      *
************************************************************************
-}

mkSigmaTy :: [TyVarBinder] -> [PredType] -> Type -> Type
mkSigmaTy bndrs theta tau = mkForAllTys bndrs (mkPhiTy theta tau)

-- | Make a sigma ty where all type variables are 'Inferred'. That is,
-- they cannot be used with visible type application.
mkInfSigmaTy :: [TyVar] -> [PredType] -> Type -> Type
mkInfSigmaTy tyvars theta ty = mkSigmaTy (mkTyVarBinders Inferred tyvars) theta ty

-- | Make a sigma ty where all type variables are "specified". That is,
-- they can be used with visible type application
mkSpecSigmaTy :: [TyVar] -> [PredType] -> Type -> Type
mkSpecSigmaTy tyvars ty = mkSigmaTy (mkTyVarBinders Specified tyvars) ty

mkPhiTy :: [PredType] -> Type -> Type
mkPhiTy = mkFunTys

---------------
getDFunTyKey :: Type -> OccName -- Get some string from a type, to be used to
                                -- construct a dictionary function name
getDFunTyKey ty | Just ty' <- coreView ty = getDFunTyKey ty'
getDFunTyKey (TyVarTy tv)            = getOccName tv
getDFunTyKey (TyConApp tc _)         = getOccName tc
getDFunTyKey (LitTy x)               = getDFunTyLitKey x
getDFunTyKey (AppTy fun _)           = getDFunTyKey fun
getDFunTyKey (FunTy _ _)             = getOccName funTyCon
getDFunTyKey (ForAllTy _ t)          = getDFunTyKey t
getDFunTyKey (CastTy ty _)           = getDFunTyKey ty
getDFunTyKey t@(CoercionTy _)        = pprPanic "getDFunTyKey" (ppr t)

getDFunTyLitKey :: TyLit -> OccName
getDFunTyLitKey (NumTyLit n) = mkOccName Name.varName (show n)
getDFunTyLitKey (StrTyLit n) = mkOccName Name.varName (show n)  -- hm

---------------
mkNakedTyConApp :: TyCon -> [Type] -> Type
-- Builds a TyConApp
--   * without being strict in TyCon,
--   * without satisfying the invariants of TyConApp
-- A subsequent zonking will establish the invariants
-- See Note [Type-checking inside the knot] in TcHsType
mkNakedTyConApp tc tys = TyConApp tc tys

mkNakedAppTys :: Type -> [Type] -> Type
-- See Note [Type-checking inside the knot] in TcHsType
mkNakedAppTys ty1                []   = ty1
mkNakedAppTys (TyConApp tc tys1) tys2 = mkNakedTyConApp tc (tys1 ++ tys2)
mkNakedAppTys ty1                tys2 = foldl AppTy ty1 tys2

mkNakedAppTy :: Type -> Type -> Type
-- See Note [Type-checking inside the knot] in TcHsType
mkNakedAppTy ty1 ty2 = mkNakedAppTys ty1 [ty2]

mkNakedCastTy :: Type -> Coercion -> Type
-- Do simple, fast compaction; especially dealing with Refl
-- for which it's plain stupid to create a cast
-- This simple function killed off a huge number of Refl casts
-- in types, at birth.
-- Note that it's fine to do this even for a "mkNaked" function,
-- because we don't look at TyCons.  isReflCo checks if the coercion
-- is structurally Refl; it does not check for shape k ~ k.
mkNakedCastTy ty co | isReflCo co = ty
mkNakedCastTy (CastTy ty co1) co2 = CastTy ty (co1 `mkTransCo` co2)
mkNakedCastTy ty co = CastTy ty co

{-
************************************************************************
*                                                                      *
\subsection{Expanding and splitting}
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
tcSplitPiTys = splitPiTys

-- | Splits a type into a TyBinder and a body, if possible. Panics otherwise
tcSplitPiTy_maybe :: Type -> Maybe (TyBinder, Type)
tcSplitPiTy_maybe = splitPiTy_maybe

tcSplitForAllTy_maybe :: Type -> Maybe (TyVarBinder, Type)
tcSplitForAllTy_maybe ty | Just ty' <- tcView ty = tcSplitForAllTy_maybe ty'
tcSplitForAllTy_maybe (ForAllTy tv ty) = Just (tv, ty)
tcSplitForAllTy_maybe _                = Nothing

-- | Like 'tcSplitPiTys', but splits off only named binders, returning
-- just the tycovars.
tcSplitForAllTys :: Type -> ([TyVar], Type)
tcSplitForAllTys = splitForAllTys

-- | Like 'tcSplitForAllTys', but splits off only named binders.
tcSplitForAllTyVarBndrs :: Type -> ([TyVarBinder], Type)
tcSplitForAllTyVarBndrs = splitForAllTyVarBndrs

-- | Is this a ForAllTy with a named binder?
tcIsForAllTy :: Type -> Bool
tcIsForAllTy ty | Just ty' <- tcView ty = tcIsForAllTy ty'
tcIsForAllTy (ForAllTy {}) = True
tcIsForAllTy _             = False

tcSplitPredFunTy_maybe :: Type -> Maybe (PredType, Type)
-- Split off the first predicate argument from a type
tcSplitPredFunTy_maybe ty
  | Just ty' <- tcView ty = tcSplitPredFunTy_maybe ty'
tcSplitPredFunTy_maybe (FunTy arg res)
  | isPredTy arg = Just (arg, res)
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

-- | Split a sigma type into its parts.
tcSplitSigmaTy :: Type -> ([TyVar], ThetaType, Type)
tcSplitSigmaTy ty = case tcSplitForAllTys ty of
                        (tvs, rho) -> case tcSplitPhiTy rho of
                                        (theta, tau) -> (tvs, theta, tau)

-- | Split a sigma type into its parts, going underneath as many @ForAllTy@s
-- as possible. For example, given this type synonym:
--
-- @
-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
-- @
--
-- if you called @tcSplitSigmaTy@ on this type:
--
-- @
-- forall s t a b. Each s t a b => Traversal s t a b
-- @
--
-- then it would return @([s,t,a,b], [Each s t a b], Traversal s t a b)@. But
-- if you instead called @tcSplitNestedSigmaTys@ on the type, it would return
-- @([s,t,a,b,f], [Each s t a b, Applicative f], (a -> f b) -> s -> f t)@.
tcSplitNestedSigmaTys :: Type -> ([TyVar], ThetaType, Type)
-- NB: This is basically a pure version of deeplyInstantiate (from Inst) that
-- doesn't compute an HsWrapper.
tcSplitNestedSigmaTys ty
    -- If there's a forall, split it apart and try splitting the rho type
    -- underneath it.
  | Just (arg_tys, tvs1, theta1, rho1) <- tcDeepSplitSigmaTy_maybe ty
  = let (tvs2, theta2, rho2) = tcSplitNestedSigmaTys rho1
    in (tvs1 ++ tvs2, theta1 ++ theta2, mkFunTys arg_tys rho2)
    -- If there's no forall, we're done.
  | otherwise = ([], [], ty)

-----------------------
tcDeepSplitSigmaTy_maybe
  :: TcSigmaType -> Maybe ([TcType], [TyVar], ThetaType, TcSigmaType)
-- Looks for a *non-trivial* quantified type, under zero or more function arrows
-- By "non-trivial" we mean either tyvars or constraints are non-empty

tcDeepSplitSigmaTy_maybe ty
  | Just (arg_ty, res_ty)           <- tcSplitFunTy_maybe ty
  , Just (arg_tys, tvs, theta, rho) <- tcDeepSplitSigmaTy_maybe res_ty
  = Just (arg_ty:arg_tys, tvs, theta, rho)

  | (tvs, theta, rho) <- tcSplitSigmaTy ty
  , not (null tvs && null theta)
  = Just ([], tvs, theta, rho)

  | otherwise = Nothing

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
tcTyConAppTyCon_maybe (FunTy _ _)
  = Just funTyCon
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

-- | Like 'tcRepSplitTyConApp_maybe', but returns 'Nothing' if,
--
-- 1. the type is structurally not a type constructor application, or
--
-- 2. the type is a function type (e.g. application of 'funTyCon'), but we
--    currently don't even enough information to fully determine its RuntimeRep
--    variables. For instance, @FunTy (a :: k) Int@.
--
-- By contrast 'tcRepSplitTyConApp_maybe' panics in the second case.
--
-- The behavior here is needed during canonicalization; see Note [FunTy and
-- decomposing tycon applications] in TcCanonical for details.
tcRepSplitTyConApp_maybe' :: HasCallStack => Type -> Maybe (TyCon, [Type])
tcRepSplitTyConApp_maybe' (TyConApp tc tys)          = Just (tc, tys)
tcRepSplitTyConApp_maybe' (FunTy arg res)
  | Just arg_rep <- getRuntimeRep_maybe arg
  , Just res_rep <- getRuntimeRep_maybe res
  = Just (funTyCon, [arg_rep, res_rep, arg, res])
tcRepSplitTyConApp_maybe' _                          = Nothing


-----------------------
tcSplitFunTys :: Type -> ([Type], Type)
tcSplitFunTys ty = case tcSplitFunTy_maybe ty of
                        Nothing        -> ([], ty)
                        Just (arg,res) -> (arg:args, res')
                                       where
                                          (args,res') = tcSplitFunTys res

tcSplitFunTy_maybe :: Type -> Maybe (Type, Type)
tcSplitFunTy_maybe ty | Just ty' <- tcView ty         = tcSplitFunTy_maybe ty'
tcSplitFunTy_maybe (FunTy arg res) | not (isPredTy arg) = Just (arg, res)
tcSplitFunTy_maybe _                                    = Nothing
        -- Note the typeKind guard
        -- Consider     (?x::Int) => Bool
        -- We don't want to treat this as a function type!
        -- A concrete example is test tc230:
        --      f :: () -> (?p :: ()) => () -> ()
        --
        --      g = f () ()

tcSplitFunTysN :: Arity                      -- N: Number of desired args
               -> TcRhoType
               -> Either Arity               -- Number of missing arrows
                        ([TcSigmaType],      -- Arg types (always N types)
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

tcSplitFunTy :: Type -> (Type, Type)
tcSplitFunTy  ty = expectJust "tcSplitFunTy" (tcSplitFunTy_maybe ty)

tcFunArgTy :: Type -> Type
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
tcGetTyVar msg ty = expectJust msg (tcGetTyVar_maybe ty)

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
-- the latter  specifically stops at PredTy arguments,
-- and we don't want to do that here
tcSplitDFunTy ty
  = case tcSplitForAllTys ty   of { (tvs, rho)    ->
    case splitFunTys rho       of { (theta, tau)  ->
    case tcSplitDFunHead tau   of { (clas, tys)   ->
    (tvs, theta, clas, tys) }}}

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
  | (sel_tyvars,sel_rho) <- tcSplitForAllTys ty
  , Just (first_pred, local_meth_ty) <- tcSplitPredFunTy_maybe sel_rho
  = (sel_tyvars, first_pred, local_meth_ty)
  | otherwise
  = pprPanic "tcSplitMethodTy" (ppr ty)


{- *********************************************************************
*                                                                      *
            Type equalities
*                                                                      *
********************************************************************* -}

tcEqKind :: TcKind -> TcKind -> Bool
tcEqKind = tcEqType

tcEqType :: TcType -> TcType -> Bool
-- tcEqType is a proper implements the same Note [Non-trivial definitional
-- equality] (in TyCoRep) as `eqType`, but Type.eqType believes (* ==
-- Constraint), and that is NOT what we want in the type checker!
tcEqType ty1 ty2
  = isNothing (tc_eq_type tcView ki1 ki2) &&
    isNothing (tc_eq_type tcView ty1 ty2)
  where
    ki1 = typeKind ty1
    ki2 = typeKind ty2

-- | Just like 'tcEqType', but will return True for types of different kinds
-- as long as their non-coercion structure is identical.
tcEqTypeNoKindCheck :: TcType -> TcType -> Bool
tcEqTypeNoKindCheck ty1 ty2
  = isNothing $ tc_eq_type tcView ty1 ty2

-- | Like 'tcEqType', but returns information about whether the difference
-- is visible in the case of a mismatch.
-- @Nothing@    : the types are equal
-- @Just True@  : the types differ, and the point of difference is visible
-- @Just False@ : the types differ, and the point of difference is invisible
tcEqTypeVis :: TcType -> TcType -> Maybe Bool
tcEqTypeVis ty1 ty2
  = tc_eq_type tcView ty1 ty2 <!> invis (tc_eq_type tcView ki1 ki2)
  where
    ki1 = typeKind ty1
    ki2 = typeKind ty2

      -- convert Just True to Just False
    invis :: Maybe Bool -> Maybe Bool
    invis = fmap (const False)

(<!>) :: Maybe Bool -> Maybe Bool -> Maybe Bool
Nothing        <!> x         = x
Just True      <!> _         = Just True
Just _vis      <!> Just True = Just True
Just vis       <!> _         = Just vis
infixr 3 <!>

-- | Real worker for 'tcEqType'. No kind check!
tc_eq_type :: (TcType -> Maybe TcType)  -- ^ @tcView@, if you want unwrapping
           -> Type -> Type -> Maybe Bool
tc_eq_type view_fun orig_ty1 orig_ty2 = go True orig_env orig_ty1 orig_ty2
  where
    go :: Bool -> RnEnv2 -> Type -> Type -> Maybe Bool
    go vis env t1 t2 | Just t1' <- view_fun t1 = go vis env t1' t2
    go vis env t1 t2 | Just t2' <- view_fun t2 = go vis env t1 t2'

    go vis env (TyVarTy tv1)       (TyVarTy tv2)
      = check vis $ rnOccL env tv1 == rnOccR env tv2

    go vis _   (LitTy lit1)        (LitTy lit2)
      = check vis $ lit1 == lit2

    go vis env (ForAllTy (TvBndr tv1 vis1) ty1)
               (ForAllTy (TvBndr tv2 vis2) ty2)
      = go (isVisibleArgFlag vis1) env (tyVarKind tv1) (tyVarKind tv2)
          <!> go vis (rnBndr2 env tv1 tv2) ty1 ty2
          <!> check vis (vis1 == vis2)
    -- Make sure we handle all FunTy cases since falling through to the
    -- AppTy case means that tcRepSplitAppTy_maybe may see an unzonked
    -- kind variable, which causes things to blow up.
    go vis env (FunTy arg1 res1) (FunTy arg2 res2)
      = go vis env arg1 arg2 <!> go vis env res1 res2
    go vis env ty (FunTy arg res)
      = eqFunTy vis env arg res ty
    go vis env (FunTy arg res) ty
      = eqFunTy vis env arg res ty

      -- See Note [Equality on AppTys] in Type
    go vis env (AppTy s1 t1)        ty2
      | Just (s2, t2) <- tcRepSplitAppTy_maybe ty2
      = go vis env s1 s2 <!> go vis env t1 t2
    go vis env ty1                  (AppTy s2 t2)
      | Just (s1, t1) <- tcRepSplitAppTy_maybe ty1
      = go vis env s1 s2 <!> go vis env t1 t2
    go vis env (TyConApp tc1 ts1)   (TyConApp tc2 ts2)
      = check vis (tc1 == tc2) <!> gos (tc_vis vis tc1) env ts1 ts2
    go vis env (CastTy t1 _)        t2              = go vis env t1 t2
    go vis env t1                   (CastTy t2 _)   = go vis env t1 t2
    go _   _   (CoercionTy {})      (CoercionTy {}) = Nothing
    go vis _   _                    _               = Just vis

    gos _      _   []       []       = Nothing
    gos (v:vs) env (t1:ts1) (t2:ts2) = go v env t1 t2 <!> gos vs env ts1 ts2
    gos (v:_)  _   _        _        = Just v
    gos _      _   _        _        = panic "tc_eq_type"

    tc_vis :: Bool -> TyCon -> [Bool]
    tc_vis True tc = viss ++ repeat True
       -- the repeat True is necessary because tycons can legitimately
       -- be oversaturated
      where
        bndrs = tyConBinders tc
        viss  = map isVisibleTyConBinder bndrs
    tc_vis False _ = repeat False  -- if we're not in a visible context, our args
                                   -- aren't either

    check :: Bool -> Bool -> Maybe Bool
    check _   True  = Nothing
    check vis False = Just vis

    orig_env = mkRnEnv2 $ mkInScopeSet $ tyCoVarsOfTypes [orig_ty1, orig_ty2]

    -- @eqFunTy arg res ty@ is True when @ty@ equals @FunTy arg res@. This is
    -- sometimes hard to know directly because @ty@ might have some casts
    -- obscuring the FunTy. And 'splitAppTy' is difficult because we can't
    -- always extract a RuntimeRep (see Note [xyz]) if the kind of the arg or
    -- res is unzonked/unflattened. Thus this function, which handles this
    -- corner case.
    eqFunTy :: Bool -> RnEnv2 -> Type -> Type -> Type -> Maybe Bool
    eqFunTy vis env arg res (FunTy arg' res')
      = go vis env arg arg' <!> go vis env res res'
    eqFunTy vis env arg res ty@(AppTy{})
      | Just (tc, [_, _, arg', res']) <- get_args ty []
      , tc == funTyCon
      = go vis env arg arg' <!> go vis env res res'
      where
        get_args :: Type -> [Type] -> Maybe (TyCon, [Type])
        get_args (AppTy f x)       args = get_args f (x:args)
        get_args (CastTy t _)      args = get_args t args
        get_args (TyConApp tc tys) args = Just (tc, tys ++ args)
        get_args _                 _    = Nothing
    eqFunTy vis _ _ _ _
      = Just vis

-- | Like 'pickyEqTypeVis', but returns a Bool for convenience
pickyEqType :: TcType -> TcType -> Bool
-- Check when two types _look_ the same, _including_ synonyms.
-- So (pickyEqType String [Char]) returns False
-- This ignores kinds and coercions, because this is used only for printing.
pickyEqType ty1 ty2
  = isNothing $
    tc_eq_type (const Nothing) ty1 ty2

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
-- This function is here rather than in TcValidity because it is
-- called from TcSimplify, which itself is imported by TcValidity
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

evVarPred_maybe :: EvVar -> Maybe PredType
evVarPred_maybe v = if isPredTy ty then Just ty else Nothing
  where ty = varType v

evVarPred :: EvVar -> PredType
evVarPred var
 | debugIsOn
  = case evVarPred_maybe var of
      Just pred -> pred
      Nothing   -> pprPanic "tcEvVarPred" (ppr var <+> ppr (varType var))
 | otherwise
  = varType var

------------------
-- | When inferring types, should we quantify over a given predicate?
-- Generally true of classes; generally false of equality constraints.
-- Equality constraints that mention quantified type variables and
-- implicit variables complicate the story. See Notes
-- [Inheriting implicit parameters] and [Quantifying over equality constraints]
pickQuantifiablePreds
  :: TyVarSet           -- Quantifying over these
  -> TcThetaType        -- Proposed constraints to quantify
  -> TcThetaType        -- A subset that we can actually quantify
-- This function decides whether a particular constraint should be
-- quantified over, given the type variables that are being quantified
pickQuantifiablePreds qtvs theta
  = let flex_ctxt = True in  -- Quantify over non-tyvar constraints, even without
                             -- -XFlexibleContexts: see Trac #10608, #10351
         -- flex_ctxt <- xoptM Opt_FlexibleContexts
    filter (pick_me flex_ctxt) theta
  where
    pick_me flex_ctxt pred
      = case classifyPredType pred of

          ClassPred cls tys
            | Just {} <- isCallStackPred pred
              -- NEVER infer a CallStack constraint
              -- Otherwise, we let the constraints bubble up to be
              -- solved from the outer context, or be defaulted when we
              -- reach the top-level.
              -- see Note [Overview of implicit CallStacks]
              -> False

            | isIPClass cls    -> True -- See note [Inheriting implicit parameters]

            | otherwise
              -> pick_cls_pred flex_ctxt cls tys

          EqPred ReprEq ty1 ty2 -> pick_cls_pred flex_ctxt coercibleClass [ty1, ty2]
            -- representational equality is like a class constraint

          EqPred NomEq ty1 ty2  -> quant_fun ty1 || quant_fun ty2
          IrredPred ty          -> tyCoVarsOfType ty `intersectsVarSet` qtvs

    pick_cls_pred flex_ctxt cls tys
      = tyCoVarsOfTypes tys `intersectsVarSet` qtvs
        && (checkValidClsArgs flex_ctxt cls tys)
           -- Only quantify over predicates that checkValidType
           -- will pass!  See Trac #10351.

    -- See Note [Quantifying over equality constraints]
    quant_fun ty
      = case tcSplitTyConApp_maybe ty of
          Just (tc, tys) | isTypeFamilyTyCon tc
                         -> tyCoVarsOfTypes tys `intersectsVarSet` qtvs
          _ -> False

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
    captured pred = isIPPred pred || (tyCoVarsOfType pred `intersectsVarSet` qtvs)


-- Superclasses

type PredWithSCs = (PredType, [PredType])

mkMinimalBySCs :: [PredType] -> [PredType]
-- Remove predicates that can be deduced from others by superclasses,
-- including duplicate predicates. The result is a subset of the input.
mkMinimalBySCs ptys = go preds_with_scs []
 where
   preds_with_scs :: [PredWithSCs]
   preds_with_scs = [ (pred, pred : transSuperClasses pred)
                    | pred <- ptys ]

   go :: [PredWithSCs]   -- Work list
      -> [PredWithSCs]   -- Accumulating result
      -> [PredType]
   go [] min_preds = map fst min_preds
   go (work_item@(p,_) : work_list) min_preds
     | p `in_cloud` work_list || p `in_cloud` min_preds
     = go work_list min_preds
     | otherwise
     = go work_list (work_item : min_preds)

   in_cloud :: PredType -> [PredWithSCs] -> Bool
   in_cloud p ps = or [ p `eqType` p' | (_, scs) <- ps, p' <- scs ]

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

{- Note [Expanding superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we expand superclasses, we use the following algorithm:

expand( so_far, pred ) returns the transitive superclasses of pred,
                               not including pred itself
 1. If pred is not a class constraint, return empty set
       Otherwise pred = C ts
 2. If C is in so_far, return empty set (breaks loops)
 3. Find the immediate superclasses constraints of (C ts)
 4. For each such sc_pred, return (sc_pred : expand( so_far+C, D ss )

Notice that

 * With normal Haskell-98 classes, the loop-detector will never bite,
   so we'll get all the superclasses.

 * Since there is only a finite number of distinct classes, expansion
   must terminate.

 * The loop breaking is a bit conservative. Notably, a tuple class
   could contain many times without threatening termination:
      (Eq a, (Ord a, Ix a))
   And this is try of any class that we can statically guarantee
   as non-recursive (in some sense).  For now, we just make a special
   case for tuples.  Something better would be cool.

See also TcTyDecls.checkClassCycles.

Note [Inheriting implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

        f x = (x::Int) + ?y

where f is *not* a top-level binding.
From the RHS of f we'll get the constraint (?y::Int).
There are two types we might infer for f:

        f :: Int -> Int

(so we get ?y from the context of f's definition), or

        f :: (?y::Int) => Int -> Int

At first you might think the first was better, because then
?y behaves like a free variable of the definition, rather than
having to be passed at each call site.  But of course, the WHOLE
IDEA is that ?y should be passed at each call site (that's what
dynamic binding means) so we'd better infer the second.

BOTTOM LINE: when *inferring types* you must quantify over implicit
parameters, *even if* they don't mention the bound type variables.
Reason: because implicit parameters, uniquely, have local instance
declarations. See pickQuantifiablePreds.

Note [Quantifying over equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Should we quantify over an equality constraint (s ~ t)?  In general, we don't.
Doing so may simply postpone a type error from the function definition site to
its call site.  (At worst, imagine (Int ~ Bool)).

However, consider this
         forall a. (F [a] ~ Int) => blah
Should we quantify over the (F [a] ~ Int).  Perhaps yes, because at the call
site we will know 'a', and perhaps we have instance  F [Bool] = Int.
So we *do* quantify over a type-family equality where the arguments mention
the quantified variables.

************************************************************************
*                                                                      *
\subsection{Predicates}
*                                                                      *
************************************************************************
-}

isSigmaTy :: TcType -> Bool
-- isSigmaTy returns true of any qualified type.  It doesn't
-- *necessarily* have any foralls.  E.g
--        f :: (?x::Int) => Int -> Int
isSigmaTy ty | Just ty' <- tcView ty = isSigmaTy ty'
isSigmaTy (ForAllTy {}) = True
isSigmaTy (FunTy a _)   = isPredTy a
isSigmaTy _             = False

isRhoTy :: TcType -> Bool   -- True of TcRhoTypes; see Note [TcRhoType]
isRhoTy ty | Just ty' <- tcView ty = isRhoTy ty'
isRhoTy (ForAllTy {}) = False
isRhoTy (FunTy a r)   = not (isPredTy a) && isRhoTy r
isRhoTy _             = True

-- | Like 'isRhoTy', but also says 'True' for 'Infer' types
isRhoExpTy :: ExpType -> Bool
isRhoExpTy (Check ty) = isRhoTy ty
isRhoExpTy (Infer {}) = True

isOverloadedTy :: Type -> Bool
-- Yes for a type of a function that might require evidence-passing
-- Used only by bindLocalMethods
isOverloadedTy ty | Just ty' <- tcView ty = isOverloadedTy ty'
isOverloadedTy (ForAllTy _  ty) = isOverloadedTy ty
isOverloadedTy (FunTy a _)      = isPredTy a
isOverloadedTy _                = False

isFloatTy, isDoubleTy, isIntegerTy, isIntTy, isWordTy, isBoolTy,
    isUnitTy, isCharTy, isAnyTy :: Type -> Bool
isFloatTy      = is_tc floatTyConKey
isDoubleTy     = is_tc doubleTyConKey
isIntegerTy    = is_tc integerTyConKey
isIntTy        = is_tc intTyConKey
isWordTy       = is_tc wordTyConKey
isBoolTy       = is_tc boolTyConKey
isUnitTy       = is_tc unitTyConKey
isCharTy       = is_tc charTyConKey
isAnyTy        = is_tc anyTyConKey

-- | Does a type represent a floating-point number?
isFloatingTy :: Type -> Bool
isFloatingTy ty = isFloatTy ty || isDoubleTy ty

-- | Is a type 'String'?
isStringTy :: Type -> Bool
isStringTy ty
  = case tcSplitTyConApp_maybe ty of
      Just (tc, [arg_ty]) -> tc == listTyCon && isCharTy arg_ty
      _                   -> False

-- | Is a type a 'CallStack'?
isCallStackTy :: Type -> Bool
isCallStackTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` callStackTyConKey
  | otherwise
  = False

-- | Is a 'PredType' a 'CallStack' implicit parameter?
--
-- If so, return the name of the parameter.
isCallStackPred :: PredType -> Maybe FastString
isCallStackPred pred
  | Just (str, ty) <- isIPPred_maybe pred
  , isCallStackTy ty
  = Just str
  | otherwise
  = Nothing

is_tc :: Unique -> Type -> Bool
-- Newtypes are opaque to this
is_tc uniq ty = case tcSplitTyConApp_maybe ty of
                        Just (tc, _) -> uniq == getUnique tc
                        Nothing      -> False

-- | Does the given tyvar appear in the given type outside of any
-- non-newtypes? Assume we're looking for @a@. Says "yes" for
-- @a@, @N a@, @b a@, @a b@, @b (N a)@. Says "no" for
-- @[a]@, @Maybe a@, @T a@, where @N@ is a newtype and @T@ is a datatype.
isTyVarExposed :: TcTyVar -> TcType -> Bool
isTyVarExposed tv (TyVarTy tv')   = tv == tv'
isTyVarExposed tv (TyConApp tc tys)
  | isNewTyCon tc                 = any (isTyVarExposed tv) tys
  | otherwise                     = False
isTyVarExposed _  (LitTy {})      = False
isTyVarExposed tv (AppTy fun arg) = isTyVarExposed tv fun
                                 || isTyVarExposed tv arg
isTyVarExposed _  (ForAllTy {})   = False
isTyVarExposed _  (FunTy {})      = False
isTyVarExposed tv (CastTy ty _)   = isTyVarExposed tv ty
isTyVarExposed _  (CoercionTy {}) = False

-- | Is the equality
--        a ~r ...a....
-- definitely insoluble or not?
--      a ~r Maybe a      -- Definitely insoluble
--      a ~N ...(F a)...  -- Not definitely insoluble
--                        -- Perhaps (F a) reduces to Int
--      a ~R ...(N a)...  -- Not definitely insoluble
--                        -- Perhaps newtype N a = MkN Int
-- See Note [Occurs check error] in
-- TcCanonical for the motivation for this function.
isInsolubleOccursCheck :: EqRel -> TcTyVar -> TcType -> Bool
isInsolubleOccursCheck eq_rel tv ty
  = go ty
  where
    go ty | Just ty' <- tcView ty = go ty'
    go (TyVarTy tv') = tv == tv' || go (tyVarKind tv')
    go (LitTy {})    = False
    go (AppTy t1 t2) = go t1 || go t2
    go (FunTy t1 t2) = go t1 || go t2
    go (ForAllTy (TvBndr tv' _) inner_ty)
      | tv' == tv = False
      | otherwise = go (tyVarKind tv') || go inner_ty
    go (CastTy ty _)  = go ty   -- ToDo: what about the coercion
    go (CoercionTy _) = False   -- ToDo: what about the coercion
    go (TyConApp tc tys)
      | isGenerativeTyCon tc role = any go tys
      | otherwise                 = any go (drop (tyConArity tc) tys)
         -- (a ~ F b a), where F has arity 1,
         -- has an insoluble occurs check

    role = eqRelRole eq_rel

isRigidTy :: TcType -> Bool
isRigidTy ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isGenerativeTyCon tc Nominal
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | isForAllTy ty                           = True
  | otherwise                               = False

isRigidEqPred :: TcLevel -> PredTree -> Bool
-- ^ True of all Nominal equalities that are solidly insoluble
-- This means all equalities *except*
--   * Meta-tv non-SigTv on LHS
--   * Meta-tv SigTv on LHS, tyvar on right
isRigidEqPred tc_lvl (EqPred NomEq ty1 _)
  | Just tv1 <- tcGetTyVar_maybe ty1
  = ASSERT2( tcIsTcTyVar tv1, ppr tv1 )
    not (isMetaTyVar tv1) || isTouchableMetaTyVar tc_lvl tv1

  | otherwise  -- LHS is not a tyvar
  = True

isRigidEqPred _ _ = False  -- Not an equality

{-
************************************************************************
*                                                                      *
\subsection{Transformation of Types to TcTypes}
*                                                                      *
************************************************************************
-}

toTcType :: Type -> TcType
-- The constraint solver expects EvVars to have TcType, in which the
-- free type variables are TcTyVars. So we convert from Type to TcType here
-- A bit tiresome; but one day I expect the two types to be entirely separate
-- in which case we'll definitely need to do this
toTcType = runIdentity . to_tc_type emptyVarSet

toTcTypeBag :: Bag EvVar -> Bag EvVar -- All TyVars are transformed to TcTyVars
toTcTypeBag evvars = mapBag (\tv -> setTyVarKind tv (toTcType (tyVarKind tv))) evvars

to_tc_mapper :: TyCoMapper VarSet Identity
to_tc_mapper
  = TyCoMapper { tcm_smart    = False   -- more efficient not to use smart ctors
               , tcm_tyvar    = tyvar
               , tcm_covar    = covar
               , tcm_hole     = hole
               , tcm_tybinder = tybinder }
  where
    tyvar :: VarSet -> TyVar -> Identity Type
    tyvar ftvs tv
      | Just var <- lookupVarSet ftvs tv = return $ TyVarTy var
      | isTcTyVar tv = TyVarTy <$> updateTyVarKindM (to_tc_type ftvs) tv
      | otherwise
      = do { kind' <- to_tc_type ftvs (tyVarKind tv)
           ; return $ TyVarTy $ mkTcTyVar (tyVarName tv) kind' vanillaSkolemTv }

    covar :: VarSet -> CoVar -> Identity Coercion
    covar ftvs cv
      | Just var <- lookupVarSet ftvs cv = return $ CoVarCo var
      | otherwise = CoVarCo <$> updateVarTypeM (to_tc_type ftvs) cv

    hole :: VarSet -> CoercionHole -> Role -> Type -> Type
         -> Identity Coercion
    hole ftvs h r t1 t2 = mkHoleCo h r <$> to_tc_type ftvs t1
                                       <*> to_tc_type ftvs t2

    tybinder :: VarSet -> TyVar -> ArgFlag -> Identity (VarSet, TyVar)
    tybinder ftvs tv _vis = do { kind' <- to_tc_type ftvs (tyVarKind tv)
                               ; let tv' = mkTcTyVar (tyVarName tv) kind'
                                                     vanillaSkolemTv
                               ; return (ftvs `extendVarSet` tv', tv') }

to_tc_type :: VarSet -> Type -> Identity TcType
to_tc_type = mapType to_tc_mapper

{-
************************************************************************
*                                                                      *
\subsection{Misc}
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
\subsection[TysWiredIn-ext-type]{External types}
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

isFFITy :: Type -> Bool
-- True for any TyCon that can possibly be an arg or result of an FFI call
isFFITy ty = isValid (checkRepTyCon legalFFITyCon ty)

isFFIArgumentTy :: DynFlags -> Safety -> Type -> Validity
-- Checks for valid argument type for a 'foreign import'
isFFIArgumentTy dflags safety ty
   = checkRepTyCon (legalOutgoingTyCon dflags safety) ty

isFFIExternalTy :: Type -> Validity
-- Types that are allowed as arguments of a 'foreign export'
isFFIExternalTy ty = checkRepTyCon legalFEArgTyCon ty

isFFIImportResultTy :: DynFlags -> Type -> Validity
isFFIImportResultTy dflags ty
  = checkRepTyCon (legalFIResultTyCon dflags) ty

isFFIExportResultTy :: Type -> Validity
isFFIExportResultTy ty = checkRepTyCon legalFEResultTyCon ty

isFFIDynTy :: Type -> Type -> Validity
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
    = NotValid (vcat [ text "Expected: Ptr/FunPtr" <+> pprParendType expected <> comma
                     , text "  Actual:" <+> ppr ty ])

isFFILabelTy :: Type -> Validity
-- The type of a foreign label must be Ptr, FunPtr, or a newtype of either.
isFFILabelTy ty = checkRepTyCon ok ty
  where
    ok tc | tc `hasKey` funPtrTyConKey || tc `hasKey` ptrTyConKey
          = IsValid
          | otherwise
          = NotValid (text "A foreign-imported address (via &foo) must have type (Ptr a) or (FunPtr a)")

isFFIPrimArgumentTy :: DynFlags -> Type -> Validity
-- Checks for valid argument type for a 'foreign import prim'
-- Currently they must all be simple unlifted types, or the well-known type
-- Any, which can be used to pass the address to a Haskell object on the heap to
-- the foreign function.
isFFIPrimArgumentTy dflags ty
  | isAnyTy ty = IsValid
  | otherwise  = checkRepTyCon (legalFIPrimArgTyCon dflags) ty

isFFIPrimResultTy :: DynFlags -> Type -> Validity
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
checkRepTyCon :: (TyCon -> Validity) -> Type -> Validity
checkRepTyCon check_tc ty
  = case splitTyConApp_maybe ty of
      Just (tc, tys)
        | isNewTyCon tc -> NotValid (hang msg 2 (mk_nt_reason tc tys $$ nt_fix))
        | otherwise     -> case check_tc tc of
                             IsValid        -> IsValid
                             NotValid extra -> NotValid (msg $$ extra)
      Nothing -> NotValid (quotes (ppr ty) <+> text "is not a data type")
  where
    msg = quotes (ppr ty) <+> text "cannot be marshalled in a foreign call"
    mk_nt_reason tc tys
      | null tys  = text "because its data constructor is not in scope"
      | otherwise = text "because the data constructor for"
                    <+> quotes (ppr tc) <+> text "is not in scope"
    nt_fix = text "Possible fix: import the data constructor to bring it into scope"

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

legalFEArgTyCon :: TyCon -> Validity
legalFEArgTyCon tc
  -- It's illegal to make foreign exports that take unboxed
  -- arguments.  The RTS API currently can't invoke such things.  --SDM 7/2000
  = boxedMarshalableTyCon tc

legalFIResultTyCon :: DynFlags -> TyCon -> Validity
legalFIResultTyCon dflags tc
  | tc == unitTyCon         = IsValid
  | otherwise               = marshalableTyCon dflags tc

legalFEResultTyCon :: TyCon -> Validity
legalFEResultTyCon tc
  | tc == unitTyCon         = IsValid
  | otherwise               = boxedMarshalableTyCon tc

legalOutgoingTyCon :: DynFlags -> Safety -> TyCon -> Validity
-- Checks validity of types going from Haskell -> external world
legalOutgoingTyCon dflags _ tc
  = marshalableTyCon dflags tc

legalFFITyCon :: TyCon -> Validity
-- True for any TyCon that can possibly be an arg or result of an FFI call
legalFFITyCon tc
  | isUnliftedTyCon tc = IsValid
  | tc == unitTyCon    = IsValid
  | otherwise          = boxedMarshalableTyCon tc

marshalableTyCon :: DynFlags -> TyCon -> Validity
marshalableTyCon dflags tc
  | isUnliftedTyCon tc
  , not (isUnboxedTupleTyCon tc || isUnboxedSumTyCon tc)
  , not (null (tyConPrimRep tc)) -- Note [Marshalling void]
  = validIfUnliftedFFITypes dflags
  | otherwise
  = boxedMarshalableTyCon tc

boxedMarshalableTyCon :: TyCon -> Validity
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

  | otherwise = NotValid empty

legalFIPrimArgTyCon :: DynFlags -> TyCon -> Validity
-- Check args of 'foreign import prim', only allow simple unlifted types.
-- Strictly speaking it is unnecessary to ban unboxed tuples and sums here since
-- currently they're of the wrong kind to use in function args anyway.
legalFIPrimArgTyCon dflags tc
  | isUnliftedTyCon tc
  , not (isUnboxedTupleTyCon tc || isUnboxedSumTyCon tc)
  = validIfUnliftedFFITypes dflags
  | otherwise
  = NotValid unlifted_only

legalFIPrimResultTyCon :: DynFlags -> TyCon -> Validity
-- Check result type of 'foreign import prim'. Allow simple unlifted
-- types and also unboxed tuple and sum result types.
legalFIPrimResultTyCon dflags tc
  | isUnliftedTyCon tc
  , isUnboxedTupleTyCon tc || isUnboxedSumTyCon tc
     || not (null (tyConPrimRep tc))   -- Note [Marshalling void]
  = validIfUnliftedFFITypes dflags

  | otherwise
  = NotValid unlifted_only

unlifted_only :: MsgDoc
unlifted_only = text "foreign import prim only accepts simple unlifted types"

validIfUnliftedFFITypes :: DynFlags -> Validity
validIfUnliftedFFITypes dflags
  | xopt LangExt.UnliftedFFITypes dflags =  IsValid
  | otherwise = NotValid (text "To marshal unlifted types, use UnliftedFFITypes")

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
    to say "size 0".  See Trac #4200.

NB: we don't want to detect PredTypes in sizeType (and then call
sizePred on them), or we might get an infinite loop if that PredType
is irreducible. See Trac #5581.
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
    go (FunTy arg res)           = go arg + go res + 1
    go (AppTy fun arg)           = go fun + go arg
    go (ForAllTy (TvBndr tv vis) ty)
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
