{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | This module defines types and simple operations over constraints, as used
-- in the type-checker and constraint solver.
module GHC.Tc.Types.Constraint (
        -- Constraints
        Xi, Ct(..), Cts,
        singleCt, listToCts, ctsElts, consCts, snocCts, extendCtsList,
        isEmptyCts, emptyCts, andCts, ctsPreds,
        isPendingScDictCt, isPendingScDict, pendingScDict_maybe,
        superClassesMightHelp, getPendingWantedScs,
        isWantedCt, isGivenCt,
        isTopLevelUserTypeError, containsUserTypeError, getUserTypeErrorMsg,
        isUnsatisfiableCt_maybe,
        ctEvidence, updCtEvidence,
        ctLoc, ctPred, ctFlavour, ctEqRel, ctOrigin,
        ctRewriters, ctHasNoRewriters, wantedCtHasNoRewriters,
        ctEvId, wantedEvId_maybe, mkTcEqPredLikeEv,
        mkNonCanonical, mkGivens,
        tyCoVarsOfCt, tyCoVarsOfCts,
        tyCoVarsOfCtList, tyCoVarsOfCtsList,
        boundOccNamesOfWC,

        -- Particular forms of constraint
        EqCt(..),    eqCtEvidence, eqCtLHS,
        DictCt(..),  dictCtEvidence, dictCtPred,
        IrredCt(..), irredCtEvidence, mkIrredCt, ctIrredCt, irredCtPred,

        -- QCInst
        QCInst(..), pendingScInst_maybe,

        ExpansionFuel, doNotExpand, consumeFuel, pendingFuel,
        assertFuelPrecondition, assertFuelPreconditionStrict,

        CtIrredReason(..), isInsolubleReason,

        CheckTyEqResult, CheckTyEqProblem, cteProblem, cterClearOccursCheck,
        cteOK, cteImpredicative, cteTypeFamily,
        cteInsolubleOccurs, cteSolubleOccurs, cterSetOccursCheckSoluble,
        cteConcrete, cteSkolemEscape,
        impredicativeProblem, insolubleOccursProblem, solubleOccursProblem,

        cterHasNoProblem, cterHasProblem, cterHasOnlyProblem, cterHasOnlyProblems,
        cterRemoveProblem, cterHasOccursCheck, cterFromKind,

        -- Equality left-hand sides, re-exported from GHC.Core.Predicate
        CanEqLHS(..), canEqLHS_maybe, canTyFamEqLHS_maybe,
        canEqLHSKind, canEqLHSType, eqCanEqLHS,

        -- Holes
        Hole(..), HoleSort(..), isOutOfScopeHole,
        DelayedError(..), NotConcreteError(..),

        WantedConstraints(..), insolubleWC, emptyWC, isEmptyWC,
        isSolvedWC, andWC, unionsWC, mkSimpleWC, mkImplicWC,
        addInsols, dropMisleading, addSimples, addImplics, addHoles,
        addNotConcreteError, addMultiplicityCoercionError, addDelayedErrors,
        tyCoVarsOfWC, tyCoVarsOfWCList,
        insolubleWantedCt, insolubleCt, insolubleIrredCt,
        insolubleImplic, nonDefaultableTyVarsOfWC,
        approximateWCX, approximateWC,

        Implication(..), implicationPrototype, checkTelescopeSkol,
        ImplicStatus(..), isInsolubleStatus, isSolvedStatus,
        UserGiven, getUserGivensFromImplics,
        HasGivenEqs(..), checkImplicationInvariants,
        EvNeedSet(..), emptyEvNeedSet, unionEvNeedSet, extendEvNeedSet, delGivensFromEvNeedSet,

        -- CtLocEnv
        CtLocEnv(..), setCtLocEnvLoc, setCtLocEnvLvl, getCtLocEnvLoc, getCtLocEnvLvl, ctLocEnvInGeneratedCode,

        -- CtEvidence
        CtEvidence(..), TcEvDest(..),
        isWanted, isGiven,
        ctEvPred, ctEvLoc, ctEvOrigin, ctEvEqRel,
        ctEvExpr, ctEvTerm,
        ctEvCoercion, givenCtEvCoercion,
        ctEvEvId, wantedCtEvEvId,
        ctEvRewriters, setWantedCtEvRewriters, ctEvUnique, tcEvDestUnique,
        ctEvRewriteRole, ctEvRewriteEqRel, setCtEvPredType, setCtEvLoc,
        tyCoVarsOfCtEvList, tyCoVarsOfCtEv, tyCoVarsOfCtEvsList,

        -- CtEvidence constructors
        GivenCtEvidence(..), WantedCtEvidence(..),

        -- RewriterSet
        RewriterSet(..), emptyRewriterSet, isEmptyRewriterSet,
           -- exported concretely only for zonkRewriterSet
        addRewriter, unitRewriterSet, unionRewriterSet, rewriterSetFromCts,

        wrapType,

        CtFlavour(..), ctEvFlavour,
        CtFlavourRole, ctEvFlavourRole, ctFlavourRole, eqCtFlavourRole,
        eqCanRewrite, eqCanRewriteFR,

        -- Pretty printing
        pprEvVarTheta,
        pprEvVars, pprEvVarWithType,

  )
  where

import GHC.Prelude

import GHC.Core
import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.TyCo.Ppr

import GHC.Types.Name
import GHC.Types.Var

import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin
import GHC.Tc.Types.CtLoc

import GHC.Builtin.Names

import GHC.Types.Var.Set
import GHC.Types.Unique.Set
import GHC.Types.Name.Reader

import GHC.Utils.FV
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)

import GHC.Data.Bag

import Data.Coerce
import qualified Data.Semigroup as S
import Control.Monad ( msum, when )
import Data.Maybe ( mapMaybe, isJust )

-- these are for CheckTyEqResult
import Data.Word  ( Word8 )
import Data.List  ( intersperse )



{-
************************************************************************
*                                                                      *
*                       Canonical constraints                          *
*                                                                      *
*   These are the constraints the low-level simplifier works with      *
*                                                                      *
************************************************************************
-}

-- | A 'Xi'-type is one that has been fully rewritten with respect
-- to the inert set; that is, it has been rewritten by the algorithm
-- in GHC.Tc.Solver.Rewrite. (Historical note: 'Xi', for years and years,
-- meant that a type was type-family-free. It does *not* mean this
-- any more.)
type Xi = TcType

type Cts = Bag Ct

-- | Says how many layers of superclasses can we expand.
--   Invariant: ExpansionFuel should always be >= 0
-- see Note [Expanding Recursive Superclasses and ExpansionFuel]
type ExpansionFuel = Int

-- | Do not expand superclasses any further
doNotExpand :: ExpansionFuel
doNotExpand = 0

-- | Consumes one unit of fuel.
--   Precondition: fuel > 0
consumeFuel :: ExpansionFuel -> ExpansionFuel
consumeFuel fuel = assertFuelPreconditionStrict fuel $ fuel - 1

-- | Returns True if we have any fuel left for superclass expansion
pendingFuel :: ExpansionFuel -> Bool
pendingFuel n = n > 0

insufficientFuelError :: SDoc
insufficientFuelError = text "Superclass expansion fuel should be > 0"

-- | asserts if fuel is non-negative
assertFuelPrecondition :: ExpansionFuel -> a -> a
{-# INLINE assertFuelPrecondition #-}
assertFuelPrecondition fuel = assertPpr (fuel >= 0) insufficientFuelError

-- | asserts if fuel is strictly greater than 0
assertFuelPreconditionStrict :: ExpansionFuel -> a -> a
{-# INLINE assertFuelPreconditionStrict #-}
assertFuelPreconditionStrict fuel = assertPpr (pendingFuel fuel) insufficientFuelError

-- | Constraint
data Ct
  -- | A dictionary constraint (canonical)
  = CDictCan      DictCt
  -- | An irreducible constraint (non-canonical)
  | CIrredCan     IrredCt
  -- | An equality constraint (canonical)
  | CEqCan        EqCt
  -- | A quantified constraint (canonical)
  | CQuantCan     QCInst
  -- | A non-canonical constraint
  --
  -- See Note [NonCanonical Semantics] in GHC.Tc.Solver.Monad
  | CNonCanonical CtEvidence

--------------- DictCt --------------

-- | A canonical dictionary constraint
data DictCt   -- e.g.  Num ty
  = DictCt { di_ev  :: CtEvidence  -- See Note [Ct/evidence invariant]

           , di_cls :: Class
           , di_tys :: [Xi]   -- di_tys are rewritten w.r.t. inerts, so Xi

           , di_pend_sc :: ExpansionFuel
               -- See Note [The superclass story] in GHC.Tc.Solver.Dict
               -- See Note [Expanding Recursive Superclasses and ExpansionFuel] in GHC.Tc.Solver
               -- Invariants: di_pend_sc > 0 <=>
               --                    (a) di_cls has superclasses
               --                    (b) those superclasses are not yet explored
    }

dictCtEvidence :: DictCt -> CtEvidence
dictCtEvidence = di_ev

dictCtPred :: DictCt -> TcPredType
dictCtPred (DictCt { di_cls = cls, di_tys = tys }) = mkClassPred cls tys

instance Outputable DictCt where
  ppr dict = ppr (CDictCan dict)

--------------- EqCt --------------

{- Note [Canonical equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An EqCt is a canonical equality constraint, one that can live in the inert set,
and that can be used to rewrite other constraints. It satisfies these invariants:

  * (TyEq:OC) lhs does not occur in rhs (occurs check)
              Note [EqCt occurs check]

  * (TyEq:F) rhs has no foralls
      (this avoids substituting a forall for the tyvar in other types)

  * (TyEq:K) typeKind lhs `tcEqKind` typeKind rhs; Note [Ct kind invariant]

  * (TyEq:N) If the equality is representational, rhs is not headed by a saturated
    application of a newtype TyCon. See GHC.Tc.Solver.Equality
    Note [No top-level newtypes on RHS of representational equalities].
    (Applies only when constructor of newtype is in scope.)

  * (TyEq:U) An EqCt is not immediately unifiable. If we can unify a:=ty, we
    will not form an EqCt (a ~ ty).

  * (TyEq:CH) rhs does not mention any coercion holes that resulted from fixing up
    a hetero-kinded equality.  See Note [Equalities with heterogeneous kinds] in
    GHC.Tc.Solver.Equality, wrinkle (EIK2)

These invariants ensure that the EqCts in inert_eqs constitute a terminating
generalised substitution. See Note [inert_eqs: the inert equalities]
in GHC.Tc.Solver.InertSet for what these words mean!

Note [EqCt occurs check]
~~~~~~~~~~~~~~~~~~~~~~~~~~
A CEqCan relates a CanEqLHS (a type variable or type family applications) on
its left to an arbitrary type on its right. It is used for rewriting.
Because it is used for rewriting, it would be disastrous if the RHS
were to mention the LHS: this would cause a loop in rewriting.

We thus perform an occurs-check. There is, of course, some subtlety:

* For type variables, the occurs-check looks deeply including kinds of
  type variables. This is because a CEqCan over a meta-variable is
  also used to inform unification, via `checkTyEqRhs`, called in
  `canEqCanLHSFinish_try_unification`.
  If the LHS appears anywhere in the RHS, at all, unification will create
  an infinite structure, which is bad.

* For type family applications, the occurs-check is shallow; it looks
  only in places where we might rewrite. (Specifically, it does not
  look in kinds or coercions.) An occurrence of the LHS in, say, an
  RHS coercion is OK, as we do not rewrite in coercions. No loop to
  be found.

  You might also worry about the possibility that a type family
  application LHS doesn't exactly appear in the RHS, but something
  that reduces to the LHS does. Yet that can't happen: the RHS is
  already inert, with all type family redexes reduced. So a simple
  syntactic check is just fine.

The occurs check is performed in GHC.Tc.Utils.Unify.checkTyEqRhs
and forms condition T3 in Note [Extending the inert equalities]
in GHC.Tc.Solver.InertSet.
-}

-- | A canonical equality constraint.
--
-- See Note [Canonical equalities] in GHC.Tc.Types.Constraint.
data EqCt
  = EqCt {  -- CanEqLHS ~ rhs
      eq_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]
      eq_lhs    :: CanEqLHS,
      eq_rhs    :: Xi,         -- See invariants above
      eq_eq_rel :: EqRel       -- INVARIANT: cc_eq_rel = ctEvEqRel cc_ev
    }

eqCtEvidence :: EqCt -> CtEvidence
eqCtEvidence = eq_ev

eqCtLHS :: EqCt -> CanEqLHS
eqCtLHS = eq_lhs

--------------- IrredCt --------------

data IrredCt    -- These stand for yet-unusable predicates
                -- See Note [CIrredCan constraints]
  = IrredCt { ir_ev     :: CtEvidence   -- See Note [Ct/evidence invariant]
            , ir_reason :: CtIrredReason }

mkIrredCt :: CtIrredReason -> CtEvidence -> Ct
mkIrredCt reason ev = CIrredCan (IrredCt { ir_ev = ev, ir_reason = reason })

irredCtEvidence :: IrredCt -> CtEvidence
irredCtEvidence = ir_ev

irredCtPred :: IrredCt -> PredType
irredCtPred = ctEvPred . irredCtEvidence

ctIrredCt :: CtIrredReason -> Ct -> IrredCt
ctIrredCt _      (CIrredCan ir) = ir
ctIrredCt reason ct             = IrredCt { ir_ev = ctEvidence ct
                                          , ir_reason = reason }

instance Outputable IrredCt where
  ppr irred = ppr (CIrredCan irred)

--------------- QCInst --------------

-- | A quantified constraint, also called a "local instance"
-- (a simplified version of 'ClsInst').
--
-- See Note [Quantified constraints] in GHC.Tc.Solver.Solve
data QCInst
  -- | A quantified constraint, of type @forall tvs. context => ty@
  = QCI { qci_ev    :: CtEvidence -- See Note [Ct/evidence invariant]
        , qci_tvs   :: [TcTyVar]  -- ^ @tvs@
        , qci_theta :: TcThetaType
        , qci_body  :: TcPredType -- ^ the body of the @forall@, i.e. @ty@
        , qci_pend_sc :: ExpansionFuel
             -- ^ Invariants: qci_pend_sc > 0 =>
             --
             --    (a) 'qci_body' is a ClassPred
             --    (b) this class has superclass(es), and
             --    (c) the superclass(es) are not explored yet
             --
             -- Same as 'di_pend_sc' flag in 'DictCt'
             -- See Note [Expanding Recursive Superclasses and ExpansionFuel] in GHC.Tc.Solver
    }

instance Outputable QCInst where
  ppr (QCI { qci_ev = ev }) = ppr ev

------------------------------------------------------------------------------
--
-- Holes and other delayed errors
--
------------------------------------------------------------------------------

-- | A delayed error, to be reported after constraint solving, in order to benefit
-- from deferred unifications.
data DelayedError
  = DE_Hole Hole
    -- ^ A hole (in a type or in a term).
    --
    -- See Note [Holes in expressions].
  | DE_NotConcrete NotConcreteError
    -- ^ A type could not be ensured to be concrete.
    --
    -- See Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete.
  | DE_Multiplicity TcCoercion CtLoc
    -- ^ An error if the TcCoercion isn't a reflexivity constraint.
    --
    -- See Note [Coercion errors in tcSubMult] in GHC.Tc.Utils.Unify.


instance Outputable DelayedError where
  ppr (DE_Hole hole) = ppr hole
  ppr (DE_NotConcrete err) = ppr err
  ppr (DE_Multiplicity co _) = ppr co

-- | A hole stores the information needed to report diagnostics
-- about holes in terms (unbound identifiers or underscores) or
-- in types (also called wildcards, as used in partial type
-- signatures). See Note [Holes in expressions] for holes in terms.
data Hole
  = Hole { hole_sort :: HoleSort -- ^ What flavour of hole is this?
         , hole_occ  :: RdrName  -- ^ The name of this hole
         , hole_ty   :: TcType   -- ^ Type to be printed to the user
                                 -- For expression holes: type of expr
                                 -- For type holes: the missing type
         , hole_loc  :: CtLoc    -- ^ Where hole was written
         }
           -- For the hole_loc, we usually only want the TcLclEnv stored within.
           -- Except when we rewrite, where we need a whole location. And this
           -- might get reported to the user if reducing type families in a
           -- hole type loops.


-- | Used to indicate which sort of hole we have.
data HoleSort = ExprHole HoleExprRef
                 -- ^ Either an out-of-scope variable or a "true" hole in an
                 -- expression (TypedHoles).
                 -- The HoleExprRef says where to write the
                 -- the erroring expression for -fdefer-type-errors.
              | TypeHole
                 -- ^ A hole in a type (PartialTypeSignatures)
              | ConstraintHole
                 -- ^ A hole in a constraint, like @f :: (_, Eq a) => ...
                 -- Differentiated from TypeHole because a ConstraintHole
                 -- is simplified differently. See
                 -- Note [Do not simplify ConstraintHoles] in GHC.Tc.Solver.

instance Outputable Hole where
  ppr (Hole { hole_sort = ExprHole ref
            , hole_occ  = occ
            , hole_ty   = ty })
    = parens $ (braces $ ppr occ <> colon <> ppr ref) <+> dcolon <+> ppr ty
  ppr (Hole { hole_sort = _other
            , hole_occ  = occ
            , hole_ty   = ty })
    = braces $ ppr occ <> colon <> ppr ty

instance Outputable HoleSort where
  ppr (ExprHole ref) = text "ExprHole:" <+> ppr ref
  ppr TypeHole       = text "TypeHole"
  ppr ConstraintHole = text "ConstraintHole"

-- | Why did we require that a certain type be concrete?
data NotConcreteError
  -- | Concreteness was required by a representation-polymorphism
  -- check.
  --
  -- See Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete.
  = NCE_FRR
    { nce_loc        :: CtLoc
      -- ^ Where did this check take place?
    , nce_frr_origin :: FixedRuntimeRepOrigin
      -- ^ Which representation-polymorphism check did we perform?
    }

instance Outputable NotConcreteError where
  ppr (NCE_FRR { nce_frr_origin = frr_orig })
    = text "NCE_FRR" <+> parens (ppr (frr_type frr_orig))

------------
-- | Used to indicate extra information about why a CIrredCan is irreducible
data CtIrredReason
  = IrredShapeReason
      -- ^ This constraint has a non-canonical shape (e.g. @c Int@, for a variable @c@)

  | NonCanonicalReason CheckTyEqResult
   -- ^ An equality where some invariant other than (TyEq:H) of 'CEqCan' is not satisfied;
   -- the 'CheckTyEqResult' states exactly why

  | ReprEqReason
    -- ^ An equality that cannot be decomposed because it is representational.
    -- Example: @a b ~R# Int@.
    -- These might still be solved later.
    -- INVARIANT: The constraint is a representational equality constraint

  | ShapeMismatchReason
    -- ^ A nominal equality that relates two wholly different types,
    -- like @Int ~# Bool@ or @a b ~# 3@.
    -- INVARIANT: The constraint is a nominal equality constraint

  | AbstractTyConReason
    -- ^ An equality like @T a b c ~ Q d e@ where either @T@ or @Q@
    -- is an abstract type constructor. See Note [Skolem abstract data]
    -- in GHC.Core.TyCon.
    -- INVARIANT: The constraint is an equality constraint between two TyConApps

  | PluginReason
    -- ^ A typechecker plugin returned this in the pluginBadCts field
    -- of TcPluginProgress

instance Outputable CtIrredReason where
  ppr IrredShapeReason          = text "(irred)"
  ppr (NonCanonicalReason cter) = ppr cter
  ppr ReprEqReason              = text "(repr)"
  ppr ShapeMismatchReason       = text "(shape)"
  ppr AbstractTyConReason       = text "(abstc)"
  ppr PluginReason              = text "(plugin)"

-- | Are we sure that more solving will never solve this constraint?
isInsolubleReason :: CtIrredReason -> Bool
isInsolubleReason IrredShapeReason          = False
isInsolubleReason (NonCanonicalReason cter) = cterIsInsoluble cter
isInsolubleReason ReprEqReason              = False
isInsolubleReason ShapeMismatchReason       = True
isInsolubleReason AbstractTyConReason       = True
isInsolubleReason PluginReason              = True

------------------------------------------------------------------------------
--
-- CheckTyEqResult, defined here because it is stored in a CtIrredReason
--
------------------------------------------------------------------------------

-- | A /set/ of problems in checking the validity of a type equality.
-- See 'checkTypeEq'.
newtype CheckTyEqResult = CTER Word8

-- | No problems in checking the validity of a type equality.
cteOK :: CheckTyEqResult
cteOK = CTER zeroBits

-- | Check whether a 'CheckTyEqResult' is marked successful.
cterHasNoProblem :: CheckTyEqResult -> Bool
cterHasNoProblem (CTER 0) = True
cterHasNoProblem _        = False

-- | An /individual/ problem that might be logged in a 'CheckTyEqResult'
newtype CheckTyEqProblem = CTEP Word8

cteImpredicative, cteTypeFamily, cteInsolubleOccurs,
  cteSolubleOccurs, cteConcrete,
  cteSkolemEscape :: CheckTyEqProblem
cteImpredicative   = CTEP (bit 0)   -- Forall or (=>) encountered
cteTypeFamily      = CTEP (bit 1)   -- Type family encountered

cteInsolubleOccurs = CTEP (bit 2)   -- Occurs-check
cteSolubleOccurs   = CTEP (bit 3)   -- Occurs-check under a type function, or in a coercion,
                                    -- or in a representational equality; see
   -- See Note [Occurs check and representational equality]
   -- cteSolubleOccurs must be one bit to the left of cteInsolubleOccurs
   -- See also Note [Insoluble mis-match] in GHC.Tc.Errors

-- NB:  CTEP (bit 4) currently unused

cteConcrete        = CTEP (bit 5)   -- Type variable that can't be made concrete
                                    --    e.g. alpha[conc] ~ Maybe beta[tv]

cteSkolemEscape    = CTEP (bit 6)   -- Skolem escape e.g.  alpha[2] ~ b[sk,4]

cteProblem :: CheckTyEqProblem -> CheckTyEqResult
cteProblem (CTEP mask) = CTER mask

impredicativeProblem, insolubleOccursProblem, solubleOccursProblem :: CheckTyEqResult
impredicativeProblem   = cteProblem cteImpredicative
insolubleOccursProblem = cteProblem cteInsolubleOccurs
solubleOccursProblem   = cteProblem cteSolubleOccurs

occurs_mask :: Word8
occurs_mask = insoluble_mask .|. soluble_mask
  where
    CTEP insoluble_mask = cteInsolubleOccurs
    CTEP soluble_mask   = cteSolubleOccurs

-- | Check whether a 'CheckTyEqResult' has a 'CheckTyEqProblem'
cterHasProblem :: CheckTyEqResult -> CheckTyEqProblem -> Bool
CTER bits `cterHasProblem` CTEP mask = (bits .&. mask) /= 0

-- | Check whether a 'CheckTyEqResult' has one 'CheckTyEqProblem' and no other
cterHasOnlyProblem :: CheckTyEqResult -> CheckTyEqProblem -> Bool
CTER bits `cterHasOnlyProblem` CTEP mask = bits == mask

cterHasOnlyProblems :: CheckTyEqResult -> CheckTyEqResult -> Bool
CTER bits `cterHasOnlyProblems` CTER mask = (bits .&. complement mask) == 0

cterRemoveProblem :: CheckTyEqResult -> CheckTyEqProblem -> CheckTyEqResult
cterRemoveProblem (CTER bits) (CTEP mask) = CTER (bits .&. complement mask)

cterHasOccursCheck :: CheckTyEqResult -> Bool
cterHasOccursCheck (CTER bits) = (bits .&. occurs_mask) /= 0

cterClearOccursCheck :: CheckTyEqResult -> CheckTyEqResult
cterClearOccursCheck (CTER bits) = CTER (bits .&. complement occurs_mask)

-- | Mark a 'CheckTyEqResult' as not having an insoluble occurs-check: any occurs
-- check under a type family or in a representation equality is soluble.
cterSetOccursCheckSoluble :: CheckTyEqResult -> CheckTyEqResult
cterSetOccursCheckSoluble (CTER bits)
  = CTER $ ((bits .&. insoluble_mask) `shift` 1) .|. (bits .&. complement insoluble_mask)
  where
    CTEP insoluble_mask = cteInsolubleOccurs

-- | Retain only information about occurs-check failures, because only that
-- matters after recurring into a kind.
cterFromKind :: CheckTyEqResult -> CheckTyEqResult
cterFromKind (CTER bits)
  = CTER (bits .&. occurs_mask)

cterIsInsoluble :: CheckTyEqResult -> Bool
cterIsInsoluble (CTER bits) = (bits .&. mask) /= 0
  where
    mask = impredicative_mask .|. insoluble_occurs_mask

    CTEP impredicative_mask    = cteImpredicative
    CTEP insoluble_occurs_mask = cteInsolubleOccurs

instance Semigroup CheckTyEqResult where
  CTER bits1 <> CTER bits2 = CTER (bits1 .|. bits2)
instance Monoid CheckTyEqResult where
  mempty = cteOK

instance Eq CheckTyEqProblem where
  (CTEP b1) == (CTEP b2) = b1==b2

instance Outputable CheckTyEqProblem where
  ppr prob@(CTEP bits) = case lookup prob allBits of
                Just s  -> text s
                Nothing -> text "unknown:" <+> ppr bits

instance Outputable CheckTyEqResult where
  ppr cter | cterHasNoProblem cter
           = text "cteOK"
           | otherwise
           = braces $ fcat $ intersperse vbar $
             [ text str
             | (bitmask, str) <- allBits
             , cter `cterHasProblem` bitmask ]

allBits :: [(CheckTyEqProblem, String)]
allBits = [ (cteImpredicative,   "cteImpredicative")
          , (cteTypeFamily,      "cteTypeFamily")
          , (cteInsolubleOccurs, "cteInsolubleOccurs")
          , (cteSolubleOccurs,   "cteSolubleOccurs")
          , (cteConcrete,        "cteConcrete")
          , (cteSkolemEscape,    "cteSkolemEscape") ]

{- Note [CIrredCan constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CIrredCan constraints are used for constraints that are "stuck"
   - we can't solve them (yet)
   - we can't use them to solve other constraints
   - but they may become soluble if we substitute for some
     of the type variables in the constraint

Example 1:  (c Int), where c :: * -> Constraint.  We can't do anything
            with this yet, but if later c := Num, *then* we can solve it

Example 2:  a ~ b, where a :: *, b :: k, where k is a kind variable
            We don't want to use this to substitute 'b' for 'a', in case
            'k' is subsequently unified with (say) *->*, because then
            we'd have ill-kinded types floating about.  Rather we want
            to defer using the equality altogether until 'k' get resolved.

Note [Ct/evidence invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If  ct :: Ct, then extra fields of 'ct' cache precisely the ctev_pred field
of (cc_ev ct), and is fully rewritten wrt the substitution.   Eg for DictCt,
   ctev_pred (di_ev ct) = (di_cls ct) (di_tys ct)
This holds by construction; look at the unique place where DictCt is
built (in GHC.Tc.Solver.Dict.canDictNC).

Note [Ct kind invariant]
~~~~~~~~~~~~~~~~~~~~~~~~
CEqCan requires that the kind of the lhs matches the kind
of the rhs. This is necessary because these constraints are used for substitutions
during solving. If the kinds differed, then the substitution would take a well-kinded
type to an ill-kinded one.
-}

mkNonCanonical :: CtEvidence -> Ct
mkNonCanonical ev = CNonCanonical ev

mkGivens :: CtLoc -> [EvId] -> [Ct]
mkGivens loc ev_ids
  = map mk ev_ids
  where
    mk ev_id = mkNonCanonical (CtGiven (GivenCt { ctev_evar = ev_id
                                               , ctev_pred = evVarPred ev_id
                                               , ctev_loc = loc }))

ctEvidence :: Ct -> CtEvidence
ctEvidence (CQuantCan (QCI { qci_ev = ev }))    = ev
ctEvidence (CEqCan (EqCt { eq_ev = ev }))       = ev
ctEvidence (CIrredCan (IrredCt { ir_ev = ev })) = ev
ctEvidence (CNonCanonical ev)                   = ev
ctEvidence (CDictCan (DictCt { di_ev = ev }))   = ev

updCtEvidence :: (CtEvidence -> CtEvidence) -> Ct -> Ct
updCtEvidence upd ct
 = case ct of
     CQuantCan qci@(QCI { qci_ev = ev })   -> CQuantCan (qci { qci_ev = upd ev })
     CEqCan eq@(EqCt { eq_ev = ev })       -> CEqCan    (eq { eq_ev = upd ev })
     CIrredCan ir@(IrredCt { ir_ev = ev }) -> CIrredCan (ir { ir_ev = upd ev })
     CNonCanonical ev                      -> CNonCanonical (upd ev)
     CDictCan di@(DictCt { di_ev = ev })   -> CDictCan (di { di_ev = upd ev })

ctLoc :: Ct -> CtLoc
ctLoc = ctEvLoc . ctEvidence

ctOrigin :: Ct -> CtOrigin
ctOrigin = ctLocOrigin . ctLoc

ctPred :: Ct -> PredType
-- See Note [Ct/evidence invariant]
ctPred ct = ctEvPred (ctEvidence ct)

ctRewriters :: Ct -> RewriterSet
ctRewriters = ctEvRewriters . ctEvidence

ctEvId :: HasDebugCallStack => Ct -> EvVar
-- The evidence Id for this Ct
ctEvId ct = ctEvEvId (ctEvidence ct)

-- | Returns the evidence 'Id' for the argument 'Ct'
-- when this 'Ct' is a 'Wanted'.
--
-- Returns 'Nothing' otherwise.
wantedEvId_maybe :: Ct -> Maybe EvVar
wantedEvId_maybe ct
  = case ctEvidence ct of
    ctev@(CtWanted {})
      | otherwise
      -> Just $ ctEvEvId ctev
    CtGiven {}
      -> Nothing

-- | Makes a new equality predicate with the same role as the given
-- evidence.
mkTcEqPredLikeEv :: CtEvidence -> TcType -> TcType -> TcType
mkTcEqPredLikeEv ev
  = case predTypeEqRel pred of
      NomEq  -> mkNomEqPred
      ReprEq -> mkReprEqPred
  where
    pred = ctEvPred ev

-- | Get the flavour of the given 'Ct'
ctFlavour :: Ct -> CtFlavour
ctFlavour = ctEvFlavour . ctEvidence

-- | Get the equality relation for the given 'Ct'
ctEqRel :: Ct -> EqRel
ctEqRel = ctEvEqRel . ctEvidence

instance Outputable Ct where
  ppr ct = ppr (ctEvidence ct) <+> parens pp_sort
    where
      pp_sort = case ct of
         CEqCan {}        -> text "CEqCan"
         CNonCanonical {} -> text "CNonCanonical"
         CDictCan (DictCt { di_pend_sc = psc })
            | psc > 0       -> text "CDictCan" <> parens (text "psc" <+> ppr psc)
            | otherwise     -> text "CDictCan"
         CIrredCan (IrredCt { ir_reason = reason }) -> text "CIrredCan" <> ppr reason
         CQuantCan (QCI { qci_pend_sc = psc })
            | psc > 0  -> text "CQuantCan"  <> parens (text "psc" <+> ppr psc)
            | otherwise -> text "CQuantCan"

instance Outputable EqCt where
  ppr (EqCt { eq_ev = ev }) = ppr ev

{-
************************************************************************
*                                                                      *
        Simple functions over evidence variables
*                                                                      *
************************************************************************
-}

---------------- Getting bound tyvars -------------------------
boundOccNamesOfWC :: WantedConstraints -> [OccName]
-- Return the OccNames of skolem-bound type variables
-- We could recurse into types, and get the forall-bound ones too,
-- but I'm going wait until that is needed
-- See Note [tidyAvoiding] in GHC.Core.TyCo.Tidy
boundOccNamesOfWC wc = bagToList (go_wc wc)
  where
    go_wc (WC { wc_impl = implics })
      = concatMapBag go_implic implics
    go_implic (Implic { ic_skols = tvs, ic_wanted = wc })
      = listToBag (map getOccName tvs) `unionBags` go_wc wc


---------------- Getting free tyvars -------------------------

-- | Returns free variables of constraints as a non-deterministic set
tyCoVarsOfCt :: Ct -> TcTyCoVarSet
tyCoVarsOfCt = fvVarSet . tyCoFVsOfCt

-- | Returns free variables of constraints as a non-deterministic set
tyCoVarsOfCtEv :: CtEvidence -> TcTyCoVarSet
tyCoVarsOfCtEv = fvVarSet . tyCoFVsOfCtEv

-- | Returns free variables of constraints as a deterministically ordered
-- list. See Note [Deterministic FV] in GHC.Utils.FV.
tyCoVarsOfCtList :: Ct -> [TcTyCoVar]
tyCoVarsOfCtList = fvVarList . tyCoFVsOfCt

-- | Returns free variables of constraints as a deterministically ordered
-- list. See Note [Deterministic FV] in GHC.Utils.FV.
tyCoVarsOfCtEvList :: CtEvidence -> [TcTyCoVar]
tyCoVarsOfCtEvList = fvVarList . tyCoFVsOfType . ctEvPred

-- | Returns free variables of constraints as a composable FV computation.
-- See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoFVsOfCt :: Ct -> FV
tyCoFVsOfCt ct = tyCoFVsOfType (ctPred ct)
  -- This must consult only the ctPred, so that it gets *tidied* fvs if the
  -- constraint has been tidied. Tidying a constraint does not tidy the
  -- fields of the Ct, only the predicate in the CtEvidence.

-- | Returns free variables of constraints as a composable FV computation.
-- See Note [Deterministic FV] in GHC.Utils.FV.
tyCoFVsOfCtEv :: CtEvidence -> FV
tyCoFVsOfCtEv ct = tyCoFVsOfType (ctEvPred ct)

-- | Returns free variables of a bag of constraints as a non-deterministic
-- set. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfCts :: Cts -> TcTyCoVarSet
tyCoVarsOfCts = fvVarSet . tyCoFVsOfCts

-- | Returns free variables of a bag of constraints as a deterministically
-- ordered list. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfCtsList :: Cts -> [TcTyCoVar]
tyCoVarsOfCtsList = fvVarList . tyCoFVsOfCts

-- | Returns free variables of a bag of constraints as a deterministically
-- ordered list. See Note [Deterministic FV] in GHC.Utils.FV.
tyCoVarsOfCtEvsList :: [CtEvidence] -> [TcTyCoVar]
tyCoVarsOfCtEvsList = fvVarList . tyCoFVsOfCtEvs

-- | Returns free variables of a bag of constraints as a composable FV
-- computation. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoFVsOfCts :: Cts -> FV
tyCoFVsOfCts = foldr (unionFV . tyCoFVsOfCt) emptyFV

-- | Returns free variables of a bag of constraints as a composable FV
-- computation. See Note [Deterministic FV] in GHC.Utils.FV.
tyCoFVsOfCtEvs :: [CtEvidence] -> FV
tyCoFVsOfCtEvs = foldr (unionFV . tyCoFVsOfCtEv) emptyFV

-- | Returns free variables of WantedConstraints as a non-deterministic
-- set. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfWC :: WantedConstraints -> TyCoVarSet
-- Only called on *zonked* things
tyCoVarsOfWC = fvVarSet . tyCoFVsOfWC

-- | Returns free variables of WantedConstraints as a deterministically
-- ordered list. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfWCList :: WantedConstraints -> [TyCoVar]
-- Only called on *zonked* things
tyCoVarsOfWCList = fvVarList . tyCoFVsOfWC

-- | Returns free variables of WantedConstraints as a composable FV
-- computation. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoFVsOfWC :: WantedConstraints -> FV
-- Only called on *zonked* things
tyCoFVsOfWC (WC { wc_simple = simple, wc_impl = implic, wc_errors = errors })
  = tyCoFVsOfCts simple `unionFV`
    tyCoFVsOfBag tyCoFVsOfImplic implic `unionFV`
    tyCoFVsOfBag tyCoFVsOfDelayedError errors

-- | Returns free variables of Implication as a composable FV computation.
-- See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoFVsOfImplic :: Implication -> FV
-- Only called on *zonked* things
tyCoFVsOfImplic (Implic { ic_skols = skols
                        , ic_given = givens
                        , ic_wanted = wanted })
  | isEmptyWC wanted
  = emptyFV
  | otherwise
  = tyCoFVsVarBndrs skols  $
    tyCoFVsVarBndrs givens $
    tyCoFVsOfWC wanted

tyCoFVsOfDelayedError :: DelayedError -> FV
tyCoFVsOfDelayedError (DE_Hole hole) = tyCoFVsOfHole hole
tyCoFVsOfDelayedError (DE_NotConcrete {}) = emptyFV
tyCoFVsOfDelayedError (DE_Multiplicity co _) = tyCoFVsOfCo co

tyCoFVsOfHole :: Hole -> FV
tyCoFVsOfHole (Hole { hole_ty = ty }) = tyCoFVsOfType ty

tyCoFVsOfBag :: (a -> FV) -> Bag a -> FV
tyCoFVsOfBag tvs_of = foldr (unionFV . tvs_of) emptyFV

{-
************************************************************************
*                                                                      *
                    CtEvidence
         The "flavor" of a canonical constraint
*                                                                      *
************************************************************************
-}

isWantedCt :: Ct -> Bool
isWantedCt = isWanted . ctEvidence

isGivenCt :: Ct -> Bool
isGivenCt = isGiven . ctEvidence

{- Note [Custom type errors in constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When GHC reports a type-error about an unsolved-constraint, we check
to see if the constraint contains any custom-type errors, and if so
we report them.  Here are some examples of constraints containing type
errors:

TypeError msg           -- The actual constraint is a type error

TypError msg ~ Int      -- Some type was supposed to be Int, but ended up
                        -- being a type error instead

Eq (TypeError msg)      -- A class constraint is stuck due to a type error

F (TypeError msg) ~ a   -- A type function failed to evaluate due to a type err

It is also possible to have constraints where the type error is nested deeper,
for example see #11990, and also:

Eq (F (TypeError msg))  -- Here the type error is nested under a type-function
                        -- call, which failed to evaluate because of it,
                        -- and so the `Eq` constraint was unsolved.
                        -- This may happen when one function calls another
                        -- and the called function produced a custom type error.
-}

-- | A constraint is considered to be a custom type error, if it contains
-- custom type errors anywhere in it.
-- See Note [Custom type errors in constraints]
getUserTypeErrorMsg :: PredType -> Maybe ErrorMsgType
getUserTypeErrorMsg pred = msum $ userTypeError_maybe pred
                                  : map getUserTypeErrorMsg (subTys pred)
  where
   -- Richard thinks this function is very broken. What is subTys
   -- supposed to be doing? Why are exactly-saturated tyconapps special?
   -- What stops this from accidentally ripping apart a call to TypeError?
    subTys t = case splitAppTys t of
                 (t,[]) ->
                   case splitTyConApp_maybe t of
                              Nothing     -> []
                              Just (_,ts) -> ts
                 (t,ts) -> t : ts

-- | Is this an user error message type, i.e. either the form @TypeError err@ or
-- @Unsatisfiable err@?
isTopLevelUserTypeError :: PredType -> Bool
isTopLevelUserTypeError pred =
  isJust (userTypeError_maybe pred) || isJust (isUnsatisfiableCt_maybe pred)

-- | Does this constraint contain an user error message?
--
-- That is, the type is either of the form @Unsatisfiable err@, or it contains
-- a type of the form @TypeError msg@, either at the top level or nested inside
-- the type.
containsUserTypeError :: PredType -> Bool
containsUserTypeError pred =
  isJust (getUserTypeErrorMsg pred) || isJust (isUnsatisfiableCt_maybe pred)

-- | Is this type an unsatisfiable constraint?
-- If so, return the error message.
isUnsatisfiableCt_maybe :: Type -> Maybe ErrorMsgType
isUnsatisfiableCt_maybe t
  | Just (tc, [msg]) <- splitTyConApp_maybe t
  , tc `hasKey` unsatisfiableClassNameKey
  = Just msg
  | otherwise
  = Nothing

isPendingScDict :: Ct -> Bool
isPendingScDict (CDictCan dict_ct) = isPendingScDictCt dict_ct
isPendingScDict _                  = False

isPendingScDictCt :: DictCt -> Bool
-- Says whether this is a CDictCan with di_pend_sc has positive fuel;
-- i.e. pending un-expanded superclasses
isPendingScDictCt (DictCt { di_pend_sc = f }) = pendingFuel f

pendingScDict_maybe :: Ct -> Maybe Ct
-- Says whether this is a CDictCan with di_pend_sc has fuel left,
-- AND if so exhausts the fuel so that they are not expanded again
pendingScDict_maybe (CDictCan dict@(DictCt { di_pend_sc = f }))
  | pendingFuel f = Just (CDictCan (dict { di_pend_sc = doNotExpand }))
  | otherwise     = Nothing
pendingScDict_maybe _ = Nothing

pendingScInst_maybe :: QCInst -> Maybe QCInst
-- Same as isPendingScDict, but for QCInsts
pendingScInst_maybe qci@(QCI { qci_ev = ev, qci_pend_sc = f })
  | isGiven ev -- Do not expand Wanted QCIs
  , pendingFuel f = Just (qci { qci_pend_sc = doNotExpand })
  | otherwise     = Nothing

superClassesMightHelp :: WantedConstraints -> Bool
-- ^ True if taking superclasses of givens, or of wanteds (to perhaps
-- expose more equalities or functional dependencies) might help to
-- solve this constraint.  See Note [When superclasses help]
superClassesMightHelp (WC { wc_simple = simples, wc_impl = implics })
  = anyBag might_help_ct simples || anyBag might_help_implic implics
  where
    might_help_implic ic
       | IC_Unsolved <- ic_status ic = superClassesMightHelp (ic_wanted ic)
       | otherwise                   = False

    might_help_ct ct = not (is_ip ct)

    is_ip (CDictCan (DictCt { di_cls = cls })) = isIPClass cls
    is_ip _                                    = False

getPendingWantedScs :: Cts -> ([Ct], Cts)
-- in the return values [Ct] has original fuel while Cts has fuel exhausted
getPendingWantedScs simples
  = mapAccumBagL get [] simples
  where
    get acc ct | Just ct_exhausted <- pendingScDict_maybe ct
               = (ct:acc, ct_exhausted)
               | otherwise
               = (acc,     ct)

{- Note [When superclasses help]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
First read Note [The superclass story] in GHC.Tc.Solver.Dict

We expand superclasses and iterate only if there is at unsolved wanted
for which expansion of superclasses (e.g. from given constraints)
might actually help. The function superClassesMightHelp tells if
doing this superclass expansion might help solve this constraint.
Note that

  * We look inside implications; maybe it'll help to expand the Givens
    at level 2 to help solve an unsolved Wanted buried inside an
    implication.  E.g.
        forall a. Ord a => forall b. [W] Eq a

  * We say "no" for implicit parameters.
    we have [W] ?x::ty, expanding superclasses won't help:
      - Superclasses can't be implicit parameters
      - If we have a [G] ?x:ty2, then we'll have another unsolved
        [W] ty ~ ty2 (from the functional dependency)
        which will trigger superclass expansion.

    It's a bit of a special case, but it's easy to do.  The runtime cost
    is low because the unsolved set is usually empty anyway (errors
    aside), and the first non-implicit-parameter will terminate the search.

    The special case is worth it (#11480, comment:2) because it
    applies to CallStack constraints, which aren't type errors. If we have
       f :: (C a) => blah
       f x = ...undefined...
    we'll get a CallStack constraint.  If that's the only unsolved
    constraint it'll eventually be solved by defaulting.  So we don't
    want to emit warnings about hitting the simplifier's iteration
    limit.  A CallStack constraint really isn't an unsolved
    constraint; it can always be solved by defaulting.
-}

singleCt :: Ct -> Cts
singleCt = unitBag

andCts :: Cts -> Cts -> Cts
andCts = unionBags

listToCts :: [Ct] -> Cts
listToCts = listToBag

ctsElts :: Cts -> [Ct]
ctsElts = bagToList

consCts :: Ct -> Cts -> Cts
consCts = consBag

snocCts :: Cts -> Ct -> Cts
snocCts = snocBag

extendCtsList :: Cts -> [Ct] -> Cts
extendCtsList cts xs | null xs   = cts
                     | otherwise = cts `unionBags` listToBag xs

emptyCts :: Cts
emptyCts = emptyBag

isEmptyCts :: Cts -> Bool
isEmptyCts = isEmptyBag

ctsPreds :: Cts -> [PredType]
ctsPreds cts = foldr ((:) . ctPred) [] cts

{-
************************************************************************
*                                                                      *
                Wanted constraints
*                                                                      *
************************************************************************
-}

data WantedConstraints
  = WC { wc_simple :: Cts              -- Unsolved constraints, all wanted
       , wc_impl   :: Bag Implication
       , wc_errors :: Bag DelayedError
    }

emptyWC :: WantedConstraints
emptyWC = WC { wc_simple = emptyBag
             , wc_impl   = emptyBag
             , wc_errors = emptyBag }

mkSimpleWC :: [CtEvidence] -> WantedConstraints
mkSimpleWC cts
  = emptyWC { wc_simple = listToBag (map mkNonCanonical cts) }

mkImplicWC :: Bag Implication -> WantedConstraints
mkImplicWC implic
  = emptyWC { wc_impl = implic }

isEmptyWC :: WantedConstraints -> Bool
isEmptyWC (WC { wc_simple = f, wc_impl = i, wc_errors = errors })
  = isEmptyBag f && isEmptyBag i && isEmptyBag errors

-- | Checks whether a the given wanted constraints are solved, i.e.
-- that there are no simple constraints left and all the implications
-- are solved.
isSolvedWC :: WantedConstraints -> Bool
isSolvedWC WC {wc_simple = wc_simple, wc_impl = wc_impl, wc_errors = errors} =
  isEmptyBag wc_simple && allBag (isSolvedStatus . ic_status) wc_impl && isEmptyBag errors

andWC :: WantedConstraints -> WantedConstraints -> WantedConstraints
andWC (WC { wc_simple = f1, wc_impl = i1, wc_errors = e1 })
      (WC { wc_simple = f2, wc_impl = i2, wc_errors = e2 })
  = WC { wc_simple = f1 `unionBags` f2
       , wc_impl   = i1 `unionBags` i2
       , wc_errors = e1 `unionBags` e2 }

unionsWC :: [WantedConstraints] -> WantedConstraints
unionsWC = foldr andWC emptyWC

addSimples :: WantedConstraints -> Bag Ct -> WantedConstraints
addSimples wc cts
  = wc { wc_simple = wc_simple wc `unionBags` cts }
    -- Consider: Put the new constraints at the front, so they get solved first

addImplics :: WantedConstraints -> Bag Implication -> WantedConstraints
addImplics wc implic = wc { wc_impl = wc_impl wc `unionBags` implic }

addInsols :: WantedConstraints -> Bag IrredCt -> WantedConstraints
addInsols wc insols
  = wc { wc_simple = wc_simple wc `unionBags` fmap CIrredCan insols }

addHoles :: WantedConstraints -> Bag Hole -> WantedConstraints
addHoles wc holes
  = wc { wc_errors = mapBag DE_Hole holes `unionBags` wc_errors wc }

addNotConcreteError :: WantedConstraints -> NotConcreteError -> WantedConstraints
addNotConcreteError wc err
  = wc { wc_errors = unitBag (DE_NotConcrete err) `unionBags` wc_errors wc }

-- See Note [Coercion errors in tcSubMult] in GHC.Tc.Utils.Unify.
addMultiplicityCoercionError :: WantedConstraints -> TcCoercion -> CtLoc -> WantedConstraints
addMultiplicityCoercionError wc mult_co loc
  = wc { wc_errors = unitBag (DE_Multiplicity mult_co loc) `unionBags` wc_errors wc }

addDelayedErrors :: WantedConstraints -> Bag DelayedError -> WantedConstraints
addDelayedErrors wc errs
  = wc { wc_errors = errs `unionBags` wc_errors wc }

dropMisleading :: WantedConstraints -> WantedConstraints
-- Drop misleading constraints; really just class constraints
-- See Note [Constraints and errors] in GHC.Tc.Utils.Monad
--   for why this function is so strange, treating the 'simples'
--   and the implications differently.  Sigh.
dropMisleading (WC { wc_simple = simples, wc_impl = implics, wc_errors = errors })
  = WC { wc_simple = filterBag insolubleWantedCt simples
       , wc_impl   = mapBag drop_implic implics
       , wc_errors = filterBag keep_delayed_error errors }
  where
    drop_implic implic
      = implic { ic_wanted = drop_wanted (ic_wanted implic) }
    drop_wanted (WC { wc_simple = simples, wc_impl = implics, wc_errors = errors })
      = WC { wc_simple = filterBag keep_ct simples
           , wc_impl   = mapBag drop_implic implics
           , wc_errors  = filterBag keep_delayed_error errors }

    keep_ct ct
      = case classifyPredType (ctPred ct) of
           ClassPred cls _ -> isEqualityClass cls
             -- isEqualityClass: see (CERR2) in Note [Constraints and errors]
             --                  in GHC.Tc.Utils.Monad
           _ -> True

    keep_delayed_error (DE_Hole hole) = isOutOfScopeHole hole
    keep_delayed_error (DE_NotConcrete {}) = True
    keep_delayed_error (DE_Multiplicity {}) = True

isSolvedStatus :: ImplicStatus -> Bool
isSolvedStatus (IC_Solved {}) = True
isSolvedStatus _              = False

isInsolubleStatus :: ImplicStatus -> Bool
isInsolubleStatus IC_Insoluble    = True
isInsolubleStatus IC_BadTelescope = True
isInsolubleStatus _               = False

insolubleImplic :: Implication -> Bool
insolubleImplic ic = isInsolubleStatus (ic_status ic)

-- | Gather all the type variables from 'WantedConstraints'
-- that it would be unhelpful to default. For the moment,
-- these are only 'ConcreteTv' metavariables participating
-- in a nominal equality whose other side is not concrete;
-- it's usually better to report those as errors instead of
-- defaulting.
nonDefaultableTyVarsOfWC :: WantedConstraints -> TyCoVarSet
-- Currently used in simplifyTop and in tcRule.
-- TODO: should we also use this in decideQuantifiedTyVars, kindGeneralize{All,Some}?
nonDefaultableTyVarsOfWC (WC { wc_simple = simples, wc_impl = implics, wc_errors = errs })
  =             concatMapBag non_defaultable_tvs_of_ct simples
  `unionVarSet` concatMapBag (nonDefaultableTyVarsOfWC . ic_wanted) implics
  `unionVarSet` concatMapBag non_defaultable_tvs_of_err errs
    where

      concatMapBag :: (a -> TyVarSet) -> Bag a -> TyCoVarSet
      concatMapBag f = foldr (\ r acc -> f r `unionVarSet` acc) emptyVarSet

      -- Don't default ConcreteTv metavariables involved
      -- in an equality with something non-concrete: it's usually
      -- better to report the unsolved Wanted.
      --
      -- Example: alpha[conc] ~# rr[sk].
      non_defaultable_tvs_of_ct :: Ct -> TyCoVarSet
      non_defaultable_tvs_of_ct ct =
        -- NB: using classifyPredType instead of inspecting the Ct
        -- so that we deal uniformly with CNonCanonical (which come up in tcRule),
        -- CEqCan (unsolved but potentially soluble, e.g. @alpha[conc] ~# RR@)
        -- and CIrredCan.
        case classifyPredType $ ctPred ct of
          EqPred NomEq lhs rhs
            | Just tv <- getTyVar_maybe lhs
            , isConcreteTyVar tv
            , not (isConcreteType rhs)
            -> unitVarSet tv
            | Just tv <- getTyVar_maybe rhs
            , isConcreteTyVar tv
            , not (isConcreteType lhs)
            -> unitVarSet tv
          _ -> emptyVarSet

      -- Make sure to apply the same logic as above to delayed errors.
      non_defaultable_tvs_of_err (DE_NotConcrete err)
        = case err of
            NCE_FRR { nce_frr_origin = frr } -> tyCoVarsOfType (frr_type frr)
      non_defaultable_tvs_of_err (DE_Hole {}) = emptyVarSet
      non_defaultable_tvs_of_err (DE_Multiplicity {}) = emptyVarSet

insolubleWC :: WantedConstraints -> Bool
insolubleWC (WC { wc_impl = implics, wc_simple = simples, wc_errors = errors })
  =  anyBag insolubleWantedCt simples
       -- insolubleWantedCt: wanteds only: see Note [Given insolubles]
  || anyBag insolubleImplic implics
  || anyBag is_insoluble errors
  where
      is_insoluble (DE_Hole hole) = isOutOfScopeHole hole -- See Note [Insoluble holes]
      is_insoluble (DE_NotConcrete {}) = True
      is_insoluble (DE_Multiplicity {}) = False

insolubleWantedCt :: Ct -> Bool
-- Definitely insoluble, in particular /excluding/ type-hole constraints
-- Namely:
--   a) an insoluble constraint as per 'insolubleIrredCt', i.e. either
--        - an insoluble equality constraint (e.g. Int ~ Bool), or
--        - a custom type error constraint, TypeError msg :: Constraint
--   b) that does not arise from a Given or a Wanted/Wanted fundep interaction
-- See Note [Insoluble Wanteds]
insolubleWantedCt ct
  | CIrredCan ir_ct <- ct
      -- CIrredCan: see (IW1) in Note [Insoluble Wanteds]
  , IrredCt { ir_ev = ev } <- ir_ct
  , CtWanted (WantedCt { ctev_loc = loc, ctev_rewriters = rewriters })  <- ev
      -- It's a Wanted
  , insolubleIrredCt ir_ct
      -- It's insoluble
  , isEmptyRewriterSet rewriters
      -- It has no rewriters; see (IW2) in Note [Insoluble Wanteds]
  , not (isGivenLoc loc)
      -- isGivenLoc: see (IW3) in Note [Insoluble Wanteds]
  , not (isWantedWantedFunDepOrigin (ctLocOrigin loc))
      -- origin check: see (IW4) in Note [Insoluble Wanteds]
  = True

  | otherwise
  = False

-- | Returns True of constraints that are definitely insoluble,
--   as well as TypeError constraints.
-- Can return 'True' for Given constraints, unlike 'insolubleWantedCt'.
--
-- The function is tuned for application /after/ constraint solving
--       i.e. assuming canonicalisation has been done
-- That's why it looks only for IrredCt; all insoluble constraints
-- are put into CIrredCan
insolubleCt :: Ct -> Bool
insolubleCt (CIrredCan ir_ct) = insolubleIrredCt ir_ct
insolubleCt _                 = False

insolubleIrredCt :: IrredCt -> Bool
-- Returns True of Irred constraints that are /definitely/ insoluble
--
-- This function is critical for accurate pattern-match overlap warnings.
-- See Note [Pattern match warnings with insoluble Givens] in GHC.Tc.Solver
--
-- Note that this does not traverse through the constraint to find
-- nested custom type errors: it only detects @TypeError msg :: Constraint@,
-- and not e.g. @Eq (TypeError msg)@.
insolubleIrredCt (IrredCt { ir_ev = ev, ir_reason = reason })
  =  isInsolubleReason reason
  || isTopLevelUserTypeError (ctEvPred ev)
  -- NB: 'isTopLevelUserTypeError' detects constraints of the form "TypeError msg"
  -- and "Unsatisfiable msg". It deliberately does not detect TypeError
  -- nested in a type (e.g. it does not use "containsUserTypeError"), as that
  -- would be too eager: the TypeError might appear inside a type family
  -- application which might later reduce, but we only want to return 'True'
  -- for constraints that are definitely insoluble.
  --
  -- For example: Num (F Int (TypeError "msg")), where F is a type family.
  --
  -- Test case: T11503, with the 'Assert' type family:
  --
  -- > type Assert :: Bool -> Constraint -> Constraint
  -- > type family Assert check errMsg where
  -- >   Assert 'True  _errMsg = ()
  -- >   Assert _check errMsg  = errMsg

-- | Does this hole represent an "out of scope" error?
-- See Note [Insoluble holes]
isOutOfScopeHole :: Hole -> Bool
isOutOfScopeHole (Hole { hole_occ = occ }) = not (startsWithUnderscore (occName occ))

instance Outputable WantedConstraints where
  ppr (WC {wc_simple = s, wc_impl = i, wc_errors = e})
   = text "WC" <+> braces (vcat
        [ ppr_bag (text "wc_simple") s
        , ppr_bag (text "wc_impl") i
        , ppr_bag (text "wc_errors") e ])

ppr_bag :: Outputable a => SDoc -> Bag a -> SDoc
ppr_bag doc bag
 | isEmptyBag bag = empty
 | otherwise      = hang (doc <+> equals)
                       2 (foldr (($$) . ppr) empty bag)

{- Note [Given insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#14325, comment:)
    class (a~b) => C a b

    foo :: C a c => a -> c
    foo x = x

    hm3 :: C (f b) b => b -> f b
    hm3 x = foo x

In the RHS of hm3, from the [G] C (f b) b we get the insoluble
[G] f b ~# b.  Then we also get an unsolved [W] C b (f b).
Residual implication looks like
    forall b. C (f b) b => [G] f b ~# b
                           [W] C f (f b)

We do /not/ want to set the implication status to IC_Insoluble,
because that'll suppress reports of [W] C b (f b).  But we
may not report the insoluble [G] f b ~# b either (see Note [Given errors]
in GHC.Tc.Errors), so we may fail to report anything at all!  Yikes.

Bottom line: insolubleWC (called in GHC.Tc.Solver.setImplicationStatus)
             should ignore givens even if they are insoluble.

Note [Insoluble Wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~
insolubleWantedCt returns True of a Wanted constraint that definitely
can't be solved.  But not quite all such constraints; see wrinkles.

(IW1) insolubleWantedCt is tuned for application /after/ constraint
   solving i.e. assuming canonicalisation has been done.  That's why
   it looks only for IrredCt; all insoluble constraints are put into
   CIrredCan

(IW2) We only treat it as insoluble if it has an empty rewriter set.  (See Note
   [Wanteds rewrite Wanteds].)  Otherwise #25325 happens: a Wanted constraint A
   that is /not/ insoluble rewrites some other Wanted constraint B, so B has A
   in its rewriter set.  Now B looks insoluble.  The danger is that we'll
   suppress reporting B because of its empty rewriter set; and suppress
   reporting A because there is an insoluble B lying around.  (This suppression
   happens in GHC.Tc.Errors.mkErrorItem.)  Solution: don't treat B as insoluble.

(IW3) If the Wanted arises from a Given (how can that happen?), don't
   treat it as a Wanted insoluble (obviously).

(IW4) If the Wanted came from a  Wanted/Wanted fundep interaction, don't
   treat the constraint as insoluble. See Note [Suppressing confusing errors]
   in GHC.Tc.Errors

Note [Insoluble holes]
~~~~~~~~~~~~~~~~~~~~~~
Hole constraints that ARE NOT treated as truly insoluble:
  a) type holes, arising from PartialTypeSignatures,
  b) "true" expression holes arising from TypedHoles

An "expression hole" or "type hole" isn't really an error
at all; it's a report saying "_ :: Int" here.  But an out-of-scope
variable masquerading as expression holes IS treated as truly
insoluble, so that it trumps other errors during error reporting.
Yuk!

************************************************************************
*                                                                      *
                Implication constraints
*                                                                      *
************************************************************************
-}

data Implication
  = Implic {   -- Invariants for a tree of implications:
               -- see TcType Note [TcLevel invariants]

      ic_tclvl :: TcLevel,       -- TcLevel of unification variables
                                 -- allocated /inside/ this implication

      ic_info  :: SkolemInfoAnon,    -- See Note [Skolems in an implication]
                                     -- See Note [Shadowing in a constraint]

      ic_skols :: [TcTyVar],     -- Introduced skolems; always skolem TcTyVars
                                 -- Their level numbers should be precisely ic_tclvl
                                 -- Their SkolemInfo should be precisely ic_info (almost)
                                 --       See Note [Implication invariants]

      ic_given  :: [EvVar],      -- Given evidence variables
                                 --   (order does not matter)
                                 -- See Invariant (GivenInv) in GHC.Tc.Utils.TcType

      ic_given_eqs :: HasGivenEqs,  -- Are there Given equalities here?

      ic_warn_inaccessible :: Bool,
                                 -- True <=> we should report inaccessible code
                                 -- Note [Avoid -Winaccessible-code when deriving]
                                 -- in GHC.Tc.TyCl.Instance

      ic_env   :: !CtLocEnv,
                                 -- Records the context at the time of creation.
                                 --
                                 -- This provides all the information needed about
                                 -- the context to report the source of errors linked
                                 -- to this implication.

      ic_wanted :: WantedConstraints,  -- The wanteds
                                       -- See Invariant (WantedInf) in GHC.Tc.Utils.TcType

      ic_binds  :: EvBindsVar,    -- Points to the place to fill in the
                                  -- abstraction and bindings.

      -- The ic_need fields keep track of which Given evidence
      -- is used by this implication or its children
      -- See Note [Tracking redundant constraints]
      -- NB: these sets include stuff used by fully-solved nested implications
      --     that have since been discarded
      ic_need  :: EvNeedSet,        -- All needed Given evidence, from this implication
                                    --   or outer ones
                                    -- That is, /after/ deleting the binders of ic_binds,
                                    --   but /before/ deleting ic_givens

      ic_need_implic :: EvNeedSet,  -- Union of of the ic_need of all implications in ic_wanted
                                    -- /including/ any fully-solved implications that have been
                                    -- discarded by `pruneImplications`.  This discarding is why
                                    -- we need to keep this field in the first place.

      ic_status   :: ImplicStatus
    }

data EvNeedSet = ENS { ens_dms :: VarSet   -- Needed only by default methods
                     , ens_fvs :: VarSet   -- Needed by things /other than/ default methods
                       -- See (TRC5) in Note [Tracking redundant constraints]
                 }

emptyEvNeedSet :: EvNeedSet
emptyEvNeedSet = ENS { ens_dms = emptyVarSet, ens_fvs = emptyVarSet }

unionEvNeedSet :: EvNeedSet -> EvNeedSet -> EvNeedSet
unionEvNeedSet (ENS { ens_dms = dm1, ens_fvs = fv1 })
               (ENS { ens_dms = dm2, ens_fvs = fv2 })
  = ENS { ens_dms = dm1 `unionVarSet` dm2, ens_fvs = fv1 `unionVarSet` fv2 }

extendEvNeedSet :: EvNeedSet -> Var -> EvNeedSet
extendEvNeedSet ens@(ENS { ens_fvs = fvs }) v = ens { ens_fvs = fvs `extendVarSet` v }

delGivensFromEvNeedSet :: EvNeedSet -> [Var] -> EvNeedSet
delGivensFromEvNeedSet (ENS { ens_dms = dms, ens_fvs = fvs }) givens
  = ENS { ens_dms = dms `delVarSetList` givens
        , ens_fvs = fvs `delVarSetList` givens }

implicationPrototype :: CtLocEnv -> Implication
implicationPrototype ct_loc_env
   = Implic { -- These fields must be initialised
              ic_tclvl      = panic "newImplic:tclvl"
            , ic_binds      = panic "newImplic:binds"
            , ic_info       = panic "newImplic:info"
            , ic_warn_inaccessible = panic "newImplic:warn_inaccessible"

              -- Given by caller
            , ic_env = ct_loc_env

              -- The rest have sensible default values
            , ic_skols       = []
            , ic_given       = []
            , ic_wanted      = emptyWC
            , ic_given_eqs   = MaybeGivenEqs
            , ic_status      = IC_Unsolved
            , ic_need        = emptyEvNeedSet
            , ic_need_implic = emptyEvNeedSet }

data ImplicStatus
  = IC_Solved     -- All wanteds in the tree are solved, all the way down
       { ics_dead :: [EvVar] }  -- Subset of ic_given that are not needed
         -- See Note [Tracking redundant constraints] in GHC.Tc.Solver

  | IC_Insoluble  -- At least one insoluble Wanted constraint in the tree

  | IC_BadTelescope  -- Solved, but the skolems in the telescope are out of
                     -- dependency order. See Note [Checking telescopes]

  | IC_Unsolved   -- Neither of the above; might go either way

data HasGivenEqs -- See Note [HasGivenEqs]
  = NoGivenEqs      -- Definitely no given equalities,
                    --   except by Note [Let-bound skolems] in GHC.Tc.Solver.InertSet
  | LocalGivenEqs   -- Might have Given equalities, but only ones that affect only
                    --   local skolems e.g. forall a b. (a ~ F b) => ...
  | MaybeGivenEqs   -- Might have any kind of Given equalities; no floating out
                    --   is possible.
  deriving Eq

type UserGiven = Implication

getUserGivensFromImplics :: [Implication] -> [UserGiven]
getUserGivensFromImplics implics
  = reverse (filterOut (null . ic_given) implics)

{- Note [HasGivenEqs]
~~~~~~~~~~~~~~~~~~~~~
The GivenEqs data type describes the Given constraints of an implication constraint:

* NoGivenEqs: definitely no Given equalities, except perhaps let-bound skolems
  which don't count: see Note [Let-bound skolems] in GHC.Tc.Solver.InertSet
  Examples: forall a. Eq a => ...
            forall a. (Show a, Num a) => ...
            forall a. a ~ Either Int Bool => ...  -- Let-bound skolem

* LocalGivenEqs: definitely no Given equalities that would affect principal
  types.  But may have equalities that affect only skolems of this implication
  (and hence do not affect principal types)
  Examples: forall a. F a ~ Int => ...
            forall a b. F a ~ G b => ...

* MaybeGivenEqs: may have Given equalities that would affect principal
  types
  Examples: forall. (a ~ b) => ...
            forall a. F a ~ b => ...
            forall a. c a => ...       -- The 'c' might be instantiated to (b ~)
            forall a. C a b => ....
               where class x~y => C a b
               so there is an equality in the superclass of a Given

The HasGivenEqs classifications affect two things:

* Suppressing redundant givens during error reporting; see GHC.Tc.Errors
  Note [Suppress redundant givens during error reporting]

* Floating in approximateWC.

Specifically, here's how it goes:

                 Stops floating    |   Suppresses Givens in errors
                 in approximateWC  |
                 -----------------------------------------------
 NoGivenEqs         NO             |         YES
 LocalGivenEqs      NO             |         NO
 MaybeGivenEqs      YES            |         NO
-}

instance Outputable Implication where
  ppr (Implic { ic_tclvl = tclvl, ic_skols = skols
              , ic_given = given, ic_given_eqs = given_eqs
              , ic_wanted = wanted, ic_status = status
              , ic_binds = binds
              , ic_need = need, ic_need_implic = need_implic
              , ic_info = info })
   = hang (text "Implic" <+> lbrace)
        2 (sep [ text "TcLevel =" <+> ppr tclvl
               , text "Skolems =" <+> pprTyVars skols
               , text "Given-eqs =" <+> ppr given_eqs
               , text "Status =" <+> ppr status
               , hang (text "Given =")  2 (pprEvVars given)
               , hang (text "Wanted =") 2 (ppr wanted)
               , text "Binds =" <+> ppr binds
               , text "need =" <+> ppr need
               , text "need_implic =" <+> ppr need_implic
               , pprSkolInfo info ] <+> rbrace)

instance Outputable EvNeedSet where
  ppr (ENS { ens_dms = dms, ens_fvs = fvs })
    = text "ENS" <> braces (sep [text "ens_dms =" <+> ppr dms
                                , text "ens_fvs =" <+> ppr fvs])

instance Outputable ImplicStatus where
  ppr IC_Insoluble    = text "Insoluble"
  ppr IC_BadTelescope = text "Bad telescope"
  ppr IC_Unsolved     = text "Unsolved"
  ppr (IC_Solved { ics_dead = dead })
    = text "Solved" <+> (braces (text "Dead givens =" <+> ppr dead))

checkTelescopeSkol :: SkolemInfoAnon -> Bool
-- See Note [Checking telescopes]
checkTelescopeSkol (ForAllSkol {}) = True
checkTelescopeSkol _               = False

instance Outputable HasGivenEqs where
  ppr NoGivenEqs    = text "NoGivenEqs"
  ppr LocalGivenEqs = text "LocalGivenEqs"
  ppr MaybeGivenEqs = text "MaybeGivenEqs"

-- Used in GHC.Tc.Solver.Monad.getHasGivenEqs
instance Semigroup HasGivenEqs where
  NoGivenEqs <> other = other
  other <> NoGivenEqs = other

  MaybeGivenEqs <> _other = MaybeGivenEqs
  _other <> MaybeGivenEqs = MaybeGivenEqs

  LocalGivenEqs <> LocalGivenEqs = LocalGivenEqs

-- Used in GHC.Tc.Solver.Monad.getHasGivenEqs
instance Monoid HasGivenEqs where
  mempty = NoGivenEqs

{- Note [Checking telescopes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When kind-checking a /user-written/ type, we might have a "bad telescope"
like this one:
  data SameKind :: forall k. k -> k -> Type
  type Foo :: forall a k (b :: k). SameKind a b -> Type

The kind of 'a' mentions 'k' which is bound after 'a'.  Oops.

One approach to doing this would be to bring each of a, k, and b into
scope, one at a time, creating a separate implication constraint for
each one, and bumping the TcLevel. This would work, because the kind
of, say, a would be untouchable when k is in scope (and the constraint
couldn't float out because k blocks it). However, it leads to terrible
error messages, complaining about skolem escape. While it is indeed a
problem of skolem escape, we can do better.

Instead, our approach is to bring the block of variables into scope
all at once, creating one implication constraint for the lot:

* We make a single implication constraint when kind-checking
  the 'forall' in Foo's kind, something like
      forall a k (b::k). { wanted constraints }

* Having solved {wanted}, before discarding the now-solved implication,
  the constraint solver checks the dependency order of the skolem
  variables (ic_skols).  This is done in setImplicationStatus.

* This check is only necessary if the implication was born from a
  'forall' in a user-written signature (the HsForAllTy case in
  GHC.Tc.Gen.HsType.  If, say, it comes from checking a pattern match
  that binds existentials, where the type of the data constructor is
  known to be valid (it in tcConPat), no need for the check.

  So the check is done /if and only if/ ic_info is ForAllSkol.

* If ic_info is (ForAllSkol dt dvs), the dvs::SDoc displays the
  original, user-written type variables.

* Be careful /NOT/ to discard an implication with a ForAllSkol
  ic_info, even if ic_wanted is empty.  We must give the
  constraint solver a chance to make that bad-telescope test!  Hence
  the extra guard in emitResidualTvConstraint; see #16247

* Don't mix up inferred and explicit variables in the same implication
  constraint.  E.g.
      foo :: forall a kx (b :: kx). SameKind a b
  We want an implication
      Implic { ic_skol = [(a::kx), kx, (b::kx)], ... }
  but GHC will attempt to quantify over kx, since it is free in (a::kx),
  and it's hopelessly confusing to report an error about quantified
  variables   kx (a::kx) kx (b::kx).
  Instead, the outer quantification over kx should be in a separate
  implication. TL;DR: an explicit forall should generate an implication
  quantified only over those explicitly quantified variables.

Note [Shadowing in a constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We assume NO SHADOWING in a constraint.  Specifically
 * The unification variables are all implicitly quantified at top
   level, and are all unique
 * The skolem variables bound in ic_skols are all fresh when the
   implication is created.
So we can safely substitute. For example, if we have
   forall a.  a~Int => ...(forall b. ...a...)...
we can push the (a~Int) constraint inwards in the "givens" without
worrying that 'b' might clash.

Note [Skolems in an implication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The skolems in an implication are used:

* When considering floating a constraint outside the implication in
  GHC.Tc.Solver.floatEqualities or GHC.Tc.Solver.approximateImplications
  For this, we can treat ic_skols as a set.

* When checking that a /user-specified/ forall (ic_info = ForAllSkol tvs)
  has its variables in the correct order; see Note [Checking telescopes].
  Only for these implications does ic_skols need to be a list.

Nota bene: Although ic_skols is a list, it is not necessarily
in dependency order:
- In the ic_info=ForAllSkol case, the user might have written them
  in the wrong order
- In the case of a type signature like
      f :: [a] -> [b]
  the renamer gathers the implicit "outer" forall'd variables {a,b}, but
  does not know what order to put them in.  The type checker can sort them
  into dependency order, but only after solving all the kind constraints;
  and to do that it's convenient to create the Implication!

So we accept that ic_skols may be out of order.  Think of it as a set or
(in the case of ic_info=ForAllSkol, a list in user-specified, and possibly
wrong, order.

Note [Insoluble constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some of the errors that we get during canonicalization are best
reported when all constraints have been simplified as much as
possible. For instance, assume that during simplification the
following constraints arise:

 [Wanted]   F alpha ~  uf1
 [Wanted]   beta ~ uf1 beta

When canonicalizing the wanted (beta ~ uf1 beta), if we eagerly fail
we will simply see a message:
    'Can't construct the infinite type  beta ~ uf1 beta'
and the user has no idea what the uf1 variable is.

Instead our plan is that we will NOT fail immediately, but:
    (1) Record the "frozen" error in the ic_insols field
    (2) Isolate the offending constraint from the rest of the inerts
    (3) Keep on simplifying/canonicalizing

At the end, we will hopefully have substituted uf1 := F alpha, and we
will be able to report a more informative error:
    'Can't construct the infinite type beta ~ F alpha beta'


************************************************************************
*                                                                      *
                     approximateWC
*                                                                      *
************************************************************************
-}

type ApproxWC = ( Bag Ct          -- Free quantifiable constraints
                , TcTyCoVarSet )  -- Free vars of non-quantifiable constraints
                                  -- due to shape, or enclosing equality

approximateWC :: Bool -> WantedConstraints -> Bag Ct
approximateWC include_non_quantifiable cts
  = fst (approximateWCX include_non_quantifiable cts)

approximateWCX :: Bool -> WantedConstraints -> ApproxWC
-- The "X" means "extended";
--    we return both quantifiable and non-quantifiable constraints
-- See Note [ApproximateWC]
-- See Note [floatKindEqualities vs approximateWC]
approximateWCX include_non_quantifiable wc
  = float_wc False emptyVarSet wc (emptyBag, emptyVarSet)
  where
    float_wc :: Bool           -- True <=> there are enclosing equalities
             -> TcTyCoVarSet   -- Enclosing skolem binders
             -> WantedConstraints
             -> ApproxWC -> ApproxWC
    float_wc encl_eqs trapping_tvs (WC { wc_simple = simples, wc_impl = implics }) acc
      = foldBag_flip (float_ct     encl_eqs trapping_tvs) simples $
        foldBag_flip (float_implic encl_eqs trapping_tvs) implics $
        acc

    float_implic :: Bool -> TcTyCoVarSet -> Implication
                 -> ApproxWC -> ApproxWC
    float_implic encl_eqs trapping_tvs imp
      = float_wc new_encl_eqs new_trapping_tvs (ic_wanted imp)
      where
        new_trapping_tvs = trapping_tvs `extendVarSetList` ic_skols imp
        new_encl_eqs = encl_eqs || ic_given_eqs imp == MaybeGivenEqs

    float_ct :: Bool -> TcTyCoVarSet -> Ct
             -> ApproxWC -> ApproxWC
    float_ct encl_eqs skol_tvs ct acc@(quant, no_quant)
       | isGivenCt ct                                = acc
           -- There can be (insoluble) Given constraints in wc_simple,
           -- there so that we get error reports for unreachable code
           -- See `given_insols` in GHC.Tc.Solver.Solve.solveImplication
       | insolubleCt ct                       = acc
       | pred_tvs `intersectsVarSet` skol_tvs = acc
       | include_non_quantifiable             = add_to_quant
       | is_quantifiable encl_eqs (ctPred ct) = add_to_quant
       | otherwise                            = add_to_no_quant
       where
         pred     = ctPred ct
         pred_tvs = tyCoVarsOfType pred
         add_to_quant    = (ct `consBag` quant, no_quant)
         add_to_no_quant = (quant, no_quant `unionVarSet` pred_tvs)

    is_quantifiable encl_eqs pred
       = case classifyPredType pred of
           -- See the classification in Note [ApproximateWC]
           EqPred eq_rel ty1 ty2
             | encl_eqs  -> False  -- encl_eqs: See Wrinkle (W1)
             | otherwise -> quantify_equality eq_rel ty1 ty2

           ClassPred cls tys
             | Just {} <- isCallStackPred cls tys
               -- NEVER infer a CallStack constraint.  Otherwise we let
               -- the constraints bubble up to be solved from the outer
               -- context, or be defaulted when we reach the top-level.
               -- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
             -> False

             | otherwise
             -> True  -- See Wrinkle (W2)

           IrredPred {}  -> True  -- See Wrinkle (W2)

           ForAllPred {} -> False  -- Never quantify these

    -- See Note [Quantifying over equality constraints]
    quantify_equality NomEq  ty1 ty2 = quant_fun ty1 || quant_fun ty2
    quantify_equality ReprEq _   _   = True

    quant_fun ty
      = case tcSplitTyConApp_maybe ty of
          Just (tc, _) -> isTypeFamilyTyCon tc
          _              -> False

{- Note [ApproximateWC]
~~~~~~~~~~~~~~~~~~~~~~~
approximateWC takes a constraint, typically arising from the RHS of a
let-binding whose type we are *inferring*, and extracts from it some *simple*
constraints that we might plausibly abstract over.  Of course the top-level
simple constraints are plausible, but we also float constraints out from inside,
if they are not captured by skolems.

The same function is used when doing type-class defaulting (see the call
to applyDefaultingRules) to extract constraints that might be defaulted.

We proceed by classifying the constraint:
  * ClassPred:
    * Never pick a CallStack constraint.
      See Note [Overview of implicit CallStacks]
    * Always pick an implicit-parameter constraint.
      Note [Inheriting implicit parameters]
    See wrinkle (W2)

  * EqPred: see Note [Quantifying over equality constraints]

  * IrredPred: we allow anything.

  * ForAllPred: never quantify over these

Wrinkle (W1)
  When inferring most-general types (in simplifyInfer), we
  do *not* quantify over equality constraint if the implication binds
  equality constraints, because that defeats the OutsideIn story.
  Consider data T a where { TInt :: T Int; MkT :: T a }
         f TInt = 3::Int
  We get the implication (a ~ Int => res ~ Int), where so far we've decided
     f :: T a -> res
  We don't want to float (res~Int) out because then we'll infer
     f :: T a -> Int
  which is only on of the possible types. (GHC 7.6 accidentally *did*
  float out of such implications, which meant it would happily infer
  non-principal types.)

Wrinkle (W2)
  We do allow /class/ constraints to float, even if the implication binds
  equalities.  This is a subtle point: see #23224.  In principle, a class
  constraint might ultimately be satisfiable from a constraint bound by an
  implication (see #19106 for an example of this kind), but it's extremely
  obscure and I was unable to construct a concrete example.  In any case, in
  super-subtle cases where this might make a difference, you would be much
  better advised to simply write a type signature.

Wrinkle (W3)
  In findDefaultableGroups we are not worried about the most-general type; and
  we /do/ want to float out of equalities (#12797).  Hence we just union the two
  returned lists.


------ Historical note -----------
There used to be a second caveat, driven by #8155

   2. We do not float out an inner constraint that shares a type variable
      (transitively) with one that is trapped by a skolem.  Eg
          forall a.  F a ~ beta, Integral beta
      We don't want to float out (Integral beta).  Doing so would be bad
      when defaulting, because then we'll default beta:=Integer, and that
      makes the error message much worse; we'd get
          Can't solve  F a ~ Integer
      rather than
          Can't solve  Integral (F a)

      Moreover, floating out these "contaminated" constraints doesn't help
      when generalising either. If we generalise over (Integral b), we still
      can't solve the retained implication (forall a. F a ~ b).  Indeed,
      arguably that too would be a harder error to understand.

But this transitive closure stuff gives rise to a complex rule for
when defaulting actually happens, and one that was never documented.
Moreover (#12923), the more complex rule is sometimes NOT what
you want.  So I simply removed the extra code to implement the
contamination stuff.  There was zero effect on the testsuite (not even #8155).
------ End of historical note -----------

Note [Quantifying over equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Should we quantify over an equality constraint (s ~ t)
in pickQuantifiablePreds?

* It is always /sound/ to quantify over a constraint -- those
  quantified constraints will need to be proved at each call site.

* We definitely don't want to quantify over (Maybe a ~ Bool), to get
     f :: forall a. (Maybe a ~ Bool) => blah
  That simply postpones a type error from the function definition site to
  its call site.  Fortunately we have already filtered out insoluble
  constraints: see `definite_error` in `simplifyInfer`.

* What about (a ~ T alpha b), where we are about to quantify alpha, `a` and
  `b` are in-scope skolems, and `T` is a data type.  It's pretty unlikely
  that this will be soluble at a call site, so we don't quantify over it.

* What about `(F beta ~ Int)` where we are going to quantify `beta`?
  Should we quantify over the (F beta ~ Int), to get
     f :: forall b. (F b ~ Int) => blah
  Aha!  Perhaps yes, because at the call site we will instantiate `b`, and
  perhaps we have `instance F Bool = Int`. So we *do* quantify over a
  type-family equality where the arguments mention the quantified variables.

This is all a bit ad-hoc.


************************************************************************
*                                                                      *
            Invariant checking (debug only)
*                                                                      *
************************************************************************

Note [Implication invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The skolems of an implication have the following invariants, which are checked
by checkImplicationInvariants:

a) They are all SkolemTv TcTyVars; no TyVars, no unification variables
b) Their TcLevel matches the ic_lvl for the implication
c) Their SkolemInfo matches the implication.

Actually (c) is not quite true.  Consider
   data T a = forall b. MkT a b

In tcConDecl for MkT we'll create an implication with ic_info of
DataConSkol; but the type variable 'a' will have a SkolemInfo of
TyConSkol.  So we allow the tyvar to have a SkolemInfo of TyConFlav if
the implication SkolemInfo is DataConSkol.
-}

checkImplicationInvariants, check_implic :: (HasCallStack, Applicative m) => Implication -> m ()
{-# INLINE checkImplicationInvariants #-}
-- Nothing => OK, Just doc => doc gives info
checkImplicationInvariants implic = when debugIsOn (check_implic implic)

check_implic implic@(Implic { ic_tclvl = lvl
                            , ic_info = skol_info
                            , ic_skols = skols })
  | null bads = pure ()
  | otherwise = massertPpr False (vcat [ text "checkImplicationInvariants failure"
                                       , nest 2 (vcat bads)
                                       , ppr implic ])
  where
    bads = mapMaybe check skols

    check :: TcTyVar -> Maybe SDoc
    check tv | not (isTcTyVar tv)
             = Just (ppr tv <+> text "is not a TcTyVar")
             | otherwise
             = check_details tv (tcTyVarDetails tv)

    check_details :: TcTyVar -> TcTyVarDetails -> Maybe SDoc
    check_details tv (SkolemTv tv_skol_info tv_lvl _)
      | not (tv_lvl `sameDepthAs` lvl)
      = Just (vcat [ ppr tv <+> text "has level" <+> ppr tv_lvl
                   , text "ic_lvl" <+> ppr lvl ])
      | not (skol_info `checkSkolInfoAnon` skol_info_anon)
      = Just (vcat [ ppr tv <+> text "has skol info" <+> ppr skol_info_anon
                   , text "ic_info" <+> ppr skol_info ])
      | otherwise
      = Nothing
      where
        skol_info_anon = getSkolemInfo tv_skol_info
    check_details tv details
      = Just (ppr tv <+> text "is not a SkolemTv" <+> ppr details)

checkSkolInfoAnon :: SkolemInfoAnon   -- From the implication
                  -> SkolemInfoAnon   -- From the type variable
                  -> Bool             -- True <=> ok
-- Used only for debug-checking; checkImplicationInvariants
-- So it doesn't matter much if its's incomplete
checkSkolInfoAnon sk1 sk2 = go sk1 sk2
  where
    go (SigSkol c1 t1 s1)   (SigSkol c2 t2 s2)   = c1==c2 && t1 `tcEqType` t2 && s1==s2
    go (SigTypeSkol cx1)    (SigTypeSkol cx2)    = cx1==cx2

    go (ForAllSkol _)       (ForAllSkol _)       = True

    go (IPSkol ips1)        (IPSkol ips2)        = ips1 == ips2
    go (DerivSkol pred1)    (DerivSkol pred2)    = pred1 `tcEqType` pred2
    go (TyConSkol f1 n1)    (TyConSkol f2 n2)    = f1==f2 && n1==n2
    go (DataConSkol n1)     (DataConSkol n2)     = n1==n2
    go (InstSkol {})        (InstSkol {})        = True
    go (MethSkol n1 d1)     (MethSkol n2 d2)     = n1==n2 && d1==d2
    go FamInstSkol          FamInstSkol          = True
    go BracketSkol          BracketSkol          = True
    go (RuleSkol n1)        (RuleSkol n2)        = n1==n2
    go (SpecESkol n1)       (SpecESkol n2)       = n1==n2
    go (PatSkol c1 _)       (PatSkol c2 _)       = getName c1 == getName c2
       -- Too tedious to compare the HsMatchContexts
    go (InferSkol ids1)     (InferSkol ids2)     = equalLength ids1 ids2 &&
                                                   and (zipWith eq_pr ids1 ids2)
    go (UnifyForAllSkol t1) (UnifyForAllSkol t2) = t1 `tcEqType` t2
    go ReifySkol            ReifySkol            = True
    go RuntimeUnkSkol       RuntimeUnkSkol       = True
    go ArrowReboundIfSkol   ArrowReboundIfSkol   = True
    go (UnkSkol _)          (UnkSkol _)          = True

    -------- Three slightly strange special cases --------
    go (DataConSkol _)      (TyConSkol f _)      = h98_data_decl f
    -- In the H98 declaration  data T a = forall b. MkT a b
    -- in tcConDecl for MkT we'll have a SkolemInfo in the implication of
    -- DataConSkol, but the type variable 'a' will have a SkolemInfo of TyConSkol

    go (DataConSkol _)      FamInstSkol          = True
    -- In  data/newtype instance T a = MkT (a -> a),
    -- in tcConDecl for MkT we'll have a SkolemInfo in the implication of
    -- DataConSkol, but 'a' will have SkolemInfo of FamInstSkol

    go FamInstSkol          (InstSkol {})         = True
    -- In instance C (T a) where { type F (T a) b = ... }
    -- we have 'a' with SkolemInfo InstSkol, but we make an implication wi
    -- SkolemInfo of FamInstSkol.  Very like the ConDecl/TyConSkol case

    go (ForAllSkol _)       _                    = True
    -- Telescope tests: we need a ForAllSkol to force the telescope
    -- test, but the skolems might come from (say) a family instance decl
    --    type instance forall a. F [a] = a->a

    go (SigTypeSkol DerivClauseCtxt) (TyConSkol f _) = h98_data_decl f
    -- e.g.   newtype T a = MkT ... deriving blah
    -- We use the skolems from T (TyConSkol) when typechecking
    -- the deriving clauses (SigTypeSkol DerivClauseCtxt)

    go _ _ = False

    eq_pr :: (Name,TcType) -> (Name,TcType) -> Bool
    eq_pr (i1,_) (i2,_) = i1==i2 -- Types may be differently zonked

    h98_data_decl DataTypeFlavour = True
    h98_data_decl NewtypeFlavour  = True
    h98_data_decl _               = False


{- *********************************************************************
*                                                                      *
            Pretty printing
*                                                                      *
********************************************************************* -}

pprEvVars :: [EvVar] -> SDoc    -- Print with their types
pprEvVars ev_vars = vcat (map pprEvVarWithType ev_vars)

pprEvVarTheta :: [EvVar] -> SDoc
pprEvVarTheta ev_vars = pprTheta (map evVarPred ev_vars)

pprEvVarWithType :: EvVar -> SDoc
pprEvVarWithType v = ppr v <+> dcolon <+> pprType (evVarPred v)



wrapType :: Type -> [TyVar] -> [PredType] -> Type
wrapType ty skols givens = mkSpecForAllTys skols $ mkPhiTy givens ty


{-
************************************************************************
*                                                                      *
            CtEvidence
*                                                                      *
************************************************************************

Note [CtEvidence invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The `ctev_pred` field of a `CtEvidence` is a just a cache for the type
of the evidence. More precisely:

* For Givens, `ctev_pred` = `varType ctev_evar`
* For Wanteds, `ctev_pred` = `evDestType ctev_dest`

where

  evDestType :: TcEvDest -> TcType
  evDestType (EvVarDest evVar)       = varType evVar
  evDestType (HoleDest coercionHole) = varType (coHoleCoVar coercionHole)

The invariant is maintained by `setCtEvPredType`, the only function that
updates the `ctev_pred` field of a `CtEvidence`.

Why is the invariant important? Because when the evidence is a coercion, it may
be used in (CastTy ty co); and then we may call `typeKind` on that type (e.g.
in the kind-check of `eqType`); and expect to see a fully zonked kind.
(This came up in test T13333, in the MR that fixed #20641, namely !6942.)

Historical Note [Evidence field of CtEvidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the past we tried leaving the `ctev_evar`/`ctev_dest` field of a
constraint untouched (and hence un-zonked) on the grounds that it is
never looked at.  But in fact it is: the evidence can become part of a
type (via `CastTy ty kco`) and we may later ask the kind of that type
and expect a zonked result.  (For example, in the kind-check
of `eqType`.)

The safest thing is simply to keep `ctev_evar`/`ctev_dest` in sync
with `ctev_pred`, as stated in `Note [CtEvidence invariants]`.

Note [Bind new Givens immediately]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Givens we make new EvVars and bind them immediately. Two main reasons:
  * Gain sharing.  E.g. suppose we start with g :: C a b, where
       class D a => C a b
       class (E a, F a) => D a
    If we generate all g's superclasses as separate EvTerms we might
    get    selD1 (selC1 g) :: E a
           selD2 (selC1 g) :: F a
           selC1 g :: D a
    which we could do more economically as:
           g1 :: D a = selC1 g
           g2 :: E a = selD1 g1
           g3 :: F a = selD2 g1

  * For *coercion* evidence we *must* bind each given:
      class (a~b) => C a b where ....
      f :: C a b => ....
    Then in f's Givens we have g:(C a b) and the superclass sc(g,0):a~b.
    But that superclass selector can't (yet) appear in a coercion
    (see evTermCoercion), so the easy thing is to bind it to an Id.

So a Given has EvVar inside it rather than (as previously) an EvTerm.

Note [The rewrite-role of a constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The rewrite-role of a constraint says what can rewrite that constraint:

* If the rewrite-role = Nominal, only a nominal equality can rewrite it

* If the rewrite-rule = Representational, either a nominal or
  representational equality can rewrit it.

Notice that the constraint may itself not be an equality at all.
For example, the rewrite-role of (Eq [a]) is Nominal; only nominal
equalities can rewrite it.
-}

-- | A place for type-checking evidence to go after it is generated.
--
--  - Wanted equalities use HoleDest,
--  - other Wanteds use EvVarDest.
data TcEvDest
  = EvVarDest EvVar         -- ^ bind this var to the evidence
              -- EvVarDest is always used for non-type-equalities
              -- e.g. class constraints

  | HoleDest  CoercionHole  -- ^ fill in this hole with the evidence
              -- HoleDest is always used for type-equalities
              -- See Note [Coercion holes] in GHC.Core.TyCo.Rep

data CtEvidence
  = CtGiven  GivenCtEvidence
  | CtWanted WantedCtEvidence

-- | Evidence for a Given constraint
data GivenCtEvidence =
  GivenCt
    { ctev_pred :: TcPredType      -- See Note [Ct/evidence invariant]
    , ctev_evar :: EvVar           -- See Note [CtEvidence invariants]
    , ctev_loc  :: CtLoc }

-- | Evidence for a Wanted constraint
data WantedCtEvidence =
  WantedCt
    { ctev_pred      :: TcPredType     -- See Note [Ct/evidence invariant]
    , ctev_dest      :: TcEvDest       -- See Note [CtEvidence invariants]
    , ctev_loc       :: CtLoc
    , ctev_rewriters :: RewriterSet }  -- See Note [Wanteds rewrite Wanteds]

ctEvPred :: CtEvidence -> TcPredType
-- The predicate of a flavor
ctEvPred (CtGiven (GivenCt { ctev_pred = pred }))  = pred
ctEvPred (CtWanted (WantedCt { ctev_pred = pred })) = pred

ctEvLoc :: CtEvidence -> CtLoc
ctEvLoc (CtGiven (GivenCt { ctev_loc = loc }))  = loc
ctEvLoc (CtWanted (WantedCt { ctev_loc = loc })) = loc

ctEvOrigin :: CtEvidence -> CtOrigin
ctEvOrigin = ctLocOrigin . ctEvLoc

-- | Get the equality relation relevant for a 'CtEvidence'
ctEvEqRel :: HasDebugCallStack => CtEvidence -> EqRel
ctEvEqRel = predTypeEqRel . ctEvPred

-- | Get the rewrite-role relevant for a 'CtEvidence'
-- See Note [The rewrite-role of a constraint]
ctEvRewriteRole :: HasDebugCallStack => CtEvidence -> Role
ctEvRewriteRole = eqRelRole . ctEvRewriteEqRel

ctEvRewriteEqRel :: CtEvidence -> EqRel
-- ^ Return the rewrite-role of an abitrary CtEvidence
-- See Note [The rewrite-role of a constraint]
-- We return ReprEq for (a ~R# b) and NomEq for all other preds
ctEvRewriteEqRel = predTypeEqRel . ctEvPred

ctEvTerm :: CtEvidence -> EvTerm
ctEvTerm ev = EvExpr (ctEvExpr ev)

-- | Extract the set of rewriters from a 'CtEvidence'
-- See Note [Wanteds rewrite Wanteds]
-- If the provided CtEvidence is not for a Wanted, just
-- return an empty set.
ctEvRewriters :: CtEvidence -> RewriterSet
ctEvRewriters (CtWanted (WantedCt { ctev_rewriters = rws })) = rws
ctEvRewriters (CtGiven {})  = emptyRewriterSet

ctHasNoRewriters :: Ct -> Bool
ctHasNoRewriters ev
  = case ctEvidence ev of
      CtWanted wev -> wantedCtHasNoRewriters wev
      CtGiven {}   -> True

wantedCtHasNoRewriters :: WantedCtEvidence -> Bool
wantedCtHasNoRewriters (WantedCt { ctev_rewriters = rws })
  = isEmptyRewriterSet rws

-- | Set the rewriter set of a Wanted constraint.
setWantedCtEvRewriters :: WantedCtEvidence -> RewriterSet -> WantedCtEvidence
setWantedCtEvRewriters ev rs = ev { ctev_rewriters = rs }

ctEvExpr :: HasDebugCallStack => CtEvidence -> EvExpr
ctEvExpr (CtWanted ev@(WantedCt { ctev_dest = HoleDest _ }))
            = Coercion $ ctEvCoercion (CtWanted ev)
ctEvExpr ev = evId (ctEvEvId ev)

givenCtEvCoercion :: GivenCtEvidence -> TcCoercion
givenCtEvCoercion _given@(GivenCt { ctev_evar = ev_id })
  = assertPpr (isCoVar ev_id)
    (text "givenCtEvCoercion used on non-equality Given constraint:" <+> ppr _given)
  $ mkCoVarCo ev_id

ctEvCoercion :: HasDebugCallStack => CtEvidence -> TcCoercion
ctEvCoercion (CtGiven _given@(GivenCt { ctev_evar = ev_id }))
  = assertPpr (isCoVar ev_id)
    (text "ctEvCoercion used on non-equality Given constraint:" <+> ppr (CtGiven _given))
  $ mkCoVarCo ev_id
ctEvCoercion (CtWanted (WantedCt { ctev_dest = dest }))
  | HoleDest hole <- dest
  = -- ctEvCoercion is only called on type equalities
    -- and they always have HoleDests
    mkHoleCo hole
ctEvCoercion ev
  = pprPanic "ctEvCoercion" (ppr ev)

ctEvEvId :: CtEvidence -> EvVar
ctEvEvId (CtWanted wtd)                         = wantedCtEvEvId wtd
ctEvEvId (CtGiven (GivenCt { ctev_evar = ev })) = ev

wantedCtEvEvId :: WantedCtEvidence -> EvVar
wantedCtEvEvId (WantedCt { ctev_dest = EvVarDest ev }) = ev
wantedCtEvEvId (WantedCt { ctev_dest = HoleDest h })   = coHoleCoVar h

ctEvUnique :: CtEvidence -> Unique
ctEvUnique (CtGiven (GivenCt { ctev_evar = ev }))     = varUnique ev
ctEvUnique (CtWanted (WantedCt { ctev_dest = dest })) = tcEvDestUnique dest

tcEvDestUnique :: TcEvDest -> Unique
tcEvDestUnique (EvVarDest ev_var) = varUnique ev_var
tcEvDestUnique (HoleDest co_hole) = varUnique (coHoleCoVar co_hole)

setCtEvLoc :: CtEvidence -> CtLoc -> CtEvidence
setCtEvLoc (CtGiven (GivenCt pred evar _)) loc = CtGiven (GivenCt pred evar loc)
setCtEvLoc (CtWanted (WantedCt pred dest _ rwrs)) loc = CtWanted (WantedCt pred dest loc rwrs)

-- | Set the type of CtEvidence.
--
-- This function ensures that the invariants on 'CtEvidence' hold, by updating
-- the evidence and the ctev_pred in sync with each other.
-- See Note [CtEvidence invariants].
setCtEvPredType :: HasDebugCallStack => CtEvidence -> Type -> CtEvidence
setCtEvPredType (CtGiven old_ev@(GivenCt { ctev_evar = ev })) new_pred
  = CtGiven (old_ev { ctev_pred = new_pred
                    , ctev_evar = setVarType ev new_pred })

setCtEvPredType (CtWanted old_ev@(WantedCt { ctev_dest = dest })) new_pred
  = CtWanted (old_ev { ctev_pred = new_pred
                     , ctev_dest = new_dest })
  where
    new_dest = case dest of
      EvVarDest ev -> EvVarDest (setVarType ev new_pred)
      HoleDest h   -> HoleDest  (setCoHoleType h new_pred)

instance Outputable TcEvDest where
  ppr (HoleDest h)   = text "hole" <> ppr h
  ppr (EvVarDest ev) = ppr ev

instance Outputable GivenCtEvidence where
  ppr = ppr . CtGiven
instance Outputable WantedCtEvidence where
  ppr = ppr . CtWanted

instance Outputable CtEvidence where
  ppr ev = ppr (ctEvFlavour ev)
           <+> hang (pp_ev <+> braces (ppr (ctl_depth (ctEvLoc ev)) <> pp_rewriters))
                         -- Show the sub-goal depth too
                  2 (dcolon <+> pprPredType (ctEvPred ev))
    where
      pp_ev = case ev of
             CtGiven ev -> ppr (ctev_evar ev)
             CtWanted ev -> ppr (ctev_dest ev)

      rewriters = ctEvRewriters ev
      pp_rewriters | isEmptyRewriterSet rewriters = empty
                   | otherwise                    = semi <> ppr rewriters

isWanted :: CtEvidence -> Bool
isWanted (CtWanted {}) = True
isWanted _ = False

isGiven :: CtEvidence -> Bool
isGiven (CtGiven {})  = True
isGiven _ = False

{-
************************************************************************
*                                                                      *
           RewriterSet
*                                                                      *
************************************************************************
-}

-- | Stores a set of CoercionHoles that have been used to rewrite a constraint.
-- See Note [Wanteds rewrite Wanteds].
newtype RewriterSet = RewriterSet (UniqSet CoercionHole)
  deriving newtype (Outputable, Semigroup, Monoid)

emptyRewriterSet :: RewriterSet
emptyRewriterSet = RewriterSet emptyUniqSet

unitRewriterSet :: CoercionHole -> RewriterSet
unitRewriterSet = coerce (unitUniqSet @CoercionHole)

unionRewriterSet :: RewriterSet -> RewriterSet -> RewriterSet
unionRewriterSet = coerce (unionUniqSets @CoercionHole)

isEmptyRewriterSet :: RewriterSet -> Bool
isEmptyRewriterSet = coerce (isEmptyUniqSet @CoercionHole)

addRewriter :: RewriterSet -> CoercionHole -> RewriterSet
addRewriter = coerce (addOneToUniqSet @CoercionHole)

rewriterSetFromCts :: Bag Ct -> RewriterSet
-- Take a bag of Wanted equalities, and collect them as a RewriterSet
rewriterSetFromCts cts
  = foldr add emptyRewriterSet cts
  where
    add ct rw_set =
      case ctEvidence ct of
        CtWanted (WantedCt { ctev_dest = HoleDest hole }) -> rw_set `addRewriter` hole
        _                                                 -> rw_set

{-
************************************************************************
*                                                                      *
           CtFlavour
*                                                                      *
************************************************************************
-}

data CtFlavour
  = Given     -- we have evidence
  | Wanted    -- we want evidence
  deriving Eq

instance Outputable CtFlavour where
  ppr Given  = text "[G]"
  ppr Wanted = text "[W]"

ctEvFlavour :: CtEvidence -> CtFlavour
ctEvFlavour (CtWanted {}) = Wanted
ctEvFlavour (CtGiven {})  = Given

-- | Whether or not one 'Ct' can rewrite another is determined by its
-- flavour and its equality relation. See also
-- Note [Flavours with roles] in GHC.Tc.Solver.InertSet
type CtFlavourRole = (CtFlavour, EqRel)

-- | Extract the flavour, role, and boxity from a 'CtEvidence'
ctEvFlavourRole :: HasDebugCallStack => CtEvidence -> CtFlavourRole
ctEvFlavourRole ev = (ctEvFlavour ev, ctEvRewriteEqRel ev)

-- | Extract the flavour and role from a 'Ct'
eqCtFlavourRole :: EqCt -> CtFlavourRole
eqCtFlavourRole (EqCt { eq_ev = ev, eq_eq_rel = eq_rel })
  = (ctEvFlavour ev, eq_rel)

dictCtFlavourRole :: DictCt -> CtFlavourRole
dictCtFlavourRole (DictCt { di_ev = ev })
  = (ctEvFlavour ev, NomEq)

-- | Extract the flavour and role from a 'Ct'
ctFlavourRole :: HasDebugCallStack => Ct -> CtFlavourRole
-- Uses short-cuts for the Role field, for special cases
ctFlavourRole (CDictCan di_ct) = dictCtFlavourRole di_ct
ctFlavourRole (CEqCan eq_ct)   = eqCtFlavourRole eq_ct
ctFlavourRole ct               = ctEvFlavourRole (ctEvidence ct)

{- Note [eqCanRewrite]
~~~~~~~~~~~~~~~~~~~~~~
(eqCanRewrite ct1 ct2) holds if the constraint ct1 (a CEqCan of form
lhs ~ ty) can be used to rewrite ct2.  It must satisfy the properties of
a can-rewrite relation, see Definition [Can-rewrite relation] in
GHC.Tc.Solver.Monad.

With the solver handling Coercible constraints like equality constraints,
the rewrite conditions must take role into account, never allowing
a representational equality to rewrite a nominal one.

Note [Wanteds rewrite Wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Should one Wanted constraint be allowed to rewrite another?

This example (along with #8450) suggests not:
   f :: a -> Bool
   f x = ( [x,'c'], [x,True] ) `seq` True
Here we get
  [W] a ~ Char
  [W] a ~ Bool
but we do not want to complain about Bool ~ Char!

This example suggests yes (indexed-types/should_fail/T4093a):
  type family Foo a
  f :: (Foo e ~ Maybe e) => Foo e
In the ambiguity check, we get
  [G] g1 :: Foo e ~ Maybe e
  [W] w1 :: Foo alpha ~ Foo e
  [W] w2 :: Foo alpha ~ Maybe alpha
w1 gets rewritten by the Given to become
  [W] w3 :: Foo alpha ~ Maybe e
Now, the only way to make progress is to allow Wanteds to rewrite Wanteds.
Rewriting w3 with w2 gives us
  [W] w4 :: Maybe alpha ~ Maybe e
which will soon get us to alpha := e and thence to victory.

TL;DR we want equality saturation.

We thus want Wanteds to rewrite Wanteds in order to accept more programs,
but we don't want Wanteds to rewrite Wanteds because doing so can create
inscrutable error messages. To solve this dilemma:

* We allow Wanteds to rewrite Wanteds, but each Wanted tracks the set of Wanteds
  it has been rewritten by, in its RewriterSet, stored in the ctev_rewriters
  field of the CtWanted constructor of CtEvidence.  (Only Wanteds have
  RewriterSets.)

* A RewriterSet is just a set of unfilled CoercionHoles. This is sufficient
  because only equalities (evidenced by coercion holes) are used for rewriting;
  other (dictionary) constraints cannot ever rewrite.

* The rewriter (in e.g. GHC.Tc.Solver.Rewrite.rewrite) tracks and returns a RewriterSet,
  consisting of the evidence (a CoercionHole) for any Wanted equalities used in
  rewriting.

* Then GHC.Tc.Solver.Solve.rewriteEvidence and GHC.Tc.Solver.Equality.rewriteEqEvidence
  add this RewriterSet to the rewritten constraint's rewriter set.

* We prevent the unifier from unifying any equality with a non-empty rewriter set;
  unification effectively turns a Wanted into a Given, and we lose all tracking.
  See (REWRITERS) in Note [Unification preconditions] in GHC.Tc.Utils.Unify and
  Note [Unify only if the rewriter set is empty] in GHC.Solver.Equality.

* In error reporting, we simply suppress any errors that have been rewritten
  by /unsolved/ wanteds. This suppression happens in GHC.Tc.Errors.mkErrorItem,
  which uses `GHC.Tc.Zonk.Type.zonkRewriterSet` to look through any filled
  coercion holes. The idea is that we wish to report the "root cause" -- the
  error that rewrote all the others.

* In `selectNextWorkItem`, priorities equalities with no rewiters.  See
  Note [Prioritise Wanteds with empty RewriterSet] in GHC.Tc.Types.Constraint
  wrinkle (PER1).

* In error reporting, we prioritise Wanteds that have an empty RewriterSet:
  see Note [Prioritise Wanteds with empty RewriterSet].

Let's continue our first example above:

  inert: [W] w1 :: a ~ Char
  work:  [W] w2 :: a ~ Bool

Because Wanteds can rewrite Wanteds, w1 will rewrite w2, yielding

  inert: [W] w1 :: a ~ Char
         [W] w2 {w1}:: Char ~ Bool

The {w1} in the second line of output is the RewriterSet of w1.

Wrinkles:

(WRW1) When we find a constraint identical to one already in the inert set,
   we solve one from the other. Other things being equal, keep the one
   that has fewer (better still no) rewriters.
   See (CE4) in Note [Combining equalities] in GHC.Tc.Solver.Equality.

   To this accurately we should use `zonkRewriterSet` during canonicalisation,
   to eliminate rewriters that have now been solved.  Currently we only do so
   during error reporting; but perhaps we should change that.

(WRW2) When zonking a constraint (with `zonkCt` and `zonkCtEvidence`) we take
   the opportunity to zonk its `RewriterSet`, which eliminates solved ones.
   This doesn't guarantee that rewriter sets are always up to date -- see
   (WRW1) -- but it helps, and it de-clutters debug output.

Note [Prioritise Wanteds with empty RewriterSet]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When extending the WorkList, in GHC.Tc.Solver.InertSet.extendWorkListEq,
we prioritise constraints that have no rewriters. Here's why.

Consider this, which came up in T22793:
  inert: {}
  work list: [W] co_ayf : awq ~ awo
  work item: [W] co_ayb : awq ~ awp

  ==> {just put work item in inert set}
  inert: co_ayb : awq ~ awp
  work list: {}
  work: [W] co_ayf : awq ~ awo

  ==> {rewrite ayf with co_ayb}
  work list: {}
  inert: co_ayb : awq ~ awp
         co_aym{co_ayb} : awp ~ awo
                ^ rewritten by ayb

  ----- start again in simplify_loop in Solver.hs -----
  inert: {}
  work list: [W] co_ayb : awq ~ awp
  work: co_aym{co_ayb} : awp ~ awo

  ==> {add to inert set}
  inert: co_aym{co_ayb} : awp ~ awo
  work list: {}
  work: co_ayb : awq ~ awp

  ==> {rewrite co_ayb}
  inert: co_aym{co_ayb} : awp ~ awo
         co_ayp{co_aym} : awq ~ awo
  work list: {}

Now both wanteds have been rewriten by the other! This happened because
in our simplify_loop iteration, we happened to start with co_aym. All would have
been well if we'd started with the (not-rewritten) co_ayb and gotten it into the
inert set.

With that in mind, we /prioritise/ the work-list to put
constraints with no rewriters first.  This prioritisation
is done in `GHC.Tc.Solver.Monad.selectNextWorkItem`.

Wrinkles

(PER1) When picking the next work item, before checking for an empty RewriterSet
  in GHC.Tc.Solver.Monad.selectNextWorkItem, we zonk the RewriterSet, because
  some of those CoercionHoles may have been filled in since we last looked.

(PER2) Despite the prioritisation, it is hard to be /certain/ that we can't end up
  in a situation where all of the Wanteds have rewritten each other. In
  order to report /some/ error in this case, we simply report all the
  Wanteds. The user will get a perhaps-confusing error message, but they've
  written a confusing program!  (T22707 and T22793 were close, but they do
  not exhibit this behaviour.)  So belt and braces: see the `suppress`
  stuff in GHC.Tc.Errors.mkErrorItem.

Note [Avoiding rewriting cycles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [inert_eqs: the inert equalities] in GHC.Tc.Solver.InertSet describes
the can-rewrite relation among CtFlavour/Role pairs, saying which constraints
can rewrite which other constraints. It puts forth (R2):
  (R2) If f1 >= f, and f2 >= f,
       then either f1 >= f2 or f2 >= f1
The naive can-rewrite relation says that (Given, Representational) can rewrite
(Wanted, Representational) and that (Wanted, Nominal) can rewrite
(Wanted, Representational), but neither of (Given, Representational) and
(Wanted, Nominal) can rewrite the other. This would violate (R2). See also
Note [Why R2?] in GHC.Tc.Solver.InertSet.

To keep R2, we do not allow (Wanted, Nominal) to rewrite (Wanted, Representational).
This can, in theory, bite, in this scenario:

  type family F a
  data T a
  type role T nominal

  [G] F a ~N T a
  [W] F alpha ~N T alpha
  [W] F alpha ~R T a

As written, this makes no progress, and GHC errors. But, if we
allowed W/N to rewrite W/R, the first W could rewrite the second:

  [G] F a ~N T a
  [W] F alpha ~N T alpha
  [W] T alpha ~R T a

Now we decompose the second W to get

  [W] alpha ~N a

noting the role annotation on T. This causes (alpha := a), and then
everything else unlocks.

What to do? We could "decompose" nominal equalities into nominal-only
("NO") equalities and representational ones, where a NO equality rewrites
only nominals. That is, when considering whether [W] F alpha ~N T alpha
should rewrite [W] F alpha ~R T a, we could require splitting the first W
into [W] F alpha ~NO T alpha, [W] F alpha ~R T alpha. Then, we use the R
half of the split to rewrite the second W, and off we go. This splitting
would allow the split-off R equality to be rewritten by other equalities,
thus avoiding the problem in Note [Why R2?] in GHC.Tc.Solver.InertSet.

However, note that I said that this bites in theory. That's because no
known program actually gives rise to this scenario. A direct encoding
ends up starting with

  [G] F a ~ T a
  [W] F alpha ~ T alpha
  [W] Coercible (F alpha) (T a)

where ~ and Coercible denote lifted class constraints. The ~s quickly
reduce to ~N: good. But the Coercible constraint gets rewritten to

  [W] Coercible (T alpha) (T a)

by the first Wanted. This is because Coercible is a class, and arguments
in class constraints use *nominal* rewriting, not the representational
rewriting that is restricted due to (R2). Note that reordering the code
doesn't help, because equalities (including lifted ones) are prioritized
over Coercible. Thus, I (Richard E.) see no way to write a program that
is rejected because of this infelicity. I have not proved it impossible,
exactly, but my usual tricks have not yielded results.

In the olden days, when we had Derived constraints, this Note was all
about G/R and D/N both rewriting D/R. Back then, the code in
typecheck/should_compile/T19665 really did get rejected. But now,
according to the rewriting of the Coercible constraint, the program
is accepted.

-}

eqCanRewrite :: EqRel -> EqRel -> Bool
eqCanRewrite NomEq  _      = True
eqCanRewrite ReprEq ReprEq = True
eqCanRewrite ReprEq NomEq  = False

eqCanRewriteFR :: CtFlavourRole -> CtFlavourRole -> Bool
-- Can fr1 actually rewrite fr2?
-- Very important function!
-- See Note [eqCanRewrite]
-- See Note [Wanteds rewrite Wanteds]
-- See Note [Avoiding rewriting cycles]
eqCanRewriteFR (Given,  r1)    (_,      r2)     = eqCanRewrite r1 r2
eqCanRewriteFR (Wanted, NomEq) (Wanted, ReprEq) = False
eqCanRewriteFR (Wanted, r1)    (Wanted, r2)     = eqCanRewrite r1 r2
eqCanRewriteFR (Wanted, _)     (Given, _)       = False
