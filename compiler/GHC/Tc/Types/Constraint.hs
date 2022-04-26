
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | This module defines types and simple operations over constraints, as used
-- in the type-checker and constraint solver.
module GHC.Tc.Types.Constraint (
        -- QCInst
        QCInst(..), isPendingScInst,

        -- Canonical constraints
        Xi, Ct(..), Cts,
        emptyCts, andCts, andManyCts, pprCts,
        singleCt, listToCts, ctsElts, consCts, snocCts, extendCtsList,
        isEmptyCts,
        isPendingScDict, superClassesMightHelp, getPendingWantedScs,
        isWantedCt, isGivenCt,
        isUserTypeError, getUserTypeErrorMsg,
        ctEvidence, ctLoc, ctPred, ctFlavour, ctEqRel, ctOrigin,
        ctRewriters,
        ctEvId, wantedEvId_maybe, mkTcEqPredLikeEv,
        mkNonCanonical, mkNonCanonicalCt, mkGivens,
        mkIrredCt,
        ctEvPred, ctEvLoc, ctEvOrigin, ctEvEqRel,
        ctEvExpr, ctEvTerm, ctEvCoercion, ctEvEvId,
        ctEvRewriters,
        tyCoVarsOfCt, tyCoVarsOfCts,
        tyCoVarsOfCtList, tyCoVarsOfCtsList,

        CtIrredReason(..), isInsolubleReason,

        CheckTyEqResult, CheckTyEqProblem, cteProblem, cterClearOccursCheck,
        cteOK, cteImpredicative, cteTypeFamily,
        cteInsolubleOccurs, cteSolubleOccurs, cterSetOccursCheckSoluble,

        cterHasNoProblem, cterHasProblem, cterHasOnlyProblem,
        cterRemoveProblem, cterHasOccursCheck, cterFromKind,

        CanEqLHS(..), canEqLHS_maybe, canEqLHSKind, canEqLHSType,
        eqCanEqLHS,

        Hole(..), HoleSort(..), isOutOfScopeHole,
        DelayedError(..), NotConcreteError(..),
        NotConcreteReason(..),

        WantedConstraints(..), insolubleWC, emptyWC, isEmptyWC,
        isSolvedWC, andWC, unionsWC, mkSimpleWC, mkImplicWC,
        addInsols, dropMisleading, addSimples, addImplics, addHoles,
        addNotConcreteError, addDelayedErrors,
        tyCoVarsOfWC,
        tyCoVarsOfWCList, insolubleWantedCt, insolubleEqCt, insolubleCt,
        insolubleImplic, nonDefaultableTyVarsOfWC,

        Implication(..), implicationPrototype, checkTelescopeSkol,
        ImplicStatus(..), isInsolubleStatus, isSolvedStatus,
        UserGiven, getUserGivensFromImplics,
        HasGivenEqs(..), checkImplicationInvariants,
        SubGoalDepth, initialSubGoalDepth, maxSubGoalDepth,
        bumpSubGoalDepth, subGoalDepthExceeded,
        CtLoc(..), ctLocSpan, ctLocEnv, ctLocLevel, ctLocOrigin,
        ctLocTypeOrKind_maybe,
        ctLocDepth, bumpCtLocDepth, isGivenLoc,
        setCtLocOrigin, updateCtLocOrigin, setCtLocEnv, setCtLocSpan,
        pprCtLoc,

        -- CtEvidence
        CtEvidence(..), TcEvDest(..),
        mkKindLoc, toKindLoc, mkGivenLoc,
        isWanted, isGiven,
        ctEvRole, setCtEvPredType, setCtEvLoc, arisesFromGivens,
        tyCoVarsOfCtEvList, tyCoVarsOfCtEv, tyCoVarsOfCtEvsList,
        ctEvUnique, tcEvDestUnique,

        RewriterSet(..), emptyRewriterSet, isEmptyRewriterSet,
           -- exported concretely only for anyUnfilledCoercionHoles
        rewriterSetFromType, rewriterSetFromTypes, rewriterSetFromCo,
        addRewriterSet,

        wrapType,

        CtFlavour(..), ctEvFlavour,
        CtFlavourRole, ctEvFlavourRole, ctFlavourRole,
        eqCanRewrite, eqCanRewriteFR,

        -- Pretty printing
        pprEvVarTheta,
        pprEvVars, pprEvVarWithType,

  )
  where

import GHC.Prelude

import {-# SOURCE #-} GHC.Tc.Types ( TcLclEnv, setLclEnvTcLevel, getLclEnvTcLevel
                                   , setLclEnvLoc, getLclEnvLoc )

import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Types.Name
import GHC.Types.Var

import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin

import GHC.Core

import GHC.Core.TyCo.Ppr
import GHC.Utils.FV
import GHC.Types.Var.Set
import GHC.Driver.Session
import GHC.Types.Basic
import GHC.Types.Unique
import GHC.Types.Unique.Set

import GHC.Utils.Outputable
import GHC.Types.SrcLoc
import GHC.Data.Bag
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)

import Data.Coerce
import Data.Monoid ( Endo(..) )
import qualified Data.Semigroup as S
import Control.Monad ( msum, when )
import Data.Maybe ( mapMaybe )
import Data.List.NonEmpty ( NonEmpty )

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

Note [CEqCan occurs check]
~~~~~~~~~~~~~~~~~~~~~~~~~~
A CEqCan relates a CanEqLHS (a type variable or type family applications) on
its left to an arbitrary type on its right. It is used for rewriting.
Because it is used for rewriting, it would be disastrous if the RHS
were to mention the LHS: this would cause a loop in rewriting.

We thus perform an occurs-check. There is, of course, some subtlety:

* For type variables, the occurs-check looks deeply. This is because
  a CEqCan over a meta-variable is also used to inform unification,
  in GHC.Tc.Solver.Interact.solveByUnification. If the LHS appears
  anywhere, at all, in the RHS, unification will create an infinite
  structure, which is bad.

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

The occurs check is performed in GHC.Tc.Utils.Unify.checkTypeEq
and forms condition T3 in Note [Extending the inert equalities]
in GHC.Tc.Solver.InertSet.

-}

-- | A 'Xi'-type is one that has been fully rewritten with respect
-- to the inert set; that is, it has been rewritten by the algorithm
-- in GHC.Tc.Solver.Rewrite. (Historical note: 'Xi', for years and years,
-- meant that a type was type-family-free. It does *not* mean this
-- any more.)
type Xi = TcType

type Cts = Bag Ct

data Ct
  -- Atomic canonical constraints
  = CDictCan {  -- e.g.  Num ty
      cc_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]

      cc_class  :: Class,
      cc_tyargs :: [Xi],   -- cc_tyargs are rewritten w.r.t. inerts, so Xi

      cc_pend_sc :: Bool,
          -- See Note [The superclass story] in GHC.Tc.Solver.Canonical
          -- True <=> (a) cc_class has superclasses
          --          (b) we have not (yet) added those
          --              superclasses as Givens

      cc_fundeps :: Bool
          -- See Note [Fundeps with instances] in GHC.Tc.Solver.Interact
          -- True <=> the class has fundeps, and we have not yet
          --          compared this constraint with the global
          --          instances for fundep improvement
    }

  | CIrredCan {  -- These stand for yet-unusable predicates
      cc_ev     :: CtEvidence,   -- See Note [Ct/evidence invariant]
      cc_reason :: CtIrredReason

        -- For the might-be-soluble case, the ctev_pred of the evidence is
        -- of form   (tv xi1 xi2 ... xin)   with a tyvar at the head
        --      or   (lhs1 ~ ty2)  where the CEqCan    kind invariant (TyEq:K) fails
        -- See Note [CIrredCan constraints]

        -- The definitely-insoluble case is for things like
        --    Int ~ Bool      tycons don't match
        --    a ~ [a]         occurs check
    }

  | CEqCan {  -- CanEqLHS ~ rhs
       -- Invariants:
       --   * See Note [inert_eqs: the inert equalities] in GHC.Tc.Solver.InertSet
       --   * Many are checked in checkTypeEq in GHC.Tc.Utils.Unify
       --   * (TyEq:OC) lhs does not occur in rhs (occurs check)
       --               Note [CEqCan occurs check]
       --   * (TyEq:F) rhs has no foralls
       --       (this avoids substituting a forall for the tyvar in other types)
       --   * (TyEq:K) tcTypeKind lhs `tcEqKind` tcTypeKind rhs; Note [Ct kind invariant]
       --   * (TyEq:N) If the equality is representational, rhs has no top-level newtype
       --     See Note [No top-level newtypes on RHS of representational equalities]
       --     in GHC.Tc.Solver.Canonical. (Applies only when constructor of newtype is
       --     in scope.)
       --   * (TyEq:TV) If rhs (perhaps under a cast) is also CanEqLHS, then it is oriented
       --     to give best chance of
       --     unification happening; eg if rhs is touchable then lhs is too
       --     Note [TyVar/TyVar orientation] in GHC.Tc.Utils.Unify
      cc_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]
      cc_lhs    :: CanEqLHS,
      cc_rhs    :: Xi,         -- See invariants above

      cc_eq_rel :: EqRel       -- INVARIANT: cc_eq_rel = ctEvEqRel cc_ev
    }

  | CNonCanonical {        -- See Note [NonCanonical Semantics] in GHC.Tc.Solver.Monad
      cc_ev  :: CtEvidence
    }

  | CQuantCan QCInst       -- A quantified constraint
      -- NB: I expect to make more of the cases in Ct
      --     look like this, with the payload in an
      --     auxiliary type

------------
-- | A 'CanEqLHS' is a type that can appear on the left of a canonical
-- equality: a type variable or exactly-saturated type family application.
data CanEqLHS
  = TyVarLHS TcTyVar
  | TyFamLHS TyCon  -- ^ of the family
             [Xi]   -- ^ exactly saturating the family

instance Outputable CanEqLHS where
  ppr (TyVarLHS tv)              = ppr tv
  ppr (TyFamLHS fam_tc fam_args) = ppr (mkTyConApp fam_tc fam_args)

------------
data QCInst  -- A much simplified version of ClsInst
             -- See Note [Quantified constraints] in GHC.Tc.Solver.Canonical
  = QCI { qci_ev   :: CtEvidence -- Always of type forall tvs. context => ty
                                 -- Always Given
        , qci_tvs  :: [TcTyVar]  -- The tvs
        , qci_pred :: TcPredType -- The ty
        , qci_pend_sc :: Bool    -- Same as cc_pend_sc flag in CDictCan
                                 -- Invariant: True => qci_pred is a ClassPred
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
    -- See Note [Holes].
  | DE_NotConcrete NotConcreteError
    -- ^ A type could not be ensured to be concrete.
    --
    -- See Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete.

instance Outputable DelayedError where
  ppr (DE_Hole hole) = ppr hole
  ppr (DE_NotConcrete err) = ppr err

-- | A hole stores the information needed to report diagnostics
-- about holes in terms (unbound identifiers or underscores) or
-- in types (also called wildcards, as used in partial type
-- signatures). See Note [Holes].
data Hole
  = Hole { hole_sort :: HoleSort -- ^ What flavour of hole is this?
         , hole_occ  :: OccName  -- ^ The name of this hole
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
    , nce_reasons    :: NonEmpty NotConcreteReason
      -- ^ Why did the check fail?
    }

-- | Why did we decide that a type was not concrete?
data NotConcreteReason
  -- | The type contains a 'TyConApp' of a non-concrete 'TyCon'.
  --
  -- See Note [Concrete types] in GHC.Tc.Utils.Concrete.
  = NonConcreteTyCon TyCon [TcType]

  -- | The type contains a type variable that could not be made
  -- concrete (e.g. a skolem type variable).
  | NonConcretisableTyVar TyVar

  -- | The type contains a cast.
  | ContainsCast TcType TcCoercionN

  -- | The type contains a forall.
  | ContainsForall TyCoVarBinder TcType

  -- | The type contains a 'CoercionTy'.
  | ContainsCoercionTy TcCoercion

instance Outputable NotConcreteError where
  ppr (NCE_FRR { nce_frr_origin = frr_orig })
    = text "NCE_FRR" <+> parens (ppr (frr_type frr_orig))

------------
-- | Used to indicate extra information about why a CIrredCan is irreducible
data CtIrredReason
  = IrredShapeReason
      -- ^ this constraint has a non-canonical shape (e.g. @c Int@, for a variable @c@)

  | NonCanonicalReason CheckTyEqResult
   -- ^ an equality where some invariant other than (TyEq:H) of 'CEqCan' is not satisfied;
   -- the 'CheckTyEqResult' states exactly why

  | ReprEqReason
    -- ^ an equality that cannot be decomposed because it is representational.
    -- Example: @a b ~R# Int@.
    -- These might still be solved later.
    -- INVARIANT: The constraint is a representational equality constraint

  | ShapeMismatchReason
    -- ^ a nominal equality that relates two wholly different types,
    -- like @Int ~# Bool@ or @a b ~# 3@.
    -- INVARIANT: The constraint is a nominal equality constraint

  | AbstractTyConReason
    -- ^ an equality like @T a b c ~ Q d e@ where either @T@ or @Q@
    -- is an abstract type constructor. See Note [Skolem abstract data]
    -- in GHC.Core.TyCon.
    -- INVARIANT: The constraint is an equality constraint between two TyConApps

instance Outputable CtIrredReason where
  ppr IrredShapeReason          = text "(irred)"
  ppr (NonCanonicalReason cter) = ppr cter
  ppr ReprEqReason              = text "(repr)"
  ppr ShapeMismatchReason       = text "(shape)"
  ppr AbstractTyConReason       = text "(abstc)"

-- | Are we sure that more solving will never solve this constraint?
isInsolubleReason :: CtIrredReason -> Bool
isInsolubleReason IrredShapeReason          = False
isInsolubleReason (NonCanonicalReason cter) = cterIsInsoluble cter
isInsolubleReason ReprEqReason              = False
isInsolubleReason ShapeMismatchReason       = True
isInsolubleReason AbstractTyConReason       = True

------------------------------------------------------------------------------
--
-- CheckTyEqResult, defined here because it is stored in a CtIrredReason
--
------------------------------------------------------------------------------

-- | A set of problems in checking the validity of a type equality.
-- See 'checkTypeEq'.
newtype CheckTyEqResult = CTER Word8

-- | No problems in checking the validity of a type equality.
cteOK :: CheckTyEqResult
cteOK = CTER zeroBits

-- | Check whether a 'CheckTyEqResult' is marked successful.
cterHasNoProblem :: CheckTyEqResult -> Bool
cterHasNoProblem (CTER 0) = True
cterHasNoProblem _        = False

-- | An individual problem that might be logged in a 'CheckTyEqResult'
newtype CheckTyEqProblem = CTEP Word8

cteImpredicative, cteTypeFamily, cteInsolubleOccurs, cteSolubleOccurs :: CheckTyEqProblem
cteImpredicative   = CTEP (bit 0)   -- forall or (=>) encountered
cteTypeFamily      = CTEP (bit 1)   -- type family encountered
cteInsolubleOccurs = CTEP (bit 2)   -- occurs-check
cteSolubleOccurs   = CTEP (bit 3)   -- occurs-check under a type function or in a coercion
                                    -- must be one bit to the left of cteInsolubleOccurs
-- See also Note [Insoluble occurs check] in GHC.Tc.Errors

cteProblem :: CheckTyEqProblem -> CheckTyEqResult
cteProblem (CTEP mask) = CTER mask

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

instance Outputable CheckTyEqResult where
  ppr cter | cterHasNoProblem cter = text "cteOK"
           | otherwise
           = parens $ fcat $ intersperse vbar $ set_bits
    where
      all_bits = [ (cteImpredicative,   "cteImpredicative")
                 , (cteTypeFamily,      "cteTypeFamily")
                 , (cteInsolubleOccurs, "cteInsolubleOccurs")
                 , (cteSolubleOccurs,   "cteSolubleOccurs") ]
      set_bits = [ text str
                 | (bitmask, str) <- all_bits
                 , cter `cterHasProblem` bitmask ]

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
of (cc_ev ct), and is fully rewritten wrt the substitution.   Eg for CDictCan,
   ctev_pred (cc_ev ct) = (cc_class ct) (cc_tyargs ct)
This holds by construction; look at the unique place where CDictCan is
built (in GHC.Tc.Solver.Canonical).

Note [Ct kind invariant]
~~~~~~~~~~~~~~~~~~~~~~~~
CEqCan requires that the kind of the lhs matches the kind
of the rhs. This is necessary because these constraints are used for substitutions
during solving. If the kinds differed, then the substitution would take a well-kinded
type to an ill-kinded one.

Note [Holes]
~~~~~~~~~~~~
This Note explains how GHC tracks *holes*.

A hole represents one of two conditions:
 - A missing bit of an expression. Example: foo x = x + _
 - A missing bit of a type. Example: bar :: Int -> _

What these have in common is that both cause GHC to emit a diagnostic to the
user describing the bit that is left out.

When a hole is encountered, a new entry of type Hole is added to the ambient
WantedConstraints. The type (hole_ty) of the hole is then simplified during
solving (with respect to any Givens in surrounding implications). It is
reported with all the other errors in GHC.Tc.Errors.

For expression holes, the user has the option of deferring errors until runtime
with -fdefer-type-errors. In this case, the hole actually has evidence: this
evidence is an erroring expression that prints an error and crashes at runtime.
The ExprHole variant of holes stores an IORef EvTerm that will contain this evidence;
during constraint generation, this IORef was stored in the HsUnboundVar extension
field by the type checker. The desugarer simply dereferences to get the CoreExpr.

Prior to fixing #17812, we used to invent an Id to hold the erroring
expression, and then bind it during type-checking. But this does not support
representation-polymorphic out-of-scope identifiers. See
typecheck/should_compile/T17812. We thus use the mutable-CoreExpr approach
described above.

You might think that the type in the HoleExprRef is the same as the type of the
hole. However, because the hole type (hole_ty) is rewritten with respect to
givens, this might not be the case. That is, the hole_ty is always (~) to the
type of the HoleExprRef, but they might not be `eqType`. We need the type of the generated
evidence to match what is expected in the context of the hole, and so we must
store these types separately.

Type-level holes have no evidence at all.
-}

mkNonCanonical :: CtEvidence -> Ct
mkNonCanonical ev = CNonCanonical { cc_ev = ev }

mkNonCanonicalCt :: Ct -> Ct
mkNonCanonicalCt ct = CNonCanonical { cc_ev = cc_ev ct }

mkIrredCt :: CtIrredReason -> CtEvidence -> Ct
mkIrredCt reason ev = CIrredCan { cc_ev = ev, cc_reason = reason }

mkGivens :: CtLoc -> [EvId] -> [Ct]
mkGivens loc ev_ids
  = map mk ev_ids
  where
    mk ev_id = mkNonCanonical (CtGiven { ctev_evar = ev_id
                                       , ctev_pred = evVarPred ev_id
                                       , ctev_loc = loc })

ctEvidence :: Ct -> CtEvidence
ctEvidence (CQuantCan (QCI { qci_ev = ev })) = ev
ctEvidence ct = cc_ev ct

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
      NomEq  -> mkPrimEqPred
      ReprEq -> mkReprPrimEqPred
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
         CDictCan { cc_pend_sc = psc, cc_fundeps = fds }
            | psc, fds     -> text "CDictCan(psc,fds)"
            | psc, not fds -> text "CDictCan(psc)"
            | not psc, fds -> text "CDictCan(fds)"
            | otherwise    -> text "CDictCan"
         CIrredCan { cc_reason = reason } -> text "CIrredCan" <> ppr reason
         CQuantCan (QCI { qci_pend_sc = pend_sc })
            | pend_sc   -> text "CQuantCan(psc)"
            | otherwise -> text "CQuantCan"

-----------------------------------
-- | Is a type a canonical LHS? That is, is it a tyvar or an exactly-saturated
-- type family application?
-- Does not look through type synonyms.
canEqLHS_maybe :: Xi -> Maybe CanEqLHS
canEqLHS_maybe xi
  | Just tv <- tcGetTyVar_maybe xi
  = Just $ TyVarLHS tv

  | Just (tc, args) <- tcSplitTyConApp_maybe xi
  , isTypeFamilyTyCon tc
  , args `lengthIs` tyConArity tc
  = Just $ TyFamLHS tc args

  | otherwise
  = Nothing

-- | Convert a 'CanEqLHS' back into a 'Type'
canEqLHSType :: CanEqLHS -> TcType
canEqLHSType (TyVarLHS tv) = mkTyVarTy tv
canEqLHSType (TyFamLHS fam_tc fam_args) = mkTyConApp fam_tc fam_args

-- | Retrieve the kind of a 'CanEqLHS'
canEqLHSKind :: CanEqLHS -> TcKind
canEqLHSKind (TyVarLHS tv) = tyVarKind tv
canEqLHSKind (TyFamLHS fam_tc fam_args) = piResultTys (tyConKind fam_tc) fam_args

-- | Are two 'CanEqLHS's equal?
eqCanEqLHS :: CanEqLHS -> CanEqLHS -> Bool
eqCanEqLHS (TyVarLHS tv1) (TyVarLHS tv2) = tv1 == tv2
eqCanEqLHS (TyFamLHS fam_tc1 fam_args1) (TyFamLHS fam_tc2 fam_args2)
  = tcEqTyConApps fam_tc1 fam_args1 fam_tc2 fam_args2
eqCanEqLHS _ _ = False

{-
************************************************************************
*                                                                      *
        Simple functions over evidence variables
*                                                                      *
************************************************************************
-}

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

tyCoFVsOfHole :: Hole -> FV
tyCoFVsOfHole (Hole { hole_ty = ty }) = tyCoFVsOfType ty

tyCoFVsOfBag :: (a -> FV) -> Bag a -> FV
tyCoFVsOfBag tvs_of = foldr (unionFV . tvs_of) emptyFV

isGivenLoc :: CtLoc -> Bool
isGivenLoc loc = isGivenOrigin (ctLocOrigin loc)

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
getUserTypeErrorMsg :: PredType -> Maybe Type
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

isUserTypeError :: PredType -> Bool
isUserTypeError pred = case getUserTypeErrorMsg pred of
                             Just _ -> True
                             _      -> False

isPendingScDict :: Ct -> Maybe Ct
-- Says whether this is a CDictCan with cc_pend_sc is True,
-- AND if so flips the flag
isPendingScDict ct@(CDictCan { cc_pend_sc = True })
                  = Just (ct { cc_pend_sc = False })
isPendingScDict _ = Nothing

isPendingScInst :: QCInst -> Maybe QCInst
-- Same as isPendingScDict, but for QCInsts
isPendingScInst qci@(QCI { qci_pend_sc = True })
                  = Just (qci { qci_pend_sc = False })
isPendingScInst _ = Nothing

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

    is_ip (CDictCan { cc_class = cls }) = isIPClass cls
    is_ip _                             = False

getPendingWantedScs :: Cts -> ([Ct], Cts)
getPendingWantedScs simples
  = mapAccumBagL get [] simples
  where
    get acc ct | Just ct' <- isPendingScDict ct
               = (ct':acc, ct')
               | otherwise
               = (acc,     ct)

{- Note [When superclasses help]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
First read Note [The superclass story] in GHC.Tc.Solver.Canonical.

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

andManyCts :: [Cts] -> Cts
andManyCts = unionManyBags

emptyCts :: Cts
emptyCts = emptyBag

isEmptyCts :: Cts -> Bool
isEmptyCts = isEmptyBag

pprCts :: Cts -> SDoc
pprCts cts = vcat (map ppr (bagToList cts))

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

addInsols :: WantedConstraints -> Bag Ct -> WantedConstraints
addInsols wc cts
  = wc { wc_simple = wc_simple wc `unionBags` cts }

addHoles :: WantedConstraints -> Bag Hole -> WantedConstraints
addHoles wc holes
  = wc { wc_errors = mapBag DE_Hole holes `unionBags` wc_errors wc }

addNotConcreteError :: WantedConstraints -> NotConcreteError -> WantedConstraints
addNotConcreteError wc err
  = wc { wc_errors = unitBag (DE_NotConcrete err) `unionBags` wc_errors wc }

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

    keep_ct ct = case classifyPredType (ctPred ct) of
                    ClassPred {} -> False
                    _ -> True

    keep_delayed_error (DE_Hole hole) = isOutOfScopeHole hole
    keep_delayed_error (DE_NotConcrete {}) = True

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
            , not (isConcrete rhs)
            -> unitVarSet tv
            | Just tv <- getTyVar_maybe rhs
            , isConcreteTyVar tv
            , not (isConcrete lhs)
            -> unitVarSet tv
          _ -> emptyVarSet

      -- Make sure to apply the same logic as above to delayed errors.
      non_defaultable_tvs_of_err (DE_NotConcrete err)
        = case err of
            NCE_FRR { nce_frr_origin = frr } -> tyCoVarsOfType (frr_type frr)
      non_defaultable_tvs_of_err (DE_Hole {}) = emptyVarSet

insolubleWC :: WantedConstraints -> Bool
insolubleWC (WC { wc_impl = implics, wc_simple = simples, wc_errors = errors })
  =  anyBag insolubleWantedCt simples
  || anyBag insolubleImplic implics
  || anyBag is_insoluble errors

    where
      is_insoluble (DE_Hole hole) = isOutOfScopeHole hole -- See Note [Insoluble holes]
      is_insoluble (DE_NotConcrete {}) = True

insolubleWantedCt :: Ct -> Bool
-- Definitely insoluble, in particular /excluding/ type-hole constraints
-- Namely:
--   a) an insoluble constraint as per 'insolubleCt', i.e. either
--        - an insoluble equality constraint (e.g. Int ~ Bool), or
--        - a custom type error constraint, TypeError msg :: Constraint
--   b) that does not arise from a Given or a Wanted/Wanted fundep interaction
--
-- See Note [Given insolubles].
insolubleWantedCt ct = insolubleCt ct &&
                       not (arisesFromGivens ct) &&
                       not (isWantedWantedFunDepOrigin (ctOrigin ct))

insolubleEqCt :: Ct -> Bool
-- Returns True of /equality/ constraints
-- that are /definitely/ insoluble
-- It won't detect some definite errors like
--       F a ~ T (F a)
-- where F is a type family, which actually has an occurs check
--
-- The function is tuned for application /after/ constraint solving
--       i.e. assuming canonicalisation has been done
-- E.g.  It'll reply True  for     a ~ [a]
--               but False for   [a] ~ a
-- and
--                   True for  Int ~ F a Int
--               but False for  Maybe Int ~ F a Int Int
--               (where F is an arity-1 type function)
insolubleEqCt (CIrredCan { cc_reason = reason }) = isInsolubleReason reason
insolubleEqCt _                                  = False

-- | Returns True of equality constraints that are definitely insoluble,
-- as well as TypeError constraints.
-- Can return 'True' for Given constraints, unlike 'insolubleWantedCt'.
--
-- This function is critical for accurate pattern-match overlap warnings.
-- See Note [Pattern match warnings with insoluble Givens] in GHC.Tc.Solver
--
-- Note that this does not traverse through the constraint to find
-- nested custom type errors: it only detects @TypeError msg :: Constraint@,
-- and not e.g. @Eq (TypeError msg)@.
insolubleCt :: Ct -> Bool
insolubleCt ct
  | Just _ <- userTypeError_maybe (ctPred ct)
  -- Don't use 'isUserTypeErrorCt' here, as that function is too eager:
  -- the TypeError might appear inside a type family application
  -- which might later reduce, but we only want to return 'True'
  -- for constraints that are definitely insoluble.
  --
  -- Test case: T11503, with the 'Assert' type family:
  --
  -- > type Assert :: Bool -> Constraint -> Constraint
  -- > type family Assert check errMsg where
  -- >   Assert 'True  _errMsg = ()
  -- >   Assert _check errMsg  = errMsg
  = True
  | otherwise
  = insolubleEqCt ct

-- | Does this hole represent an "out of scope" error?
-- See Note [Insoluble holes]
isOutOfScopeHole :: Hole -> Bool
isOutOfScopeHole (Hole { hole_occ = occ }) = not (startsWithUnderscore occ)

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
                                 -- True  <=> -Winaccessible-code is enabled
                                 -- at construction. See
                                 -- Note [Avoid -Winaccessible-code when deriving]
                                 -- in GHC.Tc.TyCl.Instance

      ic_env   :: TcLclEnv,
                                 -- Records the TcLClEnv at the time of creation.
                                 --
                                 -- The TcLclEnv gives the source location
                                 -- and error context for the implication, and
                                 -- hence for all the given evidence variables.

      ic_wanted :: WantedConstraints,  -- The wanteds
                                       -- See Invariant (WantedInf) in GHC.Tc.Utils.TcType

      ic_binds  :: EvBindsVar,    -- Points to the place to fill in the
                                  -- abstraction and bindings.

      -- The ic_need fields keep track of which Given evidence
      -- is used by this implication or its children
      -- NB: including stuff used by nested implications that have since
      --     been discarded
      -- See Note [Needed evidence variables]
      ic_need_inner :: VarSet,    -- Includes all used Given evidence
      ic_need_outer :: VarSet,    -- Includes only the free Given evidence
                                  --  i.e. ic_need_inner after deleting
                                  --       (a) givens (b) binders of ic_binds

      ic_status   :: ImplicStatus
    }

implicationPrototype :: Implication
implicationPrototype
   = Implic { -- These fields must be initialised
              ic_tclvl      = panic "newImplic:tclvl"
            , ic_binds      = panic "newImplic:binds"
            , ic_info       = panic "newImplic:info"
            , ic_env        = panic "newImplic:env"
            , ic_warn_inaccessible = panic "newImplic:warn_inaccessible"

              -- The rest have sensible default values
            , ic_skols      = []
            , ic_given      = []
            , ic_wanted     = emptyWC
            , ic_given_eqs  = MaybeGivenEqs
            , ic_status     = IC_Unsolved
            , ic_need_inner = emptyVarSet
            , ic_need_outer = emptyVarSet }

data ImplicStatus
  = IC_Solved     -- All wanteds in the tree are solved, all the way down
       { ics_dead :: [EvVar] }  -- Subset of ic_given that are not needed
         -- See Note [Tracking redundant constraints] in GHC.Tc.Solver

  | IC_Insoluble  -- At least one insoluble constraint in the tree

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
  (and hence do not affect princial types)
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
              , ic_need_inner = need_in, ic_need_outer = need_out
              , ic_info = info })
   = hang (text "Implic" <+> lbrace)
        2 (sep [ text "TcLevel =" <+> ppr tclvl
               , text "Skolems =" <+> pprTyVars skols
               , text "Given-eqs =" <+> ppr given_eqs
               , text "Status =" <+> ppr status
               , hang (text "Given =")  2 (pprEvVars given)
               , hang (text "Wanted =") 2 (ppr wanted)
               , text "Binds =" <+> ppr binds
               , whenPprDebug (text "Needed inner =" <+> ppr need_in)
               , whenPprDebug (text "Needed outer =" <+> ppr need_out)
               , pprSkolInfo info ] <+> rbrace)

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

Note [Needed evidence variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Th ic_need_evs field holds the free vars of ic_binds, and all the
ic_binds in nested implications.

  * Main purpose: if one of the ic_givens is not mentioned in here, it
    is redundant.

  * solveImplication may drop an implication altogether if it has no
    remaining 'wanteds'. But we still track the free vars of its
    evidence binds, even though it has now disappeared.

Note [Shadowing in a constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We assume NO SHADOWING in a constraint.  Specifically
 * The unification variables are all implicitly quantified at top
   level, and are all unique
 * The skolem variables bound in ic_skols are all freah when the
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
      | not (tv_lvl == lvl)
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
    go InstSkol             InstSkol             = True
    go FamInstSkol          FamInstSkol          = True
    go BracketSkol          BracketSkol          = True
    go (RuleSkol n1)        (RuleSkol n2)        = n1==n2
    go (PatSkol c1 _)       (PatSkol c2 _)       = getName c1 == getName c2
       -- Too tedious to compare the HsMatchContexts
    go (InferSkol ids1)     (InferSkol ids2)     = equalLength ids1 ids2 &&
                                                   and (zipWith eq_pr ids1 ids2)
    go (UnifyForAllSkol t1) (UnifyForAllSkol t2) = t1 `tcEqType` t2
    go ReifySkol            ReifySkol            = True
    go QuantCtxtSkol        QuantCtxtSkol        = True
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

    go FamInstSkol          InstSkol             = True
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
with `ctev_pref`, as stated in `Note [CtEvidence invariants]`.

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
  = CtGiven    -- Truly given, not depending on subgoals
      { ctev_pred :: TcPredType      -- See Note [Ct/evidence invariant]
      , ctev_evar :: EvVar           -- See Note [CtEvidence invariants]
      , ctev_loc  :: CtLoc }


  | CtWanted   -- Wanted goal
      { ctev_pred      :: TcPredType     -- See Note [Ct/evidence invariant]
      , ctev_dest      :: TcEvDest       -- See Note [CtEvidence invariants]
      , ctev_loc       :: CtLoc
      , ctev_rewriters :: RewriterSet }  -- See Note [Wanteds rewrite Wanteds]

ctEvPred :: CtEvidence -> TcPredType
-- The predicate of a flavor
ctEvPred = ctev_pred

ctEvLoc :: CtEvidence -> CtLoc
ctEvLoc = ctev_loc

ctEvOrigin :: CtEvidence -> CtOrigin
ctEvOrigin = ctLocOrigin . ctEvLoc

-- | Get the equality relation relevant for a 'CtEvidence'
ctEvEqRel :: CtEvidence -> EqRel
ctEvEqRel = predTypeEqRel . ctEvPred

-- | Get the role relevant for a 'CtEvidence'
ctEvRole :: CtEvidence -> Role
ctEvRole = eqRelRole . ctEvEqRel

ctEvTerm :: CtEvidence -> EvTerm
ctEvTerm ev = EvExpr (ctEvExpr ev)

-- | Extract the set of rewriters from a 'CtEvidence'
-- See Note [Wanteds rewrite Wanteds]
-- If the provided CtEvidence is not for a Wanted, just
-- return an empty set.
ctEvRewriters :: CtEvidence -> RewriterSet
ctEvRewriters (CtWanted { ctev_rewriters = rewriters }) = rewriters
ctEvRewriters _other                                    = emptyRewriterSet

ctEvExpr :: HasDebugCallStack => CtEvidence -> EvExpr
ctEvExpr ev@(CtWanted { ctev_dest = HoleDest _ })
            = Coercion $ ctEvCoercion ev
ctEvExpr ev = evId (ctEvEvId ev)

ctEvCoercion :: HasDebugCallStack => CtEvidence -> TcCoercion
ctEvCoercion (CtGiven { ctev_evar = ev_id })
  = mkTcCoVarCo ev_id
ctEvCoercion (CtWanted { ctev_dest = dest })
  | HoleDest hole <- dest
  = -- ctEvCoercion is only called on type equalities
    -- and they always have HoleDests
    mkHoleCo hole
ctEvCoercion ev
  = pprPanic "ctEvCoercion" (ppr ev)

ctEvEvId :: CtEvidence -> EvVar
ctEvEvId (CtWanted { ctev_dest = EvVarDest ev }) = ev
ctEvEvId (CtWanted { ctev_dest = HoleDest h })   = coHoleCoVar h
ctEvEvId (CtGiven  { ctev_evar = ev })           = ev

ctEvUnique :: CtEvidence -> Unique
ctEvUnique (CtGiven { ctev_evar = ev })    = varUnique ev
ctEvUnique (CtWanted { ctev_dest = dest }) = tcEvDestUnique dest

tcEvDestUnique :: TcEvDest -> Unique
tcEvDestUnique (EvVarDest ev_var) = varUnique ev_var
tcEvDestUnique (HoleDest co_hole) = varUnique (coHoleCoVar co_hole)

setCtEvLoc :: CtEvidence -> CtLoc -> CtEvidence
setCtEvLoc ctev loc = ctev { ctev_loc = loc }

arisesFromGivens :: Ct -> Bool
arisesFromGivens ct = isGivenCt ct || isGivenLoc (ctLoc ct)

-- | Set the type of CtEvidence.
--
-- This function ensures that the invariants on 'CtEvidence' hold, by updating
-- the evidence and the ctev_pred in sync with each other.
-- See Note [CtEvidence invariants].
setCtEvPredType :: HasDebugCallStack => CtEvidence -> Type -> CtEvidence
setCtEvPredType old_ctev new_pred
  = case old_ctev of
    CtGiven { ctev_evar = ev, ctev_loc = loc } ->
      CtGiven { ctev_pred = new_pred
              , ctev_evar = setVarType ev new_pred
              , ctev_loc  = loc
              }
    CtWanted { ctev_dest = dest, ctev_loc = loc, ctev_rewriters = rewriters } ->
      CtWanted { ctev_pred      = new_pred
               , ctev_dest      = new_dest
               , ctev_loc       = loc
               , ctev_rewriters = rewriters
               }
        where
          new_dest = case dest of
            EvVarDest ev -> EvVarDest (setVarType ev new_pred)
            HoleDest h   -> HoleDest  (setCoHoleType h new_pred)

instance Outputable TcEvDest where
  ppr (HoleDest h)   = text "hole" <> ppr h
  ppr (EvVarDest ev) = ppr ev

instance Outputable CtEvidence where
  ppr ev = ppr (ctEvFlavour ev)
           <+> pp_ev <+> braces (ppr (ctl_depth (ctEvLoc ev)) <> pp_rewriters)
                         -- Show the sub-goal depth too
               <> dcolon <+> ppr (ctEvPred ev)
    where
      pp_ev = case ev of
             CtGiven { ctev_evar = v } -> ppr v
             CtWanted {ctev_dest = d } -> ppr d

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

isEmptyRewriterSet :: RewriterSet -> Bool
isEmptyRewriterSet (RewriterSet set) = isEmptyUniqSet set

addRewriterSet :: RewriterSet -> CoercionHole -> RewriterSet
addRewriterSet = coerce (addOneToUniqSet @CoercionHole)

-- | Makes a 'RewriterSet' from all the coercion holes that occur in the
-- given coercion.
rewriterSetFromCo :: Coercion -> RewriterSet
rewriterSetFromCo co = appEndo (rewriter_set_from_co co) emptyRewriterSet

-- | Makes a 'RewriterSet' from all the coercion holes that occur in the
-- given type.
rewriterSetFromType :: Type -> RewriterSet
rewriterSetFromType ty = appEndo (rewriter_set_from_ty ty) emptyRewriterSet

-- | Makes a 'RewriterSet' from all the coercion holes that occur in the
-- given types.
rewriterSetFromTypes :: [Type] -> RewriterSet
rewriterSetFromTypes tys = appEndo (rewriter_set_from_tys tys) emptyRewriterSet

rewriter_set_from_ty :: Type -> Endo RewriterSet
rewriter_set_from_tys :: [Type] -> Endo RewriterSet
rewriter_set_from_co :: Coercion -> Endo RewriterSet
(rewriter_set_from_ty, rewriter_set_from_tys, rewriter_set_from_co, _)
  = foldTyCo folder ()
  where
    folder :: TyCoFolder () (Endo RewriterSet)
    folder = TyCoFolder
               { tcf_view  = noView
               , tcf_tyvar = \ _ tv -> rewriter_set_from_ty (tyVarKind tv)
               , tcf_covar = \ _ cv -> rewriter_set_from_ty (varType cv)
               , tcf_hole  = \ _ hole -> coerce (`addOneToUniqSet` hole) S.<>
                                         rewriter_set_from_ty (varType (coHoleCoVar hole))
               , tcf_tycobinder = \ _ _ _ -> () }

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
ctEvFlavourRole :: CtEvidence -> CtFlavourRole
ctEvFlavourRole ev = (ctEvFlavour ev, ctEvEqRel ev)

-- | Extract the flavour and role from a 'Ct'
ctFlavourRole :: Ct -> CtFlavourRole
-- Uses short-cuts to role for special cases
ctFlavourRole (CDictCan { cc_ev = ev })
  = (ctEvFlavour ev, NomEq)
ctFlavourRole (CEqCan { cc_ev = ev, cc_eq_rel = eq_rel })
  = (ctEvFlavour ev, eq_rel)
ctFlavourRole ct
  = ctEvFlavourRole (ctEvidence ct)

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
inscrutable error messages. We choose to allow the rewriting, but
every Wanted tracks the set of Wanteds it has been rewritten by. This is
called a RewriterSet, stored in the ctev_rewriters field of the CtWanted
constructor of CtEvidence.  (Only Wanteds have RewriterSets.)

Let's continue our first example above:

  inert: [W] w1 :: a ~ Char
  work:  [W] w2 :: a ~ Bool

Because Wanteds can rewrite Wanteds, w1 will rewrite w2, yielding

  inert: [W] w1 :: a ~ Char
         [W] w2 {w1}:: Char ~ Bool

The {w1} in the second line of output is the RewriterSet of w1.

A RewriterSet is just a set of unfilled CoercionHoles. This is
sufficient because only equalities (evidenced by coercion holes) are
used for rewriting; other (dictionary) constraints cannot ever
rewrite. The rewriter (in e.g. GHC.Tc.Solver.Rewrite.rewrite) tracks
and returns a RewriterSet, consisting of the evidence (a CoercionHole)
for any Wanted equalities used in rewriting.  Then rewriteEvidence and
rewriteEqEvidence (in GHC.Tc.Solver.Canonical) add this RewriterSet to
the rewritten constraint's rewriter set.

In error reporting, we simply suppress any errors that have been rewritten by
/unsolved/ wanteds. This suppression happens in GHC.Tc.Errors.mkErrorItem, which
uses GHC.Tc.Utils.anyUnfilledCoercionHoles to look through any filled coercion
holes. The idea is that we wish to report the "root cause" -- the error that
rewrote all the others.

Worry: It seems possible that *all* unsolved wanteds are rewritten by other
unsolved wanteds, so that e.g. w1 has w2 in its rewriter set, and w2 has
w1 in its rewiter set. We are unable to come up with an example of this in
practice, however, and so we believe this case cannot happen.

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

{-
************************************************************************
*                                                                      *
            SubGoalDepth
*                                                                      *
************************************************************************

Note [SubGoalDepth]
~~~~~~~~~~~~~~~~~~~
The 'SubGoalDepth' takes care of stopping the constraint solver from looping.

The counter starts at zero and increases. It includes dictionary constraints,
equality simplification, and type family reduction. (Why combine these? Because
it's actually quite easy to mistake one for another, in sufficiently involved
scenarios, like ConstraintKinds.)

The flag -freduction-depth=n fixes the maximium level.

* The counter includes the depth of type class instance declarations.  Example:
     [W] d{7} : Eq [Int]
  That is d's dictionary-constraint depth is 7.  If we use the instance
     $dfEqList :: Eq a => Eq [a]
  to simplify it, we get
     d{7} = $dfEqList d'{8}
  where d'{8} : Eq Int, and d' has depth 8.

  For civilised (decidable) instance declarations, each increase of
  depth removes a type constructor from the type, so the depth never
  gets big; i.e. is bounded by the structural depth of the type.

* The counter also increments when resolving
equalities involving type functions. Example:
  Assume we have a wanted at depth 7:
    [W] d{7} : F () ~ a
  If there is a type function equation "F () = Int", this would be rewritten to
    [W] d{8} : Int ~ a
  and remembered as having depth 8.

  Again, without UndecidableInstances, this counter is bounded, but without it
  can resolve things ad infinitum. Hence there is a maximum level.

* Lastly, every time an equality is rewritten, the counter increases. Again,
  rewriting an equality constraint normally makes progress, but it's possible
  the "progress" is just the reduction of an infinitely-reducing type family.
  Hence we need to track the rewrites.

When compiling a program requires a greater depth, then GHC recommends turning
off this check entirely by setting -freduction-depth=0. This is because the
exact number that works is highly variable, and is likely to change even between
minor releases. Because this check is solely to prevent infinite compilation
times, it seems safe to disable it when a user has ascertained that their program
doesn't loop at the type level.

-}

-- | See Note [SubGoalDepth]
newtype SubGoalDepth = SubGoalDepth Int
  deriving (Eq, Ord, Outputable)

initialSubGoalDepth :: SubGoalDepth
initialSubGoalDepth = SubGoalDepth 0

bumpSubGoalDepth :: SubGoalDepth -> SubGoalDepth
bumpSubGoalDepth (SubGoalDepth n) = SubGoalDepth (n + 1)

maxSubGoalDepth :: SubGoalDepth -> SubGoalDepth -> SubGoalDepth
maxSubGoalDepth (SubGoalDepth n) (SubGoalDepth m) = SubGoalDepth (n `max` m)

subGoalDepthExceeded :: DynFlags -> SubGoalDepth -> Bool
subGoalDepthExceeded dflags (SubGoalDepth d)
  = mkIntWithInf d > reductionDepth dflags

{-
************************************************************************
*                                                                      *
            CtLoc
*                                                                      *
************************************************************************

The 'CtLoc' gives information about where a constraint came from.
This is important for decent error message reporting because
dictionaries don't appear in the original source code.

-}

data CtLoc = CtLoc { ctl_origin   :: CtOrigin
                   , ctl_env      :: TcLclEnv
                   , ctl_t_or_k   :: Maybe TypeOrKind  -- OK if we're not sure
                   , ctl_depth    :: !SubGoalDepth }

  -- The TcLclEnv includes particularly
  --    source location:  tcl_loc   :: RealSrcSpan
  --    context:          tcl_ctxt  :: [ErrCtxt]
  --    binder stack:     tcl_bndrs :: TcBinderStack
  --    level:            tcl_tclvl :: TcLevel

mkKindLoc :: TcType -> TcType   -- original *types* being compared
          -> CtLoc -> CtLoc
mkKindLoc s1 s2 loc = setCtLocOrigin (toKindLoc loc)
                        (KindEqOrigin s1 s2 (ctLocOrigin loc)
                                      (ctLocTypeOrKind_maybe loc))

-- | Take a CtLoc and moves it to the kind level
toKindLoc :: CtLoc -> CtLoc
toKindLoc loc = loc { ctl_t_or_k = Just KindLevel }

mkGivenLoc :: TcLevel -> SkolemInfoAnon -> TcLclEnv -> CtLoc
mkGivenLoc tclvl skol_info env
  = CtLoc { ctl_origin   = GivenOrigin skol_info
          , ctl_env      = setLclEnvTcLevel env tclvl
          , ctl_t_or_k   = Nothing    -- this only matters for error msgs
          , ctl_depth    = initialSubGoalDepth }

ctLocEnv :: CtLoc -> TcLclEnv
ctLocEnv = ctl_env

ctLocLevel :: CtLoc -> TcLevel
ctLocLevel loc = getLclEnvTcLevel (ctLocEnv loc)

ctLocDepth :: CtLoc -> SubGoalDepth
ctLocDepth = ctl_depth

ctLocOrigin :: CtLoc -> CtOrigin
ctLocOrigin = ctl_origin

ctLocSpan :: CtLoc -> RealSrcSpan
ctLocSpan (CtLoc { ctl_env = lcl}) = getLclEnvLoc lcl

ctLocTypeOrKind_maybe :: CtLoc -> Maybe TypeOrKind
ctLocTypeOrKind_maybe = ctl_t_or_k

setCtLocSpan :: CtLoc -> RealSrcSpan -> CtLoc
setCtLocSpan ctl@(CtLoc { ctl_env = lcl }) loc = setCtLocEnv ctl (setLclEnvLoc lcl loc)

bumpCtLocDepth :: CtLoc -> CtLoc
bumpCtLocDepth loc@(CtLoc { ctl_depth = d }) = loc { ctl_depth = bumpSubGoalDepth d }

setCtLocOrigin :: CtLoc -> CtOrigin -> CtLoc
setCtLocOrigin ctl orig = ctl { ctl_origin = orig }

updateCtLocOrigin :: CtLoc -> (CtOrigin -> CtOrigin) -> CtLoc
updateCtLocOrigin ctl@(CtLoc { ctl_origin = orig }) upd
  = ctl { ctl_origin = upd orig }

setCtLocEnv :: CtLoc -> TcLclEnv -> CtLoc
setCtLocEnv ctl env = ctl { ctl_env = env }

pprCtLoc :: CtLoc -> SDoc
-- "arising from ... at ..."
-- Not an instance of Outputable because of the "arising from" prefix
pprCtLoc (CtLoc { ctl_origin = o, ctl_env = lcl})
  = sep [ pprCtOrigin o
        , text "at" <+> ppr (getLclEnvLoc lcl)]
