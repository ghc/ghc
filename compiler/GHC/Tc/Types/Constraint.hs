{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, MultiWayIf #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | This module defines types and simple operations over constraints, as used
-- in the type-checker and constraint solver.
module GHC.Tc.Types.Constraint (
        -- QCInst
        QCInst(..), isPendingScInst,

        -- Canonical constraints
        Xi, Ct(..), Cts, CtIrredStatus(..), HoleSet,
        emptyCts, andCts, andManyCts, pprCts,
        singleCt, listToCts, ctsElts, consCts, snocCts, extendCtsList,
        isEmptyCts,
        isPendingScDict, superClassesMightHelp, getPendingWantedScs,
        isWantedCt, isDerivedCt, isGivenCt,
        isUserTypeErrorCt, getUserTypeErrorMsg,
        ctEvidence, ctLoc, setCtLoc, ctPred, ctFlavour, ctEqRel, ctOrigin,
        ctEvId, mkTcEqPredLikeEv,
        mkNonCanonical, mkNonCanonicalCt, mkGivens,
        mkIrredCt,
        ctEvPred, ctEvLoc, ctEvOrigin, ctEvEqRel,
        ctEvExpr, ctEvTerm, ctEvCoercion, ctEvEvId,
        tyCoVarsOfCt, tyCoVarsOfCts,
        tyCoVarsOfCtList, tyCoVarsOfCtsList,

        CanEqLHS(..), canEqLHS_maybe, canEqLHSKind, canEqLHSType,
        eqCanEqLHS,

        Hole(..), HoleSort(..), isOutOfScopeHole,

        WantedConstraints(..), insolubleWC, emptyWC, isEmptyWC,
        isSolvedWC, andWC, unionsWC, mkSimpleWC, mkImplicWC,
        addInsols, dropMisleading, addSimples, addImplics, addHoles,
        tyCoVarsOfWC, dropDerivedWC, dropDerivedSimples,
        tyCoVarsOfWCList, insolubleCt, insolubleEqCt,
        isDroppableCt, insolubleImplic,
        arisesFromGivens,

        Implication(..), implicationPrototype, checkTelescopeSkol,
        ImplicStatus(..), isInsolubleStatus, isSolvedStatus,
        HasGivenEqs(..),
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
        isWanted, isGiven, isDerived,
        ctEvRole,

        wrapType,

        CtFlavour(..), ShadowInfo(..), ctEvFlavour,
        CtFlavourRole, ctEvFlavourRole, ctFlavourRole,
        eqCanRewrite, eqCanRewriteFR, eqMayRewriteFR,
        eqCanDischargeFR,

        -- Pretty printing
        pprEvVarTheta,
        pprEvVars, pprEvVarWithType,

  )
  where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.Tc.Types ( TcLclEnv, setLclEnvTcLevel, getLclEnvTcLevel
                                   , setLclEnvLoc, getLclEnvLoc )

import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Types.Var

import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin

import GHC.Core

import GHC.Core.TyCo.Ppr
import GHC.Types.Name.Occurrence
import GHC.Utils.FV
import GHC.Types.Var.Set
import GHC.Driver.Session
import GHC.Types.Basic

import GHC.Utils.Outputable
import GHC.Types.SrcLoc
import GHC.Data.Bag
import GHC.Utils.Misc
import GHC.Utils.Panic

import Control.Monad ( msum )
import qualified Data.Semigroup ( (<>) )

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

The occurs check is performed in GHC.Tc.Utils.Unify.checkTypeEq.

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

      cc_pend_sc :: Bool   -- See Note [The superclass story] in GHC.Tc.Solver.Canonical
                           -- True <=> (a) cc_class has superclasses
                           --          (b) we have not (yet) added those
                           --              superclasses as Givens
    }

  | CIrredCan {  -- These stand for yet-unusable predicates
      cc_ev     :: CtEvidence,   -- See Note [Ct/evidence invariant]
      cc_status :: CtIrredStatus

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
       --   * See Note [inert_eqs: the inert equalities] in GHC.Tc.Solver.Monad
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
       --   * (TyEq:H) The RHS has no blocking coercion holes. See GHC.Tc.Solver.Canonical
       --     Note [Equalities with incompatible kinds], wrinkle (2)
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

------------
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
  ppr ConstraintHole = text "CosntraintHole"

------------
-- | Used to indicate extra information about why a CIrredCan is irreducible
data CtIrredStatus
  = InsolubleCIS   -- this constraint will never be solved
  | BlockedCIS HoleSet
                   -- this constraint is blocked on the coercion hole(s) listed
                   -- See Note [Equalities with incompatible kinds] in GHC.Tc.Solver.Canonical
                   -- Wrinkle (4a). Why store the HoleSet? See Wrinkle (2) of that
                   -- same Note.
                   -- INVARIANT: A BlockedCIS is a homogeneous equality whose
                   --   left hand side can fit in a CanEqLHS.
  | OtherCIS

instance Outputable CtIrredStatus where
  ppr InsolubleCIS       = text "(insoluble)"
  ppr (BlockedCIS holes) = parens (text "blocked on" <+> ppr holes)
  ppr OtherCIS           = text "(soluble)"

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

In contrast, the type of the evidence *term* (ctev_dest / ctev_evar) in
the evidence may *not* be fully zonked; we are careful not to look at it
during constraint solving. See Note [Evidence field of CtEvidence].

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
levity-polymorphic out-of-scope identifiers. See
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

mkIrredCt :: CtIrredStatus -> CtEvidence -> Ct
mkIrredCt status ev = CIrredCan { cc_ev = ev, cc_status = status }

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

setCtLoc :: Ct -> CtLoc -> Ct
setCtLoc ct loc = ct { cc_ev = (cc_ev ct) { ctev_loc = loc } }

ctOrigin :: Ct -> CtOrigin
ctOrigin = ctLocOrigin . ctLoc

ctPred :: Ct -> PredType
-- See Note [Ct/evidence invariant]
ctPred ct = ctEvPred (ctEvidence ct)

ctEvId :: Ct -> EvVar
-- The evidence Id for this Ct
ctEvId ct = ctEvEvId (ctEvidence ct)

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
         CDictCan { cc_pend_sc = pend_sc }
            | pend_sc   -> text "CDictCan(psc)"
            | otherwise -> text "CDictCan"
         CIrredCan { cc_status = status } -> text "CIrredCan" <> ppr status
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

-- | Returns free variables of constraints as a deterministically ordered.
-- list. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfCtList :: Ct -> [TcTyCoVar]
tyCoVarsOfCtList = fvVarList . tyCoFVsOfCt

-- | Returns free variables of constraints as a composable FV computation.
-- See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoFVsOfCt :: Ct -> FV
tyCoFVsOfCt ct = tyCoFVsOfType (ctPred ct)
  -- This must consult only the ctPred, so that it gets *tidied* fvs if the
  -- constraint has been tidied. Tidying a constraint does not tidy the
  -- fields of the Ct, only the predicate in the CtEvidence.

-- | Returns free variables of a bag of constraints as a non-deterministic
-- set. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfCts :: Cts -> TcTyCoVarSet
tyCoVarsOfCts = fvVarSet . tyCoFVsOfCts

-- | Returns free variables of a bag of constraints as a deterministically
-- ordered list. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfCtsList :: Cts -> [TcTyCoVar]
tyCoVarsOfCtsList = fvVarList . tyCoFVsOfCts

-- | Returns free variables of a bag of constraints as a composable FV
-- computation. See Note [Deterministic FV] in "GHC.Utils.FV".
tyCoFVsOfCts :: Cts -> FV
tyCoFVsOfCts = foldr (unionFV . tyCoFVsOfCt) emptyFV

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
tyCoFVsOfWC (WC { wc_simple = simple, wc_impl = implic, wc_holes = holes })
  = tyCoFVsOfCts simple `unionFV`
    tyCoFVsOfBag tyCoFVsOfImplic implic `unionFV`
    tyCoFVsOfBag tyCoFVsOfHole holes

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

tyCoFVsOfHole :: Hole -> FV
tyCoFVsOfHole (Hole { hole_ty = ty }) = tyCoFVsOfType ty

tyCoFVsOfBag :: (a -> FV) -> Bag a -> FV
tyCoFVsOfBag tvs_of = foldr (unionFV . tvs_of) emptyFV

---------------------------
dropDerivedWC :: WantedConstraints -> WantedConstraints
-- See Note [Dropping derived constraints]
dropDerivedWC wc@(WC { wc_simple = simples })
  = wc { wc_simple = dropDerivedSimples simples }
    -- The wc_impl implications are already (recursively) filtered

--------------------------
dropDerivedSimples :: Cts -> Cts
-- Drop all Derived constraints, but make [W] back into [WD],
-- so that if we re-simplify these constraints we will get all
-- the right derived constraints re-generated.  Forgetting this
-- step led to #12936
dropDerivedSimples simples = mapMaybeBag dropDerivedCt simples

dropDerivedCt :: Ct -> Maybe Ct
dropDerivedCt ct
  = case ctEvFlavour ev of
      Wanted WOnly -> Just (ct' { cc_ev = ev_wd })
      Wanted _     -> Just ct'
      _ | isDroppableCt ct -> Nothing
        | otherwise        -> Just ct
  where
    ev    = ctEvidence ct
    ev_wd = ev { ctev_nosh = WDeriv }
    ct'   = setPendingScDict ct -- See Note [Resetting cc_pend_sc]

{- Note [Resetting cc_pend_sc]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we discard Derived constraints, in dropDerivedSimples, we must
set the cc_pend_sc flag to True, so that if we re-process this
CDictCan we will re-generate its derived superclasses. Otherwise
we might miss some fundeps.  #13662 showed this up.

See Note [The superclass story] in GHC.Tc.Solver.Canonical.
-}

isDroppableCt :: Ct -> Bool
isDroppableCt ct
  = isDerived ev && not keep_deriv
    -- Drop only derived constraints, and then only if they
    -- obey Note [Dropping derived constraints]
  where
    ev   = ctEvidence ct
    loc  = ctEvLoc ev
    orig = ctLocOrigin loc

    keep_deriv
      = case ct of
          CIrredCan { cc_status = InsolubleCIS } -> keep_eq True
          _                                      -> keep_eq False

    keep_eq definitely_insoluble
       | isGivenOrigin orig    -- Arising only from givens
       = definitely_insoluble  -- Keep only definitely insoluble
       | otherwise
       = case orig of
           -- See Note [Dropping derived constraints]
           -- For fundeps, drop wanted/wanted interactions
           FunDepOrigin2 {} -> True   -- Top-level/Wanted
           FunDepOrigin1 _ orig1 _ _ orig2 _
             | g1 || g2  -> True  -- Given/Wanted errors: keep all
             | otherwise -> False -- Wanted/Wanted errors: discard
             where
               g1 = isGivenOrigin orig1
               g2 = isGivenOrigin orig2

           _ -> False

arisesFromGivens :: Ct -> Bool
arisesFromGivens ct
  = case ctEvidence ct of
      CtGiven {}                   -> True
      CtWanted {}                  -> False
      CtDerived { ctev_loc = loc } -> isGivenLoc loc

isGivenLoc :: CtLoc -> Bool
isGivenLoc loc = isGivenOrigin (ctLocOrigin loc)

{- Note [Dropping derived constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we discard derived constraints at the end of constraint solving;
see dropDerivedWC.  For example

 * Superclasses: if we have an unsolved [W] (Ord a), we don't want to
   complain about an unsolved [D] (Eq a) as well.

 * If we have [W] a ~ Int, [W] a ~ Bool, improvement will generate
   [D] Int ~ Bool, and we don't want to report that because it's
   incomprehensible. That is why we don't rewrite wanteds with wanteds!

 * We might float out some Wanteds from an implication, leaving behind
   their insoluble Deriveds. For example:

   forall a[2]. [W] alpha[1] ~ Int
                [W] alpha[1] ~ Bool
                [D] Int ~ Bool

   The Derived is insoluble, but we very much want to drop it when floating
   out.

But (tiresomely) we do keep *some* Derived constraints:

 * Type holes are derived constraints, because they have no evidence
   and we want to keep them, so we get the error report

 * We keep most derived equalities arising from functional dependencies
      - Given/Given interactions (subset of FunDepOrigin1):
        The definitely-insoluble ones reflect unreachable code.

        Others not-definitely-insoluble ones like [D] a ~ Int do not
        reflect unreachable code; indeed if fundeps generated proofs, it'd
        be a useful equality.  See #14763.   So we discard them.

      - Given/Wanted interacGiven or Wanted interacting with an
        instance declaration (FunDepOrigin2)

      - Given/Wanted interactions (FunDepOrigin1); see #9612

      - But for Wanted/Wanted interactions we do /not/ want to report an
        error (#13506).  Consider [W] C Int Int, [W] C Int Bool, with
        a fundep on class C.  We don't want to report an insoluble Int~Bool;
        c.f. "wanteds do not rewrite wanteds".

To distinguish these cases we use the CtOrigin.

NB: we keep *all* derived insolubles under some circumstances:

  * They are looked at by simplifyInfer, to decide whether to
    generalise.  Example: [W] a ~ Int, [W] a ~ Bool
    We get [D] Int ~ Bool, and indeed the constraints are insoluble,
    and we want simplifyInfer to see that, even though we don't
    ultimately want to generate an (inexplicable) error message from it


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

isDerivedCt :: Ct -> Bool
isDerivedCt = isDerived . ctEvidence

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
getUserTypeErrorMsg :: Ct -> Maybe Type
getUserTypeErrorMsg ct = findUserTypeError (ctPred ct)
  where
  findUserTypeError t = msum ( userTypeError_maybe t
                             : map findUserTypeError (subTys t)
                             )

  subTys t            = case splitAppTys t of
                          (t,[]) ->
                            case splitTyConApp_maybe t of
                              Nothing     -> []
                              Just (_,ts) -> ts
                          (t,ts) -> t : ts




isUserTypeErrorCt :: Ct -> Bool
isUserTypeErrorCt ct = case getUserTypeErrorMsg ct of
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

setPendingScDict :: Ct -> Ct
-- Set the cc_pend_sc flag to True
setPendingScDict ct@(CDictCan { cc_pend_sc = False })
                    = ct { cc_pend_sc = True }
setPendingScDict ct = ct

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

    might_help_ct ct = isWantedCt ct && not (is_ip ct)

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

  * Superclasses help only for Wanted constraints.  Derived constraints
    are not really "unsolved" and we certainly don't want them to
    trigger superclass expansion. This was a good part of the loop
    in  #11523

  * Even for Wanted constraints, we say "no" for implicit parameters.
    we have [W] ?x::ty, expanding superclasses won't help:
      - Superclasses can't be implicit parameters
      - If we have a [G] ?x:ty2, then we'll have another unsolved
        [D] ty ~ ty2 (from the functional dependency)
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
     These are forced to be in GHC.Tc.Types because
           TcLclEnv mentions WantedConstraints
           WantedConstraint mentions CtLoc
           CtLoc mentions ErrCtxt
           ErrCtxt mentions TcM
*                                                                      *
v%************************************************************************
-}

data WantedConstraints
  = WC { wc_simple :: Cts              -- Unsolved constraints, all wanted
       , wc_impl   :: Bag Implication
       , wc_holes  :: Bag Hole
    }

emptyWC :: WantedConstraints
emptyWC = WC { wc_simple = emptyBag
             , wc_impl   = emptyBag
             , wc_holes  = emptyBag }

mkSimpleWC :: [CtEvidence] -> WantedConstraints
mkSimpleWC cts
  = emptyWC { wc_simple = listToBag (map mkNonCanonical cts) }

mkImplicWC :: Bag Implication -> WantedConstraints
mkImplicWC implic
  = emptyWC { wc_impl = implic }

isEmptyWC :: WantedConstraints -> Bool
isEmptyWC (WC { wc_simple = f, wc_impl = i, wc_holes = holes })
  = isEmptyBag f && isEmptyBag i && isEmptyBag holes

-- | Checks whether a the given wanted constraints are solved, i.e.
-- that there are no simple constraints left and all the implications
-- are solved.
isSolvedWC :: WantedConstraints -> Bool
isSolvedWC WC {wc_simple = wc_simple, wc_impl = wc_impl, wc_holes = holes} =
  isEmptyBag wc_simple && allBag (isSolvedStatus . ic_status) wc_impl && isEmptyBag holes

andWC :: WantedConstraints -> WantedConstraints -> WantedConstraints
andWC (WC { wc_simple = f1, wc_impl = i1, wc_holes = h1 })
      (WC { wc_simple = f2, wc_impl = i2, wc_holes = h2 })
  = WC { wc_simple = f1 `unionBags` f2
       , wc_impl   = i1 `unionBags` i2
       , wc_holes  = h1 `unionBags` h2 }

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
  = wc { wc_holes = holes `unionBags` wc_holes wc }

dropMisleading :: WantedConstraints -> WantedConstraints
-- Drop misleading constraints; really just class constraints
-- See Note [Constraints and errors] in GHC.Tc.Utils.Monad
--   for why this function is so strange, treating the 'simples'
--   and the implications differently.  Sigh.
dropMisleading (WC { wc_simple = simples, wc_impl = implics, wc_holes = holes })
  = WC { wc_simple = filterBag insolubleCt simples
       , wc_impl   = mapBag drop_implic implics
       , wc_holes  = filterBag isOutOfScopeHole holes }
  where
    drop_implic implic
      = implic { ic_wanted = drop_wanted (ic_wanted implic) }
    drop_wanted (WC { wc_simple = simples, wc_impl = implics, wc_holes = holes })
      = WC { wc_simple = filterBag keep_ct simples
           , wc_impl   = mapBag drop_implic implics
           , wc_holes  = filterBag isOutOfScopeHole holes }

    keep_ct ct = case classifyPredType (ctPred ct) of
                    ClassPred {} -> False
                    _ -> True

isSolvedStatus :: ImplicStatus -> Bool
isSolvedStatus (IC_Solved {}) = True
isSolvedStatus _              = False

isInsolubleStatus :: ImplicStatus -> Bool
isInsolubleStatus IC_Insoluble    = True
isInsolubleStatus IC_BadTelescope = True
isInsolubleStatus _               = False

insolubleImplic :: Implication -> Bool
insolubleImplic ic = isInsolubleStatus (ic_status ic)

insolubleWC :: WantedConstraints -> Bool
insolubleWC (WC { wc_impl = implics, wc_simple = simples, wc_holes = holes })
  =  anyBag insolubleCt simples
  || anyBag insolubleImplic implics
  || anyBag isOutOfScopeHole holes  -- See Note [Insoluble holes]

insolubleCt :: Ct -> Bool
-- Definitely insoluble, in particular /excluding/ type-hole constraints
-- Namely: a) an equality constraint
--         b) that is insoluble
--         c) and does not arise from a Given
insolubleCt ct
  | not (insolubleEqCt ct) = False
  | arisesFromGivens ct    = False              -- See Note [Given insolubles]
  | otherwise              = True

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
insolubleEqCt (CIrredCan { cc_status = InsolubleCIS }) = True
insolubleEqCt _                                        = False

-- | Does this hole represent an "out of scope" error?
-- See Note [Insoluble holes]
isOutOfScopeHole :: Hole -> Bool
isOutOfScopeHole (Hole { hole_occ = occ }) = not (startsWithUnderscore occ)

instance Outputable WantedConstraints where
  ppr (WC {wc_simple = s, wc_impl = i, wc_holes = h})
   = text "WC" <+> braces (vcat
        [ ppr_bag (text "wc_simple") s
        , ppr_bag (text "wc_impl") i
        , ppr_bag (text "wc_holes") h ])

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

The same applies to Derived constraints that /arise from/ Givens.
E.g.   f :: (C Int [a]) => blah
where a fundep means we get
       [D] Int ~ [a]
By the same reasoning we must not suppress other errors (#15767)

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

      ic_skols :: [TcTyVar],     -- Introduced skolems
      ic_info  :: SkolemInfo,    -- See Note [Skolems in an implication]
                                 -- See Note [Shadowing in a constraint]

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
                                       -- See Invariang (WantedInf) in GHC.Tc.Utils.TcType

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
                    --   except by Note [Let-bound skolems] in GHC.Tc.Solver.Monad
  | LocalGivenEqs   -- Might have Given equalities, but only ones that affect only
                    --   local skolems e.g. forall a b. (a ~ F b) => ...
  | MaybeGivenEqs   -- Might have any kind of Given equalities; no floating out
                    --   is possible.
  deriving Eq

{- Note [HasGivenEqs]
~~~~~~~~~~~~~~~~~~~~~
The GivenEqs data type describes the Given constraints of an implication constraint:

* NoGivenEqs: definitely no Given equalities, except perhaps let-bound skolems
  which don't count: see Note [Let-bound skolems] in GHC.Tc.Solver.Monad
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

checkTelescopeSkol :: SkolemInfo -> Bool
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

Insoluble constraints *do* include Derived constraints. For example,
a functional dependency might give rise to [D] Int ~ Bool, and we must
report that.  If insolubles did not contain Deriveds, reportErrors would
never see it.


************************************************************************
*                                                                      *
            Pretty printing
*                                                                      *
************************************************************************
-}

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

Note [Evidence field of CtEvidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During constraint solving we never look at the type of ctev_evar/ctev_dest;
instead we look at the ctev_pred field.  The evtm/evar field
may be un-zonked.

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
-- Wanted equalities are always HoleDest; other wanteds are always
-- EvVarDest.
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
      , ctev_evar :: EvVar           -- See Note [Evidence field of CtEvidence]
      , ctev_loc  :: CtLoc }


  | CtWanted   -- Wanted goal
      { ctev_pred :: TcPredType     -- See Note [Ct/evidence invariant]
      , ctev_dest :: TcEvDest
      , ctev_nosh :: ShadowInfo     -- See Note [Constraint flavours]
      , ctev_loc  :: CtLoc }

  | CtDerived  -- A goal that we don't really have to solve and can't
               -- immediately rewrite anything other than a derived
               -- (there's no evidence!) but if we do manage to solve
               -- it may help in solving other goals.
      { ctev_pred :: TcPredType
      , ctev_loc  :: CtLoc }

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

ctEvExpr :: CtEvidence -> EvExpr
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
ctEvEvId ctev@(CtDerived {}) = pprPanic "ctEvId:" (ppr ctev)

instance Outputable TcEvDest where
  ppr (HoleDest h)   = text "hole" <> ppr h
  ppr (EvVarDest ev) = ppr ev

instance Outputable CtEvidence where
  ppr ev = ppr (ctEvFlavour ev)
           <+> pp_ev
           <+> braces (ppr (ctl_depth (ctEvLoc ev))) <> dcolon
                  -- Show the sub-goal depth too
           <+> ppr (ctEvPred ev)
    where
      pp_ev = case ev of
             CtGiven { ctev_evar = v } -> ppr v
             CtWanted {ctev_dest = d } -> ppr d
             CtDerived {}              -> text "_"

isWanted :: CtEvidence -> Bool
isWanted (CtWanted {}) = True
isWanted _ = False

isGiven :: CtEvidence -> Bool
isGiven (CtGiven {})  = True
isGiven _ = False

isDerived :: CtEvidence -> Bool
isDerived (CtDerived {}) = True
isDerived _              = False

{-
%************************************************************************
%*                                                                      *
            CtFlavour
%*                                                                      *
%************************************************************************

Note [Constraint flavours]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Constraints come in four flavours:

* [G] Given: we have evidence

* [W] Wanted WOnly: we want evidence

* [D] Derived: any solution must satisfy this constraint, but
      we don't need evidence for it.  Examples include:
        - superclasses of [W] class constraints
        - equalities arising from functional dependencies
          or injectivity

* [WD] Wanted WDeriv: a single constraint that represents
                      both [W] and [D]
  We keep them paired as one both for efficiency

The ctev_nosh field of a Wanted distinguishes between [W] and [WD]

Wanted constraints are born as [WD], but are split into [W] and its
"shadow" [D] in GHC.Tc.Solver.Monad.maybeEmitShadow.

See Note [The improvement story and derived shadows] in GHC.Tc.Solver.Monad
-}

data CtFlavour  -- See Note [Constraint flavours]
  = Given
  | Wanted ShadowInfo
  | Derived
  deriving Eq

data ShadowInfo
  = WDeriv   -- [WD] This Wanted constraint has no Derived shadow,
             -- so it behaves like a pair of a Wanted and a Derived
  | WOnly    -- [W] It has a separate derived shadow
             -- See Note [The improvement story and derived shadows] in GHC.Tc.Solver.Monad
  deriving( Eq )

instance Outputable CtFlavour where
  ppr Given           = text "[G]"
  ppr (Wanted WDeriv) = text "[WD]"
  ppr (Wanted WOnly)  = text "[W]"
  ppr Derived         = text "[D]"

ctEvFlavour :: CtEvidence -> CtFlavour
ctEvFlavour (CtWanted { ctev_nosh = nosh }) = Wanted nosh
ctEvFlavour (CtGiven {})                    = Given
ctEvFlavour (CtDerived {})                  = Derived

-- | Whether or not one 'Ct' can rewrite another is determined by its
-- flavour and its equality relation. See also
-- Note [Flavours with roles] in "GHC.Tc.Solver.Monad"
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

Note [Wanteds do not rewrite Wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't allow Wanteds to rewrite Wanteds, because that can give rise
to very confusing type error messages.  A good example is #8450.
Here's another
   f :: a -> Bool
   f x = ( [x,'c'], [x,True] ) `seq` True
Here we get
  [W] a ~ Char
  [W] a ~ Bool
but we do not want to complain about Bool ~ Char!

Note [Deriveds do rewrite Deriveds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
However we DO allow Deriveds to rewrite Deriveds, because that's how
improvement works; see Note [The improvement story] in GHC.Tc.Solver.Interact.

However, for now at least I'm only letting (Derived,NomEq) rewrite
(Derived,NomEq) and not doing anything for ReprEq.  If we have
    eqCanRewriteFR (Derived, NomEq) (Derived, _)  = True
then we lose property R2 of Definition [Can-rewrite relation]
in GHC.Tc.Solver.Monad
  R2.  If f1 >= f, and f2 >= f,
       then either f1 >= f2 or f2 >= f1
Consider f1 = (Given, ReprEq)
         f2 = (Derived, NomEq)
          f = (Derived, ReprEq)

I thought maybe we could never get Derived ReprEq constraints, but
we can; straight from the Wanteds during improvement. And from a Derived
ReprEq we could conceivably get a Derived NomEq improvement (by decomposing
a type constructor with Nomninal role), and hence unify.
-}

eqCanRewrite :: EqRel -> EqRel -> Bool
eqCanRewrite NomEq  _      = True
eqCanRewrite ReprEq ReprEq = True
eqCanRewrite ReprEq NomEq  = False

eqCanRewriteFR :: CtFlavourRole -> CtFlavourRole -> Bool
-- Can fr1 actually rewrite fr2?
-- Very important function!
-- See Note [eqCanRewrite]
-- See Note [Wanteds do not rewrite Wanteds]
-- See Note [Deriveds do rewrite Deriveds]
eqCanRewriteFR (Given,         r1)    (_,       r2)    = eqCanRewrite r1 r2
eqCanRewriteFR (Wanted WDeriv, NomEq) (Derived, NomEq) = True
eqCanRewriteFR (Derived,       NomEq) (Derived, NomEq) = True
eqCanRewriteFR _                      _                = False

eqMayRewriteFR :: CtFlavourRole -> CtFlavourRole -> Bool
-- Is it /possible/ that fr1 can rewrite fr2?
-- This is used when deciding which inerts to kick out,
-- at which time a [WD] inert may be split into [W] and [D]
eqMayRewriteFR (Wanted WDeriv, NomEq) (Wanted WDeriv, NomEq) = True
eqMayRewriteFR (Derived,       NomEq) (Wanted WDeriv, NomEq) = True
eqMayRewriteFR fr1 fr2 = eqCanRewriteFR fr1 fr2

{- Note [eqCanDischarge]
~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have two identical CEqCan equality constraints
(i.e. both LHS and RHS are the same)
      (x1:lhs~t) `eqCanDischarge` (xs:lhs~t)
Can we just drop x2 in favour of x1?

Answer: yes if eqCanDischarge is true.

Note that we do /not/ allow Wanted to discharge Derived.
We must keep both.  Why?  Because the Derived may rewrite
other Deriveds in the model whereas the Wanted cannot.

However a Wanted can certainly discharge an identical Wanted.  So
eqCanDischarge does /not/ define a can-rewrite relation in the
sense of Definition [Can-rewrite relation] in GHC.Tc.Solver.Monad.

We /do/ say that a [W] can discharge a [WD].  In evidence terms it
certainly can, and the /caller/ arranges that the otherwise-lost [D]
is spat out as a new Derived.  -}

eqCanDischargeFR :: CtFlavourRole -> CtFlavourRole -> Bool
-- See Note [eqCanDischarge]
eqCanDischargeFR (f1,r1) (f2, r2) =  eqCanRewrite r1 r2
                                  && eqCanDischargeF f1 f2

eqCanDischargeF :: CtFlavour -> CtFlavour -> Bool
eqCanDischargeF Given   _                  = True
eqCanDischargeF (Wanted _)      (Wanted _) = True
eqCanDischargeF (Wanted WDeriv) Derived    = True
eqCanDischargeF Derived         Derived    = True
eqCanDischargeF _               _          = False


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
type will evolve...

-}

data CtLoc = CtLoc { ctl_origin :: CtOrigin
                   , ctl_env    :: TcLclEnv
                   , ctl_t_or_k :: Maybe TypeOrKind  -- OK if we're not sure
                   , ctl_depth  :: !SubGoalDepth }

  -- The TcLclEnv includes particularly
  --    source location:  tcl_loc   :: RealSrcSpan
  --    context:          tcl_ctxt  :: [ErrCtxt]
  --    binder stack:     tcl_bndrs :: TcBinderStack
  --    level:            tcl_tclvl :: TcLevel

mkKindLoc :: TcType -> TcType   -- original *types* being compared
          -> CtLoc -> CtLoc
mkKindLoc s1 s2 loc = setCtLocOrigin (toKindLoc loc)
                        (KindEqOrigin s1 (Just s2) (ctLocOrigin loc)
                                      (ctLocTypeOrKind_maybe loc))

-- | Take a CtLoc and moves it to the kind level
toKindLoc :: CtLoc -> CtLoc
toKindLoc loc = loc { ctl_t_or_k = Just KindLevel }

mkGivenLoc :: TcLevel -> SkolemInfo -> TcLclEnv -> CtLoc
mkGivenLoc tclvl skol_info env
  = CtLoc { ctl_origin = GivenOrigin skol_info
          , ctl_env    = setLclEnvTcLevel env tclvl
          , ctl_t_or_k = Nothing    -- this only matters for error msgs
          , ctl_depth  = initialSubGoalDepth }

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
