{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}

module GHC.Tc.Solver.Canonical(
     canonicalize,
     unifyDerived, unifyTest, UnifyTestResult(..),
     makeSuperClasses,
     StopOrContinue(..), stopWith, continueWith, andWhenContinue,
     solveCallStack    -- For GHC.Tc.Solver
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Tc.Solver.Rewrite
import GHC.Tc.Solver.Monad
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.EvTerm
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Multiplicity
import GHC.Core.TyCo.Rep   -- cleverly decomposes types, good for completeness checking
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core
import GHC.Types.Id( mkTemplateLocals )
import GHC.Core.FamInstEnv ( FamInstEnvs )
import GHC.Tc.Instance.Family ( tcTopNormaliseNewTypeTF_maybe )
import GHC.Types.Var
import GHC.Types.Var.Env( mkInScopeSet )
import GHC.Types.Var.Set( delVarSetList, anyVarSet )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Builtin.Types ( anyTypeOfKind )
import GHC.Driver.Session( DynFlags )
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Hs.Type( HsIPName(..) )

import GHC.Data.Pair
import GHC.Utils.Misc
import GHC.Data.Bag
import GHC.Utils.Monad
import Control.Monad
import Data.Maybe ( isJust, isNothing )
import Data.List  ( zip4, partition )
import GHC.Types.Unique.Set( nonDetEltsUniqSet )
import GHC.Types.Basic

import Data.Bifunctor ( bimap )
import Data.Foldable ( traverse_ )

{-
************************************************************************
*                                                                      *
*                      The Canonicaliser                               *
*                                                                      *
************************************************************************

Note [Canonicalization]
~~~~~~~~~~~~~~~~~~~~~~~

Canonicalization converts a simple constraint to a canonical form. It is
unary (i.e. treats individual constraints one at a time).

Constraints originating from user-written code come into being as
CNonCanonicals. We know nothing about these constraints. So, first:

     Classify CNonCanoncal constraints, depending on whether they
     are equalities, class predicates, or other.

Then proceed depending on the shape of the constraint. Generally speaking,
each constraint gets rewritten and then decomposed into one of several forms
(see type Ct in GHC.Tc.Types).

When an already-canonicalized constraint gets kicked out of the inert set,
it must be recanonicalized. But we know a bit about its shape from the
last time through, so we can skip the classification step.

-}

-- Top-level canonicalization
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

canonicalize :: Ct -> TcS (StopOrContinue Ct)
canonicalize (CNonCanonical { cc_ev = ev })
  = {-# SCC "canNC" #-}
    canNC ev

canonicalize (CQuantCan (QCI { qci_ev = ev, qci_pend_sc = pend_sc }))
  = canForAll ev pend_sc

canonicalize (CIrredCan { cc_ev = ev })
  = canNC ev
    -- Instead of rewriting the evidence before classifying, it's possible we
    -- can make progress without the rewrite. Try this first.
    -- For insolubles (all of which are equalities), do /not/ rewrite the arguments
    -- In #14350 doing so led entire-unnecessary and ridiculously large
    -- type function expansion.  Instead, canEqNC just applies
    -- the substitution to the predicate, and may do decomposition;
    --    e.g. a ~ [a], where [G] a ~ [Int], can decompose

canonicalize (CDictCan { cc_ev = ev, cc_class  = cls
                       , cc_tyargs = xis, cc_pend_sc = pend_sc })
  = {-# SCC "canClass" #-}
    canClass ev cls xis pend_sc

canonicalize (CEqCan { cc_ev     = ev
                     , cc_lhs    = lhs
                     , cc_rhs    = rhs
                     , cc_eq_rel = eq_rel })
  = {-# SCC "canEqLeafTyVarEq" #-}
    canEqNC ev eq_rel (canEqLHSType lhs) rhs

canNC :: CtEvidence -> TcS (StopOrContinue Ct)
canNC ev =
  case classifyPredType pred of
      ClassPred cls tys     -> do traceTcS "canEvNC:cls" (ppr cls <+> ppr tys)
                                  canClassNC ev cls tys
      EqPred eq_rel ty1 ty2 -> do traceTcS "canEvNC:eq" (ppr ty1 $$ ppr ty2)
                                  canEqNC    ev eq_rel ty1 ty2
      IrredPred {}          -> do traceTcS "canEvNC:irred" (ppr pred)
                                  canIrred ev
      ForAllPred tvs th p   -> do traceTcS "canEvNC:forall" (ppr pred)
                                  canForAllNC ev tvs th p
  where
    pred = ctEvPred ev

{-
************************************************************************
*                                                                      *
*                      Class Canonicalization
*                                                                      *
************************************************************************
-}

canClassNC :: CtEvidence -> Class -> [Type] -> TcS (StopOrContinue Ct)
-- "NC" means "non-canonical"; that is, we have got here
-- from a NonCanonical constraint, not from a CDictCan
-- Precondition: EvVar is class evidence
canClassNC ev cls tys
  | isGiven ev  -- See Note [Eagerly expand given superclasses]
  = do { sc_cts <- mkStrictSuperClasses ev [] [] cls tys
       ; emitWork sc_cts
       ; canClass ev cls tys False }

  | isWanted ev
  , Just ip_name <- isCallStackPred cls tys
  , OccurrenceOf func <- ctLocOrigin loc
  -- If we're given a CallStack constraint that arose from a function
  -- call, we need to push the current call-site onto the stack instead
  -- of solving it directly from a given.
  -- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
  -- and Note [Solving CallStack constraints] in GHC.Tc.Solver.Monad
  = do { -- First we emit a new constraint that will capture the
         -- given CallStack.
       ; let new_loc = setCtLocOrigin loc (IPOccOrigin (HsIPName ip_name))
                            -- We change the origin to IPOccOrigin so
                            -- this rule does not fire again.
                            -- See Note [Overview of implicit CallStacks]

       ; new_ev <- newWantedEvVarNC new_loc pred

         -- Then we solve the wanted by pushing the call-site
         -- onto the newly emitted CallStack
       ; let ev_cs = EvCsPushCall func (ctLocSpan loc) (ctEvExpr new_ev)
       ; solveCallStack ev ev_cs

       ; canClass new_ev cls tys False }

  | otherwise
  = canClass ev cls tys (has_scs cls)

  where
    has_scs cls = not (null (classSCTheta cls))
    loc  = ctEvLoc ev
    pred = ctEvPred ev

solveCallStack :: CtEvidence -> EvCallStack -> TcS ()
-- Also called from GHC.Tc.Solver when defaulting call stacks
solveCallStack ev ev_cs = do
  -- We're given ev_cs :: CallStack, but the evidence term should be a
  -- dictionary, so we have to coerce ev_cs to a dictionary for
  -- `IP ip CallStack`. See Note [Overview of implicit CallStacks]
  cs_tm <- evCallStack ev_cs
  let ev_tm = mkEvCast cs_tm (wrapIP (ctEvPred ev))
  setEvBindIfWanted ev ev_tm

canClass :: CtEvidence
         -> Class -> [Type]
         -> Bool            -- True <=> un-explored superclasses
         -> TcS (StopOrContinue Ct)
-- Precondition: EvVar is class evidence

canClass ev cls tys pend_sc
  =   -- all classes do *nominal* matching
    ASSERT2( ctEvRole ev == Nominal, ppr ev $$ ppr cls $$ ppr tys )
    do { (xis, cos) <- rewriteArgsNom ev cls_tc tys
       ; let co = mkTcTyConAppCo Nominal cls_tc cos
             xi = mkClassPred cls xis
             mk_ct new_ev = CDictCan { cc_ev = new_ev
                                     , cc_tyargs = xis
                                     , cc_class = cls
                                     , cc_pend_sc = pend_sc }
       ; mb <- rewriteEvidence ev xi co
       ; traceTcS "canClass" (vcat [ ppr ev
                                   , ppr xi, ppr mb ])
       ; return (fmap mk_ct mb) }
  where
    cls_tc = classTyCon cls

{- Note [The superclass story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to add superclass constraints for two reasons:

* For givens [G], they give us a route to proof.  E.g.
    f :: Ord a => a -> Bool
    f x = x == x
  We get a Wanted (Eq a), which can only be solved from the superclass
  of the Given (Ord a).

* For wanteds [W], and deriveds [WD], [D], they may give useful
  functional dependencies.  E.g.
     class C a b | a -> b where ...
     class C a b => D a b where ...
  Now a [W] constraint (D Int beta) has (C Int beta) as a superclass
  and that might tell us about beta, via C's fundeps.  We can get this
  by generating a [D] (C Int beta) constraint.  It's derived because
  we don't actually have to cough up any evidence for it; it's only there
  to generate fundep equalities.

See Note [Why adding superclasses can help].

For these reasons we want to generate superclass constraints for both
Givens and Wanteds. But:

* (Minor) they are often not needed, so generating them aggressively
  is a waste of time.

* (Major) if we want recursive superclasses, there would be an infinite
  number of them.  Here is a real-life example (#10318);

     class (Frac (Frac a) ~ Frac a,
            Fractional (Frac a),
            IntegralDomain (Frac a))
         => IntegralDomain a where
      type Frac a :: *

  Notice that IntegralDomain has an associated type Frac, and one
  of IntegralDomain's superclasses is another IntegralDomain constraint.

So here's the plan:

1. Eagerly generate superclasses for given (but not wanted)
   constraints; see Note [Eagerly expand given superclasses].
   This is done using mkStrictSuperClasses in canClassNC, when
   we take a non-canonical Given constraint and cannonicalise it.

   However stop if you encounter the same class twice.  That is,
   mkStrictSuperClasses expands eagerly, but has a conservative
   termination condition: see Note [Expanding superclasses] in GHC.Tc.Utils.TcType.

2. Solve the wanteds as usual, but do no further expansion of
   superclasses for canonical CDictCans in solveSimpleGivens or
   solveSimpleWanteds; Note [Danger of adding superclasses during solving]

   However, /do/ continue to eagerly expand superclasses for new /given/
   /non-canonical/ constraints (canClassNC does this).  As #12175
   showed, a type-family application can expand to a class constraint,
   and we want to see its superclasses for just the same reason as
   Note [Eagerly expand given superclasses].

3. If we have any remaining unsolved wanteds
        (see Note [When superclasses help] in GHC.Tc.Types.Constraint)
   try harder: take both the Givens and Wanteds, and expand
   superclasses again.  See the calls to expandSuperClasses in
   GHC.Tc.Solver.simpl_loop and solveWanteds.

   This may succeed in generating (a finite number of) extra Givens,
   and extra Deriveds. Both may help the proof.

3a An important wrinkle: only expand Givens from the current level.
   Two reasons:
      - We only want to expand it once, and that is best done at
        the level it is bound, rather than repeatedly at the leaves
        of the implication tree
      - We may be inside a type where we can't create term-level
        evidence anyway, so we can't superclass-expand, say,
        (a ~ b) to get (a ~# b).  This happened in #15290.

4. Go round to (2) again.  This loop (2,3,4) is implemented
   in GHC.Tc.Solver.simpl_loop.

The cc_pend_sc flag in a CDictCan records whether the superclasses of
this constraint have been expanded.  Specifically, in Step 3 we only
expand superclasses for constraints with cc_pend_sc set to true (i.e.
isPendingScDict holds).

Why do we do this?  Two reasons:

* To avoid repeated work, by repeatedly expanding the superclasses of
  same constraint,

* To terminate the above loop, at least in the -XNoRecursiveSuperClasses
  case.  If there are recursive superclasses we could, in principle,
  expand forever, always encountering new constraints.

When we take a CNonCanonical or CIrredCan, but end up classifying it
as a CDictCan, we set the cc_pend_sc flag to False.

Note [Superclass loops]
~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  class C a => D a
  class D a => C a

Then, when we expand superclasses, we'll get back to the self-same
predicate, so we have reached a fixpoint in expansion and there is no
point in fruitlessly expanding further.  This case just falls out from
our strategy.  Consider
  f :: C a => a -> Bool
  f x = x==x
Then canClassNC gets the [G] d1: C a constraint, and eager emits superclasses
G] d2: D a, [G] d3: C a (psc).  (The "psc" means it has its sc_pend flag set.)
When processing d3 we find a match with d1 in the inert set, and we always
keep the inert item (d1) if possible: see Note [Replacement vs keeping] in
GHC.Tc.Solver.Interact.  So d3 dies a quick, happy death.

Note [Eagerly expand given superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In step (1) of Note [The superclass story], why do we eagerly expand
Given superclasses by one layer?  (By "one layer" we mean expand transitively
until you meet the same class again -- the conservative criterion embodied
in expandSuperClasses.  So a "layer" might be a whole stack of superclasses.)
We do this eagerly for Givens mainly because of some very obscure
cases like this:

   instance Bad a => Eq (T a)

   f :: (Ord (T a)) => blah
   f x = ....needs Eq (T a), Ord (T a)....

Here if we can't satisfy (Eq (T a)) from the givens we'll use the
instance declaration; but then we are stuck with (Bad a).  Sigh.
This is really a case of non-confluent proofs, but to stop our users
complaining we expand one layer in advance.

Note [Instance and Given overlap] in GHC.Tc.Solver.Interact.

We also want to do this if we have

   f :: F (T a) => blah

where
   type instance F (T a) = Ord (T a)

So we may need to do a little work on the givens to expose the
class that has the superclasses.  That's why the superclass
expansion for Givens happens in canClassNC.

This same scenario happens with quantified constraints, whose superclasses
are also eagerly expanded. Test case: typecheck/should_compile/T16502b
These are handled in canForAllNC, analogously to canClassNC.

Note [Why adding superclasses can help]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples of how adding superclasses can help:

    --- Example 1
        class C a b | a -> b
    Suppose we want to solve
         [G] C a b
         [W] C a beta
    Then adding [D] beta~b will let us solve it.

    -- Example 2 (similar but using a type-equality superclass)
        class (F a ~ b) => C a b
    And try to sllve:
         [G] C a b
         [W] C a beta
    Follow the superclass rules to add
         [G] F a ~ b
         [D] F a ~ beta
    Now we get [D] beta ~ b, and can solve that.

    -- Example (tcfail138)
      class L a b | a -> b
      class (G a, L a b) => C a b

      instance C a b' => G (Maybe a)
      instance C a b  => C (Maybe a) a
      instance L (Maybe a) a

    When solving the superclasses of the (C (Maybe a) a) instance, we get
      [G] C a b, and hance by superclasses, [G] G a, [G] L a b
      [W] G (Maybe a)
    Use the instance decl to get
      [W] C a beta
    Generate its derived superclass
      [D] L a beta.  Now using fundeps, combine with [G] L a b to get
      [D] beta ~ b
    which is what we want.

Note [Danger of adding superclasses during solving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here's a serious, but now out-dated example, from #4497:

   class Num (RealOf t) => Normed t
   type family RealOf x

Assume the generated wanted constraint is:
   [W] RealOf e ~ e
   [W] Normed e

If we were to be adding the superclasses during simplification we'd get:
   [W] RealOf e ~ e
   [W] Normed e
   [D] RealOf e ~ fuv
   [D] Num fuv
==>
   e := fuv, Num fuv, Normed fuv, RealOf fuv ~ fuv

While looks exactly like our original constraint. If we add the
superclass of (Normed fuv) again we'd loop.  By adding superclasses
definitely only once, during canonicalisation, this situation can't
happen.

Mind you, now that Wanteds cannot rewrite Derived, I think this particular
situation can't happen.

Note [Nested quantified constraint superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (typecheck/should_compile/T17202)

  class C1 a
  class (forall c. C1 c) => C2 a
  class (forall b. (b ~ F a) => C2 a) => C3 a

Elsewhere in the code, we get a [G] g1 :: C3 a. We expand its superclass
to get [G] g2 :: (forall b. (b ~ F a) => C2 a). This constraint has a
superclass, as well. But we now must be careful: we cannot just add
(forall c. C1 c) as a Given, because we need to remember g2's context.
That new constraint is Given only when forall b. (b ~ F a) is true.

It's tempting to make the new Given be (forall b. (b ~ F a) => forall c. C1 c),
but that's problematic, because it's nested, and ForAllPred is not capable
of representing a nested quantified constraint. (We could change ForAllPred
to allow this, but the solution in this Note is much more local and simpler.)

So, we swizzle it around to get (forall b c. (b ~ F a) => C1 c).

More generally, if we are expanding the superclasses of
  g0 :: forall tvs. theta => cls tys
and find a superclass constraint
  forall sc_tvs. sc_theta => sc_inner_pred
we must have a selector
  sel_id :: forall cls_tvs. cls cls_tvs -> forall sc_tvs. sc_theta => sc_inner_pred
and thus build
  g_sc :: forall tvs sc_tvs. theta => sc_theta => sc_inner_pred
  g_sc = /\ tvs. /\ sc_tvs. \ theta_ids. \ sc_theta_ids.
         sel_id tys (g0 tvs theta_ids) sc_tvs sc_theta_ids

Actually, we cheat a bit by eta-reducing: note that sc_theta_ids are both the
last bound variables and the last arguments. This avoids the need to produce
the sc_theta_ids at all. So our final construction is

  g_sc = /\ tvs. /\ sc_tvs. \ theta_ids.
         sel_id tys (g0 tvs theta_ids) sc_tvs

  -}

makeSuperClasses :: [Ct] -> TcS [Ct]
-- Returns strict superclasses, transitively, see Note [The superclasses story]
-- See Note [The superclass story]
-- The loop-breaking here follows Note [Expanding superclasses] in GHC.Tc.Utils.TcType
-- Specifically, for an incoming (C t) constraint, we return all of (C t)'s
--    superclasses, up to /and including/ the first repetition of C
--
-- Example:  class D a => C a
--           class C [a] => D a
-- makeSuperClasses (C x) will return (D x, C [x])
--
-- NB: the incoming constraints have had their cc_pend_sc flag already
--     flipped to False, by isPendingScDict, so we are /obliged/ to at
--     least produce the immediate superclasses
makeSuperClasses cts = concatMapM go cts
  where
    go (CDictCan { cc_ev = ev, cc_class = cls, cc_tyargs = tys })
      = mkStrictSuperClasses ev [] [] cls tys
    go (CQuantCan (QCI { qci_pred = pred, qci_ev = ev }))
      = ASSERT2( isClassPred pred, ppr pred )  -- The cts should all have
                                               -- class pred heads
        mkStrictSuperClasses ev tvs theta cls tys
      where
        (tvs, theta, cls, tys) = tcSplitDFunTy (ctEvPred ev)
    go ct = pprPanic "makeSuperClasses" (ppr ct)

mkStrictSuperClasses
    :: CtEvidence
    -> [TyVar] -> ThetaType  -- These two args are non-empty only when taking
                             -- superclasses of a /quantified/ constraint
    -> Class -> [Type] -> TcS [Ct]
-- Return constraints for the strict superclasses of
--   ev :: forall as. theta => cls tys
mkStrictSuperClasses ev tvs theta cls tys
  = mk_strict_superclasses (unitNameSet (className cls))
                           ev tvs theta cls tys

mk_strict_superclasses :: NameSet -> CtEvidence
                       -> [TyVar] -> ThetaType
                       -> Class -> [Type] -> TcS [Ct]
-- Always return the immediate superclasses of (cls tys);
-- and expand their superclasses, provided none of them are in rec_clss
-- nor are repeated
mk_strict_superclasses rec_clss (CtGiven { ctev_evar = evar, ctev_loc = loc })
                       tvs theta cls tys
  = concatMapM (do_one_given (mk_given_loc loc)) $
    classSCSelIds cls
  where
    dict_ids  = mkTemplateLocals theta
    size      = sizeTypes tys

    do_one_given given_loc sel_id
      | isUnliftedType sc_pred
      , not (null tvs && null theta)
      = -- See Note [Equality superclasses in quantified constraints]
        return []
      | otherwise
      = do { given_ev <- newGivenEvVar given_loc $
                         mk_given_desc sel_id sc_pred
           ; mk_superclasses rec_clss given_ev tvs theta sc_pred }
      where
        sc_pred = classMethodInstTy sel_id tys

      -- See Note [Nested quantified constraint superclasses]
    mk_given_desc :: Id -> PredType -> (PredType, EvTerm)
    mk_given_desc sel_id sc_pred
      = (swizzled_pred, swizzled_evterm)
      where
        (sc_tvs, sc_rho)          = splitForAllTyCoVars sc_pred
        (sc_theta, sc_inner_pred) = splitFunTys sc_rho

        all_tvs       = tvs `chkAppend` sc_tvs
        all_theta     = theta `chkAppend` (map scaledThing sc_theta)
        swizzled_pred = mkInfSigmaTy all_tvs all_theta sc_inner_pred

        -- evar :: forall tvs. theta => cls tys
        -- sel_id :: forall cls_tvs. cls cls_tvs
        --                        -> forall sc_tvs. sc_theta => sc_inner_pred
        -- swizzled_evterm :: forall tvs sc_tvs. theta => sc_theta => sc_inner_pred
        swizzled_evterm = EvExpr $
          mkLams all_tvs $
          mkLams dict_ids $
          Var sel_id
            `mkTyApps` tys
            `App` (evId evar `mkVarApps` (tvs ++ dict_ids))
            `mkVarApps` sc_tvs

    mk_given_loc loc
       | isCTupleClass cls
       = loc   -- For tuple predicates, just take them apart, without
               -- adding their (large) size into the chain.  When we
               -- get down to a base predicate, we'll include its size.
               -- #10335

       | GivenOrigin skol_info <- ctLocOrigin loc
         -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
         -- for explantation of this transformation for givens
       = case skol_info of
            InstSkol -> loc { ctl_origin = GivenOrigin (InstSC size) }
            InstSC n -> loc { ctl_origin = GivenOrigin (InstSC (n `max` size)) }
            _        -> loc

       | otherwise  -- Probably doesn't happen, since this function
       = loc        -- is only used for Givens, but does no harm

mk_strict_superclasses rec_clss ev tvs theta cls tys
  | all noFreeVarsOfType tys
  = return [] -- Wanteds with no variables yield no deriveds.
              -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted/Derived case, just add Derived superclasses
              -- that can lead to improvement.
  = ASSERT2( null tvs && null theta, ppr tvs $$ ppr theta )
    concatMapM do_one_derived (immSuperClasses cls tys)
  where
    loc = ctEvLoc ev

    do_one_derived sc_pred
      = do { sc_ev <- newDerivedNC loc sc_pred
           ; mk_superclasses rec_clss sc_ev [] [] sc_pred }

{- Note [Improvement from Ground Wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose class C b a => D a b
and consider
  [W] D Int Bool
Is there any point in emitting [D] C Bool Int?  No!  The only point of
emitting superclass constraints for W/D constraints is to get
improvement, extra unifications that result from functional
dependencies.  See Note [Why adding superclasses can help] above.

But no variables means no improvement; case closed.
-}

mk_superclasses :: NameSet -> CtEvidence
                -> [TyVar] -> ThetaType -> PredType -> TcS [Ct]
-- Return this constraint, plus its superclasses, if any
mk_superclasses rec_clss ev tvs theta pred
  | ClassPred cls tys <- classifyPredType pred
  = mk_superclasses_of rec_clss ev tvs theta cls tys

  | otherwise   -- Superclass is not a class predicate
  = return [mkNonCanonical ev]

mk_superclasses_of :: NameSet -> CtEvidence
                   -> [TyVar] -> ThetaType -> Class -> [Type]
                   -> TcS [Ct]
-- Always return this class constraint,
-- and expand its superclasses
mk_superclasses_of rec_clss ev tvs theta cls tys
  | loop_found = do { traceTcS "mk_superclasses_of: loop" (ppr cls <+> ppr tys)
                    ; return [this_ct] }  -- cc_pend_sc of this_ct = True
  | otherwise  = do { traceTcS "mk_superclasses_of" (vcat [ ppr cls <+> ppr tys
                                                          , ppr (isCTupleClass cls)
                                                          , ppr rec_clss
                                                          ])
                    ; sc_cts <- mk_strict_superclasses rec_clss' ev tvs theta cls tys
                    ; return (this_ct : sc_cts) }
                                   -- cc_pend_sc of this_ct = False
  where
    cls_nm     = className cls
    loop_found = not (isCTupleClass cls) && cls_nm `elemNameSet` rec_clss
                 -- Tuples never contribute to recursion, and can be nested
    rec_clss'  = rec_clss `extendNameSet` cls_nm

    this_ct | null tvs, null theta
            = CDictCan { cc_ev = ev, cc_class = cls, cc_tyargs = tys
                       , cc_pend_sc = loop_found }
                 -- NB: If there is a loop, we cut off, so we have not
                 --     added the superclasses, hence cc_pend_sc = True
            | otherwise
            = CQuantCan (QCI { qci_tvs = tvs, qci_pred = mkClassPred cls tys
                             , qci_ev = ev
                             , qci_pend_sc = loop_found })


{- Note [Equality superclasses in quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#15359, #15593, #15625)
  f :: (forall a. theta => a ~ b) => stuff

It's a bit odd to have a local, quantified constraint for `(a~b)`,
but some people want such a thing (see the tickets). And for
Coercible it is definitely useful
  f :: forall m. (forall p q. Coercible p q => Coercible (m p) (m q)))
                 => stuff

Moreover it's not hard to arrange; we just need to look up /equality/
constraints in the quantified-constraint environment, which we do in
GHC.Tc.Solver.Interact.doTopReactOther.

There is a wrinkle though, in the case where 'theta' is empty, so
we have
  f :: (forall a. a~b) => stuff

Now, potentially, the superclass machinery kicks in, in
makeSuperClasses, giving us a a second quantified constraint
       (forall a. a ~# b)
BUT this is an unboxed value!  And nothing has prepared us for
dictionary "functions" that are unboxed.  Actually it does just
about work, but the simplifier ends up with stuff like
   case (/\a. eq_sel d) of df -> ...(df @Int)...
and fails to simplify that any further.  And it doesn't satisfy
isPredTy any more.

So for now we simply decline to take superclasses in the quantified
case.  Instead we have a special case in GHC.Tc.Solver.Interact.doTopReactOther,
which looks for primitive equalities specially in the quantified
constraints.

See also Note [Evidence for quantified constraints] in GHC.Core.Predicate.


************************************************************************
*                                                                      *
*                      Irreducibles canonicalization
*                                                                      *
************************************************************************
-}

canIrred :: CtEvidence -> TcS (StopOrContinue Ct)
-- Precondition: ty not a tuple and no other evidence form
canIrred ev
  = do { let pred = ctEvPred ev
       ; traceTcS "can_pred" (text "IrredPred = " <+> ppr pred)
       ; (xi,co) <- rewrite ev pred -- co :: xi ~ pred
       ; rewriteEvidence ev xi co `andWhenContinue` \ new_ev ->

    do { -- Re-classify, in case rewriting has improved its shape
         -- Code is like the canNC, except
         -- that the IrredPred branch stops work
       ; case classifyPredType (ctEvPred new_ev) of
           ClassPred cls tys     -> canClassNC new_ev cls tys
           EqPred eq_rel ty1 ty2 -> canEqNC new_ev eq_rel ty1 ty2
           ForAllPred tvs th p   -> -- this is highly suspect; Quick Look
                                    -- should never leave a meta-var filled
                                    -- in with a polytype. This is #18987.
                                    do traceTcS "canEvNC:forall" (ppr pred)
                                       canForAllNC ev tvs th p
           IrredPred {}          -> continueWith $
                                    mkIrredCt OtherCIS new_ev } }

{- *********************************************************************
*                                                                      *
*                      Quantified predicates
*                                                                      *
********************************************************************* -}

{- Note [Quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The -XQuantifiedConstraints extension allows type-class contexts like this:

  data Rose f x = Rose x (f (Rose f x))

  instance (Eq a, forall b. Eq b => Eq (f b))
        => Eq (Rose f a)  where
    (Rose x1 rs1) == (Rose x2 rs2) = x1==x2 && rs1 == rs2

Note the (forall b. Eq b => Eq (f b)) in the instance contexts.
This quantified constraint is needed to solve the
 [W] (Eq (f (Rose f x)))
constraint which arises form the (==) definition.

The wiki page is
  https://gitlab.haskell.org/ghc/ghc/wikis/quantified-constraints
which in turn contains a link to the GHC Proposal where the change
is specified, and a Haskell Symposium paper about it.

We implement two main extensions to the design in the paper:

 1. We allow a variable in the instance head, e.g.
      f :: forall m a. (forall b. m b) => D (m a)
    Notice the 'm' in the head of the quantified constraint, not
    a class.

 2. We support superclasses to quantified constraints.
    For example (contrived):
      f :: (Ord b, forall b. Ord b => Ord (m b)) => m a -> m a -> Bool
      f x y = x==y
    Here we need (Eq (m a)); but the quantified constraint deals only
    with Ord.  But we can make it work by using its superclass.

Here are the moving parts
  * Language extension {-# LANGUAGE QuantifiedConstraints #-}
    and add it to ghc-boot-th:GHC.LanguageExtensions.Type.Extension

  * A new form of evidence, EvDFun, that is used to discharge
    such wanted constraints

  * checkValidType gets some changes to accept forall-constraints
    only in the right places.

  * Predicate.Pred gets a new constructor ForAllPred, and
    and classifyPredType analyses a PredType to decompose
    the new forall-constraints

  * GHC.Tc.Solver.Monad.InertCans gets an extra field, inert_insts,
    which holds all the Given forall-constraints.  In effect,
    such Given constraints are like local instance decls.

  * When trying to solve a class constraint, via
    GHC.Tc.Solver.Interact.matchInstEnv, use the InstEnv from inert_insts
    so that we include the local Given forall-constraints
    in the lookup.  (See GHC.Tc.Solver.Monad.getInstEnvs.)

  * GHC.Tc.Solver.Canonical.canForAll deals with solving a
    forall-constraint.  See
       Note [Solving a Wanted forall-constraint]

  * We augment the kick-out code to kick out an inert
    forall constraint if it can be rewritten by a new
    type equality; see GHC.Tc.Solver.Monad.kick_out_rewritable

Note that a quantified constraint is never /inferred/
(by GHC.Tc.Solver.simplifyInfer).  A function can only have a
quantified constraint in its type if it is given an explicit
type signature.

-}

canForAllNC :: CtEvidence -> [TyVar] -> TcThetaType -> TcPredType
            -> TcS (StopOrContinue Ct)
canForAllNC ev tvs theta pred
  | isGiven ev  -- See Note [Eagerly expand given superclasses]
  , Just (cls, tys) <- cls_pred_tys_maybe
  = do { sc_cts <- mkStrictSuperClasses ev tvs theta cls tys
       ; emitWork sc_cts
       ; canForAll ev False }

  | otherwise
  = canForAll ev (isJust cls_pred_tys_maybe)

  where
    cls_pred_tys_maybe = getClassPredTys_maybe pred

canForAll :: CtEvidence -> Bool -> TcS (StopOrContinue Ct)
-- We have a constraint (forall as. blah => C tys)
canForAll ev pend_sc
  = do { -- First rewrite it to apply the current substitution
         let pred = ctEvPred ev
       ; (xi,co) <- rewrite ev pred -- co :: xi ~ pred
       ; rewriteEvidence ev xi co `andWhenContinue` \ new_ev ->

    do { -- Now decompose into its pieces and solve it
         -- (It takes a lot less code to rewrite before decomposing.)
       ; case classifyPredType (ctEvPred new_ev) of
           ForAllPred tvs theta pred
              -> solveForAll new_ev tvs theta pred pend_sc
           _  -> pprPanic "canForAll" (ppr new_ev)
    } }

solveForAll :: CtEvidence -> [TyVar] -> TcThetaType -> PredType -> Bool
            -> TcS (StopOrContinue Ct)
solveForAll ev tvs theta pred pend_sc
  | CtWanted { ctev_dest = dest } <- ev
  = -- See Note [Solving a Wanted forall-constraint]
    do { let skol_info = QuantCtxtSkol
             empty_subst = mkEmptyTCvSubst $ mkInScopeSet $
                           tyCoVarsOfTypes (pred:theta) `delVarSetList` tvs
       ; (subst, skol_tvs) <- tcInstSkolTyVarsX empty_subst tvs
       ; given_ev_vars <- mapM newEvVar (substTheta subst theta)

       ; (lvl, (w_id, wanteds))
             <- pushLevelNoWorkList (ppr skol_info) $
                do { wanted_ev <- newWantedEvVarNC loc $
                                  substTy subst pred
                   ; return ( ctEvEvId wanted_ev
                            , unitBag (mkNonCanonical wanted_ev)) }

      ; ev_binds <- emitImplicationTcS lvl skol_info skol_tvs
                                       given_ev_vars wanteds

      ; setWantedEvTerm dest $
        EvFun { et_tvs = skol_tvs, et_given = given_ev_vars
              , et_binds = ev_binds, et_body = w_id }

      ; stopWith ev "Wanted forall-constraint" }

  | isGiven ev   -- See Note [Solving a Given forall-constraint]
  = do { addInertForAll qci
       ; stopWith ev "Given forall-constraint" }

  | otherwise
  = do { traceTcS "discarding derived forall-constraint" (ppr ev)
       ; stopWith ev "Derived forall-constraint" }
  where
    loc = ctEvLoc ev
    qci = QCI { qci_ev = ev, qci_tvs = tvs
              , qci_pred = pred, qci_pend_sc = pend_sc }

{- Note [Solving a Wanted forall-constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Solving a wanted forall (quantified) constraint
  [W] df :: forall ab. (Eq a, Ord b) => C x a b
is delightfully easy.   Just build an implication constraint
    forall ab. (g1::Eq a, g2::Ord b) => [W] d :: C x a
and discharge df thus:
    df = /\ab. \g1 g2. let <binds> in d
where <binds> is filled in by solving the implication constraint.
All the machinery is to hand; there is little to do.

Note [Solving a Given forall-constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a Given constraint
  [G] df :: forall ab. (Eq a, Ord b) => C x a b
we just add it to TcS's local InstEnv of known instances,
via addInertForall.  Then, if we look up (C x Int Bool), say,
we'll find a match in the InstEnv.


************************************************************************
*                                                                      *
*        Equalities
*                                                                      *
************************************************************************

Note [Canonicalising equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to canonicalise an equality, we look at the structure of the
two types at hand, looking for similarities. A difficulty is that the
types may look dissimilar before rewriting but similar after rewriting.
However, we don't just want to jump in and rewrite right away, because
this might be wasted effort. So, after looking for similarities and failing,
we rewrite and then try again. Of course, we don't want to loop, so we
track whether or not we've already rewritten.

It is conceivable to do a better job at tracking whether or not a type
is rewritten, but this is left as future work. (Mar '15)


Note [Decomposing FunTy]
~~~~~~~~~~~~~~~~~~~~~~~~
can_eq_nc' may attempt to decompose a FunTy that is un-zonked.  This
means that we may very well have a FunTy containing a type of some
unknown kind. For instance, we may have,

    FunTy (a :: k) Int

Where k is a unification variable. So the calls to getRuntimeRep_maybe may
fail (returning Nothing).  In that case we'll fall through, zonk, and try again.
Zonking should fill the variable k, meaning that decomposition will succeed the
second time around.

Also note that we require the AnonArgFlag to match.  This will stop
us decomposing
   (Int -> Bool)  ~  (Show a => blah)
It's as if we treat (->) and (=>) as different type constructors.
-}

canEqNC :: CtEvidence -> EqRel -> Type -> Type -> TcS (StopOrContinue Ct)
canEqNC ev eq_rel ty1 ty2
  = do { result <- zonk_eq_types ty1 ty2
       ; case result of
           Left (Pair ty1' ty2') -> can_eq_nc False ev eq_rel ty1' ty1 ty2' ty2
           Right ty              -> canEqReflexive ev eq_rel ty }

can_eq_nc
   :: Bool            -- True => both types are rewritten
   -> CtEvidence
   -> EqRel
   -> Type -> Type    -- LHS, after and before type-synonym expansion, resp
   -> Type -> Type    -- RHS, after and before type-synonym expansion, resp
   -> TcS (StopOrContinue Ct)
can_eq_nc rewritten ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  = do { traceTcS "can_eq_nc" $
         vcat [ ppr rewritten, ppr ev, ppr eq_rel, ppr ty1, ppr ps_ty1, ppr ty2, ppr ps_ty2 ]
       ; rdr_env <- getGlobalRdrEnvTcS
       ; fam_insts <- getFamInstEnvs
       ; can_eq_nc' rewritten rdr_env fam_insts ev eq_rel ty1 ps_ty1 ty2 ps_ty2 }

can_eq_nc'
   :: Bool           -- True => both input types are rewritten
   -> GlobalRdrEnv   -- needed to see which newtypes are in scope
   -> FamInstEnvs    -- needed to unwrap data instances
   -> CtEvidence
   -> EqRel
   -> Type -> Type    -- LHS, after and before type-synonym expansion, resp
   -> Type -> Type    -- RHS, after and before type-synonym expansion, resp
   -> TcS (StopOrContinue Ct)

-- See Note [Comparing nullary type synonyms] in GHC.Core.Type.
can_eq_nc' _flat _rdr_env _envs ev eq_rel ty1@(TyConApp tc1 []) _ps_ty1 (TyConApp tc2 []) _ps_ty2
  | tc1 == tc2
  = canEqReflexive ev eq_rel ty1

-- Expand synonyms first; see Note [Type synonyms and canonicalization]
can_eq_nc' rewritten rdr_env envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | Just ty1' <- tcView ty1 = can_eq_nc' rewritten rdr_env envs ev eq_rel ty1' ps_ty1 ty2  ps_ty2
  | Just ty2' <- tcView ty2 = can_eq_nc' rewritten rdr_env envs ev eq_rel ty1  ps_ty1 ty2' ps_ty2

-- need to check for reflexivity in the ReprEq case.
-- See Note [Eager reflexivity check]
-- Check only when rewritten because the zonk_eq_types check in canEqNC takes
-- care of the non-rewritten case.
can_eq_nc' True _rdr_env _envs ev ReprEq ty1 _ ty2 _
  | ty1 `tcEqType` ty2
  = canEqReflexive ev ReprEq ty1

-- When working with ReprEq, unwrap newtypes.
-- See Note [Unwrap newtypes first]
-- This must be above the TyVarTy case, in order to guarantee (TyEq:N)
can_eq_nc' _rewritten rdr_env envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | ReprEq <- eq_rel
  , Just stuff1 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty1
  = can_eq_newtype_nc ev NotSwapped ty1 stuff1 ty2 ps_ty2

  | ReprEq <- eq_rel
  , Just stuff2 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty2
  = can_eq_newtype_nc ev IsSwapped  ty2 stuff2 ty1 ps_ty1

-- Then, get rid of casts
can_eq_nc' rewritten _rdr_env _envs ev eq_rel (CastTy ty1 co1) _ ty2 ps_ty2
  | isNothing (canEqLHS_maybe ty2)  -- See (3) in Note [Equalities with incompatible kinds]
  = canEqCast rewritten ev eq_rel NotSwapped ty1 co1 ty2 ps_ty2
can_eq_nc' rewritten _rdr_env _envs ev eq_rel ty1 ps_ty1 (CastTy ty2 co2) _
  | isNothing (canEqLHS_maybe ty1)  -- See (3) in Note [Equalities with incompatible kinds]
  = canEqCast rewritten ev eq_rel IsSwapped ty2 co2 ty1 ps_ty1

----------------------
-- Otherwise try to decompose
----------------------

-- Literals
can_eq_nc' _rewritten _rdr_env _envs ev eq_rel ty1@(LitTy l1) _ (LitTy l2) _
 | l1 == l2
  = do { setEvBindIfWanted ev (evCoercion $ mkReflCo (eqRelRole eq_rel) ty1)
       ; stopWith ev "Equal LitTy" }

-- Decompose FunTy: (s -> t) and (c => t)
-- NB: don't decompose (Int -> blah) ~ (Show a => blah)
can_eq_nc' _rewritten _rdr_env _envs ev eq_rel
           (FunTy { ft_mult = am1, ft_af = af1, ft_arg = ty1a, ft_res = ty1b }) _ps_ty1
           (FunTy { ft_mult = am2, ft_af = af2, ft_arg = ty2a, ft_res = ty2b }) _ps_ty2
  | af1 == af2   -- Don't decompose (Int -> blah) ~ (Show a => blah)
  , Just ty1a_rep <- getRuntimeRep_maybe ty1a  -- getRutimeRep_maybe:
  , Just ty1b_rep <- getRuntimeRep_maybe ty1b  -- see Note [Decomposing FunTy]
  , Just ty2a_rep <- getRuntimeRep_maybe ty2a
  , Just ty2b_rep <- getRuntimeRep_maybe ty2b
  = canDecomposableTyConAppOK ev eq_rel funTyCon
                              [am1, ty1a_rep, ty1b_rep, ty1a, ty1b]
                              [am2, ty2a_rep, ty2b_rep, ty2a, ty2b]

-- Decompose type constructor applications
-- NB: we have expanded type synonyms already
can_eq_nc' _rewritten _rdr_env _envs ev eq_rel ty1 _ ty2 _
  | Just (tc1, tys1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, tys2) <- tcSplitTyConApp_maybe ty2
   -- we want to catch e.g. Maybe Int ~ (Int -> Int) here for better
   -- error messages rather than decomposing into AppTys;
   -- hence no direct match on TyConApp
  , not (isTypeFamilyTyCon tc1)
  , not (isTypeFamilyTyCon tc2)
  = canTyConApp ev eq_rel tc1 tys1 tc2 tys2

can_eq_nc' _rewritten _rdr_env _envs ev eq_rel
           s1@(ForAllTy (Bndr _ vis1) _) _
           s2@(ForAllTy (Bndr _ vis2) _) _
  | vis1 `sameVis` vis2 -- Note [ForAllTy and typechecker equality]
  = can_eq_nc_forall ev eq_rel s1 s2

-- See Note [Canonicalising type applications] about why we require rewritten types
-- Use tcSplitAppTy, not matching on AppTy, to catch oversaturated type families
-- NB: Only decompose AppTy for nominal equality. See Note [Decomposing equality]
can_eq_nc' True _rdr_env _envs ev NomEq ty1 _ ty2 _
  | Just (t1, s1) <- tcSplitAppTy_maybe ty1
  , Just (t2, s2) <- tcSplitAppTy_maybe ty2
  = can_eq_app ev t1 s1 t2 s2

-------------------
-- Can't decompose.
-------------------

-- No similarity in type structure detected. Rewrite and try again.
can_eq_nc' False rdr_env envs ev eq_rel _ ps_ty1 _ ps_ty2
  = do { (xi1, co1) <- rewrite ev ps_ty1
       ; (xi2, co2) <- rewrite ev ps_ty2
       ; new_ev <- rewriteEqEvidence ev NotSwapped xi1 xi2 co1 co2
       ; can_eq_nc' True rdr_env envs new_ev eq_rel xi1 xi1 xi2 xi2 }

----------------------------
-- Look for a canonical LHS. See Note [Canonical LHS].
-- Only rewritten types end up below here.
----------------------------

-- NB: pattern match on True: we want only rewritten types sent to canEqLHS
-- This means we've rewritten any variables and reduced any type family redexes
-- See also Note [No top-level newtypes on RHS of representational equalities]
can_eq_nc' True _rdr_env _envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | Just can_eq_lhs1 <- canEqLHS_maybe ty1
  = canEqCanLHS ev eq_rel NotSwapped can_eq_lhs1 ps_ty1 ty2 ps_ty2

  | Just can_eq_lhs2 <- canEqLHS_maybe ty2
  = canEqCanLHS ev eq_rel IsSwapped can_eq_lhs2 ps_ty2 ty1 ps_ty1

     -- If the type is TyConApp tc1 args1, then args1 really can't be less
     -- than tyConArity tc1. It could be *more* than tyConArity, but then we
     -- should have handled the case as an AppTy. That case only fires if
     -- _both_ sides of the equality are AppTy-like... but if one side is
     -- AppTy-like and the other isn't (and it also isn't a variable or
     -- saturated type family application, both of which are handled by
     -- can_eq_nc'), we're in a failure mode and can just fall through.

----------------------------
-- Fall-through. Give up.
----------------------------

-- We've rewritten and the types don't match. Give up.
can_eq_nc' True _rdr_env _envs ev eq_rel _ ps_ty1 _ ps_ty2
  = do { traceTcS "can_eq_nc' catch-all case" (ppr ps_ty1 $$ ppr ps_ty2)
       ; case eq_rel of -- See Note [Unsolved equalities]
            ReprEq -> continueWith (mkIrredCt OtherCIS ev)
            NomEq  -> continueWith (mkIrredCt InsolubleCIS ev) }
          -- No need to call canEqFailure/canEqHardFailure because they
          -- rewrite, and the types involved here are already rewritten

{- Note [Unsolved equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an unsolved equality like
  (a b ~R# Int)
that is not necessarily insoluble!  Maybe 'a' will turn out to be a newtype.
So we want to make it a potentially-soluble Irred not an insoluble one.
Missing this point is what caused #15431

Note [ForAllTy and typechecker equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Should GHC type-check the following program (adapted from #15740)?

  {-# LANGUAGE PolyKinds, ... #-}
  data D a
  type family F :: forall k. k -> Type
  type instance F = D

Due to the way F is declared, any instance of F must have a right-hand side
whose kind is equal to `forall k. k -> Type`. The kind of D is
`forall {k}. k -> Type`, which is very close, but technically uses distinct
Core:

  -----------------------------------------------------------
  | Source Haskell    | Core                                |
  -----------------------------------------------------------
  | forall  k.  <...> | ForAllTy (Bndr k Specified) (<...>) |
  | forall {k}. <...> | ForAllTy (Bndr k Inferred)  (<...>) |
  -----------------------------------------------------------

We could deem these kinds to be unequal, but that would imply rejecting
programs like the one above. Whether a kind variable binder ends up being
specified or inferred can be somewhat subtle, however, especially for kinds
that aren't explicitly written out in the source code (like in D above).
For now, we decide to not make the specified/inferred status of an invisible
type variable binder affect GHC's notion of typechecker equality
(see Note [Typechecker equality vs definitional equality] in
GHC.Tc.Utils.TcType). That is, we have the following:

  --------------------------------------------------
  | Type 1            | Type 2            | Equal? |
  --------------------|-----------------------------
  | forall k. <...>   | forall k. <...>   | Yes    |
  |                   | forall {k}. <...> | Yes    |
  |                   | forall k -> <...> | No     |
  --------------------------------------------------
  | forall {k}. <...> | forall k. <...>   | Yes    |
  |                   | forall {k}. <...> | Yes    |
  |                   | forall k -> <...> | No     |
  --------------------------------------------------
  | forall k -> <...> | forall k. <...>   | No     |
  |                   | forall {k}. <...> | No     |
  |                   | forall k -> <...> | Yes    |
  --------------------------------------------------

We implement this nuance by using the GHC.Types.Var.sameVis function in
GHC.Tc.Solver.Canonical.canEqNC and GHC.Tc.Utils.TcType.tcEqType, which
respect typechecker equality. sameVis puts both forms of invisible type
variable binders into the same equivalence class.

Note that we do /not/ use sameVis in GHC.Core.Type.eqType, which implements
/definitional/ equality, a slighty more coarse-grained notion of equality
(see Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep) that does
not consider the ArgFlag of ForAllTys at all. That is, eqType would equate all
of forall k. <...>, forall {k}. <...>, and forall k -> <...>.
-}

---------------------------------
can_eq_nc_forall :: CtEvidence -> EqRel
                 -> Type -> Type    -- LHS and RHS
                 -> TcS (StopOrContinue Ct)
-- (forall as. phi1) ~ (forall bs. phi2)
-- Check for length match of as, bs
-- Then build an implication constraint: forall as. phi1 ~ phi2[as/bs]
-- But remember also to unify the kinds of as and bs
--  (this is the 'go' loop), and actually substitute phi2[as |> cos / bs]
-- Remember also that we might have forall z (a:z). blah
--  so we must proceed one binder at a time (#13879)

can_eq_nc_forall ev eq_rel s1 s2
 | CtWanted { ctev_loc = loc, ctev_dest = orig_dest } <- ev
 = do { let free_tvs       = tyCoVarsOfTypes [s1,s2]
            (bndrs1, phi1) = tcSplitForAllTyVarBinders s1
            (bndrs2, phi2) = tcSplitForAllTyVarBinders s2
      ; if not (equalLength bndrs1 bndrs2)
        then do { traceTcS "Forall failure" $
                     vcat [ ppr s1, ppr s2, ppr bndrs1, ppr bndrs2
                          , ppr (map binderArgFlag bndrs1)
                          , ppr (map binderArgFlag bndrs2) ]
                ; canEqHardFailure ev s1 s2 }
        else
   do { traceTcS "Creating implication for polytype equality" $ ppr ev
      ; let empty_subst1 = mkEmptyTCvSubst $ mkInScopeSet free_tvs
      ; (subst1, skol_tvs) <- tcInstSkolTyVarsX empty_subst1 $
                              binderVars bndrs1

      ; let skol_info = UnifyForAllSkol phi1
            phi1' = substTy subst1 phi1

            -- Unify the kinds, extend the substitution
            go :: [TcTyVar] -> TCvSubst -> [TyVarBinder]
               -> TcS (TcCoercion, Cts)
            go (skol_tv:skol_tvs) subst (bndr2:bndrs2)
              = do { let tv2 = binderVar bndr2
                   ; (kind_co, wanteds1) <- unify loc Nominal (tyVarKind skol_tv)
                                                  (substTy subst (tyVarKind tv2))
                   ; let subst' = extendTvSubstAndInScope subst tv2
                                       (mkCastTy (mkTyVarTy skol_tv) kind_co)
                         -- skol_tv is already in the in-scope set, but the
                         -- free vars of kind_co are not; hence "...AndInScope"
                   ; (co, wanteds2) <- go skol_tvs subst' bndrs2
                   ; return ( mkTcForAllCo skol_tv kind_co co
                            , wanteds1 `unionBags` wanteds2 ) }

            -- Done: unify phi1 ~ phi2
            go [] subst bndrs2
              = ASSERT( null bndrs2 )
                unify loc (eqRelRole eq_rel) phi1' (substTyUnchecked subst phi2)

            go _ _ _ = panic "cna_eq_nc_forall"  -- case (s:ss) []

            empty_subst2 = mkEmptyTCvSubst (getTCvInScope subst1)

      ; (lvl, (all_co, wanteds)) <- pushLevelNoWorkList (ppr skol_info) $
                                    go skol_tvs empty_subst2 bndrs2
      ; emitTvImplicationTcS lvl skol_info skol_tvs wanteds

      ; setWantedEq orig_dest all_co
      ; stopWith ev "Deferred polytype equality" } }

 | otherwise
 = do { traceTcS "Omitting decomposition of given polytype equality" $
        pprEq s1 s2    -- See Note [Do not decompose given polytype equalities]
      ; stopWith ev "Discard given polytype equality" }

 where
    unify :: CtLoc -> Role -> TcType -> TcType -> TcS (TcCoercion, Cts)
    -- This version returns the wanted constraint rather
    -- than putting it in the work list
    unify loc role ty1 ty2
      | ty1 `tcEqType` ty2
      = return (mkTcReflCo role ty1, emptyBag)
      | otherwise
      = do { (wanted, co) <- newWantedEq loc role ty1 ty2
           ; return (co, unitBag (mkNonCanonical wanted)) }

---------------------------------
-- | Compare types for equality, while zonking as necessary. Gives up
-- as soon as it finds that two types are not equal.
-- This is quite handy when some unification has made two
-- types in an inert Wanted to be equal. We can discover the equality without
-- rewriting, which is sometimes very expensive (in the case of type functions).
-- In particular, this function makes a ~20% improvement in test case
-- perf/compiler/T5030.
--
-- Returns either the (partially zonked) types in the case of
-- inequality, or the one type in the case of equality. canEqReflexive is
-- a good next step in the 'Right' case. Returning 'Left' is always safe.
--
-- NB: This does *not* look through type synonyms. In fact, it treats type
-- synonyms as rigid constructors. In the future, it might be convenient
-- to look at only those arguments of type synonyms that actually appear
-- in the synonym RHS. But we're not there yet.
zonk_eq_types :: TcType -> TcType -> TcS (Either (Pair TcType) TcType)
zonk_eq_types = go
  where
    go (TyVarTy tv1) (TyVarTy tv2) = tyvar_tyvar tv1 tv2
    go (TyVarTy tv1) ty2           = tyvar NotSwapped tv1 ty2
    go ty1 (TyVarTy tv2)           = tyvar IsSwapped  tv2 ty1

    -- We handle FunTys explicitly here despite the fact that they could also be
    -- treated as an application. Why? Well, for one it's cheaper to just look
    -- at two types (the argument and result types) than four (the argument,
    -- result, and their RuntimeReps). Also, we haven't completely zonked yet,
    -- so we may run into an unzonked type variable while trying to compute the
    -- RuntimeReps of the argument and result types. This can be observed in
    -- testcase tc269.
    go ty1 ty2
      | Just (Scaled w1 arg1, res1) <- split1
      , Just (Scaled w2 arg2, res2) <- split2
      , eqType w1 w2
      = do { res_a <- go arg1 arg2
           ; res_b <- go res1 res2
           ; return $ combine_rev (mkVisFunTy w1) res_b res_a
           }
      | isJust split1 || isJust split2
      = bale_out ty1 ty2
      where
        split1 = tcSplitFunTy_maybe ty1
        split2 = tcSplitFunTy_maybe ty2

    go ty1 ty2
      | Just (tc1, tys1) <- repSplitTyConApp_maybe ty1
      , Just (tc2, tys2) <- repSplitTyConApp_maybe ty2
      = if tc1 == tc2 && tys1 `equalLength` tys2
          -- Crucial to check for equal-length args, because
          -- we cannot assume that the two args to 'go' have
          -- the same kind.  E.g go (Proxy *      (Maybe Int))
          --                        (Proxy (*->*) Maybe)
          -- We'll call (go (Maybe Int) Maybe)
          -- See #13083
        then tycon tc1 tys1 tys2
        else bale_out ty1 ty2

    go ty1 ty2
      | Just (ty1a, ty1b) <- tcRepSplitAppTy_maybe ty1
      , Just (ty2a, ty2b) <- tcRepSplitAppTy_maybe ty2
      = do { res_a <- go ty1a ty2a
           ; res_b <- go ty1b ty2b
           ; return $ combine_rev mkAppTy res_b res_a }

    go ty1@(LitTy lit1) (LitTy lit2)
      | lit1 == lit2
      = return (Right ty1)

    go ty1 ty2 = bale_out ty1 ty2
      -- We don't handle more complex forms here

    bale_out ty1 ty2 = return $ Left (Pair ty1 ty2)

    tyvar :: SwapFlag -> TcTyVar -> TcType
          -> TcS (Either (Pair TcType) TcType)
      -- Try to do as little as possible, as anything we do here is redundant
      -- with rewriting. In particular, no need to zonk kinds. That's why
      -- we don't use the already-defined zonking functions
    tyvar swapped tv ty
      = case tcTyVarDetails tv of
          MetaTv { mtv_ref = ref }
            -> do { cts <- readTcRef ref
                  ; case cts of
                      Flexi        -> give_up
                      Indirect ty' -> do { trace_indirect tv ty'
                                         ; unSwap swapped go ty' ty } }
          _ -> give_up
      where
        give_up = return $ Left $ unSwap swapped Pair (mkTyVarTy tv) ty

    tyvar_tyvar tv1 tv2
      | tv1 == tv2 = return (Right (mkTyVarTy tv1))
      | otherwise  = do { (ty1', progress1) <- quick_zonk tv1
                        ; (ty2', progress2) <- quick_zonk tv2
                        ; if progress1 || progress2
                          then go ty1' ty2'
                          else return $ Left (Pair (TyVarTy tv1) (TyVarTy tv2)) }

    trace_indirect tv ty
       = traceTcS "Following filled tyvar (zonk_eq_types)"
                  (ppr tv <+> equals <+> ppr ty)

    quick_zonk tv = case tcTyVarDetails tv of
      MetaTv { mtv_ref = ref }
        -> do { cts <- readTcRef ref
              ; case cts of
                  Flexi        -> return (TyVarTy tv, False)
                  Indirect ty' -> do { trace_indirect tv ty'
                                     ; return (ty', True) } }
      _ -> return (TyVarTy tv, False)

      -- This happens for type families, too. But recall that failure
      -- here just means to try harder, so it's OK if the type function
      -- isn't injective.
    tycon :: TyCon -> [TcType] -> [TcType]
          -> TcS (Either (Pair TcType) TcType)
    tycon tc tys1 tys2
      = do { results <- zipWithM go tys1 tys2
           ; return $ case combine_results results of
               Left tys  -> Left (mkTyConApp tc <$> tys)
               Right tys -> Right (mkTyConApp tc tys) }

    combine_results :: [Either (Pair TcType) TcType]
                    -> Either (Pair [TcType]) [TcType]
    combine_results = bimap (fmap reverse) reverse .
                      foldl' (combine_rev (:)) (Right [])

      -- combine (in reverse) a new result onto an already-combined result
    combine_rev :: (a -> b -> c)
                -> Either (Pair b) b
                -> Either (Pair a) a
                -> Either (Pair c) c
    combine_rev f (Left list) (Left elt) = Left (f <$> elt     <*> list)
    combine_rev f (Left list) (Right ty) = Left (f <$> pure ty <*> list)
    combine_rev f (Right tys) (Left elt) = Left (f <$> elt     <*> pure tys)
    combine_rev f (Right tys) (Right ty) = Right (f ty tys)

{- See Note [Unwrap newtypes first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  newtype N m a = MkN (m a)
Then N will get a conservative, Nominal role for its second parameter 'a',
because it appears as an argument to the unknown 'm'. Now consider
  [W] N Maybe a  ~R#  N Maybe b

If we decompose, we'll get
  [W] a ~N# b

But if instead we unwrap we'll get
  [W] Maybe a ~R# Maybe b
which in turn gives us
  [W] a ~R# b
which is easier to satisfy.

Bottom line: unwrap newtypes before decomposing them!
c.f. #9123 comment:52,53 for a compelling example.

Note [Newtypes can blow the stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype X = MkX (Int -> X)
  newtype Y = MkY (Int -> Y)

and now wish to prove

  [W] X ~R Y

This Wanted will loop, expanding out the newtypes ever deeper looking
for a solid match or a solid discrepancy. Indeed, there is something
appropriate to this looping, because X and Y *do* have the same representation,
in the limit -- they're both (Fix ((->) Int)). However, no finitely-sized
coercion will ever witness it. This loop won't actually cause GHC to hang,
though, because we check our depth when unwrapping newtypes.

Note [Eager reflexivity check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype X = MkX (Int -> X)

and

  [W] X ~R X

Naively, we would start unwrapping X and end up in a loop. Instead,
we do this eager reflexivity check. This is necessary only for representational
equality because the rewriter technology deals with the similar case
(recursive type families) for nominal equality.

Note that this check does not catch all cases, but it will catch the cases
we're most worried about, types like X above that are actually inhabited.

Here's another place where this reflexivity check is key:
Consider trying to prove (f a) ~R (f a). The AppTys in there can't
be decomposed, because representational equality isn't congruent with respect
to AppTy. So, when canonicalising the equality above, we get stuck and
would normally produce a CIrredCan. However, we really do want to
be able to solve (f a) ~R (f a). So, in the representational case only,
we do a reflexivity check.

(This would be sound in the nominal case, but unnecessary, and I [Richard
E.] am worried that it would slow down the common case.)
-}

------------------------
-- | We're able to unwrap a newtype. Update the bits accordingly.
can_eq_newtype_nc :: CtEvidence           -- ^ :: ty1 ~ ty2
                  -> SwapFlag
                  -> TcType                                    -- ^ ty1
                  -> ((Bag GlobalRdrElt, TcCoercion), TcType)  -- ^ :: ty1 ~ ty1'
                  -> TcType               -- ^ ty2
                  -> TcType               -- ^ ty2, with type synonyms
                  -> TcS (StopOrContinue Ct)
can_eq_newtype_nc ev swapped ty1 ((gres, co), ty1') ty2 ps_ty2
  = do { traceTcS "can_eq_newtype_nc" $
         vcat [ ppr ev, ppr swapped, ppr co, ppr gres, ppr ty1', ppr ty2 ]

         -- check for blowing our stack:
         -- See Note [Newtypes can blow the stack]
       ; checkReductionDepth (ctEvLoc ev) ty1

         -- Next, we record uses of newtype constructors, since coercing
         -- through newtypes is tantamount to using their constructors.
       ; addUsedGREs gre_list
         -- If a newtype constructor was imported, don't warn about not
         -- importing it...
       ; traverse_ keepAlive $ map greMangledName gre_list
         -- ...and similarly, if a newtype constructor was defined in the same
         -- module, don't warn about it being unused.
         -- See Note [Tracking unused binding and imports] in GHC.Tc.Utils.

       ; new_ev <- rewriteEqEvidence ev swapped ty1' ps_ty2
                                     (mkTcSymCo co) (mkTcReflCo Representational ps_ty2)
       ; can_eq_nc False new_ev ReprEq ty1' ty1' ty2 ps_ty2 }
  where
    gre_list = bagToList gres

---------
-- ^ Decompose a type application.
-- All input types must be rewritten. See Note [Canonicalising type applications]
-- Nominal equality only!
can_eq_app :: CtEvidence       -- :: s1 t1 ~N s2 t2
           -> Xi -> Xi         -- s1 t1
           -> Xi -> Xi         -- s2 t2
           -> TcS (StopOrContinue Ct)

-- AppTys only decompose for nominal equality, so this case just leads
-- to an irreducible constraint; see typecheck/should_compile/T10494
-- See Note [Decomposing AppTy at representational role]
can_eq_app ev s1 t1 s2 t2
  | CtDerived {} <- ev
  = do { unifyDeriveds loc [Nominal, Nominal] [s1, t1] [s2, t2]
       ; stopWith ev "Decomposed [D] AppTy" }

  | CtWanted { ctev_dest = dest } <- ev
  = do { co_s <- unifyWanted loc Nominal s1 s2
       ; let arg_loc
               | isNextArgVisible s1 = loc
               | otherwise           = updateCtLocOrigin loc toInvisibleOrigin
       ; co_t <- unifyWanted arg_loc Nominal t1 t2
       ; let co = mkAppCo co_s co_t
       ; setWantedEq dest co
       ; stopWith ev "Decomposed [W] AppTy" }

    -- If there is a ForAll/(->) mismatch, the use of the Left coercion
    -- below is ill-typed, potentially leading to a panic in splitTyConApp
    -- Test case: typecheck/should_run/Typeable1
    -- We could also include this mismatch check above (for W and D), but it's slow
    -- and we'll get a better error message not doing it
  | s1k `mismatches` s2k
  = canEqHardFailure ev (s1 `mkAppTy` t1) (s2 `mkAppTy` t2)

  | CtGiven { ctev_evar = evar } <- ev
  = do { let co   = mkTcCoVarCo evar
             co_s = mkTcLRCo CLeft  co
             co_t = mkTcLRCo CRight co
       ; evar_s <- newGivenEvVar loc ( mkTcEqPredLikeEv ev s1 s2
                                     , evCoercion co_s )
       ; evar_t <- newGivenEvVar loc ( mkTcEqPredLikeEv ev t1 t2
                                     , evCoercion co_t )
       ; emitWorkNC [evar_t]
       ; canEqNC evar_s NomEq s1 s2 }

  where
    loc = ctEvLoc ev

    s1k = tcTypeKind s1
    s2k = tcTypeKind s2

    k1 `mismatches` k2
      =  isForAllTy k1 && not (isForAllTy k2)
      || not (isForAllTy k1) && isForAllTy k2

-----------------------
-- | Break apart an equality over a casted type
-- looking like   (ty1 |> co1) ~ ty2   (modulo a swap-flag)
canEqCast :: Bool         -- are both types rewritten?
          -> CtEvidence
          -> EqRel
          -> SwapFlag
          -> TcType -> Coercion   -- LHS (res. RHS), ty1 |> co1
          -> TcType -> TcType     -- RHS (res. LHS), ty2 both normal and pretty
          -> TcS (StopOrContinue Ct)
canEqCast rewritten ev eq_rel swapped ty1 co1 ty2 ps_ty2
  = do { traceTcS "Decomposing cast" (vcat [ ppr ev
                                           , ppr ty1 <+> text "|>" <+> ppr co1
                                           , ppr ps_ty2 ])
       ; new_ev <- rewriteEqEvidence ev swapped ty1 ps_ty2
                                     (mkTcGReflRightCo role ty1 co1)
                                     (mkTcReflCo role ps_ty2)
       ; can_eq_nc rewritten new_ev eq_rel ty1 ty1 ty2 ps_ty2 }
  where
    role = eqRelRole eq_rel

------------------------
canTyConApp :: CtEvidence -> EqRel
            -> TyCon -> [TcType]
            -> TyCon -> [TcType]
            -> TcS (StopOrContinue Ct)
-- See Note [Decomposing TyConApps]
-- Neither tc1 nor tc2 is a saturated funTyCon
canTyConApp ev eq_rel tc1 tys1 tc2 tys2
  | tc1 == tc2
  , tys1 `equalLength` tys2
  = do { inerts <- getTcSInerts
       ; if can_decompose inerts
         then canDecomposableTyConAppOK ev eq_rel tc1 tys1 tys2
         else canEqFailure ev eq_rel ty1 ty2 }

  -- See Note [Skolem abstract data] (at tyConSkolem)
  | tyConSkolem tc1 || tyConSkolem tc2
  = do { traceTcS "canTyConApp: skolem abstract" (ppr tc1 $$ ppr tc2)
       ; continueWith (mkIrredCt OtherCIS ev) }

  -- Fail straight away for better error messages
  -- See Note [Use canEqFailure in canDecomposableTyConApp]
  | eq_rel == ReprEq && not (isGenerativeTyCon tc1 Representational &&
                             isGenerativeTyCon tc2 Representational)
  = canEqFailure ev eq_rel ty1 ty2
  | otherwise
  = canEqHardFailure ev ty1 ty2
  where
    -- Reconstruct the types for error messages. This would do
    -- the wrong thing (from a pretty printing point of view)
    -- for functions, because we've lost the AnonArgFlag; but
    -- in fact we never call canTyConApp on a saturated FunTyCon
    ty1 = mkTyConApp tc1 tys1
    ty2 = mkTyConApp tc2 tys2

    loc  = ctEvLoc ev
    pred = ctEvPred ev

     -- See Note [Decomposing equality]
    can_decompose inerts
      =  isInjectiveTyCon tc1 (eqRelRole eq_rel)
      || (ctEvFlavour ev /= Given && isEmptyBag (matchableGivens loc pred inerts))

{-
Note [Use canEqFailure in canDecomposableTyConApp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must use canEqFailure, not canEqHardFailure here, because there is
the possibility of success if working with a representational equality.
Here is one case:

  type family TF a where TF Char = Bool
  data family DF a
  newtype instance DF Bool = MkDF Int

Suppose we are canonicalising (Int ~R DF (TF a)), where we don't yet
know `a`. This is *not* a hard failure, because we might soon learn
that `a` is, in fact, Char, and then the equality succeeds.

Here is another case:

  [G] Age ~R Int

where Age's constructor is not in scope. We don't want to report
an "inaccessible code" error in the context of this Given!

For example, see typecheck/should_compile/T10493, repeated here:

  import Data.Ord (Down)  -- no constructor

  foo :: Coercible (Down Int) Int => Down Int -> Int
  foo = coerce

That should compile, but only because we use canEqFailure and not
canEqHardFailure.

Note [Decomposing equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a constraint (of any flavour and role) that looks like
T tys1 ~ T tys2, what can we conclude about tys1 and tys2? The answer,
of course, is "it depends". This Note spells it all out.

In this Note, "decomposition" refers to taking the constraint
  [fl] (T tys1 ~X T tys2)
(for some flavour fl and some role X) and replacing it with
  [fls'] (tys1 ~Xs' tys2)
where that notation indicates a list of new constraints, where the
new constraints may have different flavours and different roles.

The key property to consider is injectivity. When decomposing a Given, the
decomposition is sound if and only if T is injective in all of its type
arguments. When decomposing a Wanted, the decomposition is sound (assuming the
correct roles in the produced equality constraints), but it may be a guess --
that is, an unforced decision by the constraint solver. Decomposing Wanteds
over injective TyCons does not entail guessing. But sometimes we want to
decompose a Wanted even when the TyCon involved is not injective! (See below.)

So, in broad strokes, we want this rule:

(*) Decompose a constraint (T tys1 ~X T tys2) if and only if T is injective
at role X.

Pursuing the details requires exploring three axes:
* Flavour: Given vs. Derived vs. Wanted
* Role: Nominal vs. Representational
* TyCon species: datatype vs. newtype vs. data family vs. type family vs. type variable

(A type variable isn't a TyCon, of course, but it's convenient to put the AppTy case
in the same table.)

Right away, we can say that Derived behaves just as Wanted for the purposes
of decomposition. The difference between Derived and Wanted is the handling of
evidence. Since decomposition in these cases isn't a matter of soundness but of
guessing, we want the same behaviour regardless of evidence.

Here is a table (discussion following) detailing where decomposition of
   (T s1 ... sn) ~r (T t1 .. tn)
is allowed.  The first four lines (Data types ... type family) refer
to TyConApps with various TyCons T; the last line is for AppTy, covering
both where there is a type variable at the head and the case for an over-
saturated type family.

NOMINAL               GIVEN        WANTED                         WHERE

Datatype               YES          YES                           canTyConApp
Newtype                YES          YES                           canTyConApp
Data family            YES          YES                           canTyConApp
Type family            NO{1}        YES, in injective args{1}     canEqCanLHS2
AppTy                  YES          YES                           can_eq_app

REPRESENTATIONAL      GIVEN        WANTED

Datatype               YES          YES                           canTyConApp
Newtype                NO{2}       MAYBE{2}                canTyConApp(can_decompose)
Data family            NO{3}       MAYBE{3}                canTyConApp(can_decompose)
Type family            NO           NO                            canEqCanLHS2
AppTy                  NO{4}        NO{4}                         can_eq_nc'

{1}: Type families can be injective in some, but not all, of their arguments,
so we want to do partial decomposition. This is quite different than the way
other decomposition is done, where the decomposed equalities replace the original
one. We thus proceed much like we do with superclasses, emitting new Deriveds
when "decomposing" a partially-injective type family Wanted. Injective type
families have no corresponding evidence of their injectivity, so we cannot
decompose an injective-type-family Given.

{2}: See Note [Decomposing newtypes at representational role]

{3}: Because of the possibility of newtype instances, we must treat
data families like newtypes. See also
Note [Decomposing newtypes at representational role]. See #10534 and
test case typecheck/should_fail/T10534.

{4}: See Note [Decomposing AppTy at representational role]

In the implementation of can_eq_nc and friends, we don't directly pattern
match using lines like in the tables above, as those tables don't cover
all cases (what about PrimTyCon? tuples?). Instead we just ask about injectivity,
boiling the tables above down to rule (*). The exceptions to rule (*) are for
injective type families, which are handled separately from other decompositions,
and the MAYBE entries above.

Note [Decomposing newtypes at representational role]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This note discusses the 'newtype' line in the REPRESENTATIONAL table
in Note [Decomposing equality]. (At nominal role, newtypes are fully
decomposable.)

Here is a representative example of why representational equality over
newtypes is tricky:

  newtype Nt a = Mk Bool         -- NB: a is not used in the RHS,
  type role Nt representational  -- but the user gives it an R role anyway

If we have [W] Nt alpha ~R Nt beta, we *don't* want to decompose to
[W] alpha ~R beta, because it's possible that alpha and beta aren't
representationally equal. Here's another example.

  newtype Nt a = MkNt (Id a)
  type family Id a where Id a = a

  [W] Nt Int ~R Nt Age

Because of its use of a type family, Nt's parameter will get inferred to have
a nominal role. Thus, decomposing the wanted will yield [W] Int ~N Age, which
is unsatisfiable. Unwrapping, though, leads to a solution.

Conclusion:
 * Unwrap newtypes before attempting to decompose them.
   This is done in can_eq_nc'.

It all comes from the fact that newtypes aren't necessarily injective
w.r.t. representational equality.

Furthermore, as explained in Note [NthCo and newtypes] in GHC.Core.TyCo.Rep, we can't use
NthCo on representational coercions over newtypes. NthCo comes into play
only when decomposing givens.

Conclusion:
 * Do not decompose [G] N s ~R N t

Is it sensible to decompose *Wanted* constraints over newtypes?  Yes!
It's the only way we could ever prove (IO Int ~R IO Age), recalling
that IO is a newtype.

However we must be careful.  Consider

  type role Nt representational

  [G] Nt a ~R Nt b       (1)
  [W] NT alpha ~R Nt b   (2)
  [W] alpha ~ a          (3)

If we focus on (3) first, we'll substitute in (2), and now it's
identical to the given (1), so we succeed.  But if we focus on (2)
first, and decompose it, we'll get (alpha ~R b), which is not soluble.
This is exactly like the question of overlapping Givens for class
constraints: see Note [Instance and Given overlap] in GHC.Tc.Solver.Interact.

Conclusion:
  * Decompose [W] N s ~R N t  iff there no given constraint that could
    later solve it.

Note [Decomposing AppTy at representational role]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We never decompose AppTy at a representational role. For Givens, doing
so is simply unsound: the LRCo coercion former requires a nominal-roled
arguments. (See (1) for an example of why.) For Wanteds, decomposing
would be sound, but it would be a guess, and a non-confluent one at that.

Here is an example:

    [G] g1 :: a ~R b
    [W] w1 :: Maybe b ~R alpha a
    [W] w2 :: alpha ~ Maybe

Suppose we see w1 before w2. If we were to decompose, we would decompose
this to become

    [W] w3 :: Maybe ~R alpha
    [W] w4 :: b ~ a

Note that w4 is *nominal*. A nominal role here is necessary because AppCo
requires a nominal role on its second argument. (See (2) for an example of
why.) If we decomposed w1 to w3,w4, we would then get stuck, because w4
is insoluble. On the other hand, if we see w2 first, setting alpha := Maybe,
all is well, as we can decompose Maybe b ~R Maybe a into b ~R a.

Another example:

    newtype Phant x = MkPhant Int

    [W] w1 :: Phant Int ~R alpha Bool
    [W] w2 :: alpha ~ Phant

If we see w1 first, decomposing would be disastrous, as we would then try
to solve Int ~ Bool. Instead, spotting w2 allows us to simplify w1 to become

    [W] w1' :: Phant Int ~R Phant Bool

which can then (assuming MkPhant is in scope) be simplified to Int ~R Int,
and all will be well. See also Note [Unwrap newtypes first].

Bottom line: never decompose AppTy with representational roles.

(1) Decomposing a Given AppTy over a representational role is simply
unsound. For example, if we have co1 :: Phant Int ~R a Bool (for
the newtype Phant, above), then we surely don't want any relationship
between Int and Bool, lest we also have co2 :: Phant ~ a around.

(2) The role on the AppCo coercion is a conservative choice, because we don't
know the role signature of the function. For example, let's assume we could
have a representational role on the second argument of AppCo. Then, consider

    data G a where    -- G will have a nominal role, as G is a GADT
      MkG :: G Int
    newtype Age = MkAge Int

    co1 :: G ~R a        -- by assumption
    co2 :: Age ~R Int    -- by newtype axiom
    co3 = AppCo co1 co2 :: G Age ~R a Int    -- by our broken AppCo

and now co3 can be used to cast MkG to have type G Age, in violation of
the way GADTs are supposed to work (which is to use nominal equality).

-}

canDecomposableTyConAppOK :: CtEvidence -> EqRel
                          -> TyCon -> [TcType] -> [TcType]
                          -> TcS (StopOrContinue Ct)
-- Precondition: tys1 and tys2 are the same length, hence "OK"
canDecomposableTyConAppOK ev eq_rel tc tys1 tys2
  = ASSERT( tys1 `equalLength` tys2 )
    do { traceTcS "canDecomposableTyConAppOK"
                  (ppr ev $$ ppr eq_rel $$ ppr tc $$ ppr tys1 $$ ppr tys2)
       ; case ev of
           CtDerived {}
             -> unifyDeriveds loc tc_roles tys1 tys2

           CtWanted { ctev_dest = dest }
                  -- new_locs and tc_roles are both infinite, so
                  -- we are guaranteed that cos has the same length
                  -- as tys1 and tys2
             -> do { cos <- zipWith4M unifyWanted new_locs tc_roles tys1 tys2
                   ; setWantedEq dest (mkTyConAppCo role tc cos) }

           CtGiven { ctev_evar = evar }
             -> do { let ev_co = mkCoVarCo evar
                   ; given_evs <- newGivenEvVars loc $
                                  [ ( mkPrimEqPredRole r ty1 ty2
                                    , evCoercion $ mkNthCo r i ev_co )
                                  | (r, ty1, ty2, i) <- zip4 tc_roles tys1 tys2 [0..]
                                  , r /= Phantom
                                  , not (isCoercionTy ty1) && not (isCoercionTy ty2) ]
                   ; emitWorkNC given_evs }

    ; stopWith ev "Decomposed TyConApp" }

  where
    loc        = ctEvLoc ev
    role       = eqRelRole eq_rel

      -- infinite, as tyConRolesX returns an infinite tail of Nominal
    tc_roles   = tyConRolesX role tc

      -- Add nuances to the location during decomposition:
      --  * if the argument is a kind argument, remember this, so that error
      --    messages say "kind", not "type". This is determined based on whether
      --    the corresponding tyConBinder is named (that is, dependent)
      --  * if the argument is invisible, note this as well, again by
      --    looking at the corresponding binder
      -- For oversaturated tycons, we need the (repeat loc) tail, which doesn't
      -- do either of these changes. (Forgetting to do so led to #16188)
      --
      -- NB: infinite in length
    new_locs = [ new_loc
               | bndr <- tyConBinders tc
               , let new_loc0 | isNamedTyConBinder bndr = toKindLoc loc
                              | otherwise               = loc
                     new_loc  | isInvisibleTyConBinder bndr
                              = updateCtLocOrigin new_loc0 toInvisibleOrigin
                              | otherwise
                              = new_loc0 ]
               ++ repeat loc

-- | Call when canonicalizing an equality fails, but if the equality is
-- representational, there is some hope for the future.
-- Examples in Note [Use canEqFailure in canDecomposableTyConApp]
canEqFailure :: CtEvidence -> EqRel
             -> TcType -> TcType -> TcS (StopOrContinue Ct)
canEqFailure ev NomEq ty1 ty2
  = canEqHardFailure ev ty1 ty2
canEqFailure ev ReprEq ty1 ty2
  = do { (xi1, co1) <- rewrite ev ty1
       ; (xi2, co2) <- rewrite ev ty2
            -- We must rewrite the types before putting them in the
            -- inert set, so that we are sure to kick them out when
            -- new equalities become available
       ; traceTcS "canEqFailure with ReprEq" $
         vcat [ ppr ev, ppr ty1, ppr ty2, ppr xi1, ppr xi2 ]
       ; new_ev <- rewriteEqEvidence ev NotSwapped xi1 xi2 co1 co2
       ; continueWith (mkIrredCt OtherCIS new_ev) }

-- | Call when canonicalizing an equality fails with utterly no hope.
canEqHardFailure :: CtEvidence
                 -> TcType -> TcType -> TcS (StopOrContinue Ct)
-- See Note [Make sure that insolubles are fully rewritten]
canEqHardFailure ev ty1 ty2
  = do { traceTcS "canEqHardFailure" (ppr ty1 $$ ppr ty2)
       ; (s1, co1) <- rewrite ev ty1
       ; (s2, co2) <- rewrite ev ty2
       ; new_ev <- rewriteEqEvidence ev NotSwapped s1 s2 co1 co2
       ; continueWith (mkIrredCt InsolubleCIS new_ev) }

{-
Note [Decomposing TyConApps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see (T s1 t1 ~ T s2 t2), then we can just decompose to
  (s1 ~ s2, t1 ~ t2)
and push those back into the work list.  But if
  s1 = K k1    s2 = K k2
then we will just decomopose s1~s2, and it might be better to
do so on the spot.  An important special case is where s1=s2,
and we get just Refl.

So canDecomposableTyCon is a fast-path decomposition that uses
unifyWanted etc to short-cut that work.

Note [Canonicalising type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (s1 t1) ~ ty2, how should we proceed?
The simple thing is to see if ty2 is of form (s2 t2), and
decompose.

However, over-eager decomposition gives bad error messages
for things like
   a b ~ Maybe c
   e f ~ p -> q
Suppose (in the first example) we already know a~Array.  Then if we
decompose the application eagerly, yielding
   a ~ Maybe
   b ~ c
we get an error        "Can't match Array ~ Maybe",
but we'd prefer to get "Can't match Array b ~ Maybe c".

So instead can_eq_wanted_app rewrites the LHS and RHS, in the hope of
replacing (a b) by (Array b), before using try_decompose_app to
decompose it.

Note [Make sure that insolubles are fully rewritten]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an equality fails, we still want to rewrite the equality
all the way down, so that it accurately reflects
 (a) the mutable reference substitution in force at start of solving
 (b) any ty-binds in force at this point in solving
See Note [Rewrite insolubles] in GHC.Tc.Solver.Monad.
And if we don't do this there is a bad danger that
GHC.Tc.Solver.applyTyVarDefaulting will find a variable
that has in fact been substituted.

Note [Do not decompose Given polytype equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] (forall a. t1 ~ forall a. t2).  Can we decompose this?
No -- what would the evidence look like?  So instead we simply discard
this given evidence.


Note [Combining insoluble constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As this point we have an insoluble constraint, like Int~Bool.

 * If it is Wanted, delete it from the cache, so that subsequent
   Int~Bool constraints give rise to separate error messages

 * But if it is Derived, DO NOT delete from cache.  A class constraint
   may get kicked out of the inert set, and then have its functional
   dependency Derived constraints generated a second time. In that
   case we don't want to get two (or more) error messages by
   generating two (or more) insoluble fundep constraints from the same
   class constraint.

Note [No top-level newtypes on RHS of representational equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we're in this situation:

 work item:  [W] c1 : a ~R b
     inert:  [G] c2 : b ~R Id a

where
  newtype Id a = Id a

We want to make sure canEqCanLHS sees [W] a ~R a, after b is rewritten
and the Id newtype is unwrapped. This is assured by requiring only rewritten
types in canEqCanLHS *and* having the newtype-unwrapping check above
the tyvar check in can_eq_nc.

Note [Occurs check error]
~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an occurs check error, are we necessarily hosed? Say our
tyvar is tv1 and the type it appears in is xi2. Because xi2 is function
free, then if we're computing w.r.t. nominal equality, then, yes, we're
hosed. Nothing good can come from (a ~ [a]). If we're computing w.r.t.
representational equality, this is a little subtler. Once again, (a ~R [a])
is a bad thing, but (a ~R N a) for a newtype N might be just fine. This
means also that (a ~ b a) might be fine, because `b` might become a newtype.

So, we must check: does tv1 appear in xi2 under any type constructor
that is generative w.r.t. representational equality? That's what
isInsolubleOccursCheck does.

See also #10715, which induced this addition.

Note [Put touchable variables on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticket #10009, a very nasty example:

    f :: (UnF (F b) ~ b) => F b -> ()

    g :: forall a. (UnF (F a) ~ a) => a -> ()
    g _ = f (undefined :: F a)

For g we get [G]  g1 : UnF (F a) ~ a
             [WD] w1 : UnF (F beta) ~ beta
             [WD] w2 : F a ~ F beta

g1 is canonical (CEqCan). It is oriented as above because a is not touchable.
See canEqTyVarFunEq.

w1 is similarly canonical, though the occurs-check in canEqTyVarFunEq is key
here.

w2 is canonical. But which way should it be oriented? As written, we'll be
stuck. When w2 is added to the inert set, nothing gets kicked out: g1 is
a Given (and Wanteds don't rewrite Givens), and w2 doesn't mention the LHS
of w2. We'll thus lose.

But if w2 is swapped around, to

    [D] w3 : F beta ~ F a

then (after emitting shadow Deriveds, etc. See GHC.Tc.Solver.Monad
Note [The improvement story and derived shadows]) we'll kick w1 out of the inert
set (it mentions the LHS of w3). We then rewrite w1 to

    [D] w4 : UnF (F a) ~ beta

and then, using g1, to

    [D] w5 : a ~ beta

at which point we can unify and go on to glory. (This rewriting actually
happens all at once, in the call to rewrite during canonicalisation.)

But what about the new LHS makes it better? It mentions a variable (beta)
that can appear in a Wanted -- a touchable metavariable never appears
in a Given. On the other hand, the original LHS mentioned only variables
that appear in Givens. We thus choose to put variables that can appear
in Wanteds on the left.

Ticket #12526 is another good example of this in action.

-}

---------------------
canEqCanLHS :: CtEvidence          -- ev :: lhs ~ rhs
            -> EqRel -> SwapFlag
            -> CanEqLHS              -- lhs (or, if swapped, rhs)
            -> TcType                -- lhs: pretty lhs, already rewritten
            -> TcType -> TcType      -- rhs: already rewritten
            -> TcS (StopOrContinue Ct)
canEqCanLHS ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2
  | k1 `tcEqType` k2
  = canEqCanLHSHomo ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2

  | otherwise
  = canEqCanLHSHetero ev eq_rel swapped lhs1 ps_xi1 k1 xi2 ps_xi2 k2

  where
    k1 = canEqLHSKind lhs1
    k2 = tcTypeKind xi2

canEqCanLHSHetero :: CtEvidence         -- :: (xi1 :: ki1) ~ (xi2 :: ki2)
                  -> EqRel -> SwapFlag
                  -> CanEqLHS -> TcType -- xi1, pretty xi1
                  -> TcKind             -- ki1
                  -> TcType -> TcType   -- xi2, pretty xi2 :: ki2
                  -> TcKind             -- ki2
                  -> TcS (StopOrContinue Ct)
canEqCanLHSHetero ev eq_rel swapped lhs1 ps_xi1 ki1 xi2 ps_xi2 ki2
  -- See Note [Equalities with incompatible kinds]
  = do { kind_co <- emit_kind_co   -- :: ki2 ~N ki1

       ; let  -- kind_co :: (ki2 :: *) ~N (ki1 :: *)   (whether swapped or not)
              -- co1     :: kind(tv1) ~N ki1
             rhs'    = xi2    `mkCastTy` kind_co   -- :: ki1
             ps_rhs' = ps_xi2 `mkCastTy` kind_co   -- :: ki1
             rhs_co  = mkTcGReflLeftCo role xi2 kind_co
               -- rhs_co :: (xi2 |> kind_co) ~ xi2

             lhs_co = mkTcReflCo role xi1

       ; traceTcS "Hetero equality gives rise to kind equality"
           (ppr kind_co <+> dcolon <+> sep [ ppr ki2, text "~#", ppr ki1 ])
       ; type_ev <- rewriteEqEvidence ev swapped xi1 rhs' lhs_co rhs_co

          -- rewriteEqEvidence carries out the swap, so we're NotSwapped any more
       ; canEqCanLHSHomo type_ev eq_rel NotSwapped lhs1 ps_xi1 rhs' ps_rhs' }
  where
    emit_kind_co :: TcS CoercionN
    emit_kind_co
      | CtGiven { ctev_evar = evar } <- ev
      = do { let kind_co = maybe_sym $ mkTcKindCo (mkTcCoVarCo evar)  -- :: k2 ~ k1
           ; kind_ev <- newGivenEvVar kind_loc (kind_pty, evCoercion kind_co)
           ; emitWorkNC [kind_ev]
           ; return (ctEvCoercion kind_ev) }

      | otherwise
      = unifyWanted kind_loc Nominal ki2 ki1

    xi1      = canEqLHSType lhs1
    loc      = ctev_loc ev
    role     = eqRelRole eq_rel
    kind_loc = mkKindLoc xi1 xi2 loc
    kind_pty = mkHeteroPrimEqPred liftedTypeKind liftedTypeKind ki2 ki1

    maybe_sym = case swapped of
          IsSwapped  -> id         -- if the input is swapped, then we already
                                   -- will have k2 ~ k1
          NotSwapped -> mkTcSymCo

-- guaranteed that tcTypeKind lhs == tcTypeKind rhs
canEqCanLHSHomo :: CtEvidence
                -> EqRel -> SwapFlag
                -> CanEqLHS           -- lhs (or, if swapped, rhs)
                -> TcType             -- pretty lhs
                -> TcType -> TcType   -- rhs, pretty rhs
                -> TcS (StopOrContinue Ct)
canEqCanLHSHomo ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2
  | (xi2', mco) <- split_cast_ty xi2
  , Just lhs2 <- canEqLHS_maybe xi2'
  = canEqCanLHS2 ev eq_rel swapped lhs1 ps_xi1 lhs2 (ps_xi2 `mkCastTyMCo` mkTcSymMCo mco) mco

  | otherwise
  = canEqCanLHSFinish ev eq_rel swapped lhs1 ps_xi2

  where
    split_cast_ty (CastTy ty co) = (ty, MCo co)
    split_cast_ty other          = (other, MRefl)

-- This function deals with the case that both LHS and RHS are potential
-- CanEqLHSs.
canEqCanLHS2 :: CtEvidence              -- lhs ~ (rhs |> mco)
                                        -- or, if swapped: (rhs |> mco) ~ lhs
             -> EqRel -> SwapFlag
             -> CanEqLHS                -- lhs (or, if swapped, rhs)
             -> TcType                  -- pretty lhs
             -> CanEqLHS                -- rhs
             -> TcType                  -- pretty rhs
             -> MCoercion               -- :: kind(rhs) ~N kind(lhs)
             -> TcS (StopOrContinue Ct)
canEqCanLHS2 ev eq_rel swapped lhs1 ps_xi1 lhs2 ps_xi2 mco
  | lhs1 `eqCanEqLHS` lhs2
    -- It must be the case that mco is reflexive
  = canEqReflexive ev eq_rel (canEqLHSType lhs1)

  | TyVarLHS tv1 <- lhs1
  , TyVarLHS tv2 <- lhs2
  , swapOverTyVars (isGiven ev) tv1 tv2
  = do { traceTcS "canEqLHS2 swapOver" (ppr tv1 $$ ppr tv2 $$ ppr swapped)
       ; new_ev <- do_swap
       ; canEqCanLHSFinish new_ev eq_rel IsSwapped (TyVarLHS tv2)
                                                   (ps_xi1 `mkCastTyMCo` sym_mco) }

  | TyVarLHS tv1 <- lhs1
  , TyFamLHS fun_tc2 fun_args2 <- lhs2
  = canEqTyVarFunEq ev eq_rel swapped tv1 ps_xi1 fun_tc2 fun_args2 ps_xi2 mco

  | TyFamLHS fun_tc1 fun_args1 <- lhs1
  , TyVarLHS tv2 <- lhs2
  = do { new_ev <- do_swap
       ; canEqTyVarFunEq new_ev eq_rel IsSwapped tv2 ps_xi2
                                                 fun_tc1 fun_args1 ps_xi1 sym_mco }

  | TyFamLHS fun_tc1 fun_args1 <- lhs1
  , TyFamLHS fun_tc2 fun_args2 <- lhs2
  = do { traceTcS "canEqCanLHS2 two type families" (ppr lhs1 $$ ppr lhs2)

         -- emit derived equalities for injective type families
       ; let inj_eqns :: [TypeEqn]  -- TypeEqn = Pair Type
             inj_eqns
               | ReprEq <- eq_rel   = []   -- injectivity applies only for nom. eqs.
               | fun_tc1 /= fun_tc2 = []   -- if the families don't match, stop.

               | Injective inj <- tyConInjectivityInfo fun_tc1
               = [ Pair arg1 arg2
                 | (arg1, arg2, True) <- zip3 fun_args1 fun_args2 inj ]

                 -- built-in synonym families don't have an entry point
                 -- for this use case. So, we just use sfInteractInert
                 -- and pass two equal RHSs. We *could* add another entry
                 -- point, but then there would be a burden to make
                 -- sure the new entry point and existing ones were
                 -- internally consistent. This is slightly distasteful,
                 -- but it works well in practice and localises the
                 -- problem.
               | Just ops <- isBuiltInSynFamTyCon_maybe fun_tc1
               = let ki1 = canEqLHSKind lhs1
                     ki2 | MRefl <- mco
                         = ki1   -- just a small optimisation
                         | otherwise
                         = canEqLHSKind lhs2

                     fake_rhs1 = anyTypeOfKind ki1
                     fake_rhs2 = anyTypeOfKind ki2
                 in
                 sfInteractInert ops fun_args1 fake_rhs1 fun_args2 fake_rhs2

               | otherwise  -- ordinary, non-injective type family
               = []

       ; unless (isGiven ev) $
         mapM_ (unifyDerived (ctEvLoc ev) Nominal) inj_eqns

       ; tclvl <- getTcLevel
       ; dflags <- getDynFlags
       ; let tvs1 = tyCoVarsOfTypes fun_args1
             tvs2 = tyCoVarsOfTypes fun_args2

             swap_for_rewriting = anyVarSet (isTouchableMetaTyVar tclvl) tvs2 &&
                          -- swap 'em: Note [Put touchable variables on the left]
                                  not (anyVarSet (isTouchableMetaTyVar tclvl) tvs1)
                          -- this check is just to avoid unfruitful swapping

               -- If we have F a ~ F (F a), we want to swap.
             swap_for_occurs
               | CTE_OK     <- checkTyFamEq dflags fun_tc2 fun_args2
                                            (mkTyConApp fun_tc1 fun_args1)
               , CTE_Occurs <- checkTyFamEq dflags fun_tc1 fun_args1
                                            (mkTyConApp fun_tc2 fun_args2)
               = True

               | otherwise
               = False

       ; if swap_for_rewriting || swap_for_occurs
         then do { new_ev <- do_swap
                 ; canEqCanLHSFinish new_ev eq_rel IsSwapped lhs2 (ps_xi1 `mkCastTyMCo` sym_mco) }
         else finish_without_swapping }

  -- that's all the special cases. Now we just figure out which non-special case
  -- to continue to.
  | otherwise
  = finish_without_swapping

  where
    sym_mco = mkTcSymMCo mco

    do_swap = rewriteCastedEquality ev eq_rel swapped (canEqLHSType lhs1) (canEqLHSType lhs2) mco
    finish_without_swapping = canEqCanLHSFinish ev eq_rel swapped lhs1 (ps_xi2 `mkCastTyMCo` mco)


-- This function handles the case where one side is a tyvar and the other is
-- a type family application. Which to put on the left?
--   If the tyvar is a touchable meta-tyvar, put it on the left, as this may
--   be our only shot to unify.
--   Otherwise, put the function on the left, because it's generally better to
--   rewrite away function calls. This makes types smaller. And it seems necessary:
--     [W] F alpha ~ alpha
--     [W] F alpha ~ beta
--     [W] G alpha beta ~ Int   ( where we have type instance G a a = a )
--   If we end up with a stuck alpha ~ F alpha, we won't be able to solve this.
--   Test case: indexed-types/should_compile/CEqCanOccursCheck
canEqTyVarFunEq :: CtEvidence               -- :: lhs ~ (rhs |> mco)
                                            -- or (rhs |> mco) ~ lhs if swapped
                -> EqRel -> SwapFlag
                -> TyVar -> TcType          -- lhs (or if swapped rhs), pretty lhs
                -> TyCon -> [Xi] -> TcType  -- rhs (or if swapped lhs) fun and args, pretty rhs
                -> MCoercion                -- :: kind(rhs) ~N kind(lhs)
                -> TcS (StopOrContinue Ct)
canEqTyVarFunEq ev eq_rel swapped tv1 ps_xi1 fun_tc2 fun_args2 ps_xi2 mco
  = do { can_unify <- unifyTest ev tv1 rhs
       ; dflags    <- getDynFlags
       ; if | case can_unify of { NoUnify -> False; _ -> True }
            , CTE_OK <- checkTyVarEq dflags YesTypeFamilies tv1 rhs
            -> canEqCanLHSFinish ev eq_rel swapped (TyVarLHS tv1) rhs

            | otherwise
              -> do { new_ev <- rewriteCastedEquality ev eq_rel swapped
                                  (mkTyVarTy tv1) (mkTyConApp fun_tc2 fun_args2)
                                  mco
                    ; canEqCanLHSFinish new_ev eq_rel IsSwapped
                                  (TyFamLHS fun_tc2 fun_args2)
                                  (ps_xi1 `mkCastTyMCo` sym_mco) } }
  where
    sym_mco = mkTcSymMCo mco
    rhs = ps_xi2 `mkCastTyMCo` mco

data UnifyTestResult
  -- See Note [Solve by unification] in GHC.Tc.Solver.Interact
  -- which points out that having UnifySameLevel is just an optimisation;
  -- we could manage with UnifyOuterLevel alone (suitably renamed)
  = UnifySameLevel
  | UnifyOuterLevel [TcTyVar]   -- Promote these
                    TcLevel     -- ..to this level
  | NoUnify

instance Outputable UnifyTestResult where
  ppr UnifySameLevel            = text "UnifySameLevel"
  ppr (UnifyOuterLevel tvs lvl) = text "UnifyOuterLevel" <> parens (ppr lvl <+> ppr tvs)
  ppr NoUnify                   = text "NoUnify"

unifyTest :: CtEvidence -> TcTyVar -> TcType -> TcS UnifyTestResult
-- This is the key test for untouchability:
-- See Note [Unification preconditions] in GHC.Tc.Utils.Unify
-- and Note [Solve by unification] in GHC.Tc.Solver.Interact
unifyTest ev tv1 rhs
  | not (isGiven ev)  -- See Note [Do not unify Givens]
  , MetaTv { mtv_tclvl = tv_lvl, mtv_info = info } <- tcTyVarDetails tv1
  , canSolveByUnification info rhs
  = do { ambient_lvl  <- getTcLevel
       ; given_eq_lvl <- getInnermostGivenEqLevel

       ; if | tv_lvl `sameDepthAs` ambient_lvl
            -> return UnifySameLevel

            | tv_lvl `deeperThanOrSame` given_eq_lvl   -- No intervening given equalities
            , all (does_not_escape tv_lvl) free_skols  -- No skolem escapes
            -> return (UnifyOuterLevel free_metas tv_lvl)

            | otherwise
            -> return NoUnify }
  | otherwise
  = return NoUnify
  where
     (free_metas, free_skols) = partition isPromotableMetaTyVar $
                                nonDetEltsUniqSet               $
                                tyCoVarsOfType rhs

     does_not_escape tv_lvl fv
       | isTyVar fv = tv_lvl `deeperThanOrSame` tcTyVarLevel fv
       | otherwise  = True
       -- Coercion variables are not an escape risk
       -- If an implication binds a coercion variable, it'll have equalities,
       -- so the "intervening given equalities" test above will catch it
       -- Coercion holes get filled with coercions, so again no problem.

{- Note [Do not unify Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GADT match
   data T a where
      T1 :: T Int
      ...

   f x = case x of
           T1 -> True
           ...

So we get f :: T alpha[1] -> beta[1]
          x :: T alpha[1]
and from the T1 branch we get the implication
   forall[2] (alpha[1] ~ Int) => beta[1] ~ Bool

Now, clearly we don't want to unify alpha:=Int!  Yet at the moment we
process [G] alpha[1] ~ Int, we don't have any given-equalities in the
inert set, and hence there are no given equalities to make alpha untouchable.

(NB: if it were alpha[2] ~ Int, this argument wouldn't hold.  But that
almost never happens, and will never happen at all if we cure #18929.)

Simple solution: never unify in Givens!
-}

-- The RHS here is either not CanEqLHS, or it's one that we
-- want to rewrite the LHS to (as per e.g. swapOverTyVars)
canEqCanLHSFinish :: CtEvidence
                  -> EqRel -> SwapFlag
                  -> CanEqLHS              -- lhs (or, if swapped, rhs)
                  -> TcType          -- rhs, pretty rhs
                  -> TcS (StopOrContinue Ct)
canEqCanLHSFinish ev eq_rel swapped lhs rhs
-- RHS is fully rewritten, but with type synonyms
-- preserved as much as possible
-- guaranteed that tyVarKind lhs == typeKind rhs, for (TyEq:K)
-- (TyEq:N) is checked in can_eq_nc', and (TyEq:TV) is handled in canEqTyVarHomo

  = do { dflags <- getDynFlags
       ; new_ev <- rewriteEqEvidence ev swapped lhs_ty rhs rewrite_co1 rewrite_co2

     -- Must do the occurs check even on tyvar/tyvar
     -- equalities, in case have  x ~ (y :: ..x...)
     -- #12593
     -- guarantees (TyEq:OC), (TyEq:F), and (TyEq:H)
    -- this next line checks also for coercion holes (TyEq:H); see
    -- Note [Equalities with incompatible kinds]
       ; case canEqOK dflags eq_rel lhs rhs of
           CanEqOK ->
             do { traceTcS "canEqOK" (ppr lhs $$ ppr rhs)
                ; continueWith (CEqCan { cc_ev = new_ev, cc_lhs = lhs
                                       , cc_rhs = rhs, cc_eq_rel = eq_rel }) }
       -- it is possible that cc_rhs mentions the LHS if the LHS is a type
       -- family. This will cause later rewriting to potentially loop, but
       -- that will be caught by the depth counter. The other option is an
       -- occurs-check for a function application, which seems awkward.

           CanEqNotOK status
                -- See Note [Type variable cycles in Givens]
             | OtherCIS <- status
             , Given <- ctEvFlavour ev
             , TyVarLHS lhs_tv <- lhs
             , not (isCycleBreakerTyVar lhs_tv) -- See Detail (7) of Note
             , NomEq <- eq_rel
             -> do { traceTcS "canEqCanLHSFinish breaking a cycle" (ppr lhs $$ ppr rhs)
                   ; new_rhs <- breakTyVarCycle (ctEvLoc ev) rhs
                   ; traceTcS "new RHS:" (ppr new_rhs)
                   ; let new_pred   = mkPrimEqPred (mkTyVarTy lhs_tv) new_rhs
                         new_new_ev = new_ev { ctev_pred = new_pred }
                           -- See Detail (6) of Note [Type variable cycles in Givens]

                   ; if anyRewritableTyVar True NomEq (\ _ tv -> tv == lhs_tv) new_rhs
                     then do { traceTcS "Note [Type variable cycles in Givens] Detail (1)"
                                        (ppr new_new_ev)
                             ; continueWith (mkIrredCt status new_ev) }
                     else continueWith (CEqCan { cc_ev = new_new_ev, cc_lhs = lhs
                                               , cc_rhs = new_rhs, cc_eq_rel = eq_rel }) }

               -- We must not use it for further rewriting!
             | otherwise
             -> do { traceTcS "canEqCanLHSFinish can't make a canonical" (ppr lhs $$ ppr rhs)
                   ; continueWith (mkIrredCt status new_ev) } }
  where
    role = eqRelRole eq_rel

    lhs_ty = canEqLHSType lhs

    rewrite_co1  = mkTcReflCo role lhs_ty
    rewrite_co2  = mkTcReflCo role rhs

-- | Solve a reflexive equality constraint
canEqReflexive :: CtEvidence    -- ty ~ ty
               -> EqRel
               -> TcType        -- ty
               -> TcS (StopOrContinue Ct)   -- always Stop
canEqReflexive ev eq_rel ty
  = do { setEvBindIfWanted ev (evCoercion $
                               mkTcReflCo (eqRelRole eq_rel) ty)
       ; stopWith ev "Solved by reflexivity" }

rewriteCastedEquality :: CtEvidence     -- :: lhs ~ (rhs |> mco), or (rhs |> mco) ~ lhs
                      -> EqRel -> SwapFlag
                      -> TcType         -- lhs
                      -> TcType         -- rhs
                      -> MCoercion      -- mco
                      -> TcS CtEvidence -- :: (lhs |> sym mco) ~ rhs
                                        -- result is independent of SwapFlag
rewriteCastedEquality ev eq_rel swapped lhs rhs mco
  = rewriteEqEvidence ev swapped new_lhs new_rhs lhs_co rhs_co
  where
    new_lhs = lhs `mkCastTyMCo` sym_mco
    lhs_co  = mkTcGReflLeftMCo role lhs sym_mco

    new_rhs = rhs
    rhs_co  = mkTcGReflRightMCo role rhs mco

    sym_mco = mkTcSymMCo mco
    role    = eqRelRole eq_rel

---------------------------------------------
-- | Result of checking whether a RHS is suitable for pairing
-- with a CanEqLHS in a CEqCan.
data CanEqOK
  = CanEqOK                   -- RHS is good
  | CanEqNotOK CtIrredStatus  -- don't proceed; explains why

instance Outputable CanEqOK where
  ppr CanEqOK             = text "CanEqOK"
  ppr (CanEqNotOK status) = text "CanEqNotOK" <+> ppr status

-- | This function establishes most of the invariants needed to make
-- a CEqCan.
--
--   TyEq:OC: Checked here.
--   TyEq:F:  Checked here.
--   TyEq:K:  assumed; ASSERTed here (that is, kind(lhs) = kind(rhs))
--   TyEq:N:  assumed; ASSERTed here (if eq_rel is R, rhs is not a newtype)
--   TyEq:TV: not checked (this is hard to check)
--   TyEq:H:  Checked here.
canEqOK :: DynFlags -> EqRel -> CanEqLHS -> Xi -> CanEqOK
canEqOK dflags eq_rel lhs rhs
  = ASSERT( good_rhs )
    case checkTypeEq dflags YesTypeFamilies lhs rhs of
      CTE_OK  -> CanEqOK
      CTE_Bad -> CanEqNotOK OtherCIS
                 -- Violation of TyEq:F

      CTE_HoleBlocker -> CanEqNotOK (BlockedCIS holes)
        where holes = coercionHolesOfType rhs
                 -- This is the case detailed in
                 -- Note [Equalities with incompatible kinds]
                 -- Violation of TyEq:H

                 -- These are both a violation of TyEq:OC, but we
                 -- want to differentiate for better production of
                 -- error messages
      CTE_Occurs | TyVarLHS tv <- lhs
                  , isInsolubleOccursCheck eq_rel tv rhs -> CanEqNotOK InsolubleCIS
                 -- If we have a ~ [a], it is not canonical, and in particular
                 -- we don't want to rewrite existing inerts with it, otherwise
                 -- we'd risk divergence in the constraint solver

                 -- NB: no occCheckExpand here; see Note [Rewriting synonyms]
                 -- in GHC.Tc.Solver.Rewrite

                  | otherwise                            -> CanEqNotOK OtherCIS
                 -- A representational equality with an occurs-check problem isn't
                 -- insoluble! For example:
                 --   a ~R b a
                 -- We might learn that b is the newtype Id.
                 -- But, the occurs-check certainly prevents the equality from being
                 -- canonical, and we might loop if we were to use it in rewriting.

                 -- This case also include type family occurs-check errors, which
                 -- are not generally insoluble

  where
    good_rhs    = kinds_match && not bad_newtype

    lhs_kind    = canEqLHSKind lhs
    rhs_kind    = tcTypeKind rhs

    kinds_match = lhs_kind `tcEqType` rhs_kind

    bad_newtype | ReprEq <- eq_rel
                , Just tc <- tyConAppTyCon_maybe rhs
                = isNewTyCon tc
                | otherwise
                = False

{- Note [Equalities with incompatible kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What do we do when we have an equality

  (tv :: k1) ~ (rhs :: k2)

where k1 and k2 differ? Easy: we create a coercion that relates k1 and
k2 and use this to cast. To wit, from

  [X] (tv :: k1) ~ (rhs :: k2)

(where [X] is [G], [W], or [D]), we go to

  [noDerived X] co :: k2 ~ k1
  [X]           (tv :: k1) ~ ((rhs |> co) :: k1)

where

  noDerived G = G
  noDerived _ = W

For reasons described in Wrinkle (2) below, we want the [X] constraint to be "blocked";
that is, it should be put aside, and not used to rewrite any other constraint,
until the kind-equality on which it depends (namely 'co' above) is solved.
To achieve this
* The [X] constraint is a CIrredCan
* With a cc_status of BlockedCIS bchs
* Where 'bchs' is the set of "blocking coercion holes".  The blocking coercion
  holes are the free coercion holes of [X]'s type
* When all the blocking coercion holes in the CIrredCan are filled (solved),
  we convert [X] to a CNonCanonical and put it in the work list.
All this is described in more detail in Wrinkle (2).

Wrinkles:

 (1) The noDerived step is because Derived equalities have no evidence.
     And yet we absolutely need evidence to be able to proceed here.
     Given evidence will use the KindCo coercion; Wanted evidence will
     be a coercion hole. Even a Derived hetero equality begets a Wanted
     kind equality.

 (2) Though it would be sound to do so, we must not mark the rewritten Wanted
       [W] (tv :: k1) ~ ((rhs |> co) :: k1)
     as canonical in the inert set. In particular, we must not unify tv.
     If we did, the Wanted becomes a Given (effectively), and then can
     rewrite other Wanteds. But that's bad: See Note [Wanteds do not rewrite Wanteds]
     in GHC.Tc.Types.Constraint. The problem is about poor error messages. See #11198 for
     tales of destruction.

     So, we have an invariant on CEqCan (TyEq:H) that the RHS does not have
     any coercion holes. This is checked in checkTypeEq. Any equalities that
     have such an RHS are turned into CIrredCans with a BlockedCIS status. We also
     must be sure to kick out any such CIrredCan constraints that mention coercion holes
     when those holes get filled in, so that the unification step can now proceed.

     The kicking out is done in kickOutAfterFillingCoercionHole.

     However, we must be careful: we kick out only when no coercion holes are
     left. The holes in the type are stored in the BlockedCIS CtIrredStatus.
     The extra check that there are no more remaining holes avoids
     needless work when rewriting evidence (which fills coercion holes) and
     aids efficiency.

     Moreover, kicking out when there are remaining unfilled holes can
     cause a loop in the solver in this case:
          [W] w1 :: (ty1 :: F a) ~ (ty2 :: s)
     After canonicalisation, we discover that this equality is heterogeneous.
     So we emit
          [W] co_abc :: F a ~ s
     and preserve the original as
          [W] w2 :: (ty1 |> co_abc) ~ ty2    (blocked on co_abc)
     Then, co_abc comes becomes the work item. It gets swapped in
     canEqCanLHS2 and then back again in canEqTyVarFunEq. We thus get
     co_abc := sym co_abd, and then co_abd := sym co_abe, with
          [W] co_abe :: F a ~ s
     This process has filled in co_abc. Suppose w2 were kicked out.
     When it gets processed,
     would get this whole chain going again. The solution is to
     kick out a blocked constraint only when the result of filling
     in the blocking coercion involves no further blocking coercions.
     Alternatively, we could be careful not to do unnecessary swaps during
     canonicalisation, but that seems hard to do, in general.

 (3) Suppose we have [W] (a :: k1) ~ (rhs :: k2). We duly follow the
     algorithm detailed here, producing [W] co :: k2 ~ k1, and adding
     [W] (a :: k1) ~ ((rhs |> co) :: k1) to the irreducibles. Some time
     later, we solve co, and fill in co's coercion hole. This kicks out
     the irreducible as described in (2).
     But now, during canonicalization, we see the cast
     and remove it, in canEqCast. By the time we get into canEqCanLHS, the equality
     is heterogeneous again, and the process repeats.

     To avoid this, we don't strip casts off a type if the other type
     in the equality is a CanEqLHS (the scenario above can happen with a
     type family, too. testcase: typecheck/should_compile/T13822).
     And this is an improvement regardless:
     because tyvars can, generally, unify with casted types, there's no
     reason to go through the work of stripping off the cast when the
     cast appears opposite a tyvar. This is implemented in the cast case
     of can_eq_nc'.

 (4) Reporting an error for a constraint that is blocked with status BlockedCIS
     is hard: what would we say to users? And we don't
     really need to report, because if a constraint is blocked, then
     there is unsolved wanted blocking it; that unsolved wanted will
     be reported. We thus push such errors to the bottom of the queue
     in the error-reporting code; they should never be printed.

     (4a) It would seem possible to do this filtering just based on the
          presence of a blocking coercion hole. However, this is no good,
          as it suppresses e.g. no-instance-found errors. We thus record
          a CtIrredStatus in CIrredCan and filter based on this status.
          This happened in T14584. An alternative approach is to expressly
          look for *equalities* with blocking coercion holes, but actually
          recording the blockage in a status field seems nicer.

     (4b) The error message might be printed with -fdefer-type-errors,
          so it still must exist. This is the only reason why there is
          a message at all. Otherwise, we could simply do nothing.

Historical note:

We used to do this via emitting a Derived kind equality and then parking
the heterogeneous equality as irreducible. But this new approach is much
more direct. And it doesn't produce duplicate Deriveds (as the old one did).

Note [Type synonyms and canonicalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat type synonym applications as xi types, that is, they do not
count as type function applications.  However, we do need to be a bit
careful with type synonyms: like type functions they may not be
generative or injective.  However, unlike type functions, they are
parametric, so there is no problem in expanding them whenever we see
them, since we do not need to know anything about their arguments in
order to expand them; this is what justifies not having to treat them
as specially as type function applications.  The thing that causes
some subtleties is that we prefer to leave type synonym applications
*unexpanded* whenever possible, in order to generate better error
messages.

If we encounter an equality constraint with type synonym applications
on both sides, or a type synonym application on one side and some sort
of type application on the other, we simply must expand out the type
synonyms in order to continue decomposing the equality constraint into
primitive equality constraints.  For example, suppose we have

  type F a = [Int]

and we encounter the equality

  F a ~ [b]

In order to continue we must expand F a into [Int], giving us the
equality

  [Int] ~ [b]

which we can then decompose into the more primitive equality
constraint

  Int ~ b.

However, if we encounter an equality constraint with a type synonym
application on one side and a variable on the other side, we should
NOT (necessarily) expand the type synonym, since for the purpose of
good error messages we want to leave type synonyms unexpanded as much
as possible.  Hence the ps_xi1, ps_xi2 argument passed to canEqCanLHS.

Note [Type variable cycles in Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this situation (from indexed-types/should_compile/GivenLoop):

  instance C (Maybe b)
  [G] a ~ Maybe (F a)
  [W] C a

In order to solve the Wanted, we must use the Given to rewrite `a` to
Maybe (F a). But note that the Given has an occurs-check failure, and
so we can't straightforwardly add the Given to the inert set.

The key idea is to replace the (F a) in the RHS of the Given with a
fresh variable, which we'll call a CycleBreakerTv, or cbv. Then, emit
a new Given to connect cbv with F a. So our situation becomes

  instance C (Maybe b)
  [G] a ~ Maybe cbv
  [G] F a ~ cbv
  [W] C a

Note the orientation of the second Given. The type family ends up
on the left; see commentary on canEqTyVarFunEq, which decides how to
orient such cases. No special treatment for CycleBreakerTvs is
necessary. This scenario is now easily soluble, by using the first
Given to rewrite the Wanted, which can now be solved.

(The first Given actually also rewrites the second one. This causes
no trouble.)

More generally, we detect this scenario by the following characteristics:
 - a Given CEqCan constraint
 - with a tyvar on its LHS
 - with a soluble occurs-check failure
 - and a nominal equality

Having identified the scenario, we wish to replace all type family
applications on the RHS with fresh metavariables (with MetaInfo
CycleBreakerTv). This is done in breakTyVarCycle. These metavariables are
untouchable, but we also emit Givens relating the fresh variables to the type
family applications they replace.

Of course, we don't want our fresh variables leaking into e.g. error messages.
So we fill in the metavariables with their original type family applications
after we're done running the solver (in nestImplicTcS and runTcSWithEvBinds).
This is done by restoreTyVarCycles, which uses the inert_cycle_breakers field in
InertSet, which contains the pairings invented in breakTyVarCycle.

That is:

We transform
  [G] g : a ~ ...(F a)...
to
  [G] (Refl a) : F a ~ cbv      -- CEqCan
  [G] g        : a ~ ...cbv...  -- CEqCan

Note that
* `cbv` is a fresh cycle breaker variable.
* `cbv` is a is a meta-tyvar, but it is completely untouchable.
* We track the cycle-breaker variables in inert_cycle_breakers in InertSet
* We eventually fill in the cycle-breakers, with `cbv := F a`.
  No one else fills in cycle-breakers!
* In inert_cycle_breakers, we remember the (cbv, F a) pair; that is, we
  remember the /original/ type.  The [G] F a ~ cbv constraint may be rewritten
  by other givens (eg if we have another [G] a ~ (b,c), but at the end we
  still fill in with cbv := F a
* This fill-in is done when solving is complete, by restoreTyVarCycles
  in nestImplicTcS and runTcSWithEvBinds.
* The evidence for the new `F a ~ cbv` constraint is Refl, because we know this fill-in is
  ultimately going to happen.

There are drawbacks of this approach:

 1. We apply this trick only for Givens, never for Wanted or Derived.
    It wouldn't make sense for Wanted, because Wanted never rewrite.
    But it's conceivable that a Derived would benefit from this all.
    I doubt it would ever happen, though, so I'm holding off.

 2. We don't use this trick for representational equalities, as there
    is no concrete use case where it is helpful (unlike for nominal
    equalities). Furthermore, because function applications can be
    CanEqLHSs, but newtype applications cannot, the disparities between
    the cases are enough that it would be effortful to expand the idea
    to representational equalities. A quick attempt, with

      data family N a b

      f :: (Coercible a (N a b), Coercible (N a b) b) => a -> b
      f = coerce

    failed with "Could not match 'b' with 'b'." Further work is held off
    until when we have a concrete incentive to explore this dark corner.

Details:

 (1) We don't look under foralls, at all, when substituting away type family
     applications, because doing so can never be fruitful. Recall that we
     are in a case like [G] a ~ forall b. ... a ....   Until we have a type
     family that can pull the body out from a forall, this will always be
     insoluble. Note also that the forall cannot be in an argument to a
     type family, or that outer type family application would already have
     been substituted away.

     However, we still must check to make sure that breakTyVarCycle actually
     succeeds in getting rid of all occurrences of the offending variable.
     If one is hidden under a forall, this won't be true. So we perform
     an additional check after performing the substitution.

     Skipping this check causes typecheck/should_fail/GivenForallLoop to loop.

 (2) Our goal here is to avoid loops in rewriting. We can thus skip looking
     in coercions, as we don't rewrite in coercions.
     (There is no worry about unifying a meta-variable here: this Note is
      only about Givens.)

 (3) As we're substituting, we can build ill-kinded
     types. For example, if we have Proxy (F a) b, where (b :: F a), then
     replacing this with Proxy cbv b is ill-kinded. However, we will later
     set cbv := F a, and so the zonked type will be well-kinded again.
     The temporary ill-kinded type hurts no one, and avoiding this would
     be quite painfully difficult.

     Specifically, this detail does not contravene the Purely Kinded Type Invariant
     (Note [The Purely Kinded Type Invariant (PKTI)] in GHC.Tc.Gen.HsType).
     The PKTI says that we can call typeKind on any type, without failure.
     It would be violated if we, say, replaced a kind (a -> b) with a kind c,
     because an arrow kind might be consulted in piResultTys. Here, we are
     replacing one opaque type like (F a b c) with another, cbv (opaque in
     that we never assume anything about its structure, like that it has a
     result type or a RuntimeRep argument).

 (4) The evidence for the produced Givens is all just reflexive, because
     we will eventually set the cycle-breaker variable to be the type family,
     and then, after the zonk, all will be well.

 (5) The approach here is inefficient. For instance, we could choose to
     affect only type family applications that mention the offending variable:
     in a ~ (F b, G a), we need to replace only G a, not F b. Furthermore,
     we could try to detect cases like a ~ (F a, F a) and use the same
     tyvar to replace F a. (Cf.
     Note [Flattening type-family applications when matching instances]
     in GHC.Core.Unify, which
     goes to this extra effort.) There may be other opportunities for
     improvement. However, this is really a very small corner case, always
     tickled by a user-written Given. The investment to craft a clever,
     performant solution seems unworthwhile.

 (6) We often get the predicate associated with a constraint from its
     evidence. We thus must not only make sure the generated CEqCan's
     fields have the updated RHS type, but we must also update the
     evidence itself. As in Detail (4), we don't need to change the
     evidence term (as in e.g. rewriteEqEvidence) because the cycle
     breaker variables are all zonked away by the time we examine the
     evidence. That is, we must set the ctev_pred of the ctEvidence.
     This is implemented in canEqCanLHSFinish, with a reference to
     this detail.

 (7) We don't wish to apply this magic to CycleBreakerTvs themselves.
     Consider this, from typecheck/should_compile/ContextStack2:

       type instance TF (a, b) = (TF a, TF b)
       t :: (a ~ TF (a, Int)) => ...

       [G] a ~ TF (a, Int)

     The RHS reduces, so we get

       [G] a ~ (TF a, TF Int)

     We then break cycles, to get

       [G] g1 :: a ~ (cbv1, cbv2)
       [G] g2 :: TF a ~ cbv1
       [G] g3 :: TF Int ~ cbv2

     g1 gets added to the inert set, as written. But then g2 becomes
     the work item. g1 rewrites g2 to become

       [G] TF (cbv1, cbv2) ~ cbv1

     which then uses the type instance to become

       [G] (TF cbv1, TF cbv2) ~ cbv1

     which looks remarkably like the Given we started with. If left
     unchecked, this will end up breaking cycles again, looping ad
     infinitum (and resulting in a context-stack reduction error,
     not an outright loop). The solution is easy: don't break cycles
     if the var is already a CycleBreakerTv. Instead, we mark this
     final Given as a CIrredCan with an OtherCIS status (it's not
     insoluble).

     NB: When filling in CycleBreakerTvs, we fill them in with what
     they originally stood for (e.g. cbv1 := TF a, cbv2 := TF Int),
     not what may be in a rewritten constraint.

     Not breaking cycles further (which would mean changing TF cbv1 to cbv3
     and TF cbv2 to cbv4) makes sense, because we only want to break cycles
     for user-written loopy Givens, and a CycleBreakerTv certainly isn't
     user-written.

NB: This same situation (an equality like b ~ Maybe (F b)) can arise with
Wanteds, but we have no concrete case incentivising special treatment. It
would just be a CIrredCan.

-}

{-
************************************************************************
*                                                                      *
                  Evidence transformation
*                                                                      *
************************************************************************
-}

data StopOrContinue a
  = ContinueWith a    -- The constraint was not solved, although it may have
                      --   been rewritten

  | Stop CtEvidence   -- The (rewritten) constraint was solved
         SDoc         -- Tells how it was solved
                      -- Any new sub-goals have been put on the work list
  deriving (Functor)

instance Outputable a => Outputable (StopOrContinue a) where
  ppr (Stop ev s)      = text "Stop" <> parens s <+> ppr ev
  ppr (ContinueWith w) = text "ContinueWith" <+> ppr w

continueWith :: a -> TcS (StopOrContinue a)
continueWith = return . ContinueWith

stopWith :: CtEvidence -> String -> TcS (StopOrContinue a)
stopWith ev s = return (Stop ev (text s))

andWhenContinue :: TcS (StopOrContinue a)
                -> (a -> TcS (StopOrContinue b))
                -> TcS (StopOrContinue b)
andWhenContinue tcs1 tcs2
  = do { r <- tcs1
       ; case r of
           Stop ev s       -> return (Stop ev s)
           ContinueWith ct -> tcs2 ct }
infixr 0 `andWhenContinue`    -- allow chaining with ($)

rewriteEvidence :: CtEvidence   -- old evidence
                -> TcPredType   -- new predicate
                -> TcCoercion   -- Of type :: new predicate ~ <type of old evidence>
                -> TcS (StopOrContinue CtEvidence)
-- Returns Just new_ev iff either (i)  'co' is reflexivity
--                             or (ii) 'co' is not reflexivity, and 'new_pred' not cached
-- In either case, there is nothing new to do with new_ev
{-
     rewriteEvidence old_ev new_pred co
Main purpose: create new evidence for new_pred;
              unless new_pred is cached already
* Returns a new_ev : new_pred, with same wanted/given/derived flag as old_ev
* If old_ev was wanted, create a binding for old_ev, in terms of new_ev
* If old_ev was given, AND not cached, create a binding for new_ev, in terms of old_ev
* Returns Nothing if new_ev is already cached

        Old evidence    New predicate is               Return new evidence
        flavour                                        of same flavor
        -------------------------------------------------------------------
        Wanted          Already solved or in inert     Nothing
        or Derived      Not                            Just new_evidence

        Given           Already in inert               Nothing
                        Not                            Just new_evidence

Note [Rewriting with Refl]
~~~~~~~~~~~~~~~~~~~~~~~~~~
If the coercion is just reflexivity then you may re-use the same
variable.  But be careful!  Although the coercion is Refl, new_pred
may reflect the result of unification alpha := ty, so new_pred might
not _look_ the same as old_pred, and it's vital to proceed from now on
using new_pred.

The rewriter preserves type synonyms, so they should appear in new_pred
as well as in old_pred; that is important for good error messages.
 -}


rewriteEvidence old_ev@(CtDerived {}) new_pred _co
  = -- If derived, don't even look at the coercion.
    -- This is very important, DO NOT re-order the equations for
    -- rewriteEvidence to put the isTcReflCo test first!
    -- Why?  Because for *Derived* constraints, c, the coercion, which
    -- was produced by rewriting, may contain suspended calls to
    -- (ctEvExpr c), which fails for Derived constraints.
    -- (Getting this wrong caused #7384.)
    continueWith (old_ev { ctev_pred = new_pred })

rewriteEvidence old_ev new_pred co
  | isTcReflCo co -- See Note [Rewriting with Refl]
  = continueWith (old_ev { ctev_pred = new_pred })

rewriteEvidence ev@(CtGiven { ctev_evar = old_evar, ctev_loc = loc }) new_pred co
  = do { new_ev <- newGivenEvVar loc (new_pred, new_tm)
       ; continueWith new_ev }
  where
    -- mkEvCast optimises ReflCo
    new_tm = mkEvCast (evId old_evar) (tcDowngradeRole Representational
                                                       (ctEvRole ev)
                                                       (mkTcSymCo co))

rewriteEvidence ev@(CtWanted { ctev_dest = dest
                             , ctev_nosh = si
                             , ctev_loc = loc }) new_pred co
  = do { mb_new_ev <- newWanted_SI si loc new_pred
               -- The "_SI" variant ensures that we make a new Wanted
               -- with the same shadow-info as the existing one
               -- with the same shadow-info as the existing one (#16735)
       ; MASSERT( tcCoercionRole co == ctEvRole ev )
       ; setWantedEvTerm dest
            (mkEvCast (getEvExpr mb_new_ev)
                      (tcDowngradeRole Representational (ctEvRole ev) co))
       ; case mb_new_ev of
            Fresh  new_ev -> continueWith new_ev
            Cached _      -> stopWith ev "Cached wanted" }


rewriteEqEvidence :: CtEvidence         -- Old evidence :: olhs ~ orhs (not swapped)
                                        --              or orhs ~ olhs (swapped)
                  -> SwapFlag
                  -> TcType -> TcType   -- New predicate  nlhs ~ nrhs
                  -> TcCoercion         -- lhs_co, of type :: nlhs ~ olhs
                  -> TcCoercion         -- rhs_co, of type :: nrhs ~ orhs
                  -> TcS CtEvidence     -- Of type nlhs ~ nrhs
-- For (rewriteEqEvidence (Given g olhs orhs) False nlhs nrhs lhs_co rhs_co)
-- we generate
-- If not swapped
--      g1 : nlhs ~ nrhs = lhs_co ; g ; sym rhs_co
-- If 'swapped'
--      g1 : nlhs ~ nrhs = lhs_co ; Sym g ; sym rhs_co
--
-- For (Wanted w) we do the dual thing.
-- New  w1 : nlhs ~ nrhs
-- If not swapped
--      w : olhs ~ orhs = sym lhs_co ; w1 ; rhs_co
-- If swapped
--      w : orhs ~ olhs = sym rhs_co ; sym w1 ; lhs_co
--
-- It's all a form of rewwriteEvidence, specialised for equalities
rewriteEqEvidence old_ev swapped nlhs nrhs lhs_co rhs_co
  | CtDerived {} <- old_ev  -- Don't force the evidence for a Derived
  = return (old_ev { ctev_pred = new_pred })

  | NotSwapped <- swapped
  , isTcReflCo lhs_co      -- See Note [Rewriting with Refl]
  , isTcReflCo rhs_co
  = return (old_ev { ctev_pred = new_pred })

  | CtGiven { ctev_evar = old_evar } <- old_ev
  = do { let new_tm = evCoercion (lhs_co
                                  `mkTcTransCo` maybeTcSymCo swapped (mkTcCoVarCo old_evar)
                                  `mkTcTransCo` mkTcSymCo rhs_co)
       ; newGivenEvVar loc' (new_pred, new_tm) }

  | CtWanted { ctev_dest = dest, ctev_nosh = si } <- old_ev
  = do { (new_ev, hole_co) <- newWantedEq_SI si loc'
                                             (ctEvRole old_ev) nlhs nrhs
               -- The "_SI" variant ensures that we make a new Wanted
               -- with the same shadow-info as the existing one (#16735)
       ; let co = maybeTcSymCo swapped $
                  mkSymCo lhs_co
                  `mkTransCo` hole_co
                  `mkTransCo` rhs_co
       ; setWantedEq dest co
       ; traceTcS "rewriteEqEvidence" (vcat [ppr old_ev, ppr nlhs, ppr nrhs, ppr co])
       ; return new_ev }

#if __GLASGOW_HASKELL__ <= 810
  | otherwise
  = panic "rewriteEvidence"
#endif
  where
    new_pred = mkTcEqPredLikeEv old_ev nlhs nrhs

      -- equality is like a type class. Bumping the depth is necessary because
      -- of recursive newtypes, where "reducing" a newtype can actually make
      -- it bigger. See Note [Newtypes can blow the stack].
    loc      = ctEvLoc old_ev
    loc'     = bumpCtLocDepth loc

{-
************************************************************************
*                                                                      *
              Unification
*                                                                      *
************************************************************************

Note [unifyWanted and unifyDerived]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When decomposing equalities we often create new wanted constraints for
(s ~ t).  But what if s=t?  Then it'd be faster to return Refl right away.
Similar remarks apply for Derived.

Rather than making an equality test (which traverses the structure of the
type, perhaps fruitlessly), unifyWanted traverses the common structure, and
bales out when it finds a difference by creating a new Wanted constraint.
But where it succeeds in finding common structure, it just builds a coercion
to reflect it.
-}

unifyWanted :: CtLoc -> Role
            -> TcType -> TcType -> TcS Coercion
-- Return coercion witnessing the equality of the two types,
-- emitting new work equalities where necessary to achieve that
-- Very good short-cut when the two types are equal, or nearly so
-- See Note [unifyWanted and unifyDerived]
-- The returned coercion's role matches the input parameter
unifyWanted loc Phantom ty1 ty2
  = do { kind_co <- unifyWanted loc Nominal (tcTypeKind ty1) (tcTypeKind ty2)
       ; return (mkPhantomCo kind_co ty1 ty2) }

unifyWanted loc role orig_ty1 orig_ty2
  = go orig_ty1 orig_ty2
  where
    go ty1 ty2 | Just ty1' <- tcView ty1 = go ty1' ty2
    go ty1 ty2 | Just ty2' <- tcView ty2 = go ty1 ty2'

    go (FunTy _ w1 s1 t1) (FunTy _ w2 s2 t2)
      = do { co_s <- unifyWanted loc role s1 s2
           ; co_t <- unifyWanted loc role t1 t2
           ; co_w <- unifyWanted loc Nominal w1 w2
           ; return (mkFunCo role co_w co_s co_t) }
    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2, tys1 `equalLength` tys2
      , isInjectiveTyCon tc1 role -- don't look under newtypes at Rep equality
      = do { cos <- zipWith3M (unifyWanted loc)
                              (tyConRolesX role tc1) tys1 tys2
           ; return (mkTyConAppCo role tc1 cos) }

    go ty1@(TyVarTy tv) ty2
      = do { mb_ty <- isFilledMetaTyVar_maybe tv
           ; case mb_ty of
                Just ty1' -> go ty1' ty2
                Nothing   -> bale_out ty1 ty2}
    go ty1 ty2@(TyVarTy tv)
      = do { mb_ty <- isFilledMetaTyVar_maybe tv
           ; case mb_ty of
                Just ty2' -> go ty1 ty2'
                Nothing   -> bale_out ty1 ty2 }

    go ty1@(CoercionTy {}) (CoercionTy {})
      = return (mkReflCo role ty1) -- we just don't care about coercions!

    go ty1 ty2 = bale_out ty1 ty2

    bale_out ty1 ty2
       | ty1 `tcEqType` ty2 = return (mkTcReflCo role ty1)
        -- Check for equality; e.g. a ~ a, or (m a) ~ (m a)
       | otherwise = emitNewWantedEq loc role orig_ty1 orig_ty2

unifyDeriveds :: CtLoc -> [Role] -> [TcType] -> [TcType] -> TcS ()
-- See Note [unifyWanted and unifyDerived]
unifyDeriveds loc roles tys1 tys2 = zipWith3M_ (unify_derived loc) roles tys1 tys2

unifyDerived :: CtLoc -> Role -> Pair TcType -> TcS ()
-- See Note [unifyWanted and unifyDerived]
unifyDerived loc role (Pair ty1 ty2) = unify_derived loc role ty1 ty2

unify_derived :: CtLoc -> Role -> TcType -> TcType -> TcS ()
-- Create new Derived and put it in the work list
-- Should do nothing if the two types are equal
-- See Note [unifyWanted and unifyDerived]
unify_derived _   Phantom _        _        = return ()
unify_derived loc role    orig_ty1 orig_ty2
  = go orig_ty1 orig_ty2
  where
    go ty1 ty2 | Just ty1' <- tcView ty1 = go ty1' ty2
    go ty1 ty2 | Just ty2' <- tcView ty2 = go ty1 ty2'

    go (FunTy _ w1 s1 t1) (FunTy _ w2 s2 t2)
      = do { unify_derived loc role s1 s2
           ; unify_derived loc role t1 t2
           ; unify_derived loc Nominal w1 w2 }
    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2, tys1 `equalLength` tys2
      , isInjectiveTyCon tc1 role
      = unifyDeriveds loc (tyConRolesX role tc1) tys1 tys2
    go ty1@(TyVarTy tv) ty2
      = do { mb_ty <- isFilledMetaTyVar_maybe tv
           ; case mb_ty of
                Just ty1' -> go ty1' ty2
                Nothing   -> bale_out ty1 ty2 }
    go ty1 ty2@(TyVarTy tv)
      = do { mb_ty <- isFilledMetaTyVar_maybe tv
           ; case mb_ty of
                Just ty2' -> go ty1 ty2'
                Nothing   -> bale_out ty1 ty2 }
    go ty1 ty2 = bale_out ty1 ty2

    bale_out ty1 ty2
       | ty1 `tcEqType` ty2 = return ()
        -- Check for equality; e.g. a ~ a, or (m a) ~ (m a)
       | otherwise = emitNewDerivedEq loc role orig_ty1 orig_ty2
