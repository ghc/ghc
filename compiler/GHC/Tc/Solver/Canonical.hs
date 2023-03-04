{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}

module GHC.Tc.Solver.Canonical(
     canonicalize,
     unifyWanted,
     makeSuperClasses,
     StopOrContinue(..), stopWith, continueWith, andWhenContinue,
     solveCallStack    -- For GHC.Tc.Solver
  ) where

import GHC.Prelude

import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType
import GHC.Tc.Solver.Rewrite
import GHC.Tc.Solver.Monad
import GHC.Tc.Solver.Equality( solveNonCanonicalEquality, solveCanonicalEquality )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.EvTerm

import GHC.Core.Type
import GHC.Core.Predicate
import GHC.Core.Class
import GHC.Core.Multiplicity
import GHC.Core.Coercion
import GHC.Core.Reduction
import GHC.Core.InstEnv ( Coherence(..) )
import GHC.Core

import GHC.Hs.Type( HsIPName(..) )

import GHC.Types.Id( mkTemplateLocals )
import GHC.Types.Var
import GHC.Types.Var.Env( mkInScopeSet )
import GHC.Types.Var.Set( delVarSetList )
import GHC.Types.Name.Set
import GHC.Types.Unique  ( hasKey )

import GHC.Builtin.Names ( coercibleTyConKey )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import GHC.Utils.Monad

import GHC.Driver.Session ( givensFuel, wantedsFuel, qcsFuel )

import GHC.Data.Bag

import Data.Maybe ( isJust )
import qualified Data.Semigroup as S

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

canonicalize (CEqCan can_eq_ct) = solveCanonicalEquality can_eq_ct

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

canNC :: CtEvidence -> TcS (StopOrContinue Ct)
canNC ev =
  case classifyPredType pred of
      ClassPred cls tys     -> do traceTcS "canEvNC:cls" (ppr cls <+> ppr tys)
                                  canClassNC ev cls tys
      EqPred eq_rel ty1 ty2 -> do traceTcS "canEvNC:eq" (ppr ty1 $$ ppr ty2)
                                  solveNonCanonicalEquality    ev eq_rel ty1 ty2
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
  = do { dflags <- getDynFlags
       ; sc_cts <- mkStrictSuperClasses (givensFuel dflags) ev [] [] cls tys
                           -- givensFuel dflags: See Note [Expanding Recursive Superclasses and ExpansionFuel]
       ; emitWork sc_cts
       ; canClass ev cls tys doNotExpand }
                           -- doNotExpand: We have already expanded superclasses for /this/ dict
                           -- so set the fuel to doNotExpand to avoid repeating expansion

  | CtWanted { ctev_rewriters = rewriters } <- ev
  , Just ip_name <- isCallStackPred cls tys
  , isPushCallStackOrigin orig
  -- If we're given a CallStack constraint that arose from a function
  -- call, we need to push the current call-site onto the stack instead
  -- of solving it directly from a given.
  -- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
  -- and Note [Solving CallStack constraints] in GHC.Tc.Solver.Types
  = do { -- First we emit a new constraint that will capture the
         -- given CallStack.
         let new_loc = setCtLocOrigin loc (IPOccOrigin (HsIPName ip_name))
                            -- We change the origin to IPOccOrigin so
                            -- this rule does not fire again.
                            -- See Note [Overview of implicit CallStacks]
                            -- in GHC.Tc.Types.Evidence

       ; new_ev <- newWantedEvVarNC new_loc rewriters pred

         -- Then we solve the wanted by pushing the call-site
         -- onto the newly emitted CallStack
       ; let ev_cs = EvCsPushCall (callStackOriginFS orig)
                                  (ctLocSpan loc) (ctEvExpr new_ev)
       ; solveCallStack ev ev_cs

       ; canClass new_ev cls tys doNotExpand
                              -- doNotExpand: No superclasses for class CallStack
                              -- See invariants in CDictCan.cc_pend_sc
       }

  | otherwise
  = do { dflags <- getDynFlags
       ; let fuel | classHasSCs cls = wantedsFuel dflags
                  | otherwise   = doNotExpand
                  -- See Invariants in `CCDictCan.cc_pend_sc`
       ; canClass ev cls tys fuel
       }

  where
    loc  = ctEvLoc ev
    orig = ctLocOrigin loc
    pred = ctEvPred ev

solveCallStack :: CtEvidence -> EvCallStack -> TcS ()
-- Also called from GHC.Tc.Solver when defaulting call stacks
solveCallStack ev ev_cs = do
  -- We're given ev_cs :: CallStack, but the evidence term should be a
  -- dictionary, so we have to coerce ev_cs to a dictionary for
  -- `IP ip CallStack`. See Note [Overview of implicit CallStacks]
  cs_tm <- evCallStack ev_cs
  let ev_tm = mkEvCast cs_tm (wrapIP (ctEvPred ev))
  setEvBindIfWanted ev IsCoherent ev_tm

canClass :: CtEvidence
         -> Class -> [Type]
         -> ExpansionFuel            -- n > 0 <=> un-explored superclasses
         -> TcS (StopOrContinue Ct)
-- Precondition: EvVar is class evidence

canClass ev cls tys pend_sc
  = -- all classes do *nominal* matching
    assertPpr (ctEvRole ev == Nominal) (ppr ev $$ ppr cls $$ ppr tys) $
    do { (redns@(Reductions _ xis), rewriters) <- rewriteArgsNom ev cls_tc tys
       ; let redn@(Reduction _ xi) = mkClassPredRedn cls redns
             mk_ct new_ev = CDictCan { cc_ev = new_ev
                                     , cc_tyargs = xis
                                     , cc_class = cls
                                     , cc_pend_sc = pend_sc }
       ; mb <- rewriteEvidence rewriters ev redn
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

* For wanteds [W], they may give useful
  functional dependencies.  E.g.
     class C a b | a -> b where ...
     class C a b => D a b where ...
  Now a [W] constraint (D Int beta) has (C Int beta) as a superclass
  and that might tell us about beta, via C's fundeps.  We can get this
  by generating a [W] (C Int beta) constraint. We won't use the evidence,
  but it may lead to unification.

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
   and extra Wanteds. Both may help the proof.

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

The cc_pend_sc field in a CDictCan records whether the superclasses of
this constraint have been expanded.  Specifically, in Step 3 we only
expand superclasses for constraints with cc_pend_sc > 0
(i.e. isPendingScDict holds).
See Note [Expanding Recursive Superclasses and ExpansionFuel]

Why do we do this?  Two reasons:

* To avoid repeated work, by repeatedly expanding the superclasses of
  same constraint,

* To terminate the above loop, at least in the -XNoUndecidableSuperClasses
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
G] d2: D a, [G] d3: C a (psc).  (The "psc" means it has its cc_pend_sc has pending
expansion fuel.)
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
    Then adding [W] beta~b will let us solve it.

    -- Example 2 (similar but using a type-equality superclass)
        class (F a ~ b) => C a b
    And try to sllve:
         [G] C a b
         [W] C a beta
    Follow the superclass rules to add
         [G] F a ~ b
         [W] F a ~ beta
    Now we get [W] beta ~ b, and can solve that.

    -- Example (tcfail138)
      class L a b | a -> b
      class (G a, L a b) => C a b

      instance C a b' => G (Maybe a)
      instance C a b  => C (Maybe a) a
      instance L (Maybe a) a

    When solving the superclasses of the (C (Maybe a) a) instance, we get
      [G] C a b, and hence by superclasses, [G] G a, [G] L a b
      [W] G (Maybe a)
    Use the instance decl to get
      [W] C a beta
    Generate its superclass
      [W] L a beta.  Now using fundeps, combine with [G] L a b to get
      [W] beta ~ b
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
   [W] RealOf e ~ fuv
   [W] Num fuv
==>
   e := fuv, Num fuv, Normed fuv, RealOf fuv ~ fuv

While looks exactly like our original constraint. If we add the
superclass of (Normed fuv) again we'd loop.  By adding superclasses
definitely only once, during canonicalisation, this situation can't
happen.

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
-- Returns strict superclasses, transitively, see Note [The superclass story]
-- The loop-breaking here follows Note [Expanding superclasses] in GHC.Tc.Utils.TcType
-- Specifically, for an incoming (C t) constraint, we return all of (C t)'s
--    superclasses, up to /and including/ the first repetition of C
--
-- Example:  class D a => C a
--           class C [a] => D a
-- makeSuperClasses (C x) will return (D x, C [x])
--
-- NB: the incoming constraint's superclass will consume a unit of fuel
-- Preconditions on `cts`: 1. They are either `CDictCan` or `CQuantCan`
--                         2. Their fuel (stored in cc_pend_sc or qci_pend_sc) is > 0
makeSuperClasses cts = concatMapM go cts
  where
    go (CDictCan { cc_ev = ev, cc_class = cls, cc_tyargs = tys, cc_pend_sc = fuel })
      = assertFuelPreconditionStrict fuel $ -- fuel needs to be more than 0 always
        mkStrictSuperClasses fuel ev [] [] cls tys
    go (CQuantCan (QCI { qci_pred = pred, qci_ev = ev, qci_pend_sc = fuel }))
      = assertPpr (isClassPred pred) (ppr pred) $  -- The cts should all have
                                                   -- class pred heads
        assertFuelPreconditionStrict fuel $ -- fuel needs to be more than 0 always
        mkStrictSuperClasses fuel ev tvs theta cls tys
      where
        (tvs, theta, cls, tys) = tcSplitDFunTy (ctEvPred ev)
    go ct = pprPanic "makeSuperClasses" (ppr ct)

mkStrictSuperClasses
    :: ExpansionFuel -> CtEvidence
    -> [TyVar] -> ThetaType  -- These two args are non-empty only when taking
                             -- superclasses of a /quantified/ constraint
    -> Class -> [Type] -> TcS [Ct]
-- Return constraints for the strict superclasses of
--   ev :: forall as. theta => cls tys
-- Precondition: fuel > 0
-- Postcondition: fuel for recursive superclass ct is one unit less than cls fuel
mkStrictSuperClasses fuel ev tvs theta cls tys
  = mk_strict_superclasses (consumeFuel fuel) (unitNameSet (className cls))
                           ev tvs theta cls tys

mk_strict_superclasses :: ExpansionFuel -> NameSet -> CtEvidence
                       -> [TyVar] -> ThetaType
                       -> Class -> [Type] -> TcS [Ct]
-- Always return the immediate superclasses of (cls tys);
-- and expand their superclasses, provided none of them are in rec_clss
-- nor are repeated
-- The caller of this function is supposed to perform fuel book keeping
-- Precondition: fuel >= 0
mk_strict_superclasses fuel rec_clss (CtGiven { ctev_evar = evar, ctev_loc = loc })
                       tvs theta cls tys
  = concatMapM do_one_given $
    classSCSelIds cls
  where
    dict_ids  = mkTemplateLocals theta
    this_size = pSizeClassPred cls tys

    do_one_given sel_id
      | isUnliftedType sc_pred
         -- NB: class superclasses are never representation-polymorphic,
         -- so isUnliftedType is OK here.
      , not (null tvs && null theta)
      = -- See Note [Equality superclasses in quantified constraints]
        return []
      | otherwise
      = do { given_ev <- newGivenEvVar sc_loc $
                         mk_given_desc sel_id sc_pred
           ; assertFuelPrecondition fuel
             $ mk_superclasses fuel rec_clss given_ev tvs theta sc_pred }
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

    sc_loc | isCTupleClass cls
           = loc   -- For tuple predicates, just take them apart, without
                   -- adding their (large) size into the chain.  When we
                   -- get down to a base predicate, we'll include its size.
                   -- #10335

           |  isEqPredClass cls || cls `hasKey` coercibleTyConKey
           = loc   -- The only superclasses of ~, ~~, and Coercible are primitive
                   -- equalities, and they don't use the GivenSCOrigin mechanism
                   -- detailed in Note [Solving superclass constraints] in
                   -- GHC.Tc.TyCl.Instance. Skip for a tiny performance win.

           | otherwise
           = loc { ctl_origin = mk_sc_origin (ctLocOrigin loc) }

    -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
    -- for explanation of GivenSCOrigin and Note [Replacement vs keeping] in
    -- GHC.Tc.Solver.Interact for why we need depths
    mk_sc_origin :: CtOrigin -> CtOrigin
    mk_sc_origin (GivenSCOrigin skol_info sc_depth already_blocked)
      = GivenSCOrigin skol_info (sc_depth + 1)
                      (already_blocked || newly_blocked skol_info)

    mk_sc_origin (GivenOrigin skol_info)
      = -- These cases do not already have a superclass constraint: depth starts at 1
        GivenSCOrigin skol_info 1 (newly_blocked skol_info)

    mk_sc_origin other_orig = pprPanic "Given constraint without given origin" $
                              ppr evar $$ ppr other_orig

    newly_blocked (InstSkol _ head_size) = isJust (this_size `ltPatersonSize` head_size)
    newly_blocked _                      = False

mk_strict_superclasses fuel rec_clss ev tvs theta cls tys
  | all noFreeVarsOfType tys
  = return [] -- Wanteds with no variables yield no superclass constraints.
              -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted case, just add Wanted superclasses
              -- that can lead to improvement.
  = assertPpr (null tvs && null theta) (ppr tvs $$ ppr theta) $
    concatMapM do_one (immSuperClasses cls tys)
  where
    loc = ctEvLoc ev `updateCtLocOrigin` WantedSuperclassOrigin (ctEvPred ev)

    do_one sc_pred
      = do { traceTcS "mk_strict_superclasses Wanted" (ppr (mkClassPred cls tys) $$ ppr sc_pred)
           ; sc_ev <- newWantedNC loc (ctEvRewriters ev) sc_pred
           ; mk_superclasses fuel rec_clss sc_ev [] [] sc_pred }

{- Note [Improvement from Ground Wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose class C b a => D a b
and consider
  [W] D Int Bool
Is there any point in emitting [W] C Bool Int?  No!  The only point of
emitting superclass constraints for W constraints is to get
improvement, extra unifications that result from functional
dependencies.  See Note [Why adding superclasses can help] above.

But no variables means no improvement; case closed.
-}

mk_superclasses :: ExpansionFuel -> NameSet -> CtEvidence
                -> [TyVar] -> ThetaType -> PredType -> TcS [Ct]
-- Return this constraint, plus its superclasses, if any
-- Precondition: fuel >= 0
mk_superclasses fuel rec_clss ev tvs theta pred
  | ClassPred cls tys <- classifyPredType pred
  = assertFuelPrecondition fuel $
    mk_superclasses_of fuel rec_clss ev tvs theta cls tys

  | otherwise   -- Superclass is not a class predicate
  = return [mkNonCanonical ev]

mk_superclasses_of :: ExpansionFuel -> NameSet -> CtEvidence
                   -> [TyVar] -> ThetaType -> Class -> [Type]
                   -> TcS [Ct]
-- Always return this class constraint,
-- and expand its superclasses
-- Precondition: fuel >= 0
mk_superclasses_of fuel rec_clss ev tvs theta cls tys
  | loop_found = do { traceTcS "mk_superclasses_of: loop" (ppr cls <+> ppr tys)
                    ; assertFuelPrecondition fuel $ return [mk_this_ct fuel] }
                                                  -- cc_pend_sc of returning ct = fuel
  | otherwise  = do { traceTcS "mk_superclasses_of" (vcat [ ppr cls <+> ppr tys
                                                          , ppr (isCTupleClass cls)
                                                          , ppr rec_clss
                                                          ])
                    ; sc_cts <- assertFuelPrecondition fuel $
                                mk_strict_superclasses fuel rec_clss' ev tvs theta cls tys
                    ; return (mk_this_ct doNotExpand : sc_cts) }
                                      -- doNotExpand: we have expanded this cls's superclasses, so
                                      -- exhaust the associated constraint's fuel,
                                      -- to avoid duplicate work
  where
    cls_nm     = className cls
    loop_found = not (isCTupleClass cls) && cls_nm `elemNameSet` rec_clss
                 -- Tuples never contribute to recursion, and can be nested
    rec_clss'  = rec_clss `extendNameSet` cls_nm
    mk_this_ct :: ExpansionFuel -> Ct
    mk_this_ct fuel | null tvs, null theta
                    = CDictCan { cc_ev = ev, cc_class = cls, cc_tyargs = tys
                               , cc_pend_sc = fuel }
                    -- NB: If there is a loop, we cut off, so we have not
                    --     added the superclasses, hence cc_pend_sc = fuel
                    | otherwise
                    = CQuantCan (QCI { qci_tvs = tvs, qci_pred = mkClassPred cls tys
                                     , qci_ev = ev
                                     , qci_pend_sc = fuel })


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
       ; (redn, rewriters) <- rewrite ev pred
       ; rewriteEvidence rewriters ev redn `andWhenContinue` \ new_ev ->

    do { -- Re-classify, in case rewriting has improved its shape
         -- Code is like the canNC, except
         -- that the IrredPred branch stops work
       ; case classifyPredType (ctEvPred new_ev) of
           ClassPred cls tys     -> canClassNC new_ev cls tys
           EqPred eq_rel ty1 ty2 -> -- IrredPreds have kind Constraint, so
                                    -- cannot become EqPreds
                                    pprPanic "canIrred: EqPred"
                                      (ppr ev $$ ppr eq_rel $$ ppr ty1 $$ ppr ty2)
           ForAllPred tvs th p   -> -- this is highly suspect; Quick Look
                                    -- should never leave a meta-var filled
                                    -- in with a polytype. This is #18987.
                                    do traceTcS "canEvNC:forall" (ppr pred)
                                       canForAllNC ev tvs th p
           IrredPred {}          -> continueWith $
                                    mkIrredCt IrredShapeReason new_ev } }

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
  = do { dflags <- getDynFlags
       ; sc_cts <- mkStrictSuperClasses (givensFuel dflags) ev tvs theta cls tys
       -- givensFuel dflags: See Note [Expanding Recursive Superclasses and ExpansionFuel]
       ; emitWork sc_cts
       ; canForAll ev doNotExpand }
       -- doNotExpand: as we have already (eagerly) expanded superclasses for this class

  | otherwise
  = do { dflags <- getDynFlags
       ; let fuel | Just (cls, _) <- cls_pred_tys_maybe
                  , classHasSCs cls = qcsFuel dflags
                  -- See invariants (a) and (b) in QCI.qci_pend_sc
                  -- qcsFuel dflags: See Note [Expanding Recursive Superclasses and ExpansionFuel]
                  -- See Note [Quantified constraints]
                  | otherwise = doNotExpand
       ; canForAll ev fuel }
  where
    cls_pred_tys_maybe = getClassPredTys_maybe pred

canForAll :: CtEvidence -> ExpansionFuel -> TcS (StopOrContinue Ct)
-- We have a constraint (forall as. blah => C tys)
canForAll ev fuel
  = do { -- First rewrite it to apply the current substitution
         let pred = ctEvPred ev
       ; (redn, rewriters) <- rewrite ev pred
       ; rewriteEvidence rewriters ev redn `andWhenContinue` \ new_ev ->

    do { -- Now decompose into its pieces and solve it
         -- (It takes a lot less code to rewrite before decomposing.)
       ; case classifyPredType (ctEvPred new_ev) of
           ForAllPred tvs theta pred
              -> solveForAll new_ev tvs theta pred fuel
           _  -> pprPanic "canForAll" (ppr new_ev)
    } }

solveForAll :: CtEvidence -> [TyVar] -> TcThetaType -> PredType -> ExpansionFuel
            -> TcS (StopOrContinue Ct)
solveForAll ev@(CtWanted { ctev_dest = dest, ctev_rewriters = rewriters, ctev_loc = loc })
            tvs theta pred _fuel
  = -- See Note [Solving a Wanted forall-constraint]
    setLclEnv (ctLocEnv loc) $
    -- This setLclEnv is important: the emitImplicationTcS uses that
    -- TcLclEnv for the implication, and that in turn sets the location
    -- for the Givens when solving the constraint (#21006)
    do { let empty_subst = mkEmptySubst $ mkInScopeSet $
                           tyCoVarsOfTypes (pred:theta) `delVarSetList` tvs
             is_qc = IsQC (ctLocOrigin loc)

         -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
         --           in GHC.Tc.Utils.TcType
         -- Very like the code in tcSkolDFunType
       ; rec { skol_info <- mkSkolemInfo skol_info_anon
             ; (subst, skol_tvs) <- tcInstSkolTyVarsX skol_info empty_subst tvs
             ; let inst_pred  = substTy    subst pred
                   inst_theta = substTheta subst theta
                   skol_info_anon = InstSkol is_qc (get_size inst_pred) }

       ; given_ev_vars <- mapM newEvVar inst_theta
       ; (lvl, (w_id, wanteds))
             <- pushLevelNoWorkList (ppr skol_info) $
                do { let loc' = setCtLocOrigin loc (ScOrigin is_qc NakedSc)
                         -- Set the thing to prove to have a ScOrigin, so we are
                         -- careful about its termination checks.
                         -- See (QC-INV) in Note [Solving a Wanted forall-constraint]
                   ; wanted_ev <- newWantedEvVarNC loc' rewriters inst_pred
                   ; return ( ctEvEvId wanted_ev
                            , unitBag (mkNonCanonical wanted_ev)) }

      ; ev_binds <- emitImplicationTcS lvl (getSkolemInfo skol_info) skol_tvs
                                       given_ev_vars wanteds

      ; setWantedEvTerm dest IsCoherent $
        EvFun { et_tvs = skol_tvs, et_given = given_ev_vars
              , et_binds = ev_binds, et_body = w_id }

      ; stopWith ev "Wanted forall-constraint" }
  where
    -- Getting the size of the head is a bit horrible
    -- because of the special treament for class predicates
    get_size pred = case classifyPredType pred of
                      ClassPred cls tys -> pSizeClassPred cls tys
                      _                 -> pSizeType pred

 -- See Note [Solving a Given forall-constraint]
solveForAll ev@(CtGiven {}) tvs _theta pred fuel
  = do { addInertForAll qci
       ; stopWith ev "Given forall-constraint" }
  where
    qci = QCI { qci_ev = ev, qci_tvs = tvs
              , qci_pred = pred, qci_pend_sc = fuel }

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

The tricky point is about termination: see #19690.  We want to maintain
the invariant (QC-INV):

  (QC-INV) Every quantified constraint returns a non-bottom dictionary

just as every top-level instance declaration guarantees to return a non-bottom
dictionary.  But as #19690 shows, it is possible to get a bottom dicionary
by superclass selection if we aren't careful.  The situation is very similar
to that described in Note [Recursive superclasses] in GHC.Tc.TyCl.Instance;
and we use the same solution:

* Give the Givens a CtOrigin of (GivenOrigin (InstSkol IsQC head_size))
* Give the Wanted a CtOrigin of (ScOrigin IsQC NakedSc)

Both of these things are done in solveForAll.  Now the mechanism described
in Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance takes over.

Note [Solving a Given forall-constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a Given constraint
  [G] df :: forall ab. (Eq a, Ord b) => C x a b
we just add it to TcS's local InstEnv of known instances,
via addInertForall.  Then, if we look up (C x Int Bool), say,
we'll find a match in the InstEnv.


************************************************************************
*                                                                      *
                  Evidence transformation
*                                                                      *
************************************************************************
-}

rewriteEvidence :: RewriterSet  -- ^ See Note [Wanteds rewrite Wanteds]
                                -- in GHC.Tc.Types.Constraint
                -> CtEvidence   -- ^ old evidence
                -> Reduction    -- ^ new predicate + coercion, of type <type of old evidence> ~ new predicate
                -> TcS (StopOrContinue CtEvidence)
-- Returns Just new_ev iff either (i)  'co' is reflexivity
--                             or (ii) 'co' is not reflexivity, and 'new_pred' not cached
-- In either case, there is nothing new to do with new_ev
{-
     rewriteEvidence old_ev new_pred co
Main purpose: create new evidence for new_pred;
              unless new_pred is cached already
* Returns a new_ev : new_pred, with same wanted/given flag as old_ev
* If old_ev was wanted, create a binding for old_ev, in terms of new_ev
* If old_ev was given, AND not cached, create a binding for new_ev, in terms of old_ev
* Returns Nothing if new_ev is already cached

        Old evidence    New predicate is               Return new evidence
        flavour                                        of same flavor
        -------------------------------------------------------------------
        Wanted          Already solved or in inert     Nothing
                        Not                            Just new_evidence

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

If we are rewriting with Refl, then there are no new rewriters to add to
the rewriter set. We check this with an assertion.
 -}


rewriteEvidence rewriters old_ev (Reduction co new_pred)
  | isReflCo co -- See Note [Rewriting with Refl]
  = assert (isEmptyRewriterSet rewriters) $
    continueWith (setCtEvPredType old_ev new_pred)

rewriteEvidence rewriters ev@(CtGiven { ctev_evar = old_evar, ctev_loc = loc })
                (Reduction co new_pred)
  = assert (isEmptyRewriterSet rewriters) $ -- this is a Given, not a wanted
    do { new_ev <- newGivenEvVar loc (new_pred, new_tm)
       ; continueWith new_ev }
  where
    -- mkEvCast optimises ReflCo
    new_tm = mkEvCast (evId old_evar)
                (downgradeRole Representational (ctEvRole ev) co)

rewriteEvidence new_rewriters
                ev@(CtWanted { ctev_dest = dest
                             , ctev_loc = loc
                             , ctev_rewriters = rewriters })
                (Reduction co new_pred)
  = do { mb_new_ev <- newWanted loc rewriters' new_pred
       ; massert (coercionRole co == ctEvRole ev)
       ; setWantedEvTerm dest IsCoherent $
            mkEvCast (getEvExpr mb_new_ev)
                     (downgradeRole Representational (ctEvRole ev) (mkSymCo co))
       ; case mb_new_ev of
            Fresh  new_ev -> continueWith new_ev
            Cached _      -> stopWith ev "Cached wanted" }
  where
    rewriters' = rewriters S.<> new_rewriters
