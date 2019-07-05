{-# LANGUAGE CPP #-}

module TcCanonical(
     canonicalize,
     unifyDerived,
     makeSuperClasses, maybeSym,
     StopOrContinue(..), stopWith, continueWith,
     solveCallStack    -- For TcSimplify
  ) where

#include "HsVersions.h"

import GhcPrelude

import TcRnTypes
import TcUnify( swapOverTyVars, metaTyVarUpdateOK )
import TcType
import Type
import TcFlatten
import TcSMonad
import TcEvidence
import TcEvTerm
import Class
import TyCon
import TyCoRep   -- cleverly decomposes types, good for completeness checking
import Coercion
import CoreSyn
import Id( idType, mkTemplateLocals )
import FamInstEnv ( FamInstEnvs )
import FamInst ( tcTopNormaliseNewTypeTF_maybe )
import Var
import VarEnv( mkInScopeSet )
import VarSet( delVarSetList )
import Outputable
import DynFlags( DynFlags )
import NameSet
import RdrName
import HsTypes( HsIPName(..) )

import Pair
import Util
import Bag
import MonadUtils
import Control.Monad
import Data.Maybe ( isJust )
import Data.List  ( zip4 )
import BasicTypes

import Data.Bifunctor ( bimap )

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
CNonCanonicals (except for CHoleCans, arising from holes). We know nothing
about these constraints. So, first:

     Classify CNonCanoncal constraints, depending on whether they
     are equalities, class predicates, or other.

Then proceed depending on the shape of the constraint. Generally speaking,
each constraint gets flattened and then decomposed into one of several forms
(see type Ct in TcRnTypes).

When an already-canonicalized constraint gets kicked out of the inert set,
it must be recanonicalized. But we know a bit about its shape from the
last time through, so we can skip the classification step.

-}

-- Top-level canonicalization
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

canonicalize :: Ct -> TcS (StopOrContinue Ct)
canonicalize (CNonCanonical { cc_ev = ev })
  = {-# SCC "canNC" #-}
    case classifyPredType pred of
      ClassPred cls tys     -> do traceTcS "canEvNC:cls" (ppr cls <+> ppr tys)
                                  canClassNC ev cls tys
      EqPred eq_rel ty1 ty2 -> do traceTcS "canEvNC:eq" (ppr ty1 $$ ppr ty2)
                                  canEqNC    ev eq_rel ty1 ty2
      IrredPred {}          -> do traceTcS "canEvNC:irred" (ppr pred)
                                  canIrred ev
      ForAllPred _ _ pred   -> do traceTcS "canEvNC:forall" (ppr pred)
                                  canForAll ev (isClassPred pred)
  where
    pred = ctEvPred ev

canonicalize (CQuantCan (QCI { qci_ev = ev, qci_pend_sc = pend_sc }))
  = canForAll ev pend_sc

canonicalize (CIrredCan { cc_ev = ev })
  | EqPred eq_rel ty1 ty2 <- classifyPredType (ctEvPred ev)
  = -- For insolubles (all of which are equalities, do /not/ flatten the arguments
    -- In Trac #14350 doing so led entire-unnecessary and ridiculously large
    -- type function expansion.  Instead, canEqNC just applies
    -- the substitution to the predicate, and may do decomposition;
    --    e.g. a ~ [a], where [G] a ~ [Int], can decompose
    canEqNC ev eq_rel ty1 ty2

  | otherwise
  = canIrred ev

canonicalize (CDictCan { cc_ev = ev, cc_class  = cls
                       , cc_tyargs = xis, cc_pend_sc = pend_sc })
  = {-# SCC "canClass" #-}
    canClass ev cls xis pend_sc

canonicalize (CTyEqCan { cc_ev = ev
                       , cc_tyvar  = tv
                       , cc_rhs    = xi
                       , cc_eq_rel = eq_rel })
  = {-# SCC "canEqLeafTyVarEq" #-}
    canEqNC ev eq_rel (mkTyVarTy tv) xi
      -- NB: Don't use canEqTyVar because that expects flattened types,
      -- and tv and xi may not be flat w.r.t. an updated inert set

canonicalize (CFunEqCan { cc_ev = ev
                        , cc_fun    = fn
                        , cc_tyargs = xis1
                        , cc_fsk    = fsk })
  = {-# SCC "canEqLeafFunEq" #-}
    canCFunEqCan ev fn xis1 fsk

canonicalize (CHoleCan { cc_ev = ev, cc_hole = hole })
  = canHole ev hole

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
  -- See Note [Overview of implicit CallStacks] in TcEvidence
  -- and Note [Solving CallStack constraints] in TcSMonad
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
-- Also called from TcSimplify when defaulting call stacks
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
    do { (xis, cos, _kind_co) <- flattenArgsNom ev cls_tc tys
       ; MASSERT( isTcReflCo _kind_co )
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
  number of them.  Here is a real-life example (Trac #10318);

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
   termination condition: see Note [Expanding superclasses] in TcType.

2. Solve the wanteds as usual, but do no further expansion of
   superclasses for canonical CDictCans in solveSimpleGivens or
   solveSimpleWanteds; Note [Danger of adding superclasses during solving]

   However, /do/ continue to eagerly expand superlasses for new /given/
   /non-canonical/ constraints (canClassNC does this).  As Trac #12175
   showed, a type-family application can expand to a class constraint,
   and we want to see its superclasses for just the same reason as
   Note [Eagerly expand given superclasses].

3. If we have any remaining unsolved wanteds
        (see Note [When superclasses help] in TcRnTypes)
   try harder: take both the Givens and Wanteds, and expand
   superclasses again.  See the calls to expandSuperClasses in
   TcSimplify.simpl_loop and solveWanteds.

   This may succeed in generating (a finite number of) extra Givens,
   and extra Deriveds. Both may help the proof.

3a An important wrinkle: only expand Givens from the current level.
   Two reasons:
      - We only want to expand it once, and that is best done at
        the level it is bound, rather than repeatedly at the leaves
        of the implication tree
      - We may be inside a type where we can't create term-level
        evidence anyway, so we can't superclass-expand, say,
        (a ~ b) to get (a ~# b).  This happened in Trac #15290.

4. Go round to (2) again.  This loop (2,3,4) is implemented
   in TcSimplify.simpl_loop.

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
TcInteract.  So d3 dies a quick, happy death.

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

Note [Instance and Given overlap] in TcInteract.

We also want to do this if we have

   f :: F (T a) => blah

where
   type instance F (T a) = Ord (T a)

So we may need to do a little work on the givens to expose the
class that has the superclasses.  That's why the superclass
expansion for Givens happens in canClassNC.

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
Here's a serious, but now out-dated example, from Trac #4497:

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
  -}

makeSuperClasses :: [Ct] -> TcS [Ct]
-- Returns strict superclasses, transitively, see Note [The superclasses story]
-- See Note [The superclass story]
-- The loop-breaking here follows Note [Expanding superclasses] in TcType
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
mk_strict_superclasses rec_clss ev tvs theta cls tys
  | CtGiven { ctev_evar = evar, ctev_loc = loc } <- ev
  = concatMapM (do_one_given evar (mk_given_loc loc)) $
    classSCSelIds cls
  where
    dict_ids  = mkTemplateLocals theta
    size      = sizeTypes tys

    do_one_given evar given_loc sel_id
      | isUnliftedType sc_pred
      , not (null tvs && null theta)
      = -- See Note [Equality superclasses in quantified constraints]
        return []
      | otherwise
      = do { given_ev <- newGivenEvVar given_loc $
                         (given_ty, mk_sc_sel evar sel_id)
           ; mk_superclasses rec_clss given_ev tvs theta sc_pred }
      where
        sc_pred  = funResultTy (piResultTys (idType sel_id) tys)
        given_ty = mkInfSigmaTy tvs theta sc_pred

    mk_sc_sel evar sel_id
      = EvExpr $ mkLams tvs $ mkLams dict_ids $
        Var sel_id `mkTyApps` tys `App`
        (evId evar `mkTyApps` mkTyVarTys tvs `mkVarApps` dict_ids)

    mk_given_loc loc
       | isCTupleClass cls
       = loc   -- For tuple predicates, just take them apart, without
               -- adding their (large) size into the chain.  When we
               -- get down to a base predicate, we'll include its size.
               -- Trac #10335

       | GivenOrigin skol_info <- ctLocOrigin loc
         -- See Note [Solving superclass constraints] in TcInstDcls
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
Consider (Trac #15359, #15593, #15625)
  f :: (forall a. theta => a ~ b) => stuff

It's a bit odd to have a local, quantified constraint for `(a~b)`,
but some people want such a thing (see the tickets). And for
Coercible it is definitely useful
  f :: forall m. (forall p q. Coercible p q => Coercible (m p) (m q)))
                 => stuff

Moreover it's not hard to arrange; we just need to look up /equality/
constraints in the quantified-constraint environment, which we do in
TcInteract.doTopReactOther.

There is a wrinkle though, in the case where 'theta' is empty, so
we have
  f :: (forall a. a~b) => stuff

Now, potentially, the superclass machinery kicks in, in
makeSuperClasses, giving us a a second quantified constrait
       (forall a. a ~# b)
BUT this is an unboxed value!  And nothing has prepared us for
dictionary "functions" that are unboxed.  Actually it does just
about work, but the simplier ends up with stuff like
   case (/\a. eq_sel d) of df -> ...(df @Int)...
and fails to simplify that any further.  And it doesn't satisfy
isPredTy any more.

So for now we simply decline to take superclasses in the quantified
case.  Instead we have a special case in TcInteract.doTopReactOther,
which looks for primitive equalities specially in the quantified
constraints.

See also Note [Evidence for quantified constraints] in Type.


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
       ; (xi,co) <- flatten FM_FlattenAll ev pred -- co :: xi ~ pred
       ; rewriteEvidence ev xi co `andWhenContinue` \ new_ev ->
    do { -- Re-classify, in case flattening has improved its shape
       ; case classifyPredType (ctEvPred new_ev) of
           ClassPred cls tys     -> canClassNC new_ev cls tys
           EqPred eq_rel ty1 ty2 -> canEqNC new_ev eq_rel ty1 ty2
           _                     -> continueWith $
                                    mkIrredCt new_ev } }

canHole :: CtEvidence -> Hole -> TcS (StopOrContinue Ct)
canHole ev hole
  = do { let pred = ctEvPred ev
       ; (xi,co) <- flatten FM_SubstOnly ev pred -- co :: xi ~ pred
       ; rewriteEvidence ev xi co `andWhenContinue` \ new_ev ->
    do { updInertIrreds (`snocCts` (CHoleCan { cc_ev = new_ev
                                             , cc_hole = hole }))
       ; stopWith new_ev "Emit insoluble hole" } }


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
  https://ghc.haskell.org/trac/ghc/wiki/QuantifiedConstraints
which in turn contains a link to the GHC Proposal where the change
is specified, and a Haskell Symposium paper about it.

We implement two main extensions to the design in the paper:

 1. We allow a variable in the instance head, e.g.
      f :: forall m a. (forall b. m b) => D (m a)
    Notice the 'm' in the head of the quantified constraint, not
    a class.

 2. We suport superclasses to quantified constraints.
    For example (contrived):
      f :: (Ord b, forall b. Ord b => Ord (m b)) => m a -> m a -> Bool
      f x y = x==y
    Here we need (Eq (m a)); but the quantifed constraint deals only
    with Ord.  But we can make it work by using its superclass.

Here are the moving parts
  * Language extension {-# LANGUAGE QuantifiedConstraints #-}
    and add it to ghc-boot-th:GHC.LanguageExtensions.Type.Extension

  * A new form of evidence, EvDFun, that is used to discharge
    such wanted constraints

  * checkValidType gets some changes to accept forall-constraints
    only in the right places.

  * Type.PredTree gets a new constructor ForAllPred, and
    and classifyPredType analyses a PredType to decompose
    the new forall-constraints

  * TcSMonad.InertCans gets an extra field, inert_insts,
    which holds all the Given forall-constraints.  In effect,
    such Given constraints are like local instance decls.

  * When trying to solve a class constraint, via
    TcInteract.matchInstEnv, use the InstEnv from inert_insts
    so that we include the local Given forall-constraints
    in the lookup.  (See TcSMonad.getInstEnvs.)

  * TcCanonical.canForAll deals with solving a
    forall-constraint.  See
       Note [Solving a Wanted forall-constraint]

  * We augment the kick-out code to kick out an inert
    forall constraint if it can be rewritten by a new
    type equality; see TcSMonad.kick_out_rewritable

Note that a quantified constraint is never /inferred/
(by TcSimplify.simplifyInfer).  A function can only have a
quantified constraint in its type if it is given an explicit
type signature.

Note that we implement
-}

canForAll :: CtEvidence -> Bool -> TcS (StopOrContinue Ct)
-- We have a constraint (forall as. blah => C tys)
canForAll ev pend_sc
  = do { -- First rewrite it to apply the current substitution
         -- Do not bother with type-family reductions; we can't
         -- do them under a forall anyway (c.f. Flatten.flatten_one
         -- on a forall type)
         let pred = ctEvPred ev
       ; (xi,co) <- flatten FM_SubstOnly ev pred -- co :: xi ~ pred
       ; rewriteEvidence ev xi co `andWhenContinue` \ new_ev ->

    do { -- Now decompose into its pieces and solve it
         -- (It takes a lot less code to flatten before decomposing.)
       ; case classifyPredType (ctEvPred new_ev) of
           ForAllPred tv_bndrs theta pred
              -> solveForAll new_ev tv_bndrs theta pred pend_sc
           _  -> pprPanic "canForAll" (ppr new_ev)
    } }

solveForAll :: CtEvidence -> [TyVarBinder] -> TcThetaType -> PredType -> Bool
            -> TcS (StopOrContinue Ct)
solveForAll ev tv_bndrs theta pred pend_sc
  | CtWanted { ctev_dest = dest } <- ev
  = -- See Note [Solving a Wanted forall-constraint]
    do { let skol_info = QuantCtxtSkol
             empty_subst = mkEmptyTCvSubst $ mkInScopeSet $
                           tyCoVarsOfTypes (pred:theta) `delVarSetList` tvs
       ; (subst, skol_tvs) <- tcInstSkolTyVarsX empty_subst tvs
       ; given_ev_vars <- mapM newEvVar (substTheta subst theta)

       ; (w_id, ev_binds)
             <- checkConstraintsTcS skol_info skol_tvs given_ev_vars $
                do { wanted_ev <- newWantedEvVarNC loc $
                                  substTy subst pred
                   ; return ( ctEvEvId wanted_ev
                            , unitBag (mkNonCanonical wanted_ev)) }

      ; setWantedEvTerm dest $
        EvFun { et_tvs = skol_tvs, et_given = given_ev_vars
              , et_binds = ev_binds, et_body = w_id }

      ; stopWith ev "Wanted forall-constraint" }

  | isGiven ev   -- See Note [Solving a Given forall-constraint]
  = do { addInertForAll qci
       ; stopWith ev "Given forall-constraint" }

  | otherwise
  = stopWith ev "Derived forall-constraint"
  where
    loc = ctEvLoc ev
    tvs = binderVars tv_bndrs
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
types may look dissimilar before flattening but similar after flattening.
However, we don't just want to jump in and flatten right away, because
this might be wasted effort. So, after looking for similarities and failing,
we flatten and then try again. Of course, we don't want to loop, so we
track whether or not we've already flattened.

It is conceivable to do a better job at tracking whether or not a type
is flattened, but this is left as future work. (Mar '15)


Note [FunTy and decomposing tycon applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When can_eq_nc' attempts to decompose a tycon application we haven't yet zonked.
This means that we may very well have a FunTy containing a type of some unknown
kind. For instance, we may have,

    FunTy (a :: k) Int

Where k is a unification variable. tcRepSplitTyConApp_maybe panics in the event
that it sees such a type as it cannot determine the RuntimeReps which the (->)
is applied to. Consequently, it is vital that we instead use
tcRepSplitTyConApp_maybe', which simply returns Nothing in such a case.

When this happens can_eq_nc' will fail to decompose, zonk, and try again.
Zonking should fill the variable k, meaning that decomposition will succeed the
second time around.
-}

canEqNC :: CtEvidence -> EqRel -> Type -> Type -> TcS (StopOrContinue Ct)
canEqNC ev eq_rel ty1 ty2
  = do { result <- zonk_eq_types ty1 ty2
       ; case result of
           Left (Pair ty1' ty2') -> can_eq_nc False ev eq_rel ty1' ty1 ty2' ty2
           Right ty              -> canEqReflexive ev eq_rel ty }

can_eq_nc
   :: Bool            -- True => both types are flat
   -> CtEvidence
   -> EqRel
   -> Type -> Type    -- LHS, after and before type-synonym expansion, resp
   -> Type -> Type    -- RHS, after and before type-synonym expansion, resp
   -> TcS (StopOrContinue Ct)
can_eq_nc flat ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  = do { traceTcS "can_eq_nc" $
         vcat [ ppr flat, ppr ev, ppr eq_rel, ppr ty1, ppr ps_ty1, ppr ty2, ppr ps_ty2 ]
       ; rdr_env <- getGlobalRdrEnvTcS
       ; fam_insts <- getFamInstEnvs
       ; can_eq_nc' flat rdr_env fam_insts ev eq_rel ty1 ps_ty1 ty2 ps_ty2 }

can_eq_nc'
   :: Bool           -- True => both input types are flattened
   -> GlobalRdrEnv   -- needed to see which newtypes are in scope
   -> FamInstEnvs    -- needed to unwrap data instances
   -> CtEvidence
   -> EqRel
   -> Type -> Type    -- LHS, after and before type-synonym expansion, resp
   -> Type -> Type    -- RHS, after and before type-synonym expansion, resp
   -> TcS (StopOrContinue Ct)

-- Expand synonyms first; see Note [Type synonyms and canonicalization]
can_eq_nc' flat _rdr_env _envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | Just ty1' <- tcView ty1 = can_eq_nc flat ev eq_rel ty1' ps_ty1 ty2  ps_ty2
  | Just ty2' <- tcView ty2 = can_eq_nc flat ev eq_rel ty1  ps_ty1 ty2' ps_ty2

-- need to check for reflexivity in the ReprEq case.
-- See Note [Eager reflexivity check]
-- Check only when flat because the zonk_eq_types check in canEqNC takes
-- care of the non-flat case.
can_eq_nc' True _rdr_env _envs ev ReprEq ty1 _ ty2 _
  | ty1 `tcEqType` ty2
  = canEqReflexive ev ReprEq ty1

-- When working with ReprEq, unwrap newtypes.
-- See Note [Unwrap newtypes first]
can_eq_nc' _flat rdr_env envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | ReprEq <- eq_rel
  , Just stuff1 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty1
  = can_eq_newtype_nc ev NotSwapped ty1 stuff1 ty2 ps_ty2

  | ReprEq <- eq_rel
  , Just stuff2 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty2
  = can_eq_newtype_nc ev IsSwapped  ty2 stuff2 ty1 ps_ty1

-- Then, get rid of casts
can_eq_nc' flat _rdr_env _envs ev eq_rel (CastTy ty1 co1) _ ty2 ps_ty2
  = canEqCast flat ev eq_rel NotSwapped ty1 co1 ty2 ps_ty2
can_eq_nc' flat _rdr_env _envs ev eq_rel ty1 ps_ty1 (CastTy ty2 co2) _
  = canEqCast flat ev eq_rel IsSwapped ty2 co2 ty1 ps_ty1

-- NB: pattern match on True: we want only flat types sent to canEqTyVar.
-- See also Note [No top-level newtypes on RHS of representational equalities]
can_eq_nc' True _rdr_env _envs ev eq_rel (TyVarTy tv1) ps_ty1 ty2 ps_ty2
  = canEqTyVar ev eq_rel NotSwapped tv1 ps_ty1 ty2 ps_ty2
can_eq_nc' True _rdr_env _envs ev eq_rel ty1 ps_ty1 (TyVarTy tv2) ps_ty2
  = canEqTyVar ev eq_rel IsSwapped tv2 ps_ty2 ty1 ps_ty1

----------------------
-- Otherwise try to decompose
----------------------

-- Literals
can_eq_nc' _flat _rdr_env _envs ev eq_rel ty1@(LitTy l1) _ (LitTy l2) _
 | l1 == l2
  = do { setEvBindIfWanted ev (evCoercion $ mkReflCo (eqRelRole eq_rel) ty1)
       ; stopWith ev "Equal LitTy" }

-- Try to decompose type constructor applications
-- Including FunTy (s -> t)
can_eq_nc' _flat _rdr_env _envs ev eq_rel ty1 _ ty2 _
    --- See Note [FunTy and decomposing type constructor applications].
  | Just (tc1, tys1) <- tcRepSplitTyConApp_maybe' ty1
  , Just (tc2, tys2) <- tcRepSplitTyConApp_maybe' ty2
  , not (isTypeFamilyTyCon tc1)
  , not (isTypeFamilyTyCon tc2)
  = canTyConApp ev eq_rel tc1 tys1 tc2 tys2

can_eq_nc' _flat _rdr_env _envs ev eq_rel
           s1@(ForAllTy {}) _ s2@(ForAllTy {}) _
  = can_eq_nc_forall ev eq_rel s1 s2

-- See Note [Canonicalising type applications] about why we require flat types
can_eq_nc' True _rdr_env _envs ev eq_rel (AppTy t1 s1) _ ty2 _
  | NomEq <- eq_rel
  , Just (t2, s2) <- tcSplitAppTy_maybe ty2
  = can_eq_app ev t1 s1 t2 s2
can_eq_nc' True _rdr_env _envs ev eq_rel ty1 _ (AppTy t2 s2) _
  | NomEq <- eq_rel
  , Just (t1, s1) <- tcSplitAppTy_maybe ty1
  = can_eq_app ev t1 s1 t2 s2

-- No similarity in type structure detected. Flatten and try again.
can_eq_nc' False rdr_env envs ev eq_rel _ ps_ty1 _ ps_ty2
  = do { (xi1, co1) <- flatten FM_FlattenAll ev ps_ty1
       ; (xi2, co2) <- flatten FM_FlattenAll ev ps_ty2
       ; new_ev <- rewriteEqEvidence ev NotSwapped xi1 xi2 co1 co2
       ; can_eq_nc' True rdr_env envs new_ev eq_rel xi1 xi1 xi2 xi2 }

-- We've flattened and the types don't match. Give up.
can_eq_nc' True _rdr_env _envs ev eq_rel _ ps_ty1 _ ps_ty2
  = do { traceTcS "can_eq_nc' catch-all case" (ppr ps_ty1 $$ ppr ps_ty2)
       ; case eq_rel of -- See Note [Unsolved equalities]
            ReprEq -> continueWith (mkIrredCt ev)
            NomEq  -> continueWith (mkInsolubleCt ev) }
          -- No need to call canEqFailure/canEqHardFailure because they
          -- flatten, and the types involved here are already flat

{- Note [Unsolved equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an unsolved equality like
  (a b ~R# Int)
that is not necessarily insoluble!  Maybe 'a' will turn out to be a newtype.
So we want to make it a potentially-soluble Irred not an insoluble one.
Missing this point is what caused Trac #15431
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
--  so we must proceed one binder at a time (Trac #13879)

can_eq_nc_forall ev eq_rel s1 s2
 | CtWanted { ctev_loc = loc, ctev_dest = orig_dest } <- ev
 = do { let free_tvs       = tyCoVarsOfTypes [s1,s2]
            (bndrs1, phi1) = tcSplitForAllVarBndrs s1
            (bndrs2, phi2) = tcSplitForAllVarBndrs s2
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
                   ; let subst' = extendTvSubst subst tv2
                                       (mkCastTy (mkTyVarTy skol_tv) kind_co)
                   ; (co, wanteds2) <- go skol_tvs subst' bndrs2
                   ; return ( mkTcForAllCo skol_tv kind_co co
                            , wanteds1 `unionBags` wanteds2 ) }

            -- Done: unify phi1 ~ phi2
            go [] subst bndrs2
              = ASSERT( null bndrs2 )
                unify loc (eqRelRole eq_rel) phi1' (substTy subst phi2)

            go _ _ _ = panic "cna_eq_nc_forall"  -- case (s:ss) []

            empty_subst2 = mkEmptyTCvSubst (getTCvInScope subst1)

      ; all_co <- checkTvConstraintsTcS skol_info skol_tvs $
                  go skol_tvs empty_subst2 bndrs2

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
-- types in an inert wanted to be equal. We can discover the equality without
-- flattening, which is sometimes very expensive (in the case of type functions).
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
      | Just (arg1, res1) <- split1
      , Just (arg2, res2) <- split2
      = do { res_a <- go arg1 arg2
           ; res_b <- go res1 res2
           ; return $ combine_rev mkFunTy res_b res_a
           }
      | isJust split1 || isJust split2
      = bale_out ty1 ty2
      where
        split1 = tcSplitFunTy_maybe ty1
        split2 = tcSplitFunTy_maybe ty2

    go ty1 ty2
      | Just (tc1, tys1) <- tcRepSplitTyConApp_maybe ty1
      , Just (tc2, tys2) <- tcRepSplitTyConApp_maybe ty2
      = if tc1 == tc2 && tys1 `equalLength` tys2
          -- Crucial to check for equal-length args, because
          -- we cannot assume that the two args to 'go' have
          -- the same kind.  E.g go (Proxy *      (Maybe Int))
          --                        (Proxy (*->*) Maybe)
          -- We'll call (go (Maybe Int) Maybe)
          -- See Trac #13083
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
      -- with flattening. In particular, no need to zonk kinds. That's why
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
c.f. Trac #9123 comment:52,53 for a compelling example.

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
equality because the flattener technology deals with the similar case
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
       ; addUsedGREs (bagToList gres)
           -- we have actually used the newtype constructor here, so
           -- make sure we don't warn about importing it!

       ; new_ev <- rewriteEqEvidence ev swapped ty1' ps_ty2
                                     (mkTcSymCo co) (mkTcReflCo Representational ps_ty2)
       ; can_eq_nc False new_ev ReprEq ty1' ty1' ty2 ps_ty2 }

---------
-- ^ Decompose a type application.
-- All input types must be flat. See Note [Canonicalising type applications]
-- Nominal equality only!
can_eq_app :: CtEvidence       -- :: s1 t1 ~N s2 t2
           -> Xi -> Xi         -- s1 t1
           -> Xi -> Xi         -- s2 t2
           -> TcS (StopOrContinue Ct)

-- AppTys only decompose for nominal equality, so this case just leads
-- to an irreducible constraint; see typecheck/should_compile/T10494
-- See Note [Decomposing equality], note {4}
can_eq_app ev s1 t1 s2 t2
  | CtDerived { ctev_loc = loc } <- ev
  = do { unifyDeriveds loc [Nominal, Nominal] [s1, t1] [s2, t2]
       ; stopWith ev "Decomposed [D] AppTy" }
  | CtWanted { ctev_dest = dest, ctev_loc = loc } <- ev
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

  | CtGiven { ctev_evar = evar, ctev_loc = loc } <- ev
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
    s1k = tcTypeKind s1
    s2k = tcTypeKind s2

    k1 `mismatches` k2
      =  isForAllTy k1 && not (isForAllTy k2)
      || not (isForAllTy k1) && isForAllTy k2

-----------------------
-- | Break apart an equality over a casted type
-- looking like   (ty1 |> co1) ~ ty2   (modulo a swap-flag)
canEqCast :: Bool         -- are both types flat?
          -> CtEvidence
          -> EqRel
          -> SwapFlag
          -> TcType -> Coercion   -- LHS (res. RHS), ty1 |> co1
          -> TcType -> TcType     -- RHS (res. LHS), ty2 both normal and pretty
          -> TcS (StopOrContinue Ct)
canEqCast flat ev eq_rel swapped ty1 co1 ty2 ps_ty2
  = do { traceTcS "Decomposing cast" (vcat [ ppr ev
                                           , ppr ty1 <+> text "|>" <+> ppr co1
                                           , ppr ps_ty2 ])
       ; new_ev <- rewriteEqEvidence ev swapped ty1 ps_ty2
                                     (mkTcGReflRightCo role ty1 co1)
                                     (mkTcReflCo role ps_ty2)
       ; can_eq_nc flat new_ev eq_rel ty1 ty1 ty2 ps_ty2 }
  where
    role = eqRelRole eq_rel

------------------------
canTyConApp :: CtEvidence -> EqRel
            -> TyCon -> [TcType]
            -> TyCon -> [TcType]
            -> TcS (StopOrContinue Ct)
-- See Note [Decomposing TyConApps]
canTyConApp ev eq_rel tc1 tys1 tc2 tys2
  | tc1 == tc2
  , tys1 `equalLength` tys2
  = do { inerts <- getTcSInerts
       ; if can_decompose inerts
         then do { traceTcS "canTyConApp"
                       (ppr ev $$ ppr eq_rel $$ ppr tc1 $$ ppr tys1 $$ ppr tys2)
                 ; canDecomposableTyConAppOK ev eq_rel tc1 tys1 tys2
                 ; stopWith ev "Decomposed TyConApp" }
         else canEqFailure ev eq_rel ty1 ty2 }

  -- See Note [Skolem abstract data] (at tyConSkolem)
  | tyConSkolem tc1 || tyConSkolem tc2
  = do { traceTcS "canTyConApp: skolem abstract" (ppr tc1 $$ ppr tc2)
       ; continueWith (mkIrredCt ev) }

  -- Fail straight away for better error messages
  -- See Note [Use canEqFailure in canDecomposableTyConApp]
  | eq_rel == ReprEq && not (isGenerativeTyCon tc1 Representational &&
                             isGenerativeTyCon tc2 Representational)
  = canEqFailure ev eq_rel ty1 ty2
  | otherwise
  = canEqHardFailure ev ty1 ty2
  where
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

The key property to consider is injectivity. When decomposing a Given the
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

(So a type variable isn't a TyCon, but it's convenient to put the AppTy case
in the same table.)

Right away, we can say that Derived behaves just as Wanted for the purposes
of decomposition. The difference between Derived and Wanted is the handling of
evidence. Since decomposition in these cases isn't a matter of soundness but of
guessing, we want the same behavior regardless of evidence.

Here is a table (discussion following) detailing where decomposition of
   (T s1 ... sn) ~r (T t1 .. tn)
is allowed.  The first four lines (Data types ... type family) refer
to TyConApps with various TyCons T; the last line is for AppTy, where
there is presumably a type variable at the head, so it's actually
   (s s1 ... sn) ~r (t t1 .. tn)

NOMINAL               GIVEN                       WANTED

Datatype               YES                         YES
Newtype                YES                         YES
Data family            YES                         YES
Type family            YES, in injective args{1}   YES, in injective args{1}
Type variable          YES                         YES

REPRESENTATIONAL      GIVEN                       WANTED

Datatype               YES                         YES
Newtype                NO{2}                      MAYBE{2}
Data family            NO{3}                      MAYBE{3}
Type family             NO                          NO
Type variable          NO{4}                       NO{4}

{1}: Type families can be injective in some, but not all, of their arguments,
so we want to do partial decomposition. This is quite different than the way
other decomposition is done, where the decomposed equalities replace the original
one. We thus proceed much like we do with superclasses: emitting new Givens
when "decomposing" a partially-injective type family Given and new Deriveds
when "decomposing" a partially-injective type family Wanted. (As of the time of
writing, 13 June 2015, the implementation of injective type families has not
been merged, but it should be soon. Please delete this parenthetical if the
implementation is indeed merged.)

{2}: See Note [Decomposing newtypes at representational role]

{3}: Because of the possibility of newtype instances, we must treat
data families like newtypes. See also Note [Decomposing newtypes at
representational role]. See #10534 and test case
typecheck/should_fail/T10534.

{4}: Because type variables can stand in for newtypes, we conservatively do not
decompose AppTys over representational equality.

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

Furthermore, as explained in Note [NthCo and newtypes] in TyCoRep, we can't use
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
constraints: see Note [Instance and Given overlap] in TcInteract.

Conclusion:
  * Decompose [W] N s ~R N t  iff there no given constraint that could
    later solve it.
-}

canDecomposableTyConAppOK :: CtEvidence -> EqRel
                          -> TyCon -> [TcType] -> [TcType]
                          -> TcS ()
-- Precondition: tys1 and tys2 are the same length, hence "OK"
canDecomposableTyConAppOK ev eq_rel tc tys1 tys2
  = ASSERT( tys1 `equalLength` tys2 )
    case ev of
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
                     new_loc  | isVisibleTyConBinder bndr
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
  = do { (xi1, co1) <- flatten FM_FlattenAll ev ty1
       ; (xi2, co2) <- flatten FM_FlattenAll ev ty2
            -- We must flatten the types before putting them in the
            -- inert set, so that we are sure to kick them out when
            -- new equalities become available
       ; traceTcS "canEqFailure with ReprEq" $
         vcat [ ppr ev, ppr ty1, ppr ty2, ppr xi1, ppr xi2 ]
       ; new_ev <- rewriteEqEvidence ev NotSwapped xi1 xi2 co1 co2
       ; continueWith (mkIrredCt new_ev) }

-- | Call when canonicalizing an equality fails with utterly no hope.
canEqHardFailure :: CtEvidence
                 -> TcType -> TcType -> TcS (StopOrContinue Ct)
-- See Note [Make sure that insolubles are fully rewritten]
canEqHardFailure ev ty1 ty2
  = do { (s1, co1) <- flatten FM_SubstOnly ev ty1
       ; (s2, co2) <- flatten FM_SubstOnly ev ty2
       ; new_ev <- rewriteEqEvidence ev NotSwapped s1 s2 co1 co2
       ; continueWith (mkInsolubleCt new_ev) }

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
The simple things is to see if ty2 is of form (s2 t2), and
decompose.  By this time s1 and s2 can't be saturated type
function applications, because those have been dealt with
by an earlier equation in can_eq_nc, so it is always sound to
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

So instead can_eq_wanted_app flattens the LHS and RHS, in the hope of
replacing (a b) by (Array b), before using try_decompose_app to
decompose it.

Note [Make sure that insolubles are fully rewritten]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an equality fails, we still want to rewrite the equality
all the way down, so that it accurately reflects
 (a) the mutable reference substitution in force at start of solving
 (b) any ty-binds in force at this point in solving
See Note [Rewrite insolubles] in TcSMonad.
And if we don't do this there is a bad danger that
TcSimplify.applyTyVarDefaulting will find a variable
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

We want to make sure canEqTyVar sees [W] a ~R a, after b is flattened
and the Id newtype is unwrapped. This is assured by requiring only flat
types in canEqTyVar *and* having the newtype-unwrapping check above
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

Note [canCFunEqCan]
~~~~~~~~~~~~~~~~~~~
Flattening the arguments to a type family can change the kind of the type
family application. As an easy example, consider (Any k) where (k ~ Type)
is in the inert set. The original (Any k :: k) becomes (Any Type :: Type).
The problem here is that the fsk in the CFunEqCan will have the old kind.

The solution is to come up with a new fsk/fmv of the right kind. For
givens, this is easy: just introduce a new fsk and update the flat-cache
with the new one. For wanteds, we want to solve the old one if favor of
the new one, so we use dischargeFmv. This also kicks out constraints
from the inert set; this behavior is correct, as the kind-change may
allow more constraints to be solved.

We use `isTcReflexiveCo`, to ensure that we only use the hetero-kinded case
if we really need to.  Of course `flattenArgsNom` should return `Refl`
whenever possible, but Trac #15577 was an infinite loop because even
though the coercion was homo-kinded, `kind_co` was not `Refl`, so we
made a new (identical) CFunEqCan, and then the entire process repeated.
-}

canCFunEqCan :: CtEvidence
             -> TyCon -> [TcType]   -- LHS
             -> TcTyVar             -- RHS
             -> TcS (StopOrContinue Ct)
-- ^ Canonicalise a CFunEqCan.  We know that
--     the arg types are already flat,
-- and the RHS is a fsk, which we must *not* substitute.
-- So just substitute in the LHS
canCFunEqCan ev fn tys fsk
  = do { (tys', cos, kind_co) <- flattenArgsNom ev fn tys
                        -- cos :: tys' ~ tys

       ; let lhs_co  = mkTcTyConAppCo Nominal fn cos
                        -- :: F tys' ~ F tys
             new_lhs = mkTyConApp fn tys'

             flav    = ctEvFlavour ev
       ; (ev', fsk')
           <- if isTcReflexiveCo kind_co   -- See Note [canCFunEqCan]
              then do { traceTcS "canCFunEqCan: refl" (ppr new_lhs)
                      ; let fsk_ty = mkTyVarTy fsk
                      ; ev' <- rewriteEqEvidence ev NotSwapped new_lhs fsk_ty
                                                 lhs_co (mkTcNomReflCo fsk_ty)
                      ; return (ev', fsk) }
              else do { traceTcS "canCFunEqCan: non-refl" $
                        vcat [ text "Kind co:" <+> ppr kind_co
                             , text "RHS:" <+> ppr fsk <+> dcolon <+> ppr (tyVarKind fsk)
                             , text "LHS:" <+> hang (ppr (mkTyConApp fn tys))
                                                  2 (dcolon <+> ppr (tcTypeKind (mkTyConApp fn tys)))
                             , text "New LHS" <+> hang (ppr new_lhs)
                                                     2 (dcolon <+> ppr (tcTypeKind new_lhs)) ]
                      ; (ev', new_co, new_fsk)
                          <- newFlattenSkolem flav (ctEvLoc ev) fn tys'
                      ; let xi = mkTyVarTy new_fsk `mkCastTy` kind_co
                               -- sym lhs_co :: F tys ~ F tys'
                               -- new_co     :: F tys' ~ new_fsk
                               -- co         :: F tys ~ (new_fsk |> kind_co)
                            co = mkTcSymCo lhs_co `mkTcTransCo`
                                 mkTcCoherenceRightCo Nominal
                                                      (mkTyVarTy new_fsk)
                                                      kind_co
                                                      new_co

                      ; traceTcS "Discharging fmv/fsk due to hetero flattening" (ppr ev)
                      ; dischargeFunEq ev fsk co xi
                      ; return (ev', new_fsk) }

       ; extendFlatCache fn tys' (ctEvCoercion ev', mkTyVarTy fsk', ctEvFlavour ev')
       ; continueWith (CFunEqCan { cc_ev = ev', cc_fun = fn
                                 , cc_tyargs = tys', cc_fsk = fsk' }) }

---------------------
canEqTyVar :: CtEvidence          -- ev :: lhs ~ rhs
           -> EqRel -> SwapFlag
           -> TcTyVar               -- tv1
           -> TcType                -- lhs: pretty lhs, already flat
           -> TcType -> TcType      -- rhs: already flat
           -> TcS (StopOrContinue Ct)
canEqTyVar ev eq_rel swapped tv1 ps_ty1 xi2 ps_xi2
  | k1 `tcEqType` k2
  = canEqTyVarHomo ev eq_rel swapped tv1 ps_ty1 xi2 ps_xi2

         -- Note [Flattening] in TcFlatten gives us (F2), which says that
         -- flattening is always homogeneous (doesn't change kinds). But
         -- perhaps by flattening the kinds of the two sides of the equality
         -- at hand makes them equal. So let's try that.
  | otherwise
  = do { (flat_k1, k1_co) <- flattenKind loc flav k1  -- k1_co :: flat_k1 ~N kind(xi1)
       ; (flat_k2, k2_co) <- flattenKind loc flav k2  -- k2_co :: flat_k2 ~N kind(xi2)
       ; traceTcS "canEqTyVar tried flattening kinds"
                  (vcat [ sep [ parens (ppr tv1 <+> dcolon <+> ppr k1)
                              , text "~"
                              , parens (ppr xi2 <+> dcolon <+> ppr k2) ]
                        , ppr flat_k1
                        , ppr k1_co
                        , ppr flat_k2
                        , ppr k2_co ])

         -- we know the LHS is a tyvar. So let's dump all the coercions on the RHS
         -- If flat_k1 == flat_k2, let's dump all the coercions on the RHS and
         -- then call canEqTyVarHomo. If they don't equal, just rewriteEqEvidence
         -- (as an optimization, so that we don't have to flatten the kinds again)
         -- and then emit a kind equality in canEqTyVarHetero.
         -- See Note [Equalities with incompatible kinds]

       ; let role = eqRelRole eq_rel
       ; if flat_k1 `tcEqType` flat_k2
         then do { let rhs_kind_co = mkTcSymCo k2_co `mkTcTransCo` k1_co
                         -- :: kind(xi2) ~N kind(xi1)

                       new_rhs     = xi2 `mkCastTy` rhs_kind_co
                       ps_rhs      = ps_xi2 `mkCastTy` rhs_kind_co
                       rhs_co      = mkTcGReflLeftCo role xi2 rhs_kind_co

                 ; new_ev <- rewriteEqEvidence ev swapped xi1 new_rhs
                                               (mkTcReflCo role xi1) rhs_co
                       -- NB: rewriteEqEvidence executes a swap, if any, so we're
                       -- NotSwapped now.
                 ; canEqTyVarHomo new_ev eq_rel NotSwapped tv1 ps_ty1 new_rhs ps_rhs }
         else
    do { let sym_k1_co = mkTcSymCo k1_co  -- :: kind(xi1) ~N flat_k1
             sym_k2_co = mkTcSymCo k2_co  -- :: kind(xi2) ~N flat_k2

             new_lhs = xi1 `mkCastTy` sym_k1_co  -- :: flat_k1
             new_rhs = xi2 `mkCastTy` sym_k2_co  -- :: flat_k2
             ps_rhs  = ps_xi2 `mkCastTy` sym_k2_co

             lhs_co = mkTcGReflLeftCo role xi1 sym_k1_co
             rhs_co = mkTcGReflLeftCo role xi2 sym_k2_co
               -- lhs_co :: (xi1 |> sym k1_co) ~ xi1
               -- rhs_co :: (xi2 |> sym k2_co) ~ xi2

       ; new_ev <- rewriteEqEvidence ev swapped new_lhs new_rhs lhs_co rhs_co
         -- no longer swapped, due to rewriteEqEvidence
       ; canEqTyVarHetero new_ev eq_rel tv1 sym_k1_co flat_k1 ps_ty1
                                        new_rhs flat_k2 ps_rhs } }
  where
    xi1 = mkTyVarTy tv1

    k1 = tyVarKind tv1
    k2 = tcTypeKind xi2

    loc  = ctEvLoc ev
    flav = ctEvFlavour ev

canEqTyVarHetero :: CtEvidence   -- :: (tv1 |> co1 :: ki1) ~ (xi2 :: ki2)
                 -> EqRel
                 -> TcTyVar -> TcCoercionN -> TcKind  -- tv1 |> co1 :: ki1
                 -> TcType            -- pretty tv1 (*without* the coercion)
                 -> TcType -> TcKind  -- xi2 :: ki2
                 -> TcType            -- pretty xi2
                 -> TcS (StopOrContinue Ct)
canEqTyVarHetero ev eq_rel tv1 co1 ki1 ps_tv1 xi2 ki2 ps_xi2
  -- See Note [Equalities with incompatible kinds]
  | CtGiven { ctev_evar = evar } <- ev
    -- unswapped: tm :: (lhs :: ki1) ~ (rhs :: ki2)
    -- swapped  : tm :: (rhs :: ki2) ~ (lhs :: ki1)
  = do { let kind_co = mkTcKindCo (mkTcCoVarCo evar)
       ; kind_ev <- newGivenEvVar kind_loc (kind_pty, evCoercion kind_co)
       ; let  -- kind_ev :: (ki1 :: *) ~ (ki2 :: *)   (whether swapped or not)
              -- co1     :: kind(tv1) ~N ki1
              -- homo_co :: ki2 ~N kind(tv1)
             homo_co = mkTcSymCo (ctEvCoercion kind_ev) `mkTcTransCo` mkTcSymCo co1
             rhs'    = mkCastTy xi2 homo_co     -- :: kind(tv1)
             ps_rhs' = mkCastTy ps_xi2 homo_co  -- :: kind(tv1)
             rhs_co  = mkTcGReflLeftCo role xi2 homo_co
               -- rhs_co :: (xi2 |> homo_co :: kind(tv1)) ~ xi2

             lhs'   = mkTyVarTy tv1       -- :: kind(tv1)
             lhs_co = mkTcGReflRightCo role lhs' co1
               -- lhs_co :: (tv1 :: kind(tv1)) ~ (tv1 |> co1 :: ki1)

       ; traceTcS "Hetero equality gives rise to given kind equality"
           (ppr kind_ev <+> dcolon <+> ppr kind_pty)
       ; emitWorkNC [kind_ev]
       ; type_ev <- rewriteEqEvidence ev NotSwapped lhs' rhs' lhs_co rhs_co
       ; canEqTyVarHomo type_ev eq_rel NotSwapped tv1 ps_tv1 rhs' ps_rhs' }

  -- See Note [Equalities with incompatible kinds]
  | otherwise   -- Wanted and Derived
                  -- NB: all kind equalities are Nominal
  = do { emitNewDerivedEq kind_loc Nominal ki1 ki2
             -- kind_ev :: (ki1 :: *) ~ (ki2 :: *)
       ; traceTcS "Hetero equality gives rise to derived kind equality" $
           ppr ev
       ; continueWith (mkIrredCt ev) }

  where
    kind_pty = mkHeteroPrimEqPred liftedTypeKind liftedTypeKind ki1 ki2
    kind_loc = mkKindLoc (mkTyVarTy tv1 `mkCastTy` co1) xi2 loc

    loc  = ctev_loc ev
    role = eqRelRole eq_rel

-- guaranteed that tcTypeKind lhs == tcTypeKind rhs
canEqTyVarHomo :: CtEvidence
               -> EqRel -> SwapFlag
               -> TcTyVar                -- lhs: tv1
               -> TcType                 -- pretty lhs
               -> TcType -> TcType       -- rhs (might not be flat)
               -> TcS (StopOrContinue Ct)
canEqTyVarHomo ev eq_rel swapped tv1 ps_ty1 ty2 _
  | Just (tv2, _) <- tcGetCastedTyVar_maybe ty2
  , tv1 == tv2
  = canEqReflexive ev eq_rel (mkTyVarTy tv1)
    -- we don't need to check co because it must be reflexive

  | Just (tv2, co2) <- tcGetCastedTyVar_maybe ty2
  , swapOverTyVars tv1 tv2
  = do { traceTcS "canEqTyVar swapOver" (ppr tv1 $$ ppr tv2 $$ ppr swapped)
         -- FM_Avoid commented out: see Note [Lazy flattening] in TcFlatten
         -- let fmode = FE { fe_ev = ev, fe_mode = FM_Avoid tv1' True }
         -- Flatten the RHS less vigorously, to avoid gratuitous flattening
         -- True <=> xi2 should not itself be a type-function application

       ; let role    = eqRelRole eq_rel
             sym_co2 = mkTcSymCo co2
             ty1     = mkTyVarTy tv1
             new_lhs = ty1 `mkCastTy` sym_co2
             lhs_co  = mkTcGReflLeftCo role ty1 sym_co2

             new_rhs = mkTyVarTy tv2
             rhs_co  = mkTcGReflRightCo role new_rhs co2

       ; new_ev <- rewriteEqEvidence ev swapped new_lhs new_rhs lhs_co rhs_co

       ; dflags <- getDynFlags
       ; canEqTyVar2 dflags new_ev eq_rel IsSwapped tv2 (ps_ty1 `mkCastTy` sym_co2) }

canEqTyVarHomo ev eq_rel swapped tv1 _ _ ps_ty2
  = do { dflags <- getDynFlags
       ; canEqTyVar2 dflags ev eq_rel swapped tv1 ps_ty2 }

-- The RHS here is either not a casted tyvar, or it's a tyvar but we want
-- to rewrite the LHS to the RHS (as per swapOverTyVars)
canEqTyVar2 :: DynFlags
            -> CtEvidence   -- lhs ~ rhs (or, if swapped, orhs ~ olhs)
            -> EqRel
            -> SwapFlag
            -> TcTyVar                  -- lhs = tv, flat
            -> TcType                   -- rhs
            -> TcS (StopOrContinue Ct)
-- LHS is an inert type variable,
-- and RHS is fully rewritten, but with type synonyms
-- preserved as much as possible
canEqTyVar2 dflags ev eq_rel swapped tv1 rhs
  | Just rhs' <- metaTyVarUpdateOK dflags tv1 rhs  -- No occurs check
     -- Must do the occurs check even on tyvar/tyvar
     -- equalities, in case have  x ~ (y :: ..x...)
     -- Trac #12593
  = do { new_ev <- rewriteEqEvidence ev swapped lhs rhs' rewrite_co1 rewrite_co2
       ; continueWith (CTyEqCan { cc_ev = new_ev, cc_tyvar = tv1
                                , cc_rhs = rhs', cc_eq_rel = eq_rel }) }

  | otherwise  -- For some reason (occurs check, or forall) we can't unify
               -- We must not use it for further rewriting!
  = do { traceTcS "canEqTyVar2 can't unify" (ppr tv1 $$ ppr rhs)
       ; new_ev <- rewriteEqEvidence ev swapped lhs rhs rewrite_co1 rewrite_co2
       ; if isInsolubleOccursCheck eq_rel tv1 rhs
         then continueWith (mkInsolubleCt new_ev)
             -- If we have a ~ [a], it is not canonical, and in particular
             -- we don't want to rewrite existing inerts with it, otherwise
             -- we'd risk divergence in the constraint solver

         else continueWith (mkIrredCt new_ev) }
             -- A representational equality with an occurs-check problem isn't
             -- insoluble! For example:
             --   a ~R b a
             -- We might learn that b is the newtype Id.
             -- But, the occurs-check certainly prevents the equality from being
             -- canonical, and we might loop if we were to use it in rewriting.
  where
    role = eqRelRole eq_rel

    lhs = mkTyVarTy tv1

    rewrite_co1  = mkTcReflCo role lhs
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

{-
Note [Canonical orientation for tyvar/tyvar equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have a ~ b where both 'a' and 'b' are TcTyVars, which way
round should be oriented in the CTyEqCan?  The rules, implemented by
canEqTyVarTyVar, are these

 * If either is a flatten-meta-variables, it goes on the left.

 * Put a meta-tyvar on the left if possible
       alpha[3] ~ r

 * If both are meta-tyvars, put the more touchable one (deepest level
   number) on the left, so there is the best chance of unifying it
        alpha[3] ~ beta[2]

 * If both are meta-tyvars and both at the same level, put a TyVarTv
   on the right if possible
        alpha[2] ~ beta[2](sig-tv)
   That way, when we unify alpha := beta, we don't lose the TyVarTv flag.

 * Put a meta-tv with a System Name on the left if possible so it
   gets eliminated (improves error messages)

 * If one is a flatten-skolem, put it on the left so that it is
   substituted out  Note [Eliminate flat-skols] in TcUinfy
        fsk ~ a

Note [Equalities with incompatible kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What do we do when we have an equality

  (tv :: k1) ~ (rhs :: k2)

where k1 and k2 differ? This Note explores this treacherous area.

We must proceed differently here depending on whether we have a Wanted
or a Given. Consider this:

 [W] w :: (alpha :: k) ~ (Int :: Type)

where k is a skolem. One possible way forward is this:

 [W] co :: k ~ Type
 [W] w :: (alpha :: k) ~ (Int |> sym co :: k)

The next step will be to unify

  alpha := Int |> sym co

Now, consider what error we'll report if we can't solve the "co"
wanted. Its CtOrigin is the w wanted... which now reads (after zonking)
Int ~ Int. The user thus sees that GHC can't solve Int ~ Int, which
is embarrassing. See #11198 for more tales of destruction.

The reason for this odd behavior is much the same as
Note [Wanteds do not rewrite Wanteds] in TcRnTypes: note that the
new `co` is a Wanted.

The solution is then not to use `co` to "rewrite" -- that is, cast -- `w`, but
instead to keep `w` heterogeneous and irreducible. Given that we're not using
`co`, there is no reason to collect evidence for it, so `co` is born a
Derived, with a CtOrigin of KindEqOrigin. When the Derived is solved (by
unification), the original wanted (`w`) will get kicked out. We thus get

[D] _ :: k ~ Type
[W] w :: (alpha :: k) ~ (Int :: Type)

Note that the Wanted is unchanged and will be irreducible. This all happens
in canEqTyVarHetero.

Note that, if we had [G] co1 :: k ~ Type available, then we never get
to canEqTyVarHetero: canEqTyVar tries flattening the kinds first. If
we have [G] co1 :: k ~ Type, then flattening the kind of alpha would
rewrite k to Type, and we would end up in canEqTyVarHomo.

Successive canonicalizations of the same Wanted may produce
duplicate Deriveds. Similar duplications can happen with fundeps, and there
seems to be no easy way to avoid. I expect this case to be rare.

For Givens, this problem (the Wanteds-rewriting-Wanteds action of
a kind coercion) doesn't bite, so a heterogeneous Given gives
rise to a Given kind equality. No Deriveds here. We thus homogenise
the Given (see the "homo_co" in the Given case in canEqTyVarHetero) and
carry on with a homogeneous equality constraint.

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
as possible.  Hence the ps_ty1, ps_ty2 argument passed to canEqTyVar.

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

instance Functor StopOrContinue where
  fmap f (ContinueWith x) = ContinueWith (f x)
  fmap _ (Stop ev s)      = Stop ev s

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

qThe flattener preserves type synonyms, so they should appear in new_pred
as well as in old_pred; that is important for good error messages.
 -}


rewriteEvidence old_ev@(CtDerived {}) new_pred _co
  = -- If derived, don't even look at the coercion.
    -- This is very important, DO NOT re-order the equations for
    -- rewriteEvidence to put the isTcReflCo test first!
    -- Why?  Because for *Derived* constraints, c, the coercion, which
    -- was produced by flattening, may contain suspended calls to
    -- (ctEvExpr c), which fails for Derived constraints.
    -- (Getting this wrong caused Trac #7384.)
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
                             , ctev_loc = loc }) new_pred co
  = do { mb_new_ev <- newWanted loc new_pred
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
                                        -- Should be zonked, because we use tcTypeKind on nlhs/nrhs
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
                                  `mkTcTransCo` maybeSym swapped (mkTcCoVarCo old_evar)
                                  `mkTcTransCo` mkTcSymCo rhs_co)
       ; newGivenEvVar loc' (new_pred, new_tm) }

  | CtWanted { ctev_dest = dest } <- old_ev
  = do { (new_ev, hole_co) <- newWantedEq loc' (ctEvRole old_ev) nlhs nrhs
       ; let co = maybeSym swapped $
                  mkSymCo lhs_co
                  `mkTransCo` hole_co
                  `mkTransCo` rhs_co
       ; setWantedEq dest co
       ; traceTcS "rewriteEqEvidence" (vcat [ppr old_ev, ppr nlhs, ppr nrhs, ppr co])
       ; return new_ev }

  | otherwise
  = panic "rewriteEvidence"
  where
    new_pred = mkTcEqPredLikeEv old_ev nlhs nrhs

      -- equality is like a type class. Bumping the depth is necessary because
      -- of recursive newtypes, where "reducing" a newtype can actually make
      -- it bigger. See Note [Newtypes can blow the stack].
    loc      = ctEvLoc old_ev
    loc'     = bumpCtLocDepth loc

{- Note [unifyWanted and unifyDerived]
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

    go (FunTy s1 t1) (FunTy s2 t2)
      = do { co_s <- unifyWanted loc role s1 s2
           ; co_t <- unifyWanted loc role t1 t2
           ; return (mkFunCo role co_s co_t) }
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

    go (FunTy s1 t1) (FunTy s2 t2)
      = do { unify_derived loc role s1 s2
           ; unify_derived loc role t1 t2 }
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

maybeSym :: SwapFlag -> TcCoercion -> TcCoercion
maybeSym IsSwapped  co = mkTcSymCo co
maybeSym NotSwapped co = co
