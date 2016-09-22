{-# LANGUAGE CPP #-}

module TcCanonical(
     canonicalize,
     unifyDerived,
     makeSuperClasses,
     StopOrContinue(..), stopWith, continueWith
  ) where

#include "HsVersions.h"

import TcRnTypes
import TcUnify( swapOverTyVars )
import TcType
import Type
import TcFlatten
import TcSMonad
import TcEvidence
import Class
import TyCon
import TyCoRep   -- cleverly decomposes types, good for completeness checking
import Coercion
import FamInstEnv ( FamInstEnvs )
import FamInst ( tcTopNormaliseNewTypeTF_maybe )
import Var
import Outputable
import DynFlags( DynFlags )
import VarSet
import NameSet
import RdrName

import Pair
import Util
import Bag
import MonadUtils
import Control.Monad
import Data.List  ( zip4, foldl' )
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
unary (i.e. treats individual constraints one at a time), does not do
any zonking, but lives in TcS monad because it needs to create fresh
variables (for flattening) and consult the inerts (for efficiency).

The execution plan for canonicalization is the following:

  1) Decomposition of equalities happens as necessary until we reach a
     variable or type family in one side. There is no decomposition step
     for other forms of constraints.

  2) If, when we decompose, we discover a variable on the head then we
     look at inert_eqs from the current inert for a substitution for this
     variable and contine decomposing. Hence we lazily apply the inert
     substitution if it is needed.

  3) If no more decomposition is possible, we deeply apply the substitution
     from the inert_eqs and continue with flattening.

  4) During flattening, we examine whether we have already flattened some
     function application by looking at all the CTyFunEqs with the same
     function in the inert set. The reason for deeply applying the inert
     substitution at step (3) is to maximise our chances of matching an
     already flattened family application in the inert.

The net result is that a constraint coming out of the canonicalization
phase cannot be rewritten any further from the inerts (but maybe /it/ can
rewrite an inert or still interact with an inert in a further phase in the
simplifier.

Note [Caching for canonicals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our plan with pre-canonicalization is to be able to solve a constraint
really fast from existing bindings in TcEvBinds. So one may think that
the condition (isCNonCanonical) is not necessary.  However consider
the following setup:

InertSet = { [W] d1 : Num t }
WorkList = { [W] d2 : Num t, [W] c : t ~ Int}

Now, we prioritize equalities, but in our concrete example
(should_run/mc17.hs) the first (d2) constraint is dealt with first,
because (t ~ Int) is an equality that only later appears in the
worklist since it is pulled out from a nested implication
constraint. So, let's examine what happens:

   - We encounter work item (d2 : Num t)

   - Nothing is yet in EvBinds, so we reach the interaction with inerts
     and set:
              d2 := d1
    and we discard d2 from the worklist. The inert set remains unaffected.

   - Now the equation ([W] c : t ~ Int) is encountered and kicks-out
     (d1 : Num t) from the inerts.  Then that equation gets
     spontaneously solved, perhaps. We end up with:
        InertSet : { [G] c : t ~ Int }
        WorkList : { [W] d1 : Num t}

   - Now we examine (d1), we observe that there is a binding for (Num
     t) in the evidence binds and we set:
             d1 := d2
     and end up in a loop!

Now, the constraints that get kicked out from the inert set are always
Canonical, so by restricting the use of the pre-canonicalizer to
NonCanonical constraints we eliminate this danger. Moreover, for
canonical constraints we already have good caching mechanisms
(effectively the interaction solver) and we are interested in reducing
things like superclasses of the same non-canonical constraint being
generated hence I don't expect us to lose a lot by introducing the
(isCNonCanonical) restriction.

A similar situation can arise in TcSimplify, at the end of the
solve_wanteds function, where constraints from the inert set are
returned as new work -- our substCt ensures however that if they are
not rewritten by subst, they remain canonical and hence we will not
attempt to solve them from the EvBinds. If on the other hand they did
get rewritten and are now non-canonical they will still not match the
EvBinds, so we are again good.
-}

-- Top-level canonicalization
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

canonicalize :: Ct -> TcS (StopOrContinue Ct)
canonicalize ct@(CNonCanonical { cc_ev = ev })
  = do { traceTcS "canonicalize (non-canonical)" (ppr ct)
       ; {-# SCC "canEvVar" #-}
         canEvNC ev }

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

canonicalize (CIrredEvCan { cc_ev = ev })
  = canIrred ev
canonicalize (CHoleCan { cc_ev = ev, cc_hole = hole })
  = canHole ev hole

canEvNC :: CtEvidence -> TcS (StopOrContinue Ct)
-- Called only for non-canonical EvVars
canEvNC ev
  = case classifyPredType (ctEvPred ev) of
      ClassPred cls tys     -> do traceTcS "canEvNC:cls" (ppr cls <+> ppr tys)
                                  canClassNC ev cls tys
      EqPred eq_rel ty1 ty2 -> do traceTcS "canEvNC:eq" (ppr ty1 $$ ppr ty2)
                                  canEqNC    ev eq_rel ty1 ty2
      IrredPred {}          -> do traceTcS "canEvNC:irred" (ppr (ctEvPred ev))
                                  canIrred   ev
{-
************************************************************************
*                                                                      *
*                      Class Canonicalization
*                                                                      *
************************************************************************
-}

canClassNC :: CtEvidence -> Class -> [Type] -> TcS (StopOrContinue Ct)
-- "NC" means "non-canonical"; that is, we have got here
-- from a NonCanonical constrataint, not from a CDictCan
-- Precondition: EvVar is class evidence
canClassNC ev cls tys
  | isGiven ev  -- See Note [Eagerly expand given superclasses]
  = do { sc_cts <- mkStrictSuperClasses ev cls tys
       ; emitWork sc_cts
       ; canClass ev cls tys False }
  | otherwise
  = canClass ev cls tys (has_scs cls)
  where
    has_scs cls = not (null (classSCTheta cls))

canClass :: CtEvidence
         -> Class -> [Type]
         -> Bool            -- True <=> un-explored superclasses
         -> TcS (StopOrContinue Ct)
-- Precondition: EvVar is class evidence

canClass ev cls tys pend_sc
  =   -- all classes do *nominal* matching
    ASSERT2( ctEvRole ev == Nominal, ppr ev $$ ppr cls $$ ppr tys )
    do { (xis, cos) <- flattenManyNom ev tys
       ; let co = mkTcTyConAppCo Nominal (classTyCon cls) cos
             xi = mkClassPred cls xis
             mk_ct new_ev = CDictCan { cc_ev = new_ev
                                     , cc_tyargs = xis
                                     , cc_class = cls
                                     , cc_pend_sc = pend_sc }
       ; mb <- rewriteEvidence ev xi co
       ; traceTcS "canClass" (vcat [ ppr ev
                                   , ppr xi, ppr mb ])
       ; return (fmap mk_ct mb) }

{- Note [The superclass story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to add superclass constraints for two reasons:

* For givens, they give us a route to to proof.  E.g.
    f :: Ord a => a -> Bool
    f x = x == x
  We get a Wanted (Eq a), which can only be solved from the superclass
  of the Given (Ord a).

* For wanteds, they may give useful functional dependencies.  E.g.
     class C a b | a -> b where ...
     class C a b => D a b where ...
  Now a Wanted constraint (D Int beta) has (C Int beta) as a superclass
  and that might tell us about beta, via C's fundeps.  We can get this
  by generateing a Derived (C Int beta) constraint.  It's derived because
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
   This is done in canClassNC, when we take a non-canonical constraint
   and cannonicalise it.

   However stop if you encounter the same class twice.  That is,
   expand eagerly, but have a conservative termination condition: see
   Note [Expanding superclasses] in TcType.

2. Solve the wanteds as usual, but do no further expansion of
   superclasses for canonical CDictCans in solveSimpleGivens or
   solveSimpleWanteds; Note [Danger of adding superclasses during solving]

   However, /do/ continue to eagerly expand superlasses for /given/
   non-canonical constraints (canClassNC does this).  As Trac #12175
   showed, a type-family application can expand to a class constraint,
   and we want to see its superclasses for just the same reason as
   Note [Eagerly expand given superclasses].

3. If we have any remaining unsolved wanteds
        (see Note [When superclasses help] in TcRnTypes)
   try harder: take both the Givens and Wanteds, and expand
   superclasses again.  This may succeed in generating (a finite
   number of) extra Givens, and extra Deriveds. Both may help the
   proof.  This is done in TcSimplify.expandSuperClasses.

4. Go round to (2) again.  This loop (2,3,4) is implemented
   in TcSimplify.simpl_loop.

We try to terminate the loop by flagging which class constraints
(given or wanted) are potentially un-expanded.  This is what the
cc_pend_sc flag is for in CDictCan.  So in Step 3 we only expand
superclasses for constraints with cc_pend_sc set to true (i.e.
isPendingScDict holds).

When we take a CNonCanonical or CIrredCan, but end up classifying it
as a CDictCan, we set the cc_pend_sc flag to False.

Note [Eagerly expand given superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In step (1) of Note [The superclass story], why do we eagerly expand
Given superclasses by one layer?  Mainly because of some very obscure
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
    Now we we get [D] beta ~ b, and can solve that.

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
          = mkStrictSuperClasses ev cls tys
    go ct = pprPanic "makeSuperClasses" (ppr ct)

mkStrictSuperClasses :: CtEvidence -> Class -> [Type] -> TcS [Ct]
-- Return constraints for the strict superclasses of (c tys)
mkStrictSuperClasses ev cls tys
  = mk_strict_superclasses (unitNameSet (className cls)) ev cls tys

mk_superclasses :: NameSet -> CtEvidence -> TcS [Ct]
-- Return this constraint, plus its superclasses, if any
mk_superclasses rec_clss ev
  | ClassPred cls tys <- classifyPredType (ctEvPred ev)
  = mk_superclasses_of rec_clss ev cls tys

  | otherwise   -- Superclass is not a class predicate
  = return [mkNonCanonical ev]

mk_superclasses_of :: NameSet -> CtEvidence -> Class -> [Type] -> TcS [Ct]
-- Always return this class constraint,
-- and expand its superclasses
mk_superclasses_of rec_clss ev cls tys
  | loop_found = return [this_ct]  -- cc_pend_sc of this_ct = True
  | otherwise  = do { sc_cts <- mk_strict_superclasses rec_clss' ev cls tys
                    ; return (this_ct : sc_cts) }
                                   -- cc_pend_sc of this_ct = False
  where
    cls_nm     = className cls
    loop_found = cls_nm `elemNameSet` rec_clss
    rec_clss'  | isCTupleClass cls = rec_clss  -- Never contribute to recursion
               | otherwise         = rec_clss `extendNameSet` cls_nm
    this_ct    = CDictCan { cc_ev = ev, cc_class = cls, cc_tyargs = tys
                          , cc_pend_sc = loop_found }
                 -- NB: If there is a loop, we cut off, so we have not
                 --     added the superclasses, hence cc_pend_sc = True

mk_strict_superclasses :: NameSet -> CtEvidence -> Class -> [Type] -> TcS [Ct]
-- Always return the immediate superclasses of (cls tys);
-- and expand their superclasses, provided none of them are in rec_clss
-- nor are repeated
mk_strict_superclasses rec_clss ev cls tys
  | CtGiven { ctev_evar = evar, ctev_loc = loc } <- ev
  = do { sc_evs <- newGivenEvVars (mk_given_loc loc)
                                  (mkEvScSelectors (EvId evar) cls tys)
       ; concatMapM (mk_superclasses rec_clss) sc_evs }

  | isEmptyVarSet (tyCoVarsOfTypes tys)
  = return [] -- Wanteds with no variables yield no deriveds.
              -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted/Derived case, just add those SC that can lead to improvement.
  = do { let loc = ctEvLoc ev
       ; sc_evs <- mapM (newDerivedNC loc) (immSuperClasses cls tys)
       ; concatMapM (mk_superclasses rec_clss) sc_evs }
  where
    size = sizeTypes tys
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


{-
************************************************************************
*                                                                      *
*                      Irreducibles canonicalization
*                                                                      *
************************************************************************
-}

canIrred :: CtEvidence -> TcS (StopOrContinue Ct)
-- Precondition: ty not a tuple and no other evidence form
canIrred old_ev
  = do { let old_ty = ctEvPred old_ev
       ; traceTcS "can_pred" (text "IrredPred = " <+> ppr old_ty)
       ; (xi,co) <- flatten FM_FlattenAll old_ev old_ty -- co :: xi ~ old_ty
       ; rewriteEvidence old_ev xi co `andWhenContinue` \ new_ev ->
    do { -- Re-classify, in case flattening has improved its shape
       ; case classifyPredType (ctEvPred new_ev) of
           ClassPred cls tys     -> canClassNC new_ev cls tys
           EqPred eq_rel ty1 ty2 -> canEqNC new_ev eq_rel ty1 ty2
           _                     -> continueWith $
                                    CIrredEvCan { cc_ev = new_ev } } }

canHole :: CtEvidence -> Hole -> TcS (StopOrContinue Ct)
canHole ev hole
  = do { let ty = ctEvPred ev
       ; (xi,co) <- flatten FM_SubstOnly ev ty -- co :: xi ~ ty
       ; rewriteEvidence ev xi co `andWhenContinue` \ new_ev ->
    do { emitInsoluble (CHoleCan { cc_ev = new_ev
                                 , cc_hole = hole })
       ; stopWith new_ev "Emit insoluble hole" } }

{-
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
  | Just ty1' <- coreView ty1 = can_eq_nc flat ev eq_rel ty1' ps_ty1 ty2  ps_ty2
  | Just ty2' <- coreView ty2 = can_eq_nc flat ev eq_rel ty1  ps_ty1 ty2' ps_ty2

-- need to check for reflexivity in the ReprEq case.
-- See Note [Eager reflexivity check]
-- Check only when flat because the zonk_eq_types check in canEqNC takes
-- care of the non-flat case.
can_eq_nc' True _rdr_env _envs ev ReprEq ty1 _ ty2 _
  | ty1 `tcEqType` ty2
  = canEqReflexive ev ReprEq ty1

-- When working with ReprEq, unwrap newtypes.
can_eq_nc' _flat rdr_env envs ev ReprEq ty1 _ ty2 ps_ty2
  | Just stuff1 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty1
  = can_eq_newtype_nc ev NotSwapped ty1 stuff1 ty2 ps_ty2
can_eq_nc' _flat rdr_env envs ev ReprEq ty1 ps_ty1 ty2 _
  | Just stuff2 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty2
  = can_eq_newtype_nc ev IsSwapped  ty2 stuff2 ty1 ps_ty1

-- Then, get rid of casts
can_eq_nc' flat _rdr_env _envs ev eq_rel (CastTy ty1 co1) _ ty2 ps_ty2
  = canEqCast flat ev eq_rel NotSwapped ty1 co1 ty2 ps_ty2
can_eq_nc' flat _rdr_env _envs ev eq_rel ty1 ps_ty1 (CastTy ty2 co2) _
  = canEqCast flat ev eq_rel IsSwapped ty2 co2 ty1 ps_ty1

----------------------
-- Otherwise try to decompose
----------------------

-- Literals
can_eq_nc' _flat _rdr_env _envs ev eq_rel ty1@(LitTy l1) _ (LitTy l2) _
 | l1 == l2
  = do { setEqIfWanted ev (mkReflCo (eqRelRole eq_rel) ty1)
       ; stopWith ev "Equal LitTy" }

-- Try to decompose type constructor applications
-- Including FunTy (s -> t)
can_eq_nc' _flat _rdr_env _envs ev eq_rel ty1 _ ty2 _
  | Just (tc1, tys1) <- tcRepSplitTyConApp_maybe ty1
  , Just (tc2, tys2) <- tcRepSplitTyConApp_maybe ty2
  , not (isTypeFamilyTyCon tc1)
  , not (isTypeFamilyTyCon tc2)
  = canTyConApp ev eq_rel tc1 tys1 tc2 tys2

can_eq_nc' _flat _rdr_env _envs ev eq_rel
           s1@(ForAllTy {}) _ s2@(ForAllTy {}) _
 | CtWanted { ctev_loc = loc, ctev_dest = orig_dest } <- ev
 = do { let (bndrs1,body1) = tcSplitForAllTyVarBndrs s1
            (bndrs2,body2) = tcSplitForAllTyVarBndrs s2
      ; if not (equalLength bndrs1 bndrs2)
        then do { traceTcS "Forall failure" $
                     vcat [ ppr s1, ppr s2, ppr bndrs1, ppr bndrs2
                          , ppr (map binderArgFlag bndrs1)
                          , ppr (map binderArgFlag bndrs2) ]
                ; canEqHardFailure ev s1 s2 }
        else
          do { traceTcS "Creating implication for polytype equality" $ ppr ev
             ; kind_cos <- zipWithM (unifyWanted loc Nominal)
                             (map binderKind bndrs1) (map binderKind bndrs2)
             ; all_co <- deferTcSForAllEq (eqRelRole eq_rel) loc
                                           kind_cos (bndrs1,body1) (bndrs2,body2)
             ; setWantedEq orig_dest all_co
             ; stopWith ev "Deferred polytype equality" } }
 | otherwise
 = do { traceTcS "Omitting decomposition of given polytype equality" $
        pprEq s1 s2    -- See Note [Do not decompose given polytype equalities]
      ; stopWith ev "Discard given polytype equality" }

-- See Note [Canonicalising type applications] about why we require flat types
can_eq_nc' True _rdr_env _envs ev eq_rel (AppTy t1 s1) _ ty2 _
  | Just (t2, s2) <- tcSplitAppTy_maybe ty2
  = can_eq_app ev eq_rel t1 s1 t2 s2
can_eq_nc' True _rdr_env _envs ev eq_rel ty1 _ (AppTy t2 s2) _
  | Just (t1, s1) <- tcSplitAppTy_maybe ty1
  = can_eq_app ev eq_rel t1 s1 t2 s2

-- No similarity in type structure detected. Flatten and try again.
can_eq_nc' False rdr_env envs ev eq_rel _ ps_ty1 _ ps_ty2
  = do { (xi1, co1) <- flatten FM_FlattenAll ev ps_ty1
       ; (xi2, co2) <- flatten FM_FlattenAll ev ps_ty2
       ; rewriteEqEvidence ev NotSwapped xi1 xi2 co1 co2
         `andWhenContinue` \ new_ev ->
         can_eq_nc' True rdr_env envs new_ev eq_rel xi1 xi1 xi2 xi2 }

-- Type variable on LHS or RHS are last.
-- NB: pattern match on True: we want only flat types sent to canEqTyVar.
-- See also Note [No top-level newtypes on RHS of representational equalities]
can_eq_nc' True _rdr_env _envs ev eq_rel (TyVarTy tv1) ps_ty1 ty2 ps_ty2
  = canEqTyVar ev eq_rel NotSwapped tv1 ps_ty1 ty2 ps_ty2
can_eq_nc' True _rdr_env _envs ev eq_rel ty1 ps_ty1 (TyVarTy tv2) ps_ty2
  = canEqTyVar ev eq_rel IsSwapped tv2 ps_ty2 ty1 ps_ty1

-- We've flattened and the types don't match. Give up.
can_eq_nc' True _rdr_env _envs ev _eq_rel _ ps_ty1 _ ps_ty2
  = do { traceTcS "can_eq_nc' catch-all case" (ppr ps_ty1 $$ ppr ps_ty2)
       ; canEqHardFailure ev ps_ty1 ps_ty2 }

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

    go ty1 ty2
      | Just (tc1, tys1) <- tcRepSplitTyConApp_maybe ty1
      , Just (tc2, tys2) <- tcRepSplitTyConApp_maybe ty2
      , tc1 == tc2
      = tycon tc1 tys1 tys2

    go ty1 ty2
      | Just (ty1a, ty1b) <- tcRepSplitAppTy_maybe ty1
      , Just (ty2a, ty2b) <- tcRepSplitAppTy_maybe ty2
      = do { res_a <- go ty1a ty2a
           ; res_b <- go ty1b ty2b
           ; return $ combine_rev mkAppTy res_b res_a }

    go ty1@(LitTy lit1) (LitTy lit2)
      | lit1 == lit2
      = return (Right ty1)

    go ty1 ty2 = return $ Left (Pair ty1 ty2)
      -- we don't handle more complex forms here

    tyvar :: SwapFlag -> TcTyVar -> TcType
          -> TcS (Either (Pair TcType) TcType)
      -- try to do as little as possible, as anything we do here is redundant
      -- with flattening. In particular, no need to zonk kinds. That's why
      -- we don't use the already-defined zonking functions
    tyvar swapped tv ty
      = case tcTyVarDetails tv of
          MetaTv { mtv_ref = ref }
            -> do { cts <- readTcRef ref
                  ; case cts of
                      Flexi        -> give_up
                      Indirect ty' -> unSwap swapped go ty' ty }
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

    quick_zonk tv = case tcTyVarDetails tv of
      MetaTv { mtv_ref = ref }
        -> do { cts <- readTcRef ref
              ; case cts of
                  Flexi        -> return (TyVarTy tv, False)
                  Indirect ty' -> return (ty', True) }
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

{-
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
would normally produce a CIrredEvCan. However, we really do want to
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

       ; rewriteEqEvidence ev swapped ty1' ps_ty2
                           (mkTcSymCo co) (mkTcReflCo Representational ps_ty2)
         `andWhenContinue` \ new_ev ->
         can_eq_nc False new_ev ReprEq ty1' ty1' ty2 ps_ty2 }

---------
-- ^ Decompose a type application.
-- All input types must be flat. See Note [Canonicalising type applications]
can_eq_app :: CtEvidence       -- :: s1 t1 ~r s2 t2
           -> EqRel            -- r
           -> Xi -> Xi         -- s1 t1
           -> Xi -> Xi         -- s2 t2
           -> TcS (StopOrContinue Ct)

-- AppTys only decompose for nominal equality, so this case just leads
-- to an irreducible constraint; see typecheck/should_compile/T10494
-- See Note [Decomposing equality], note {4}
can_eq_app ev ReprEq _ _ _ _
  = do { traceTcS "failing to decompose representational AppTy equality" (ppr ev)
       ; continueWith (CIrredEvCan { cc_ev = ev }) }
          -- no need to call canEqFailure, because that flattens, and the
          -- types involved here are already flat

can_eq_app ev NomEq s1 t1 s2 t2
  | CtDerived { ctev_loc = loc } <- ev
  = do { unifyDeriveds loc [Nominal, Nominal] [s1, t1] [s2, t2]
       ; stopWith ev "Decomposed [D] AppTy" }
  | CtWanted { ctev_dest = dest, ctev_loc = loc } <- ev
  = do { co_s <- unifyWanted loc Nominal s1 s2
       ; co_t <- unifyWanted loc Nominal t1 t2
       ; let co = mkAppCo co_s co_t
       ; setWantedEq dest co
       ; stopWith ev "Decomposed [W] AppTy" }
  | CtGiven { ctev_evar = evar, ctev_loc = loc } <- ev
  = do { let co   = mkTcCoVarCo evar
             co_s = mkTcLRCo CLeft  co
             co_t = mkTcLRCo CRight co
       ; evar_s <- newGivenEvVar loc ( mkTcEqPredLikeEv ev s1 s2
                                     , EvCoercion co_s )
       ; evar_t <- newGivenEvVar loc ( mkTcEqPredLikeEv ev t1 t2
                                     , EvCoercion co_t )
       ; emitWorkNC [evar_t]
       ; canEqNC evar_s NomEq s1 s2 }
  | otherwise  -- Can't happen
  = error "can_eq_app"

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
       ; rewriteEqEvidence ev swapped ty1 ps_ty2
                           (mkTcReflCo role ty1
                              `mkTcCoherenceRightCo` co1)
                           (mkTcReflCo role ps_ty2)
         `andWhenContinue` \ new_ev ->
         can_eq_nc flat new_ev eq_rel ty1 ty1 ty2 ps_ty2 }
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
  , length tys1 == length tys2
  = do { inerts <- getTcSInerts
       ; if can_decompose inerts
         then do { traceTcS "canTyConApp"
                       (ppr ev $$ ppr eq_rel $$ ppr tc1 $$ ppr tys1 $$ ppr tys2)
                 ; canDecomposableTyConAppOK ev eq_rel tc1 tys1 tys2
                 ; stopWith ev "Decomposed TyConApp" }
         else canEqFailure ev eq_rel ty1 ty2 }

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
  = case ev of
     CtDerived {}
        -> unifyDeriveds loc tc_roles tys1 tys2

     CtWanted { ctev_dest = dest }
        -> do { cos <- zipWith4M unifyWanted new_locs tc_roles tys1 tys2
              ; setWantedEq dest (mkTyConAppCo role tc cos) }

     CtGiven { ctev_evar = evar }
        -> do { let ev_co = mkCoVarCo evar
              ; given_evs <- newGivenEvVars loc $
                             [ ( mkPrimEqPredRole r ty1 ty2
                               , EvCoercion (mkNthCo i ev_co) )
                             | (r, ty1, ty2, i) <- zip4 tc_roles tys1 tys2 [0..]
                             , r /= Phantom
                             , not (isCoercionTy ty1) && not (isCoercionTy ty2) ]
              ; emitWorkNC given_evs }
  where
    loc        = ctEvLoc ev
    role       = eqRelRole eq_rel
    tc_roles   = tyConRolesX role tc

      -- the following makes a better distinction between "kind" and "type"
      -- in error messages
    bndrs      = tyConBinders tc
    kind_loc   = toKindLoc loc
    is_kinds   = map isNamedTyConBinder bndrs
    new_locs | Just KindLevel <- ctLocTypeOrKind_maybe loc
             = repeat loc
             | otherwise
             = map (\is_kind -> if is_kind then kind_loc else loc) is_kinds


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
       ; rewriteEqEvidence ev NotSwapped xi1 xi2 co1 co2
         `andWhenContinue` \ new_ev ->
         continueWith (CIrredEvCan { cc_ev = new_ev }) }

-- | Call when canonicalizing an equality fails with utterly no hope.
canEqHardFailure :: CtEvidence
                 -> TcType -> TcType -> TcS (StopOrContinue Ct)
-- See Note [Make sure that insolubles are fully rewritten]
canEqHardFailure ev ty1 ty2
  = do { (s1, co1) <- flatten FM_SubstOnly ev ty1
       ; (s2, co2) <- flatten FM_SubstOnly ev ty2
       ; rewriteEqEvidence ev NotSwapped s1 s2 co1 co2
         `andWhenContinue` \ new_ev ->
    do { emitInsoluble (mkNonCanonical new_ev)
       ; stopWith new_ev "Definitely not equal" }}

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
See Note [Kick out insolubles] in TcSMonad.
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

So, we must check: does tv1 appear in xi2 under any type constructor that
is generative w.r.t. representational equality? That's what isTyVarUnderDatatype
does. (The other name I considered, isTyVarUnderTyConGenerativeWrtReprEq was
a bit verbose. And the shorter name gets the point across.)

See also #10715, which induced this addition.

Note [No derived kind equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we're working with a heterogeneous derived equality

  [D] (t1 :: k1) ~ (t2 :: k2)

we want to homogenise to establish the kind invariant on CTyEqCans.
But we can't emit [D] k1 ~ k2 because we wouldn't then be able to
use the evidence in the homogenised types. So we emit a wanted
constraint, because we do really need the evidence here.

Thus: no derived kind equalities.

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
  = do { (tys', cos) <- flattenManyNom ev tys
                        -- cos :: tys' ~ tys
       ; let lhs_co  = mkTcTyConAppCo Nominal fn cos
                        -- :: F tys' ~ F tys
             new_lhs = mkTyConApp fn tys'
             fsk_ty  = mkTyVarTy fsk
       ; rewriteEqEvidence ev NotSwapped new_lhs fsk_ty
                           lhs_co (mkTcNomReflCo fsk_ty)
         `andWhenContinue` \ ev' ->
    do { extendFlatCache fn tys' (ctEvCoercion ev', fsk_ty, ctEvFlavour ev')
       ; continueWith (CFunEqCan { cc_ev = ev', cc_fun = fn
                                 , cc_tyargs = tys', cc_fsk = fsk }) } }

---------------------
canEqTyVar :: CtEvidence          -- ev :: lhs ~ rhs
           -> EqRel -> SwapFlag
           -> TcTyVar -> TcType   -- lhs: already flat, not a cast
           -> TcType -> TcType    -- rhs: already flat, not a cast
           -> TcS (StopOrContinue Ct)
canEqTyVar ev eq_rel swapped tv1 ps_ty1 (TyVarTy tv2) _
  | tv1 == tv2
  = canEqReflexive ev eq_rel ps_ty1

  | swapOverTyVars tv1 tv2
  = do { traceTcS "canEqTyVar" (ppr tv1 $$ ppr tv2 $$ ppr swapped)
         -- FM_Avoid commented out: see Note [Lazy flattening] in TcFlatten
         -- let fmode = FE { fe_ev = ev, fe_mode = FM_Avoid tv1' True }
         -- Flatten the RHS less vigorously, to avoid gratuitous flattening
         -- True <=> xi2 should not itself be a type-function application
       ; dflags <- getDynFlags
       ; canEqTyVar2 dflags ev eq_rel (flipSwap swapped) tv2 ps_ty1 }

canEqTyVar ev eq_rel swapped tv1 _ _ ps_ty2
  = do { dflags <- getDynFlags
       ; canEqTyVar2 dflags ev eq_rel swapped tv1 ps_ty2 }

canEqTyVar2 :: DynFlags
            -> CtEvidence   -- lhs ~ rhs (or, if swapped, orhs ~ olhs)
            -> EqRel
            -> SwapFlag
            -> TcTyVar      -- lhs, flat
            -> TcType       -- rhs, flat
            -> TcS (StopOrContinue Ct)
-- LHS is an inert type variable,
-- and RHS is fully rewritten, but with type synonyms
-- preserved as much as possible

canEqTyVar2 dflags ev eq_rel swapped tv1 xi2
  | OC_OK xi2' <- occurCheckExpand dflags tv1 xi2  -- No occurs check
     -- Must do the occurs check even on tyvar/tyvar
     -- equalities, in case have  x ~ (y :: ..x...)
     -- Trac #12593
  = rewriteEqEvidence ev swapped xi1 xi2' co1 co2
    `andWhenContinue` \ new_ev ->
    homogeniseRhsKind new_ev eq_rel xi1 xi2' $ \new_new_ev xi2'' ->
    CTyEqCan { cc_ev = new_new_ev, cc_tyvar = tv1
             , cc_rhs = xi2'', cc_eq_rel = eq_rel }

  | otherwise  -- Occurs check error (or a forall)
  = do { traceTcS "canEqTyVar2 occurs check error" (ppr tv1 $$ ppr xi2)
       ; rewriteEqEvidence ev swapped xi1 xi2 co1 co2
         `andWhenContinue` \ new_ev ->
         if eq_rel == NomEq || isTyVarUnderDatatype tv1 xi2
         then do { emitInsoluble (mkNonCanonical new_ev)
             -- If we have a ~ [a], it is not canonical, and in particular
             -- we don't want to rewrite existing inerts with it, otherwise
             -- we'd risk divergence in the constraint solver
                 ; stopWith new_ev "Occurs check" }

             -- A representational equality with an occurs-check problem isn't
             -- insoluble! For example:
             --   a ~R b a
             -- We might learn that b is the newtype Id.
             -- But, the occurs-check certainly prevents the equality from being
             -- canonical, and we might loop if we were to use it in rewriting.
         else do { traceTcS "Occurs-check in representational equality"
                           (ppr xi1 $$ ppr xi2)
                 ; continueWith (CIrredEvCan { cc_ev = new_ev }) } }
  where
    role = eqRelRole eq_rel
    xi1  = mkTyVarTy tv1
    co1  = mkTcReflCo role xi1
    co2  = mkTcReflCo role xi2

-- | Solve a reflexive equality constraint
canEqReflexive :: CtEvidence    -- ty ~ ty
               -> EqRel
               -> TcType        -- ty
               -> TcS (StopOrContinue Ct)   -- always Stop
canEqReflexive ev eq_rel ty
  = do { setEvBindIfWanted ev (EvCoercion $
                               mkTcReflCo (eqRelRole eq_rel) ty)
       ; stopWith ev "Solved by reflexivity" }

-- See Note [Equalities with incompatible kinds]
homogeniseRhsKind :: CtEvidence -- ^ the evidence to homogenise
                  -> EqRel
                  -> TcType              -- ^ original LHS
                  -> Xi                  -- ^ original RHS
                  -> (CtEvidence -> Xi -> Ct)
                           -- ^ how to build the homogenised constraint;
                           -- the 'Xi' is the new RHS
                  -> TcS (StopOrContinue Ct)
homogeniseRhsKind ev eq_rel lhs rhs build_ct
  | k1 `tcEqType` k2
  = continueWith (build_ct ev rhs)

  | CtGiven { ctev_evar = evar } <- ev
    -- tm :: (lhs :: k1) ~ (rhs :: k2)
  = do { kind_ev_id <- newBoundEvVarId kind_pty
                                       (EvCoercion $
                                        mkTcKindCo $ mkTcCoVarCo evar)
           -- kind_ev_id :: (k1 :: *) ~# (k2 :: *)
       ; let kind_ev = CtGiven { ctev_pred = kind_pty
                               , ctev_evar = kind_ev_id
                               , ctev_loc  = kind_loc }
             homo_co = mkSymCo $ mkCoVarCo kind_ev_id
             rhs'    = mkCastTy rhs homo_co
       ; traceTcS "Hetero equality gives rise to given kind equality"
           (ppr kind_ev_id <+> dcolon <+> ppr kind_pty)
       ; emitWorkNC [kind_ev]
       ; type_ev <- newGivenEvVar loc
                      ( mkTcEqPredLikeEv ev lhs rhs'
                      , EvCoercion $
                        mkTcCoherenceRightCo (mkTcCoVarCo evar) homo_co )
          -- type_ev :: (lhs :: k1) ~ ((rhs |> sym kind_ev_id) :: k1)
       ; continueWith (build_ct type_ev rhs') }

  | otherwise   -- Wanted and Derived. See Note [No derived kind equalities]
    -- evar :: (lhs :: k1) ~ (rhs :: k2)
  = do { (kind_ev, kind_co) <- newWantedEq kind_loc Nominal k1 k2
             -- kind_ev :: (k1 :: *) ~ (k2 :: *)
       ; traceTcS "Hetero equality gives rise to wanted kind equality" $
           ppr (kind_ev)
       ; emitWorkNC [kind_ev]
       ; let homo_co   = mkSymCo kind_co
           -- homo_co :: k2 ~ k1
             rhs'      = mkCastTy rhs homo_co
       ; case ev of
           CtGiven {} -> panic "homogeniseRhsKind"
           CtDerived {} -> continueWith (build_ct (ev { ctev_pred = homo_pred })
                                                  rhs')
             where homo_pred = mkTcEqPredLikeEv ev lhs rhs'
           CtWanted { ctev_dest = dest } -> do
             { (type_ev, hole_co) <- newWantedEq loc role lhs rhs'
                  -- type_ev :: (lhs :: k1) ~ (rhs |> sym kind_ev :: k1)
             ; setWantedEq dest
                           (hole_co `mkTransCo`
                            (mkReflCo role rhs
                             `mkCoherenceLeftCo` homo_co))

                -- dest := hole ; <rhs> |> homo_co :: (lhs :: k1) ~ (rhs :: k2)
             ; continueWith (build_ct type_ev rhs') }}

  where
    k1 = typeKind lhs
    k2 = typeKind rhs

    kind_pty = mkHeteroPrimEqPred liftedTypeKind liftedTypeKind k1 k2
    kind_loc = mkKindLoc lhs rhs loc

    loc  = ctev_loc ev
    role = eqRelRole eq_rel

{-
Note [Canonical orientation for tyvar/tyvar equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have a ~ b where both 'a' and 'b' are TcTyVars, which way
round should be oriented in the CTyEqCan?  The rules, implemented by
canEqTyVarTyVar, are these

 * If either is a flatten-meta-variables, it goes on the left.

 * If one is a strict sub-kind of the other e.g.
       (alpha::?) ~ (beta::*)
   orient them so RHS is a subkind of LHS.  That way we will replace
   'a' with 'b', correctly narrowing the kind.
   This establishes the subkind invariant of CTyEqCan.

 * Put a meta-tyvar on the left if possible
       alpha[3] ~ r

 * If both are meta-tyvars, put the more touchable one (deepest level
   number) on the left, so there is the best chance of unifying it
        alpha[3] ~ beta[2]

 * If both are meta-tyvars and both at the same level, put a SigTv
   on the right if possible
        alpha[2] ~ beta[2](sig-tv)
   That way, when we unify alpha := beta, we don't lose the SigTv flag.

 * Put a meta-tv with a System Name on the left if possible so it
   gets eliminated (improves error messages)

 * If one is a flatten-skolem, put it on the left so that it is
   substituted out  Note [Elminate flat-skols]
        fsk ~ a

Note [Avoid unnecessary swaps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we swap without actually improving matters, we can get an infnite loop.
Consider
    work item:  a ~ b
   inert item:  b ~ c
We canonicalise the work-time to (a ~ c).  If we then swap it before
aeding to the inert set, we'll add (c ~ a), and therefore kick out the
inert guy, so we get
   new work item:  b ~ c
   inert item:     c ~ a
And now the cycle just repeats

Note [Eliminate flat-skols]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have  [G] Num (F [a])
then we flatten to
     [G] Num fsk
     [G] F [a] ~ fsk
where fsk is a flatten-skolem (FlatSkol). Suppose we have
      type instance F [a] = a
then we'll reduce the second constraint to
     [G] a ~ fsk
and then replace all uses of 'a' with fsk.  That's bad because
in error messages intead of saying 'a' we'll say (F [a]).  In all
places, including those where the programmer wrote 'a' in the first
place.  Very confusing!  See Trac #7862.

Solution: re-orient a~fsk to fsk~a, so that we preferentially eliminate
the fsk.

Note [Equalities with incompatible kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
canEqLeaf is about to make a CTyEqCan or CFunEqCan; but both have the
invariant that LHS and RHS satisfy the kind invariants for CTyEqCan,
CFunEqCan.  What if we try to unify two things with incompatible
kinds?

eg    a ~ b  where a::*, b::*->*
or    a ~ b  where a::*, b::k, k is a kind variable

The CTyEqCan compatKind invariant is important.  If we make a CTyEqCan
for a~b, then we might well *substitute* 'b' for 'a', and that might make
a well-kinded type ill-kinded; and that is bad (eg typeKind can crash, see
Trac #7696).

So instead for these ill-kinded equalities we homogenise the RHS of the
equality, emitting new constraints as necessary.

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

The flattener preserves type synonyms, so they should appear in new_pred
as well as in old_pred; that is important for good error messages.
 -}


rewriteEvidence old_ev@(CtDerived {}) new_pred _co
  = -- If derived, don't even look at the coercion.
    -- This is very important, DO NOT re-order the equations for
    -- rewriteEvidence to put the isTcReflCo test first!
    -- Why?  Because for *Derived* constraints, c, the coercion, which
    -- was produced by flattening, may contain suspended calls to
    -- (ctEvTerm c), which fails for Derived constraints.
    -- (Getting this wrong caused Trac #7384.)
    continueWith (old_ev { ctev_pred = new_pred })

rewriteEvidence old_ev new_pred co
  | isTcReflCo co -- See Note [Rewriting with Refl]
  = continueWith (old_ev { ctev_pred = new_pred })

rewriteEvidence ev@(CtGiven { ctev_evar = old_evar , ctev_loc = loc }) new_pred co
  = do { new_ev <- newGivenEvVar loc (new_pred, new_tm)
       ; continueWith new_ev }
  where
    -- mkEvCast optimises ReflCo
    new_tm = mkEvCast (EvId old_evar) (tcDowngradeRole Representational
                                                       (ctEvRole ev)
                                                       (mkTcSymCo co))

rewriteEvidence ev@(CtWanted { ctev_dest = dest
                             , ctev_loc = loc }) new_pred co
  = do { mb_new_ev <- newWanted loc new_pred
       ; MASSERT( tcCoercionRole co == ctEvRole ev )
       ; setWantedEvTerm dest
                   (mkEvCast (getEvTerm mb_new_ev)
                             (tcDowngradeRole Representational (ctEvRole ev) co))
       ; case mb_new_ev of
            Fresh  new_ev -> continueWith new_ev
            Cached _      -> stopWith ev "Cached wanted" }


rewriteEqEvidence :: CtEvidence         -- Old evidence :: olhs ~ orhs (not swapped)
                                        --              or orhs ~ olhs (swapped)
                  -> SwapFlag
                  -> TcType -> TcType   -- New predicate  nlhs ~ nrhs
                                        -- Should be zonked, because we use typeKind on nlhs/nrhs
                  -> TcCoercion         -- lhs_co, of type :: nlhs ~ olhs
                  -> TcCoercion         -- rhs_co, of type :: nrhs ~ orhs
                  -> TcS (StopOrContinue CtEvidence)  -- Of type nlhs ~ nrhs
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
  = continueWith (old_ev { ctev_pred = new_pred })

  | NotSwapped <- swapped
  , isTcReflCo lhs_co      -- See Note [Rewriting with Refl]
  , isTcReflCo rhs_co
  = continueWith (old_ev { ctev_pred = new_pred })

  | CtGiven { ctev_evar = old_evar } <- old_ev
  = do { let new_tm = EvCoercion (lhs_co
                                  `mkTcTransCo` maybeSym swapped (mkTcCoVarCo old_evar)
                                  `mkTcTransCo` mkTcSymCo rhs_co)
       ; new_ev <- newGivenEvVar loc' (new_pred, new_tm)
       ; continueWith new_ev }

  | CtWanted { ctev_dest = dest } <- old_ev
  = do { (new_ev, hole_co) <- newWantedEq loc' (ctEvRole old_ev) nlhs nrhs
       ; let co = maybeSym swapped $
                  mkSymCo lhs_co
                  `mkTransCo` hole_co
                  `mkTransCo` rhs_co
       ; setWantedEq dest co
       ; traceTcS "rewriteEqEvidence" (vcat [ppr old_ev, ppr nlhs, ppr nrhs, ppr co])
       ; continueWith new_ev }

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
type, perhaps fruitlessly, unifyWanted traverses the common structure, and
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
  = do { kind_co <- unifyWanted loc Nominal (typeKind ty1) (typeKind ty2)
       ; return (mkPhantomCo kind_co ty1 ty2) }

unifyWanted loc role orig_ty1 orig_ty2
  = go orig_ty1 orig_ty2
  where
    go ty1 ty2 | Just ty1' <- coreView ty1 = go ty1' ty2
    go ty1 ty2 | Just ty2' <- coreView ty2 = go ty1 ty2'

    go (FunTy s1 t1) (FunTy s2 t2)
      = do { co_s <- unifyWanted loc role s1 s2
           ; co_t <- unifyWanted loc role t1 t2
           ; return (mkTyConAppCo role funTyCon [co_s,co_t]) }
    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2, tys1 `equalLength` tys2
      , isInjectiveTyCon tc1 role -- don't look under newtypes at Rep equality
      = do { cos <- zipWith3M (unifyWanted loc)
                              (tyConRolesX role tc1) tys1 tys2
           ; return (mkTyConAppCo role tc1 cos) }
    go (TyVarTy tv) ty2
      = do { mb_ty <- isFilledMetaTyVar_maybe tv
           ; case mb_ty of
                Just ty1' -> go ty1' ty2
                Nothing   -> bale_out }
    go ty1 (TyVarTy tv)
      = do { mb_ty <- isFilledMetaTyVar_maybe tv
           ; case mb_ty of
                Just ty2' -> go ty1 ty2'
                Nothing   -> bale_out }

    go ty1@(CoercionTy {}) (CoercionTy {})
      = return (mkReflCo role ty1) -- we just don't care about coercions!

    go _ _ = bale_out

    bale_out = do { (new_ev, co) <- newWantedEq loc role orig_ty1 orig_ty2
                  ; emitWorkNC [new_ev]
                  ; return co }

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
    go ty1 ty2 | Just ty1' <- coreView ty1 = go ty1' ty2
    go ty1 ty2 | Just ty2' <- coreView ty2 = go ty1 ty2'

    go (FunTy s1 t1) (FunTy s2 t2)
      = do { unify_derived loc role s1 s2
           ; unify_derived loc role t1 t2 }
    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2, tys1 `equalLength` tys2
      , isInjectiveTyCon tc1 role
      = unifyDeriveds loc (tyConRolesX role tc1) tys1 tys2
    go (TyVarTy tv) ty2
      = do { mb_ty <- isFilledMetaTyVar_maybe tv
           ; case mb_ty of
                Just ty1' -> go ty1' ty2
                Nothing   -> bale_out }
    go ty1 (TyVarTy tv)
      = do { mb_ty <- isFilledMetaTyVar_maybe tv
           ; case mb_ty of
                Just ty2' -> go ty1 ty2'
                Nothing   -> bale_out }
    go _ _ = bale_out

    bale_out = emitNewDerivedEq loc role orig_ty1 orig_ty2

maybeSym :: SwapFlag -> TcCoercion -> TcCoercion
maybeSym IsSwapped  co = mkTcSymCo co
maybeSym NotSwapped co = co
