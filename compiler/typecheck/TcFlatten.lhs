\begin{code}
{-# LANGUAGE CPP #-}

module TcFlatten(
   FlattenEnv(..), FlattenMode(..),
   flatten, flattenMany, flattenFamApp, flattenTyVarOuter,
   unflatten,
   eqCanRewrite, canRewriteOrSame
 ) where

#include "HsVersions.h"

import TcRnTypes
import TcType
import Type
import TcEvidence
import TyCon
import TypeRep
import Kind( isSubKind )
import Var
import VarEnv
import NameEnv
import Outputable
import VarSet
import TcSMonad as TcS
import DynFlags( DynFlags )

import Util
import Bag
import FastString
import Control.Monad( when )
\end{code}


Note [The flattening story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* A CFunEqCan is either of form
     [G] <F xis> : F xis ~ fsk   -- fsk is a FlatSkol
     [W]       x : F xis ~ fmv   -- fmv is a unification variable,
                                 -- but untouchable,
                                 -- with MetaInfo = FlatMetaTv
  where
     x is the witness variable
     fsk/fmv is a flatten skolem
     xis are function-free
  CFunEqCans are always [Wanted], or [Given], never [Derived]

  fmv untouchable just means that in a CTyVarEq, say,
       fmv ~ Int
  we do NOT unify fmv.

* KEY INSIGHTS:

   - A given flatten-skolem, fsk, is known a-priori to be equal to
     F xis (the LHS), with <F xis> evidence

   - A unification flatten-skolem, fmv, stands for the as-yet-unknown
     type to which (F xis) will eventually reduce

* Inert set invariant: if F xis1 ~ fsk1, F xis2 ~ fsk2
                       then xis1 /= xis2
  i.e. at most one CFunEqCan with a particular LHS

* Each canonical CFunEqCan x : F xis ~ fsk/fmv has its own
  distinct evidence variable x and flatten-skolem fsk/fmv.
  Why? We make a fresh fsk/fmv when the constraint is born;
  and we never rewrite the RHS of a CFunEqCan.

* Function applications can occur in the RHS of a CTyEqCan.  No reason
  not allow this, and it reduces the amount of flattening that must occur.

* Flattening a type (F xis):
    - If we are flattening in a Wanted/Derived constraint
      then create new [W] x : F xis ~ fmv
      else create new [G] x : F xis ~ fsk
      with fresh evidence variable x and flatten-skolem fsk/fmv

    - Add it to the work list

    - Replace (F xis) with fsk/fmv in the type you are flattening

    - You can also add the CFunEqCan to the "flat cache", which
      simply keeps track of all the function applications you
      have flattened.

    - If (F xis) is in the cache already, just
      use its fsk/fmv and evidence x, and emit nothing.

    - No need to substitute in the flat-cache. It's not the end
      of the world if we start with, say (F alpha ~ fmv1) and
      (F Int ~ fmv2) and then find alpha := Int.  Athat will
      simply give rise to fmv1 := fmv2 via [Interacting rule] below

* Canonicalising a CFunEqCan [G/W] x : F xis ~ fsk/fmv
    - Flatten xis (to substitute any tyvars; there are already no functions)
                  cos :: xis ~ flat_xis
    - New wanted  x2 :: F flat_xis ~ fsk/fmv
    - Add new wanted to flat cache
    - Discharge x = F cos ; x2

* Unification flatten-skolems, fmv, ONLY get unified when either
    a) The CFunEqCan takes a step, using an axiom
    b) During un-flattening
  They are never unified in any other form of equality.
  For example [W] ffmv ~ Int  is stuck; it does not unify with fmv.

* We *never* substitute in the RHS (i.e. the fsk/fmv) of a CFunEqCan.
  That would destroy the invariant about the shape of a CFunEqCan,
  and it would risk wanted/wanted interactions. The only way we
  learn information about fsk is when the CFunEqCan takes a step.

  However we *do* substitute in the LHS of a CFunEqCan (else it
  would never get to fire!)

* [Interacting rule]
    (inert)     [W] x1 : F tys ~ fmv1
    (work item) [W] x2 : F tys ~ fmv2
  Just solve one from the other:
    x2 := x1
    fmv2 := fmv1
  This just unites the two fsks into one.
  Always solve given from wanted if poss.

* [Firing rule: wanteds]
    (work item) [W] x : F tys ~ fmv
    instantiate axiom: ax_co : F tys ~ rhs

   Dischard fmv:
      fmv := alpha
      x := ax_co ; sym x2
      [W] x2 : alpha ~ rhs  (Non-canonical)
   discharging the work item. This is the way that fmv's get
   unified; even though they are "untouchable".

   NB: this deals with the case where fmv appears in xi, which can
   happen; it just happens through the non-canonical stuff

   Possible short cut (shortCutReduction) if rhs = G rhs_tys,
   where G is a type function.  Then
      - Flatten rhs_tys (cos : rhs_tys ~ rhs_xis)
      - Add G rhs_xis ~ fmv to flat cache
      - New wanted [W] x2 : G rhs_xis ~ fmv
      - Discharge x := co ; G cos ; x2

* [Firing rule: givens]
    (work item) [G] g : F tys ~ fsk
    instantiate axiom: co : F tys ~ rhs

   Now add non-canonical (since rhs is not flat)
      [G] (sym g ; co) : fsk ~ rhs

   Short cut (shortCutReduction) for when rhs = G rhs_tys and G is a type function
      [G] (co ; g) : G tys ~ fsk
   But need to flatten tys:  flat_cos : tys ~ flat_tys
      [G] (sym (G flat_cos) ; co ; g) : G flat_tys ~ fsk


Why given-fsks, alone, doesn't work
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Could we get away with only flatten meta-tyvars, with no flatten-skolems? No.

  [W] w : alpha ~ [F alpha Int]

---> flatten
  w = ...w'...
  [W] w' : alpha ~ [fsk]
  [G] <F alpha Int> : F alpha Int ~ fsk

--> unify (no occurs check)
  alpha := [fsk]

But since fsk = F alpha Int, this is really an occurs check error.  If
that is all we know about alpha, we will succeed in constraint
solving, producing a program with an infinite type.

Even if we did finally get (g : fsk ~ Boo)l by solving (F alpha Int ~ fsk)
using axiom, zonking would not see it, so (x::alpha) sitting in the
tree will get zonked to an infinite type.  (Zonking always only does
refl stuff.)

Why flatten-meta-vars, alone doesn't work
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Look at Simple13, with unification-fmvs only

  [G] g : a ~ [F a]

---> Flatten given
  g' = g;[x]
  [G] g'  : a ~ [fmv]
  [W] x : F a ~ fmv

--> subst a in x
       x = F g' ; x2
   [W] x2 : F [fmv] ~ fmv

And now we have an evidence cycle between g' and x!

If we used a given instead (ie current story)

  [G] g : a ~ [F a]

---> Flatten given
  g' = g;[x]
  [G] g'  : a ~ [fsk]
  [G] <F a> : F a ~ fsk

---> Substitute for a
  [G] g'  : a ~ [fsk]
  [G] F (sym g'); <F a> : F [fsk] ~ fsk


Why is it right to treat fmv's differently to ordinary unification vars?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f :: forall a. a -> a -> Bool
  g :: F Int -> F Int -> Bool

Consider
  f (x:Int) (y:Bool)
This gives alpha~Int, alpha~Bool.  There is an inconsistency,
but really only one error.  SherLoc may tell you which location
is most likely, based on other occurrences of alpha.

Consider
  g (x:Int) (y:Bool)
Here we get (F Int ~ Int, F Int ~ Bool), which flattens to
  (fmv ~ Int, fmv ~ Bool)
But there are really TWO separate errors.  We must not complain
about Int~Bool.  Moreover these two errors could arise in entirely
unrelated parts of the code.  (In the alpha case, there must be
*some* connection (eg v:alpha in common envt).)

Note [Orient equalities with flatten-meta-vars on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example comes from IndTypesPerfMerge

From the ambiguity check for
  f :: (F a ~ a) => a
we get:
      [G] F a ~ a
      [W] F alpha ~ alpha, alpha ~ a

    From Givens we get
      [G] F a ~ fsk, fsk ~ a

    Now if we flatten we get
      [W] alpha ~ fmv, F alpha ~ fmv, alpha ~ a

    Now, processing the first one first, choosing alpha := fmv
      [W] F fmv ~ fmv, fmv ~ a

    And now we are stuck.  We must either *unify* fmv := a, or
    use the fmv ~ a to rewrite F fmv ~ fmv, so we can make it
    meet up with the given F a ~ blah.

Solution: always put fmvs on the left, so we get
      [W] fmv ~ alpha, F alpha ~ fmv, alpha ~ a
  The point is that fmvs are very uninformative, so doing alpha := fmv
  is a bad idea.  We want to use other constraints on alpha first.


Note [Derived constraints from wanted CTyEqCans]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is this type ambiguous:  (Foo e ~ Maybe e) => Foo e
 (indexed-types/should_fail/T4093a)

 [G] Foo e ~ Maybe e
 [W] Foo e ~ Foo ee      -- ee is a unification variable
 [W] Foo ee ~ Maybe ee)
---
 [G] Foo e ~ fsk
 [G] fsk ~ Maybe e

 [W] Foo e ~ fmv1
 [W] Foo ee ~ fmv2
 [W] fmv1 ~ fmv2
 [W] fmv2 ~ Maybe ee

--->   fmv1 := fsk  by matching LHSs
 [W] Foo ee ~ fmv2
 [W] fsk ~ fmv2
 [W] fmv2 ~ Maybe ee

--->
 [W] Foo ee ~ fmv2
 [W] fmv2 ~ Maybe e
 [W] fmv2 ~ Maybe ee

Now maybe we shuld get [D] e ~ ee, and then we'd solve it entirely.
But if in a smilar situation we got [D] Int ~ Bool we'd be back
to complaining about wanted/wanted interactions.  Maybe this arises
also for fundeps?

Here's another example:
  f :: [a] -> [b] -> blah
  f (e1 :: F Int) (e2 :: F Int)

  we get
     F Int ~ fmv
     fmv ~ [alpha]
     fmv ~ [beta]

  We want: alpha := beta (which might unlock something else).  If we
  generated [D] [alpha] ~ [beta] we'd be good here.

Current story: we don't generate these derived constraints.  We could, but
we'd want to make them very weak, so we didn't get the Int~Bool complaint.


%************************************************************************
%*                                                                      *
%*                  Other notes (Oct 14)
      I have not revisted these, but I didn't want to discard them
%*                                                                      *
%************************************************************************


Try: rewrite wanted with wanted only for fmvs (not all meta-tyvars)

But:   fmv ~ alpha[0]
       alpha[0] ~ fmv’
Now we don’t see that fmv ~ fmv’, which is a problem for injectivity detection.

Conclusion: rewrite wanteds with wanted for all untouchables.

skol ~ untch, must re-orieint to untch ~ skol, so that we can use it to rewrite.



%************************************************************************
%*                                                                      *
%*                  Examples
     Here is a long series of examples I had to work through
%*                                                                      *
%************************************************************************

Simple20
~~~~~~~~
axiom F [a] = [F a]

 [G] F [a] ~ a
-->
 [G] fsk ~ a
 [G] [F a] ~ fsk  (nc)
-->
 [G] F a ~ fsk2
 [G] fsk ~ [fsk2] 
 [G] fsk ~ a
-->
 [G] F a ~ fsk2
 [G] a ~ [fsk2]
 [G] fsk ~ a


-----------------------------------

----------------------------------------
indexed-types/should_compile/T44984

  [W] H (F Bool) ~ H alpha
  [W] alpha ~ F Bool
-->
  F Bool  ~ fmv0
  H fmv0  ~ fmv1
  H alpha ~ fmv2

  fmv1 ~ fmv2
  fmv0 ~ alpha

flatten
~~~~~~~
  fmv0  := F Bool
  fmv1  := H (F Bool)
  fmv2  := H alpha
  alpha := F Bool
plus
  fmv1 ~ fmv2

But these two are equal under the above assumptions.
Solve by Refl.


--- under plan B, namely solve fmv1:=fmv2 eagerly ---
  [W] H (F Bool) ~ H alpha
  [W] alpha ~ F Bool
-->
  F Bool  ~ fmv0
  H fmv0  ~ fmv1
  H alpha ~ fmv2

  fmv1 ~ fmv2
  fmv0 ~ alpha
-->
  F Bool  ~ fmv0
  H fmv0  ~ fmv1
  H alpha ~ fmv2    fmv2 := fmv1

  fmv0 ~ alpha

flatten
  fmv0 := F Bool
  fmv1 := H fmv0 = H (F Bool)
  retain   H alpha ~ fmv2
    because fmv2 has been filled
  alpha := F Bool


----------------------------
indexed-types/should_failt/T4179

after solving
  [W] fmv_1 ~ fmv_2
  [W] A3 (FCon x)           ~ fmv_1    (CFunEqCan)
  [W] A3 (x (aoa -> fmv_2)) ~ fmv_2    (CFunEqCan)

----------------------------------------
indexed-types/should_fail/T7729a

a)  [W]   BasePrimMonad (Rand m) ~ m1
b)  [W]   tt m1 ~ BasePrimMonad (Rand m)

--->  process (b) first
    BasePrimMonad (Ramd m) ~ fmv_atH
    fmv_atH ~ tt m1

--->  now process (a)
    m1 ~ s_atH ~ tt m1    -- An obscure occurs check


----------------------------------------
typecheck/TcTypeNatSimple

Original constraint
  [W] x + y ~ x + alpha  (non-canonical)
==>
  [W] x + y     ~ fmv1   (CFunEqCan)
  [W] x + alpha ~ fmv2   (CFuneqCan)
  [W] fmv1 ~ fmv2        (CTyEqCan)

(sigh)

----------------------------------------
indexed-types/should_fail/GADTwrong1

  [G] Const a ~ ()
==> flatten
  [G] fsk ~ ()
  work item: Const a ~ fsk
==> fire top rule
  [G] fsk ~ ()
  work item fsk ~ ()

Surely the work item should rewrite to () ~ ()?  Well, maybe not;
it'a very special case.  More generally, our givens look like 
F a ~ Int, where (F a) is not reducible.


----------------------------------------
indexed_types/should_fail/T8227:

Why using a different can-rewrite rule in CFunEqCan heads
does not work.

Assuming NOT rewriting wanteds with wanteds

   Inert: [W] fsk_aBh ~ fmv_aBk -> fmv_aBk
          [W] fmv_aBk ~ fsk_aBh

          [G] Scalar fsk_aBg ~ fsk_aBh
          [G] V a ~ f_aBg

   Worklist includes  [W] Scalar fmv_aBi ~ fmv_aBk
   fmv_aBi, fmv_aBk are flatten unificaiton variables

   Work item: [W] V fsk_aBh ~ fmv_aBi

Note that the inert wanteds are cyclic, because we do not rewrite
wanteds with wanteds.


Then we go into a loop when normalise the work-item, because we
use rewriteOrSame on the argument of V.

Conclusion: Don't make canRewrite context specific; instead use 
[W] a ~ ty to rewrite a wanted iff 'a' is a unification variable.


----------------------------------------

Here is a somewhat similar case:

   type family G a :: *

   blah :: (G a ~ Bool, Eq (G a)) => a -> a
   blah = error "urk"

   foo x = blah x

For foo we get
   [W] Eq (G a), G a ~ Bool
Flattening
   [W] G a ~ fmv, Eq fmv, fmv ~ Bool
We can't simplify away the Eq Bool unless we substitute for fmv.
Maybe that doesn't matter: we would still be left with unsolved
G a ~ Bool.

--------------------------
Trac #9318 has a very simple program leading to

  [W] F Int ~ Int
  [W] F Int ~ Bool

We don't want to get "Error Int~Bool".  But if fmv's can rewrite
wanteds, we will

  [W] fmv ~ Int
  [W] fmv ~ Bool
--->
  [W] Int ~ Bool


%************************************************************************
%*                                                                      *
%*           The main flattening functions
%*                                                                      *
%************************************************************************

Note [Flattening]
~~~~~~~~~~~~~~~~~~~~
  flatten ty  ==>   (xi, cc)
    where
      xi has no type functions, unless they appear under ForAlls

      cc = Auxiliary given (equality) constraints constraining
           the fresh type variables in xi.  Evidence for these
           is always the identity coercion, because internally the
           fresh flattening skolem variables are actually identified
           with the types they have been generated to stand in for.

Note that it is flatten's job to flatten *every type function it sees*.
flatten is only called on *arguments* to type functions, by canEqGiven.

Recall that in comments we use alpha[flat = ty] to represent a
flattening skolem variable alpha which has been generated to stand in
for ty.

----- Example of flattening a constraint: ------
  flatten (List (F (G Int)))  ==>  (xi, cc)
    where
      xi  = List alpha
      cc  = { G Int ~ beta[flat = G Int],
              F beta ~ alpha[flat = F beta] }
Here
  * alpha and beta are 'flattening skolem variables'.
  * All the constraints in cc are 'given', and all their coercion terms
    are the identity.

NB: Flattening Skolems only occur in canonical constraints, which
are never zonked, so we don't need to worry about zonking doing
accidental unflattening.

Note that we prefer to leave type synonyms unexpanded when possible,
so when the flattener encounters one, it first asks whether its
transitive expansion contains any type function applications.  If so,
it expands the synonym and proceeds; if not, it simply returns the
unexpanded synonym.

\begin{code}
data FlattenEnv
  = FE { fe_mode :: FlattenMode
       , fe_ev   :: CtEvidence }

data FlattenMode  -- Postcondition for all three: inert wrt the type substitution
  = FM_FlattenAll          -- Postcondition: function-free

  | FM_Avoid TcTyVar Bool  -- See Note [Lazy flattening]
                           -- Postcondition:
                           --  * tyvar is only mentioned in result under a rigid path
                           --    e.g.   [a] is ok, but F a won't happen
                           --  * If flat_top is True, top level is not a function application
                           --   (but under type constructors is ok e.g. [F a])

  | FM_SubstOnly           -- See Note [Flattening under a forall]
\end{code}

Note [Lazy flattening]
~~~~~~~~~~~~~~~~~~~~~~
The idea of FM_Avoid mode is to flatten less aggressively.  If we have
       a ~ [F Int]
there seems to be no great merit in lifting out (F Int).  But if it was
       a ~ [G a Int]
then we *do* want to lift it out, in case (G a Int) reduces to Bool, say,
which gets rid of the occurs-check problem.  (For the flat_top Bool, see
comments above and at call sites.)

HOWEVER, the lazy flattening actually seems to make type inference go
*slower*, not faster.  perf/compiler/T3064 is a case in point; it gets
*dramatically* worse with FM_Avoid.  I think it may be because
floating the types out means we normalise them, and that often makes
them smaller and perhaps allows more re-use of previously solved
goals.  But to be honest I'm not absolutely certain, so I am leaving
FM_Avoid in the code base.  What I'm removing is the unique place
where it is *used*, namely in TcCanonical.canEqTyVar.

See also Note [Conservative unification check] in TcUnify, which gives
other examples where lazy flattening caused problems.

Bottom line: FM_Avoid is unused for now (Nov 14).
Note: T5321Fun got faster when I disabled FM_Avoid
      T5837 did too, but it's pathalogical anyway

\begin{code}
-- Flatten a bunch of types all at once.
flattenMany :: FlattenEnv -> [Type] -> TcS ([Xi], [TcCoercion])
-- Coercions :: Xi ~ Type
-- Returns True iff (no flattening happened)
-- NB: The EvVar inside the 'fe_ev :: CtEvidence' is unused,
--     we merely want (a) Given/Solved/Derived/Wanted info
--                    (b) the GivenLoc/WantedLoc for when we create new evidence
flattenMany fmode tys
  = -- pprTrace "flattenMany" empty $
    go tys
  where go []       = return ([],[])
        go (ty:tys) = do { (xi,co)    <- flatten fmode ty
                         ; (xis,cos)  <- go tys
                         ; return (xi:xis,co:cos) }

flatten :: FlattenEnv -> TcType -> TcS (Xi, TcCoercion)
-- Flatten a type to get rid of type function applications, returning
-- the new type-function-free type, and a collection of new equality
-- constraints.  See Note [Flattening] for more detail.
--
-- Postcondition: Coercion :: Xi ~ TcType

flatten _ xi@(LitTy {}) = return (xi, mkTcNomReflCo xi)

flatten fmode (TyVarTy tv)
  = flattenTyVar fmode tv

flatten fmode (AppTy ty1 ty2)
  = do { (xi1,co1) <- flatten fmode ty1
       ; (xi2,co2) <- flatten fmode ty2
       ; traceTcS "flatten/appty" (ppr ty1 $$ ppr ty2 $$ ppr xi1 $$ ppr co1 $$ ppr xi2 $$ ppr co2)
       ; return (mkAppTy xi1 xi2, mkTcAppCo co1 co2) }

flatten fmode (FunTy ty1 ty2)
  = do { (xi1,co1) <- flatten fmode ty1
       ; (xi2,co2) <- flatten fmode ty2
       ; return (mkFunTy xi1 xi2, mkTcFunCo Nominal co1 co2) }

flatten fmode (TyConApp tc tys)

  -- Expand type synonyms that mention type families 
  -- on the RHS; see Note [Flattening synonyms]
  | Just (tenv, rhs, tys') <- tcExpandTyCon_maybe tc tys
  , let expanded_ty = mkAppTys (substTy (mkTopTvSubst tenv) rhs) tys'
  = case fe_mode fmode of
      FM_FlattenAll | anyNameEnv isTypeFamilyTyCon (tyConsOfType rhs)
                   -> flatten fmode expanded_ty
                    | otherwise
                   -> flattenTyConApp fmode tc tys
      _ -> flattenTyConApp fmode tc tys

  -- Otherwise, it's a type function application, and we have to
  -- flatten it away as well, and generate a new given equality constraint
  -- between the application and a newly generated flattening skolem variable.
  | isTypeFamilyTyCon tc
  = flattenFamApp fmode tc tys

  -- For * a normal data type application
  --     * data family application
  -- we just recursively flatten the arguments.
  | otherwise
-- FM_Avoid stuff commented out; see Note [Lazy flattening]
--  , let fmode' = case fmode of  -- Switch off the flat_top bit in FM_Avoid
--                   FE { fe_mode = FM_Avoid tv _ }
--                     -> fmode { fe_mode = FM_Avoid tv False }
--                   _ -> fmode
  = flattenTyConApp fmode tc tys

flatten fmode ty@(ForAllTy {})
-- We allow for-alls when, but only when, no type function
-- applications inside the forall involve the bound type variables.
  = do { let (tvs, rho) = splitForAllTys ty
       ; (rho', co) <- flatten (fmode { fe_mode = FM_SubstOnly }) rho
                         -- Substitute only under a forall
                         -- See Note [Flattening under a forall]
       ; return (mkForAllTys tvs rho', foldr mkTcForAllCo co tvs) }

flattenTyConApp :: FlattenEnv -> TyCon -> [TcType] -> TcS (Xi, TcCoercion)
flattenTyConApp fmode tc tys
  = do { (xis, cos) <- flattenMany fmode tys
       ; return (mkTyConApp tc xis, mkTcTyConAppCo Nominal tc cos) }
\end{code}

Note [Flattening synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Not expanding synonyms aggressively improves error messages, and
keeps types smaller. But we need to take care.

Suppose
   type T a = a -> a
and we want to flatten the type (T (F a)).  Then we can safely flatten
the (F a) to a skolem, and return (T fsk).  We don't need to expand the
synonym.  This works because TcTyConAppCo can deal with synonyms
(unlike TyConAppCo), see Note [TcCoercions] in TcEvidence.

But (Trac #8979) for
   type T a = (F a, a)    where F is a type function
we must expand the synonym in (say) T Int, to expose the type function
to the flattener.


Note [Flattening under a forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under a forall, we
  (a) MUST apply the inert substitution
  (b) MUST NOT flatten type family applications
Hence FMSubstOnly.

For (a) consider   c ~ a, a ~ T (forall b. (b, [c])
If we don't apply the c~a substitution to the second constraint
we won't see the occurs-check error.

For (b) consider  (a ~ forall b. F a b), we don't want to flatten
to     (a ~ forall b.fsk, F a b ~ fsk)
because now the 'b' has escaped its scope.  We'd have to flatten to
       (a ~ forall b. fsk b, forall b. F a b ~ fsk b)
and we have not begun to think about how to make that work!

%************************************************************************
%*                                                                      *
             Flattening a type-family application
%*                                                                      *
%************************************************************************

\begin{code}
flattenFamApp, flattenExactFamApp, flattenExactFamApp_fully
  :: FlattenEnv -> TyCon -> [TcType] -> TcS (Xi, TcCoercion)
  --   flattenFamApp            can be over-saturated
  --   flattenExactFamApp       is exactly saturated
  --   flattenExactFamApp_fully lifts out the application to top level
  -- Postcondition: Coercion :: Xi ~ F tys
flattenFamApp fmode tc tys  -- Can be over-saturated
    = ASSERT( tyConArity tc <= length tys )  -- Type functions are saturated
                 -- The type function might be *over* saturated
                 -- in which case the remaining arguments should
                 -- be dealt with by AppTys
      do { let (tys1, tys_rest) = splitAt (tyConArity tc) tys
         ; (xi1, co1) <- flattenExactFamApp fmode tc tys1
               -- co1 :: xi1 ~ F tys1
         ; (xis_rest, cos_rest) <- flattenMany fmode tys_rest
               -- cos_res :: xis_rest ~ tys_rest
         ; return ( mkAppTys xi1 xis_rest   -- NB mkAppTys: rhs_xi might not be a type variable
                                            --    cf Trac #5655
                  , mkTcAppCos co1 cos_rest -- (rhs_xi :: F xis) ; (F cos :: F xis ~ F tys)
                  ) }

flattenExactFamApp fmode tc tys
  = case fe_mode fmode of
       FM_FlattenAll -> flattenExactFamApp_fully fmode tc tys

       FM_SubstOnly -> do { (xis, cos) <- flattenMany fmode tys
                          ; return ( mkTyConApp tc xis
                                   , mkTcTyConAppCo Nominal tc cos ) }

       FM_Avoid tv flat_top -> do { (xis, cos) <- flattenMany fmode tys
                                  ; if flat_top || tv `elemVarSet` tyVarsOfTypes xis
                                    then flattenExactFamApp_fully fmode tc tys
                                    else return ( mkTyConApp tc xis
                                                , mkTcTyConAppCo Nominal tc cos ) }

flattenExactFamApp_fully fmode tc tys
  = do { (xis, cos) <- flattenMany (fmode { fe_mode = FM_FlattenAll })tys
       ; let ret_co = mkTcTyConAppCo Nominal tc cos
              -- ret_co :: F xis ~ F tys
             ctxt_ev = fe_ev fmode

       ; mb_ct <- lookupFlatCache tc xis
       ; case mb_ct of
           Just (co, fsk)  -- co :: F xis ~ fsk
             | isFskTyVar fsk || not (isGiven ctxt_ev)
             ->  -- Usable hit in the flat-cache
                 -- isFskTyVar checks for a "given" in the cache
                do { traceTcS "flatten/flat-cache hit" $ (ppr tc <+> ppr xis $$ ppr fsk $$ ppr co)
                   ; (fsk_xi, fsk_co) <- flattenTyVar fmode fsk
                          -- The fsk may already have been unified, so flatten it
                          -- fsk_co :: fsk_xi ~ fsk
                   ; return (fsk_xi, fsk_co `mkTcTransCo` mkTcSymCo co `mkTcTransCo` ret_co) }
                                    -- :: fsk_xi ~ F xis

           _ -> do { let fam_ty = mkTyConApp tc xis
                   ; (ev, fsk) <- newFlattenSkolem ctxt_ev fam_ty
                   ; extendFlatCache tc xis (ctEvCoercion ev, fsk)

                   -- The new constraint (F xis ~ fsk) is not necessarily inert
                   -- (e.g. the LHS may be a redex) so we must put it in the work list
                   ; let ct = CFunEqCan { cc_ev     = ev
                                        , cc_fun    = tc
                                        , cc_tyargs = xis
                                        , cc_fsk    = fsk }
                   ; updWorkListTcS (extendWorkListFunEq ct)

                   ; traceTcS "flatten/flat-cache miss" $ (ppr fam_ty $$ ppr fsk $$ ppr ev)
                   ; return (mkTyVarTy fsk, mkTcSymCo (ctEvCoercion ev) `mkTcTransCo` ret_co) } }
\end{code}

%************************************************************************
%*                                                                      *
             Flattening a type variable
%*                                                                      *
%************************************************************************

\begin{code}
flattenTyVar :: FlattenEnv -> TcTyVar -> TcS (Xi, TcCoercion)
-- "Flattening" a type variable means to apply the substitution to it
-- The substitution is actually the union of 
--     * the unifications that have taken place (either before the 
--       solver started, or in TcInteract.solveByUnification)
--     * the CTyEqCans held in the inert set
--
-- Postcondition: co : xi ~ tv
flattenTyVar fmode tv
  = do { mb_yes <- flattenTyVarOuter (fe_ev fmode) tv
       ; case mb_yes of
           Left tv' -> -- Done
                       do { traceTcS "flattenTyVar1" (ppr tv $$ ppr (tyVarKind tv'))
                          ; return (ty', mkTcNomReflCo ty') }
                    where
                       ty' = mkTyVarTy tv'

           Right (ty1, co1, True)   -- No need to recurse
                    -> do { traceTcS "flattenTyVar2" (ppr tv $$ ppr ty1)
                          ; return (ty1, co1) }

           Right (ty1, co1, False)  -- Recurse
                    -> do { (ty2, co2) <- flatten fmode ty1
                          ; traceTcS "flattenTyVar3" (ppr tv $$ ppr ty2)
                          ; return (ty2, co2 `mkTcTransCo` co1) }
       }

flattenTyVarOuter, flattenTyVarFinal
   :: CtEvidence -> TcTyVar
   -> TcS (Either TyVar (TcType, TcCoercion, Bool))
-- Look up the tyvar in
--   a) the internal MetaTyVar box
--   b) the tyvar binds
--   c) the inerts
-- Return (Left tv')                if it is not found, tv' has a properly zonked kind
--        (Right (ty, co, is_flat)) if found, with co :: ty ~ tv;
--                                  is_flat says if the result is guaranteed flattened

flattenTyVarOuter ctxt_ev tv
  | not (isTcTyVar tv)             -- Happens when flatten under a (forall a. ty)
  = flattenTyVarFinal ctxt_ev tv   -- So ty contains refernces to the non-TcTyVar a
  | otherwise
  = do { mb_ty <- isFilledMetaTyVar_maybe tv
       ; case mb_ty of {
           Just ty -> do { traceTcS "Following filled tyvar" (ppr tv <+> equals <+> ppr ty)
                         ; return (Right (ty, mkTcNomReflCo ty, False)) } ;
           Nothing ->

    -- Try in the inert equalities
    -- See Note [Applying the inert substitution]
    do { ieqs <- getInertEqs
       ; case lookupVarEnv ieqs tv of
           Just (ct:_)   -- If the first doesn't work,
                         -- the subsequent ones won't either
             | CTyEqCan { cc_ev = ctev, cc_tyvar = tv, cc_rhs = rhs_ty } <- ct
             , eqCanRewrite ctev ctxt_ev
             ->  do { traceTcS "Following inert tyvar" (ppr tv <+> equals <+> ppr rhs_ty $$ ppr ctev)
                    ; return (Right (rhs_ty, mkTcSymCo (ctEvCoercion ctev), True)) }
                    -- NB: ct is Derived then (fe_ev fmode) must be also, hence
                    -- we are not going to touch the returned coercion
                    -- so ctEvCoercion is fine.

           _other -> flattenTyVarFinal ctxt_ev tv
    } } }

flattenTyVarFinal ctxt_ev tv
  = -- Done, but make sure the kind is zonked
    do { let kind       = tyVarKind tv
             kind_fmode = FE { fe_ev = ctxt_ev, fe_mode = FM_SubstOnly }
       ; (new_knd, _kind_co) <- flatten kind_fmode kind
       ; return (Left (setVarType tv new_knd)) }
\end{code}

Note [Applying the inert substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The inert CTyEqCans (a ~ ty), inert_eqs, can be treated as a
substitution, and indeed flattenTyVarOuter applies it to the type
being flattened.  It has the following properties:

 * 'a' is not in fvs(ty)
 * They are *inert*; that is the eqCanRewrite relation is everywhere false

An example of such an inert substitution is:

 [G] g1 : ta8 ~ ta4
 [W] g2 : ta4 ~ a5Fj

If you ignored the G/W, it would not be an idempotent, but we don't ignore
it.  When rewriting a constraint
    ev_work :: blah
we only rewrite it with an inert constraint
    ev_inert1 :: a ~ ty
if
    ev_inert1 `eqCanRewrite` ev_work

This process stops in exactly one step; that is, the RHS 'ty' cannot be further
rewritten by any other inert.  Why not?  If it could, we'd have
    ev_inert1 :: a ~ ty[b]
    ev_inert2 :: b ~ ty'
and
    ev_inert2 `canRewrite` ev_work
But by the EqCanRewrite Property (see Note [eqCanRewrite]), that means
that ev_inert2 `eqCanRewrite` ev_inert1; but that means that 'b' can't
appear free in ev_inert1's RHS.

When we *unify* a variable, which we write
  alpha := ty
we must be sure we aren't creating an infinite type.  But that comes
from the CTyEqCan invariant that 'a' not in fvs(ty), plus the fact that
an inert CTyEqCan is fully zonked wrt the current unification assignments.
In effect they become Givens, implemented via the side-effected substitution.

Note [An alternative story for the inert substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used (GHC 7.8) to have this story for the inert substitution inert_eqs

 * 'a' is not in fvs(ty)
 * They are *inert* in the weaker sense that there is no infinite chain of
   (i1 `eqCanRewrite` i2), (i2 `eqCanRewrite` i3), etc

This means that flattening must be recursive, but it does allow
  [G] a ~ [b]
  [G] b ~ Maybe c

This avoids "saturating" the Givens, which can save a modest amount of work.
It is easy to implement, in TcInteract.kick_out, by only kicking out an inert
only if (a) the work item can rewrite the inert AND
        (b) the inert cannot rewrite the work item

This is signifcantly harder to think about. It can save a LOT of work
in occurs-check cases, but we don't care about them much.  Trac #5837
is an example; all the constraints here are Givens

             [G] a ~ TF (a,Int)
    -->
    work     TF (a,Int) ~ fsk
    inert    fsk ~ a

    --->
    work     fsk ~ (TF a, TF Int)
    inert    fsk ~ a

    --->
    work     a ~ (TF a, TF Int)
    inert    fsk ~ a

    ---> (attempting to flatten (TF a) so that it does not mention a
    work     TF a ~ fsk2
    inert    a ~ (fsk2, TF Int)
    inert    fsk ~ (fsk2, TF Int)

    ---> (substitute for a)
    work     TF (fsk2, TF Int) ~ fsk2
    inert    a ~ (fsk2, TF Int)
    inert    fsk ~ (fsk2, TF Int)

    ---> (top-level reduction, re-orient)
    work     fsk2 ~ (TF fsk2, TF Int)
    inert    a ~ (fsk2, TF Int)
    inert    fsk ~ (fsk2, TF Int)

    ---> (attempt to flatten (TF fsk2) to get rid of fsk2
    work     TF fsk2 ~ fsk3
    work     fsk2 ~ (fsk3, TF Int)
    inert    a   ~ (fsk2, TF Int)
    inert    fsk ~ (fsk2, TF Int)

    --->
    work     TF fsk2 ~ fsk3
    inert    fsk2 ~ (fsk3, TF Int)
    inert    a   ~ ((fsk3, TF Int), TF Int)
    inert    fsk ~ ((fsk3, TF Int), TF Int)

Because the incoming given rewrites all the inert givens, we get more and
more duplication in the inert set.  But this really only happens in pathalogical
casee, so we don't care.


\begin{code}
eqCanRewrite :: CtEvidence -> CtEvidence -> Bool
-- Very important function!
-- See Note [eqCanRewrite]
eqCanRewrite (CtGiven {})   _              = True
eqCanRewrite (CtDerived {}) (CtDerived {}) = True  -- Derived can't solve wanted/given
eqCanRewrite _ _ = False

canRewriteOrSame :: CtEvidence -> CtEvidence -> Bool
-- See Note [canRewriteOrSame]
canRewriteOrSame (CtGiven {})   _              = True
canRewriteOrSame (CtWanted {})  (CtWanted {})  = True
canRewriteOrSame (CtWanted {})  (CtDerived {}) = True
canRewriteOrSame (CtDerived {}) (CtDerived {}) = True
canRewriteOrSame _ _ = False
\end{code}

Note [eqCanRewrite]
~~~~~~~~~~~~~~~~~~~
(eqCanRewrite ct1 ct2) holds if the constraint ct1 (a CTyEqCan of form
tv ~ ty) can be used to rewrite ct2.

The EqCanRewrite Property:
  * For any a,b in {G,W,D}  if   a canRewrite b
                            then a canRewrite a
  This is what guarantees that canonicalisation will terminate.
  See Note [Applying the inert substitution]

At the moment we don't allow Wanteds to rewrite Wanteds, because that can give
rise to very confusing type error messages.  A good example is Trac #8450.
Here's another
   f :: a -> Bool
   f x = ( [x,'c'], [x,True] ) `seq` True
Here we get
  [W] a ~ Char
  [W] a ~ Bool
but we do not want to complain about Bool ~ Char!

Note [canRewriteOrSame]
~~~~~~~~~~~~~~~~~~~~~~~
canRewriteOrSame is similar but
 * returns True for Wanted/Wanted.
 * works for all kinds of constraints, not just CTyEqCans
See the call sites for explanations.

%************************************************************************
%*                                                                      *
             Unflattening
%*                                                                      *
%************************************************************************

An unflattening example:
    [W] F a ~ alpha
flattens to
    [W] F a ~ fmv   (CFunEqCan)
    [W] fmv ~ alpha (CTyEqCan)
We must solve both!


\begin{code}
unflatten :: Cts -> Cts -> TcS Cts
unflatten tv_eqs funeqs
 = do { dflags   <- getDynFlags
      ; untch    <- getUntouchables

      ; traceTcS "Unflattening" $ braces $
        vcat [ ptext (sLit "Funeqs =") <+> pprCts funeqs
             , ptext (sLit "Tv eqs =") <+> pprCts tv_eqs ]

         -- Step 1: unflatten the CFunEqCans, except if that causes an occurs check
         -- See Note [Unflatten using funeqs first]
      ; funeqs <- foldrBagM (unflatten_funeq dflags) emptyCts funeqs
      ; traceTcS "Unflattening 1" $ braces (pprCts funeqs)

          -- Step 2: unify the irreds, if possible
      ; tv_eqs  <- foldrBagM (unflatten_eq dflags untch) emptyCts tv_eqs
      ; traceTcS "Unflattening 2" $ braces (pprCts tv_eqs)

          -- Step 3: fill any remaining fmvs with fresh unification variables
      ; funeqs <- mapBagM finalise_funeq funeqs
      ; traceTcS "Unflattening 3" $ braces (pprCts funeqs)

          -- Step 4: remove any irreds that look like ty ~ ty
      ; tv_eqs <- foldrBagM finalise_eq emptyCts tv_eqs

      ; let all_flat = tv_eqs `andCts` funeqs
      ; traceTcS "Unflattening done" $ braces (pprCts all_flat)

      ; return all_flat }
  where
    ----------------
    unflatten_funeq :: DynFlags -> Ct -> Cts -> TcS Cts
    unflatten_funeq dflags ct@(CFunEqCan { cc_fun = tc, cc_tyargs = xis
                                         , cc_fsk = fmv, cc_ev = ev }) rest
      = do {   -- fmv should be a flatten meta-tv; we now fix its final
               -- value, and then zonking will eliminate it
             filled <- tryFill dflags fmv (mkTyConApp tc xis) ev
           ; return (if filled then rest else ct `consCts` rest) }

    unflatten_funeq _ other_ct _
      = pprPanic "unflatten_funeq" (ppr other_ct)

    ----------------
    finalise_funeq :: Ct -> TcS Ct
    finalise_funeq (CFunEqCan { cc_fsk = fmv, cc_ev = ev })
      = do { demoteUnfilledFmv fmv
           ; return (mkNonCanonical ev) }
    finalise_funeq ct = pprPanic "finalise_funeq" (ppr ct)

    ----------------
    unflatten_eq ::  DynFlags -> Untouchables -> Ct -> Cts -> TcS Cts
    unflatten_eq dflags untch ct@(CTyEqCan { cc_ev = ev, cc_tyvar = tv, cc_rhs = rhs }) rest
      | isFmvTyVar tv
      = do { lhs_elim <- tryFill dflags tv rhs ev
           ; if lhs_elim then return rest else
        do { rhs_elim <- try_fill dflags untch ev rhs (mkTyVarTy tv)
           ; if rhs_elim then return rest else
             return (ct `consCts` rest) } }

      | otherwise
      = return (ct `consCts` rest)

    unflatten_eq _ _ ct _ = pprPanic "unflatten_irred" (ppr ct)

    ----------------
    finalise_eq :: Ct -> Cts -> TcS Cts
    finalise_eq (CTyEqCan { cc_ev = ev, cc_tyvar = tv, cc_rhs = rhs }) rest
      | isFmvTyVar tv
      = do { ty1 <- zonkTcTyVar tv
           ; ty2 <- zonkTcType rhs
           ; let is_refl = ty1 `tcEqType` ty2
           ; if is_refl then do { when (isWanted ev) $
                                  setEvBind (ctEvId ev) (EvCoercion $ mkTcNomReflCo rhs)
                                ; return rest }
                        else return (mkNonCanonical ev `consCts` rest) }
      | otherwise
      = return (mkNonCanonical ev `consCts` rest)

    finalise_eq ct _ = pprPanic "finalise_irred" (ppr ct)

    ----------------
    try_fill dflags untch ev ty1 ty2
      | Just tv1 <- tcGetTyVar_maybe ty1
      , isTouchableOrFmv untch tv1
      , typeKind ty1 `isSubKind` tyVarKind tv1
      = tryFill dflags tv1 ty2 ev
      | otherwise
      = return False

tryFill :: DynFlags -> TcTyVar -> TcType -> CtEvidence -> TcS Bool
-- (tryFill tv rhs ev) sees if 'tv' is an un-filled MetaTv
-- If so, and if tv does not appear in 'rhs', set tv := rhs
-- bind the evidence (which should be a CtWanted) to Refl<rhs>
-- and return True.  Otherwise return False
tryFill dflags tv rhs ev
  = ASSERT2( not (isGiven ev), ppr ev )
    do { is_filled <- isFilledMetaTyVar tv
       ; if is_filled then return False else
    do { rhs' <- zonkTcType rhs
       ; case occurCheckExpand dflags tv rhs' of
           OC_OK rhs''    -- Normal case: fill the tyvar
             -> do { when (isWanted ev) $
                     setEvBind (ctEvId ev) (EvCoercion (mkTcNomReflCo rhs''))
                   ; setWantedTyBind tv rhs''
                   ; return True }

           _ ->  -- Occurs check
                 return False } }
\end{code}

Note [Unflatten using funeqs first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    [W] G a ~ Int
    [W] F (G a) ~ G a

do not want to end up with
    [W} F Int ~ Int
because that might actually hold!  Better to end up with the two above
unsolved constraints.  The flat form will be

    G a ~ fmv1     (CFunEqCan)
    F fmv1 ~ fmv2  (CFunEqCan)
    fmv1 ~ Int     (CTyEqCan)
    fmv1 ~ fmv2    (CTyEqCan)

Flatten using the fun-eqs first.

