{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.Tc.Solver.InertSet (
    -- * The work list
    WorkList(..), isEmptyWorkList, emptyWorkList,
    extendWorkListNonEq, extendWorkListCt,
    extendWorkListCts, extendWorkListCtList,
    extendWorkListEq, extendWorkListEqs,
    appendWorkList, extendWorkListImplic,
    workListSize,
    selectWorkItem,

    -- * The inert set
    InertSet(..),
    InertCans(..),
    emptyInert,

    noMatchableGivenDicts,
    noGivenNewtypeReprEqs, updGivenEqs,
    mightEqualLater,
    prohibitedSuperClassSolve,

    -- * Inert equalities
    InertEqs,
    foldTyEqs, delEq, findEq,
    partitionInertEqs, partitionFunEqs,
    foldFunEqs, addEqToCans,

    -- * Inert Dicts
    updDicts, delDict, addDict, filterDicts, partitionDicts,
    addSolvedDict,

    -- * Inert Irreds
    InertIrreds, delIrred, addIrreds, addIrred, foldIrreds,
    findMatchingIrreds, updIrreds, addIrredToCans,

    -- * Kick-out
    KickOutSpec(..), kickOutRewritableLHS,

    -- * Cycle breaker vars
    CycleBreakerVarStack,
    pushCycleBreakerVarStack,
    addCycleBreakerBindings,
    forAllCycleBreakerBindings_,

    -- * Solving one from another
    InteractResult(..), solveOneFromTheOther

  ) where

import GHC.Prelude

import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Solver.Types
import GHC.Tc.Utils.TcType

import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic( SwapFlag(..) )

import GHC.Core.Reduction
import GHC.Core.Predicate
import GHC.Core.TyCo.FVs
import qualified GHC.Core.TyCo.Rep as Rep
import GHC.Core.Class( Class )
import GHC.Core.TyCon
import GHC.Core.Class( classTyCon )
import GHC.Core.Unify

import GHC.Utils.Misc       ( partitionWith )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.Maybe
import GHC.Data.Bag

import Data.List.NonEmpty ( NonEmpty(..), (<|) )
import qualified Data.List.NonEmpty as NE
import Data.Function ( on )

import Control.Monad      ( forM_ )

{-
************************************************************************
*                                                                      *
*                            Worklists                                *
*  Canonical and non-canonical constraints that the simplifier has to  *
*  work on. Including their simplification depths.                     *
*                                                                      *
*                                                                      *
************************************************************************

Note [WorkList priorities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A WorkList contains canonical and non-canonical items (of all flavours).
Notice that each Ct now has a simplification depth. We may
consider using this depth for prioritization as well in the future.

As a simple form of priority queue, our worklist separates out

* equalities (wl_eqs); see Note [Prioritise equalities]
* all the rest (wl_rest)

Note [Prioritise equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very important to process equalities over class constraints:

* (Efficiency)  The general reason to do so is that if we process a
  class constraint first, we may end up putting it into the inert set
  and then kicking it out later.  That's extra work compared to just
  doing the equality first.

* (Avoiding fundep iteration) As #14723 showed, it's possible to
  get non-termination if we
      - Emit the fundep equalities for a class constraint,
        generating some fresh unification variables.
      - That leads to some unification
      - Which kicks out the class constraint
      - Which isn't solved (because there are still some more
        equalities in the work-list), but generates yet more fundeps
  Solution: prioritise equalities over class constraints

* (Class equalities) We need to prioritise equalities even if they
  are hidden inside a class constraint; see Note [Prioritise class equalities]

* (Kick-out) We want to apply this priority scheme to kicked-out
  constraints too (see the call to extendWorkListCt in kick_out_rewritable)
  E.g. a CIrredCan can be a hetero-kinded (t1 ~ t2), which may become
  homo-kinded when kicked out, and hence we want to prioritise it.

Among the equalities we prioritise ones with an empty rewriter set;
see Note [Wanteds rewrite Wanteds] in GHC.Tc.Types.Constraint, wrinkle (W1).


Note [Prioritise class equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We prioritise equalities in the solver (see selectWorkItem). But class
constraints like (a ~ b) and (a ~~ b) are actually equalities too;
see Note [The equality types story] in GHC.Builtin.Types.Prim.

Failing to prioritise these is inefficient (more kick-outs etc).
But, worse, it can prevent us spotting a "recursive knot" among
Wanted constraints.  See comment:10 of #12734 for a worked-out
example.

So we arrange to put these particular class constraints in the wl_eqs.

  NB: since we do not currently apply the substitution to the
  inert_solved_dicts, the knot-tying still seems a bit fragile.
  But this makes it better.

Note [Residual implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wl_implics in the WorkList are the residual implication
constraints that are generated while solving or canonicalising the
current worklist.  Specifically, when canonicalising
   (forall a. t1 ~ forall a. t2)
from which we get the implication
   (forall a. t1 ~ t2)
See GHC.Tc.Solver.Monad.deferTcSForAllEq

-}

-- See Note [WorkList priorities]
data WorkList
  = WL { wl_eqs     :: [Ct]  -- CEqCan, CDictCan, CIrredCan
                             -- Given and Wanted
                       -- Contains both equality constraints and their
                       -- class-level variants (a~b) and (a~~b);
                       -- See Note [Prioritise equalities]
                       -- See Note [Prioritise class equalities]

       , wl_rw_eqs  :: [Ct]  -- Like wl_eqs, but ones that have a non-empty
                             -- rewriter set; or, more precisely, did when
                             -- added to the WorkList
         -- We prioritise wl_eqs over wl_rw_eqs;
         -- see Note [Prioritise Wanteds with empty RewriterSet]
         -- in GHC.Tc.Types.Constraint for more details.

       , wl_rest    :: [Ct]

       , wl_implics :: Bag Implication  -- See Note [Residual implications]
    }

appendWorkList :: WorkList -> WorkList -> WorkList
appendWorkList
    (WL { wl_eqs = eqs1, wl_rw_eqs = rw_eqs1
        , wl_rest = rest1, wl_implics = implics1 })
    (WL { wl_eqs = eqs2, wl_rw_eqs = rw_eqs2
        , wl_rest = rest2, wl_implics = implics2 })
   = WL { wl_eqs     = eqs1     ++ eqs2
        , wl_rw_eqs  = rw_eqs1  ++ rw_eqs2
        , wl_rest    = rest1    ++ rest2
        , wl_implics = implics1 `unionBags`   implics2 }

workListSize :: WorkList -> Int
workListSize (WL { wl_eqs = eqs, wl_rw_eqs = rw_eqs, wl_rest = rest })
  = length eqs + length rw_eqs + length rest

extendWorkListEq :: RewriterSet -> Ct -> WorkList -> WorkList
extendWorkListEq rewriters ct wl
  | isEmptyRewriterSet rewriters      -- A wanted that has not been rewritten
    -- isEmptyRewriterSet: see Note [Prioritise Wanteds with empty RewriterSet]
    --                         in GHC.Tc.Types.Constraint
  = wl { wl_eqs = ct : wl_eqs wl }
  | otherwise
  = wl { wl_rw_eqs = ct : wl_rw_eqs wl }

extendWorkListEqs :: RewriterSet -> Bag Ct -> WorkList -> WorkList
-- Add [eq1,...,eqn] to the work-list
-- They all have the same rewriter set
-- The constraints will be solved in left-to-right order:
--   see Note [Work-list ordering] in GHC.Tc.Solved.Equality
extendWorkListEqs rewriters eqs wl
  | isEmptyRewriterSet rewriters
    -- isEmptyRewriterSet: see Note [Prioritise Wanteds with empty RewriterSet]
    --                         in GHC.Tc.Types.Constraint
  = wl { wl_eqs = foldr (:) (wl_eqs wl) eqs }
         -- The foldr just appends wl_eqs to the bag of eqs
  | otherwise
  = wl { wl_rw_eqs = foldr (:) (wl_rw_eqs wl) eqs }

extendWorkListNonEq :: Ct -> WorkList -> WorkList
-- Extension by non equality
extendWorkListNonEq ct wl = wl { wl_rest = ct : wl_rest wl }

extendWorkListImplic :: Implication -> WorkList -> WorkList
extendWorkListImplic implic wl = wl { wl_implics = implic `consBag` wl_implics wl }

extendWorkListCt :: Ct -> WorkList -> WorkList
-- Agnostic about what kind of constraint
extendWorkListCt ct wl
 = case classifyPredType (ctEvPred ev) of
     EqPred {}
       -> extendWorkListEq rewriters ct wl

     ClassPred cls _  -- See Note [Prioritise class equalities]
       |  isEqualityClass cls
       -> extendWorkListEq rewriters ct wl

     _ -> extendWorkListNonEq ct wl
  where
    ev = ctEvidence ct
    rewriters = ctEvRewriters ev

extendWorkListCtList :: [Ct] -> WorkList -> WorkList
extendWorkListCtList cts wl = foldr extendWorkListCt wl cts

extendWorkListCts :: Cts -> WorkList -> WorkList
extendWorkListCts cts wl = foldr extendWorkListCt wl cts

isEmptyWorkList :: WorkList -> Bool
isEmptyWorkList (WL { wl_eqs = eqs, wl_rest = rest, wl_implics = implics })
  = null eqs && null rest && isEmptyBag implics

emptyWorkList :: WorkList
emptyWorkList = WL { wl_eqs  = [], wl_rw_eqs = [], wl_rest = [], wl_implics = emptyBag }

selectWorkItem :: WorkList -> Maybe (Ct, WorkList)
-- See Note [Prioritise equalities]
selectWorkItem wl@(WL { wl_eqs = eqs, wl_rw_eqs = rw_eqs, wl_rest = rest })
  | ct:cts <- eqs    = Just (ct, wl { wl_eqs    = cts })
  | ct:cts <- rw_eqs = Just (ct, wl { wl_rw_eqs = cts })
  | ct:cts <- rest   = Just (ct, wl { wl_rest   = cts })
  | otherwise        = Nothing

-- Pretty printing
instance Outputable WorkList where
  ppr (WL { wl_eqs = eqs, wl_rw_eqs = rw_eqs, wl_rest = rest, wl_implics = implics })
   = text "WL" <+> (braces $
     vcat [ ppUnless (null eqs) $
            text "Eqs =" <+> vcat (map ppr eqs)
          , ppUnless (null rw_eqs) $
            text "RwEqs =" <+> vcat (map ppr rw_eqs)
          , ppUnless (null rest) $
            text "Non-eqs =" <+> vcat (map ppr rest)
          , ppUnless (isEmptyBag implics) $
            ifPprDebug (text "Implics =" <+> vcat (map ppr (bagToList implics)))
                       (text "(Implics omitted)")
          ])

{- *********************************************************************
*                                                                      *
                InertSet: the inert set
*                                                                      *
*                                                                      *
********************************************************************* -}

type CycleBreakerVarStack = NonEmpty (Bag (TcTyVar, TcType))
   -- ^ a stack of (CycleBreakerTv, original family applications) lists
   -- first element in the stack corresponds to current implication;
   --   later elements correspond to outer implications
   -- used to undo the cycle-breaking needed to handle
   -- Note [Type equality cycles] in GHC.Tc.Solver.Equality
   -- Why store the outer implications? For the use in mightEqualLater (only)
   --
   -- Why NonEmpty? So there is always a top element to add to

data InertSet
  = IS { inert_cans :: InertCans
              -- Canonical Given, Wanted
              -- Sometimes called "the inert set"

       , inert_cycle_breakers :: CycleBreakerVarStack

       , inert_famapp_cache :: FunEqMap Reduction
              -- Just a hash-cons cache for use when reducing family applications
              -- only
              --
              -- If    F tys :-> (co, rhs, flav),
              -- then  co :: F tys ~N rhs
              -- all evidence is from instances or Givens; no coercion holes here
              -- (We have no way of "kicking out" from the cache, so putting
              --  wanteds here means we can end up solving a Wanted with itself. Bad)

       , inert_solved_dicts :: DictMap DictCt
              -- All Wanteds, of form (C t1 .. tn)
              -- Always a dictionary solved by an instance decl; never an implict parameter
              -- See Note [Solved dictionaries]
              -- and Note [Do not add superclasses of solved dictionaries]
       }

instance Outputable InertSet where
  ppr (IS { inert_cans = ics
          , inert_solved_dicts = solved_dicts })
      = vcat [ ppr ics
             , ppUnless (null dicts) $
               text "Solved dicts =" <+> vcat (map ppr dicts) ]
         where
           dicts = bagToList (dictsToBag solved_dicts)

emptyInertCans :: InertCans
emptyInertCans
  = IC { inert_eqs          = emptyTyEqs
       , inert_funeqs       = emptyFunEqs
       , inert_given_eq_lvl = topTcLevel
       , inert_given_eqs    = False
       , inert_dicts        = emptyDictMap
       , inert_safehask     = emptyDictMap
       , inert_insts        = []
       , inert_irreds       = emptyBag }

emptyInert :: InertSet
emptyInert
  = IS { inert_cans           = emptyInertCans
       , inert_cycle_breakers = emptyBag :| []
       , inert_famapp_cache   = emptyFunEqs
       , inert_solved_dicts   = emptyDictMap }


{- Note [Solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we apply a top-level instance declaration, we add the "solved"
dictionary to the inert_solved_dicts.  In general, we use it to avoid
creating a new EvVar when we have a new goal that we have solved in
the past.

But in particular, we can use it to create *recursive* dictionaries.
The simplest, degenerate case is
    instance C [a] => C [a] where ...
If we have
    [W] d1 :: C [x]
then we can apply the instance to get
    d1 = $dfCList d
    [W] d2 :: C [x]
Now 'd1' goes in inert_solved_dicts, and we can solve d2 directly from d1.
    d1 = $dfCList d
    d2 = d1

See Note [Example of recursive dictionaries]

VERY IMPORTANT INVARIANT:

 (Solved Dictionary Invariant)
    Every member of the inert_solved_dicts is the result
    of applying an instance declaration that "takes a step"

    An instance "takes a step" if it has the form
        dfunDList d1 d2 = MkD (...) (...) (...)
    That is, the dfun is lazy in its arguments, and guarantees to
    immediately return a dictionary constructor.  NB: all dictionary
    data constructors are lazy in their arguments.

    This property is crucial to ensure that all dictionaries are
    non-bottom, which in turn ensures that the whole "recursive
    dictionary" idea works at all, even if we get something like
        rec { d = dfunDList d dx }
    See Note [Recursive superclasses] in GHC.Tc.TyCl.Instance.

 Reason:
   - All instances, except two exceptions listed below, "take a step"
     in the above sense

   - Exception 1: local quantified constraints have no such guarantee;
     indeed, adding a "solved dictionary" when applying a quantified
     constraint led to the ability to define unsafeCoerce
     in #17267.

   - Exception 2: the magic built-in instance for (~) has no
     such guarantee.  It behaves as if we had
         class    (a ~# b) => (a ~ b) where {}
         instance (a ~# b) => (a ~ b) where {}
     The "dfun" for the instance is strict in the coercion.
     Anyway there's no point in recording a "solved dict" for
     (t1 ~ t2); it's not going to allow a recursive dictionary
     to be constructed.  Ditto (~~) and Coercible.

THEREFORE we only add a "solved dictionary"
  - when applying an instance declaration
  - subject to Exceptions 1 and 2 above

In implementation terms
  - GHC.Tc.Solver.Monad.addSolvedDict adds a new solved dictionary,
    conditional on the kind of instance

  - It is only called when applying an instance decl,
    in GHC.Tc.Solver.Dict.tryInstances

  - ClsInst.InstanceWhat says what kind of instance was
    used to solve the constraint.  In particular
      * LocalInstance identifies quantified constraints
      * BuiltinEqInstance identifies the strange built-in
        instances for equality.

  - ClsInst.instanceReturnsDictCon says which kind of
    instance guarantees to return a dictionary constructor

Other notes about solved dictionaries

* See also Note [Do not add superclasses of solved dictionaries]

* The inert_solved_dicts field is not rewritten by equalities,
  so it may get out of date.

* The inert_solved_dicts are all Wanteds, never givens

* We only cache dictionaries from top-level instances, not from
  local quantified constraints.  Reason: if we cached the latter
  we'd need to purge the cache when bringing new quantified
  constraints into scope, because quantified constraints "shadow"
  top-level instances.

Note [Do not add superclasses of solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every member of inert_solved_dicts is the result of applying a
dictionary function, NOT of applying superclass selection to anything.
Consider

        class Ord a => C a where
        instance Ord [a] => C [a] where ...

Suppose we are trying to solve
  [G] d1 : Ord a
  [W] d2 : C [a]

Then we'll use the instance decl to give

  [G] d1 : Ord a     Solved: d2 : C [a] = $dfCList d3
  [W] d3 : Ord [a]

We must not add d4 : Ord [a] to the 'solved' set (by taking the
superclass of d2), otherwise we'll use it to solve d3, without ever
using d1, which would be a catastrophe.

Solution: when extending the solved dictionaries, do not add superclasses.
That's why each element of the inert_solved_dicts is the result of applying
a dictionary function.

Note [Example of recursive dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--- Example 1

    data D r = ZeroD | SuccD (r (D r));

    instance (Eq (r (D r))) => Eq (D r) where
        ZeroD     == ZeroD     = True
        (SuccD a) == (SuccD b) = a == b
        _         == _         = False;

    equalDC :: D [] -> D [] -> Bool;
    equalDC = (==);

We need to prove (Eq (D [])). Here's how we go:

   [W] d1 : Eq (D [])
By instance decl of Eq (D r):
   [W] d2 : Eq [D []]      where   d1 = dfEqD d2
By instance decl of Eq [a]:
   [W] d3 : Eq (D [])      where   d2 = dfEqList d3
                                   d1 = dfEqD d2
Now this wanted can interact with our "solved" d1 to get:
    d3 = d1

-- Example 2:
This code arises in the context of "Scrap Your Boilerplate with Class"

    class Sat a
    class Data ctx a
    instance  Sat (ctx Char)             => Data ctx Char       -- dfunData1
    instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]        -- dfunData2

    class Data Maybe a => Foo a

    instance Foo t => Sat (Maybe t)                             -- dfunSat

    instance Data Maybe a => Foo a                              -- dfunFoo1
    instance Foo a        => Foo [a]                            -- dfunFoo2
    instance                 Foo [Char]                         -- dfunFoo3

Consider generating the superclasses of the instance declaration
         instance Foo a => Foo [a]

So our problem is this
    [G] d0 : Foo t
    [W] d1 : Data Maybe [t]   -- Desired superclass

We may add the given in the inert set, along with its superclasses
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  WorkList
    [W] d1 : Data Maybe [t]

Solve d1 using instance dfunData2; d1 := dfunData2 d2 d3
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
  WorkList:
    [W] d2 : Sat (Maybe [t])
    [W] d3 : Data Maybe t

Now, we may simplify d2 using dfunSat; d2 := dfunSat d4
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
  WorkList:
    [W] d3 : Data Maybe t
    [W] d4 : Foo [t]

Now, we can just solve d3 from d01; d3 := d01
  Inert
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
  WorkList
    [W] d4 : Foo [t]

Now, solve d4 using dfunFoo2;  d4 := dfunFoo2 d5
  Inert
    [G] d0  : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
        d4 : Foo [t]
  WorkList:
    [W] d5 : Foo t

Now, d5 can be solved! d5 := d0

Result
   d1 := dfunData2 d2 d3
   d2 := dfunSat d4
   d3 := d01
   d4 := dfunFoo2 d5
   d5 := d0
-}

{- *********************************************************************
*                                                                      *
                InertCans: the canonical inerts
*                                                                      *
*                                                                      *
********************************************************************* -}

{- Note [Tracking Given equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For reasons described in (UNTOUCHABLE) in GHC.Tc.Utils.Unify
Note [Unification preconditions], we can't unify
   alpha[2] ~ Int
under a level-4 implication if there are any Given equalities
bound by the implications at level 3 of 4.  To that end, the
InertCans tracks

  inert_given_eq_lvl :: TcLevel
     -- The TcLevel of the innermost implication that has a Given
     -- equality of the sort that make a unification variable untouchable
     -- (see Note [Unification preconditions] in GHC.Tc.Utils.Unify).

We update inert_given_eq_lvl whenever we add a Given to the
inert set, in updGivenEqs.

Then a unification variable alpha[n] is untouchable iff
    n < inert_given_eq_lvl
that is, if the unification variable was born outside an
enclosing Given equality.

Exactly which constraints should trigger (UNTOUCHABLE), and hence
should update inert_given_eq_lvl?

* We do /not/ need to worry about let-bound skolems, such ast
     forall[2] a. a ~ [b] => blah
  See Note [Let-bound skolems]

* Consider an implication
      forall[2]. beta[1] => alpha[1] ~ Int
  where beta is a unification variable that has already been unified
  to () in an outer scope.  Then alpha[1] is perfectly touchable and
  we can unify alpha := Int. So when deciding whether the givens contain
  an equality, we should canonicalise first, rather than just looking at
  the /original/ givens (#8644).

 * However, we must take account of *potential* equalities. Consider the
   same example again, but this time we have /not/ yet unified beta:
      forall[2] beta[1] => ...blah...

   Because beta might turn into an equality, updGivenEqs conservatively
   treats it as a potential equality, and updates inert_give_eq_lvl

 * What about something like forall[2] a b. a ~ F b => [W] alpha[1] ~ X y z?

   That Given cannot affect the Wanted, because the Given is entirely
   *local*: it mentions only skolems bound in the very same
   implication. Such equalities need not make alpha untouchable. (Test
   case typecheck/should_compile/LocalGivenEqs has a real-life
   motivating example, with some detailed commentary.)
   Hence the 'mentionsOuterVar' test in updGivenEqs.

   However, solely to support better error messages
   (see Note [HasGivenEqs] in GHC.Tc.Types.Constraint) we also track
   these "local" equalities in the boolean inert_given_eqs field.
   This field is used only to set the ic_given_eqs field to LocalGivenEqs;
   see the function getHasGivenEqs.

   Here is a simpler case that triggers this behaviour:

     data T where
       MkT :: F a ~ G b => a -> b -> T

     f (MkT _ _) = True

   Because of this behaviour around local equality givens, we can infer the
   type of f. This is typecheck/should_compile/LocalGivenEqs2.

 * We need not look at the equality relation involved (nominal vs
   representational), because representational equalities can still
   imply nominal ones. For example, if (G a ~R G b) and G's argument's
   role is nominal, then we can deduce a ~N b.

Note [Let-bound skolems]
~~~~~~~~~~~~~~~~~~~~~~~~
If   * the inert set contains a canonical Given CEqCan (a ~ ty)
and  * 'a' is a skolem bound in this very implication,

then:
a) The Given is pretty much a let-binding, like
      f :: (a ~ b->c) => a -> a
   Here the equality constraint is like saying
      let a = b->c in ...
   It is not adding any new, local equality  information,
   and hence can be ignored by has_given_eqs

b) 'a' will have been completely substituted out in the inert set,
   so we can safely discard it.

For an example, see #9211.

See also GHC.Tc.Utils.Unify Note [Deeper level on the left] for how we ensure
that the right variable is on the left of the equality when both are
tyvars.

You might wonder whether the skolem really needs to be bound "in the
very same implication" as the equality constraint.
Consider this (c.f. #15009):

  data S a where
    MkS :: (a ~ Int) => S a

  g :: forall a. S a -> a -> blah
  g x y = let h = \z. ( z :: Int
                      , case x of
                           MkS -> [y,z])
          in ...

From the type signature for `g`, we get `y::a` .  Then when we
encounter the `\z`, we'll assign `z :: alpha[1]`, say.  Next, from the
body of the lambda we'll get

  [W] alpha[1] ~ Int                             -- From z::Int
  [W] forall[2]. (a ~ Int) => [W] alpha[1] ~ a   -- From [y,z]

Now, unify alpha := a.  Now we are stuck with an unsolved alpha~Int!
So we must treat alpha as untouchable under the forall[2] implication.

Note [Detailed InertCans Invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The InertCans represents a collection of constraints with the following properties:

  * All canonical

  * No two dictionaries with the same head
  * No two CIrreds with the same type

  * Family equations inert wrt top-level family axioms

  * Dictionaries have no matching top-level instance

  * Given family or dictionary constraints don't mention touchable
    unification variables

  * Non-CEqCan constraints are fully rewritten with respect
    to the CEqCan equalities (modulo eqCanRewrite of course;
    eg a wanted cannot rewrite a given)

  * CEqCan equalities: see Note [inert_eqs: the inert equalities]
    Also see documentation in Constraint.Ct for a list of invariants

Note [inert_eqs: the inert equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Definition [Can-rewrite relation]
A "can-rewrite" relation between flavours, written f1 >= f2, is a
binary relation with the following properties

  (R1) >= is transitive
  (R2) If f1 >= f, and f2 >= f,
       then either f1 >= f2 or f2 >= f1
  (See Note [Why R2?].)

Lemma (L0). If f1 >= f then f1 >= f1
Proof.      By property (R2), with f1=f2

Definition [Generalised substitution]
A "generalised substitution" S is a set of triples (lhs -f-> t), where
  lhs is a type variable or an exactly-saturated type family application
    (that is, lhs is a CanEqLHS)
  t is a type
  f is a flavour
such that
  (WF1) if (lhs1 -f1-> t1) in S
           (lhs2 -f2-> t2) in S
        then (f1 >= f2) implies that lhs1 does not appear within lhs2
  (WF2) if (lhs -f-> t) is in S, then t /= lhs

Definition [Applying a generalised substitution]
If S is a generalised substitution
   S(f,t0) = t,  if (t0 -fs-> t) in S, and fs >= f
           = apply S to components of t0, otherwise
See also Note [Flavours with roles].

Theorem: S(f,t0) is well defined as a function.
Proof: Suppose (lhs -f1-> t1) and (lhs -f2-> t2) are both in S,
               and  f1 >= f and f2 >= f
       Then by (R2) f1 >= f2 or f2 >= f1, which contradicts (WF1)

Notation: repeated application.
  S^0(f,t)     = t
  S^(n+1)(f,t) = S(f, S^n(t))

Definition: terminating generalised substitution
A generalised substitution S is *terminating* iff

  (IG1) there is an n such that
        for every f,t, S^n(f,t) = S^(n+1)(f,t)

By (IG1) we define S*(f,t) to be the result of exahaustively
applying S(f,_) to t.

-----------------------------------------------------------------------------
Our main invariant:
   the EqCts in inert_eqs should be a terminating generalised substitution
-----------------------------------------------------------------------------

Note that termination is not the same as idempotence.  To apply S to a
type, you may have to apply it recursively.  But termination does
guarantee that this recursive use will terminate.

Note [Why R2?]
~~~~~~~~~~~~~~
R2 states that, if we have f1 >= f and f2 >= f, then either f1 >= f2 or f2 >=
f1. If we do not have R2, we will easily fall into a loop.

To see why, imagine we have f1 >= f, f2 >= f, and that's it. Then, let our
inert set S = {a -f1-> b, b -f2-> a}. Computing S(f,a) does not terminate. And
yet, we have a hard time noticing an occurs-check problem when building S, as
the two equalities cannot rewrite one another.

R2 actually restricts our ability to accept user-written programs. See
Note [Avoiding rewriting cycles] in GHC.Tc.Types.Constraint for an example.

Note [Rewritable]
~~~~~~~~~~~~~~~~~
This Note defines what it means for a type variable or type family application
(that is, a CanEqLHS) to be rewritable in a type. This definition is used
by the anyRewritableXXX family of functions and is meant to model the actual
behaviour in GHC.Tc.Solver.Rewrite.

Ignoring roles (for now): A CanEqLHS lhs is *rewritable* in a type t if the
lhs tree appears as a subtree within t without traversing any of the following
components of t:
  * coercions (whether they appear in casts CastTy or as arguments CoercionTy)
  * kinds of variable occurrences
The check for rewritability *does* look in kinds of the bound variable of a
ForAllTy.

Goal: If lhs is not rewritable in t, then t is a fixpoint of the generalised
substitution containing only {lhs -f*-> t'}, where f* is a flavour such that f* >= f
for all f.

The reason for this definition is that the rewriter does not rewrite in coercions
or variables' kinds. In turn, the rewriter does not need to rewrite there because
those places are never used for controlling the behaviour of the solver: these
places are not used in matching instances or in decomposing equalities.

There is one exception to the claim that non-rewritable parts of the tree do
not affect the solver: we sometimes do an occurs-check to decide e.g. how to
orient an equality. (See the comments on
GHC.Tc.Solver.Equality.canEqTyVarFunEq.) Accordingly, the presence of a
variable in a kind or coercion just might influence the solver. Here is an
example:

  type family Const x y where
    Const x y = x

  AxConst :: forall x y. Const x y ~# x

  alpha :: Const Type Nat
  [W] alpha ~ Int |> (sym (AxConst Type alpha) ;;
                      AxConst Type alpha ;;
                      sym (AxConst Type Nat))

The cast is clearly ludicrous (it ties together a cast and its symmetric version),
but we can't quite rule it out. (See (EQ1) from
Note [Respecting definitional equality] in GHC.Core.TyCo.Rep to see why we need
the Const Type Nat bit.) And yet this cast will (quite rightly) prevent alpha
from unifying with the RHS. I (Richard E) don't have an example of where this
problem can arise from a Haskell program, but we don't have an air-tight argument
for why the definition of *rewritable* given here is correct.

Taking roles into account: we must consider a rewrite at a given role. That is,
a rewrite arises from some equality, and that equality has a role associated
with it. As we traverse a type, we track what role we are allowed to rewrite with.

For example, suppose we have an inert [G] b ~R# Int. Then b is rewritable in
Maybe b but not in F b, where F is a type function. This role-aware logic is
present in both the anyRewritableXXX functions and in the rewriter.
See also Note [anyRewritableTyVar must be role-aware] in GHC.Tc.Utils.TcType.

Note [Extending the inert equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Main Theorem [Stability under extension]
   Suppose we have a "work item"
       lhs -fw-> t
   and a terminating generalised substitution S,
   THEN the extended substitution T = S+(lhs -fw-> t)
        is a terminating generalised substitution
   PROVIDED
      (T1) S(fw,lhs) = lhs   -- LHS of work-item is a fixpoint of S(fw,_)
      (T2) S(fw,t)   = t     -- RHS of work-item is a fixpoint of S(fw,_)
      (T3) lhs not in t      -- No occurs check in the work item
          -- If lhs is a type family application, we require only that
          -- lhs is not *rewritable* in t. See Note [Rewritable] and
          -- Note [EqCt occurs check] in GHC.Tc.Types.Constraint.

      AND, for every (lhs1 -fs-> s) in S:
           (K0) not (fw >= fs)
                Reason: suppose we kick out (lhs1 -fs-> s),
                        and add (lhs -fw-> t) to the inert set.
                        The latter can't rewrite the former,
                        so the kick-out achieved nothing

              -- From here, we can assume fw >= fs
           OR (K4) lhs1 is a tyvar AND fs >= fw

           OR { (K1) lhs is not rewritable in lhs1. See Note [Rewritable].
                     Reason: if fw >= fs, WF1 says we can't have both
                             lhs0 -fw-> t  and  F lhs0 -fs-> s

                AND (K2): guarantees termination of the new substitution
                    {  (K2a) not (fs >= fs)
                    OR (K2b) lhs not in s }

                AND (K3) See Note [K3: completeness of solving]
                    { (K3a) If the role of fs is nominal: s /= lhs
                      (K3b) If the role of fs is representational:
                            s is not of form (lhs t1 .. tn) } }


Conditions (T1-T3) are established by the canonicaliser
Conditions (K1-K3) are established by GHC.Tc.Solver.Monad.kickOutRewritable

The idea is that
* T1 and T2 are guaranteed by exhaustively rewriting the work-item
  with S(fw,_).

* T3 is guaranteed by an occurs-check on the work item.
  This is done during canonicalisation, in checkTypeEq; invariant
  (TyEq:OC) of CEqCan. See also Note [EqCt occurs check] in GHC.Tc.Types.Constraint.

* (K1-3) are the "kick-out" criteria.  (As stated, they are really the
  "keep" criteria.) If the current inert S contains a triple that does
  not satisfy (K1-3), then we remove it from S by "kicking it out",
  and re-processing it.

* Note that kicking out is a Bad Thing, because it means we have to
  re-process a constraint.  The less we kick out, the better.
  TODO: Make sure that kicking out really *is* a Bad Thing. We've assumed
  this but haven't done the empirical study to check.

* Assume we have  G>=G, G>=W and that's all.  Then, when performing
  a unification we add a new given  a -G-> ty.  But doing so does NOT require
  us to kick out an inert wanted that mentions a, because of (K2a).  This
  is a common case, hence good not to kick out. See also (K2a) below.

* Lemma (L1): The conditions of the Main Theorem imply that there is no
              (lhs -fs-> t) in S, s.t.  (fs >= fw).
  Proof. Suppose the contrary (fs >= fw).  Then because of (T1),
  S(fw,lhs)=lhs.  But since fs>=fw, S(fw,lhs) = t, hence t=lhs.  But now we
  have (lhs -fs-> lhs) in S, which contradicts (WF2).

* The extended substitution satisfies (WF1) and (WF2)
  - (K1) plus (L1) guarantee that the extended substitution satisfies (WF1).
  - (T3) guarantees (WF2).

* (K2) and (K4) are about termination.  Intuitively, any infinite chain S^0(f,t),
  S^1(f,t), S^2(f,t).... must pass through the new work item infinitely
  often, since the substitution without the work item is terminating; and must
  pass through at least one of the triples in S infinitely often.

  - (K2a): if not(fs>=fs) then there is no f that fs can rewrite (fs>=f)
    (this is Lemma (L0)), and hence this triple never plays a role in application S(f,t).
    It is always safe to extend S with such a triple.

    (NB: we could strengthen K1) in this way too, but see K3.

  - (K2b): if lhs not in s, we have no further opportunity to apply the
    work item

  - (K4): See Note [K4]

* Lemma (L3). Suppose we have f* such that, for all f, f* >= f. Then
  if we are adding lhs -fw-> t (where T1, T2, and T3 hold), we will keep a -f*-> s.
  Proof. K4 holds; thus, we keep.

Key lemma to make it watertight.
  Under the conditions of the Main Theorem,
  forall f st fw >= f, a is not in S^k(f,t), for any k

Also, consider roles more carefully. See Note [Flavours with roles]

Note [K4]
~~~~~~~~~
K4 is a "keep" condition of Note [Extending the inert equalities].
Here is the scenario:

* We are considering adding (lhs -fw-> t) to the inert set S.
* S already has (lhs1 -fs-> s).
* We know S(fw, lhs) = lhs, S(fw, t) = t, and lhs is not rewritable in t.
  See Note [Rewritable]. These are (T1), (T2), and (T3).
* We further know fw >= fs. (If not, then we short-circuit via (K0).)

K4 says that we may keep lhs1 -fs-> s in S if:
  lhs1 is a tyvar AND fs >= fw

Why K4 guarantees termination:
  * If fs >= fw, we know a is not rewritable in t, because of (T2).
  * We further know lhs /= a, because of (T1).
  * Accordingly, a use of the new inert item lhs -fw-> t cannot create the conditions
    for a use of a -fs-> s (precisely because t does not mention a), and hence,
    the extended substitution (with lhs -fw-> t in it) is a terminating
    generalised substitution.

Recall that the termination generalised substitution includes only mappings that
pass an occurs check. This is (T3). At one point, we worried that the
argument here would fail if s mentioned a, but (T3) rules out this possibility.
Put another way: the terminating generalised substitution considers only the inert_eqs,
not other parts of the inert set (such as the irreds).

Can we liberalise K4? No.

Why we cannot drop the (fs >= fw) condition:
  * Suppose not (fs >= fw). It might be the case that t mentions a, and this
    can cause a loop. Example:

      Work:  [G] b ~ a
      Inert: [W] a ~ b

    (where G >= G, G >= W, and W >= W)
    If we don't kick out the inert, then we get a loop on e.g. [W] a ~ Int.

  * Note that the above example is different if the inert is a Given G, because
    (T1) won't hold.

Why we cannot drop the tyvar condition:
  * Presume fs >= fw. Thus, F tys is not rewritable in t, because of (T2).
  * Can the use of lhs -fw-> t create the conditions for a use of F tys -fs-> s?
    Yes! This can happen if t appears within tys.

    Here is an example:

      Work:  [G] a ~ Int
      Inert: [G] F Int ~ F a

    Now, if we have [W] F a ~ Bool, we will rewrite ad infinitum on the left-hand
    side. The key reason why K2b works in the tyvar case is that tyvars are atomic:
    if the right-hand side of an equality does not mention a variable a, then it
    cannot allow an equality with an LHS of a to fire. This is not the case for
    type family applications.

Bottom line: K4 can keep only inerts with tyvars on the left. Put differently,
K4 will never prevent an inert with a type family on the left from being kicked
out.

Consequence: We never kick out a Given/Nominal equality with a tyvar on the left.
This is Lemma (L3) of Note [Extending the inert equalities]. It is good because
it means we can effectively model the mutable filling of metavariables with
Given/Nominal equalities. That is: it should be the case that we could rewrite
our solver never to fill in a metavariable; instead, it would "solve" a wanted
like alpha ~ Int by turning it into a Given, allowing it to be used in rewriting.
We would want the solver to behave the same whether it uses metavariables or
Givens. And (L3) says that no Given/Nominals over tyvars are ever kicked out,
just like we never unfill a metavariable. Nice.

Getting this wrong (that is, allowing K4 to apply to situations with the type
family on the left) led to #19042. (At that point, K4 was known as K2b.)

Originally, this condition was part of K2, but #17672 suggests it should be
a top-level K condition.

Note [K3: completeness of solving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(K3) is not necessary for the extended substitution
to be terminating.  In fact K1 could be made stronger by saying
   ... then (not (fw >= fs) or not (fs >= fs))
But it's not enough for S to be terminating; we also want completeness.
That is, we want to be able to solve all soluble wanted equalities.
Suppose we have

   work-item   b -G-> a
   inert-item  a -W-> b

Assuming (G >= W) but not (W >= W), this fulfills all the conditions,
so we could extend the inerts, thus:

   inert-items   b -G-> a
                 a -W-> b

But if we kicked-out the inert item, we'd get

   work-item     a -W-> b
   inert-item    b -G-> a

Then rewrite the work-item gives us (a -W-> a), which is soluble via Refl.
So we add one more clause to the kick-out criteria

Another way to understand (K3) is that we treat an inert item
        a -f-> b
in the same way as
        b -f-> a
So if we kick out one, we should kick out the other.  The orientation
is somewhat accidental.

When considering roles, we also need the second clause (K3b). Consider

  work-item    c -G/N-> a
  inert-item   a -W/R-> b c

The work-item doesn't get rewritten by the inert, because (>=) doesn't hold.
But we don't kick out the inert item because not (W/R >= W/R).  So we just
add the work item. But then, consider if we hit the following:

  work-item    b -G/N-> Id
  inert-items  a -W/R-> b c
               c -G/N-> a
where
  newtype Id x = Id x

For similar reasons, if we only had (K3a), we wouldn't kick the
representational inert out. And then, we'd miss solving the inert, which
now reduced to reflexivity.

The solution here is to kick out representational inerts whenever the
lhs of a work item is "exposed", where exposed means being at the
head of the top-level application chain (lhs t1 .. tn).  See
is_can_eq_lhs_head. This is encoded in (K3b).

Beware: if we make this test succeed too often, we kick out too much,
and the solver might loop.  Consider (#14363)
  work item:   [G] a ~R f b
  inert item:  [G] b ~R f a
In GHC 8.2 the completeness tests more aggressive, and kicked out
the inert item; but no rewriting happened and there was an infinite
loop.  All we need is to have the tyvar at the head.

Note [Flavours with roles]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The system described in Note [inert_eqs: the inert equalities]
discusses an abstract
set of flavours. In GHC, flavours have two components: the flavour proper,
taken from {Wanted, Given} and the equality relation (often called
role), taken from {NomEq, ReprEq}.
When substituting w.r.t. the inert set,
as described in Note [inert_eqs: the inert equalities],
we must be careful to respect all components of a flavour.
For example, if we have

  inert set: a -G/R-> Int
             b -G/R-> Bool

  type role T nominal representational

and we wish to compute S(W/R, T a b), the correct answer is T a Bool, NOT
T Int Bool. The reason is that T's first parameter has a nominal role, and
thus rewriting a to Int in T a b is wrong. Indeed, this non-congruence of
substitution means that the proof in Note [inert_eqs: the inert equalities] may
need to be revisited, but we don't think that the end conclusion is wrong.
-}

data InertCans   -- See Note [Detailed InertCans Invariants] for more
  = IC { inert_eqs :: InertEqs
              -- See Note [inert_eqs: the inert equalities]
              -- All EqCt with a TyVarLHS; index is the LHS tyvar
              -- Domain = skolems and untouchables; a touchable would be unified

       , inert_funeqs :: InertFunEqs
              -- All EqCt with a TyFamLHS; index is the whole family head type.
              -- LHS is fully rewritten (modulo eqCanRewrite constraints)
              --     wrt inert_eqs
              -- Can include both [G] and [W]

       , inert_dicts :: DictMap DictCt
              -- Dictionaries only
              -- All fully rewritten (modulo flavour constraints)
              --     wrt inert_eqs

       , inert_insts :: [QCInst]

       , inert_safehask :: DictMap DictCt
              -- Failed dictionary resolution due to Safe Haskell overlapping
              -- instances restriction. We keep this separate from inert_dicts
              -- as it doesn't cause compilation failure, just safe inference
              -- failure.
              --
              -- ^ See Note [Safe Haskell Overlapping Instances Implementation]
              -- in GHC.Tc.Solver

       , inert_irreds :: InertIrreds
              -- Irreducible predicates that cannot be made canonical,
              --     and which don't interact with others (e.g.  (c a))
              -- and insoluble predicates (e.g.  Int ~ Bool, or a ~ [a])

       , inert_given_eq_lvl :: TcLevel
              -- The TcLevel of the innermost implication that has a Given
              -- equality of the sort that make a unification variable untouchable
              -- (see Note [Unification preconditions] in GHC.Tc.Utils.Unify).
              -- See Note [Tracking Given equalities]

       , inert_given_eqs :: Bool
              -- True <=> The inert Givens *at this level* (tcl_tclvl)
              --          could includes at least one equality /other than/ a
              --          let-bound skolem equality.
              -- Reason: report these givens when reporting a failed equality
              -- See Note [Tracking Given equalities]
       }

type InertEqs    = DTyVarEnv EqualCtList
type InertFunEqs = FunEqMap  EqualCtList
type InertIrreds = Bag IrredCt

instance Outputable InertCans where
  ppr (IC { inert_eqs = eqs
          , inert_funeqs = funeqs
          , inert_dicts = dicts
          , inert_safehask = safehask
          , inert_irreds = irreds
          , inert_given_eq_lvl = ge_lvl
          , inert_given_eqs = given_eqs
          , inert_insts = insts })

    = braces $ vcat
      [ ppUnless (isEmptyDVarEnv eqs) $
        text "Equalities ="
          <+> pprBag (foldTyEqs consBag eqs emptyBag)
      , ppUnless (isEmptyTcAppMap funeqs) $
        text "Type-function equalities ="
          <+> pprBag (foldFunEqs consBag funeqs emptyBag)
      , ppUnless (isEmptyTcAppMap dicts) $
        text "Dictionaries =" <+> pprBag (dictsToBag dicts)
      , ppUnless (isEmptyTcAppMap safehask) $
        text "Safe Haskell unsafe overlap =" <+> pprBag (dictsToBag safehask)
      , ppUnless (isEmptyBag irreds) $
        text "Irreds =" <+> pprBag irreds
      , ppUnless (null insts) $
        text "Given instances =" <+> vcat (map ppr insts)
      , text "Innermost given equalities =" <+> ppr ge_lvl
      , text "Given eqs at this level =" <+> ppr given_eqs
      ]


{- *********************************************************************
*                                                                      *
                   Inert equalities
*                                                                      *
********************************************************************* -}

emptyTyEqs :: InertEqs
emptyTyEqs = emptyDVarEnv

addEqToCans :: TcLevel -> EqCt -> InertCans -> InertCans
addEqToCans tc_lvl eq_ct@(EqCt { eq_lhs = lhs })
            ics@(IC { inert_funeqs = funeqs, inert_eqs = eqs })
  = updGivenEqs tc_lvl (CEqCan eq_ct) $
    case lhs of
       TyFamLHS tc tys -> ics { inert_funeqs = addCanFunEq funeqs tc tys eq_ct }
       TyVarLHS tv     -> ics { inert_eqs    = addTyEq eqs tv eq_ct }

addTyEq :: InertEqs -> TcTyVar -> EqCt -> InertEqs
addTyEq old_eqs tv ct
  = extendDVarEnv_C add_eq old_eqs tv [ct]
  where
    add_eq old_eqs _ = addToEqualCtList ct old_eqs

foldTyEqs :: (EqCt -> b -> b) -> InertEqs -> b -> b
foldTyEqs k eqs z
  = foldDVarEnv (\cts z -> foldr k z cts) z eqs

findTyEqs :: InertCans -> TyVar -> [EqCt]
findTyEqs icans tv = concat @Maybe (lookupDVarEnv (inert_eqs icans) tv)

delEq :: EqCt -> InertCans -> InertCans
delEq (EqCt { eq_lhs = lhs, eq_rhs = rhs }) ic = case lhs of
    TyVarLHS tv
      -> ic { inert_eqs = alterDVarEnv upd (inert_eqs ic) tv }
    TyFamLHS tf args
      -> ic { inert_funeqs = alterTcApp (inert_funeqs ic) tf args upd }
  where
    isThisOne :: EqCt -> Bool
    isThisOne (EqCt { eq_rhs = t1 }) = tcEqTypeNoKindCheck rhs t1

    upd :: Maybe EqualCtList -> Maybe EqualCtList
    upd (Just eq_ct_list) = filterEqualCtList (not . isThisOne) eq_ct_list
    upd Nothing           = Nothing

findEq :: InertCans -> CanEqLHS -> [EqCt]
findEq icans (TyVarLHS tv) = findTyEqs icans tv
findEq icans (TyFamLHS fun_tc fun_args)
  = concat @Maybe (findFunEq (inert_funeqs icans) fun_tc fun_args)

{-# INLINE partition_eqs_container #-}
partition_eqs_container
  :: forall container
   . container    -- empty container
  -> (forall b. (EqCt -> b -> b) ->  container -> b -> b) -- folder
  -> (EqCt -> container -> container)  -- extender
  -> (EqCt -> Bool)
  -> container
  -> ([EqCt], container)
partition_eqs_container empty_container fold_container extend_container pred orig_inerts
  = fold_container folder orig_inerts ([], empty_container)
  where
    folder :: EqCt -> ([EqCt], container) -> ([EqCt], container)
    folder eq_ct (acc_true, acc_false)
      | pred eq_ct = (eq_ct : acc_true, acc_false)
      | otherwise  = (acc_true,         extend_container eq_ct acc_false)

partitionInertEqs :: (EqCt -> Bool)   -- EqCt will always have a TyVarLHS
                  -> InertEqs
                  -> ([EqCt], InertEqs)
partitionInertEqs = partition_eqs_container emptyTyEqs foldTyEqs addInertEqs

addInertEqs :: EqCt -> InertEqs -> InertEqs
-- Precondition: CanEqLHS is a TyVarLHS
addInertEqs eq_ct@(EqCt { eq_lhs = TyVarLHS tv }) eqs = addTyEq eqs tv eq_ct
addInertEqs other _ = pprPanic "extendInertEqs" (ppr other)

------------------------

addCanFunEq :: InertFunEqs -> TyCon -> [TcType] -> EqCt -> InertFunEqs
addCanFunEq old_eqs fun_tc fun_args ct
  = alterTcApp old_eqs fun_tc fun_args upd
  where
    upd (Just old_equal_ct_list) = Just $ addToEqualCtList ct old_equal_ct_list
    upd Nothing                  = Just [ct]

foldFunEqs :: (EqCt -> b -> b) -> FunEqMap EqualCtList -> b -> b
foldFunEqs k fun_eqs z = foldTcAppMap (\eqs z -> foldr k z eqs) fun_eqs z

partitionFunEqs :: (EqCt -> Bool)    -- EqCt will have a TyFamLHS
                -> InertFunEqs
                -> ([EqCt], InertFunEqs)
partitionFunEqs = partition_eqs_container emptyFunEqs foldFunEqs addFunEqs

addFunEqs :: EqCt -> InertFunEqs -> InertFunEqs
-- Precondition: EqCt is a TyFamLHS
addFunEqs eq_ct@(EqCt { eq_lhs = TyFamLHS tc args }) fun_eqs
  = addCanFunEq fun_eqs tc args eq_ct
addFunEqs other _ = pprPanic "extendFunEqs" (ppr other)



{- *********************************************************************
*                                                                      *
                   Inert Dicts
*                                                                      *
********************************************************************* -}

updDicts :: (DictMap DictCt -> DictMap DictCt) -> InertCans -> InertCans
updDicts upd ics = ics { inert_dicts = upd (inert_dicts ics) }

delDict :: DictCt -> DictMap a -> DictMap a
delDict (DictCt { di_cls = cls, di_tys = tys }) m
  = delTcApp m (classTyCon cls) tys

addDict :: DictCt -> DictMap DictCt -> DictMap DictCt
addDict item@(DictCt { di_cls = cls, di_tys = tys }) dm
  = insertTcApp dm (classTyCon cls) tys item

addSolvedDict :: DictCt -> DictMap DictCt -> DictMap DictCt
addSolvedDict item@(DictCt { di_cls = cls, di_tys = tys }) dm
  = insertTcApp dm (classTyCon cls) tys item

filterDicts :: (DictCt -> Bool) -> DictMap DictCt -> DictMap DictCt
filterDicts f m = filterTcAppMap f m

partitionDicts :: (DictCt -> Bool) -> DictMap DictCt -> (Bag DictCt, DictMap DictCt)
partitionDicts f m = foldTcAppMap k m (emptyBag, emptyDictMap)
  where
    k ct (yeses, noes) | f ct      = (ct `consBag` yeses, noes)
                       | otherwise = (yeses,              addDict ct noes)


{- *********************************************************************
*                                                                      *
                   Inert Irreds
*                                                                      *
********************************************************************* -}

addIrredToCans :: TcLevel -> IrredCt -> InertCans -> InertCans
addIrredToCans tc_lvl irred ics
  = updGivenEqs tc_lvl (CIrredCan irred) $
    updIrreds (addIrred irred) ics

addIrreds :: [IrredCt] -> InertIrreds -> InertIrreds
addIrreds extras irreds
  | null extras = irreds
  | otherwise   = irreds `unionBags` listToBag extras

addIrred :: IrredCt -> InertIrreds -> InertIrreds
addIrred extra irreds = irreds `snocBag` extra

updIrreds :: (InertIrreds -> InertIrreds) -> InertCans -> InertCans
updIrreds upd ics = ics { inert_irreds = upd (inert_irreds ics) }

delIrred :: IrredCt -> InertCans -> InertCans
-- Remove a particular (Given) Irred, on the instructions of a plugin
-- For some reason this is done vis the evidence Id, not the type
-- Compare delEq.  I have not idea why
delIrred (IrredCt { ir_ev = ev }) ics
  = updIrreds (filterBag keep) ics
  where
    ev_id = ctEvEvId ev
    keep (IrredCt { ir_ev = ev' }) = ev_id /= ctEvEvId ev'

foldIrreds :: (IrredCt -> b -> b) -> InertIrreds -> b -> b
foldIrreds k irreds z = foldr k z irreds

findMatchingIrreds :: InertIrreds -> CtEvidence
                   -> (Bag (IrredCt, SwapFlag), InertIrreds)
findMatchingIrreds irreds ev
  | EqPred eq_rel1 lty1 rty1 <- classifyPredType pred
    -- See Note [Solving irreducible equalities]
  = partitionBagWith (match_eq eq_rel1 lty1 rty1) irreds
  | otherwise
  = partitionBagWith match_non_eq irreds
  where
    pred = ctEvPred ev
    match_non_eq irred
      | irredCtPred irred `tcEqTypeNoKindCheck` pred = Left (irred, NotSwapped)
      | otherwise                                    = Right irred

    match_eq eq_rel1 lty1 rty1 irred
      | EqPred eq_rel2 lty2 rty2 <- classifyPredType (irredCtPred irred)
      , eq_rel1 == eq_rel2
      , Just swap <- match_eq_help lty1 rty1 lty2 rty2
      = Left (irred, swap)
      | otherwise
      = Right irred

    match_eq_help lty1 rty1 lty2 rty2
      | lty1 `tcEqTypeNoKindCheck` lty2, rty1 `tcEqTypeNoKindCheck` rty2
      = Just NotSwapped
      | lty1 `tcEqTypeNoKindCheck` rty2, rty1 `tcEqTypeNoKindCheck` lty2
      = Just IsSwapped
      | otherwise
      = Nothing

{- Note [Solving irreducible equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#14333)
  [G] a b ~R# c d
  [W] c d ~R# a b
Clearly we should be able to solve this! Even though the constraints are
not decomposable. We solve this when looking up the work-item in the
irreducible constraints to look for an identical one.  When doing this
lookup, findMatchingIrreds spots the equality case, and matches either
way around. It has to return a swap-flag so we can generate evidence
that is the right way round too.
-}

{- *********************************************************************
*                                                                      *
                Adding to and removing from the inert set
*                                                                      *
*                                                                      *
********************************************************************* -}

updGivenEqs :: TcLevel -> Ct -> InertCans -> InertCans
-- Set the inert_given_eq_level to the current level (tclvl)
-- if the constraint is a given equality that should prevent
-- filling in an outer unification variable.
-- See Note [Tracking Given equalities]
updGivenEqs tclvl ct inerts@(IC { inert_given_eq_lvl = ge_lvl })
  | not (isGivenCt ct) = inerts
  | not_equality ct    = inerts -- See Note [Let-bound skolems]
  | otherwise          = inerts { inert_given_eq_lvl = ge_lvl'
                                , inert_given_eqs    = True }
  where
    ge_lvl' | mentionsOuterVar tclvl (ctEvidence ct)
              -- Includes things like (c a), which *might* be an equality
            = tclvl
            | otherwise
            = ge_lvl

    not_equality :: Ct -> Bool
    -- True <=> definitely not an equality of any kind
    --          except for a let-bound skolem, which doesn't count
    --          See Note [Let-bound skolems]
    -- NB: no need to spot the boxed CDictCan (a ~ b) because its
    --     superclass (a ~# b) will be a CEqCan
    not_equality (CEqCan (EqCt { eq_lhs = TyVarLHS tv })) = not (isOuterTyVar tclvl tv)
    not_equality (CDictCan {})                            = True
    not_equality _                                        = False

data KickOutSpec -- See Note [KickOutSpec]
  = KOAfterUnify  TcTyVarSet   -- We have unified these tyvars
  | KOAfterAdding CanEqLHS     -- We are adding to the inert set a canonical equality
                               -- constraint with this LHS

{- Note [KickOutSpec]
~~~~~~~~~~~~~~~~~~~~~~
KickOutSpec explains why we are kicking out.

Important property:
  KOAfterAdding (TyVarLHS tv) should behave exactly like
  KOAfterUnifying (unitVarSet tv)

The main reasons for treating the two separately are
* More efficient in the single-tyvar case
* The code is far more perspicuous
-}

kickOutRewritableLHS :: KickOutSpec -> CtFlavourRole -> InertCans -> (Cts, InertCans)
-- See Note [kickOutRewritable]
kickOutRewritableLHS ko_spec new_fr@(_, new_role)
                     ics@(IC { inert_eqs      = tv_eqs
                             , inert_dicts    = dictmap
                             , inert_funeqs   = funeqmap
                             , inert_irreds   = irreds
                             , inert_insts    = old_insts })
  = (kicked_out, inert_cans_in)
  where
    -- inert_safehask stays unchanged; is that right?
    inert_cans_in = ics { inert_eqs      = tv_eqs_in
                        , inert_dicts    = dicts_in
                        , inert_funeqs   = feqs_in
                        , inert_irreds   = irs_in
                        , inert_insts    = insts_in }

    kicked_out :: Cts
    kicked_out = (fmap CDictCan dicts_out `andCts` fmap CIrredCan irs_out)
                  `extendCtsList` insts_out
                  `extendCtsList` map CEqCan tv_eqs_out
                  `extendCtsList` map CEqCan feqs_out

    (tv_eqs_out, tv_eqs_in) = partitionInertEqs kick_out_eq tv_eqs
    (feqs_out,   feqs_in)   = partitionFunEqs   kick_out_eq funeqmap
    (dicts_out,  dicts_in)  = partitionDicts    (kick_out_ct . CDictCan) dictmap
    (irs_out,    irs_in)    = partitionBag      (kick_out_ct . CIrredCan) irreds
      -- Kick out even insolubles: See Note [Rewrite insolubles]
      -- Of course we must kick out irreducibles like (c a), in case
      -- we can rewrite 'c' to something more useful

    -- Kick-out for inert instances
    -- See Note [Quantified constraints] in GHC.Tc.Solver.Solve
    insts_out :: [Ct]
    insts_in  :: [QCInst]
    (insts_out, insts_in)
       | fr_may_rewrite (Given, NomEq)  -- All the insts are Givens
       = partitionWith kick_out_qci old_insts
       | otherwise
       = ([], old_insts)
    kick_out_qci qci
      | let ev = qci_ev qci
      , fr_can_rewrite_ty NomEq (ctEvPred (qci_ev qci))
      = Left (mkNonCanonical ev)
      | otherwise
      = Right qci

    fr_tv_can_rewrite_ty :: (TyVar -> Bool) -> EqRel -> Type -> Bool
    fr_tv_can_rewrite_ty ok_tv role ty
      = anyRewritableTyVar role can_rewrite ty
      where
        can_rewrite :: EqRel -> TyVar -> Bool
        can_rewrite old_role tv = new_role `eqCanRewrite` old_role && ok_tv tv

    fr_tf_can_rewrite_ty :: TyCon -> [TcType] -> EqRel -> Type -> Bool
    fr_tf_can_rewrite_ty new_tf new_tf_args role ty
      = anyRewritableTyFamApp role can_rewrite ty
      where
        can_rewrite :: EqRel -> TyCon -> [TcType] -> Bool
        can_rewrite old_role old_tf old_tf_args
          = new_role `eqCanRewrite` old_role &&
            tcEqTyConApps new_tf new_tf_args old_tf old_tf_args
              -- it's possible for old_tf_args to have too many. This is fine;
              -- we'll only check what we need to.

    {-# INLINE fr_can_rewrite_ty #-}   -- Perform case analysis on ko_spec only once
    fr_can_rewrite_ty :: EqRel -> Type -> Bool
    fr_can_rewrite_ty = case ko_spec of  -- See Note [KickOutSpec]
      KOAfterUnify tvs                    -> fr_tv_can_rewrite_ty (`elemVarSet` tvs)
      KOAfterAdding (TyVarLHS tv)         -> fr_tv_can_rewrite_ty (== tv)
      KOAfterAdding (TyFamLHS tf tf_args) -> fr_tf_can_rewrite_ty tf tf_args

    fr_may_rewrite :: CtFlavourRole -> Bool
    fr_may_rewrite fs = new_fr `eqCanRewriteFR` fs
        -- Can the new item rewrite the inert item?

    kick_out_ct :: Ct -> Bool
    -- Kick it out if the new CEqCan can rewrite the inert one
    -- See Note [kickOutRewritable]
    kick_out_ct ct = fr_may_rewrite fs && fr_can_rewrite_ty role (ctPred ct)
      where
        fs@(_,role) = ctFlavourRole ct

    -- Implements criteria K1-K3 in Note [Extending the inert equalities]
    kick_out_eq :: EqCt -> Bool
    kick_out_eq (EqCt { eq_lhs = lhs, eq_rhs = rhs_ty
                      , eq_ev = ev, eq_eq_rel = eq_rel })
      | not (fr_may_rewrite fs)
      = False  -- (K0) Keep it in the inert set if the new thing can't rewrite it

      -- Below here (fr_may_rewrite fs) is True

      | TyVarLHS _ <- lhs
      , fs `eqCanRewriteFR` new_fr
      = False  -- (K4) Keep it in the inert set if the LHS is a tyvar and
               -- it can rewrite the work item. See Note [K4]

      | fr_can_rewrite_ty eq_rel (canEqLHSType lhs)
      = True   -- (K1)
         -- The above check redundantly checks the role & flavour,
         -- but it's very convenient

      | kick_out_for_inertness    = True   -- (K2)
      | kick_out_for_completeness = True   -- (K3)
      | otherwise                 = False

      where
        fs = (ctEvFlavour ev, eq_rel)
        kick_out_for_inertness
          =    (fs `eqCanRewriteFR` fs)           -- (K2a)
            && fr_can_rewrite_ty eq_rel rhs_ty    -- (K2b)

        kick_out_for_completeness  -- (K3) and Note [K3: completeness of solving]
          = case eq_rel of
              NomEq  -> is_new_lhs      rhs_ty   -- (K3a)
              ReprEq -> head_is_new_lhs rhs_ty   -- (K3b)

    is_new_lhs :: Type -> Bool
    is_new_lhs = case ko_spec of   -- See Note [KickOutSpec]
          KOAfterUnify tvs  -> is_tyvar_ty_for tvs
          KOAfterAdding lhs -> (`eqType` canEqLHSType lhs)

    is_tyvar_ty_for :: TcTyVarSet -> Type -> Bool
    -- True if the type is equal to one of the tyvars
    is_tyvar_ty_for tvs ty
      = case getTyVar_maybe ty of
          Nothing -> False
          Just tv -> tv `elemVarSet` tvs

    head_is_new_lhs :: Type -> Bool
    head_is_new_lhs = case ko_spec of   -- See Note [KickOutSpec]
          KOAfterUnify tvs                    -> tv_at_head (`elemVarSet` tvs)
          KOAfterAdding (TyVarLHS tv)         -> tv_at_head (== tv)
          KOAfterAdding (TyFamLHS tf tf_args) -> fam_at_head tf tf_args

    tv_at_head :: (TyVar -> Bool) -> Type -> Bool
    tv_at_head is_tv = go
      where
        go (Rep.TyVarTy tv)    = is_tv tv
        go (Rep.AppTy fun _)   = go fun
        go (Rep.CastTy ty _)   = go ty
        go (Rep.TyConApp {})   = False
        go (Rep.LitTy {})      = False
        go (Rep.ForAllTy {})   = False
        go (Rep.FunTy {})      = False
        go (Rep.CoercionTy {}) = False

    fam_at_head :: TyCon -> [Type] -> Type -> Bool
    fam_at_head fun_tc fun_args = go
      where
        go (Rep.TyVarTy {})       = False
        go (Rep.AppTy {})         = False  -- no TyConApp to the left of an AppTy
        go (Rep.CastTy ty _)      = go ty
        go (Rep.TyConApp tc args) = tcEqTyConApps fun_tc fun_args tc args
        go (Rep.LitTy {})         = False
        go (Rep.ForAllTy {})      = False
        go (Rep.FunTy {})         = False
        go (Rep.CoercionTy {})    = False

{- Note [kickOutRewritable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [inert_eqs: the inert equalities].

When we add a new inert equality (lhs ~N ty) to the inert set,
we must kick out any inert items that could be rewritten by the
new equality, to maintain the inert-set invariants.

  - We want to kick out an existing inert constraint if
    a) the new constraint can rewrite the inert one
    b) 'lhs' is free in the inert constraint (so that it *will*)
       rewrite it if we kick it out.

    For (b) we use anyRewritableCanLHS, which examines the types /and
    kinds/ that are directly visible in the type. Hence
    we will have exposed all the rewriting we care about to make the
    most precise kinds visible for matching classes etc. No need to
    kick out constraints that mention type variables whose kinds
    contain this LHS!

  - We don't kick out constraints from inert_solved_dicts, and
    inert_solved_funeqs optimistically. But when we lookup we have to
    take the substitution into account

NB: we could in principle avoid kick-out:
  a) When unifying a meta-tyvar from an outer level, because
     then the entire implication will be iterated; see
     Note [The Unification Level Flag] in GHC.Tc.Solver.Monad.

  b) For Givens, after a unification.  By (GivenInv) in GHC.Tc.Utils.TcType
     Note [TcLevel invariants], a Given can't include a meta-tyvar from
     its own level, so it falls under (a).  Of course, we must still
     kick out Givens when adding a new non-unification Given.

But kicking out more vigorously may lead to earlier unification and fewer
iterations, so we don't take advantage of these possibilities.

Note [Rewrite insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an insoluble alpha ~ [alpha], which is insoluble
because an occurs check.  And then we unify alpha := [Int].  Then we
really want to rewrite the insoluble to [Int] ~ [[Int]].  Now it can
be decomposed.  Otherwise we end up with a "Can't match [Int] ~
[[Int]]" which is true, but a bit confusing because the outer type
constructors match.

Hence:
 * In the main simplifier loops in GHC.Tc.Solver (solveWanteds,
   simpl_loop), we feed the insolubles in solveSimpleWanteds,
   so that they get rewritten (albeit not solved).

 * We kick insolubles out of the inert set, if they can be
   rewritten (see GHC.Tc.Solver.Monad.kick_out_rewritable)

 * We rewrite those insolubles in GHC.Tc.Solver.Equality
   See Note [Make sure that insolubles are fully rewritten]
   in GHC.Tc.Solver.Equality
-}

{- *********************************************************************
*                                                                      *
                 Queries
*                                                                      *
*                                                                      *
********************************************************************* -}

mentionsOuterVar :: TcLevel -> CtEvidence -> Bool
mentionsOuterVar tclvl ev
  = anyFreeVarsOfType (isOuterTyVar tclvl) $
    ctEvPred ev

isOuterTyVar :: TcLevel -> TyCoVar -> Bool
-- True of a type variable that comes from a
-- shallower level than the ambient level (tclvl)
isOuterTyVar tclvl tv
  | isTyVar tv = assertPpr (not (isTouchableMetaTyVar tclvl tv)) (ppr tv <+> ppr tclvl) $
                 tclvl `strictlyDeeperThan` tcTyVarLevel tv
    -- ASSERT: we are dealing with Givens here, and invariant (GivenInv) from
    -- Note Note [TcLevel invariants] in GHC.Tc.Utils.TcType ensures that there can't
    -- be a touchable meta tyvar.   If this wasn't true, you might worry that,
    -- at level 3, a meta-tv alpha[3] gets unified with skolem b[2], and thereby
    -- becomes "outer" even though its level numbers says it isn't.
  | otherwise  = False  -- Coercion variables; doesn't much matter

noGivenNewtypeReprEqs :: TyCon -> InertSet -> Bool
-- True <=> there is no Irred looking like (N tys1 ~ N tys2)
-- See Note [Decomposing newtype equalities] (EX2) in GHC.Tc.Solver.Equality
--     This is the only call site.
noGivenNewtypeReprEqs tc inerts
  = not (anyBag might_help (inert_irreds (inert_cans inerts)))
  where
    might_help irred
      = case classifyPredType (ctEvPred (irredCtEvidence irred)) of
          EqPred ReprEq t1 t2
             | Just (tc1,_) <- tcSplitTyConApp_maybe t1
             , tc == tc1
             , Just (tc2,_) <- tcSplitTyConApp_maybe t2
             , tc == tc2
             -> True
          _  -> False

-- | Returns True iff there are no Given constraints that might,
-- potentially, match the given class constraint. This is used when checking to see if a
-- Given might overlap with an instance. See Note [Instance and Given overlap]
-- in GHC.Tc.Solver.Dict
noMatchableGivenDicts :: InertSet -> CtLoc -> Class -> [TcType] -> Bool
noMatchableGivenDicts inerts@(IS { inert_cans = inert_cans }) loc_w clas tys
  = not $ anyBag matchable_given $
    findDictsByClass (inert_dicts inert_cans) clas
  where
    pred_w = mkClassPred clas tys

    matchable_given :: DictCt -> Bool
    matchable_given (DictCt { di_ev = ev })
      | CtGiven { ctev_loc = loc_g, ctev_pred = pred_g } <- ev
      = isJust $ mightEqualLater inerts pred_g loc_g pred_w loc_w

      | otherwise
      = False

mightEqualLater :: InertSet -> TcPredType -> CtLoc -> TcPredType -> CtLoc -> Maybe Subst
-- See Note [What might equal later?]
-- Used to implement logic in Note [Instance and Given overlap] in GHC.Tc.Solver.Dict
mightEqualLater inert_set given_pred given_loc wanted_pred wanted_loc
  | prohibitedSuperClassSolve given_loc wanted_loc
  = Nothing

  | otherwise
  = case tcUnifyTysFG bind_fun [flattened_given] [flattened_wanted] of
      Unifiable subst
        -> Just subst
      MaybeApart reason subst
        | MARInfinite <- reason -- see Example 7 in the Note.
        -> Nothing
        | otherwise
        -> Just subst
      SurelyApart -> Nothing

  where
    in_scope  = mkInScopeSet $ tyCoVarsOfTypes [given_pred, wanted_pred]

    -- NB: flatten both at the same time, so that we can share mappings
    -- from type family applications to variables, and also to guarantee
    -- that the fresh variables are really fresh between the given and
    -- the wanted. Flattening both at the same time is needed to get
    -- Example 10 from the Note.
    ([flattened_given, flattened_wanted], var_mapping)
      = flattenTysX in_scope [given_pred, wanted_pred]

    bind_fun :: BindFun
    bind_fun tv rhs_ty
      | isMetaTyVar tv
      , can_unify tv (metaTyVarInfo tv) rhs_ty
         -- this checks for CycleBreakerTvs and TyVarTvs; forgetting
         -- the latter was #19106.
      = BindMe

         -- See Examples 4, 5, and 6 from the Note
      | Just (_fam_tc, fam_args) <- lookupVarEnv var_mapping tv
      , anyFreeVarsOfTypes mentions_meta_ty_var fam_args
      = BindMe

      | otherwise
      = Apart

    -- True for TauTv and TyVarTv (and RuntimeUnkTv) meta-tyvars
    -- (as they can be unified)
    -- and also for CycleBreakerTvs that mentions meta-tyvars
    mentions_meta_ty_var :: TyVar -> Bool
    mentions_meta_ty_var tv
      | isMetaTyVar tv
      = case metaTyVarInfo tv of
          -- See Examples 8 and 9 in the Note
          CycleBreakerTv
            -> anyFreeVarsOfType mentions_meta_ty_var
                 (lookupCycleBreakerVar tv inert_set)
          _ -> True
      | otherwise
      = False

    -- Like checkTopShape, but allows cbv variables to unify
    can_unify :: TcTyVar -> MetaInfo -> Type -> Bool
    can_unify _lhs_tv TyVarTv rhs_ty  -- see Example 3 from the Note
      | Just rhs_tv <- getTyVar_maybe rhs_ty
      = case tcTyVarDetails rhs_tv of
          MetaTv { mtv_info = TyVarTv } -> True
          MetaTv {}                     -> False  -- Could unify with anything
          SkolemTv {}                   -> True
          RuntimeUnk                    -> True
      | otherwise  -- not a var on the RHS
      = False
    can_unify lhs_tv _other _rhs_ty = mentions_meta_ty_var lhs_tv

-- | Is it (potentially) loopy to use the first @ct1@ to solve @ct2@?
--
-- Necessary (but not sufficient) conditions for this function to return @True@:
--
--   - @ct1@ and @ct2@ both arise from superclass expansion,
--   - @ct1@ is a Given and @ct2@ is a Wanted.
--
-- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance, (sc2).
prohibitedSuperClassSolve :: CtLoc -- ^ is it loopy to use this one ...
                          -> CtLoc -- ^ ... to solve this one?
                          -> Bool  -- ^ True ==> don't solve it
prohibitedSuperClassSolve given_loc wanted_loc
  | GivenSCOrigin _ _ blocked <- ctLocOrigin given_loc
  , blocked
  , ScOrigin _ NakedSc <- ctLocOrigin wanted_loc
  = True    -- Prohibited if the Wanted is a superclass
            -- and the Given has come via a superclass selection from
            -- a predicate bigger than the head
  | otherwise
  = False

{- Note [What might equal later?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must determine whether a Given might later equal a Wanted. We
definitely need to account for the possibility that any metavariable
might be arbitrarily instantiated. Yet we do *not* want
to allow skolems in to be instantiated, as we've already rewritten
with respect to any Givens. (We're solving a Wanted here, and so
all Givens have already been processed.)

This is best understood by example.

1. C alpha  ~?  C Int

   That given certainly might match later.

2. C a  ~?  C Int

   No. No new givens are going to arise that will get the `a` to rewrite
   to Int.

3. C alpha[tv]   ~?  C Int

   That alpha[tv] is a TyVarTv, unifiable only with other type variables.
   It cannot equal later.

4. C (F alpha)   ~?   C Int

   Sure -- that can equal later, if we learn something useful about alpha.

5. C (F alpha[tv])  ~?  C Int

   This, too, might equal later. Perhaps we have [G] F b ~ Int elsewhere.
   Or maybe we have C (F alpha[tv] beta[tv]), these unify with each other,
   and F x x = Int. Remember: returning True doesn't commit ourselves to
   anything.

6. C (F a)  ~?  C a

   No, this won't match later. If we could rewrite (F a) or a, we would
   have by now. But see also Red Herring below.

7. C (Maybe alpha)  ~?  C alpha

   We say this cannot equal later, because it would require
   alpha := Maybe (Maybe (Maybe ...)). While such a type can be contrived,
   we choose not to worry about it. See Note [Infinitary substitution in lookup]
   in GHC.Core.InstEnv. Getting this wrong let to #19107, tested in
   typecheck/should_compile/T19107.

8. C cbv   ~?  C Int
   where cbv = F a

   The cbv is a cycle-breaker var which stands for F a. See
   Note [Type equality cycles] in GHC.Tc.Solver.Equality
   This is just like case 6, and we say "no". Saying "no" here is
   essential in getting the parser to type-check, with its use of DisambECP.

9. C cbv   ~?   C Int
   where cbv = F alpha

   Here, we might indeed equal later. Distinguishing between
   this case and Example 8 is why we need the InertSet in mightEqualLater.

10. C (F alpha, Int)  ~?  C (Bool, F alpha)

   This cannot equal later, because F a would have to equal both Bool and
   Int.

To deal with type family applications, we use the Core flattener. See
Note [Flattening type-family applications when matching instances] in GHC.Core.Unify.
The Core flattener replaces all type family applications with
fresh variables. The next question: should we allow these fresh
variables in the domain of a unifying substitution?

A type family application that mentions only skolems (example 6) is settled:
any skolems would have been rewritten w.r.t. Givens by now. These type family
applications match only themselves. A type family application that mentions
metavariables, on the other hand, can match anything. So, if the original type
family application contains a metavariable, we use BindMe to tell the unifier
to allow it in the substitution. On the other hand, a type family application
with only skolems is considered rigid.

This treatment fixes #18910 and is tested in
typecheck/should_compile/InstanceGivenOverlap{,2}

Red Herring
~~~~~~~~~~~
In #21208, we have this scenario:

instance forall b. C b
[G] C a[sk]
[W] C (F a[sk])

What should we do with that wanted? According to the logic above, the Given
cannot match later (this is example 6), and so we use the global instance.
But wait, you say: What if we learn later (say by a future type instance F a = a)
that F a unifies with a? That looks like the Given might really match later!

This mechanism described in this Note is *not* about this kind of situation, however.
It is all asking whether a Given might match the Wanted *in this run of the solver*.
It is *not* about whether a variable might be instantiated so that the Given matches,
or whether a type instance introduced in a downstream module might make the Given match.
The reason we care about what might match later is only about avoiding order-dependence.
That is, we don't want to commit to a course of action that depends on seeing constraints
in a certain order. But an instantiation of a variable and a later type instance
don't introduce order dependency in this way, and so mightMatchLater is right to ignore
these possibilities.

Here is an example, with no type families, that is perhaps clearer:

instance forall b. C (Maybe b)
[G] C (Maybe Int)
[W] C (Maybe a)

What to do? We *might* say that the Given could match later and should thus block
us from using the global instance. But we don't do this. Instead, we rely on class
coherence to say that choosing the global instance is just fine, even if later we
call a function with (a := Int). After all, in this run of the solver, [G] C (Maybe Int)
will definitely never match [W] C (Maybe a). (Recall that we process Givens before
Wanteds, so there is no [G] a ~ Int hanging about unseen.)

Interestingly, in the first case (from #21208), the behavior changed between
GHC 8.10.7 and GHC 9.2, with the latter behaving correctly and the former
reporting overlapping instances.

Test case: typecheck/should_compile/T21208.

-}

{- *********************************************************************
*                                                                      *
    Cycle breakers
*                                                                      *
********************************************************************* -}

-- | Return the type family application a CycleBreakerTv maps to.
lookupCycleBreakerVar :: TcTyVar    -- ^ cbv, must be a CycleBreakerTv
                      -> InertSet
                      -> TcType     -- ^ type family application the cbv maps to
lookupCycleBreakerVar cbv (IS { inert_cycle_breakers = cbvs_stack })
-- This function looks at every environment in the stack. This is necessary
-- to avoid #20231. This function (and its one usage site) is the only reason
-- that we store a stack instead of just the top environment.
  | Just tyfam_app <- assert (isCycleBreakerTyVar cbv) $
                      firstJusts (NE.map (lookupBag cbv) cbvs_stack)
  = tyfam_app
  | otherwise
  = pprPanic "lookupCycleBreakerVar found an unbound cycle breaker" (ppr cbv $$ ppr cbvs_stack)

-- | Push a fresh environment onto the cycle-breaker var stack. Useful
-- when entering a nested implication.
pushCycleBreakerVarStack :: CycleBreakerVarStack -> CycleBreakerVarStack
pushCycleBreakerVarStack = (emptyBag <|)

-- | Add a new cycle-breaker binding to the top environment on the stack.
addCycleBreakerBindings :: Bag (TcTyVar, Type)   -- ^ (cbv,expansion) pairs
                        -> InertSet -> InertSet
addCycleBreakerBindings prs ics
  = assertPpr (all (isCycleBreakerTyVar . fst) prs) (ppr prs) $
    ics { inert_cycle_breakers = add_to (inert_cycle_breakers ics) }
  where
    add_to (top_env :| rest_envs) = (prs `unionBags` top_env) :| rest_envs

-- | Perform a monadic operation on all pairs in the top environment
-- in the stack.
forAllCycleBreakerBindings_ :: Monad m
                            => CycleBreakerVarStack
                            -> (TcTyVar -> TcType -> m ()) -> m ()
forAllCycleBreakerBindings_ (top_env :| _rest_envs) action
  = forM_ top_env (uncurry action)
{-# INLINABLE forAllCycleBreakerBindings_ #-}  -- to allow SPECIALISE later


{- *********************************************************************
*                                                                      *
         Solving one from another
*                                                                      *
********************************************************************* -}

data InteractResult
   = KeepInert   -- Keep the inert item, and solve the work item from it
                 -- (if the latter is Wanted; just discard it if not)
   | KeepWork    -- Keep the work item, and solve the inert item from it

instance Outputable InteractResult where
  ppr KeepInert = text "keep inert"
  ppr KeepWork  = text "keep work-item"

solveOneFromTheOther :: Ct  -- Inert    (Dict or Irred)
                     -> Ct  -- WorkItem (same predicate as inert)
                     -> InteractResult
-- Precondition:
-- * inert and work item represent evidence for the /same/ predicate
-- * Both are CDictCan or CIrredCan
--
-- We can always solve one from the other: even if both are wanted,
-- although we don't rewrite wanteds with wanteds, we can combine
-- two wanteds into one by solving one from the other

solveOneFromTheOther ct_i ct_w
  | CtWanted { ctev_loc = loc_w } <- ev_w
  , prohibitedSuperClassSolve loc_i loc_w
  -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
  = -- Inert must be Given
    KeepWork

  | CtWanted {} <- ev_w
  = -- Inert is Given or Wanted
    case ev_i of
      CtGiven {} -> KeepInert
        -- work is Wanted; inert is Given: easy choice.

      CtWanted {} -- Both are Wanted
        -- If only one has no pending superclasses, use it
        -- Otherwise we can get infinite superclass expansion (#22516)
        -- in silly cases like   class C T b => C a b where ...
        | not is_psc_i, is_psc_w     -> KeepInert
        | is_psc_i,     not is_psc_w -> KeepWork

        -- If only one is a WantedSuperclassOrigin (arising from expanding
        -- a Wanted class constraint), keep the other: wanted superclasses
        -- may be unexpected by users
        | not is_wsc_orig_i, is_wsc_orig_w     -> KeepInert
        | is_wsc_orig_i,     not is_wsc_orig_w -> KeepWork

        -- otherwise, just choose the lower span
        -- reason: if we have something like (abs 1) (where the
        -- Num constraint cannot be satisfied), it's better to
        -- get an error about abs than about 1.
        -- This test might become more elaborate if we see an
        -- opportunity to improve the error messages
        | ((<) `on` ctLocSpan) loc_i loc_w -> KeepInert
        | otherwise                        -> KeepWork

  -- From here on the work-item is Given

  | CtWanted { ctev_loc = loc_i } <- ev_i
  , prohibitedSuperClassSolve loc_w loc_i
  = KeepInert   -- Just discard the un-usable Given
                -- This never actually happens because
                -- Givens get processed first

  | CtWanted {} <- ev_i
  = KeepWork

  -- From here on both are Given
  -- See Note [Replacement vs keeping]

  | lvl_i `sameDepthAs` lvl_w
  = same_level_strategy

  | otherwise   -- Both are Given, levels differ
  = different_level_strategy
  where
     ev_i  = ctEvidence ct_i
     ev_w  = ctEvidence ct_w

     pred  = ctEvPred ev_i

     loc_i  = ctEvLoc ev_i
     loc_w  = ctEvLoc ev_w
     orig_i = ctLocOrigin loc_i
     orig_w = ctLocOrigin loc_w
     lvl_i  = ctLocLevel loc_i
     lvl_w  = ctLocLevel loc_w

     is_psc_w = isPendingScDict ct_w
     is_psc_i = isPendingScDict ct_i

     is_wsc_orig_i = isWantedSuperclassOrigin orig_i
     is_wsc_orig_w = isWantedSuperclassOrigin orig_w

     different_level_strategy  -- Both Given
       | isIPLikePred pred = if lvl_w `strictlyDeeperThan` lvl_i then KeepWork  else KeepInert
       | otherwise         = if lvl_w `strictlyDeeperThan` lvl_i then KeepInert else KeepWork
       -- See Note [Replacement vs keeping] part (1)
       -- For the isIPLikePred case see Note [Shadowing of implicit parameters]
       --                               in GHC.Tc.Solver.Dict

     same_level_strategy -- Both Given
       = case (orig_i, orig_w) of

           (GivenSCOrigin _ depth_i blocked_i, GivenSCOrigin _ depth_w blocked_w)
             | blocked_i, not blocked_w -> KeepWork  -- Case 2(a) from
             | not blocked_i, blocked_w -> KeepInert -- Note [Replacement vs keeping]

             -- Both blocked or both not blocked

             | depth_w < depth_i -> KeepWork   -- Case 2(c) from
             | otherwise         -> KeepInert  -- Note [Replacement vs keeping]

           (GivenSCOrigin {}, _) -> KeepWork  -- Case 2(b) from Note [Replacement vs keeping]

           _ -> KeepInert  -- Case 2(d) from Note [Replacement vs keeping]

{-
Note [Replacement vs keeping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have two Given constraints both of type (C tys), say, which should
we keep?  More subtle than you might think! This is all implemented in
solveOneFromTheOther.

  1) Constraints come from different levels (different_level_strategy)

      - For implicit parameters we want to keep the innermost (deepest)
        one, so that it overrides the outer one.
        See Note [Shadowing of implicit parameters] in GHC.Tc.Solver.Dict

      - For everything else, we want to keep the outermost one.  Reason: that
        makes it more likely that the inner one will turn out to be unused,
        and can be reported as redundant.  See Note [Tracking redundant constraints]
        in GHC.Tc.Solver.

        It transpires that using the outermost one is responsible for an
        8% performance improvement in nofib cryptarithm2, compared to
        just rolling the dice.  I didn't investigate why.

  2) Constraints coming from the same level (i.e. same implication)

       (a) If both are GivenSCOrigin, choose the one that is unblocked if possible
           according to Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance.

       (b) Prefer constraints that are not superclass selections. Example:

             f :: (Eq a, Ord a) => a -> Bool
             f x = x == x

           Eager superclass expansion gives us two [G] Eq a constraints. We
           want to keep the one from the user-written Eq a, not the superclass
           selection. This means we report the Ord a as redundant with
           -Wredundant-constraints, not the Eq a.

           Getting this wrong was #20602. See also
           Note [Tracking redundant constraints] in GHC.Tc.Solver.

       (c) If both are GivenSCOrigin, chooose the one with the shallower
           superclass-selection depth, in the hope of identifying more correct
           redundant constraints. This is really a generalization of point (b),
           because the superclass depth of a non-superclass constraint is 0.

           (If the levels differ, we definitely won't have both with GivenSCOrigin.)

       (d) Finally, when there is still a choice, use KeepInert rather than
           KeepWork, for two reasons:
             - to avoid unnecessary munging of the inert set.
             - to cut off superclass loops; see Note [Superclass loops] in GHC.Tc.Solver.Dict

Doing the level-check for implicit parameters, rather than making the work item
always override, is important.  Consider

    data T a where { T1 :: (?x::Int) => T Int; T2 :: T a }

    f :: (?x::a) => T a -> Int
    f T1 = ?x
    f T2 = 3

We have a [G] (?x::a) in the inert set, and at the pattern match on T1 we add
two new givens in the work-list:  [G] (?x::Int)
                                  [G] (a ~ Int)
Now consider these steps
  - process a~Int, kicking out (?x::a)
  - process (?x::Int), the inner given, adding to inert set
  - process (?x::a), the outer given, overriding the inner given
Wrong!  The level-check ensures that the inner implicit parameter wins.
(Actually I think that the order in which the work-list is processed means
that this chain of events won't happen, but that's very fragile.)
-}
