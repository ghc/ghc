{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Tc.Solver.InertSet (
    -- * The work list
    WorkList(..), isEmptyWorkList, emptyWorkList,
    extendWorkListNonEq, extendWorkListCt,
    extendWorkListCts, extendWorkListCtList,
    extendWorkListEq, extendWorkListChildEqs,
    extendWorkListRewrittenEqs,
    appendWorkList, extendWorkListImplic,
    workListSize,

    -- * The inert set
    InertSet(..),
    InertCans(..),
    emptyInert,

    noGivenNewtypeReprEqs, updGivenEqs,
    prohibitedSuperClassSolve,

    -- * Inert equalities
    InertEqs,
    foldTyEqs, delEq, findEq,
    partitionInertEqs, partitionFunEqs,
    filterInertEqs, filterFunEqs,
    inertGivens,
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
import GHC.Tc.Types.CtLoc( CtLoc, ctLocOrigin, ctLocSpan, ctLocLevel )
import GHC.Tc.Solver.Types
import GHC.Tc.Utils.TcType

import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Unique( hasKey )
import GHC.Types.Basic( SwapFlag(..) )

import GHC.Core.Reduction
import GHC.Core.Predicate
import qualified GHC.Core.TyCo.Rep as Rep
import GHC.Core.TyCon
import GHC.Core.Class( classTyCon )
import GHC.Builtin.Names( eqPrimTyConKey, heqTyConKey, eqTyConKey, coercibleTyConKey )
import GHC.Utils.Misc       ( partitionWith )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.Bag

import Control.Monad      ( forM_ )
import Data.List.NonEmpty ( NonEmpty(..), (<|) )
import Data.Function      ( on )

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

Further refinements:

* Among the equalities we prioritise ones with an empty rewriter set;
  see Note [Wanteds rewrite Wanteds] in GHC.Tc.Types.Constraint, wrinkle (W1).

* Among equalities with an empty rewriter set, we prioritise nominal equalities.
   * They have more rewriting power, so doing them first is better.
   * Prioritising them ameliorates the incompleteness of newtype
     solving: see (Ex2) in Note [Decomposing newtype equalities] in
     GHC.Tc.Solver.Equality.

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
  = WL { wl_eqs_N :: [Ct]  -- /Nominal/ equalities (s ~#N t), (s ~ t), (s ~~ t)
                           -- with definitely-empty rewriter set

       , wl_eqs_X :: [Ct]  -- CEqCan, CDictCan, CIrredCan
                           -- with definitely-empty rewriter set
           -- All other equalities: contains both equality constraints and
           -- their class-level variants (a~b) and (a~~b);
           -- See Note [Prioritise equalities]
           -- See Note [Prioritise class equalities]

       , wl_rw_eqs  :: [Ct]  -- Like wl_eqs, but ones that may have a non-empty
                             -- rewriter set
         -- We prioritise wl_eqs over wl_rw_eqs;
         -- see Note [Prioritise Wanteds with empty RewriterSet]
         -- in GHC.Tc.Types.Constraint for more details.

       , wl_rest :: [Ct]

       , wl_implics :: Bag Implication  -- See Note [Residual implications]
    }

isNominalEqualityCt :: Ct -> Bool
-- A nominal equality, primitive or not,
--                     canonical or not
--     (s ~# t), (s ~ t), or (s ~~ t)
isNominalEqualityCt ct
  | Just tc <- tcTyConAppTyCon_maybe (ctPred ct)
  = tc `hasKey` eqPrimTyConKey || tc `hasKey` heqTyConKey || tc `hasKey` eqTyConKey
  | otherwise
  = False

appendWorkList :: WorkList -> WorkList -> WorkList
appendWorkList
    (WL { wl_eqs_N = eqs1_N, wl_eqs_X = eqs1_X, wl_rw_eqs = rw_eqs1
        , wl_rest = rest1, wl_implics = implics1 })
    (WL { wl_eqs_N = eqs2_N, wl_eqs_X = eqs2_X, wl_rw_eqs = rw_eqs2
        , wl_rest = rest2, wl_implics = implics2 })
   = WL { wl_eqs_N   = eqs1_N   ++ eqs2_N
        , wl_eqs_X   = eqs1_X   ++ eqs2_X
        , wl_rw_eqs  = rw_eqs1  ++ rw_eqs2
        , wl_rest    = rest1    ++ rest2
        , wl_implics = implics1 `unionBags`   implics2 }

workListSize :: WorkList -> Int
workListSize (WL { wl_eqs_N = eqs_N, wl_eqs_X = eqs_X, wl_rw_eqs = rw_eqs, wl_rest = rest })
  = length eqs_N + length eqs_X + length rw_eqs + length rest

extendWorkListEq :: RewriterSet -> Ct -> WorkList -> WorkList
extendWorkListEq rewriters ct
    wl@(WL { wl_eqs_N = eqs_N, wl_eqs_X = eqs_X, wl_rw_eqs = rw_eqs })
  | isEmptyRewriterSet rewriters      -- A wanted that has not been rewritten
    -- isEmptyRewriterSet: see Note [Prioritise Wanteds with empty RewriterSet]
    --                         in GHC.Tc.Types.Constraint
  = if isNominalEqualityCt ct
    then wl { wl_eqs_N = ct : eqs_N }
    else wl { wl_eqs_X = ct : eqs_X }

  | otherwise
  = wl { wl_rw_eqs = ct : rw_eqs }

extendWorkListChildEqs :: CtEvidence -> Bag Ct -> WorkList -> WorkList
-- Add [eq1,...,eqn] to the work-list
-- The constraints will be solved in left-to-right order:
--   see Note [Work-list ordering] in GHC.Tc.Solver.Equality
--
-- Precondition: if the parent constraint has an empty
--               rewriter set, so will the new equalities
-- Precondition: new_eqs is non-empty
extendWorkListChildEqs parent_ev new_eqs
    wl@(WL { wl_eqs_N = eqs_N, wl_eqs_X = eqs_X, wl_rw_eqs = rw_eqs })
  | isEmptyRewriterSet (ctEvRewriters parent_ev)
    -- isEmptyRewriterSet: see Note [Prioritise Wanteds with empty RewriterSet]
    --                         in GHC.Tc.Types.Constraint
    -- If the rewriter set is empty, add to wl_eqs_X and wl_eqs_N
  = case partitionBag isNominalEqualityCt new_eqs of
       (new_eqs_N, new_eqs_X)
          | isEmptyBag new_eqs_N -> wl { wl_eqs_X = new_eqs_X `push_on_front` eqs_X }
          | isEmptyBag new_eqs_X -> wl { wl_eqs_N = new_eqs_N `push_on_front` eqs_N }
          | otherwise            -> wl { wl_eqs_N = new_eqs_N `push_on_front` eqs_N
                                       , wl_eqs_X = new_eqs_X `push_on_front` eqs_X }
          -- These isEmptyBag tests are just trying
          -- to avoid creating unnecessary thunks

  | otherwise  -- If the rewriter set is non-empty, add to wl_rw_eqs
  = wl { wl_rw_eqs = new_eqs `push_on_front` rw_eqs }
  where
    push_on_front :: Bag Ct -> [Ct] -> [Ct]
    -- push_on_front puts the new equalities on the front of the queue
    push_on_front new_eqs eqs = foldr (:) eqs new_eqs

extendWorkListRewrittenEqs :: [EqCt] -> WorkList -> WorkList
-- Don't bother checking the RewriterSet: just pop them into wl_rw_eqs
extendWorkListRewrittenEqs new_eqs wl@(WL { wl_rw_eqs = rw_eqs })
  = wl { wl_rw_eqs = foldr ((:) . CEqCan) rw_eqs new_eqs }

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
isEmptyWorkList (WL { wl_eqs_N = eqs_N, wl_eqs_X = eqs_X, wl_rw_eqs = rw_eqs
                    , wl_rest = rest, wl_implics = implics })
  = null eqs_N && null eqs_X && null rw_eqs && null rest && isEmptyBag implics

emptyWorkList :: WorkList
emptyWorkList = WL { wl_eqs_N = [], wl_eqs_X = []
                   , wl_rw_eqs = [], wl_rest = [], wl_implics = emptyBag }

-- Pretty printing
instance Outputable WorkList where
  ppr (WL { wl_eqs_N = eqs_N, wl_eqs_X = eqs_X, wl_rw_eqs = rw_eqs
          , wl_rest = rest, wl_implics = implics })
   = text "WL" <+> (braces $
     vcat [ ppUnless (null eqs_N) $
            text "Eqs_N =" <+> vcat (map ppr eqs_N)
          , ppUnless (null eqs_X) $
            text "Eqs_X =" <+> vcat (map ppr eqs_X)
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

emptyInertCans :: TcLevel -> InertCans
emptyInertCans given_eq_lvl
  = IC { inert_eqs          = emptyTyEqs
       , inert_funeqs       = emptyFunEqs
       , inert_given_eq_lvl = given_eq_lvl
       , inert_given_eqs    = False
       , inert_dicts        = emptyDictMap
       , inert_safehask     = emptyDictMap
       , inert_insts        = []
       , inert_irreds       = emptyBag }

emptyInert :: TcLevel -> InertSet
emptyInert given_eq_lvl
  = IS { inert_cans           = emptyInertCans given_eq_lvl
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

(TGE1) We do /not/ need to worry about let-bound skolems, such as
     forall[2] a. a ~ [b] => blah
  See Note [Let-bound skolems] and the isOuterTyVar tests in `updGivenEqs`

(TGE2) However, solely to support better error messages (see Note [HasGivenEqs] in
   GHC.Tc.Types.Constraint) we also track these "local" equalities in the
   boolean inert_given_eqs field.  This field is used only subsequntly (see
   `getHasGivenEqs`), to set the ic_given_eqs field to LocalGivenEqs.

(TGE3) Consider an implication
      forall[2]. beta[1] => alpha[1] ~ Int
  where beta is a unification variable that has already been unified
  to () in an outer scope.  Then alpha[1] is perfectly touchable and
  we can unify alpha := Int. So when deciding whether the givens contain
  an equality, we should canonicalise first, rather than just looking at
  the /original/ givens (#8644).

(TGE4) However, we must take account of *potential* equalities. Consider the
   same example again, but this time we have /not/ yet unified beta:
      forall[2] beta[1] => ...blah...

   Because beta might turn into an equality, updGivenEqs conservatively
   treats it as a potential equality, and updates inert_give_eq_lvl

(TGE5) We should not look at the equality relation involved (nominal vs
   representational), because representational equalities can still
   imply nominal ones. For example, if (G a ~R G b) and G's argument's
   role is nominal, then we can deduce a ~N b.

(TGE6) A subtle point is this: when initialising the solver, giving it
   an empty InertSet, we must conservatively initialise `inert_given_lvl`
   to the /current/ TcLevel.  This matters when doing let-generalisation.
   Consider #26004:
      f w e = case e of
                  T1 -> let y = not w in False   -- T1 is a GADT
                  T2 -> True
   When let-generalising `y`, we will have (w :: alpha[1]) in the type
   envt; and we are under GADT pattern match.  So when we solve the
   constraints from y's RHS, in simplifyInfer, we must NOT unify
       alpha[1] := Bool
   Since we don't know what enclosing equalities there are, we just
   conservatively assume that there are some.

   This initialisation in done in `runTcSWithEvBinds`, which passes
   the current TcLevl to `emptyInert`.

Historical note: prior to #24938 we also ignored Given equalities that
did not mention an "outer" type variable.  But that is wrong, as #24938
showed. Another example is immortalised in test LocalGivenEqs2
   data T where
      MkT :: F a ~ G b => a -> b -> T
   f (MkT _ _) = True
We should not infer the type for `f`; let-bound-skolems does not apply.

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

The actual test is in `isLetBoundSkolemCt`

Wrinkles:

(LBS1) See GHC.Tc.Utils.Unify Note [Deeper level on the left] for how we ensure
       that the correct variable is on the left of the equality when both are
       tyvars.

(LBS2) We also want this to work for
            forall a. [G] F b ~ a   (CEqCt with TyFamLHS)
   Here the Given will have a TyFamLHS, with the skolem-bound tyvar on the RHS.
   See tests T24938a, and LocalGivenEqs.

(LBS3) Happily (LBS2) also makes cycle-breakers work. Suppose we have
            forall a. [G] (F a) Int ~ a
  where F has arity 1, and `a` is the locally-bound skolem.  Then, as
  Note [Type equality cycles] explains, we split into
           [G] F a ~ cbv, [G] cbv Int ~ a
  where `cbv` is the cycle breaker variable.  But cbv has the same level
  as `a`, so `isOuterTyVar` (called in `isLetBoundSkolemCt`) will return False.

  This actually matters occasionally: see test LocalGivenEqs.

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

Possible future improvements.  The current test just looks to see whether one
side of an equality is a locally-bound skolem.  But actually we could, in
theory, do better: if one side (or both sides, actually) of an equality
ineluctably mentions a local skolem, then the equality cannot possibly impact
types outside of the implication (because doing to would cause those types to be
ill-scoped). The problem is the "ineluctably": this means that no expansion,
other solving, etc., could possibly get rid of the variable. This is hard,
perhaps impossible, to know for sure, especially when we think about type family
interactions. (And it's a user-visible property so we don't want it to be hard
to predict.) So we keep the existing check, looking for one lone variable,
because we're sure that variable isn't going anywhere.

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
Our main invariant:
   the EqCts in inert_eqs should be a
        terminating generalised substitution

-------------- Definition [Can-rewrite relation] --------------
A "can-rewrite" relation between flavours, written f1 >= f2, is a
binary relation with the following properties

  (R1) >= is transitive
  (R2) If f1 >= f, and f2 >= f,
       then either f1 >= f2 or f2 >= f1
  (See Note [Why R2?].)

Lemma (L0). If f1 >= f then f1 >= f1
Proof.      By property (R2), with f1=f2

--------- Definition [Generalised substitution] ---------------
A "generalised substitution" S is a set of triples (lhs -f-> t), where
  - lhs is a type variable or an exactly-saturated type family application
                  (that is, lhs is a CanEqLHS)
  - t is a type
  - f is a flavour

such that

  (WF1) if (lhs1 -f1-> t1) in S
           (lhs2 -f2-> t2) in S
        then (f1 >= f2) implies that lhs1 does not appear within lhs2

  (WF2) if (lhs -f-> t) is in S, then t /= lhs

  (WF3) No LHS in S is rewritable in an RHS in S,
        in the argument of a type family application (F ty1..tyn)
        where F heads a LHS in S

--------- Definition [Applying a generalised substitution] ----------
If S is a generalised substitution
   S(f,lhs)      = rhs,             if (lhs -fs-> rhs) in S, and fs >= f
   S(f,T t1..tn) = T S(f1,t1)..S(fn,tn)
   S(f,t1 t2)    = S(f,t1) S(f_N,t2)
   S(f,t)        = t
Here f1..fn are obtained from f and T using the roles of T, and f_N is
the nominal version of f.  See Note [Flavours with roles].

Notation: repeated application.
  S^0(f,t)     = t
  S^(n+1)(f,t) = S(f, S^n(t))
  S*(f,t) is the result of applying S until you reach a fixpoint

---------  Definition [Terminating generalised substitution] ---------
A generalised substitution S is *terminating* iff

  (IG1) for every f,t, there is an n such that
             S^n(f,t) = S^(n+1)(f,t)

By (IG1) we define S*(f,t) to be the result of exahaustively
applying S(f,_) to t.
--------- End of definitions ------------------------------------


Rationale for (WF1)-(WF3)
-------------------------
* (WF1) guarantees that S is well-defined /as a function/;
  see Theorem (S is a function)

   Theorem (S is a function): S(f,t0) is well defined as a function.
   Proof: Suppose (lhs -f1-> t1) and (lhs -f2-> t2) are both in S,
               and  f1 >= f and f2 >= f
          Then by (R2) f1 >= f2 or f2 >= f1, which contradicts (WF1)
   Note: this argument isn't quite right.  WF1 ensures that lhs1 does
   not appear inside lhs2, and that guarantees confluence. But I can't quite
   see how to make that argument precise.

* (WF2) is a bit trivial.  It means that if S is terminating, so that
  S^(n+1)(f,t) = S^n(f,t), then there is no LHS of S in S^n(f,t).  We
  never get a silly infinite sequence a -> a -> a -> a  .... which is
  technically a fixed point but would still go on for ever.

* (WF3) is need for the termination proof.

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
Definition. A CanEqLHS lhs is *rewritable* in a type t if the
lhs tree appears as a subtree within t without traversing any of the following
components of t:
  * coercions (whether they appear in casts CastTy or as arguments CoercionTy)
  * kinds of variable occurrences
The check for rewritability *does* look in kinds of the bound variable of a
ForAllTy.

The reason for this definition is that the rewriter does not rewrite in coercions
or variables' kinds. In turn, the rewriter does not need to rewrite there because
those places are never used for controlling the behaviour of the solver: these
places are not used in matching instances or in decomposing equalities.

This definition is used by the anyRewritableXXX family of functions and is meant
to model the actual behaviour in GHC.Tc.Solver.Rewrite.

Goal: If lhs is not rewritable in t, then t is a fixpoint of the generalised
substitution containing only {lhs -f*-> t'}, where f* is a flavour such that f* >= f
for all f.

Wrinkles

* Taking roles into account: we must consider a rewrite at a given role. That is,
  a rewrite arises from some equality, and that equality has a role associated
  with it. As we traverse a type, we track what role we are allowed to rewrite with.

  For example, suppose we have an inert [G] b ~R# Int. Then b is rewritable in
  Maybe b but not in F b, where F is a type function. This role-aware logic is
  present in both the anyRewritableXXX functions and in the rewriter.
  See also Note [anyRewritableTyVar must be role-aware] in GHC.Tc.Utils.TcType.

* There is one exception to the claim that non-rewritable parts of the tree do
  not affect the solver: we sometimes do an occurs-check to decide e.g. how to
  orient an equality. (See the comments on GHC.Tc.Solver.Equality.canEqTyVarFunEq.)
  Accordingly, the presence of a variable in a kind or coercion just might
  influence the solver. Here is an example:

    type family Const x y where
      Const x y = x

    AxConst :: forall x y. Const x y ~# x

    alpha :: Const Type Nat
    [W] alpha ~ Int |> (sym (AxConst Type alpha) ;;
                        AxConst Type alpha ;;
                        sym (AxConst Type Nat))

  The cast is clearly ludicrous (it ties together a cast and its symmetric
  version), but we can't quite rule it out. (See (EQ1) from Note [Respecting
  definitional equality] in GHC.Core.TyCo.Rep to see why we need the Const Type
  Nat bit.) And yet this cast will (quite rightly) prevent alpha from unifying
  with the RHS. I (Richard E) don't have an example of where this problem can
  arise from a Haskell program, but we don't have an air-tight argument for why
  the definition of *rewritable* given here is correct.

Note [Extending the inert equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Main Theorem [Stability under extension]
   GIVEN a "work item" [lhs_w -fw-> rhs_w]
         and a terminating generalised substitution S,

   SUCH THAT
      (T1) S(fw,lhs_w) = lhs_w   -- LHS of work-item is a fixpoint of S(fw,_)
      (T2) S(fw,rhs_w) = rhs_w   -- RHS of work-item is a fixpoint of S(fw,_)
      (T3) lhs_w not in rhs_w    -- No occurs check in the work item
             -- If lhs is a type family application, we require only that
             -- lhs is not *rewritable* in rhs_w. See Note [Rewritable] and
             -- Note [EqCt occurs check] in GHC.Tc.Types.Constraint.
      (T4) no [lhs_s -fs-> rhs_s] in S meets [The KickOut Criteria]
           (i.e. we already kicked any such items out!)

   THEN the extended substitution T = S+(lhs_w -fw-> rhs_w)
        is a terminating generalised substitution

How do we establish these conditions?

  * (T1) and (T2) are guaranteed by exhaustively rewriting the work-item
    with S(fw,_).

  * (T3) is guaranteed by an occurs-check on the work item.
    This is done during canonicalisation, in checkTypeEq; invariant
    (TyEq:OC) of CEqCan. See also Note [EqCt occurs check] in GHC.Tc.Types.Constraint.

  * (T4) is established by GHC.Tc.Solver.Monad.kickOutRewritable.  If the inert
    set contains a triple that meets the KickOut Criteria, we kick it out and
    add it to the work list for later re-examination.  See
    Note [The KickOut Criteria]

Theorem: T (defined in "THEN" above) is a generalised substitution;
  that is, it satisfies (WF1)-(WF3)
Proof:
  (WF1) Suppose we are adding [lhs_w -fw-> rhs_w], and [lhs_s -fs-> rhs_s] is in S.
        Then:
          - by (T1)  if fs>=fw, lhs_s does not occur within lhs_w.
          - by (KK1) if fw>=fs, lhs_w is not rewritable in lhs_s, or we'd have
            kicked out the stable constraint.

  (WF2) is directly guaranteed by (T3)

  (WF3) No lhs_s in S is rewritable in rhs_w at all, because of (T2)
        And (KK2) guarantees that lhs_w is not rewritable under a type
        family in rhs_s

Note [The KickOut Criteria]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Kicking out is a Bad Thing:
* It means we have to re-process a constraint.  The less we kick out, the better.
* In the limit, kicking can lead to non-termination: imagine that we /always/
  kick out the entire inert set!
* Because (mid 2024) we don't support sharing in constraints, excessive kicking out
  can lead to exponentially big constraints (#24984).

So we seek to do as little kicking out as possible.  For example, consider this,
which happens a lot:

   Inert:  g1: a ~ Maybe b
   Work:   g2: b ~ Int

We do /not/ kick out g1 when adding g2.  The new substitution S' = {g1,g2} is still
/terminating/ but it is not /idmempotent/.  To apply S' to, say, (Tree a), we may
need to apply it twice:  Tree a --> Tree (Maybe b) --> Tree (Maybe Int)

Here are the KickOut Criteria:

    When adding [lhs_w -fw-> rhs_w] to a well-formed terminating substitution S,
    element [lhs_s -fs-> rhs_s] in S meets the KickOut Criteria if:

    (KK0) fw >= fs    AND   any of (KK1), (KK2) or (KK3) hold

    * (KK1: satisfy WF1) `lhs_w` is rewritable in `lhs_s`.

    * (KK2: termination) `lhs_w` is rewritable in `rhs_s` in these positions:
        If not(fs>=fw)
        then (KK2a) anywhere
        else (KK2b) look only in the argument of type family applications,
                    whose type family heads some LHS in `S`

    * (KK3: completeness)
      If not(fs >= fw)   -- If fs can rewrite fw, kick-out is redundant/harmful
      * (KK3a) If the role of `fs` is Nominal:
           kick out if `rhs_s = lhs_w`
      * (KK3b) If the role of `fs` is Representational:
           kick out if `rhs_s` is of form `(lhs_w t1 .. tn)`

Rationale

* (KK0) kick out only if `fw` can rewrite `fs`.
  Reason: suppose we kick out (lhs1 -fs-> s), and add (lhs -fw-> t) to the ineart
  set. The latter can't rewrite the former, so the kick-out achieved nothing

* (KK1) `lhs_w` is rewritable in `lhs_s`.
  Reason: needed to guarantee (WF1).  See Theorem: T is well formed

* (KK2) see Note [KK2: termination of the extended substitution]

* (KK3) see Note [KK3: completeness of solving]

The above story is a bit vague wrt roles, but the code is not.
See Note [Flavours with roles]

Note [KK2: termination of the extended substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Proving termination of the extended substitution T is surprisingly tricky.

* Reason for (KK2a).  Consider
      Work:  [G] b ~ a
      Inert: [W] a ~ b
  If we don't kick out the inert, then we get a loop on e.g. [W] a ~ Int.
  But if both were Wanted we really should not kick out (the substitution does not
  have to be idempotent). So we only look everywhere for the `lhs_w` if
  not (fs>=fw), that is the inert item cannot rewrite the work item.  So in the
  above example we will kick out; but if both were Wanted we won't.

* Reason for (KK2b).  Consider the case where (fs >= fw)
      Work:  [G] a ~ Int
      Inert: [G] F Int ~ F a
  If we just added the work item, the substitution would loop on type (F Int).
  So we must kick out the inert item, even though (fs>=fw). (KK2b) does this
  by looking for lhs_w under type family applications in rhs_s.

  (KK2b) makes kick-out less aggressive by looking only under type-family applications,
  in the case where (fs >= fw), and that made a /huge/ difference to #24944.

Tricky examples in: #19042, #17672, #24984. The last (#24984) is particular subtle:

  Inert:   [W] g1: F a0 ~ F a1
           [W] g2: F a2 ~ F a1
           [W] g3: F a3 ~ F a1

Now we add [W] g4: F a1 ~ F a7.  Should we kick out g1,g2,g3?  No!  The
substitution doesn't need to be idempotent, merely terminating.  And in #24984
it turned out that we kept adding one new constraint and kicking out all the
previous inert ones (and that rewriting led to exponentially big constraints due
to lack of contraint sharing.)  So we only want to look /under/ type family applications.

The proof is hard. We start by ignoring flavours. Suppose that:
* We are adding [lhs_w -fw-> rhs_w] to a well-formed, terminating substitution S.
* None of the constraints in S meet the KickOut Criteria.
* Define T = S+[lhs_w -fw-> rhs_w]
* `f` is an arbitrary flavour

Lemma 1: for any lhs_s in S, T*(f,lhs_s) terminates.
  Proof.
  * We know that r1 = S*(f,lhs_s) terminates.
  * Moreover, we know there are no occurrences of lhs_w under a type family (which
    is the head of a LHS) in r1 (KK2)+(WF3).  We need (WF3) because you might wonder
    what if rhs_s is (F a), and [a --> lhs_w] was in S.  But (WF3) prevents that.
  * Define r2 = r1{rhs_w/lhs_w}.  We know that rhs_w has no occurrences of any lhs in S,
    nor of lhs_w.
  * Since any occurrence of lhs_w does not occur under a type family, the substitution
    won't make any F t1..tn ~ s in S match.
  * So r2 is a fixed point of T.

Lemma 2: T*(f,lhs_w) teminates.
  Proof: no occurrences of any LHS in rhs_w.

Theorem. For any type r, T*(r) terminates.
  Proof:
  1. Consider any sub-term of r, which is a LHS of T.
     - Rewrite it with T*; this terminates (Lemma 1).
     - Do this simultaneously to all sub-terms that match a LHS of T, yielding r1.
  2. Could this new r1 have a sub-term that is an LHS of T?  Yes, but only if r has a
     sub-term F w, and w rewrote in Step 1 to w' and F w' matches a LHS in T.
  3. Very well: apply step 1 again, but note that /doing so consumes one of the family
     applications in the original r/.
  4. After Step 1 either we have reached a fixed point, or we repeat Step 1 consuming at
     least one family application of r.
  5. There are only a finite number of family applications in r, so this process terminates.

Example:

Inert set:   gs : F Int ~ b
Work item:   gw : b ~ Int

F (F (F b)) --[gw]--> F (F (F Int)) --[gs]--> F (F b)
            --[gw]--> F (F Int)     --[gs]--> F b
            --[gw]--> F Int         --[gs]--> b
            --[gw]--> Int

Notice that each iteration of Step 1 strips off one of the layers of F, all
of which were in the original r.

The argument is even more tricky when flavours are involved, and we have not
fleshed it out in detail.

Note [KK3: completeness of solving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(KK3) is not necessary for the extended substitution
to be terminating.  In fact (KK0) could be made stronger by saying
   ... then (not (fw >= fs) or not (fs >= fs))
But it's not enough for S to be /terminating/; we also want /completeness/.
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
So we add one more clause (KK3) to the kick-out criteria:

    * (KK3: completeness)
      If not(fs >= fw)   (KK3a)
      * (KK3b) If the role of `fs` is Nominal:
           kick out if `rhs_s = lhs_w`
      * (KK3c) If the role of `fs` is Representational:
           kick out if `rhs_s` is of form `(lhs_w t1 .. tn)`

Wrinkles:

* (KK3a) All this can only happen if the work-item can rewrite the inert
  one, /but not vice versa/; that is not(fs >= fw).  It is useless to kick
  out if (fs >= fw) becuase then the work-item is already fully rewritten
  by the inert item.  And too much kick-out is positively harmful.
  (Historical example #14363.)

* (KK3b) addresses teh main example above for KK3. Another way to understand
  (KK3b) is that we treat an inert item
        a -f-> b
  in the same way as
        b -f-> a
  So if we kick out one, we should kick out the other.  The orientation
  is somewhat accidental.

* (KK3c) When considering roles, we also need the second clause (KK3b). Consider
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

  For similar reasons, if we only had (KK3a), we wouldn't kick the
  representational inert out. And then, we'd miss solving the inert, which now
  reduced to reflexivity.

  The solution here is to kick out representational inerts whenever the lhs of a
  work item is "exposed", where exposed means being at the head of the top-level
  application chain (lhs t1 .. tn).  See head_is_new_lhs. This is encoded in
  (KK3c)).


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

-- | Filter InertEqs according to a predicate
filterInertEqs :: (EqCt -> Bool) -> InertEqs -> InertEqs
filterInertEqs f = mapMaybeDVarEnv g
  where
    g xs =
      let filtered = filter f xs
      in
        if null filtered
        then Nothing
        else Just filtered

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

-- | Filter entries in InertFunEqs that satisfy the predicate
filterFunEqs :: (EqCt -> Bool) -> InertFunEqs -> InertFunEqs
filterFunEqs f = mapMaybeTcAppMap g
  where
    g xs =
      let filtered = filter f xs
      in
        if null filtered
        then Nothing
        else Just filtered

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
      | irredCtPred irred `tcEqType` pred = Left (irred, NotSwapped)
      | otherwise                         = Right irred

    match_eq eq_rel1 lty1 rty1 irred
      | EqPred eq_rel2 lty2 rty2 <- classifyPredType (irredCtPred irred)
      , eq_rel1 == eq_rel2
      , Just swap <- match_eq_help lty1 rty1 lty2 rty2
      = Left (irred, swap)
      | otherwise
      = Right irred

    match_eq_help lty1 rty1 lty2 rty2
      | lty1 `tcEqType` lty2, rty1 `tcEqType` rty2
      = Just NotSwapped
      | lty1 `tcEqType` rty2, rty1 `tcEqType` lty2
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
--
-- Precondition: Ct is either CEqCan or CIrredCan
updGivenEqs tclvl ct inerts
  | not (isGivenCt ct) = inerts

  -- See Note [Let-bound skolems]
  | isLetBoundSkolemCt tclvl ct = inerts { inert_given_eqs = True }

  -- At this point we are left with a constraint that either
  -- is an equality (F a ~ ty), or /might/ be, like (c a)
  | otherwise = inerts { inert_given_eq_lvl = tclvl
                       , inert_given_eqs    = True }

isLetBoundSkolemCt :: TcLevel -> Ct -> Bool
-- See Note [Let-bound skolems]
isLetBoundSkolemCt tclvl (CEqCan (EqCt { eq_lhs = lhs, eq_rhs = rhs }))
  = case lhs of
      TyVarLHS tv -> not (isOuterTyVar tclvl tv)
      TyFamLHS {} -> case getTyVar_maybe rhs of
                       Just tv -> not (isOuterTyVar tclvl tv)
                       Nothing -> False
isLetBoundSkolemCt _ _ = False

data KickOutSpec -- See Note [KickOutSpec]
  = KOAfterUnify  TcTyVarSet   -- We have unified these tyvars
  | KOAfterAdding CanEqLHS     -- We are adding to the inert set a canonical equality
                               -- constraint with this LHS

instance Outputable KickOutSpec where
  ppr (KOAfterUnify tvs)  = text "KOAfterUnify" <> ppr tvs
  ppr (KOAfterAdding lhs) = text "KOAfterAdding" <> parens (ppr lhs)

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

data WhereToLook = LookEverywhere | LookOnlyUnderFamApps
                   deriving( Eq )

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
    (dicts_out,  dicts_in)  = partitionDicts    kick_out_dict dictmap
    (irs_out,    irs_in)    = partitionBag      kick_out_irred irreds
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
      , fr_can_rewrite_ty LookEverywhere NomEq (ctEvPred (qci_ev qci))
      = Left (mkNonCanonical ev)
      | otherwise
      = Right qci

    fr_tv_can_rewrite_ty :: WhereToLook -> (TyVar -> Bool) -> EqRel -> Type -> Bool
    fr_tv_can_rewrite_ty where_to_look check_tv role ty
      = anyRewritableTyVar role can_rewrite ty
      where
        can_rewrite :: UnderFam -> EqRel -> TyVar -> Bool
        can_rewrite is_under_famapp old_role tv
           = (where_to_look == LookEverywhere || is_under_famapp) &&
             new_role `eqCanRewrite` old_role && check_tv tv

    fr_tf_can_rewrite_ty :: WhereToLook -> TyCon -> [TcType] -> EqRel -> Type -> Bool
    fr_tf_can_rewrite_ty where_to_look new_tf new_tf_args role ty
      = anyRewritableTyFamApp role can_rewrite ty
      where
        can_rewrite :: UnderFam -> EqRel -> TyCon -> [TcType] -> Bool
        can_rewrite is_under_famapp old_role old_tf old_tf_args
          = (where_to_look == LookEverywhere || is_under_famapp) &&
            new_role `eqCanRewrite` old_role &&
            tcEqTyConApps new_tf new_tf_args old_tf old_tf_args
              -- it's possible for old_tf_args to have too many. This is fine;
              -- we'll only check what we need to.


    fr_can_rewrite_ty :: WhereToLook -> EqRel -> Type -> Bool
    -- UnderFam = True <=> look only under type-family applications
    fr_can_rewrite_ty uf = case ko_spec of  -- See Note [KickOutSpec]
      KOAfterUnify tvs                    -> fr_tv_can_rewrite_ty uf (`elemVarSet` tvs)
      KOAfterAdding (TyVarLHS tv)         -> fr_tv_can_rewrite_ty uf (== tv)
      KOAfterAdding (TyFamLHS tf tf_args) -> fr_tf_can_rewrite_ty uf tf tf_args

    fr_may_rewrite :: CtFlavourRole -> Bool
    fr_may_rewrite fs = new_fr `eqCanRewriteFR` fs
        -- Can the new item rewrite the inert item?

    kick_out_dict :: DictCt -> Bool
    -- Kick it out if the new CEqCan can rewrite the inert one
    -- See Note [kickOutRewritable]
    kick_out_dict (DictCt { di_tys = tys, di_ev = ev })
      =  fr_may_rewrite (ctEvFlavour ev, NomEq)
      && any (fr_can_rewrite_ty LookEverywhere NomEq) tys

    kick_out_irred :: IrredCt -> Bool
    kick_out_irred (IrredCt { ir_ev = ev })
      =  fr_may_rewrite (ctEvFlavour ev, eq_rel)
      && fr_can_rewrite_ty LookEverywhere eq_rel pred
      where
       pred   = ctEvPred ev
       eq_rel = predTypeEqRel pred

    -- Implements criteria K1-K3 in Note [Extending the inert equalities]
    kick_out_eq :: EqCt -> Bool
    kick_out_eq (EqCt { eq_lhs = lhs, eq_rhs = rhs_ty
                      , eq_ev = ev, eq_eq_rel = eq_rel })

      -- (KK0) Keep it in the inert set if the new thing can't rewrite it
      | not (fr_may_rewrite fs)
      = False

      -- Below here (fr_may_rewrite fs) is True

      -- (KK1)
      | fr_can_rewrite_ty LookEverywhere eq_rel (canEqLHSType lhs)
      = True   -- (KK1)
         -- The above check redundantly checks the role & flavour,
         -- but it's very convenient

      -- (KK2)
      | let where_to_look | fs_can_rewrite_fr = LookOnlyUnderFamApps
                          | otherwise         = LookEverywhere
      , fr_can_rewrite_ty where_to_look eq_rel rhs_ty
      = True

      -- (KK3)
      | not fs_can_rewrite_fr                    -- (KK3a)
      , case eq_rel of
              NomEq  -> is_new_lhs      rhs_ty   -- (KK3b)
              ReprEq -> head_is_new_lhs rhs_ty   -- (KK3c)
      = True

      | otherwise = False

      where
        fs_can_rewrite_fr = fs `eqCanRewriteFR` new_fr
        fs = (ctEvFlavour ev, eq_rel)

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
-- True <=> there is no Given looking like (N tys1 ~ N tys2)
-- See Note [Decomposing newtype equalities] (EX3) in GHC.Tc.Solver.Equality
noGivenNewtypeReprEqs tc (IS { inert_cans = inerts })
  | IC { inert_irreds = irreds, inert_insts = quant_cts } <- inerts
  = not (anyBag might_help_irred irreds || any might_help_qc quant_cts)
    -- Look in both inert_irreds /and/ inert_insts (#26020)
  where
    might_help_irred (IrredCt { ir_ev = ev })
      | EqPred ReprEq t1 t2 <- classifyPredType (ctEvPred ev)
      = headed_by_tc t1 t2
      | otherwise
      = False

    might_help_qc (QCI { qci_body = pred })
      | ClassPred cls [_, t1, t2] <- classifyPredType pred
      , cls `hasKey` coercibleTyConKey
      = headed_by_tc t1 t2
      | otherwise
      = False

    headed_by_tc t1 t2
      | Just (tc1,_) <- tcSplitTyConApp_maybe t1
      , tc == tc1
      , Just (tc2,_) <- tcSplitTyConApp_maybe t2
      , tc == tc2
      = True
      | otherwise
      = False

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


{- *********************************************************************
*                                                                      *
    Cycle breakers
*                                                                      *
********************************************************************* -}

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
  | CtWanted {} <- ev_w
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

  | CtWanted {} <- ev_i
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

{- *********************************************************************
*                                                                      *
               Extracting Givens from the inert set
*                                                                      *
********************************************************************* -}


-- | Extract only Given constraints from the inert set.
inertGivens :: InertSet -> InertSet
inertGivens is@(IS { inert_cans = cans }) =
  is { inert_cans = givens_cans
     , inert_solved_dicts = emptyDictMap
     }
  where

    isGivenEq :: EqCt -> Bool
    isGivenEq eq = isGiven (ctEvidence (CEqCan eq))
    isGivenDict :: DictCt -> Bool
    isGivenDict dict = isGiven (ctEvidence (CDictCan dict))
    isGivenIrred :: IrredCt -> Bool
    isGivenIrred irred = isGiven (ctEvidence (CIrredCan irred))

    -- Filter the inert constraints for Givens
    (eq_givens_list, _) = partitionInertEqs isGivenEq (inert_eqs cans)
    (funeq_givens_list, _) = partitionFunEqs isGivenEq (inert_funeqs cans)
    dict_givens = filterDicts isGivenDict (inert_dicts cans)
    safehask_givens = filterDicts isGivenDict (inert_safehask cans)
    irreds_givens = filterBag isGivenIrred (inert_irreds cans)

    eq_givens = foldr addInertEqs emptyTyEqs eq_givens_list
    funeq_givens = foldr addFunEqs emptyFunEqs funeq_givens_list

    givens_cans =
      cans
        { inert_eqs      = eq_givens
        , inert_funeqs   = funeq_givens
        , inert_dicts    = dict_givens
        , inert_safehask = safehask_givens
        , inert_irreds   = irreds_givens
        }
