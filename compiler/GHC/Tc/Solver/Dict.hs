{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

-- | Solving Class constraints CDictCan
module GHC.Tc.Solver.Dict (
  solveDict, solveDictNC, solveCallStack,
  matchLocalInst, chooseInstance,
  makeSuperClasses, mkStrictSuperClasses
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Tc.Solver.Solve( solveSimpleWanteds )

import GHC.Tc.Errors.Types
import GHC.Tc.Instance.Class( matchEqualityInst )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc
import GHC.Tc.Types.Origin
import GHC.Tc.Solver.FunDeps( tryDictFunDeps )
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad
import GHC.Tc.Solver.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Unify( uType, mightEqualLater )

import GHC.Hs.Type( HsIPName(..) )

import GHC.Core
import GHC.Core.Make
import GHC.Core.Type
import GHC.Core.Class
import GHC.Core.Predicate
import GHC.Core.InstEnv( DFunInstType )
import GHC.Core.Multiplicity ( scaledThing )
import GHC.Core.Unify ( ruleMatchTyKiX )

import GHC.Types.TyThing( lookupDataCon, lookupId )
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Var
import GHC.Types.Id( mkTemplateLocals )
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.SrcLoc

import GHC.Builtin.Names( srcLocDataConName, pushCallStackName, emptyCallStackName )

import GHC.Utils.Monad ( concatMapM )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Unit.Module

import GHC.Data.Bag
import GHC.Data.FastString

import GHC.Driver.DynFlags

import qualified GHC.LanguageExtensions as LangExt

import Data.Maybe ( listToMaybe, mapMaybe, isJust )
import Data.Void( Void )

import Control.Monad

{- *********************************************************************
*                                                                      *
*                      Class Canonicalization
*                                                                      *
********************************************************************* -}

solveDictNC :: CtEvidence -> Class -> [Type] -> SolverStage Void
-- NC: this comes from CNonCanonical or CIrredCan
-- Precondition: already rewritten by inert set
solveDictNC ev cls tys
  = do { simpleStage $ traceTcS "solveDictNC" (ppr (mkClassPred cls tys) $$ ppr ev)
       ; dict_ct <- canDictCt ev cls tys
       ; solveDict dict_ct }

solveDict :: DictCt -> SolverStage Void
-- Preconditions: `tys` are already rewritten by the inert set
solveDict dict_ct@(DictCt { di_ev = ev, di_cls = cls, di_tys = tys })
  | isEqualityClass cls
  = solveEqualityDict ev cls tys

  | otherwise
  = assertPpr (ctEvRewriteRole ev == Nominal) (ppr ev $$ ppr cls $$ ppr tys) $
    do { simpleStage $ traceTcS "solveDict" (ppr dict_ct)

       -- Look in the inert dictionaries
       ; tryInertDicts dict_ct

       -- Try top-level instances
       ; tryInstances dict_ct

       -- Try fundeps /after/ tryInstances:
       --     see (DFL2) in Note [Do fundeps last]
       ; tryDictFunDeps dict_ct

       ; simpleStage (updInertDicts dict_ct)
       ; stopWithStage (dictCtEvidence dict_ct) "Kept inert DictCt" }

canDictCt :: CtEvidence -> Class -> [Type] -> SolverStage DictCt
-- Once-only processing of Dict constraints:
--   * expand superclasses
--   * deal with CallStack
canDictCt ev cls tys
  | isGiven ev  -- See Note [Eagerly expand given superclasses]
  = Stage $
    do { dflags <- getDynFlags
       ; sc_cts <- mkStrictSuperClasses (givensFuel dflags) ev [] [] cls tys
         -- givensFuel dflags: See Note [Expanding Recursive Superclasses and ExpansionFuel]
       ; emitWork (listToBag sc_cts)

       ; continueWith (DictCt { di_ev = ev, di_cls = cls
                              , di_tys = tys, di_pend_sc = doNotExpand }) }
         -- doNotExpand: We have already expanded superclasses for /this/ dict
         -- so set the fuel to doNotExpand to avoid repeating expansion

  | CtWanted (WantedCt { ctev_rewriters = rws }) <- ev
  , Just ip_name <- isCallStackPred cls tys
  , Just fun_fs  <- isPushCallStackOrigin_maybe orig
  -- If we're given a CallStack constraint that arose from a function
  -- call, we need to push the current call-site onto the stack instead
  -- of solving it directly from a given.
  -- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
  -- and Note [Solving CallStack constraints]
  = Stage $
    do { -- First we emit a new constraint that will capture the
         -- given CallStack.

         let new_loc = setCtLocOrigin loc (IPOccOrigin (HsIPName $ fastStringToText ip_name))
                            -- We change the origin to IPOccOrigin so
                            -- this rule does not fire again.
                            -- See Note [Overview of implicit CallStacks]
                            -- in GHC.Tc.Types.Evidence

       ; new_ev <- CtWanted <$> newWantedEvVarNC new_loc rws pred

         -- Then we solve the wanted by pushing the call-site
         -- onto the newly emitted CallStack
       ; let ev_cs = EvCsPushCall fun_fs (ctLocSpan loc) (ctEvExpr new_ev)
       ; solveCallStack ev ev_cs

       ; continueWith (DictCt { di_ev = new_ev, di_cls = cls
                              , di_tys = tys, di_pend_sc = doNotExpand }) }
         -- doNotExpand: No superclasses for class CallStack
         -- See invariants in CDictCan.cc_pend_sc

  | otherwise
  = Stage $
    do { dflags <- getDynFlags
       ; let fuel | classHasSCs cls = wantedsFuel dflags
                  | otherwise       = doNotExpand
                  -- See Invariants in `CCDictCan.cc_pend_sc`
       ; continueWith (DictCt { di_ev = ev, di_cls = cls
                              , di_tys = tys, di_pend_sc = fuel }) }

  where
    loc  = ctEvLoc ev
    orig = ctLocOrigin loc
    pred = ctEvPred ev

{- *********************************************************************
*                                                                      *
*           Implicit parameters and call stacks
*                                                                      *
********************************************************************* -}

solveCallStack :: CtEvidence -> EvCallStack -> TcS ()
-- Also called from GHC.Tc.Solver when defaulting call stacks
solveCallStack ev ev_cs
  -- We're given ev_cs :: CallStack, but the evidence term should be a
  -- dictionary, so we have to coerce ev_cs to a dictionary for
  -- `IP ip CallStack`. See Note [Overview of implicit CallStacks]
  = do { inner_stk <- evCallStack pred ev_cs
       ; let ev_tm = EvExpr (evWrapIPE pred inner_stk)
       ; setDictIfWanted ev EvCanonical ev_tm }
         -- EvCanonical: see Note [CallStack and ExceptionContext hack]
  where
    pred = ctEvPred ev

-- Dictionary for CallStack implicit parameters
evCallStack :: TcPredType -> EvCallStack -> TcS EvExpr
-- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
evCallStack _ EvCsEmpty
  = Var <$> lookupId emptyCallStackName
evCallStack pred (EvCsPushCall fs loc tm)
  = do { df <- getDynFlags
       ; m  <- getModule
       ; srcLocDataCon <- lookupDataCon srcLocDataConName
       ; let platform = targetPlatform df
             mkSrcLoc l = mkCoreConWrapApps srcLocDataCon <$>
                          sequence [ mkStringExprFS (unitFS $ moduleUnit m)
                                   , mkStringExprFS (moduleNameFS $ moduleName m)
                                   , mkStringExprFS (srcSpanFile l)
                                   , return $ mkIntExprInt platform (srcSpanStartLine l)
                                   , return $ mkIntExprInt platform (srcSpanStartCol l)
                                   , return $ mkIntExprInt platform (srcSpanEndLine l)
                                   , return $ mkIntExprInt platform (srcSpanEndCol l)
                                   ]

       ; push_cs_id <- lookupId pushCallStackName
       ; name_expr  <- mkStringExprFS fs
       ; loc_expr   <- mkSrcLoc loc
               -- At this point tm :: IP sym CallStack
               -- but we need the actual CallStack to pass to pushCS,
               -- so we use evUwrapIP to strip the dictionary wrapper
               -- See Note [Overview of implicit CallStacks]
       ; let outer_stk = evUnwrapIPE pred tm
       ; return (mkCoreApps (Var push_cs_id)
                    [mkCoreTup [name_expr, loc_expr], outer_stk]) }

{- Note [Solving CallStack constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [Overview of implicit CallStacks] in GHc.Tc.Types.Evidence.

Suppose f :: HasCallStack => blah.  Then

* Each call to 'f' gives rise to
    [W] s1 :: IP "callStack" CallStack    -- CtOrigin = OccurrenceOf f
  with a CtOrigin that says "OccurrenceOf f".
  Remember that HasCallStack is just shorthand for
    IP "callStack" CallStack
  See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence

* We canonicalise such constraints, in GHC.Tc.Solver.Dict.canDictNC, by
  pushing the call-site info on the stack, and changing the CtOrigin
  to record that has been done.
   Bind:  s1 = pushCallStack <site-info> s2
   [W] s2 :: IP "callStack" CallStack   -- CtOrigin = IPOccOrigin

* Then, and only then, we can solve the constraint from an enclosing
  Given.

So we must be careful /not/ to solve 's1' from the Givens.  We guarantee
this by canonicalising before looking up in the inert set.

Note [CallStack and ExceptionContext hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It isn't really right that we treat CallStack and ExceptionContext dictionaries
as canonical, in the sense of Note [Coherence and specialisation: overview].
They definitely are not!

But if we use EvNonCanonical here we get lots of
    nospec (error @Int) dict  string
(since `error` takes a HasCallStack dict), and that isn't bottoming  (at least not
without extra work)  So, hackily, we just say that HasCallStack and ExceptionContext
are canonical, even though they aren't really.

Note [Shadowing of implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we add a new /given/ implicit parameter to the inert set, it /replaces/
any existing givens for the same implicit parameter.  This makes a difference
in two places:

* In `GHC.Tc.Solver.InertSet.solveOneFromTheOther`, be careful when we have
   (?x :: ty) in the inert set and an identical (?x :: ty) as the work item.

* In `updInertDicts`, in this module, when adding [G] (?x :: ty), remove any
  existing [G] (?x :: ty'), regardless of ty'.

There are wrinkles:

* Wrinkle (SIP1): we must be careful of superclasses (#14218).  Consider
     f,g :: (?x::Int, C a) => a -> a
     f v = let ?x = 4 in g v

  The call to 'g' gives rise to a Wanted constraint (?x::Int, C a).
  We must /not/ solve this from the Given (?x::Int, C a), because of
  the intervening binding for (?x::Int).  #14218.

  We deal with this by arranging that when we add [G] (?x::ty) we /delete/
  * from the inert_cans, and
  * from the inert_solved_dicts
  any existing [G] (?x::ty) /and/ any [G] D tys, where (D tys) has a superclass
  with (?x::ty).  See Note [Local implicit parameters] in GHC.Core.Predicate.

  An very important special case is constraint tuples like [G] (% ?x::ty, Eq a %).

  But it could also happen for `class xx => D xx where ...` and the constraint
  D (?x :: int); again see Note [Local implicit parameters].  This corner
  (constraint-kinded variables instantiated with implicit parameter constraints)
  is not well explored.

  You might worry about whether deleting an /entire/ constraint just because
  a distant superclass has an implicit parameter might make another Wanted for
  that constraint un-solvable.  Indeed so. But for constraint tuples it doesn't
  matter -- their entire payload is their superclasses.  And the other case is
  the ill-explored corner above.

  The code that accounts for (SIP1) is in updInertDicts; in particular the call to
  GHC.Core.Predicate.mentionsIP.

* Wrinkle (SIP2): we must apply this update semantics for `inert_solved_dicts`
  as well as `inert_cans` (#23761).

  You might think that wouldn't be necessary, because an element of
  `inert_solved_dicts` is never an implicit parameter (see
  Note [Solved dictionaries] in GHC.Tc.Solver.InertSet).
  While that is true, dictionaries in `inert_solved_dicts` may still have
  implicit parameters as a /superclass/! For example:

    class c => C c where ...
    f :: C (?x::Int) => blah

  Now (C (?x::Int)) has a superclass (?x::Int). This may look exotic, but it
  happens particularly for constraint tuples, like `(% ?x::Int, Eq a %)`.

* Wrinkle (SIP3)
  - Note that for the inert dictionaries, `inert_cans`, we must /only/ delete
    existing /Givens/!  Deleting an existing Wanted led to #26451; we just never
    solved it!

  - In contrast, the solved dictionaries, `inert_solved_dicts`, are really like
    Givens; they may be "inherited" from outer scopes, so we must delete any
    solved dictionaries for this implicit parameter for /both/ Givens /and/
    Wanteds.

    Otherwise the new Given doesn't properly shadow those inherited solved
    dictionaries. Test T23761 showed this up.

Example 1:

Suppose we have (typecheck/should_compile/ImplicitParamFDs)
  flub :: (?x :: Int) => (Int, Integer)
  flub = (?x, let ?x = 5 in ?x)
When we are checking the last ?x occurrence, we guess its type to be a fresh
unification variable alpha and emit an (IP "x" alpha) constraint. But the given
(?x :: Int) has been translated to an IP "x" Int constraint, which has a
functional dependency from the name to the type. So if that (?x::Int) is still
in the inert set, we'd get a fundep interaction that tells us that alpha ~ Int,
and we get a type error. This is bad.  The "replacement" semantics stops this
happening.

Example 2:

f :: (?x :: Char) => Char
f = let ?x = 'a' in ?x

The "let ?x = ..." generates an implication constraint of the form:

?x :: Char => ?x :: Char

Furthermore, the signature for `f` also generates an implication
constraint, so we end up with the following nested implication:

?x :: Char => (?x :: Char => ?x :: Char)

Note that the wanted (?x :: Char) constraint may be solved in two incompatible
ways: either by using the parameter from the signature, or by using the local
definition.  Our intention is that the local definition should "shadow" the
parameter of the signature.  The "replacement" semantics for implicit parameters
does this.

Example 3:

Similarly, consider
   f :: (?x::a) => Bool -> a

   g v = let ?x::Int = 3
         in (f v, let ?x::Bool = True in f v)

This should probably be well typed, with
   g :: Bool -> (Int, Bool)

So the inner binding for ?x::Bool *overrides* the outer one.

See ticket #17104 for a rather tricky example of this overriding
behaviour.

All this works for the normal cases but it has an odd side effect in
some pathological programs like this:

    -- This is accepted, the second parameter shadows
    f1 :: (?x :: Int, ?x :: Char) => Char
    f1 = ?x

    -- This is rejected, the second parameter shadows
    f2 :: (?x :: Int, ?x :: Char) => Int
    f2 = ?x

Both of these are actually wrong:  when we try to use either one,
we'll get two incompatible wanted constraints (?x :: Int, ?x :: Char),
which would lead to an error.

I can think of two ways to fix this:

  1. Simply disallow multiple constraints for the same implicit
    parameter---this is never useful, and it can be detected completely
    syntactically.

  2. Move the shadowing machinery to the location where we nest
     implications, and add some code here that will produce an
     error if we get multiple givens for the same implicit parameter.
-}


{- ******************************************************************************
*                                                                               *
                   solveEqualityDict
*                                                                               *
****************************************************************************** -}

{- Note [Solving equality classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (~), which behaves as if it was defined like this:
  class a ~# b => a ~ b
  instance a ~# b => a ~ b
There are two more similar "equality classes" like this.  The full list is
  * (~)         eqTyCon
  * (~~)        heqTyCon
  * Coercible   coercibleTyCon
(See Note [The equality types story] in GHC.Builtin.Types.Prim.)

(EQC1) For a Given (boxed) equality like (t1 ~ t2), we /replace/ the constraint
  with its superclass (which, remember, is equally powerful) rather than /adding/
  it.  Thus, we turn  [G] d : t1 ~ t2  into
         [G] g : t1 ~# t2
         g := sc_sel d       -- Extend the evidence bindings

  We achieve this by
   (a) not expanding superclasses for equality classes at all;
       see the `isEqualityClass` test in `mk_strict_superclasses`
   (b) special logic to solve (t1 ~ t2) in the Given case of `solveEqualityDict`.

  Using replacement rather than adding can make a huge difference.
  Consider T17836, which has a constraint like
      forall b,c. a ~ (b,c) =>
        forall d,e. c ~ (d,e) =>
          ...etc...
  If we just /add/ the superclasses of [G] g1:a ~ (b,c), we'll put
  [G] g1:(a~(b,c)) in the inert set and emit [G] g2:a ~# (b,c).  That will
  kick out g1, and it'll be re-inserted as [G] g1':(b,c)~(b,c) which does
  no good to anyone.  When the implication is deeply nested, this has
  quadratic cost, and no benefit.  Just replace!

  (This can have a /big/ effect: test T17836 involves deeply-nested GADT
  pattern matching. Its compile-time allocation decreased by 40% when
  I added the "replace" rather than "add" semantics.)

(EQC2) Faced with [W] t1 ~ t2, it's always OK to reduce it to [W] t1 ~# t2,
  without worrying about Note [Instance and Given overlap].  Why?  Because
  if we had [G] s1 ~ s2, then we'd get the superclass [G] s1 ~# s2, and
  so the reduction of the [W] constraint does not risk losing any solutions.

  On the other hand, it can be fatal to /fail/ to reduce such equalities
  on the grounds of Note [Instance and Given overlap], because many good
  things flow from [W] t1 ~# t2.

Conclusion: we have a special solver pipeline for equality-class constraints,
`solveEqualityDict`.  It aggressively decomposes the boxed equality constraint
into an unboxed coercion, both for Givens and Wanteds, and /replaces/ the
boxed equality constraint with the unboxed one, so that the inert set never
contains the boxed one.

Note [Solving tuple constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I tried treating tuple constraints, such as (% Eq a, Show a %), rather like
equality-class constraints (see Note [Solving equality classes]). That is, by
eagerly decomposing tuple-constraints into their component (Eq a) and (Show a).

But discarding the tuple Given (which "replacing" does) means that we may
have to reconstruct it for a recursive call.  For example
    f :: (% Eq a, Show a %) => blah
    f x = ....(f x')....
If we decomposed eagerly we'd get
    f = \(d : (% Eq a, Show a %)).
         let de = fst d
             ds = snd d
         in ....(f (% de, ds %))...
and the optimiser may not be clever enough to transform (f (% de, ds %)) into
(f d). See #10359 and its test case, and #23398.  (This issue is less pressing for
equality classes because they have to be unpacked strictly, so CSE-ing away
the reconstruction works fine.

So at the moment we don't decompose tuple constraints eagerly; instead we mostly
just treat them like other constraints.
* Given tuples are decomposed via their superclasses, in `canDictCt`.  So
    [G] (% Eq a, Show a %)
  has superclasses
    [G] Eq a,  [G] Show a

* Wanted tuples are decomposed via a built-in "instance". See
  `GHC.Tc.Instance.Class.matchCTuple`

There is a bit of special treatment: search for isCTupleClass.
-}

solveEqualityDict :: CtEvidence -> Class -> [Type] -> SolverStage Void
-- See Note [Solving equality classes]
-- Precondition: (isEqualityClass cls) True, so cls is (~), (~~), or Coercible
solveEqualityDict ev cls tys
  | CtGiven (GivenCt { ctev_evar = ev_id }) <- ev
  , [sel_id] <- classSCSelIds cls  -- Equality classes have just one superclass
  = Stage $
    do { let loc = ctEvLoc ev
             sc_pred = classMethodInstTy sel_id tys
             ev_expr = EvExpr $ Var sel_id `mkTyApps` tys `App` evId ev_id
       -- See (EQC1) in Note [Solving equality classes]
       -- This call to newGivenEv makes the evidence binding for the (unboxed) coercion
       ; given_ev <- newGivenEv loc (sc_pred, ev_expr)
       ; startAgainWith (mkNonCanonical $ CtGiven given_ev) }

  | CtWanted (WantedCt { ctev_dest = dest }) <- ev
  = Stage $
    do { let (role, t1, t2) = matchEqualityInst cls tys
         -- Unify t1~t2, putting anything that can't be solved
         -- immediately into the work list
       ; CPH { cph_co = co } <- wrapUnifierAndEmit ev role $ \uenv ->
                                uType uenv t1 t2
         -- Set  d :: (t1~t2) = Eq# co
       ; setWantedDict dest EvCanonical $
         evDictApp cls tys [Coercion co]
       ; stopWith ev "Solved wanted lifted equality" }

  | otherwise
  = pprPanic "solveEqualityDict" (ppr cls)

{- ******************************************************************************
*                                                                               *
                   interactDict
*                                                                               *
*********************************************************************************

Note [Shortcut solving]
~~~~~~~~~~~~~~~~~~~~~~~
When we interact a [W] constraint with a [G] constraint that solves it, there is
a possibility that we could produce better code if instead we solved from a
top-level instance declaration (See #12791, #5835). For example:

    class M a b where m :: a -> b

    type C a b = (Num a, M a b)

    f :: C Int b => b -> Int -> Int
    f _ x = x + 1

The body of `f` requires a [W] `Num Int` instance. We could solve this
constraint from the givens because we have `C Int b` and that provides us a
solution for `Num Int`. This would let us produce core like the following
(with -O2):

    f :: forall b. C Int b => b -> Int -> Int
    f = \ (@ b) ($d(%,%) :: C Int b) _ (eta1 :: Int) ->
        + @ Int
          (GHC.Classes.$p1(%,%) @ (Num Int) @ (M Int b) $d(%,%))
          eta1
          A.f1

This is bad! We could do /much/ better if we solved [W] `Num Int` directly
from the instance that we have in scope:

    f :: forall b. C Int b => b -> Int -> Int
    f = \ (@ b) _ _ (x :: Int) ->
        case x of { GHC.Types.I# x1 -> GHC.Types.I# (GHC.Prim.+# x1 1#) }

** NB: It is important to emphasize that all this is purely an optimization:
** exactly the same programs should typecheck with or without this procedure.

Consider
       f :: Ord [a] => ...
       f x = ..Need Eq [a]...
We could use the Eq [a] superclass of the Ord [a], or we could use the top-level
instance `Eq a => Eq [a]`.   But if we did the latter we'd be stuck with an
insoluble constraint (Eq a).

-----------------------------------
So the ShortCutSolving plan is this:
   If we could solve a constraint from a local Given,
       try first to /completely/ solve the constraint
       using only top-level instances,
       /without/ using any local Givens.
   - If that succeeds, use it
   - If not, use the local Given
-----------------------------------

An example that succeeds:

    class Eq a => C a b | b -> a where
      m :: b -> a

    f :: C [Int] b => b -> Bool
    f x = m x == []

We solve for `Eq [Int]`, which requires `Eq Int`, which we also have. This
produces the following core:

    f :: forall b. C [Int] b => b -> Bool
    f = \ (@ b) ($dC :: C [Int] b) (x :: b) ->
        GHC.Classes.$fEq[]_$s$c==
          (m @ [Int] @ b $dC x) (GHC.Types.[] @ Int)

An example that fails:

    class Eq a => C a b | b -> a where
      m :: b -> a

    f :: C [a] b => b -> Bool
    f x = m x == []

Which, because solving `Eq [a]` demands `Eq a` which we cannot solve. so short-cut
solving fails and we use the superclass of C:

    f :: forall a b. C [a] b => b -> Bool
    f = \ (@ a) (@ b) ($dC :: C [a] b) (eta :: b) ->
        ==
          @ [a]
          (A.$p1C @ [a] @ b $dC)
          (m @ [a] @ b $dC eta)
          (GHC.Types.[] @ a)

The moving parts are relatively simple:

* To attempt to solve the constraint completely, we just recursively
  call the constraint solver. See the use of `tryShortCutTcS` in
  `tcShortCutSolver`.

* When this attempted recursive solving, in `tryShortCutTcS`, we
  - start with an empty inert set: no Givens and no Wanteds
  - set a special mode  `TcSShortCut`, which signals that we are trying to solve
    using only top-level instances.

* When in TcSShortCut mode, since there are no Givens we can short-circuit;
  these are all just optimisations:
      - `tryInertDicts`
      - `GHC.Tc.Solver.Monad.lookupInertDict`
      - `noMatchableGivenDicts`
      - `matchLocalInst`
      - `GHC.Tc.Solver.Solve.runTcPluginsWanted`

* In `GHC.Tc.Solver.Instance.Class.matchInstEnv`: when short-cut solving,
  don't pick overlappable top-level instances

Some wrinkles:

(SCS1) Note [Shortcut solving: incoherence]

(SCS2) The short-cut solver just uses the solver recursively, so we get its
  full power:

    * We need to be able to handle recursive super classes. The
      solved_dicts state  ensures that we remember what we have already
      tried to solve to avoid looping.

    * As #15164 showed, it can be important to exploit sharing between
      goals. E.g. To solve G we may need G1 and G2. To solve G1 we may need H;
      and to solve G2 we may need H. If we don't spot this sharing we may
      solve H twice; and if this pattern repeats we may get exponentially bad
      behaviour.

    * Suppose we have (#13943)
          class Take (n :: Nat) where ...
          instance {-# OVERLAPPING #-}                    Take 0 where ..
          instance {-# OVERLAPPABLE #-} (Take (n - 1)) => Take n where ..

      And we have [W] Take 3.  That only matches one instance so we get
      [W] Take (3-1).  Then we should reduce the (3-1) to 2, and continue.

(SCS3) When doing short-cut solving we can (and should) inherit the `solved_dicts`
    of the caller (#15164).  You might worry about having a solved-dict that uses
    a Given -- but that too will have been subject to short-cut solving so it's fine.

(SCS4) In `tryShortCutSolver`, when deciding if we have "completely solved" the
   constraint, we must use `isSolvedWC` not `isEmptyWC`.  The latter says "False"
   if the residual constraint has any implications, even solved ones; and we
   don't want to reject short-cut solving just because we have some leftover
   /solved/ implications.  #26805 was a case in point.

Note [Shortcut solving: incoherence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This optimization relies on coherence of dictionaries to be correct. When we
cannot assume coherence because of IncoherentInstances then this optimization
can change the behavior of the user's code.

The following four modules produce a program whose output would change depending
on whether we apply this optimization when IncoherentInstances is in effect:

=========
    {-# LANGUAGE MultiParamTypeClasses #-}
    module A where

    class A a where
      int :: a -> Int

    class A a => C a b where
      m :: b -> a -> a

=========
    {-# LANGUAGE FlexibleInstances     #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    module B where

    import A

    instance A a where
      int _ = 1

    instance C a [b] where
      m _ = id

=========
    {-# LANGUAGE FlexibleContexts      #-}
    {-# LANGUAGE FlexibleInstances     #-}
    {-# LANGUAGE IncoherentInstances   #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    module C where

    import A

    instance A Int where
      int _ = 2

    instance C Int [Int] where
      m _ = id

    intC :: C Int a => a -> Int -> Int
    intC _ x = int x

=========
    module Main where

    import A
    import B
    import C

    main :: IO ()
    main = print (intC [] (0::Int))

The output of `main` if we avoid the optimization under the effect of
IncoherentInstances is `1`. If we were to do the optimization, the output of
`main` would be `2`.
-}

tryInertDicts :: DictCt -> SolverStage ()
tryInertDicts dict_ct
  = Stage $ do { inerts <- getInertCans
               ; try_inert_dicts inerts dict_ct }

try_inert_dicts :: InertCans -> DictCt -> TcS (StopOrContinue ())
try_inert_dicts inerts dict_w@(DictCt { di_ev = ev_w, di_cls = cls, di_tys = tys })
  | Just dict_i <- lookupInertDict inerts cls tys
  , let ev_i  = dictCtEvidence dict_i
        loc_i = ctEvLoc ev_i
        loc_w = ctEvLoc ev_w
  = -- There is a matching dictionary in the inert set
    do { -- For a Wanted, first to try to solve it /completely/ from top level instances
         -- See Note [Shortcut solving]
       ; dflags <- getDynFlags
       ; short_cut_worked <- tryShortCutSolver dflags (isGiven ev_i) dict_w

       ; if | short_cut_worked
            -> stopWith ev_w "shortCutSolver worked(1)"

            -- Next see if we are in "loopy-superclass" land.  If so,
            -- we don't want to replace the (Given) inert with the
            -- (Wanted) work-item, or vice versa; we want to hang on
            -- to both, and try to solve the work-item via an instance.
            -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
            | prohibitedSuperClassSolve loc_i loc_w
            -> continueWith ()

            | otherwise -- We can either solve the inert from the work-item or vice-versa.
            -> case solveOneFromTheOther (CDictCan dict_i) (CDictCan dict_w) of
                 KeepInert -> do { traceTcS "lookupInertDict:KeepInert" (ppr dict_w)
                                 ; setDictIfWanted ev_w EvCanonical (ctEvTerm ev_i)
                                 ; return $ Stop ev_w (text "Dict equal" <+> ppr dict_w) }
                 KeepWork  -> do { traceTcS "lookupInertDict:KeepWork" (ppr dict_w)
                                 ; setDictIfWanted ev_i EvCanonical (ctEvTerm ev_w)
                                 ; updInertCans (updDicts $ delDict dict_w)
                                 ; continueWith () } }

  | otherwise
  = do { traceTcS "tryInertDicts:no" (ppr dict_w $$ ppr cls <+> ppr tys)
       ; continueWith () }

-- See Note [Shortcut solving]
tryShortCutSolver :: DynFlags
                  -> Bool       -- True <=> try the short-cut solver; False <=> don't
                  -> DictCt     -- Work item
                  -> TcS Bool   -- True <=> success
-- We are about to solve a [W] constraint from a [G] constraint. We take
-- a moment to see if we can get a better solution using an instance.
-- Note that we only do this for the sake of performance. Exactly the same
-- programs should typecheck regardless of whether we take this step or
-- not. See Note [Shortcut solving]
tryShortCutSolver dflags try_short_cut dict_w
  | try_short_cut
  , DictCt { di_ev = ev_w } <- dict_w
  , CtWanted (WantedCt { ctev_pred = pred_w }) <- ev_w
  , not (couldBeIPLike pred_w)   -- Not for implicit parameters (#18627)

  , not (xopt LangExt.IncoherentInstances dflags)
              -- If IncoherentInstances is on then we cannot rely on coherence of proofs
              -- in order to justify this optimization: The proof provided by the
              -- [G] constraint's superclass may be different from the top-level proof.
              -- See Note [Shortcut solving: incoherence]
  , gopt Opt_SolveConstantDicts dflags
              -- Enabled by the -fsolve-constant-dicts flag

  = tryShortCutTcS $  -- tryTcS tries to completely solve some contraints
    solveSimpleWanteds (unitBag (CDictCan dict_w))

  | otherwise
  = return False


{- *******************************************************************
*                                                                    *
         Top-level reaction for class constraints (CDictCan)
*                                                                    *
**********************************************************************-}

tryInstances :: DictCt -> SolverStage ()
tryInstances dict_ct
  = Stage $ do { inerts <- getInertSet
               ; try_instances inerts dict_ct }

try_instances :: InertSet -> DictCt -> TcS (StopOrContinue ())
-- Try to use type-class instance declarations to simplify the constraint

-- Case for Givens
-- Never use instances for Given constraints
try_instances _ (DictCt { di_ev = CtGiven {} })
  = -- See Note [No Given/Given fundeps]
    continueWith ()

-- Case for Wanteds
try_instances inerts work_item@(DictCt { di_ev = ev@(CtWanted wev), di_cls = cls, di_tys = xis })
  | Just solved_ev <- lookupSolvedDict inerts cls xis   -- Cached
  = do { setDictIfWanted ev EvCanonical (ctEvTerm solved_ev)
       ; stopWith ev "Dict/Top (cached)" }

  | otherwise  -- Wanted, but not cached
   = do { dflags <- getDynFlags
        ; lkup_res <- matchClassInst dflags inerts cls xis dict_loc
        ; case lkup_res of
               OneInst { cir_what = what }
                  -> do { let is_local_given = case what of { LocalInstance -> True; _ -> False }
                        ; take_shortcut <- tryShortCutSolver dflags is_local_given work_item
                        ; if take_shortcut
                          then stopWith ev "shortCutSolver worked(2)"
                          else do { insertSafeOverlapFailureTcS what work_item
                                  ; updSolvedDicts what work_item
                                  ; chooseInstance wev lkup_res
                                  ; stopWith ev "Dict/Top (solved wanted)" } }
               _  -> -- NoInstance or NotSure: we didn't solve it
                     continueWith () }
   where
     dict_loc = ctEvLoc ev

chooseInstance :: WantedCtEvidence -> ClsInstResult -> TcS ()
chooseInstance work_item@(WantedCt { ctev_dest = dest, ctev_rewriters = rws
                                   , ctev_loc = loc, ctev_pred = pred })
               (OneInst { cir_new_theta   = theta
                        , cir_what        = what
                        , cir_mk_ev       = mk_ev
                        , cir_canonical   = canonical })
  = do { traceTcS "doTopReact/found instance for" $ ppr work_item

      -- Check that the dfun is well-staged in the Template Haskell sense
       ; checkWellLevelledDFun loc what pred

       -- zapped_loc: after applying an instance we can set ScOrigin to
       -- NotNakedSc, so that prohibitedSuperClassSolve never fires
       -- See Note [Solving superclass constraints] in
       -- GHC.Tc.TyCl.Instance, (sc1).
       ; let zapped_loc | ScOrigin what _ <- ctLocOrigin loc
                        = setCtLocOrigin loc (ScOrigin what NotNakedSc)
                        | otherwise
                        = loc

       -- Deeper location for new constraints
       ; deeper_loc <- bumpReductionDepth zapped_loc pred

       ; assertPprM (getTcEvBindsVar >>= return . not . isCoEvBindsVar)
                    (ppr work_item)
       ; evc_vars <- mapM (newWanted deeper_loc rws) theta
       ; setWantedDict dest canonical (mk_ev (map getEvExpr evc_vars))
       ; emitWorkNC (map CtWanted $ freshGoals evc_vars) }

chooseInstance work_item lookup_res
  = pprPanic "chooseInstance" (ppr work_item $$ ppr lookup_res)

matchClassInst :: DynFlags -> InertSet
               -> Class -> [Type]
               -> CtLoc -> TcS ClsInstResult
matchClassInst dflags inerts clas tys loc
-- First check whether there is an in-scope Given that could
-- match this constraint.  In that case, do not use any instance
-- whether top level, or local quantified constraints.
-- See Note [Instance and Given overlap] and see
-- (IL0) in Note [Rules for instance lookup] in GHC.Core.InstEnv
  | not (xopt LangExt.IncoherentInstances dflags)
  , not (isCTupleClass clas)
        -- It is always safe to unpack constraint tuples
        -- And if we don't do so, we may never solve it at all
        -- See Note [Solving tuple constraints]
  , not (noMatchableGivenDicts inerts loc clas tys)
  = do { traceTcS "Delaying instance application" $
           vcat [ text "Work item:" <+> pprClassPred clas tys ]
       ; return NotSure }

  | otherwise
  = do { traceTcS "matchClassInst" $ text "pred =" <+> ppr pred <+> char '{'
       ; local_res <- matchLocalInst pred loc
       ; case local_res of
           OneInst {} ->  -- See Note [Local instances and incoherence]
                do { traceTcS "} matchClassInst local match" $ ppr local_res
                   ; return local_res }

           NotSure -> -- In the NotSure case for local instances
                      -- we don't want to try global instances
                do { traceTcS "} matchClassInst local not sure" empty
                   ; return local_res }

           NoInstance  -- No local instances, so try global ones
              -> do { global_res <- matchGlobalInst dflags clas tys loc
                    ; warn_custom_warn_instance global_res loc
                          -- See Note [Implementation of deprecated instances]
                    ; traceTcS "} matchClassInst global result" $ ppr global_res
                    ; return global_res } }
  where
    pred = mkClassPred clas tys

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
      | CtGiven (GivenCt { ctev_loc = loc_g, ctev_pred = pred_g }) <- ev
      = isJust $ mightEqualLater inerts pred_g loc_g pred_w loc_w

      | otherwise
      = False

{- Note [Implementation of deprecated instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This note describes the implementation of the deprecated instances GHC proposal
  https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0575-deprecated-instances.rst

In the parser, we parse deprecations/warnings attached to instances:

  instance {-# DEPRECATED "msg" #-} Show X
  deriving instance {-# WARNING "msg2" #-} Eq Y

(Note that non-standalone deriving instance declarations do not support this mechanism.)
(Note that the DEPRECATED and WARNING pragmas can be used here interchangeably.)

We store the resulting warning message in the extension field of `ClsInstDecl`
(respectively, `DerivDecl`; See Note [Trees That Grow]).

In `GHC.Tc.TyCl.Instance.tcClsInstDecl` (respectively, `GHC.Tc.Deriv.Utils.newDerivClsInst`),
we pass on that information to `ClsInst` (and eventually store it in `IfaceClsInst` too).

Next, if we solve a constraint using such an instance, in
`GHC.Tc.Instance.Class.matchInstEnv`, we pass it further into the
`Ghc.Tc.Types.Origin.InstanceWhat`.

Finally, if the instance solving function `GHC.Tc.Solver.Monad.matchGlobalInst` returns
a `Ghc.Tc.Instance.Class.ClsInstResult` with `Ghc.Tc.Types.Origin.InstanceWhat` containing
a warning, when called from either `matchClassInst` or `shortCutSolver`, we call
`warn_custom_warn_instance` that ultimately emits the warning if needed.

Note that we only emit a warning when the instance is used in a different module
than it is defined, which keeps the behaviour in line with the deprecation of
top-level identifiers.
-}

-- | Emits the custom warning for a deprecated instance
--
-- See Note [Implementation of deprecated instances]
warn_custom_warn_instance :: ClsInstResult -> CtLoc -> TcS ()
warn_custom_warn_instance (OneInst{ cir_what = what }) ct_loc
  | TopLevInstance{ iw_dfun_id = dfun, iw_warn = Just warn } <- what = do
      let mod = nameModule $ getName dfun
      this_mod <- getModule
      when (this_mod /= mod)
          -- We don't emit warnings for usages inside of the same module
          -- to prevent it being triggered for instance child declarations
        $ ctLocWarnTcS ct_loc
          $ TcRnPragmaWarning
              { pragma_warning_info = PragmaWarningInstance dfun (ctl_origin ct_loc)
              , pragma_warning_msg  = warn }
warn_custom_warn_instance _ _ = return ()

{- Note [Instance and Given overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example, from the OutsideIn(X) paper:
       instance P x => Q [x]
       instance (x ~ y) => R y [x]

       wob :: forall a b. (Q [b], R b a) => a -> Int

       g :: forall a. Q [a] => [a] -> Int
       g x = wob x

From 'g' we get the implication constraint:
            forall a. Q [a] => (Q [beta], R beta [a])
If we react (Q [beta]) with its top-level axiom, we end up with a
(P beta), which we have no way of discharging. On the other hand,
if we react R beta [a] with the top-level we get  (beta ~ a), which
is solvable and can help us rewrite (Q [beta]) to (Q [a]) which is
now solvable by the given Q [a].

The partial solution is that:
  In matchClassInst (and thus in topReact), we return a matching
  instance only when there is no Given in the inerts which is
  unifiable to this particular dictionary.

  We treat any meta-tyvar as "unifiable" for this purpose,
  *including* untouchable ones.  But not skolems like 'a' in
  the implication constraint above.

The end effect is that, much as we do for overlapping instances, we
delay choosing a class instance if there is a possibility of another
instance OR a given to match our constraint later on. This fixes
tickets #4981 and #5002.

Other notes:

* The check is done *first*, so that it also covers classes
  with built-in instance solving, such as
     - constraint tuples
     - natural numbers
     - Typeable

* See also Note [What might equal later?] in GHC.Tc.Utils.Unify

* The given-overlap problem is arguably not easy to appear in practice
  due to our aggressive prioritization of equality solving over other
  constraints, but it is possible. I've added a test case in
  typecheck/should-compile/GivenOverlapping.hs

* Another "live" example is #10195; another is #10177.

* We ignore the overlap problem if -XIncoherentInstances is in force:
  see #6002 for a worked-out example where this makes a
  difference.

* Moreover notice that our goals here are different than the goals of
  the top-level overlapping checks. There we are interested in
  validating the following principle:

      If we inline a function f at a site where the same global
      instance environment is available as the instance environment at
      the definition site of f then we should get the same behaviour.

  But for the Given Overlap check our goal is just related to completeness of
  constraint solving.

* The solution is only a partial one.  Consider the above example with
       g :: forall a. Q [a] => [a] -> Int
       g x = let v = wob x
             in v
  and suppose we have -XNoMonoLocalBinds, so that we attempt to find the most
  general type for 'v'.  When generalising v's type we'll simplify its
  Q [alpha] constraint, but we don't have Q [a] in the 'givens', so we
  will use the instance declaration after all. #11948 was a case
  in point.

All of this is disgustingly delicate, so to discourage people from writing
simplifiable class givens, we warn about signatures that contain them;
see GHC.Tc.Validity Note [Simplifiable given constraints].


Note [Local instances and incoherence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: forall b c. (Eq b, forall a. Eq a => Eq (c a))
                 => c b -> Bool
   f x = x==x

We get [W] Eq (c b), and we must use the local instance to solve it.

BUT that wanted also unifies with the top-level Eq [a] instance,
and Eq (Maybe a) etc.  We want the local instance to "win", otherwise
we can't solve the wanted at all.  So we mark it as Incohherent.
According to Note [Rules for instance lookup] in GHC.Core.InstEnv, that'll
make it win even if there are other instances that unify.

Moreover this is not a hack!  The evidence for this local instance
will be constructed by GHC at a call site... from the very instances
that unify with it here.  It is not like an incoherent user-written
instance which might have utterly different behaviour.

Consider  f :: Eq a => blah.  If we have [W] Eq a, we certainly
get it from the Eq a context, without worrying that there are
lots of top-level instances that unify with [W] Eq a!  We'll use
those instances to build evidence to pass to f. That's just the
nullary case of what's happening here.
-}

matchLocalInst :: TcPredType -> CtLoc -> TcS ClsInstResult
-- Look up the predicate in Given quantified constraints,
-- which are effectively just local instance declarations.
matchLocalInst body_pred loc
  = do { -- Look in the inert set for a matching Given quantified constraint
         inerts@(IS { inert_cans = ics }) <- getInertSet
       ; case match_local_inst inerts (inert_qcis ics) of
            { ([], []) -> do { traceTcS "No local instance for" (ppr body_pred)
                             ; return NoInstance }
            ; (matches, unifs) ->

    do { -- Find the best match
         -- See Note [Use only the best matching quantified constraint]
         matches <- mapM mk_instDFun matches
       ; unifs   <- mapM mk_instDFun unifs
       ; case dominatingMatch matches of
          { Just (dfun_id, tys, theta)
            | all ((theta `impliedBySCs`) . thdOf3) unifs
            ->
            do { let result = OneInst { cir_new_theta   = theta
                                      , cir_mk_ev       = evDFunApp dfun_id tys
                                      , cir_canonical   = EvCanonical
                                      , cir_what        = LocalInstance }
               ; traceTcS "Best local instance found:" $
                  vcat [ text "body_pred:" <+> ppr body_pred
                       , text "result:" <+> ppr result
                       , text "matches:" <+> ppr matches
                       , text "unifs:" <+> ppr unifs ]
               ; return result }

          ; mb_best ->
            do { traceTcS "Multiple local instances; not committing to any"
                  $ vcat [ text "body_pred:" <+> ppr body_pred
                         , text "matches:" <+> ppr matches
                         , text "unifs:" <+> ppr unifs
                         , text "best_match:" <+> ppr mb_best ]
               ; return NotSure }}}}}
  where
    body_pred_tv_set = tyCoVarsOfType body_pred

    mk_instDFun :: (CtEvidence, [DFunInstType]) -> TcS InstDFun
    mk_instDFun (ev, tys) =
      let dfun_id = ctEvEvId ev
      in do { (tys, theta) <- instDFunType (ctEvEvId ev) tys
            ; return (dfun_id, tys, theta) }

    -- Compute matching and unifying local instances
    match_local_inst :: InertSet
                     -> [QCInst]
                     -> ( [(CtEvidence, [DFunInstType])]
                        , [(CtEvidence, [DFunInstType])] )
    match_local_inst _inerts []
      = ([], [])
    match_local_inst inerts (qci@(QCI { qci_tvs  = qtvs
                                      , qci_body = qbody
                                      , qci_ev   = qev })
                            : qcis)
      | isWanted qev    -- Skip Wanteds
      = match_local_inst inerts qcis

      | let in_scope = mkInScopeSet (qtv_set `unionVarSet` body_pred_tv_set)
      , Just tv_subst <- ruleMatchTyKiX qtv_set (mkRnEnv2 in_scope)
                                        emptyTvSubstEnv qbody body_pred
      , let match = (qev, map (lookupVarEnv tv_subst) qtvs)
      = (match:matches, unifs)

      | otherwise
      = assertPpr (disjointVarSet qtv_set (tyCoVarsOfType body_pred))
                  (ppr qci $$ ppr body_pred)
            -- ASSERT: unification relies on the
            -- quantified variables being fresh
        (matches, this_unif `combine` unifs)
      where
        qloc = ctEvLoc qev
        qtv_set = mkVarSet qtvs
        (matches, unifs) = match_local_inst inerts qcis
        this_unif
          | Just subst <- mightEqualLater inerts qbody qloc body_pred loc
          = Just (qev, map  (lookupTyVar subst) qtvs)
          | otherwise
          = Nothing

        combine Nothing  us = us
        combine (Just u) us = u : us

-- | Instance dictionary function and type.
type InstDFun = (DFunId, [TcType], TcThetaType)

-- | Try to find a local quantified instance that dominates all others,
-- i.e. which has a weaker instance context than all the others.
--
-- See Note [Use only the best matching quantified constraint].
dominatingMatch :: [InstDFun] -> Maybe InstDFun
dominatingMatch matches =
  listToMaybe $ mapMaybe (uncurry go) (holes matches)
  -- listToMaybe: arbitrarily pick any one context that is weaker than
  -- all others, e.g. so that we can handle [Eq a, Num a] vs [Num a, Eq a]
  -- (see test case T22223).

  where
    go :: InstDFun -> [InstDFun] -> Maybe InstDFun
    go this [] = Just this
    go this@(_,_,this_theta) ((_,_,other_theta):others)
      | this_theta `impliedBySCs` other_theta
      = go this others
      | otherwise
      = Nothing

-- | Whether a collection of constraints is implied by another collection,
-- according to a simple superclass check.
--
-- See Note [When does a quantified instance dominate another?].
impliedBySCs :: TcThetaType -> TcThetaType -> Bool
impliedBySCs c1 c2 = all in_c2 c1
  where
    in_c2 :: TcPredType -> Bool
    in_c2 pred = any (pred `tcEqType`) c2_expanded

    c2_expanded :: [TcPredType]  -- Includes all superclasses
    c2_expanded = [ q | p <- c2, q <- p : transSuperClasses p ]


{- Note [When does a quantified instance dominate another?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When matching local quantified instances, it's useful to be able to pick
the one with the weakest precondition, e.g. if one has both

  [G] d1: forall a b. ( Eq a, Num b, C a b  ) => D a b
  [G] d2: forall a  .                C a Int  => D a Int
  [W] {w}: D a Int

Then it makes sense to use d2 to solve w, as doing so we end up with a strictly
weaker proof obligation of `C a Int`, compared to `(Eq a, Num Int, C a Int)`
were we to use d1.

In theory, to compute whether one context implies another, we would need to
recursively invoke the constraint solver. This is expensive, so we instead do
a simple check using superclasses, implemented in impliedBySCs.

Examples:

 - [Eq a] is implied by [Ord a]
 - [Ord a] is not implied by [Eq a],
 - any context is implied by itself,
 - the empty context is implied by any context.

Note [Use only the best matching quantified constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#20582) the ambiguity check for
  (forall a. Ord (m a), forall a. Semigroup a => Eq (m a)) => m Int

Because of eager expansion of given superclasses, we get
  [G] d1: forall a. Ord (m a)
  [G] d2: forall a. Eq (m a)
  [G] d3: forall a. Semigroup a => Eq (m a)

  [W] {w1}: forall a. Ord (m a)
  [W] {w2}: forall a. Semigroup a => Eq (m a)

The first wanted is solved straightforwardly. But the second wanted
matches *two* local instances: d2 and d3. Our general rule around multiple local
instances is that we refuse to commit to any of them. However, that
means that our type fails the ambiguity check. That's bad: the type
is perfectly fine. (This actually came up in the wild, in the streamly
library.)

The solution is to prefer local instances which are easier to prove, meaning
that they have a weaker precondition. In this case, the empty context
of d2 is a weaker constraint than the "Semigroup a" context of d3, so we prefer
using it when proving w2. This allows us to pass the ambiguity check here.

Our criterion for solving a Wanted by matching local quantified instances is
thus as follows:

  - There is a matching local quantified instance that dominates all others
    matches, in the sense of [When does a quantified instance dominate another?].
    Any such match do, we pick it arbitrarily (the T22223 example below says why).
  - This local quantified instance also dominates all the unifiers, as we
    wouldn't want to commit to a single match when we might have multiple,
    genuinely different matches after further unification takes place.

Some other examples:


  #15244:

    f :: (C g, D g) => ....
    class S g => C g where ...
    class S g => D g where ...
    class (forall a. Eq a => Eq (g a)) => S g where ...

  Here, in f's RHS, there are two identical quantified constraints
  available, one via the superclasses of C and one via the superclasses
  of D. Given that each implies the other, we pick one arbitrarily.


  #22216:

    class Eq a
    class Eq a => Ord a
    class (forall b. Eq b => Eq (f b)) => Eq1 f
    class (Eq1 f, forall b. Ord b => Ord (f b)) => Ord1 f

  Suppose we have

    [G] d1: Ord1 f
    [G] d2: Eq a
    [W] {w}: Eq (f a)

  Superclass expansion of d1 gives us:

    [G] d3 : Eq1 f
    [G] d4 : forall b. Ord b => Ord (f b)

  expanding d4 and d5 gives us, respectively:

    [G] d5 : forall b. Eq  b => Eq (f b)
    [G] d6 : forall b. Ord b => Eq (f b)

  Now we have two matching local instances that we could use when solving the
  Wanted. However, it's obviously silly to use d6, given that d5 provides us with
  as much information, with a strictly weaker precondition. So we pick d5 to solve
  w. If we chose d6, we would get [W] Ord a, which in this case we can't solve.


  #22223:

    [G] forall a b. (Eq a, Ord b) => C a b
    [G] forall a b. (Ord b, Eq a) => C a b
    [W] C x y

  Here we should be free to pick either quantified constraint, as they are
  equivalent up to re-ordering of the constraints in the context.
  See also Note [Do not add duplicate quantified instances]
  in GHC.Tc.Solver.Monad.

Test cases:
  typecheck/should_compile/T20582
  quantified-constraints/T15244
  quantified-constraints/T22216{a,b,c,d,e}
  quantified-constraints/T22223

Historical note: a previous solution was to instead pick the local instance
with the least superclass depth (see Note [Replacement vs keeping]),
but that doesn't work for the example from #22216.
-}


{- *********************************************************************
*                                                                      *
*                      Superclasses
*                                                                      *
********************************************************************* -}

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
   This is done using mkStrictSuperClasses in canDictCt, when
   we take a non-canonical Given constraint and cannonicalise it.

   However stop if you encounter the same class twice.  That is,
   mkStrictSuperClasses expands eagerly, but has a conservative
   termination condition: see Note [Expanding superclasses] in GHC.Tc.Utils.TcType.

2. Solve the wanteds as usual, but do no further expansion of
   superclasses for canonical CDictCans in solveSimpleGivens or
   solveSimpleWanteds; Note [Danger of adding superclasses during solving]

   However, /do/ continue to eagerly expand superclasses for new /given/
   /non-canonical/ constraints (canDictCt does this).  As #12175
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
Then canDictCt gets the [G] d1: C a constraint, and eager emits superclasses
G] d2: D a, [G] d3: C a (psc).  (The "psc" means it has its cc_pend_sc has pending
expansion fuel.)
When processing d3 we find a match with d1 in the inert set, and we always
keep the inert item (d1) if possible: see Note [Replacement vs keeping] in
GHC.Tc.Solver.InertSet.  So d3 dies a quick, happy death.

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

See Note [Instance and Given overlap].

We also want to do this if we have

   f :: F (T a) => blah

where
   type instance F (T a) = Ord (T a)

So we may need to do a little work on the givens to expose the
class that has the superclasses.  That's why the superclass
expansion for Givens happens in canDictCt.

This same scenario happens with quantified constraints, whose superclasses
are also eagerly expanded. Test case: typecheck/should_compile/T16502b
These are handled in canForAllNC, analogously to canDictCt.

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
and find a superclass constraint (itself perhaps a quantified constraint)
  forall sc_tvs. sc_theta => sc_inner_pred
we must have a selector
  sel_id :: forall cls_tvs. cls cls_tvs => forall sc_tvs. sc_theta => sc_inner_pred
and thus build
  g_sc :: forall tvs sc_tvs. theta => sc_theta => sc_inner_pred
  g_sc = /\ tvs. /\ sc_tvs. \ theta_ids. \ sc_theta_ids.
         sel_id tys (g0 tvs theta_ids) sc_tvs sc_theta_ids

Actually, we cheat a bit by eta-reducing: note that sc_theta_ids are both the
last bound variables and the last arguments. This avoids the need to produce
the sc_theta_ids at all. So our final construction is

  g_sc = /\ tvs. /\ sc_tvs. \ theta_ids.
         sel_id tys (g0 tvs theta_ids) sc_tvs

(NQCS1) Common case.  If theta_ids[] we get just
     g_sc = /\ tvs. /\sc_tvs. sel_id tys (g0 tvs) sc_tvs
  and can eta-reduce even more to
     g_sc = /\ tvs. sel_id tys (g0 tvs)
  and if tvs=[] we get the straight superclass selection
     g_sc = sel_id tys g0
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
    go (CDictCan (DictCt { di_ev = ev, di_cls = cls, di_tys = tys, di_pend_sc = fuel }))
      = assertFuelPreconditionStrict fuel $ -- fuel needs to be more than 0 always
        mkStrictSuperClasses fuel ev [] [] cls tys
    go (CQuantCan (QCI { qci_body = body_pred, qci_ev = ev, qci_pend_sc = fuel }))
      = assertPpr (isClassPred body_pred) (ppr body_pred) $ -- The cts should all have class pred heads
        assertFuelPreconditionStrict fuel $                 -- fuel needs to be more than 0, always
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
mk_strict_superclasses _ _ _ _ _ cls _
  | isEqualityClass cls  -- See (EQC1) in Note [Solving equality classes]
  = return []

mk_strict_superclasses fuel rec_clss ev@(CtGiven (GivenCt { ctev_evar = evar })) tvs theta cls tys
  = -- Given case
    do { traceTcS "mk_strict" (ppr ev $$ ppr (ctLocOrigin loc))
       ; concatMapM do_one_given (classSCSelIds cls) }
  where
    loc  = ctEvLoc ev
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
      = do { given_ev <- newGivenEv sc_loc $
                         mk_given_desc sel_id sc_pred
           ; assertFuelPrecondition fuel $
             mk_superclasses fuel rec_clss (CtGiven given_ev) tvs theta sc_pred }
      where
        sc_pred = classMethodInstTy sel_id tys

      -- See Note [Nested quantified constraint superclasses]
    mk_given_desc :: Id -> PredType -> (PredType, EvTerm)
    mk_given_desc sel_id sc_pred
      = (swizzled_pred, EvExpr swizzled_evterm)
      where
        (sc_tvs, sc_rho)          = splitForAllTyCoVars sc_pred
        (sc_theta, sc_inner_pred) = splitFunTys sc_rho

        all_tvs       = tvs `chkAppend` sc_tvs
        all_theta     = theta `chkAppend` (map scaledThing sc_theta)
        swizzled_pred = mkInfSigmaTy all_tvs all_theta sc_inner_pred

        -- evar   :: forall tvs. theta => cls tys
        -- sel_id :: forall cls_tvs. cls cls_tvs
        --                        => forall sc_tvs. sc_theta => sc_inner_pred
        -- swizzled_evterm :: forall tvs sc_tvs. theta => sc_theta => sc_inner_pred
        swizzled_evterm
          | null tvs, null theta -- See wrinkle (NQCS1)
          = Var sel_id `mkTyApps` tys `App` evId evar
          | otherwise
          = mkLams all_tvs $ mkLams dict_ids $
            Var sel_id `mkTyApps` tys
                       `App` (evId evar `mkVarApps` (tvs ++ dict_ids))
                       `mkVarApps` sc_tvs

    sc_loc | isCTupleClass cls = loc
           | otherwise         = loc { ctl_origin = mk_sc_origin (ctLocOrigin loc) }
           -- isCTupleClass: we don't want tuples to mess up the size calculations
           -- of Note [Solving superclass constraints]. For tuple predicates, this
           -- matters, because their size can be large, and we don't want to add a
           -- big class to the size of the dictionaries in the chain. When we get
           -- down to a base predicate, we'll include its size. See #10335.
           -- See Note [Solving tuple constraints]

    -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
    -- for explanation of GivenSCOrigin and Note [Replacement vs keeping] in
    -- GHC.Tc.Solver.InertSet for why we need depths
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

-- Wanted case
mk_strict_superclasses fuel rec_clss
  (CtWanted (WantedCt { ctev_pred = pty, ctev_loc = loc0, ctev_rewriters = rws }))
  tvs theta cls tys
  | all noFreeVarsOfType tys
  = return [] -- Wanteds with no variables yield no superclass constraints.
              -- See Note [Improvement from Ground Wanteds]

  | otherwise -- Wanted case, just add Wanted superclasses
              -- that can lead to improvement.
  = assertPpr (null tvs && null theta) (ppr tvs $$ ppr theta) $
    concatMapM do_one (immSuperClasses cls tys)
  where
    loc = loc0 `updateCtLocOrigin` WantedSuperclassOrigin pty

    do_one sc_pred
      = do { traceTcS "mk_strict_superclasses Wanted" (ppr (mkClassPred cls tys) $$ ppr sc_pred)
           ; sc_ev <- newWantedNC loc rws sc_pred
           ; mk_superclasses fuel rec_clss (CtWanted sc_ev) [] [] sc_pred }

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
    -- We can't use CNonCanonical here because we need to track the fuel
    mk_this_ct fuel | null tvs, null theta
                    = CDictCan (DictCt { di_ev = ev, di_cls = cls
                                       , di_tys = tys, di_pend_sc = fuel })
                    -- NB: If there is a loop, we cut off, so we have not
                    --     added the superclasses, hence cc_pend_sc = fuel
                    | otherwise
                    = CQuantCan (QCI { qci_tvs = tvs, qci_theta = theta
                                     , qci_body = mkClassPred cls tys
                                     , qci_ev = ev, qci_pend_sc = fuel })


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
GHC.Tc.Solver.Equality.tryQCsEqCt.

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
and fails to simplify that any further.

So for now we simply decline to take superclasses in the quantified
case.  Instead we have a special case in GHC.Tc.Solver.Equality.tryQCsEqCt
which looks for primitive equalities specially in the quantified
constraints.

See also Note [Evidence for quantified constraints] in GHC.Core.Predicate.
-}
