{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

************************************************************************
*                                                                      *
\section[OccurAnal]{Occurrence analysis pass}
*                                                                      *
************************************************************************

The occurrence analyser re-typechecks a core expression, returning a new
core expression with (hopefully) improved usage information.
-}

module GHC.Core.Opt.OccurAnal ( occurAnalysePgm, occurAnalyseExpr ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.FVs
import GHC.Core.Utils   ( exprIsTrivial, isDefaultAlt, isExpandableApp,
                          stripTicksTopE, mkTicks )
import GHC.Core.Opt.Arity   ( joinRhsArity, isOneShotBndr )
import GHC.Core.Coercion
import GHC.Core.Type
import GHC.Core.TyCo.FVs( tyCoVarsOfMCo )

import GHC.Data.Maybe( isJust )
import GHC.Data.Graph.Directed ( SCC(..), Node(..)
                               , stronglyConnCompFromEdgedVerticesUniq
                               , stronglyConnCompFromEdgedVerticesUniqR )
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Basic
import GHC.Types.Tickish
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Var
import GHC.Types.Demand ( argOneShots, argsOneShots )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import GHC.Utils.Trace

import GHC.Builtin.Names( runRWKey )
import GHC.Unit.Module( Module )

import Data.List (mapAccumL, mapAccumR)

{-
************************************************************************
*                                                                      *
    occurAnalysePgm, occurAnalyseExpr
*                                                                      *
************************************************************************

Here's the externally-callable interface:
-}

-- | Do occurrence analysis, and discard occurrence info returned
occurAnalyseExpr :: CoreExpr -> CoreExpr
occurAnalyseExpr expr = expr'
  where
    (WithUsageDetails _ expr') = occAnal initOccEnv expr

occurAnalysePgm :: Module         -- Used only in debug output
                -> (Id -> Bool)         -- Active unfoldings
                -> (Activation -> Bool) -- Active rules
                -> [CoreRule]           -- Local rules for imported Ids
                -> CoreProgram -> CoreProgram
occurAnalysePgm this_mod active_unf active_rule imp_rules binds
  | isEmptyDetails final_usage
  = occ_anald_binds

  | otherwise   -- See Note [Glomming]
  = warnPprTrace True (hang (text "Glomming in" <+> ppr this_mod <> colon)
                        2 (ppr final_usage))
    occ_anald_glommed_binds
  where
    init_env = initOccEnv { occ_rule_act = active_rule
                          , occ_unf_act  = active_unf }

    (WithUsageDetails final_usage occ_anald_binds) = go init_env binds
    (WithUsageDetails _ occ_anald_glommed_binds) = occAnalRecBind init_env TopLevel
                                                    imp_rule_edges
                                                    (flattenBinds binds)
                                                    initial_uds
          -- It's crucial to re-analyse the glommed-together bindings
          -- so that we establish the right loop breakers. Otherwise
          -- we can easily create an infinite loop (#9583 is an example)
          --
          -- Also crucial to re-analyse the /original/ bindings
          -- in case the first pass accidentally discarded as dead code
          -- a binding that was actually needed (albeit before its
          -- definition site).  #17724 threw this up.

    initial_uds = addManyOccs emptyDetails (rulesFreeVars imp_rules)
    -- The RULES declarations keep things alive!

    -- imp_rule_edges maps a top-level local binder 'f' to the
    -- RHS free vars of any IMP-RULE, a local RULE for an imported function,
    -- where 'f' appears on the LHS
    --   e.g.  RULE foldr f = blah
    --         imp_rule_edges contains f :-> fvs(blah)
    -- We treat such RULES as extra rules for 'f'
    -- See Note [Preventing loops due to imported functions rules]
    imp_rule_edges :: ImpRuleEdges
    imp_rule_edges = foldr (plusVarEnv_C (++)) emptyVarEnv
                           [ mapVarEnv (const [(act,rhs_fvs)]) $ getUniqSet $
                             exprsFreeIds args `delVarSetList` bndrs
                           | Rule { ru_act = act, ru_bndrs = bndrs
                                   , ru_args = args, ru_rhs = rhs } <- imp_rules
                                   -- Not BuiltinRules; see Note [Plugin rules]
                           , let rhs_fvs = exprFreeIds rhs `delVarSetList` bndrs ]

    go :: OccEnv -> [CoreBind] -> WithUsageDetails [CoreBind]
    go !_ []
        = WithUsageDetails initial_uds []
    go env (bind:binds)
        = WithUsageDetails final_usage (bind' ++ binds')
        where
           (WithUsageDetails bs_usage binds')   = go env binds
           (WithUsageDetails final_usage bind') = occAnalBind env TopLevel imp_rule_edges bind bs_usage

{- *********************************************************************
*                                                                      *
                IMP-RULES
         Local rules for imported functions
*                                                                      *
********************************************************************* -}

type ImpRuleEdges = IdEnv [(Activation, VarSet)]
    -- Mapping from a local Id 'f' to info about its IMP-RULES,
    -- i.e. /local/ rules for an imported Id that mention 'f' on the LHS
    -- We record (a) its Activation and (b) the RHS free vars
    -- See Note [IMP-RULES: local rules for imported functions]

noImpRuleEdges :: ImpRuleEdges
noImpRuleEdges = emptyVarEnv

lookupImpRules :: ImpRuleEdges -> Id -> [(Activation,VarSet)]
lookupImpRules imp_rule_edges bndr
  = case lookupVarEnv imp_rule_edges bndr of
      Nothing -> []
      Just vs -> vs

impRulesScopeUsage :: [(Activation,VarSet)] -> UsageDetails
-- Variable mentioned in RHS of an IMP-RULE for the bndr,
-- whether active or not
impRulesScopeUsage imp_rules_info
  = foldr add emptyDetails imp_rules_info
  where
    add (_,vs) usage = addManyOccs usage vs

impRulesActiveFvs :: (Activation -> Bool) -> VarSet
                  -> [(Activation,VarSet)] -> VarSet
impRulesActiveFvs is_active bndr_set vs
  = foldr add emptyVarSet vs `intersectVarSet` bndr_set
  where
    add (act,vs) acc | is_active act = vs `unionVarSet` acc
                     | otherwise     = acc

{- Note [IMP-RULES: local rules for imported functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We quite often have
  * A /local/ rule
  * for an /imported/ function
like this:
  foo x = blah
  {-# RULE "map/foo" forall xs. map foo xs = xs #-}
We call them IMP-RULES.  They are important in practice, and occur a
lot in the libraries.

IMP-RULES are held in mg_rules of ModGuts, and passed in to
occurAnalysePgm.

Main Invariant:

* Throughout, we treat an IMP-RULE that mentions 'f' on its LHS
  just like a RULE for f.

Note [IMP-RULES: unavoidable loops]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
   f = /\a. B.g a
   RULE B.g Int = 1 + f Int
Note that
  * The RULE is for an imported function.
  * f is non-recursive
Now we
can get
   f Int --> B.g Int      Inlining f
         --> 1 + f Int    Firing RULE
and so the simplifier goes into an infinite loop. This
would not happen if the RULE was for a local function,
because we keep track of dependencies through rules.  But
that is pretty much impossible to do for imported Ids.  Suppose
f's definition had been
   f = /\a. C.h a
where (by some long and devious process), C.h eventually inlines to
B.g.  We could only spot such loops by exhaustively following
unfoldings of C.h etc, in case we reach B.g, and hence (via the RULE)
f.

We regard this potential infinite loop as a *programmer* error.
It's up the programmer not to write silly rules like
     RULE f x = f x
and the example above is just a more complicated version.

Note [Specialising imported functions] (referred to from Specialise)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For *automatically-generated* rules, the programmer can't be
responsible for the "programmer error" in Note [IMP-RULES: unavoidable
loops].  In particular, consider specialising a recursive function
defined in another module.  If we specialise a recursive function B.g,
we get
  g_spec = .....(B.g Int).....
  RULE B.g Int = g_spec
Here, g_spec doesn't look recursive, but when the rule fires, it
becomes so.  And if B.g was mutually recursive, the loop might not be
as obvious as it is here.

To avoid this,
 * When specialising a function that is a loop breaker,
   give a NOINLINE pragma to the specialised function

Note [Preventing loops due to imported functions rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:
  import GHC.Base (foldr)

  {-# RULES "filterList" forall p. foldr (filterFB (:) p) [] = filter p #-}
  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
  filterFB c p = ...

  f = filter p xs

Note that filter is not a loop-breaker, so what happens is:
  f =          filter p xs
    = {inline} build (\c n -> foldr (filterFB c p) n xs)
    = {inline} foldr (filterFB (:) p) [] xs
    = {RULE}   filter p xs

We are in an infinite loop.

A more elaborate example (that I actually saw in practice when I went to
mark GHC.List.filter as INLINABLE) is as follows. Say I have this module:
  {-# LANGUAGE RankNTypes #-}
  module GHCList where

  import Prelude hiding (filter)
  import GHC.Base (build)

  {-# INLINABLE filter #-}
  filter :: (a -> Bool) -> [a] -> [a]
  filter p [] = []
  filter p (x:xs) = if p x then x : filter p xs else filter p xs

  {-# NOINLINE [0] filterFB #-}
  filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
  filterFB c p x r | p x       = x `c` r
                   | otherwise = r

  {-# RULES
  "filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr
  (filterFB c p) n xs)
  "filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
   #-}

Then (because RULES are applied inside INLINABLE unfoldings, but inlinings
are not), the unfolding given to "filter" in the interface file will be:
  filter p []     = []
  filter p (x:xs) = if p x then x : build (\c n -> foldr (filterFB c p) n xs)
                           else     build (\c n -> foldr (filterFB c p) n xs

Note that because this unfolding does not mention "filter", filter is not
marked as a strong loop breaker. Therefore at a use site in another module:
  filter p xs
    = {inline}
      case xs of []     -> []
                 (x:xs) -> if p x then x : build (\c n -> foldr (filterFB c p) n xs)
                                  else     build (\c n -> foldr (filterFB c p) n xs)

  build (\c n -> foldr (filterFB c p) n xs)
    = {inline} foldr (filterFB (:) p) [] xs
    = {RULE}   filter p xs

And we are in an infinite loop again, except that this time the loop is producing an
infinitely large *term* (an unrolling of filter) and so the simplifier finally
dies with "ticks exhausted"

SOLUTION: we treat the rule "filterList" as an extra rule for 'filterFB'
because it mentions 'filterFB' on the LHS.  This is the Main Invariant
in Note [IMP-RULES: local rules for imported functions].

So, during loop-breaker analysis:

- for each active RULE for a local function 'f' we add an edge between
  'f' and the local FVs of the rule RHS

- for each active RULE for an *imported* function we add dependency
  edges between the *local* FVS of the rule LHS and the *local* FVS of
  the rule RHS.

Even with this extra hack we aren't always going to get things
right. For example, it might be that the rule LHS mentions an imported
Id, and another module has a RULE that can rewrite that imported Id to
one of our local Ids.

Note [Plugin rules]
~~~~~~~~~~~~~~~~~~~
Conal Elliott (#11651) built a GHC plugin that added some
BuiltinRules (for imported Ids) to the mg_rules field of ModGuts, to
do some domain-specific transformations that could not be expressed
with an ordinary pattern-matching CoreRule.  But then we can't extract
the dependencies (in imp_rule_edges) from ru_rhs etc, because a
BuiltinRule doesn't have any of that stuff.

So we simply assume that BuiltinRules have no dependencies, and filter
them out from the imp_rule_edges comprehension.

Note [Glomming]
~~~~~~~~~~~~~~~
RULES for imported Ids can make something at the top refer to
something at the bottom:

        foo = ...(B.f @Int)...
        $sf = blah
        RULE:  B.f @Int = $sf

Applying this rule makes foo refer to $sf, although foo doesn't appear to
depend on $sf.  (And, as in Note [Rules for imported functions], the
dependency might be more indirect. For example, foo might mention C.t
rather than B.f, where C.t eventually inlines to B.f.)

NOTICE that this cannot happen for rules whose head is a
locally-defined function, because we accurately track dependencies
through RULES.  It only happens for rules whose head is an imported
function (B.f in the example above).

Solution:
  - When simplifying, bring all top level identifiers into
    scope at the start, ignoring the Rec/NonRec structure, so
    that when 'h' pops up in f's rhs, we find it in the in-scope set
    (as the simplifier generally expects). This happens in simplTopBinds.

  - In the occurrence analyser, if there are any out-of-scope
    occurrences that pop out of the top, which will happen after
    firing the rule:      f = \x -> h x
                          h = \y -> 3
    then just glom all the bindings into a single Rec, so that
    the *next* iteration of the occurrence analyser will sort
    them all out.   This part happens in occurAnalysePgm.
-}

{-
************************************************************************
*                                                                      *
                Bindings
*                                                                      *
************************************************************************

Note [Recursive bindings: the grand plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Loop breaking is surprisingly subtle.  First read the section 4 of
"Secrets of the GHC inliner".  This describes our basic plan.  We
avoid infinite inlinings by choosing loop breakers, and ensuring that
a loop breaker cuts each loop.

See also Note [Inlining and hs-boot files] in GHC.Core.ToIface, which
deals with a closely related source of infinite loops.

When we come across a binding group
  Rec { x1 = r1; ...; xn = rn }
we treat it like this (occAnalRecBind):

1. Note [Forming Rec groups]
   Occurrence-analyse each right hand side, and build a
   "Details" for each binding to capture the results.
   Wrap the details in a LetrecNode, ready for SCC analysis.
   All this is done by makeNode.

   The edges of this graph are the "scope edges".

2. Do SCC-analysis on these Nodes:
   - Each CyclicSCC will become a new Rec
   - Each AcyclicSCC will become a new NonRec

   The key property is that every free variable of a binding is
   accounted for by the scope edges, so that when we are done
   everything is still in scope.

3. For each AcyclicSCC, just make a NonRec binding.

4. For each CyclicSCC of the scope-edge SCC-analysis in (2), we
   identify suitable loop-breakers to ensure that inlining terminates.
   This is done by occAnalRec.

   To do so, form the loop-breaker graph, do SCC analysis. For each
   CyclicSCC we choose a loop breaker, delete all edges to that node,
   re-analyse the SCC, and iterate. See Note [Choosing loop breakers]
   for the details


Note [Dead code]
~~~~~~~~~~~~~~~~
Dropping dead code for a cyclic Strongly Connected Component is done
in a very simple way:

        the entire SCC is dropped if none of its binders are mentioned
        in the body; otherwise the whole thing is kept.

The key observation is that dead code elimination happens after
dependency analysis: so 'occAnalBind' processes SCCs instead of the
original term's binding groups.

Thus 'occAnalBind' does indeed drop 'f' in an example like

        letrec f = ...g...
               g = ...(...g...)...
        in
           ...g...

when 'g' no longer uses 'f' at all (eg 'f' does not occur in a RULE in
'g'). 'occAnalBind' first consumes 'CyclicSCC g' and then it consumes
'AcyclicSCC f', where 'body_usage' won't contain 'f'.

Note [Forming Rec groups]
~~~~~~~~~~~~~~~~~~~~~~~~~
The key point about the "Forming Rec groups" step is that it /preserves
scoping/.  If 'x' is mentioned, it had better be bound somewhere.  So if
we start with
  Rec { f = ...h...
      ; g = ...f...
      ; h = ...f... }
we can split into SCCs
  Rec { f = ...h...
      ; h = ..f... }
  NonRec { g = ...f... }

We put bindings {f = ef; g = eg } in a Rec group if "f uses g" and "g
uses f", no matter how indirectly.  We do a SCC analysis with an edge
f -> g if "f mentions g". That is, g is free in:
  a) the rhs 'ef'
  b) or the RHS of a rule for f, whether active or inactive
       Note [Rules are extra RHSs]
  c) or the LHS or a rule for f, whether active or inactive
       Note [Rule dependency info]
  d) the RHS of an /active/ local IMP-RULE
       Note [IMP-RULES: local rules for imported functions]

(b) and (c) apply regardless of the activation of the RULE, because even if
the rule is inactive its free variables must be bound.  But (d) doesn't need
to worry about this because IMP-RULES are always notionally at the bottom
of the file.

  * Note [Rules are extra RHSs]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    A RULE for 'f' is like an extra RHS for 'f'. That way the "parent"
    keeps the specialised "children" alive.  If the parent dies
    (because it isn't referenced any more), then the children will die
    too (unless they are already referenced directly).

    So in Example [eftInt], eftInt and eftIntFB will be put in the
    same Rec, even though their 'main' RHSs are both non-recursive.

    We must also include inactive rules, so that their free vars
    remain in scope.

  * Note [Rule dependency info]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    The VarSet in a RuleInfo is used for dependency analysis in the
    occurrence analyser.  We must track free vars in *both* lhs and rhs.
    Hence use of idRuleVars, rather than idRuleRhsVars in occAnalBind.
    Why both? Consider
        x = y
        RULE f x = v+4
    Then if we substitute y for x, we'd better do so in the
    rule's LHS too, so we'd better ensure the RULE appears to mention 'x'
    as well as 'v'

  * Note [Rules are visible in their own rec group]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    We want the rules for 'f' to be visible in f's right-hand side.
    And we'd like them to be visible in other functions in f's Rec
    group.  E.g. in Note [Specialisation rules] we want f' rule
    to be visible in both f's RHS, and fs's RHS.

    This means that we must simplify the RULEs first, before looking
    at any of the definitions.  This is done by Simplify.simplRecBind,
    when it calls addLetIdInfo.

Note [Stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~
None of the above stuff about RULES applies to a stable unfolding
stored in a CoreUnfolding.  The unfolding, if any, is simplified
at the same time as the regular RHS of the function (ie *not* like
Note [Rules are visible in their own rec group]), so it should be
treated *exactly* like an extra RHS.

Or, rather, when computing loop-breaker edges,
  * If f has an INLINE pragma, and it is active, we treat the
    INLINE rhs as f's rhs
  * If it's inactive, we treat f as having no rhs
  * If it has no INLINE pragma, we look at f's actual rhs


There is a danger that we'll be sub-optimal if we see this
     f = ...f...
     [INLINE f = ..no f...]
where f is recursive, but the INLINE is not. This can just about
happen with a sufficiently odd set of rules; eg

        foo :: Int -> Int
        {-# INLINE [1] foo #-}
        foo x = x+1

        bar :: Int -> Int
        {-# INLINE [1] bar #-}
        bar x = foo x + 1

        {-# RULES "foo" [~1] forall x. foo x = bar x #-}

Here the RULE makes bar recursive; but it's INLINE pragma remains
non-recursive. It's tempting to then say that 'bar' should not be
a loop breaker, but an attempt to do so goes wrong in two ways:
   a) We may get
         $df = ...$cfoo...
         $cfoo = ...$df....
         [INLINE $cfoo = ...no-$df...]
      But we want $cfoo to depend on $df explicitly so that we
      put the bindings in the right order to inline $df in $cfoo
      and perhaps break the loop altogether.  (Maybe this
   b)


Example [eftInt]
~~~~~~~~~~~~~~~
Example (from GHC.Enum):

  eftInt :: Int# -> Int# -> [Int]
  eftInt x y = ...(non-recursive)...

  {-# INLINE [0] eftIntFB #-}
  eftIntFB :: (Int -> r -> r) -> r -> Int# -> Int# -> r
  eftIntFB c n x y = ...(non-recursive)...

  {-# RULES
  "eftInt"  [~1] forall x y. eftInt x y = build (\ c n -> eftIntFB c n x y)
  "eftIntList"  [1] eftIntFB  (:) [] = eftInt
   #-}

Note [Specialisation rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this group, which is typical of what SpecConstr builds:

   fs a = ....f (C a)....
   f  x = ....f (C a)....
   {-# RULE f (C a) = fs a #-}

So 'f' and 'fs' are in the same Rec group (since f refers to fs via its RULE).

But watch out!  If 'fs' is not chosen as a loop breaker, we may get an infinite loop:
  - the RULE is applied in f's RHS (see Note [Self-recursive rules] in GHC.Core.Opt.Simplify
  - fs is inlined (say it's small)
  - now there's another opportunity to apply the RULE

This showed up when compiling Control.Concurrent.Chan.getChanContents.
Hence the transitive rule_fv_env stuff described in
Note [Rules and  loop breakers].

------------------------------------------------------------
Note [Finding join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~
It's the occurrence analyser's job to find bindings that we can turn into join
points, but it doesn't perform that transformation right away. Rather, it marks
the eligible bindings as part of their occurrence data, leaving it to the
simplifier (or to simpleOptPgm) to actually change the binder's 'IdDetails'.
The simplifier then eta-expands the RHS if needed and then updates the
occurrence sites. Dividing the work this way means that the occurrence analyser
still only takes one pass, yet one can always tell the difference between a
function call and a jump by looking at the occurrence (because the same pass
changes the 'IdDetails' and propagates the binders to their occurrence sites).

To track potential join points, we use the 'occ_tail' field of OccInfo. A value
of `AlwaysTailCalled n` indicates that every occurrence of the variable is a
tail call with `n` arguments (counting both value and type arguments). Otherwise
'occ_tail' will be 'NoTailCallInfo'. The tail call info flows bottom-up with the
rest of 'OccInfo' until it goes on the binder.

Note [Join points and unfoldings/rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   let j2 y = blah
   let j x = j2 (x+x)
       {-# INLINE [2] j #-}
   in case e of { A -> j 1; B -> ...; C -> j 2 }

Before j is inlined, we'll have occurrences of j2 in
both j's RHS and in its stable unfolding.  We want to discover
j2 as a join point.  So we must do the adjustRhsUsage thing
on j's RHS.  That's why we pass mb_join_arity to calcUnfolding.

Same with rules. Suppose we have:

  let j :: Int -> Int
      j y = 2 * y
  let k :: Int -> Int -> Int
      {-# RULES "SPEC k 0" k 0 y = j y #-}
      k x y = x + 2 * y
  in case e of { A -> k 1 2; B -> k 3 5; C -> blah }

We identify k as a join point, and we want j to be a join point too.
Without the RULE it would be, and we don't want the RULE to mess it
up.  So provided the join-point arity of k matches the args of the
rule we can allow the tail-call info from the RHS of the rule to
propagate.

* Wrinkle for Rec case. In the recursive case we don't know the
  join-point arity in advance, when calling occAnalUnfolding and
  occAnalRules.  (See makeNode.)  We don't want to pass Nothing,
  because then a recursive joinrec might lose its join-poin-hood
  when SpecConstr adds a RULE.  So we just make do with the
  *current* join-poin-hood, stored in the Id.

  In the non-recursive case things are simple: see occAnalNonRecBind

* Wrinkle for RULES.  Suppose the example was a bit different:
      let j :: Int -> Int
          j y = 2 * y
          k :: Int -> Int -> Int
          {-# RULES "SPEC k 0" k 0 = j #-}
          k x y = x + 2 * y
      in ...
  If we eta-expanded the rule all would be well, but as it stands the
  one arg of the rule don't match the join-point arity of 2.

  Conceivably we could notice that a potential join point would have
  an "undersaturated" rule and account for it. This would mean we
  could make something that's been specialised a join point, for
  instance. But local bindings are rarely specialised, and being
  overly cautious about rules only costs us anything when, for some `j`:

  * Before specialisation, `j` has non-tail calls, so it can't be a join point.
  * During specialisation, `j` gets specialised and thus acquires rules.
  * Sometime afterward, the non-tail calls to `j` disappear (as dead code, say),
    and so now `j` *could* become a join point.

  This appears to be very rare in practice. TODO Perhaps we should gather
  statistics to be sure.

Note [Unfoldings and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We assume that anything in an unfolding occurs multiple times, since
unfoldings are often copied (that's the whole point!). But we still
need to track tail calls for the purpose of finding join points.


------------------------------------------------------------
Note [Adjusting right-hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's a bit of a dance we need to do after analysing a lambda expression or
a right-hand side. In particular, we need to

  a) call 'markAllInsideLam' *unless* the binding is for a thunk, a one-shot
     lambda, or a non-recursive join point; and
  b) call 'markAllNonTail' *unless* the binding is for a join point.

Some examples, with how the free occurrences in e (assumed not to be a value
lambda) get marked:

                             inside lam    non-tail-called
  ------------------------------------------------------------
  let x = e                  No            Yes
  let f = \x -> e            Yes           Yes
  let f = \x{OneShot} -> e   No            Yes
  \x -> e                    Yes           Yes
  join j x = e               No            No
  joinrec j x = e            Yes           No

There are a few other caveats; most importantly, if we're marking a binding as
'AlwaysTailCalled', it's *going* to be a join point, so we treat it as one so
that the effect cascades properly. Consequently, at the time the RHS is
analysed, we won't know what adjustments to make; thus 'occAnalLamOrRhs' must
return the unadjusted 'UsageDetails', to be adjusted by 'adjustRhsUsage' once
join-point-hood has been decided.

Thus the overall sequence taking place in 'occAnalNonRecBind' and
'occAnalRecBind' is as follows:

  1. Call 'occAnalLamOrRhs' to find usage information for the RHS.
  2. Call 'tagNonRecBinder' or 'tagRecBinders', which decides whether to make
     the binding a join point.
  3. Call 'adjustRhsUsage' accordingly. (Done as part of 'tagRecBinders' when
     recursive.)

(In the recursive case, this logic is spread between 'makeNode' and
'occAnalRec'.)
-}


data WithUsageDetails a = WithUsageDetails !UsageDetails !a

------------------------------------------------------------------
--                 occAnalBind
------------------------------------------------------------------

occAnalBind :: OccEnv           -- The incoming OccEnv
            -> TopLevelFlag
            -> ImpRuleEdges
            -> CoreBind
            -> UsageDetails             -- Usage details of scope
            -> WithUsageDetails [CoreBind] -- Of the whole let(rec)

occAnalBind !env lvl top_env (NonRec binder rhs) body_usage
  = occAnalNonRecBind env lvl top_env binder rhs body_usage
occAnalBind env lvl top_env (Rec pairs) body_usage
  = occAnalRecBind env lvl top_env pairs body_usage

-----------------
occAnalNonRecBind :: OccEnv -> TopLevelFlag -> ImpRuleEdges -> Var -> CoreExpr
                  -> UsageDetails -> WithUsageDetails [CoreBind]
occAnalNonRecBind !env lvl imp_rule_edges bndr rhs body_usage
  | isTyVar bndr      -- A type let; we don't gather usage info
  = WithUsageDetails body_usage [NonRec bndr rhs]

  | not (bndr `usedIn` body_usage)    -- It's not mentioned
  = WithUsageDetails body_usage []

  | otherwise                   -- It's mentioned in the body
  = WithUsageDetails (body_usage' `andUDs` rhs_usage) [NonRec final_bndr rhs']
  where
    (body_usage', tagged_bndr) = tagNonRecBinder lvl body_usage bndr
    final_bndr = tagged_bndr `setIdUnfolding` unf'
                             `setIdSpecialisation` mkRuleInfo rules'
    rhs_usage = rhs_uds `andUDs` unf_uds `andUDs` rule_uds

    -- Get the join info from the *new* decision
    -- See Note [Join points and unfoldings/rules]
    mb_join_arity = willBeJoinId_maybe tagged_bndr
    is_join_point = isJust mb_join_arity

    --------- Right hand side ---------
    env1 | is_join_point    = env  -- See Note [Join point RHSs]
         | certainly_inline = env  -- See Note [Cascading inlines]
         | otherwise        = rhsCtxt env

    -- See Note [Sources of one-shot information]
    rhs_env = env1 { occ_one_shots = argOneShots dmd }
    (WithUsageDetails rhs_uds rhs') = occAnalRhs rhs_env NonRecursive mb_join_arity rhs

    --------- Unfolding ---------
    -- See Note [Unfoldings and join points]
    unf | isId bndr = idUnfolding bndr
        | otherwise = NoUnfolding
    (WithUsageDetails unf_uds unf') = occAnalUnfolding rhs_env NonRecursive mb_join_arity unf

    --------- Rules ---------
    -- See Note [Rules are extra RHSs] and Note [Rule dependency info]
    rules_w_uds  = occAnalRules rhs_env mb_join_arity bndr
    rules'       = map fstOf3 rules_w_uds
    imp_rule_uds = impRulesScopeUsage (lookupImpRules imp_rule_edges bndr)
         -- imp_rule_uds: consider
         --     h = ...
         --     g = ...
         --     RULE map g = h
         -- Then we want to ensure that h is in scope everwhere
         -- that g is (since the RULE might turn g into h), so
         -- we make g mention h.

    rule_uds = foldr add_rule_uds imp_rule_uds rules_w_uds
    add_rule_uds (_, l, r) uds = l `andUDs` r `andUDs` uds

    ----------
    occ = idOccInfo tagged_bndr
    certainly_inline -- See Note [Cascading inlines]
      = case occ of
          OneOcc { occ_in_lam = NotInsideLam, occ_n_br = 1 }
            -> active && not_stable
          _ -> False

    dmd        = idDemandInfo bndr
    active     = isAlwaysActive (idInlineActivation bndr)
    not_stable = not (isStableUnfolding (idUnfolding bndr))

-----------------
occAnalRecBind :: OccEnv -> TopLevelFlag -> ImpRuleEdges -> [(Var,CoreExpr)]
               -> UsageDetails -> WithUsageDetails [CoreBind]
-- For a recursive group, we
--      * occ-analyse all the RHSs
--      * compute strongly-connected components
--      * feed those components to occAnalRec
-- See Note [Recursive bindings: the grand plan]
occAnalRecBind !env lvl imp_rule_edges pairs body_usage
  = foldr (occAnalRec rhs_env lvl) (WithUsageDetails body_usage []) sccs
  where
    sccs :: [SCC Details]
    sccs = {-# SCC "occAnalBind.scc" #-}
           stronglyConnCompFromEdgedVerticesUniq nodes

    nodes :: [LetrecNode]
    nodes = {-# SCC "occAnalBind.assoc" #-}
            map (makeNode rhs_env imp_rule_edges bndr_set) pairs

    bndrs    = map fst pairs
    bndr_set = mkVarSet bndrs
    rhs_env  = env `addInScope` bndrs


-----------------------------
occAnalRec :: OccEnv -> TopLevelFlag
           -> SCC Details
           -> WithUsageDetails [CoreBind]
           -> WithUsageDetails [CoreBind]

        -- The NonRec case is just like a Let (NonRec ...) above
occAnalRec !_ lvl (AcyclicSCC (ND { nd_bndr = bndr, nd_rhs = rhs
                                 , nd_uds = rhs_uds, nd_rhs_bndrs = rhs_bndrs }))
           (WithUsageDetails body_uds binds)
  | not (bndr `usedIn` body_uds)
  = WithUsageDetails body_uds binds -- See Note [Dead code]

  | otherwise                   -- It's mentioned in the body
  = WithUsageDetails (body_uds' `andUDs` rhs_uds')
                     (NonRec tagged_bndr rhs : binds)
  where
    (body_uds', tagged_bndr) = tagNonRecBinder lvl body_uds bndr
    rhs_uds'   = adjustRhsUsage NonRecursive (willBeJoinId_maybe tagged_bndr)
                                rhs_bndrs rhs_uds

        -- The Rec case is the interesting one
        -- See Note [Recursive bindings: the grand plan]
        -- See Note [Loop breaking]
occAnalRec env lvl (CyclicSCC details_s) (WithUsageDetails body_uds binds)
  | not (any (`usedIn` body_uds) bndrs) -- NB: look at body_uds, not total_uds
  = WithUsageDetails body_uds binds     -- See Note [Dead code]

  | otherwise   -- At this point we always build a single Rec
  = -- pprTrace "occAnalRec" (ppr loop_breaker_nodes)
    WithUsageDetails final_uds (Rec pairs : binds)

  where
    bndrs      = map nd_bndr details_s
    all_simple = all nd_simple details_s

    ------------------------------
    -- Make the nodes for the loop-breaker analysis
    -- See Note [Choosing loop breakers] for loop_breaker_nodes
    final_uds :: UsageDetails
    loop_breaker_nodes :: [LetrecNode]
    (WithUsageDetails final_uds loop_breaker_nodes) = mkLoopBreakerNodes env lvl body_uds details_s

    ------------------------------
    active_rule_fvs :: VarSet
    active_rule_fvs = mapUnionVarSet nd_active_rule_fvs details_s

    ---------------------------
    -- Now reconstruct the cycle
    pairs :: [(Id,CoreExpr)]
    pairs | all_simple = reOrderNodes   0 active_rule_fvs loop_breaker_nodes []
          | otherwise  = loopBreakNodes 0 active_rule_fvs loop_breaker_nodes []
          -- In the common case when all are "simple" (no rules at all)
          -- the loop_breaker_nodes will include all the scope edges
          -- so a SCC computation would yield a single CyclicSCC result;
          -- and reOrderNodes deals with exactly that case.
          -- Saves a SCC analysis in a common case


{- *********************************************************************
*                                                                      *
                Loop breaking
*                                                                      *
********************************************************************* -}

{- Note [Choosing loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Step 4 in Note [Recursive bindings: the grand plan]), occAnalRec does
loop-breaking on each CyclicSCC of the original program:

* mkLoopBreakerNodes: Form the loop-breaker graph for that CyclicSCC

* loopBreakNodes: Do SCC analysis on it

* reOrderNodes: For each CyclicSCC, pick a loop breaker
    * Delete edges to that loop breaker
    * Do another SCC analysis on that reduced SCC
    * Repeat

To form the loop-breaker graph, we construct a new set of Nodes, the
"loop-breaker nodes", with the same details but different edges, the
"loop-breaker edges".  The loop-breaker nodes have both more and fewer
dependencies than the scope edges:

  More edges:
     If f calls g, and g has an active rule that mentions h then
     we add an edge from f -> h.  See Note [Rules and loop breakers].

  Fewer edges: we only include dependencies
     * only on /active/ rules,
     * on rule /RHSs/ (not LHSs)

The scope edges, by contrast, must be much more inclusive.

The nd_simple flag tracks the common case when a binding has no RULES
at all, in which case the loop-breaker edges will be identical to the
scope edges.

Note that in Example [eftInt], *neither* eftInt *nor* eftIntFB is
chosen as a loop breaker, because their RHSs don't mention each other.
And indeed both can be inlined safely.

Note [inl_fvs]
~~~~~~~~~~~~~~
Note that the loop-breaker graph includes edges for occurrences in
/both/ the RHS /and/ the stable unfolding.  Consider this, which actually
occurred when compiling BooleanFormula.hs in GHC:

  Rec { lvl1 = go
      ; lvl2[StableUnf = go] = lvl1
      ; go = ...go...lvl2... }

From the point of view of infinite inlining, we need only these edges:
   lvl1 :-> go
   lvl2 :-> go       -- The RHS lvl1 will never be used for inlining
   go   :-> go, lvl2

But the danger is that, lacking any edge to lvl1, we'll put it at the
end thus
  Rec { lvl2[ StableUnf = go] = lvl1
      ; go[LoopBreaker] = ...go...lvl2... }
      ; lvl1[Occ=Once]  = go }

And now the Simplifer will try to use PreInlineUnconditionally on lvl1
(which occurs just once), but because it is last we won't actually
substitute in lvl2.  Sigh.

To avoid this possiblity, we include edges from lvl2 to /both/ its
stable unfolding /and/ its RHS.  Hence the defn of inl_fvs in
makeNode.  Maybe we could be more clever, but it's very much a corner
case.

Note [Weak loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~
There is a last nasty wrinkle.  Suppose we have

    Rec { f = f_rhs
          RULE f [] = g

          h = h_rhs
          g = h
          ...more...
    }

Remember that we simplify the RULES before any RHS (see Note
[Rules are visible in their own rec group] above).

So we must *not* postInlineUnconditionally 'g', even though
its RHS turns out to be trivial.  (I'm assuming that 'g' is
not chosen as a loop breaker.)  Why not?  Because then we
drop the binding for 'g', which leaves it out of scope in the
RULE!

Here's a somewhat different example of the same thing
    Rec { q = r
        ; r = ...p...
        ; p = p_rhs
          RULE p [] = q }
Here the RULE is "below" q, but we *still* can't postInlineUnconditionally
q, because the RULE for p is active throughout.  So the RHS of r
might rewrite to     r = ...q...
So q must remain in scope in the output program!

We "solve" this by:

    Make q a "weak" loop breaker (OccInfo = IAmLoopBreaker True)
    iff q is a mentioned in the RHS of an active RULE in the Rec group

A normal "strong" loop breaker has IAmLoopBreaker False.  So:

                                Inline  postInlineUnconditionally
strong   IAmLoopBreaker False    no      no
weak     IAmLoopBreaker True     yes     no
         other                   yes     yes

The **sole** reason for this kind of loop breaker is so that
postInlineUnconditionally does not fire.  Ugh.

Annoyingly, since we simplify the rules *first* we'll never inline
q into p's RULE.  That trivial binding for q will hang around until
we discard the rule.  Yuk.  But it's rare.

 Note [Rules and loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we form the loop-breaker graph (Step 4 in Note [Recursive
bindings: the grand plan]), we must be careful about RULEs.

For a start, we want a loop breaker to cut every cycle, so inactive
rules play no part; we need only consider /active/ rules.
See Note [Finding rule RHS free vars]

The second point is more subtle.  A RULE is like an equation for
'f' that is *always* inlined if it is applicable.  We do *not* disable
rules for loop-breakers.  It's up to whoever makes the rules to make
sure that the rules themselves always terminate.  See Note [Rules for
recursive functions] in GHC.Core.Opt.Simplify

Hence, if
    f's RHS (or its stable unfolding if it has one) mentions g, and
    g has a RULE that mentions h, and
    h has a RULE that mentions f

then we *must* choose f to be a loop breaker.  Example: see Note
[Specialisation rules]. So out plan is this:

   Take the free variables of f's RHS, and augment it with all the
   variables reachable by a transitive sequence RULES from those
   starting points.

That is the whole reason for computing rule_fv_env in mkLoopBreakerNodes.
Wrinkles:

* We only consider /active/ rules. See Note [Finding rule RHS free vars]

* We need only consider free vars that are also binders in this Rec
  group.  See also Note [Finding rule RHS free vars]

* We only consider variables free in the *RHS* of the rule, in
  contrast to the way we build the Rec group in the first place (Note
  [Rule dependency info])

* Why "transitive sequence of rules"?  Because active rules apply
  unconditionally, without checking loop-breaker-ness.
 See Note [Loop breaker dependencies].

Note [Finding rule RHS free vars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this real example from Data Parallel Haskell
     tagZero :: Array Int -> Array Tag
     {-# INLINE [1] tagZeroes #-}
     tagZero xs = pmap (\x -> fromBool (x==0)) xs

     {-# RULES "tagZero" [~1] forall xs n.
         pmap fromBool <blah blah> = tagZero xs #-}
So tagZero's RHS mentions pmap, and pmap's RULE mentions tagZero.
However, tagZero can only be inlined in phase 1 and later, while
the RULE is only active *before* phase 1.  So there's no problem.

To make this work, we look for the RHS free vars only for
*active* rules. That's the reason for the occ_rule_act field
of the OccEnv.

Note [loopBreakNodes]
~~~~~~~~~~~~~~~~~~~~~
loopBreakNodes is applied to the list of nodes for a cyclic strongly
connected component (there's guaranteed to be a cycle).  It returns
the same nodes, but
        a) in a better order,
        b) with some of the Ids having a IAmALoopBreaker pragma

The "loop-breaker" Ids are sufficient to break all cycles in the SCC.  This means
that the simplifier can guarantee not to loop provided it never records an inlining
for these no-inline guys.

Furthermore, the order of the binds is such that if we neglect dependencies
on the no-inline Ids then the binds are topologically sorted.  This means
that the simplifier will generally do a good job if it works from top bottom,
recording inlinings for any Ids which aren't marked as "no-inline" as it goes.
-}

type Binding = (Id,CoreExpr)

-- See Note [loopBreakNodes]
loopBreakNodes :: Int
               -> VarSet        -- Binders whose dependencies may be "missing"
                                -- See Note [Weak loop breakers]
               -> [LetrecNode]
               -> [Binding]             -- Append these to the end
               -> [Binding]

-- Return the bindings sorted into a plausible order, and marked with loop breakers.
-- See Note [loopBreakNodes]
loopBreakNodes depth weak_fvs nodes binds
  = -- pprTrace "loopBreakNodes" (ppr nodes) $
    go (stronglyConnCompFromEdgedVerticesUniqR nodes)
  where
    go []         = binds
    go (scc:sccs) = loop_break_scc scc (go sccs)

    loop_break_scc scc binds
      = case scc of
          AcyclicSCC node  -> nodeBinding (mk_non_loop_breaker weak_fvs) node : binds
          CyclicSCC nodes  -> reOrderNodes depth weak_fvs nodes binds

----------------------------------
reOrderNodes :: Int -> VarSet -> [LetrecNode] -> [Binding] -> [Binding]
    -- Choose a loop breaker, mark it no-inline,
    -- and call loopBreakNodes on the rest
reOrderNodes _ _ []     _     = panic "reOrderNodes"
reOrderNodes _ _ [node] binds = nodeBinding mk_loop_breaker node : binds
reOrderNodes depth weak_fvs (node : nodes) binds
  = -- pprTrace "reOrderNodes" (vcat [ text "unchosen" <+> ppr unchosen
    --                               , text "chosen" <+> ppr chosen_nodes ]) $
    loopBreakNodes new_depth weak_fvs unchosen $
    (map (nodeBinding mk_loop_breaker) chosen_nodes ++ binds)
  where
    (chosen_nodes, unchosen) = chooseLoopBreaker approximate_lb
                                                 (nd_score (node_payload node))
                                                 [node] [] nodes

    approximate_lb = depth >= 2
    new_depth | approximate_lb = 0
              | otherwise      = depth+1
        -- After two iterations (d=0, d=1) give up
        -- and approximate, returning to d=0

nodeBinding :: (Id -> Id) -> LetrecNode -> Binding
nodeBinding set_id_occ (node_payload -> ND { nd_bndr = bndr, nd_rhs = rhs})
  = (set_id_occ bndr, rhs)

mk_loop_breaker :: Id -> Id
mk_loop_breaker bndr
  = bndr `setIdOccInfo` occ'
  where
    occ'      = strongLoopBreaker { occ_tail = tail_info }
    tail_info = tailCallInfo (idOccInfo bndr)

mk_non_loop_breaker :: VarSet -> Id -> Id
-- See Note [Weak loop breakers]
mk_non_loop_breaker weak_fvs bndr
  | bndr `elemVarSet` weak_fvs = setIdOccInfo bndr occ'
  | otherwise                  = bndr
  where
    occ'      = weakLoopBreaker { occ_tail = tail_info }
    tail_info = tailCallInfo (idOccInfo bndr)

----------------------------------
chooseLoopBreaker :: Bool             -- True <=> Too many iterations,
                                      --          so approximate
                  -> NodeScore            -- Best score so far
                  -> [LetrecNode]       -- Nodes with this score
                  -> [LetrecNode]       -- Nodes with higher scores
                  -> [LetrecNode]       -- Unprocessed nodes
                  -> ([LetrecNode], [LetrecNode])
    -- This loop looks for the bind with the lowest score
    -- to pick as the loop  breaker.  The rest accumulate in
chooseLoopBreaker _ _ loop_nodes acc []
  = (loop_nodes, acc)        -- Done

    -- If approximate_loop_breaker is True, we pick *all*
    -- nodes with lowest score, else just one
    -- See Note [Complexity of loop breaking]
chooseLoopBreaker approx_lb loop_sc loop_nodes acc (node : nodes)
  | approx_lb
  , rank sc == rank loop_sc
  = chooseLoopBreaker approx_lb loop_sc (node : loop_nodes) acc nodes

  | sc `betterLB` loop_sc  -- Better score so pick this new one
  = chooseLoopBreaker approx_lb sc [node] (loop_nodes ++ acc) nodes

  | otherwise              -- Worse score so don't pick it
  = chooseLoopBreaker approx_lb loop_sc loop_nodes (node : acc) nodes
  where
    sc = nd_score (node_payload node)

{-
Note [Complexity of loop breaking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The loop-breaking algorithm knocks out one binder at a time, and
performs a new SCC analysis on the remaining binders.  That can
behave very badly in tightly-coupled groups of bindings; in the
worst case it can be (N**2)*log N, because it does a full SCC
on N, then N-1, then N-2 and so on.

To avoid this, we switch plans after 2 (or whatever) attempts:
  Plan A: pick one binder with the lowest score, make it
          a loop breaker, and try again
  Plan B: pick *all* binders with the lowest score, make them
          all loop breakers, and try again
Since there are only a small finite number of scores, this will
terminate in a constant number of iterations, rather than O(N)
iterations.

You might thing that it's very unlikely, but RULES make it much
more likely.  Here's a real example from #1969:
  Rec { $dm = \d.\x. op d
        {-# RULES forall d. $dm Int d  = $s$dm1
                  forall d. $dm Bool d = $s$dm2 #-}

        dInt = MkD .... opInt ...
        dInt = MkD .... opBool ...
        opInt  = $dm dInt
        opBool = $dm dBool

        $s$dm1 = \x. op dInt
        $s$dm2 = \x. op dBool }
The RULES stuff means that we can't choose $dm as a loop breaker
(Note [Choosing loop breakers]), so we must choose at least (say)
opInt *and* opBool, and so on.  The number of loop breakders is
linear in the number of instance declarations.

Note [Loop breakers and INLINE/INLINABLE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Avoid choosing a function with an INLINE pramga as the loop breaker!
If such a function is mutually-recursive with a non-INLINE thing,
then the latter should be the loop-breaker.

It's vital to distinguish between INLINE and INLINABLE (the
Bool returned by hasStableCoreUnfolding_maybe).  If we start with
   Rec { {-# INLINABLE f #-}
         f x = ...f... }
and then worker/wrapper it through strictness analysis, we'll get
   Rec { {-# INLINABLE $wf #-}
         $wf p q = let x = (p,q) in ...f...

         {-# INLINE f #-}
         f x = case x of (p,q) -> $wf p q }

Now it is vital that we choose $wf as the loop breaker, so we can
inline 'f' in '$wf'.

Note [DFuns should not be loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's particularly bad to make a DFun into a loop breaker.  See
Note [How instance declarations are translated] in GHC.Tc.TyCl.Instance

We give DFuns a higher score than ordinary CONLIKE things because
if there's a choice we want the DFun to be the non-loop breaker. Eg

rec { sc = /\ a \$dC. $fBWrap (T a) ($fCT @ a $dC)

      $fCT :: forall a_afE. (Roman.C a_afE) => Roman.C (Roman.T a_afE)
      {-# DFUN #-}
      $fCT = /\a \$dC. MkD (T a) ((sc @ a $dC) |> blah) ($ctoF @ a $dC)
    }

Here 'sc' (the superclass) looks CONLIKE, but we'll never get to it
if we can't unravel the DFun first.

Note [Constructor applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's really really important to inline dictionaries.  Real
example (the Enum Ordering instance from GHC.Base):

     rec     f = \ x -> case d of (p,q,r) -> p x
             g = \ x -> case d of (p,q,r) -> q x
             d = (v, f, g)

Here, f and g occur just once; but we can't inline them into d.
On the other hand we *could* simplify those case expressions if
we didn't stupidly choose d as the loop breaker.
But we won't because constructor args are marked "Many".
Inlining dictionaries is really essential to unravelling
the loops in static numeric dictionaries, see GHC.Float.

Note [Closure conversion]
~~~~~~~~~~~~~~~~~~~~~~~~~
We treat (\x. C p q) as a high-score candidate in the letrec scoring algorithm.
The immediate motivation came from the result of a closure-conversion transformation
which generated code like this:

    data Clo a b = forall c. Clo (c -> a -> b) c

    ($:) :: Clo a b -> a -> b
    Clo f env $: x = f env x

    rec { plus = Clo plus1 ()

        ; plus1 _ n = Clo plus2 n

        ; plus2 Zero     n = n
        ; plus2 (Succ m) n = Succ (plus $: m $: n) }

If we inline 'plus' and 'plus1', everything unravels nicely.  But if
we choose 'plus1' as the loop breaker (which is entirely possible
otherwise), the loop does not unravel nicely.


@occAnalUnfolding@ deals with the question of bindings where the Id is marked
by an INLINE pragma.  For these we record that anything which occurs
in its RHS occurs many times.  This pessimistically assumes that this
inlined binder also occurs many times in its scope, but if it doesn't
we'll catch it next time round.  At worst this costs an extra simplifier pass.
ToDo: try using the occurrence info for the inline'd binder.

[March 97] We do the same for atomic RHSs.  Reason: see notes with loopBreakSCC.
[June 98, SLPJ]  I've undone this change; I don't understand it.  See notes with loopBreakSCC.


************************************************************************
*                                                                      *
                   Making nodes
*                                                                      *
************************************************************************
-}

type LetrecNode = Node Unique Details  -- Node comes from Digraph
                                       -- The Unique key is gotten from the Id
data Details
  = ND { nd_bndr :: Id          -- Binder

       , nd_rhs  :: CoreExpr    -- RHS, already occ-analysed

       , nd_rhs_bndrs :: [CoreBndr] -- Outer lambdas of RHS
                                    -- INVARIANT: (nd_rhs_bndrs nd, _) ==
                                    --              collectBinders (nd_rhs nd)

       , nd_uds  :: UsageDetails  -- Usage from RHS, and RULES, and stable unfoldings
                                  -- ignoring phase (ie assuming all are active)
                                  -- See Note [Forming Rec groups]

       , nd_inl  :: IdSet       -- Free variables of the stable unfolding and the RHS
                                -- but excluding any RULES
                                -- This is the IdSet that may be used if the Id is inlined

       , nd_simple :: Bool      -- True iff this binding has no local RULES
                                -- If all nodes are simple we don't need a loop-breaker
                                -- dep-anal before reconstructing.

       , nd_active_rule_fvs :: IdSet    -- Variables bound in this Rec group that are free
                                        -- in the RHS of an active rule for this bndr

       , nd_score :: NodeScore
  }

instance Outputable Details where
   ppr nd = text "ND" <> braces
             (sep [ text "bndr =" <+> ppr (nd_bndr nd)
                  , text "uds =" <+> ppr (nd_uds nd)
                  , text "inl =" <+> ppr (nd_inl nd)
                  , text "simple =" <+> ppr (nd_simple nd)
                  , text "active_rule_fvs =" <+> ppr (nd_active_rule_fvs nd)
                  , text "score =" <+> ppr (nd_score nd)
             ])

-- The NodeScore is compared lexicographically;
--      e.g. lower rank wins regardless of size
type NodeScore = ( Int     -- Rank: lower => more likely to be picked as loop breaker
                 , Int     -- Size of rhs: higher => more likely to be picked as LB
                           -- Maxes out at maxExprSize; we just use it to prioritise
                           -- small functions
                 , Bool )  -- Was it a loop breaker before?
                           -- True => more likely to be picked
                           -- Note [Loop breakers, node scoring, and stability]

rank :: NodeScore -> Int
rank (r, _, _) = r

makeNode :: OccEnv -> ImpRuleEdges -> VarSet
         -> (Var, CoreExpr) -> LetrecNode
-- See Note [Recursive bindings: the grand plan]
makeNode !env imp_rule_edges bndr_set (bndr, rhs)
  = DigraphNode { node_payload      = details
                , node_key          = varUnique bndr
                , node_dependencies = nonDetKeysUniqSet scope_fvs }
    -- It's OK to use nonDetKeysUniqSet here as stronglyConnCompFromEdgedVerticesR
    -- is still deterministic with edges in nondeterministic order as
    -- explained in Note [Deterministic SCC] in GHC.Data.Graph.Directed.
  where
    details = ND { nd_bndr            = bndr'
                 , nd_rhs             = rhs'
                 , nd_rhs_bndrs       = bndrs'
                 , nd_uds             = scope_uds
                 , nd_inl             = inl_fvs
                 , nd_simple          = null rules_w_uds && null imp_rule_info
                 , nd_active_rule_fvs = active_rule_fvs
                 , nd_score           = pprPanic "makeNodeDetails" (ppr bndr) }

    bndr' = bndr `setIdUnfolding`      unf'
                 `setIdSpecialisation` mkRuleInfo rules'

    inl_uds = rhs_uds `andUDs` unf_uds
    scope_uds = inl_uds `andUDs` rule_uds
                   -- Note [Rules are extra RHSs]
                   -- Note [Rule dependency info]
    scope_fvs = udFreeVars bndr_set scope_uds
    -- scope_fvs: all occurrences from this binder: RHS, unfolding,
    --            and RULES, both LHS and RHS thereof, active or inactive

    inl_fvs  = udFreeVars bndr_set inl_uds
    -- inl_fvs: vars that would become free if the function was inlined.
    -- We conservatively approximate that by thefree vars from the RHS
    -- and the unfolding together.
    -- See Note [inl_fvs]

    mb_join_arity = isJoinId_maybe bndr
    -- Get join point info from the *current* decision
    -- We don't know what the new decision will be!
    -- Using the old decision at least allows us to
    -- preserve existing join point, even RULEs are added
    -- See Note [Join points and unfoldings/rules]

    --------- Right hand side ---------
    -- Constructing the edges for the main Rec computation
    -- See Note [Forming Rec groups]
    -- Do not use occAnalRhs because we don't yet know
    -- the final answer for mb_join_arity
    (bndrs, body)            = collectBinders rhs
    rhs_env                  = rhsCtxt env
    (WithUsageDetails rhs_uds (bndrs', body')) = occAnalLamOrRhs rhs_env bndrs body
    rhs'                     = mkLams bndrs' body'

    --------- Unfolding ---------
    -- See Note [Unfoldings and join points]
    unf = realIdUnfolding bndr -- realIdUnfolding: Ignore loop-breaker-ness
                               -- here because that is what we are setting!
    (WithUsageDetails unf_uds unf') = occAnalUnfolding rhs_env Recursive mb_join_arity unf

    --------- IMP-RULES --------
    is_active     = occ_rule_act env :: Activation -> Bool
    imp_rule_info = lookupImpRules imp_rule_edges bndr
    imp_rule_uds  = impRulesScopeUsage imp_rule_info
    imp_rule_fvs  = impRulesActiveFvs is_active bndr_set imp_rule_info

    --------- All rules --------
    rules_w_uds :: [(CoreRule, UsageDetails, UsageDetails)]
    rules_w_uds = occAnalRules rhs_env mb_join_arity bndr
    rules'      = map fstOf3 rules_w_uds

    rule_uds = foldr add_rule_uds imp_rule_uds rules_w_uds
    add_rule_uds (_, l, r) uds = l `andUDs` r `andUDs` uds

    active_rule_fvs = foldr add_active_rule imp_rule_fvs rules_w_uds
    add_active_rule (rule, _, rhs_uds) fvs
      | is_active (ruleActivation rule)
      = udFreeVars bndr_set rhs_uds `unionVarSet` fvs
      | otherwise
      = fvs


mkLoopBreakerNodes :: OccEnv -> TopLevelFlag
                   -> UsageDetails   -- for BODY of let
                   -> [Details]
                   -> WithUsageDetails [LetrecNode] -- adjusted
-- See Note [Choosing loop breakers]
-- This function primarily creates the Nodes for the
-- loop-breaker SCC analysis.  More specifically:
--   a) tag each binder with its occurrence info
--   b) add a NodeScore to each node
--   c) make a Node with the right dependency edges for
--      the loop-breaker SCC analysis
--   d) adjust each RHS's usage details according to
--      the binder's (new) shotness and join-point-hood
mkLoopBreakerNodes !env lvl body_uds details_s
  = WithUsageDetails final_uds (zipWithEqual "mkLoopBreakerNodes" mk_lb_node details_s bndrs')
  where
    (final_uds, bndrs')
       = tagRecBinders lvl body_uds
            [ (bndr, uds, rhs_bndrs)
            | ND { nd_bndr = bndr, nd_uds = uds, nd_rhs_bndrs = rhs_bndrs }
                 <- details_s ]

    mk_lb_node nd@(ND { nd_bndr = old_bndr, nd_inl = inl_fvs }) new_bndr
      = DigraphNode { node_payload      = new_nd
                    , node_key          = varUnique old_bndr
                    , node_dependencies = nonDetKeysUniqSet lb_deps }
              -- It's OK to use nonDetKeysUniqSet here as
              -- stronglyConnCompFromEdgedVerticesR is still deterministic with edges
              -- in nondeterministic order as explained in
              -- Note [Deterministic SCC] in GHC.Data.Graph.Directed.
      where
        new_nd = nd { nd_bndr = new_bndr, nd_score = score }
        score  = nodeScore env new_bndr lb_deps nd
        lb_deps = extendFvs_ rule_fv_env inl_fvs
        -- See Note [Loop breaker dependencies]

    rule_fv_env :: IdEnv IdSet
    -- Maps a variable f to the variables from this group
    --      reachable by a sequence of RULES starting with f
    -- Domain is *subset* of bound vars (others have no rule fvs)
    -- See Note [Finding rule RHS free vars]
    -- Why transClosureFV?  See Note [Loop breaker dependencies]
    rule_fv_env = transClosureFV $ mkVarEnv $
                  [ (b, rule_fvs)
                  | ND { nd_bndr = b, nd_active_rule_fvs = rule_fvs } <- details_s
                  , not (isEmptyVarSet rule_fvs) ]

{- Note [Loop breaker dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The loop breaker dependencies of x in a recursive
group { f1 = e1; ...; fn = en } are:

- The "inline free variables" of f: the fi free in
  f's stable unfolding and RHS; see Note [inl_fvs]

- Any fi reachable from those inline free variables by a sequence
  of RULE rewrites.  Remember, rule rewriting is not affected
  by fi being a loop breaker, so we have to take the transitive
  closure in case f is the only possible loop breaker in the loop.

  Hence rule_fv_env.  We need only account for /active/ rules.
-}

------------------------------------------
nodeScore :: OccEnv
          -> Id        -- Binder with new occ-info
          -> VarSet    -- Loop-breaker dependencies
          -> Details
          -> NodeScore
nodeScore !env new_bndr lb_deps
          (ND { nd_bndr = old_bndr, nd_rhs = bind_rhs })

  | not (isId old_bndr)     -- A type or coercion variable is never a loop breaker
  = (100, 0, False)

  | old_bndr `elemVarSet` lb_deps  -- Self-recursive things are great loop breakers
  = (0, 0, True)                   -- See Note [Self-recursion and loop breakers]

  | not (occ_unf_act env old_bndr) -- A binder whose inlining is inactive (e.g. has
  = (0, 0, True)                   -- a NOINLINE pragma) makes a great loop breaker

  | exprIsTrivial rhs
  = mk_score 10  -- Practically certain to be inlined
    -- Used to have also: && not (isExportedId bndr)
    -- But I found this sometimes cost an extra iteration when we have
    --      rec { d = (a,b); a = ...df...; b = ...df...; df = d }
    -- where df is the exported dictionary. Then df makes a really
    -- bad choice for loop breaker

  | DFunUnfolding { df_args = args } <- old_unf
    -- Never choose a DFun as a loop breaker
    -- Note [DFuns should not be loop breakers]
  = (9, length args, is_lb)

    -- Data structures are more important than INLINE pragmas
    -- so that dictionary/method recursion unravels

  | CoreUnfolding { uf_guidance = UnfWhen {} } <- old_unf
  = mk_score 6

  | is_con_app rhs   -- Data types help with cases:
  = mk_score 5       -- Note [Constructor applications]

  | isStableUnfolding old_unf
  , can_unfold
  = mk_score 3

  | isOneOcc (idOccInfo new_bndr)
  = mk_score 2  -- Likely to be inlined

  | can_unfold  -- The Id has some kind of unfolding
  = mk_score 1

  | otherwise
  = (0, 0, is_lb)

  where
    mk_score :: Int -> NodeScore
    mk_score rank = (rank, rhs_size, is_lb)

    -- is_lb: see Note [Loop breakers, node scoring, and stability]
    is_lb = isStrongLoopBreaker (idOccInfo old_bndr)

    old_unf = realIdUnfolding old_bndr
    can_unfold = canUnfold old_unf
    rhs        = case old_unf of
                   CoreUnfolding { uf_src = src, uf_tmpl = unf_rhs }
                     | isStableSource src
                     -> unf_rhs
                   _ -> bind_rhs
       -- 'bind_rhs' is irrelevant for inlining things with a stable unfolding
    rhs_size = case old_unf of
                 CoreUnfolding { uf_guidance = guidance }
                    | UnfIfGoodArgs { ug_size = size } <- guidance
                    -> size
                 _  -> cheapExprSize rhs


        -- Checking for a constructor application
        -- Cheap and cheerful; the simplifier moves casts out of the way
        -- The lambda case is important to spot x = /\a. C (f a)
        -- which comes up when C is a dictionary constructor and
        -- f is a default method.
        -- Example: the instance for Show (ST s a) in GHC.ST
        --
        -- However we *also* treat (\x. C p q) as a con-app-like thing,
        --      Note [Closure conversion]
    is_con_app (Var v)    = isConLikeId v
    is_con_app (App f _)  = is_con_app f
    is_con_app (Lam _ e)  = is_con_app e
    is_con_app (Tick _ e) = is_con_app e
    is_con_app _          = False

maxExprSize :: Int
maxExprSize = 20  -- Rather arbitrary

cheapExprSize :: CoreExpr -> Int
-- Maxes out at maxExprSize
cheapExprSize e
  = go 0 e
  where
    go n e | n >= maxExprSize = n
           | otherwise        = go1 n e

    go1 n (Var {})        = n+1
    go1 n (Lit {})        = n+1
    go1 n (Type {})       = n
    go1 n (Coercion {})   = n
    go1 n (Tick _ e)      = go1 n e
    go1 n (Cast e _)      = go1 n e
    go1 n (App f a)       = go (go1 n f) a
    go1 n (Lam b e)
      | isTyVar b         = go1 n e
      | otherwise         = go (n+1) e
    go1 n (Let b e)       = gos (go1 n e) (rhssOfBind b)
    go1 n (Case e _ _ as) = gos (go1 n e) (rhssOfAlts as)

    gos n [] = n
    gos n (e:es) | n >= maxExprSize = n
                 | otherwise        = gos (go1 n e) es

betterLB :: NodeScore -> NodeScore -> Bool
-- If  n1 `betterLB` n2  then choose n1 as the loop breaker
betterLB (rank1, size1, lb1) (rank2, size2, _)
  | rank1 < rank2 = True
  | rank1 > rank2 = False
  | size1 < size2 = False   -- Make the bigger n2 into the loop breaker
  | size1 > size2 = True
  | lb1           = True    -- Tie-break: if n1 was a loop breaker before, choose it
  | otherwise     = False   -- See Note [Loop breakers, node scoring, and stability]

{- Note [Self-recursion and loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
   rec { f = ...f...g...
       ; g = .....f...   }
then 'f' has to be a loop breaker anyway, so we may as well choose it
right away, so that g can inline freely.

This is really just a cheap hack. Consider
   rec { f = ...g...
       ; g = ..f..h...
      ;  h = ...f....}
Here f or g are better loop breakers than h; but we might accidentally
choose h.  Finding the minimal set of loop breakers is hard.

Note [Loop breakers, node scoring, and stability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To choose a loop breaker, we give a NodeScore to each node in the SCC,
and pick the one with the best score (according to 'betterLB').

We need to be jolly careful (#12425, #12234) about the stability
of this choice. Suppose we have

    let rec { f = ...g...g...
            ; g = ...f...f... }
    in
    case x of
      True  -> ...f..
      False -> ..f...

In each iteration of the simplifier the occurrence analyser OccAnal
chooses a loop breaker. Suppose in iteration 1 it choose g as the loop
breaker. That means it is free to inline f.

Suppose that GHC decides to inline f in the branches of the case, but
(for some reason; eg it is not saturated) in the rhs of g. So we get

    let rec { f = ...g...g...
            ; g = ...f...f... }
    in
    case x of
      True  -> ...g...g.....
      False -> ..g..g....

Now suppose that, for some reason, in the next iteration the occurrence
analyser chooses f as the loop breaker, so it can freely inline g. And
again for some reason the simplifier inlines g at its calls in the case
branches, but not in the RHS of f. Then we get

    let rec { f = ...g...g...
            ; g = ...f...f... }
    in
    case x of
      True  -> ...(...f...f...)...(...f..f..).....
      False -> ..(...f...f...)...(..f..f...)....

You can see where this is going! Each iteration of the simplifier
doubles the number of calls to f or g. No wonder GHC is slow!

(In the particular example in comment:3 of #12425, f and g are the two
mutually recursive fmap instances for CondT and Result. They are both
marked INLINE which, oddly, is why they don't inline in each other's
RHS, because the call there is not saturated.)

The root cause is that we flip-flop on our choice of loop breaker. I
always thought it didn't matter, and indeed for any single iteration
to terminate, it doesn't matter. But when we iterate, it matters a
lot!!

So The Plan is this:
   If there is a tie, choose the node that
   was a loop breaker last time round

Hence the is_lb field of NodeScore

************************************************************************
*                                                                      *
                   Right hand sides
*                                                                      *
************************************************************************
-}

occAnalRhs :: OccEnv -> RecFlag -> Maybe JoinArity
           -> CoreExpr   -- RHS
           -> WithUsageDetails CoreExpr
occAnalRhs !env is_rec mb_join_arity rhs
  = let
      (bndrs, body) = collectBinders rhs
      (WithUsageDetails body_usage (bndrs',body')) = occAnalLamOrRhs env bndrs body
      final_bndrs | isRec is_rec = bndrs'
                  | otherwise    = markJoinOneShots mb_join_arity bndrs'
             -- For a /non-recursive/ join point we can mark all
             -- its join-lambda as one-shot; and it's a good idea to do so

      -- Final adjustment
      rhs_usage = adjustRhsUsage is_rec mb_join_arity final_bndrs body_usage
    in WithUsageDetails rhs_usage (mkLams final_bndrs body')

occAnalUnfolding :: OccEnv
                 -> RecFlag
                 -> Maybe JoinArity   -- See Note [Join points and unfoldings/rules]
                 -> Unfolding
                 -> WithUsageDetails Unfolding
-- Occurrence-analyse a stable unfolding;
-- discard a non-stable one altogether.
occAnalUnfolding !env is_rec mb_join_arity unf
  = case unf of
      unf@(CoreUnfolding { uf_tmpl = rhs, uf_src = src })
        | isStableSource src ->
            let
              (WithUsageDetails usage rhs') = occAnalRhs env is_rec mb_join_arity rhs

              unf' | noBinderSwaps env = unf -- Note [Unfoldings and rules]
                   | otherwise         = unf { uf_tmpl = rhs' }
            in WithUsageDetails (markAllMany usage) unf'
              -- markAllMany: see Note [Occurrences in stable unfoldings]
        | otherwise          -> WithUsageDetails emptyDetails unf
              -- For non-Stable unfoldings we leave them undisturbed, but
              -- don't count their usage because the simplifier will discard them.
              -- We leave them undisturbed because nodeScore uses their size info
              -- to guide its decisions.  It's ok to leave un-substituted
              -- expressions in the tree because all the variables that were in
              -- scope remain in scope; there is no cloning etc.

      unf@(DFunUnfolding { df_bndrs = bndrs, df_args = args })
        -> WithUsageDetails final_usage (unf { df_args = args' })
        where
          env'            = env `addInScope` bndrs
          (WithUsageDetails usage args') = occAnalList env' args
          final_usage     = markAllManyNonTail (delDetailsList usage bndrs)
                            `addLamCoVarOccs` bndrs

      unf -> WithUsageDetails emptyDetails unf

occAnalRules :: OccEnv
             -> Maybe JoinArity  -- See Note [Join points and unfoldings/rules]
             -> Id               -- Get rules from here
             -> [(CoreRule,      -- Each (non-built-in) rule
                  UsageDetails,  -- Usage details for LHS
                  UsageDetails)] -- Usage details for RHS
occAnalRules !env mb_join_arity bndr
  = map occ_anal_rule (idCoreRules bndr)
  where
    occ_anal_rule rule@(Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs })
      = (rule', lhs_uds', rhs_uds')
      where
        env' = env `addInScope` bndrs
        rule' | noBinderSwaps env = rule  -- Note [Unfoldings and rules]
              | otherwise         = rule { ru_args = args', ru_rhs = rhs' }

        (WithUsageDetails lhs_uds args') = occAnalList env' args
        lhs_uds'         = markAllManyNonTail (lhs_uds `delDetailsList` bndrs)
                           `addLamCoVarOccs` bndrs

        (WithUsageDetails rhs_uds rhs') = occAnal env' rhs
                            -- Note [Rules are extra RHSs]
                            -- Note [Rule dependency info]
        rhs_uds' = markAllNonTailIf (not exact_join) $
                   markAllMany                             $
                   rhs_uds `delDetailsList` bndrs

        exact_join = exactJoin mb_join_arity args
                     -- See Note [Join points and unfoldings/rules]

    occ_anal_rule other_rule = (other_rule, emptyDetails, emptyDetails)

{- Note [Join point RHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   x = e
   join j = Just x

We want to inline x into j right away, so we don't want to give
the join point a RhsCtxt (#14137).  It's not a huge deal, because
the FloatIn pass knows to float into join point RHSs; and the simplifier
does not float things out of join point RHSs.  But it's a simple, cheap
thing to do.  See #14137.

Note [Occurrences in stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    f p = BIG
    {-# INLINE g #-}
    g y = not (f y)
where this is the /only/ occurrence of 'f'.  So 'g' will get a stable
unfolding.  Now suppose that g's RHS gets optimised (perhaps by a rule
or inlining f) so that it doesn't mention 'f' any more.  Now the last
remaining call to f is in g's Stable unfolding. But, even though there
is only one syntactic occurrence of f, we do /not/ want to do
preinlineUnconditionally here!

The INLINE pragma says "inline exactly this RHS"; perhaps the
programmer wants to expose that 'not', say. If we inline f that will make
the Stable unfoldign big, and that wasn't what the programmer wanted.

Another way to think about it: if we inlined g as-is into multiple
call sites, now there's be multiple calls to f.

Bottom line: treat all occurrences in a stable unfolding as "Many".

Note [Unfoldings and rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally unfoldings and rules are already occurrence-analysed, so we
don't want to reconstruct their trees; we just want to analyse them to
find how they use their free variables.

EXCEPT if there is a binder-swap going on, in which case we do want to
produce a new tree.

So we have a fast-path that keeps the old tree if the occ_bs_env is
empty.   This just saves a bit of allocation and reconstruction; not
a big deal.

Note [Cascading inlines]
~~~~~~~~~~~~~~~~~~~~~~~~
By default we use an rhsCtxt for the RHS of a binding.  This tells the
occ anal n that it's looking at an RHS, which has an effect in
occAnalApp.  In particular, for constructor applications, it makes
the arguments appear to have NoOccInfo, so that we don't inline into
them. Thus    x = f y
              k = Just x
we do not want to inline x.

But there's a problem.  Consider
     x1 = a0 : []
     x2 = a1 : x1
     x3 = a2 : x2
     g  = f x3
First time round, it looks as if x1 and x2 occur as an arg of a
let-bound constructor ==> give them a many-occurrence.
But then x3 is inlined (unconditionally as it happens) and
next time round, x2 will be, and the next time round x1 will be
Result: multiple simplifier iterations.  Sigh.

So, when analysing the RHS of x3 we notice that x3 will itself
definitely inline the next time round, and so we analyse x3's rhs in
an ordinary context, not rhsCtxt.  Hence the "certainly_inline" stuff.

Annoyingly, we have to approximate GHC.Core.Opt.Simplify.Utils.preInlineUnconditionally.
If (a) the RHS is expandable (see isExpandableApp in occAnalApp), and
   (b) certainly_inline says "yes" when preInlineUnconditionally says "no"
then the simplifier iterates indefinitely:
        x = f y
        k = Just x   -- We decide that k is 'certainly_inline'
        v = ...k...  -- but preInlineUnconditionally doesn't inline it
inline ==>
        k = Just (f y)
        v = ...k...
float ==>
        x1 = f y
        k = Just x1
        v = ...k...

This is worse than the slow cascade, so we only want to say "certainly_inline"
if it really is certain.  Look at the note with preInlineUnconditionally
for the various clauses.


************************************************************************
*                                                                      *
                Expressions
*                                                                      *
************************************************************************
-}

occAnalList :: OccEnv -> [CoreExpr] -> WithUsageDetails [CoreExpr]
occAnalList !_   []    = WithUsageDetails emptyDetails []
occAnalList env (e:es) = let
                          (WithUsageDetails uds1 e') = occAnal env e
                          (WithUsageDetails uds2 es') = occAnalList env es
                         in WithUsageDetails (uds1 `andUDs` uds2) (e' : es')

occAnal :: OccEnv
        -> CoreExpr
        -> WithUsageDetails CoreExpr       -- Gives info only about the "interesting" Ids

occAnal !_   expr@(Lit _)  = WithUsageDetails emptyDetails expr

occAnal env expr@(Var _) = occAnalApp env (expr, [], [])
    -- At one stage, I gathered the idRuleVars for the variable here too,
    -- which in a way is the right thing to do.
    -- But that went wrong right after specialisation, when
    -- the *occurrences* of the overloaded function didn't have any
    -- rules in them, so the *specialised* versions looked as if they
    -- weren't used at all.

occAnal _ expr@(Type ty)
  = WithUsageDetails (addManyOccs emptyDetails (coVarsOfType ty)) expr
occAnal _ expr@(Coercion co)
  = WithUsageDetails (addManyOccs emptyDetails (coVarsOfCo co)) expr
        -- See Note [Gather occurrences of coercion variables]

{- Note [Gather occurrences of coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to gather info about what coercion variables appear, for two reasons:

1. So that we can sort them into the right place when doing dependency analysis.

2. So that we know when they are surely dead.

It is useful to know when they a coercion variable is surely dead,
when we want to discard a case-expression, in GHC.Core.Opt.Simplify.rebuildCase.
For example (#20143):

  case unsafeEqualityProof @blah of
     UnsafeRefl cv -> ...no use of cv...

Here we can discard the case, since unsafeEqualityProof always terminates.
But only if the coercion variable 'cv' is unused.

Another example from #15696: we had something like
  case eq_sel d of co -> ...(typeError @(...co...) "urk")...
Then 'd' was substituted by a dictionary, so the expression
simpified to
  case (Coercion <blah>) of cv -> ...(typeError @(...cv...) "urk")...

We can only  drop the case altogether if 'cv' is unused, which is not
the case here.

Conclusion: we need accurate dead-ness info for CoVars.
We gather CoVar occurrences from:

  * The (Type ty) and (Coercion co) cases of occAnal

  * The type 'ty' of a lambda-binder (\(x:ty). blah)
    See addLamCoVarOccs

But it is not necessary to gather CoVars from the types of other binders.

* For let-binders, if the type mentions a CoVar, so will the RHS (since
  it has the same type)

* For case-alt binders, if the type mentions a CoVar, so will the scrutinee
  (since it has the same type)
-}

occAnal env (Tick tickish body)
  | SourceNote{} <- tickish
  = WithUsageDetails usage (Tick tickish body')
                  -- SourceNotes are best-effort; so we just proceed as usual.
                  -- If we drop a tick due to the issues described below it's
                  -- not the end of the world.

  | tickish `tickishScopesLike` SoftScope
  = WithUsageDetails (markAllNonTail usage) (Tick tickish body')

  | Breakpoint _ _ ids <- tickish
  = WithUsageDetails (usage_lam `andUDs` foldr addManyOcc emptyDetails ids) (Tick tickish body')
    -- never substitute for any of the Ids in a Breakpoint

  | otherwise
  = WithUsageDetails usage_lam (Tick tickish body')
  where
    (WithUsageDetails usage body') = occAnal env body
    -- for a non-soft tick scope, we can inline lambdas only
    usage_lam = markAllNonTail (markAllInsideLam usage)
                  -- TODO There may be ways to make ticks and join points play
                  -- nicer together, but right now there are problems:
                  --   let j x = ... in tick<t> (j 1)
                  -- Making j a join point may cause the simplifier to drop t
                  -- (if the tick is put into the continuation). So we don't
                  -- count j 1 as a tail call.
                  -- See #14242.

occAnal env (Cast expr co)
  = let
      (WithUsageDetails usage expr') = occAnal env expr
      usage1 = markAllManyNonTailIf (isRhsEnv env) usage
          -- usage1: if we see let x = y `cast` co
          -- then mark y as 'Many' so that we don't
          -- immediately inline y again.
      usage2 = addManyOccs usage1 (coVarsOfCo co)
          -- usage2: see Note [Gather occurrences of coercion variables]
    in WithUsageDetails (markAllNonTail usage2) (Cast expr' co)

occAnal env app@(App _ _)
  = occAnalApp env (collectArgsTicks tickishFloatable app)

-- Ignore type variables altogether
--   (a) occurrences inside type lambdas only not marked as InsideLam
--   (b) type variables not in environment

occAnal env (Lam x body)
  | isTyVar x
  = let
      (WithUsageDetails body_usage body') = occAnal env body
    in WithUsageDetails (markAllNonTail body_usage) (Lam x body')

{- Note [Occurrence analysis for lambda binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For value lambdas we do a special hack.  Consider
     (\x. \y. ...x...)
If we did nothing, x is used inside the \y, so would be marked
as dangerous to dup.  But in the common case where the abstraction
is applied to two arguments this is over-pessimistic, which delays
inlining x, which forces more simplifier iterations.

So instead, we just mark each binder with its occurrence info in the
*body* of the multiple lambda.  Then, the simplifier is careful when
partially applying lambdas. See the calls to zapLamBndrs in
  GHC.Core.Opt.Simplify.simplExprF1
  GHC.Core.SimpleOpt.simple_app
-}

occAnal env expr@(Lam _ _)
  = -- See Note [Occurrence analysis for lambda binders]
    let
      (bndrs, body) = collectBinders expr
      (WithUsageDetails usage (tagged_bndrs, body')) = occAnalLamOrRhs env bndrs body
      expr'       = mkLams tagged_bndrs body'
      usage1      = markAllNonTail usage
      one_shot_gp = all isOneShotBndr tagged_bndrs
      final_usage = markAllInsideLamIf (not one_shot_gp) usage1
                    `addLamCoVarOccs` bndrs
        -- See Note [Gather occurrences of coercion variables]
    in WithUsageDetails final_usage expr'

occAnal env (Case scrut bndr ty alts)
  = let
      (WithUsageDetails scrut_usage scrut') = occAnal (scrutCtxt env alts) scrut
      alt_env = addBndrSwap scrut' bndr $ env { occ_encl = OccVanilla } `addInScope` [bndr]
      (alts_usage_s, alts') = mapAndUnzip ((\(WithUsageDetails uds a) -> (uds,a)) . occAnalAlt alt_env) alts
      alts_usage  = foldr orUDs emptyDetails alts_usage_s
      (alts_usage1, tagged_bndr) = tagLamBinder alts_usage bndr
      total_usage = markAllNonTail scrut_usage `andUDs` alts_usage1
                    -- Alts can have tail calls, but the scrutinee can't
    in WithUsageDetails total_usage (Case scrut' tagged_bndr ty alts')

occAnal env (Let bind body)
  = let
      (WithUsageDetails body_usage body') = occAnal (env `addInScope` bindersOf bind) body
      (WithUsageDetails final_usage new_binds) = occAnalBind env NotTopLevel
                                                    noImpRuleEdges bind body_usage
    in WithUsageDetails final_usage (mkLets new_binds body')

occAnalArgs :: OccEnv -> CoreExpr -> [CoreExpr] -> [OneShots] -> WithUsageDetails CoreExpr
-- The `fun` argument is just an accumulating parameter,
-- the base for building the application we return
occAnalArgs !env fun args !one_shots
  = go emptyDetails fun args one_shots
  where
    go uds fun [] _ = WithUsageDetails uds fun
    go uds fun (arg:args) one_shots
      = go (uds `andUDs` arg_uds) (fun `App` arg') args one_shots'
      where
        !(WithUsageDetails arg_uds arg') = occAnal arg_env arg
        !(arg_env, one_shots')
            | isTypeArg arg = (env, one_shots)
            | otherwise     = valArgCtxt env one_shots

{-
Applications are dealt with specially because we want
the "build hack" to work.

Note [Arguments of let-bound constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    f x = let y = expensive x in
          let z = (True,y) in
          (case z of {(p,q)->q}, case z of {(p,q)->q})
We feel free to duplicate the WHNF (True,y), but that means
that y may be duplicated thereby.

If we aren't careful we duplicate the (expensive x) call!
Constructors are rather like lambdas in this way.
-}

occAnalApp :: OccEnv
           -> (Expr CoreBndr, [Arg CoreBndr], [CoreTickish])
           -> WithUsageDetails (Expr CoreBndr)
-- Naked variables (not applied) end up here too
occAnalApp !env (Var fun, args, ticks)
  -- Account for join arity of runRW# continuation
  -- See Note [Simplification of runRW#]
  --
  -- NB: Do not be tempted to make the next (Var fun, args, tick)
  --     equation into an 'otherwise' clause for this equation
  --     The former has a bang-pattern to occ-anal the args, and
  --     we don't want to occ-anal them twice in the runRW# case!
  --     This caused #18296
  | fun `hasKey` runRWKey
  , [t1, t2, arg]  <- args
  , let (WithUsageDetails usage arg') = occAnalRhs env NonRecursive (Just 1) arg
  = WithUsageDetails usage (mkTicks ticks $ mkApps (Var fun) [t1, t2, arg'])

occAnalApp env (Var fun_id, args, ticks)
  = WithUsageDetails all_uds (mkTicks ticks app')
  where
    -- Lots of banged bindings: this is a very heavily bit of code,
    -- so it pays not to make lots of thunks here, all of which
    -- will ultimately be forced.
    !(fun', fun_id')  = lookupBndrSwap env fun_id
    !(WithUsageDetails args_uds app') = occAnalArgs env fun' args one_shots

    fun_uds = mkOneOcc fun_id' int_cxt n_args
       -- NB: fun_uds is computed for fun_id', not fun_id
       -- See (BS1) in Note [The binder-swap substitution]

    all_uds = fun_uds `andUDs` final_args_uds

    !final_args_uds = markAllNonTail                              $
                      markAllInsideLamIf (isRhsEnv env && is_exp) $
                      args_uds
       -- We mark the free vars of the argument of a constructor or PAP
       -- as "inside-lambda", if it is the RHS of a let(rec).
       -- This means that nothing gets inlined into a constructor or PAP
       -- argument position, which is what we want.  Typically those
       -- constructor arguments are just variables, or trivial expressions.
       -- We use inside-lam because it's like eta-expanding the PAP.
       --
       -- This is the *whole point* of the isRhsEnv predicate
       -- See Note [Arguments of let-bound constructors]

    !n_val_args = valArgCount args
    !n_args     = length args
    !int_cxt    = case occ_encl env of
                   OccScrut -> IsInteresting
                   _other   | n_val_args > 0 -> IsInteresting
                            | otherwise      -> NotInteresting

    !is_exp     = isExpandableApp fun_id n_val_args
        -- See Note [CONLIKE pragma] in GHC.Types.Basic
        -- The definition of is_exp should match that in GHC.Core.Opt.Simplify.prepareRhs

    one_shots  = argsOneShots (idDmdSig fun_id) guaranteed_val_args
    guaranteed_val_args = n_val_args + length (takeWhile isOneShotInfo
                                                         (occ_one_shots env))
        -- See Note [Sources of one-shot information], bullet point A']

occAnalApp env (fun, args, ticks)
  = WithUsageDetails (markAllNonTail (fun_uds `andUDs` args_uds))
                     (mkTicks ticks app')
  where
    !(WithUsageDetails args_uds app') = occAnalArgs env fun' args []
    !(WithUsageDetails fun_uds fun')  = occAnal (addAppCtxt env args) fun
        -- The addAppCtxt is a bit cunning.  One iteration of the simplifier
        -- often leaves behind beta redexs like
        --      (\x y -> e) a1 a2
        -- Here we would like to mark x,y as one-shot, and treat the whole
        -- thing much like a let.  We do this by pushing some OneShotLam items
        -- onto the context stack.

addAppCtxt :: OccEnv -> [Arg CoreBndr] -> OccEnv
addAppCtxt env@(OccEnv { occ_one_shots = ctxt }) args
  | n_val_args > 0
  = env { occ_one_shots = replicate n_val_args OneShotLam ++ ctxt
        , occ_encl      = OccVanilla }
          -- OccVanilla: the function part of the application
          -- is no longer on OccRhs or OccScrut
  | otherwise
  = env
  where
    n_val_args = valArgCount args


{-
Note [Sources of one-shot information]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The occurrence analyser obtains one-shot-lambda information from two sources:

A:  Saturated applications:  eg   f e1 .. en

    In general, given a call (f e1 .. en) we can propagate one-shot info from
    f's strictness signature into e1 .. en, but /only/ if n is enough to
    saturate the strictness signature. A strictness signature like

          f :: C1(C1(L))LS

    means that *if f is applied to three arguments* then it will guarantee to
    call its first argument at most once, and to call the result of that at
    most once. But if f has fewer than three arguments, all bets are off; e.g.

          map (f (\x y. expensive) e2) xs

    Here the \x y abstraction may be called many times (once for each element of
    xs) so we should not mark x and y as one-shot. But if it was

          map (f (\x y. expensive) 3 2) xs

    then the first argument of f will be called at most once.

    The one-shot info, derived from f's strictness signature, is
    computed by 'argsOneShots', called in occAnalApp.

A': Non-obviously saturated applications: eg    build (f (\x y -> expensive))
    where f is as above.

    In this case, f is only manifestly applied to one argument, so it does not
    look saturated. So by the previous point, we should not use its strictness
    signature to learn about the one-shotness of \x y. But in this case we can:
    build is fully applied, so we may use its strictness signature; and from
    that we learn that build calls its argument with two arguments *at most once*.

    So there is really only one call to f, and it will have three arguments. In
    that sense, f is saturated, and we may proceed as described above.

    Hence the computation of 'guaranteed_val_args' in occAnalApp, using
    '(occ_one_shots env)'.  See also #13227, comment:9

B:  Let-bindings:  eg   let f = \c. let ... in \n -> blah
                        in (build f, build f)

    Propagate one-shot info from the demanand-info on 'f' to the
    lambdas in its RHS (which may not be syntactically at the top)

    This information must have come from a previous run of the demanand
    analyser.

Previously, the demand analyser would *also* set the one-shot information, but
that code was buggy (see #11770), so doing it only in on place, namely here, is
saner.

Note [OneShots]
~~~~~~~~~~~~~~~
When analysing an expression, the occ_one_shots argument contains information
about how the function is being used. The length of the list indicates
how many arguments will eventually be passed to the analysed expression,
and the OneShotInfo indicates whether this application is once or multiple times.

Example:

 Context of f                occ_one_shots when analysing f

 f 1 2                       [OneShot, OneShot]
 map (f 1)                   [OneShot, NoOneShotInfo]
 build f                     [OneShot, OneShot]
 f 1 2 `seq` f 2 1           [NoOneShotInfo, OneShot]

Note [Binders in case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    case x of y { (a,b) -> f y }
We treat 'a', 'b' as dead, because they don't physically occur in the
case alternative.  (Indeed, a variable is dead iff it doesn't occur in
its scope in the output of OccAnal.)  It really helps to know when
binders are unused.  See esp the call to isDeadBinder in
Simplify.mkDupableAlt

In this example, though, the Simplifier will bring 'a' and 'b' back to
life, because it binds 'y' to (a,b) (imagine got inlined and
scrutinised y).
-}

occAnalLamOrRhs :: OccEnv -> [CoreBndr] -> CoreExpr
                -> WithUsageDetails ([CoreBndr], CoreExpr)
-- Tags the returned binders with their OccInfo, but does
-- not do any markInsideLam to the returned usage details
occAnalLamOrRhs !env [] body
  = let (WithUsageDetails body_usage body') = occAnal env body
    in WithUsageDetails body_usage ([], body')
      -- RHS of thunk or nullary join point

occAnalLamOrRhs env (bndr:bndrs) body
  | isTyVar bndr
  = -- Important: Keep the environment so that we don't inline into an RHS like
    --   \(@ x) -> C @x (f @x)
    -- (see the beginning of Note [Cascading inlines]).
    let
      (WithUsageDetails body_usage (bndrs',body')) = occAnalLamOrRhs env bndrs body
    in WithUsageDetails body_usage (bndr:bndrs', body')

occAnalLamOrRhs env binders body
  = let
      (WithUsageDetails body_usage body') = occAnal env_body body
      (final_usage, tagged_binders) = tagLamBinders body_usage binders'
                      -- Use binders' to put one-shot info on the lambdas
    in
    WithUsageDetails final_usage (tagged_binders, body')
  where
    env1 = env `addInScope` binders
    (env_body, binders') = oneShotGroup env1 binders

occAnalAlt :: OccEnv
           -> CoreAlt -> WithUsageDetails (Alt IdWithOccInfo)
occAnalAlt !env (Alt con bndrs rhs)
  = let
      (WithUsageDetails rhs_usage1 rhs1) = occAnal (env `addInScope` bndrs) rhs
      (alt_usg, tagged_bndrs) = tagLamBinders rhs_usage1 bndrs
    in                          -- See Note [Binders in case alternatives]
    WithUsageDetails alt_usg (Alt con tagged_bndrs rhs1)

{-
************************************************************************
*                                                                      *
                    OccEnv
*                                                                      *
************************************************************************
-}

data OccEnv
  = OccEnv { occ_encl       :: !OccEncl      -- Enclosing context information
           , occ_one_shots  :: !OneShots     -- See Note [OneShots]
           , occ_unf_act    :: Id -> Bool          -- Which Id unfoldings are active
           , occ_rule_act   :: Activation -> Bool  -- Which rules are active
             -- See Note [Finding rule RHS free vars]

           -- See Note [The binder-swap substitution]
           -- If  x :-> (y, co)  is in the env,
           -- then please replace x by (y |> sym mco)
           -- Invariant of course: idType x = exprType (y |> sym mco)
           , occ_bs_env  :: !(VarEnv (OutId, MCoercion))
           , occ_bs_rng  :: !VarSet   -- Vars free in the range of occ_bs_env
                   -- Domain is Global and Local Ids
                   -- Range is just Local Ids
    }


-----------------------------
-- OccEncl is used to control whether to inline into constructor arguments
-- For example:
--      x = (p,q)               -- Don't inline p or q
--      y = /\a -> (p a, q a)   -- Still don't inline p or q
--      z = f (p,q)             -- Do inline p,q; it may make a rule fire
-- So OccEncl tells enough about the context to know what to do when
-- we encounter a constructor application or PAP.
--
-- OccScrut is used to set the "interesting context" field of OncOcc

data OccEncl
  = OccRhs         -- RHS of let(rec), albeit perhaps inside a type lambda
                   -- Don't inline into constructor args here

  | OccScrut       -- Scrutintee of a case
                   -- Can inline into constructor args

  | OccVanilla     -- Argument of function, body of lambda, etc
                   -- Do inline into constructor args here

instance Outputable OccEncl where
  ppr OccRhs     = text "occRhs"
  ppr OccScrut   = text "occScrut"
  ppr OccVanilla = text "occVanilla"

-- See note [OneShots]
type OneShots = [OneShotInfo]

initOccEnv :: OccEnv
initOccEnv
  = OccEnv { occ_encl      = OccVanilla
           , occ_one_shots = []

                 -- To be conservative, we say that all
                 -- inlines and rules are active
           , occ_unf_act   = \_ -> True
           , occ_rule_act  = \_ -> True

           , occ_bs_env = emptyVarEnv
           , occ_bs_rng = emptyVarSet }

noBinderSwaps :: OccEnv -> Bool
noBinderSwaps (OccEnv { occ_bs_env = bs_env }) = isEmptyVarEnv bs_env

scrutCtxt :: OccEnv -> [CoreAlt] -> OccEnv
scrutCtxt !env alts
  | interesting_alts =  env { occ_encl = OccScrut,   occ_one_shots = [] }
  | otherwise        =  env { occ_encl = OccVanilla, occ_one_shots = [] }
  where
    interesting_alts = case alts of
                         []    -> False
                         [alt] -> not (isDefaultAlt alt)
                         _     -> True
     -- 'interesting_alts' is True if the case has at least one
     -- non-default alternative.  That in turn influences
     -- pre/postInlineUnconditionally.  Grep for "occ_int_cxt"!

rhsCtxt :: OccEnv -> OccEnv
rhsCtxt !env = env { occ_encl = OccRhs, occ_one_shots = [] }

valArgCtxt :: OccEnv -> [OneShots] -> (OccEnv, [OneShots])
valArgCtxt !env []
  = (env { occ_encl = OccVanilla, occ_one_shots = [] }, [])
valArgCtxt env (one_shots:one_shots_s)
  = (env { occ_encl = OccVanilla, occ_one_shots = one_shots }, one_shots_s)

isRhsEnv :: OccEnv -> Bool
isRhsEnv (OccEnv { occ_encl = cxt }) = case cxt of
                                          OccRhs -> True
                                          _      -> False

addInScope :: OccEnv -> [Var] -> OccEnv
-- See Note [The binder-swap substitution]
addInScope env@(OccEnv { occ_bs_env = swap_env, occ_bs_rng = rng_vars }) bndrs
  | any (`elemVarSet` rng_vars) bndrs = env { occ_bs_env = emptyVarEnv, occ_bs_rng = emptyVarSet }
  | otherwise                         = env { occ_bs_env = swap_env `delVarEnvList` bndrs }

oneShotGroup :: OccEnv -> [CoreBndr]
             -> ( OccEnv
                , [CoreBndr] )
        -- The result binders have one-shot-ness set that they might not have had originally.
        -- This happens in (build (\c n -> e)).  Here the occurrence analyser
        -- linearity context knows that c,n are one-shot, and it records that fact in
        -- the binder. This is useful to guide subsequent float-in/float-out transformations

oneShotGroup env@(OccEnv { occ_one_shots = ctxt }) bndrs
  = go ctxt bndrs []
  where
    go ctxt [] rev_bndrs
      = ( env { occ_one_shots = ctxt, occ_encl = OccVanilla }
        , reverse rev_bndrs )

    go [] bndrs rev_bndrs
      = ( env { occ_one_shots = [], occ_encl = OccVanilla }
        , reverse rev_bndrs ++ bndrs )

    go ctxt@(one_shot : ctxt') (bndr : bndrs) rev_bndrs
      | isId bndr = go ctxt' bndrs (bndr': rev_bndrs)
      | otherwise = go ctxt  bndrs (bndr : rev_bndrs)
      where
        bndr' = updOneShotInfo bndr one_shot
               -- Use updOneShotInfo, not setOneShotInfo, as pre-existing
               -- one-shot info might be better than what we can infer, e.g.
               -- due to explicit use of the magic 'oneShot' function.
               -- See Note [The oneShot function]


markJoinOneShots :: Maybe JoinArity -> [Var] -> [Var]
-- Mark the lambdas of a non-recursive join point as one-shot.
-- This is good to prevent gratuitous float-out etc
markJoinOneShots mb_join_arity bndrs
  = case mb_join_arity of
      Nothing -> bndrs
      Just n  -> go n bndrs
 where
   go 0 bndrs  = bndrs
   go _ []     = [] -- This can legitimately happen.
                    -- e.g.    let j = case ... in j True
                    -- This will become an arity-1 join point after the
                    -- simplifier has eta-expanded it; but it may not have
                    -- enough lambdas /yet/. (Lint checks that JoinIds do
                    -- have enough lambdas.)
   go n (b:bs) = b' : go (n-1) bs
     where
       b' | isId b    = setOneShotLambda b
          | otherwise = b

--------------------
transClosureFV :: VarEnv VarSet -> VarEnv VarSet
-- If (f,g), (g,h) are in the input, then (f,h) is in the output
--                                   as well as (f,g), (g,h)
transClosureFV env
  | no_change = env
  | otherwise = transClosureFV (listToUFM_Directly new_fv_list)
  where
    (no_change, new_fv_list) = mapAccumL bump True (nonDetUFMToList env)
      -- It's OK to use nonDetUFMToList here because we'll forget the
      -- ordering by creating a new set with listToUFM
    bump no_change (b,fvs)
      | no_change_here = (no_change, (b,fvs))
      | otherwise      = (False,     (b,new_fvs))
      where
        (new_fvs, no_change_here) = extendFvs env fvs

-------------
extendFvs_ :: VarEnv VarSet -> VarSet -> VarSet
extendFvs_ env s = fst (extendFvs env s)   -- Discard the Bool flag

extendFvs :: VarEnv VarSet -> VarSet -> (VarSet, Bool)
-- (extendFVs env s) returns
--     (s `union` env(s), env(s) `subset` s)
extendFvs env s
  | isNullUFM env
  = (s, True)
  | otherwise
  = (s `unionVarSet` extras, extras `subVarSet` s)
  where
    extras :: VarSet    -- env(s)
    extras = nonDetStrictFoldUFM unionVarSet emptyVarSet $
      -- It's OK to use nonDetStrictFoldUFM here because unionVarSet commutes
             intersectUFM_C (\x _ -> x) env (getUniqSet s)

{-
************************************************************************
*                                                                      *
                    Binder swap
*                                                                      *
************************************************************************

Note [Binder swap]
~~~~~~~~~~~~~~~~~~
The "binder swap" transformation swaps occurrence of the
scrutinee of a case for occurrences of the case-binder:

 (1)  case x of b { pi -> ri }
         ==>
      case x of b { pi -> ri[b/x] }

 (2)  case (x |> co) of b { pi -> ri }
        ==>
      case (x |> co) of b { pi -> ri[b |> sym co/x] }

The substitution ri[b/x] etc is done by the occurrence analyser.
See Note [The binder-swap substitution].

There are two reasons for making this swap:

(A) It reduces the number of occurrences of the scrutinee, x.
    That in turn might reduce its occurrences to one, so we
    can inline it and save an allocation.  E.g.
      let x = factorial y in case x of b { I# v -> ...x... }
    If we replace 'x' by 'b' in the alternative we get
      let x = factorial y in case x of b { I# v -> ...b... }
    and now we can inline 'x', thus
      case (factorial y) of b { I# v -> ...b... }

(B) The case-binder b has unfolding information; in the
    example above we know that b = I# v. That in turn allows
    nested cases to simplify.  Consider
       case x of b { I# v ->
       ...(case x of b2 { I# v2 -> rhs })...
    If we replace 'x' by 'b' in the alternative we get
       case x of b { I# v ->
       ...(case b of b2 { I# v2 -> rhs })...
    and now it is trivial to simplify the inner case:
       case x of b { I# v ->
       ...(let b2 = b in rhs)...

    The same can happen even if the scrutinee is a variable
    with a cast: see Note [Case of cast]

The reason for doing these transformations /here in the occurrence
analyser/ is because it allows us to adjust the OccInfo for 'x' and
'b' as we go.

  * Suppose the only occurrences of 'x' are the scrutinee and in the
    ri; then this transformation makes it occur just once, and hence
    get inlined right away.

  * If instead the Simplifier replaces occurrences of x with
    occurrences of b, that will mess up b's occurrence info. That in
    turn might have consequences.

There is a danger though.  Consider
      let v = x +# y
      in case (f v) of w -> ...v...v...
And suppose that (f v) expands to just v.  Then we'd like to
use 'w' instead of 'v' in the alternative.  But it may be too
late; we may have substituted the (cheap) x+#y for v in the
same simplifier pass that reduced (f v) to v.

I think this is just too bad.  CSE will recover some of it.

Note [The binder-swap substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The binder-swap is implemented by the occ_bs_env field of OccEnv.
There are two main pieces:

* Given    case x |> co of b { alts }
  we add [x :-> (b, co)] to the occ_bs_env environment; this is
  done by addBndrSwap.

* Then, at an occurrence of a variable, we look up in the occ_bs_env
  to perform the swap. This is done by lookupBndrSwap.

Some tricky corners:

(BS1) We do the substitution before gathering occurrence info. So in
      the above example, an occurrence of x turns into an occurrence
      of b, and that's what we gather in the UsageDetails.  It's as
      if the binder-swap occurred before occurrence analysis. See
      the computation of fun_uds in occAnalApp.

(BS2) When doing a lookup in occ_bs_env, we may need to iterate,
      as you can see implemented in lookupBndrSwap.  Why?
      Consider   case x of a { 1# -> e1; DEFAULT ->
                 case x of b { 2# -> e2; DEFAULT ->
                 case x of c { 3# -> e3; DEFAULT -> ..x..a..b.. }}}
      At the first case addBndrSwap will extend occ_bs_env with
          [x :-> a]
      At the second case we occ-anal the scrutinee 'x', which looks up
        'x in occ_bs_env, returning 'a', as it should.
      Then addBndrSwap will add [a :-> b] to occ_bs_env, yielding
         occ_bs_env = [x :-> a, a :-> b]
      At the third case we'll again look up 'x' which returns 'a'.
      But we don't want to stop the lookup there, else we'll end up with
                 case x of a { 1# -> e1; DEFAULT ->
                 case a of b { 2# -> e2; DEFAULT ->
                 case a of c { 3# -> e3; DEFAULT -> ..a..b..c.. }}}
      Instead, we want iterate the lookup in addBndrSwap, to give
                 case x of a { 1# -> e1; DEFAULT ->
                 case a of b { 2# -> e2; DEFAULT ->
                 case b of c { 3# -> e3; DEFAULT -> ..c..c..c.. }}}
      This makes a particular difference for case-merge, which works
      only if the scrutinee is the case-binder of the immediately enclosing
      case (Note [Merge Nested Cases] in GHC.Core.Opt.Simplify.Utils
      See #19581 for the bug report that showed this up.

(BS3) We need care when shadowing.  Suppose [x :-> b] is in occ_bs_env,
      and we encounter:
         - \x. blah
           Here we want to delete the x-binding from occ_bs_env

         - \b. blah
           This is harder: we really want to delete all bindings that
           have 'b' free in the range.  That is a bit tiresome to implement,
           so we compromise.  We keep occ_bs_rng, which is the set of
           free vars of rng(occc_bs_env).  If a binder shadows any of these
           variables, we discard all of occ_bs_env.  Safe, if a bit
           brutal.  NB, however: the simplifer de-shadows the code, so the
           next time around this won't happen.

      These checks are implemented in addInScope.

      The occurrence analyser itself does /not/ do cloning. It could, in
      principle, but it'd make it a bit more complicated and there is no
      great benefit. The simplifer uses cloning to get a no-shadowing
      situation, the care-when-shadowing behaviour above isn't needed for
      long.

(BS4) The domain of occ_bs_env can include GlobaIds.  Eg
         case M.foo of b { alts }
      We extend occ_bs_env with [M.foo :-> b].  That's fine.

(BS5) We have to apply the occ_bs_env substitution uniformly,
      including to (local) rules and unfoldings.

Historical note
---------------
We used to do the binder-swap transformation by introducing
a proxy let-binding, thus;

   case x of b { pi -> ri }
      ==>
   case x of b { pi -> let x = b in ri }

But that had two problems:

1. If 'x' is an imported GlobalId, we'd end up with a GlobalId
   on the LHS of a let-binding which isn't allowed.  We worked
   around this for a while by "localising" x, but it turned
   out to be very painful #16296,

2. In CorePrep we use the occurrence analyser to do dead-code
   elimination (see Note [Dead code in CorePrep]).  But that
   occasionally led to an unlifted let-binding
       case x of b { DEFAULT -> let x::Int# = b in ... }
   which disobeys one of CorePrep's output invariants (no unlifted
   let-bindings) -- see #5433.

Doing a substitution (via occ_bs_env) is much better.

Note [Case of cast]
~~~~~~~~~~~~~~~~~~~
Consider        case (x `cast` co) of b { I# ->
                ... (case (x `cast` co) of {...}) ...
We'd like to eliminate the inner case.  That is the motivation for
equation (2) in Note [Binder swap].  When we get to the inner case, we
inline x, cancel the casts, and away we go.

Note [Zap case binders in proxy bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From the original
     case x of cb(dead) { p -> ...x... }
we will get
     case x of cb(live) { p -> ...cb... }

Core Lint never expects to find an *occurrence* of an Id marked
as Dead, so we must zap the OccInfo on cb before making the
binding x = cb.  See #5028.

NB: the OccInfo on /occurrences/ really doesn't matter much; the simplifier
doesn't use it. So this is only to satisfy the perhaps-over-picky Lint.

Historical note [no-case-of-case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We *used* to suppress the binder-swap in case expressions when
-fno-case-of-case is on.  Old remarks:
    "This happens in the first simplifier pass,
    and enhances full laziness.  Here's the bad case:
            f = \ y -> ...(case x of I# v -> ...(case x of ...) ... )
    If we eliminate the inner case, we trap it inside the I# v -> arm,
    which might prevent some full laziness happening.  I've seen this
    in action in spectral/cichelli/Prog.hs:
             [(m,n) | m <- [1..max], n <- [1..max]]
    Hence the check for NoCaseOfCase."
However, now the full-laziness pass itself reverses the binder-swap, so this
check is no longer necessary.

Historical note [Suppressing the case binder-swap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This old note describes a problem that is also fixed by doing the
binder-swap in OccAnal:

    There is another situation when it might make sense to suppress the
    case-expression binde-swap. If we have

        case x of w1 { DEFAULT -> case x of w2 { A -> e1; B -> e2 }
                       ...other cases .... }

    We'll perform the binder-swap for the outer case, giving

        case x of w1 { DEFAULT -> case w1 of w2 { A -> e1; B -> e2 }
                       ...other cases .... }

    But there is no point in doing it for the inner case, because w1 can't
    be inlined anyway.  Furthermore, doing the case-swapping involves
    zapping w2's occurrence info (see paragraphs that follow), and that
    forces us to bind w2 when doing case merging.  So we get

        case x of w1 { A -> let w2 = w1 in e1
                       B -> let w2 = w1 in e2
                       ...other cases .... }

    This is plain silly in the common case where w2 is dead.

    Even so, I can't see a good way to implement this idea.  I tried
    not doing the binder-swap if the scrutinee was already evaluated
    but that failed big-time:

            data T = MkT !Int

            case v of w  { MkT x ->
            case x of x1 { I# y1 ->
            case x of x2 { I# y2 -> ...

    Notice that because MkT is strict, x is marked "evaluated".  But to
    eliminate the last case, we must either make sure that x (as well as
    x1) has unfolding MkT y1.  The straightforward thing to do is to do
    the binder-swap.  So this whole note is a no-op.

It's fixed by doing the binder-swap in OccAnal because we can do the
binder-swap unconditionally and still get occurrence analysis
information right.
-}

addBndrSwap :: OutExpr -> Id -> OccEnv -> OccEnv
-- See Note [The binder-swap substitution]
addBndrSwap scrut case_bndr
            env@(OccEnv { occ_bs_env = swap_env, occ_bs_rng = rng_vars })
  | Just (scrut_var, mco) <- get_scrut_var (stripTicksTopE (const True) scrut)
  , scrut_var /= case_bndr
      -- Consider: case x of x { ... }
      -- Do not add [x :-> x] to occ_bs_env, else lookupBndrSwap will loop
  = env { occ_bs_env = extendVarEnv swap_env scrut_var (case_bndr', mco)
        , occ_bs_rng = rng_vars `extendVarSet` case_bndr'
                       `unionVarSet` tyCoVarsOfMCo mco }

  | otherwise
  = env
  where
    get_scrut_var :: OutExpr -> Maybe (OutVar, MCoercion)
    get_scrut_var (Var v)           = Just (v, MRefl)
    get_scrut_var (Cast (Var v) co) = Just (v, MCo co) -- See Note [Case of cast]
    get_scrut_var _                 = Nothing

    case_bndr' = zapIdOccInfo case_bndr
                 -- See Note [Zap case binders in proxy bindings]

lookupBndrSwap :: OccEnv -> Id -> (CoreExpr, Id)
-- See Note [The binder-swap substitution]
-- Returns an expression of the same type as Id
lookupBndrSwap env@(OccEnv { occ_bs_env = bs_env })  bndr
  = case lookupVarEnv bs_env bndr of {
       Nothing           -> (Var bndr, bndr) ;
       Just (bndr1, mco) ->

    -- Why do we iterate here?
    -- See (BS2) in Note [The binder-swap substitution]
    case lookupBndrSwap env bndr1 of
      (fun, fun_id) -> (add_cast fun mco, fun_id) }

  where
    add_cast fun MRefl    = fun
    add_cast fun (MCo co) = Cast fun (mkSymCo co)
    -- We must switch that 'co' to 'sym co';
    -- see the comment with occ_bs_env
    -- No need to test for isReflCo, because 'co' came from
    -- a (Cast e co) and hence is unlikely to be Refl

{-
************************************************************************
*                                                                      *
\subsection[OccurAnal-types]{OccEnv}
*                                                                      *
************************************************************************

Note [UsageDetails and zapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On many occasions, we must modify all gathered occurrence data at once. For
instance, all occurrences underneath a (non-one-shot) lambda set the
'occ_in_lam' flag to become 'True'. We could use 'mapVarEnv' to do this, but
that takes O(n) time and we will do this often---in particular, there are many
places where tail calls are not allowed, and each of these causes all variables
to get marked with 'NoTailCallInfo'.

Instead of relying on `mapVarEnv`, then, we carry three 'IdEnv's around along
with the 'OccInfoEnv'. Each of these extra environments is a "zapped set"
recording which variables have been zapped in some way. Zapping all occurrence
info then simply means setting the corresponding zapped set to the whole
'OccInfoEnv', a fast O(1) operation.
-}

type OccInfoEnv = IdEnv OccInfo -- A finite map from ids to their usage
                -- INVARIANT: never IAmDead
                -- (Deadness is signalled by not being in the map at all)

type ZappedSet = OccInfoEnv -- Values are ignored

data UsageDetails
  = UD { ud_env       :: !OccInfoEnv
       , ud_z_many    :: !ZappedSet   -- apply 'markMany' to these
       , ud_z_in_lam  :: !ZappedSet   -- apply 'markInsideLam' to these
       , ud_z_no_tail :: !ZappedSet } -- apply 'markNonTail' to these
  -- INVARIANT: All three zapped sets are subsets of the OccInfoEnv

instance Outputable UsageDetails where
  ppr ud = ppr (ud_env (flattenUsageDetails ud))

-------------------
-- UsageDetails API

andUDs, orUDs
        :: UsageDetails -> UsageDetails -> UsageDetails
andUDs = combineUsageDetailsWith addOccInfo
orUDs  = combineUsageDetailsWith orOccInfo

mkOneOcc ::Id -> InterestingCxt -> JoinArity -> UsageDetails
mkOneOcc id int_cxt arity
  | isLocalId id
  = emptyDetails { ud_env = unitVarEnv id occ_info }
  | otherwise
  = emptyDetails
  where
    occ_info = OneOcc { occ_in_lam  = NotInsideLam
                      , occ_n_br    = oneBranch
                      , occ_int_cxt = int_cxt
                      , occ_tail    = AlwaysTailCalled arity }

addManyOccId :: UsageDetails -> Id -> UsageDetails
-- Add the non-committal (id :-> noOccInfo) to the usage details
addManyOccId ud id = ud { ud_env = extendVarEnv (ud_env ud) id noOccInfo }

-- Add several occurrences, assumed not to be tail calls
addManyOcc :: Var -> UsageDetails -> UsageDetails
addManyOcc v u | isId v    = addManyOccId u v
               | otherwise = u
        -- Give a non-committal binder info (i.e noOccInfo) because
        --   a) Many copies of the specialised thing can appear
        --   b) We don't want to substitute a BIG expression inside a RULE
        --      even if that's the only occurrence of the thing
        --      (Same goes for INLINE.)

addManyOccs :: UsageDetails -> VarSet -> UsageDetails
addManyOccs usage id_set = nonDetStrictFoldUniqSet addManyOcc usage id_set
  -- It's OK to use nonDetStrictFoldUniqSet here because addManyOcc commutes

addLamCoVarOccs :: UsageDetails -> [Var] -> UsageDetails
-- Add any CoVars free in the type of a lambda-binder
-- See Note [Gather occurrences of coercion variables]
addLamCoVarOccs uds bndrs
  = uds `addManyOccs` coVarsOfTypes (map varType bndrs)

delDetails :: UsageDetails -> Id -> UsageDetails
delDetails ud bndr
  = ud `alterUsageDetails` (`delVarEnv` bndr)

delDetailsList :: UsageDetails -> [Id] -> UsageDetails
delDetailsList ud bndrs
  = ud `alterUsageDetails` (`delVarEnvList` bndrs)

emptyDetails :: UsageDetails
emptyDetails = UD { ud_env       = emptyVarEnv
                  , ud_z_many    = emptyVarEnv
                  , ud_z_in_lam  = emptyVarEnv
                  , ud_z_no_tail = emptyVarEnv }

isEmptyDetails :: UsageDetails -> Bool
isEmptyDetails = isEmptyVarEnv . ud_env

markAllMany, markAllInsideLam, markAllNonTail, markAllManyNonTail
  :: UsageDetails -> UsageDetails
markAllMany          ud = ud { ud_z_many    = ud_env ud }
markAllInsideLam     ud = ud { ud_z_in_lam  = ud_env ud }
markAllNonTail ud = ud { ud_z_no_tail = ud_env ud }

markAllInsideLamIf, markAllNonTailIf :: Bool -> UsageDetails -> UsageDetails

markAllInsideLamIf  True  ud = markAllInsideLam ud
markAllInsideLamIf  False ud = ud

markAllNonTailIf True  ud = markAllNonTail ud
markAllNonTailIf False ud = ud


markAllManyNonTail = markAllMany . markAllNonTail -- effectively sets to noOccInfo

markAllManyNonTailIf :: Bool              -- If this is true
             -> UsageDetails      -- Then do markAllManyNonTail on this
             -> UsageDetails
markAllManyNonTailIf True  uds = markAllManyNonTail uds
markAllManyNonTailIf False uds = uds

lookupDetails :: UsageDetails -> Id -> OccInfo
lookupDetails ud id
  = case lookupVarEnv (ud_env ud) id of
      Just occ -> doZapping ud id occ
      Nothing  -> IAmDead

usedIn :: Id -> UsageDetails -> Bool
v `usedIn` ud = isExportedId v || v `elemVarEnv` ud_env ud

udFreeVars :: VarSet -> UsageDetails -> VarSet
-- Find the subset of bndrs that are mentioned in uds
udFreeVars bndrs ud = restrictFreeVars bndrs (ud_env ud)

restrictFreeVars :: VarSet -> OccInfoEnv -> VarSet
restrictFreeVars bndrs fvs = restrictUniqSetToUFM bndrs fvs

-------------------
-- Auxiliary functions for UsageDetails implementation

combineUsageDetailsWith :: (OccInfo -> OccInfo -> OccInfo)
                        -> UsageDetails -> UsageDetails -> UsageDetails
combineUsageDetailsWith plus_occ_info ud1 ud2
  | isEmptyDetails ud1 = ud2
  | isEmptyDetails ud2 = ud1
  | otherwise
  = UD { ud_env       = plusVarEnv_C plus_occ_info (ud_env ud1) (ud_env ud2)
       , ud_z_many    = plusVarEnv (ud_z_many    ud1) (ud_z_many    ud2)
       , ud_z_in_lam  = plusVarEnv (ud_z_in_lam  ud1) (ud_z_in_lam  ud2)
       , ud_z_no_tail = plusVarEnv (ud_z_no_tail ud1) (ud_z_no_tail ud2) }

doZapping :: UsageDetails -> Var -> OccInfo -> OccInfo
doZapping ud var occ
  = doZappingByUnique ud (varUnique var) occ

doZappingByUnique :: UsageDetails -> Unique -> OccInfo -> OccInfo
doZappingByUnique (UD { ud_z_many = many
                      , ud_z_in_lam = in_lam
                      , ud_z_no_tail = no_tail })
                  uniq occ
  = occ2
  where
    occ1 | uniq `elemVarEnvByKey` many    = markMany occ
         | uniq `elemVarEnvByKey` in_lam  = markInsideLam occ
         | otherwise                      = occ
    occ2 | uniq `elemVarEnvByKey` no_tail = markNonTail occ1
         | otherwise                      = occ1

alterUsageDetails :: UsageDetails -> (OccInfoEnv -> OccInfoEnv) -> UsageDetails
alterUsageDetails !ud f
  = UD { ud_env       = f (ud_env       ud)
       , ud_z_many    = f (ud_z_many    ud)
       , ud_z_in_lam  = f (ud_z_in_lam  ud)
       , ud_z_no_tail = f (ud_z_no_tail ud) }

flattenUsageDetails :: UsageDetails -> UsageDetails
flattenUsageDetails ud@(UD { ud_env = env })
  = UD { ud_env       = mapUFM_Directly (doZappingByUnique ud) env
       , ud_z_many    = emptyVarEnv
       , ud_z_in_lam  = emptyVarEnv
       , ud_z_no_tail = emptyVarEnv }

-------------------
-- See Note [Adjusting right-hand sides]
adjustRhsUsage :: RecFlag -> Maybe JoinArity
               -> [CoreBndr]     -- Outer lambdas, AFTER occ anal
               -> UsageDetails   -- From body of lambda
               -> UsageDetails
adjustRhsUsage is_rec mb_join_arity bndrs usage
  = markAllInsideLamIf (not one_shot) $
    markAllNonTailIf (not exact_join) $
    usage
  where
    one_shot = case mb_join_arity of
                 Just join_arity
                   | isRec is_rec -> False
                   | otherwise    -> all isOneShotBndr (drop join_arity bndrs)
                 Nothing          -> all isOneShotBndr bndrs

    exact_join = exactJoin mb_join_arity bndrs

exactJoin :: Maybe JoinArity -> [a] -> Bool
exactJoin Nothing           _    = False
exactJoin (Just join_arity) args = args `lengthIs` join_arity
  -- Remember join_arity includes type binders

type IdWithOccInfo = Id

tagLamBinders :: UsageDetails          -- Of scope
              -> [Id]                  -- Binders
              -> (UsageDetails,        -- Details with binders removed
                 [IdWithOccInfo])    -- Tagged binders
tagLamBinders usage binders
  = usage' `seq` (usage', bndrs')
  where
    (usage', bndrs') = mapAccumR tagLamBinder usage binders

tagLamBinder :: UsageDetails       -- Of scope
             -> Id                 -- Binder
             -> (UsageDetails,     -- Details with binder removed
                 IdWithOccInfo)    -- Tagged binders
-- Used for lambda and case binders
-- It copes with the fact that lambda bindings can have a
-- stable unfolding, used for join points
tagLamBinder usage bndr
  = (usage2, bndr')
  where
        occ    = lookupDetails usage bndr
        bndr'  = setBinderOcc (markNonTail occ) bndr
                   -- Don't try to make an argument into a join point
        usage1 = usage `delDetails` bndr
        usage2 | isId bndr = addManyOccs usage1 (idUnfoldingVars bndr)
                               -- This is effectively the RHS of a
                               -- non-join-point binding, so it's okay to use
                               -- addManyOccsSet, which assumes no tail calls
               | otherwise = usage1

tagNonRecBinder :: TopLevelFlag           -- At top level?
                -> UsageDetails           -- Of scope
                -> CoreBndr               -- Binder
                -> (UsageDetails,         -- Details with binder removed
                    IdWithOccInfo)        -- Tagged binder

tagNonRecBinder lvl usage binder
 = let
     occ     = lookupDetails usage binder
     will_be_join = decideJoinPointHood lvl usage [binder]
     occ'    | will_be_join = -- must already be marked AlwaysTailCalled
                              assert (isAlwaysTailCalled occ) occ
             | otherwise    = markNonTail occ
     binder' = setBinderOcc occ' binder
     usage'  = usage `delDetails` binder
   in
   usage' `seq` (usage', binder')

tagRecBinders :: TopLevelFlag           -- At top level?
              -> UsageDetails           -- Of body of let ONLY
              -> [(CoreBndr,            -- Binder
                   UsageDetails,        -- RHS usage details
                   [CoreBndr])]         -- Lambdas in new RHS
              -> (UsageDetails,         -- Adjusted details for whole scope,
                                        -- with binders removed
                  [IdWithOccInfo])      -- Tagged binders
-- Substantially more complicated than non-recursive case. Need to adjust RHS
-- details *before* tagging binders (because the tags depend on the RHSes).
tagRecBinders lvl body_uds triples
 = let
     (bndrs, rhs_udss, _) = unzip3 triples

     -- 1. Determine join-point-hood of whole group, as determined by
     --    the *unadjusted* usage details
     unadj_uds     = foldr andUDs body_uds rhs_udss
     will_be_joins = decideJoinPointHood lvl unadj_uds bndrs

     -- 2. Adjust usage details of each RHS, taking into account the
     --    join-point-hood decision
     rhs_udss' = map adjust triples
     adjust (bndr, rhs_uds, rhs_bndrs)
       = adjustRhsUsage Recursive mb_join_arity rhs_bndrs rhs_uds
       where
         -- Can't use willBeJoinId_maybe here because we haven't tagged the
         -- binder yet (the tag depends on these adjustments!)
         mb_join_arity
           | will_be_joins
           , let occ = lookupDetails unadj_uds bndr
           , AlwaysTailCalled arity <- tailCallInfo occ
           = Just arity
           | otherwise
           = assert (not will_be_joins) -- Should be AlwaysTailCalled if
             Nothing                   -- we are making join points!

     -- 3. Compute final usage details from adjusted RHS details
     adj_uds   = foldr andUDs body_uds rhs_udss'

     -- 4. Tag each binder with its adjusted details
     bndrs'    = [ setBinderOcc (lookupDetails adj_uds bndr) bndr
                 | bndr <- bndrs ]

     -- 5. Drop the binders from the adjusted details and return
     usage'    = adj_uds `delDetailsList` bndrs
   in
   (usage', bndrs')

setBinderOcc :: OccInfo -> CoreBndr -> CoreBndr
setBinderOcc occ_info bndr
  | isTyVar bndr      = bndr
  | isExportedId bndr = if isManyOccs (idOccInfo bndr)
                          then bndr
                          else setIdOccInfo bndr noOccInfo
            -- Don't use local usage info for visible-elsewhere things
            -- BUT *do* erase any IAmALoopBreaker annotation, because we're
            -- about to re-generate it and it shouldn't be "sticky"

  | otherwise = setIdOccInfo bndr occ_info

-- | Decide whether some bindings should be made into join points or not.
-- Returns `False` if they can't be join points. Note that it's an
-- all-or-nothing decision, as if multiple binders are given, they're
-- assumed to be mutually recursive.
--
-- It must, however, be a final decision. If we say "True" for 'f',
-- and then subsequently decide /not/ make 'f' into a join point, then
-- the decision about another binding 'g' might be invalidated if (say)
-- 'f' tail-calls 'g'.
--
-- See Note [Invariants on join points] in "GHC.Core".
decideJoinPointHood :: TopLevelFlag -> UsageDetails
                    -> [CoreBndr]
                    -> Bool
decideJoinPointHood TopLevel _ _
  = False
decideJoinPointHood NotTopLevel usage bndrs
  | isJoinId (head bndrs)
  = warnPprTrace (not all_ok)
                 (text "OccurAnal failed to rediscover join point(s):" <+> ppr bndrs)
                 all_ok
  | otherwise
  = all_ok
  where
    -- See Note [Invariants on join points]; invariants cited by number below.
    -- Invariant 2 is always satisfiable by the simplifier by eta expansion.
    all_ok = -- Invariant 3: Either all are join points or none are
             all ok bndrs

    ok bndr
      | -- Invariant 1: Only tail calls, all same join arity
        AlwaysTailCalled arity <- tailCallInfo (lookupDetails usage bndr)

      , -- Invariant 1 as applied to LHSes of rules
        all (ok_rule arity) (idCoreRules bndr)

        -- Invariant 2a: stable unfoldings
        -- See Note [Join points and INLINE pragmas]
      , ok_unfolding arity (realIdUnfolding bndr)

        -- Invariant 4: Satisfies polymorphism rule
      , isValidJoinPointType arity (idType bndr)
      = True

      | otherwise
      = False

    ok_rule _ BuiltinRule{} = False -- only possible with plugin shenanigans
    ok_rule join_arity (Rule { ru_args = args })
      = args `lengthIs` join_arity
        -- Invariant 1 as applied to LHSes of rules

    -- ok_unfolding returns False if we should /not/ convert a non-join-id
    -- into a join-id, even though it is AlwaysTailCalled
    ok_unfolding join_arity (CoreUnfolding { uf_src = src, uf_tmpl = rhs })
      = not (isStableSource src && join_arity > joinRhsArity rhs)
    ok_unfolding _ (DFunUnfolding {})
      = False
    ok_unfolding _ _
      = True

willBeJoinId_maybe :: CoreBndr -> Maybe JoinArity
willBeJoinId_maybe bndr
  | isId bndr
  , AlwaysTailCalled arity <- tailCallInfo (idOccInfo bndr)
  = Just arity
  | otherwise
  = isJoinId_maybe bndr


{- Note [Join points and INLINE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f x = let g = \x. not  -- Arity 1
             {-# INLINE g #-}
         in case x of
              A -> g True True
              B -> g True False
              C -> blah2

Here 'g' is always tail-called applied to 2 args, but the stable
unfolding captured by the INLINE pragma has arity 1.  If we try to
convert g to be a join point, its unfolding will still have arity 1
(since it is stable, and we don't meddle with stable unfoldings), and
Lint will complain (see Note [Invariants on join points], (2a), in
GHC.Core.  #13413.

Moreover, since g is going to be inlined anyway, there is no benefit
from making it a join point.

If it is recursive, and uselessly marked INLINE, this will stop us
making it a join point, which is annoying.  But occasionally
(notably in class methods; see Note [Instances and loop breakers] in
GHC.Tc.TyCl.Instance) we mark recursive things as INLINE but the recursion
unravels; so ignoring INLINE pragmas on recursive things isn't good
either.

See Invariant 2a of Note [Invariants on join points] in GHC.Core


************************************************************************
*                                                                      *
\subsection{Operations over OccInfo}
*                                                                      *
************************************************************************
-}

markMany, markInsideLam, markNonTail :: OccInfo -> OccInfo

markMany IAmDead = IAmDead
markMany occ     = ManyOccs { occ_tail = occ_tail occ }

markInsideLam occ@(OneOcc {}) = occ { occ_in_lam = IsInsideLam }
markInsideLam occ             = occ

markNonTail IAmDead = IAmDead
markNonTail occ     = occ { occ_tail = NoTailCallInfo }

addOccInfo, orOccInfo :: OccInfo -> OccInfo -> OccInfo

addOccInfo a1 a2  = assert (not (isDeadOcc a1 || isDeadOcc a2)) $
                    ManyOccs { occ_tail = tailCallInfo a1 `andTailCallInfo`
                                          tailCallInfo a2 }
                                -- Both branches are at least One
                                -- (Argument is never IAmDead)

-- (orOccInfo orig new) is used
-- when combining occurrence info from branches of a case

orOccInfo (OneOcc { occ_in_lam  = in_lam1
                  , occ_n_br    = nbr1
                  , occ_int_cxt = int_cxt1
                  , occ_tail    = tail1 })
          (OneOcc { occ_in_lam  = in_lam2
                  , occ_n_br    = nbr2
                  , occ_int_cxt = int_cxt2
                  , occ_tail    = tail2 })
  = OneOcc { occ_n_br    = nbr1 + nbr2
           , occ_in_lam  = in_lam1 `mappend` in_lam2
           , occ_int_cxt = int_cxt1 `mappend` int_cxt2
           , occ_tail    = tail1 `andTailCallInfo` tail2 }

orOccInfo a1 a2 = assert (not (isDeadOcc a1 || isDeadOcc a2)) $
                  ManyOccs { occ_tail = tailCallInfo a1 `andTailCallInfo`
                                        tailCallInfo a2 }

andTailCallInfo :: TailCallInfo -> TailCallInfo -> TailCallInfo
andTailCallInfo info@(AlwaysTailCalled arity1) (AlwaysTailCalled arity2)
  | arity1 == arity2 = info
andTailCallInfo _ _  = NoTailCallInfo
