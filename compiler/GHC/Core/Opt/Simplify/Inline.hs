{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

This module contains inlining logic used by the simplifier.
-}


module GHC.Core.Opt.Simplify.Inline (
        -- * Cheap and cheerful inlining checks.
        couldBeSmallEnoughToInline,
        smallEnoughToInline, activeUnfolding,

        -- * The smart inlining decisions are made by callSiteInline
        callSiteInline, CallCtxt(..),
    ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Core.Opt.Simplify.Env

import GHC.Core
import GHC.Core.Unfold
import GHC.Core.FVs( exprFreeIds )

import GHC.Types.Id
import GHC.Types.Var.Env( InScopeSet, lookupInScope )
import GHC.Types.Var.Set
import GHC.Types.Basic  ( Arity, RecFlag(..), isActive )
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Types.Name

import Data.List (isPrefixOf)

{-
************************************************************************
*                                                                      *
\subsection[considerUnfolding]{Given all the info, do (not) do the unfolding}
*                                                                      *
************************************************************************

We use 'couldBeSmallEnoughToInline' to avoid exporting inlinings that
we ``couldn't possibly use'' on the other side.  Can be overridden w/
flaggery.  Just the same as smallEnoughToInline, except that it has no
actual arguments.
-}

couldBeSmallEnoughToInline :: UnfoldingOpts -> Int -> CoreExpr -> Bool
couldBeSmallEnoughToInline opts threshold rhs
  = case sizeExpr opts threshold [] body of
       TooBig -> False
       _      -> True
  where
    (_, body) = collectBinders rhs

----------------
smallEnoughToInline :: UnfoldingOpts -> Unfolding -> Bool
smallEnoughToInline opts (CoreUnfolding {uf_guidance = guidance})
  = case guidance of
       UnfIfGoodArgs {ug_size = size} -> size <= unfoldingUseThreshold opts
       UnfWhen {} -> True
       UnfNever   -> False
smallEnoughToInline _ _
  = False

{-
************************************************************************
*                                                                      *
\subsection{callSiteInline}
*                                                                      *
************************************************************************

This is the key function.  It decides whether to inline a variable at a call site

callSiteInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.
A non-WHNF can be inlined if it doesn't occur inside a lambda,
and occurs exactly once or
    occurs once in each branch of a case and is small

If the thing is in WHNF, there's no danger of duplicating work,
so we can inline if it occurs once, or is small

NOTE: we don't want to inline top-level functions that always diverge.
It just makes the code bigger.  Tt turns out that the convenient way to prevent
them inlining is to give them a NOINLINE pragma, which we do in
StrictAnal.addStrictnessInfoToTopId
-}

callSiteInline :: SimplEnv
               -> Logger
               -> Id                    -- The Id
               -> Bool                  -- True if there are no arguments at all (incl type args)
               -> [ArgSummary]          -- One for each value arg; True if it is interesting
               -> CallCtxt              -- True <=> continuation is interesting
               -> Maybe CoreExpr        -- Unfolding, if any
callSiteInline env logger id lone_variable arg_infos cont_info
  = case idUnfolding id of
      -- idUnfolding checks for loop-breakers, returning NoUnfolding
      -- Things with an INLINE pragma may have an unfolding *and*
      -- be a loop breaker  (maybe the knot is not yet untied)
        CoreUnfolding { uf_tmpl = unf_template
                      , uf_cache = unf_cache
                      , uf_guidance = guidance }
          | active_unf -> tryUnfolding env logger id lone_variable
                                    arg_infos cont_info unf_template
                                    unf_cache guidance
          | otherwise -> traceInline logger uf_opts id "Inactive unfolding:" (ppr id) Nothing
        NoUnfolding      -> Nothing
        BootUnfolding    -> Nothing
        OtherCon {}      -> Nothing
        DFunUnfolding {} -> Nothing     -- Never unfold a DFun
  where
    uf_opts    = seUnfoldingOpts env
    active_unf = activeUnfolding (seMode env) id

activeUnfolding :: SimplMode -> Id -> Bool
activeUnfolding mode id
  | isCompulsoryUnfolding (realIdUnfolding id)
  = True   -- Even sm_inline can't override compulsory unfoldings
  | otherwise
  = isActive (sm_phase mode) (idInlineActivation id)
  && sm_inline mode
      -- `or` isStableUnfolding (realIdUnfolding id)
      -- Inline things when
      --  (a) they are active
      --  (b) sm_inline says so, except that for stable unfoldings
      --                         (ie pragmas) we inline anyway

-- | Report the inlining of an identifier's RHS to the user, if requested.
traceInline :: Logger -> UnfoldingOpts -> Id -> String -> SDoc -> a -> a
traceInline logger opts inline_id str doc result
  -- We take care to ensure that doc is used in only one branch, ensuring that
  -- the simplifier can push its allocation into the branch. See Note [INLINE
  -- conditional tracing utilities].
  | enable    = logTraceMsg logger str doc result
  | otherwise = result
  where
    enable
      | logHasDumpFlag logger Opt_D_dump_verbose_inlinings
      = True
      | Just prefix <- unfoldingReportPrefix opts
      = prefix `isPrefixOf` occNameString (getOccName inline_id)
      | otherwise
      = False
{-# INLINE traceInline #-} -- see Note [INLINE conditional tracing utilities]

{- Note [Avoid inlining into deeply nested cases]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Also called "exponential inlining".

Consider a function f like this: (#18730)

  f arg1 arg2 =
    case ...
      ... -> g arg1
      ... -> g arg2

This function is small. So should be safe to inline.
However sometimes this doesn't quite work out like that.
Consider this code:

    f1 arg1 arg2 ... = ...
        case _foo of
          alt1 -> ... f2 arg1 ...
          alt2 -> ... f2 arg2 ...

    f2 arg1 arg2 ... = ...
        case _foo of
          alt1 -> ... f3 arg1 ...
          alt2 -> ... f3 arg2 ...

    f3 arg1 arg2 ... = ...

    ... repeats up to n times. And then f1 is
    applied to some arguments:

    foo = ... f1 <interestingArgs> ...

Initially f2..fn are not interesting to inline so we don't.  However we see
that f1 is applied to interesting args.  So it's an obvious choice to inline
those:

    foo = ...
          case _foo of
            alt1 -> ... f2 <interestingArg> ...
            alt2 -> ... f2 <interestingArg> ...

As a result we go and inline f2 both mentions of f2 in turn are now applied to
interesting arguments and f2 is small:

    foo = ...
          case _foo of
            alt1 -> ... case _foo of
                alt1 -> ... f3 <interestingArg> ...
                alt2 -> ... f3 <interestingArg> ...

            alt2 -> ... case _foo of
                alt1 -> ... f3 <interestingArg> ...
                alt2 -> ... f3 <interestingArg> ...

The same thing happens for each binding up to f_n, duplicating the amount of inlining
done in each step. Until at some point we are either done or run out of simplifier
ticks/RAM. This pattern happened #18730.

To combat this we introduce one more heuristic when weighing inlining decision.
We keep track of a "case-depth". Which increases each time we look inside a case
expression with more than one alternative.

We then apply a penalty to inlinings based on the case-depth at which they would
be inlined. Bounding the number of inlinings in such a scenario.

The heuristic can be tuned in two ways:

* We can ignore the first n levels of case nestings for inlining decisions using
  -funfolding-case-threshold.

* The penalty grows linear with the depth. It's computed as
     size*(depth-threshold)/scaling.
  Scaling can be set with -funfolding-case-scaling.

Reflections and wrinkles

* See also Note [Do not add unfoldings to join points at birth] in
  GHC.Core.Opt.Simplify.Iteration

* The total case depth is really the wrong thing; it will inhibit inlining of a
  local function, just because there is some giant case nest further out.  What we
  want is the /difference/ in case-depth between the binding site and the call site.
  That could be done quite easily by adding the case-depth to the Unfolding of the
  function.

* What matters more than /depth/ is total /width/; that is how many alternatives
  are in the tree.  We could perhaps multiply depth by width at each case expression.

* There might be a case nest with many alternatives, but the function is called in
  only a handful of them.  So maybe we should ignore case-depth, and instead penalise
  funtions that are called many times -- after all, inlining them bloats code.

  But in the scenario above, we are simplifying an inlined fuction, without doing a
  global occurrence analysis each time.  So if we based the penalty on multiple
  occurences, we should /also/ add a penalty when simplifying an already-simplified
  expression.  We do track this (seInlineDepth) but currently we barely use it.

  An advantage of using occurrences+inline depth is that it'll work when no
  case expressions are involved.  See #15488.

* Test T18730 did not involve join points.  But join points are very prone to
  the same kind of thing.  For exampe in #13253, and several related tickets,
  we got an exponential blowup in code size from a program that looks like
  this.

  let j1a x = case f y     of { True -> p;   False -> q }
      j1b x = case f y     of { True -> q;   False -> p }
      j2a x = case f (y+1) of { True -> j1a x; False -> j1b x}
      j2b x = case f (y+1) of { True -> j1b x; False -> j1a x}
      ...
  in case f (y+10) of { True -> j10a 7; False -> j10b 8 }

  The first danger is this: in Simplifier iteration 1 postInlineUnconditionally
  inlines the last functions, j10a and j10b (they are both small).  Now we have
  two calls to j9a and two to j9b.  In the next Simplifer iteration,
  postInlineUnconditionally inlines all four of these calls, leaving four calls
  to j8a and j8b. Etc.

  Happily, this probably /won't/ happen because the Simplifier works top down, so it'll
  inline j1a/j1b into j2a/j2b, which will make the latter bigger; so the process
  will stop.  But we still need to stop the inline cascade described at the head
  of this Note.

Some guidance on setting these defaults:

* A low threshold (<= 2) is needed to prevent exponential cases from spiraling out of
  control. We picked 2 for no particular reason.

* Scaling the penalty by any more than 30 means the reproducer from
  T18730 won't compile even with reasonably small values of n. Instead
  it will run out of runs/ticks. This means to positively affect the reproducer
  a scaling <= 30 is required.

* A scaling of >= 15 still causes a few very large regressions on some nofib benchmarks.
  (+80% for gc/fulsom, +90% for real/ben-raytrace, +20% for spectral/fibheaps)

* A scaling of >= 25 showed no regressions on nofib. However it showed a number of
  (small) regression for compiler perf benchmarks.

The end result is that we are settling for a scaling of 30, with a threshold of 2.
This gives us minimal compiler perf regressions. No nofib runtime regressions and
will still avoid this pattern sometimes. This is a "safe" default, where we err on
the side of compiler blowup instead of risking runtime regressions.

For cases where the default falls short the flag can be changed to allow
more/less inlining as needed on a per-module basis.

-}

tryUnfolding :: SimplEnv -> Logger -> Id -> Bool -> [ArgSummary] -> CallCtxt
             -> CoreExpr -> UnfoldingCache -> UnfoldingGuidance
             -> Maybe CoreExpr
tryUnfolding env logger id lone_variable arg_infos
             cont_info unf_template unf_cache guidance
 = case guidance of
     UnfNever -> traceInline logger opts id str (text "UnfNever") Nothing

     UnfWhen { ug_arity = uf_arity, ug_unsat_ok = unsat_ok, ug_boring_ok = boring_ok }
        | enough_args && (boring_ok || some_benefit || unfoldingVeryAggressive opts)
                -- See Note [INLINE for small functions] (3)
        -> traceInline logger opts id str (mk_doc some_benefit empty True) (Just unf_template)
        | otherwise
        -> traceInline logger opts id str (mk_doc some_benefit empty False) Nothing
        where
          some_benefit = calc_some_benefit uf_arity True
          enough_args  = (n_val_args >= uf_arity) || (unsat_ok && n_val_args > 0)

     UnfIfGoodArgs { ug_args = arg_discounts, ug_res = res_discount, ug_size = size }
        | isJoinId id, small_enough         -> inline_join_point
        | unfoldingVeryAggressive opts      -> yes
        | is_wf, some_benefit, small_enough -> yes
        | otherwise                         -> no
        where
          yes = traceInline logger opts id str (mk_doc some_benefit extra_doc True)  (Just unf_template)
          no  = traceInline logger opts id str (mk_doc some_benefit extra_doc False) Nothing

          some_benefit = calc_some_benefit (length arg_discounts) False

          -- depth_penalty: see Note [Avoid inlining into deeply nested cases]
          depth_threshold = unfoldingCaseThreshold opts
          depth_scaling   = unfoldingCaseScaling opts
          depth_penalty | case_depth <= depth_threshold = 0
                        | otherwise = (size * (case_depth - depth_threshold)) `div` depth_scaling

          adjusted_size = size + depth_penalty - discount
          small_enough = adjusted_size <= unfoldingUseThreshold opts
          discount = computeDiscount arg_discounts res_discount arg_infos cont_info

          extra_doc = vcat [ ppWhen (isJoinId id) $
                             text "join" <+> fsep [ ppr (v, hasCoreUnfolding (idUnfolding v)
                                                        , fmap (isEvaldUnfolding . idUnfolding) (lookupInScope in_scope v)
                                                        , is_more_evald in_scope v)
                                                  | v <- vselems (exprFreeIds unf_template) ]
                           , text "depth based penalty =" <+> int depth_penalty
                           , text "adjusted size =" <+> int adjusted_size ]

          inline_join_point  -- See Note [Inlining join points]
            | or (zipWith scrut_arg arg_discounts arg_infos) = yes
            | anyVarSet (is_more_evald in_scope) $
              exprFreeIds unf_template                       = yes
            | otherwise                                      = no
          -- scrut_arg is True if the function body has a discount and the arg is a value
          scrut_arg disc ValueArg = disc > 0
          scrut_arg _    _        = False

  where
    opts         = seUnfoldingOpts env
    case_depth   = seCaseDepth env
    inline_depth = seInlineDepth env
    in_scope     = seInScope env

    -- Unpack the UnfoldingCache lazily because it may not be needed, and all
    -- its fields are strict; so evaluating unf_cache at all forces all the
    -- isWorkFree etc computations to take place.  That risks wasting effort for
    -- Ids that are never going to inline anyway.
    -- See Note [UnfoldingCache] in GHC.Core
    UnfoldingCache{ uf_is_work_free = is_wf, uf_expandable = is_exp } = unf_cache

    mk_doc some_benefit extra_doc yes_or_no
      = vcat [ text "arg infos" <+> ppr arg_infos
             , text "interesting continuation" <+> ppr cont_info
             , text "some_benefit" <+> ppr some_benefit
             , text "is exp:" <+> ppr is_exp
             , text "is work-free:" <+> ppr is_wf
             , text "guidance" <+> ppr guidance
             , text "case depth =" <+> int case_depth
             , text "inline depth =" <+> int inline_depth
             , extra_doc
             , text "ANSWER =" <+> if yes_or_no then text "YES" else text "NO"]

    ctx = log_default_dump_context (logFlags logger)
    str = "Considering inlining: " ++ showSDocOneLine ctx (ppr id)
    n_val_args = length arg_infos

           -- some_benefit is used when the RHS is small enough
           -- and the call has enough (or too many) value
           -- arguments (ie n_val_args >= arity). But there must
           -- be *something* interesting about some argument, or the
           -- result context, to make it worth inlining
    calc_some_benefit :: Arity -> Bool -> Bool   -- The Arity is the number of args
                                         -- expected by the unfolding
    calc_some_benefit uf_arity is_inline
       | not saturated = interesting_args       -- Under-saturated
                                        -- Note [Unsaturated applications]
       | otherwise = interesting_args   -- Saturated or over-saturated
                  || interesting_call
      where
        saturated      = n_val_args >= uf_arity
        over_saturated = n_val_args > uf_arity
        interesting_args = any nonTriv arg_infos
                -- NB: (any nonTriv arg_infos) looks at the
                -- over-saturated args too which is "wrong";
                -- but if over-saturated we inline anyway.

        interesting_call
          | over_saturated
          = True
          | otherwise
          = case cont_info of
              CaseCtxt   -> not (lone_variable && is_exp)  -- Note [Lone variables]
              ValAppCtxt -> True                           -- Note [Cast then apply]
              RuleArgCtxt -> uf_arity > 0  -- See Note [RHS of lets]
              DiscArgCtxt -> uf_arity > 0  -- Note [Inlining in ArgCtxt]
              RhsCtxt NonRecursive | is_inline
                          -> uf_arity > 0  -- See Note [RHS of lets]
              _other      -> False         -- See Note [Nested functions]


vselems :: VarSet -> [Var]
vselems s = nonDetStrictFoldVarSet (\v vs -> v : vs) [] s

is_more_evald :: InScopeSet -> Id -> Bool
-- See Note [Inlining join points]
is_more_evald in_scope v
  | Just v1 <- lookupInScope in_scope v
  , idUnfolding v1 `isBetterUnfoldingThan` idUnfolding v
  = True
  | otherwise
  = False

{- Note [RHS of lets]
~~~~~~~~~~~~~~~~~~~~~
When the call is the argument of a function with a RULE, or the RHS of a let,
we are a little bit keener to inline (in tryUnfolding).  For example
     f y = (y,y,y)
     g y = let x = f y in ...(case x of (a,b,c) -> ...) ...
We'd inline 'f' if the call was in a case context, and it kind-of-is,
only we can't see it.  Also
     x = f v
could be expensive whereas
     x = case v of (a,b) -> a
is patently cheap and may allow more eta expansion.

So, in `interesting_call` in `tryUnfolding`, we treat the RHS of a
/non-recursive/ let as not-totally-boring.  A /recursive/ let isn't
going be inlined so there is much less point.  Hence the (only reason
for the) RecFlag in RhsCtxt

We inline only if `f` has an `UnfWhen` guidance.  I found that being more eager
led to fruitless inlining.  See Note [Seq is boring] wrinkle (SB1) in
GHC.Core.Opt.Simplify.Utils.

Note [Inlining join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we /do not/ want to inline join points /even if they are small/.
See Note [Duplicating join points] in GHC.Core.Opt.Simplify.Iteration.

But, assuming it is small, there are various times when we /do/ want to
inline a (non-recursive) join point.  Namely, if either of these hold:

(1) A /scrutinised/ argument (non-zero discount) has a /ValueArg/ info.
    Inlining will give some benefit.

(2) A free variable of the RHS is
    * Is /not/ evaluated at the join point defn site
    * Is evaluated at the join point call site.
    This is the is_more_evald predicate.

(1) is fairly obvious but (2) is less so. Here is the code for `integerGT`
without (2):

  integerGt = \ (x :: Integer) (y :: Integer) ->
     join fail _ = case x of {
       IS x1 -> case y of {
           IS y1 -> case <# x1 y1  of
                      _DEFAULT -> case ==# x1 y1 of
                                    DEFAULT -> True;
                                    1#      -> False
                      1# -> False
           IP ds1 -> False
           IN ds1 -> True

       IP x1 -> case y of {
                 _DEFAULT -> True;
                 IP y1    -> case bigNatCompare x1 y1 of
                               _DEFAULT -> False;
                               GT -> True
       IN x1 -> case y of {
                  _DEFAULT -> False;
                  IN y1    -> case bigNatCompare y1 x1 of
                                _DEFAULT -> False;
                                GT -> True
     in case x of {
       _DEFAULT -> jump fail GHC.Prim.(##);
       IS x1    -> case y of {
                     _DEFAULT -> jump fail GHC.Prim.(##);
                     IS y1 -> tagToEnum# @Bool (># x1 y1)

If we inline `fail` we get /much/ better code.  The only clue is that
`x` and `y` (a) are not evaluated at the definition site, and (b) are
evaluated at the call site.  This predicate is `isBetterUnfoldingThan`.

You might think that the variable should also be /scrutinised/ in the
join-point RHS, but here are two reasons for not taking that into
account.

First, we see code somewhat like this in imaginary/wheel-sieve1:
    let x = <small thunk> in
    join $j = (x,y) in
    case z of
      A -> case x of
             P -> $j
             Q -> blah
      B -> (x,x)
      C -> True
Here `x` can't be duplicated into the branches becuase it is used
in both the join point and the A branch.  But if we inline $j we get
    let x = <small thunk> in
    case z of
      A -> case x of x'
             P -> (x', y)
             Q -> blah
      B -> x
      C -> True
and now we /can/ duplicate x into the branches, at which point:
  * it is used strictly in the A branch (evaluated, but no thunk)
  * it is used lazily in the B branch (still a thunk)
  * it is not used at all in the C branch (no thunk)

Second, spectral/treejoin gets a big win from SpecConstr due
to evaluated-ness. Something like this:
    join $j x = ...(foo fv)...
    in case fv of I# x ->
       ...  jump $j True ...
If we inline $j, SpecConstr sees a call (foo (I# x)) and specialises.

Note [Unsaturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a call is not saturated, we *still* inline if one of the
arguments has interesting structure.  That's sometimes very important.
A good example is the Ord instance for Bool in Base:

 Rec {
    $fOrdBool =GHC.Classes.D:Ord
                 @ Bool
                 ...
                 $cmin_ajX

    $cmin_ajX [Occ=LoopBreaker] :: Bool -> Bool -> Bool
    $cmin_ajX = GHC.Classes.$dmmin @ Bool $fOrdBool
  }

But the defn of GHC.Classes.$dmmin is:

  $dmmin :: forall a. GHC.Classes.Ord a => a -> a -> a
    {- Arity: 3, HasNoCafRefs, Strictness: SLL,
       Unfolding: (\ @ a $dOrd :: GHC.Classes.Ord a x :: a y :: a ->
                   case @ a GHC.Classes.<= @ a $dOrd x y of wild {
                     GHC.Types.False -> y GHC.Types.True -> x }) -}

We *really* want to inline $dmmin, even though it has arity 3, in
order to unravel the recursion.


Note [Things to watch]
~~~~~~~~~~~~~~~~~~~~~~
*   { y = I# 3; x = y `cast` co; ...case (x `cast` co) of ... }
    Assume x is exported, so not inlined unconditionally.
    Then we want x to inline unconditionally; no reason for it
    not to, and doing so avoids an indirection.

*   { x = I# 3; ....f x.... }
    Make sure that x does not inline unconditionally!
    Lest we get extra allocation.

Note [Nested functions]
~~~~~~~~~~~~~~~~~~~~~~~
At one time we treated a call of a non-top-level function as
"interesting" (regardless of how boring the context) in the hope
that inlining it would eliminate the binding, and its allocation.
Specifically, in the default case of interesting_call we had
   _other -> not is_top && uf_arity > 0

But actually postInlineUnconditionally does some of this and overall
it makes virtually no difference to nofib.  So I simplified away this
special case

Note [Cast then apply]
~~~~~~~~~~~~~~~~~~~~~~
Consider
   myIndex = __inline_me ( (/\a. <blah>) |> co )
   co :: (forall a. a -> a) ~ (forall a. T a)
     ... /\a.\x. case ((myIndex a) |> sym co) x of { ... } ...

We need to inline myIndex to unravel this; but the actual call (myIndex a) has
no value arguments.  The ValAppCtxt gives it enough incentive to inline.

Note [Inlining in ArgCtxt]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The condition (arity > 0) here is very important, because otherwise
we end up inlining top-level stuff into useless places; eg
   x = I# 3#
   f = \y.  g x
This can make a very big difference: it adds 16% to nofib 'integer' allocs,
and 20% to 'power'.

At one stage I replaced this condition by 'True' (leading to the above
slow-down).  The motivation was test eyeball/inline1.hs; but that seems
to work ok now.

NOTE: arguably, we should inline in ArgCtxt only if the result of the
call is at least CONLIKE.  At least for the cases where we use ArgCtxt
for the RHS of a 'let', we only profit from the inlining if we get a
CONLIKE thing (modulo lets).

Note [Lone variables]
~~~~~~~~~~~~~~~~~~~~~
See also Note [Interaction of exprIsWorkFree and lone variables]
which appears below

The "lone-variable" case is important.  I spent ages messing about
with unsatisfactory variants, but this is nice.  The idea is that if a
variable appears all alone

        as an arg of lazy fn, or rhs    BoringCtxt
        as scrutinee of a case          CaseCtxt
        as arg of a fn                  ArgCtxt
AND
        it is bound to a cheap expression

then we should not inline it (unless there is some other reason,
e.g. it is the sole occurrence).  That is what is happening at
the use of 'lone_variable' in 'interesting_call'.

Why?  At least in the case-scrutinee situation, turning
        let x = (a,b) in case x of y -> ...
into
        let x = (a,b) in case (a,b) of y -> ...
and thence to
        let x = (a,b) in let y = (a,b) in ...
is bad if the binding for x will remain.

Another example: I discovered that strings
were getting inlined straight back into applications of 'error'
because the latter is strict.
        s = "foo"
        f = \x -> ...(error s)...

Fundamentally such contexts should not encourage inlining because, provided
the RHS is "expandable" (see Note [exprIsExpandable] in GHC.Core.Utils) the
context can ``see'' the unfolding of the variable (e.g. case or a
RULE) so there's no gain.

However, watch out:

 * Consider this:
        foo = \n. [n])  {-# INLINE foo #-}
        bar = foo 20    {-# INLINE bar #-}
        baz = \n. case bar of { (m:_) -> m + n }
   Here we really want to inline 'bar' so that we can inline 'foo'
   and the whole thing unravels as it should obviously do.  This is
   important: in the NDP project, 'bar' generates a closure data
   structure rather than a list.

   So the non-inlining of lone_variables should only apply if the
   unfolding is regarded as expandable; because that is when
   exprIsConApp_maybe looks through the unfolding.  Hence the "&&
   is_exp" in the CaseCtxt branch of interesting_call

 * Even a type application or coercion isn't a lone variable.
   Consider
        case $fMonadST @ RealWorld of { :DMonad a b c -> c }
   We had better inline that sucker!  The case won't see through it.

   For now, I'm treating treating a variable applied to types
   in a *lazy* context "lone". The motivating example was
        f = /\a. \x. BIG
        g = /\a. \y.  h (f a)
   There's no advantage in inlining f here, and perhaps
   a significant disadvantage.  Hence some_val_args in the Stop case

Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The lone-variable test says "don't inline if a case expression
scrutinises a lone variable whose unfolding is cheap".  It's very
important that, under these circumstances, exprIsConApp_maybe
can spot a constructor application. So, for example, we don't
consider
        let x = e in (x,x)
to be cheap, and that's good because exprIsConApp_maybe doesn't
think that expression is a constructor application.

In the 'not (lone_variable && is_wf)' test, I used to test is_value
rather than is_wf, which was utterly wrong, because the above
expression responds True to exprIsHNF, which is what sets is_value.

This kind of thing can occur if you have

        {-# INLINE foo #-}
        foo = let x = e in (x,x)

which Roman did.


-}

computeDiscount :: [Int] -> Int -> [ArgSummary] -> CallCtxt
                -> Int
computeDiscount arg_discounts res_discount arg_infos cont_info

  = 10          -- Discount of 10 because the result replaces the call
                -- so we count 10 for the function itself

    + 10 * length actual_arg_discounts
               -- Discount of 10 for each arg supplied,
               -- because the result replaces the call

    + total_arg_discount + res_discount'
  where
    actual_arg_discounts = zipWith mk_arg_discount arg_discounts arg_infos
    total_arg_discount   = sum actual_arg_discounts

    mk_arg_discount _        TrivArg    = 0
    mk_arg_discount _        NonTrivArg = 10
    mk_arg_discount discount ValueArg   = discount

    res_discount'
      | LT <- arg_discounts `compareLength` arg_infos
      = res_discount   -- Over-saturated
      | otherwise
      = case cont_info of
           BoringCtxt  -> 0
           CaseCtxt    -> res_discount  -- Presumably a constructor
           ValAppCtxt  -> res_discount  -- Presumably a function
           _           -> 40 `min` res_discount
                -- ToDo: this 40 `min` res_discount doesn't seem right
                --   for DiscArgCtxt it shouldn't matter because the function will
                --       get the arg discount for any non-triv arg
                --   for RuleArgCtxt we do want to be keener to inline; but not only
                --       constructor results
                --   for RhsCtxt I suppose that exposing a data con is good in general
                --   And 40 seems very arbitrary
                --
                -- res_discount can be very large when a function returns
                -- constructors; but we only want to invoke that large discount
                -- when there's a case continuation.
                -- Otherwise we, rather arbitrarily, threshold it.  Yuk.
                -- But we want to avoid inlining large functions that return
                -- constructors into contexts that are simply "interesting"
