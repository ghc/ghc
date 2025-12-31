{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

This module contains inlining logic used by the simplifier.
-}



module GHC.Core.Opt.Simplify.Inline (
        -- * The smart inlining decisions are made by callSiteInline
        callSiteInline, activeUnfolding,
        exprDigest
    ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Core.Opt.Simplify.Env
import GHC.Core.Opt.Simplify.Utils
import GHC.Core
import GHC.Core.Unfold
import GHC.Core.FVs( exprFreeIds )

import GHC.Types.Id
import GHC.Types.Literal ( isLitRubbish )
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Basic  ( Arity, RecFlag(..) )
import GHC.Types.Name

import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.List ( isPrefixOf )

{-
************************************************************************
*                                                                      *
                  callSiteInline
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

callSiteInline :: Logger -> SimplEnv
               -> Id -> SimplCont
               -> Maybe CoreExpr        -- Unfolding, if any
callSiteInline logger env fn cont
  = case idUnfolding fn of
      -- idUnfolding checks for loop-breakers, returning NoUnfolding
      -- Things with an INLINE pragma may have an unfolding *and*
      -- be a loop breaker  (maybe the knot is not yet untied)
        CoreUnfolding { uf_tmpl = unf_template
                      , uf_cache = unf_cache
                      , uf_guidance = guidance }
          | active_unf -> tryUnfolding logger env fn cont unf_template unf_cache guidance
          | otherwise  -> traceInline logger env fn "Inactive unfolding:" (ppr fn) Nothing
        NoUnfolding      -> Nothing
        BootUnfolding    -> Nothing
        OtherCon {}      -> Nothing
        DFunUnfolding {} -> Nothing     -- Never unfold a DFun
  where
    active_unf = activeUnfolding (seMode env) fn

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
traceInline :: Logger -> SimplEnv -> Id -> String -> SDoc -> a -> a
traceInline logger env inline_id str doc result
  -- We take care to ensure that doc is used in only one branch, ensuring that
  -- the simplifier can push its allocation into the branch. See Note [INLINE
  -- conditional tracing utilities].
  | enable    = logTraceMsg logger str doc result
  | otherwise = result
  where
    opts = seUnfoldingOpts env
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
  the same kind of thing.  For example in #13253, and several related tickets,
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

tryUnfolding :: Logger -> SimplEnv -> Id -> SimplCont
             -> CoreExpr -> UnfoldingCache -> UnfoldingGuidance
             -> Maybe CoreExpr
tryUnfolding logger env fn cont unf_template unf_cache guidance
 = case guidance of
     UnfNever -> traceInline logger env fn str (text "UnfNever") Nothing

     UnfWhen { ug_arity = uf_arity, ug_unsat_ok = unsat_ok, ug_boring_ok = boring_ok }
        | enough_args && (boring_ok || some_benefit || unfoldingVeryAggressive opts)
                -- See Note [INLINE for small functions] (3)
        -> traceInline logger env fn str (mk_doc some_benefit empty True) (Just unf_template)
        | otherwise
        -> traceInline logger env fn str (mk_doc some_benefit empty False) Nothing
        where
          some_benefit = calc_some_benefit uf_arity True
          enough_args  = (n_val_args >= uf_arity) || (unsat_ok && n_val_args > 0)

     UnfIfGoodArgs { ug_args = val_arg_bndrs
                   , ug_tree = expr_tree@(ExprTree { et_ret = ret_discount}) }
        | isJoinId fn, small_enough         -> inline_join_point
        | unfoldingVeryAggressive opts      -> yes
        | is_wf, some_benefit, small_enough -> yes
        | otherwise                         -> no
        where
          yes = traceInline logger env fn str (mk_doc some_benefit extra_doc True)  (Just unf_template)
          no  = traceInline logger env fn str (mk_doc some_benefit extra_doc False) Nothing

          inline_join_point  -- See Note [Inlining join points]
            | any hasArgInfo arg_infos                       = yes   -- This is pretty aggressive
            | anyVarSet (is_more_evald in_scope) $
              exprFreeIds unf_template                       = yes
            | otherwise                                      = no

          n_bndrs       = length val_arg_bndrs
          some_benefit  = calc_some_benefit n_bndrs False
          small_enough  = adjusted_size <= unfoldingUseThreshold opts
          rhs_size      = exprTreeSize context expr_tree
          adjusted_size = rhs_size - call_size_discount - actual_ret_discount + depth_penalty

          --------  Compute the size of the ExprTree in this context -----------
          context = IC { ic_bound = bound_env
                       , ic_free  = getFreeDigest }

          bound_env = mkVarEnv (val_arg_bndrs `zip` (arg_infos ++ repeat ArgNoInfo))
                      -- Crucial to include /all/ val_arg_bndrs, lest we treat
                      -- them as free and use ic_free instead

          in_scope = seInScope env
          getFreeDigest :: Id -> ArgDigest -- The ArgDigest of a free variable
          getFreeDigest x
            = case lookupInScope in_scope x of
                Just x' | warnPprTrace (not (isId x')) "GFS" (vcat
                            [ ppr fn <+> equals <+> ppr unf_template
                            , text "expr_tree:" <+> ppr expr_tree
                            , ppr x <+> ppr x'
                           ]) True
                        , Just expr <- maybeUnfoldingTemplate (idUnfolding x')
                        -> exprDigest env expr
                _ -> ArgNoInfo

          -------- Size adjustements ----------------
          -- Subtract size of the call, because the result replaces the call
          -- We count 10 for the function itself, 10 for each arg supplied,
          -- plus an extra discount of 10 for each argument which has
          -- interesting info, regardless of the function body.
          call_size_discount = 10 + args_discount
          args_discount = foldr ((+) . arg_discount) 0 (take n_bndrs arg_infos)
          arg_discount arg_info | hasArgInfo arg_info = 20
                                | otherwise           = 10

          actual_ret_discount | n_bndrs < n_val_args
                              = ret_discount
                              | otherwise
                              = case cont_info of
                                  BoringCtxt  -> 0
                                  DiscArgCtxt -> 0
                                  RuleArgCtxt -> ret_discount
                                  CaseCtxt    -> ret_discount
                                  ValAppCtxt  -> ret_discount
                                  RhsCtxt {}  -> 40 `min` ret_discount
                -- For RhsCtxt I suppose that exposing a data con is good in general;
                -- although 40 seems very arbitrary
                --
                -- `min` thresholding: res_discount can be very large when a
                -- function returns constructors; but we only want to invoke
                -- that large discount when there's a case continuation.

          -- Adjust by the depth scaling
          -- See Note [Avoid inlining into deeply nested cases]
          depth_threshold = unfoldingCaseThreshold opts
          depth_scaling   = unfoldingCaseScaling opts

          depth_penalty
            | case_depth <= depth_threshold = 0
            | otherwise = (rhs_size * (case_depth - depth_threshold)) `div` depth_scaling

          extra_doc = vcat [ text "size =" <+> ppr rhs_size
                           , text "case depth =" <+> int case_depth
                           , text "adjusted_size =" <+> ppr adjusted_size ]

  where
    (arg_infos, call_cont) = contArgDigests cont
    n_val_args       = length arg_infos
    lone_variable    = loneVariable cont
    cont_info        = interestingCallContext env call_cont
    case_depth       = seCaseDepth env
    opts             = seUnfoldingOpts env

    -- Unpack the UnfoldingCache lazily because it may not be needed, and all
    -- its fields are strict; so evaluating unf_cache at all forces all the
    -- isWorkFree etc computations to take place.  That risks wasting effort for
    -- Ids that are never going to inline anyway.
    -- See Note [UnfoldingCache] in GHC.Core
    UnfoldingCache{ uf_is_work_free = is_wf, uf_expandable = is_exp } = unf_cache

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
        interesting_args = any hasArgInfo arg_infos
          -- NB: (any hasArgInfo arg_infos) looks at the
          -- over-saturated args too which is "wrong";
          -- but if over-saturated we return True anyway
        saturated      = n_val_args >= uf_arity
        over_saturated = n_val_args > uf_arity

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

    mk_doc some_benefit extra_doc yes_or_no
      = vcat [ text "arg infos" <+> ppr arg_infos
             , text "interesting continuation" <+> ppr cont_info
             , text "some_benefit" <+> ppr some_benefit
             , text "is exp:" <+> ppr is_exp
             , text "is work-free:" <+> ppr is_wf
             , text "guidance" <+> ppr guidance
             , extra_doc
             , text "ANSWER =" <+> if yes_or_no then text "YES" else text "NO"]

    ctx = log_default_dump_context (logFlags logger)
    str = "Considering inlining: " ++ showSDocOneLine ctx (ppr fn)

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


{- *********************************************************************
*                                                                      *
                  Computing ArgDigest
*                                                                      *
********************************************************************* -}

-------------------
contArgDigests :: SimplCont -> ( [ArgDigest]   -- One for each value argument
                               , SimplCont )    -- The rest
-- Summarises value args, discards type args and casts.
-- The returned continuation of the call is only used to
-- answer questions like "are you interesting?"
contArgDigests cont = go [] cont
  where
    go args (ApplyToVal { sc_arg = arg, sc_env = se, sc_cont = k })
                                        = go (exprDigest se arg : args) k
    go args (ApplyToTy { sc_cont = k }) = go args k
    go args (CastIt    { sc_cont = k }) = go args k
    go args k                           = (reverse args, k)

loneVariable :: SimplCont -> Bool
loneVariable (ApplyToTy  {}) = False  -- See Note [Lone variables] in GHC.Core.Unfold
loneVariable (ApplyToVal {}) = False  -- NB: even a type application or cast
loneVariable (CastIt {})     = False  --     stops it being "lone"
loneVariable _               = True

------------------------------
exprDigest :: SimplEnv -> CoreExpr -> ArgDigest
-- Very simple version of exprIsConApp_maybe
-- But /do/ take the SimplEnv into account.  We must:
-- (a) Apply the substitution.  E.g
--       (\x. ...(f x)...) (a,b)
---    We may have x:->(a,b) in the substitution, and we want to see that
--     (a,b) when we are deciding whether or not to inline f
-- (b) Refine using the in-scope set. E.g
--       \x. ....case x of { (a,b) -> ...f x... }....
--     We want to see that x is (a,b) at the call site of f
exprDigest env e = go env e []
  where
    go :: SimplEnv -> InExpr
       -> [ArgDigest]   -- Value args only
       -> ArgDigest
    go env (Cast e _) as = go env e as
    go env (Tick _ e) as = go env e as
    go env (App f a)  as | isValArg a = go env f (exprDigest env a : as)
                         | otherwise  = go env f as

    -- Look through let-expressions
    go env (Let b e)  as
      | let env' = env `addNewInScopeIds` bindersOf b
      = go env' e as

{-  -- I considered looking through single-branch case-expressions; like lets
    -- But this is a bit too aggressive, because we might have
    --     f (case x of (a,b) -> (b,a))
    -- where   f p = Just (case p of (a,b) -> a)
    -- So f is lazy, and we won't get cancellation of the case in the body.
    --
    -- If f is strict and we don't inline, we'll float the case out, so in fact
    -- we don't need to be clever here.
    go env (Case _ b _ alts) as
      | [Alt _ bs e] <- alts
      , let env' = env `addNewInScopeIds` (b:bs)
      = go env' e as
-}

    go env (Var v) as
       = -- Simplify.Env.substId Looks up in substitution
         -- /and/ refines from the InScopeset
         case substId env v of
           DoneId v'            -> go_var env v' as
           DoneEx e _           -> go (zapSubstEnv env)             e as
           ContEx tvs cvs ids e -> go (setSubstEnv env tvs cvs ids) e as

    go _ (Lit l) as
       | isLitRubbish l = ArgNoInfo -- Leads to unproductive inlining in WWRec, #20035
       | otherwise      = assertPpr (null as) (ppr as) $
                          ArgIsCon (LitAlt l) []

    go env (Lam b e)  as
      | null as = if isRuntimeVar b
                  then ArgIsLam
                  else go env' e []
      where
        env' = modifyInScope env b  -- Tricky corner here

    go _ _ _ = ArgNonTriv  -- Some structure; not all boring
                           -- Example of improvement: base/tests/T9848

    go_var :: SimplEnv -> OutId
           -> [ArgDigest]   -- Value args only
           -> ArgDigest
    go_var env f val_args
      | Just con <- isDataConWorkId_maybe f
      = ArgIsCon (DataAlt con) val_args

      | Just rhs <- expandUnfolding_maybe unfolding
      = go (zapSubstEnv env) rhs val_args

--      | DFunUnfolding {} <- unfolding
      | hasSomeUnfolding unfolding
      = ArgIsNot []  -- We (slightly hackily) use ArgIsNot [] for dfun applications
                     -- ($df d1 .. dn).  This is very important to encourage inlining
                     -- of overloaded functions.  Example. GHC.Base.liftM2. It has
                     -- three uses of its dict arg, each of which attracts a ScrutOf
                     -- discount.  But the ArgDigest had better be good enough to
                     -- attract that ScrutOf discount!  We want liftM2 to be inlined
                     -- in its use in the liftA2 method of instance Applicative (ST s)
                     -- See Note [DFun applications are interesting]
                     --
                     -- Actually in spectral/puzzle I found that we got a big (40%!)
                     -- benefit from    let newDest = ... in case (notSeen newDest) of ...
                     -- We want to inline notSeen.  The argument has structure (its RHS)
                     -- and in fact if we inline notSeen, newDest stops being a thunk
                     -- (SPJ GHC log 13 Nov).

      -- ToDo:
      --   * Maybe we should use ArgIsNot   for (isConLikeUnfolding unfolding)
      --                     and ArgNonTriv for (hasSomeUnfolding unfolding)
      --   * Perhaps we should use ArgIsNot [] for (isConLikeId f)

      | OtherCon cs <- unfolding
      = ArgIsNot cs

      | idArity f > length val_args
      = ArgIsLam

      | not (null val_args)
      = ArgNonTriv  -- Use ArgNonTriv args with some structure e.g. (f xs)
                    -- This makes the call not totally-boring, and hence makes
                    -- INLINE things inline (which they won't if all args are boring)

      | otherwise
      = ArgNoInfo
      where
        unfolding = idUnfolding f


{- Note [DFun applications are interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        f d = ...((*) d x y)...
        ... f (df d')...
where df is con-like. Then we'd really like to inline 'f' so that the
rule for (*) (df d) can fire.  To do this
  a) we give a discount for being an argument of a class-op (eg (*) d)
  b) we say that a con-like argument (eg (df d)) has digest (ArgIsNot [])
-}

