{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}
-}


module GHC.Core.Opt.WorkWrap
 ( WwOpts (..)
 , wwTopBinds
 )
where

import GHC.Prelude

import GHC.Core
import GHC.Core.Unfold.Make
import GHC.Core.Utils  ( exprType, exprIsHNF )
import GHC.Core.Type
import GHC.Core.Opt.WorkWrap.Utils
import GHC.Core.SimpleOpt

import GHC.Data.FastString

import GHC.Hs.Extension (GhcPass, GhcTc)

import GHC.Types.Var
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.InlinePragma
import GHC.Types.Unique.Supply
import GHC.Types.Basic
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Types.SourceText
import GHC.Types.Unique

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Monad
import GHC.Core.DataCon

{-
We take Core bindings whose binders have:

\begin{enumerate}

\item Strictness attached (by the front-end of the strictness
analyser), and / or

\item Constructed Product Result information attached by the CPR
analysis pass.

\end{enumerate}

and we return some ``plain'' bindings which have been
worker/wrapper-ified, meaning:

\begin{enumerate}

\item Functions have been split into workers and wrappers where
appropriate.  If a function has both strictness and CPR properties
then only one worker/wrapper doing both transformations is produced;

\item Binders' @IdInfos@ have been updated to reflect the existence of
these workers/wrappers (this is where we get STRICTNESS and CPR pragma
info for exported values).
\end{enumerate}
-}

wwTopBinds :: WwOpts -> UniqSupply -> CoreProgram -> CoreProgram

wwTopBinds ww_opts us top_binds
  = initUs_ us $ concatMapM (wwBind ww_opts) top_binds

{-
************************************************************************
*                                                                      *
\subsection[wwBind-wwExpr]{@wwBind@ and @wwExpr@}
*                                                                      *
************************************************************************

@wwBind@ works on a binding, trying each \tr{(binder, expr)} pair in
turn.  Non-recursive case first, then recursive...
-}

wwBind  :: WwOpts
        -> CoreBind
        -> UniqSM [CoreBind]    -- returns a WwBinding intermediate form;
                                -- the caller will convert to Expr/Binding,
                                -- as appropriate.

wwBind ww_opts (NonRec binder rhs) = do
    new_rhs   <- wwExpr ww_opts rhs
    new_pairs <- tryWW ww_opts NonRecursive binder new_rhs
    return [NonRec b e | (b,e) <- new_pairs]
      -- Generated bindings must be non-recursive
      -- because the original binding was.

wwBind ww_opts (Rec pairs)
  = return . Rec <$> concatMapM do_one pairs
  where
    do_one (binder, rhs) = do new_rhs <- wwExpr ww_opts rhs
                              tryWW ww_opts Recursive binder new_rhs

{-
@wwExpr@ basically just walks the tree, looking for appropriate
annotations that can be used. Remember it is @wwBind@ that does the
matching by looking for strict arguments of the correct type.
@wwExpr@ is a version that just returns the ``Plain'' Tree.
-}

wwExpr :: WwOpts -> CoreExpr -> UniqSM CoreExpr

wwExpr _ e@(Type {}) = return e
wwExpr _ e@(Coercion {}) = return e
wwExpr _ e@(Lit  {}) = return e
wwExpr _ e@(Var  {}) = return e

wwExpr ww_opts (Lam binder expr)
  = Lam new_binder <$> wwExpr ww_opts expr
  where new_binder | isId binder = zapIdUsedOnceInfo binder
                   | otherwise   = binder
  -- See Note [Zapping Used Once info in WorkWrap]

wwExpr ww_opts (App f a)
  = App <$> wwExpr ww_opts f <*> wwExpr ww_opts a

wwExpr ww_opts (Tick note expr)
  = Tick note <$> wwExpr ww_opts expr

wwExpr ww_opts (Cast expr co) = do
    new_expr <- wwExpr ww_opts expr
    return (Cast new_expr co)

wwExpr ww_opts (Let bind expr)
  = mkLets <$> wwBind ww_opts bind <*> wwExpr ww_opts expr

wwExpr ww_opts (Case expr binder ty alts) = do
    new_expr <- wwExpr ww_opts expr
    new_alts <- mapM ww_alt alts
    let new_binder = zapIdUsedOnceInfo binder
      -- See Note [Zapping Used Once info in WorkWrap]
    return (Case new_expr new_binder ty new_alts)
  where
    ww_alt (Alt con binders rhs) = do
        new_rhs <- wwExpr ww_opts rhs
        let new_binders = [ if isId b then zapIdUsedOnceInfo b else b
                          | b <- binders ]
           -- See Note [Zapping Used Once info in WorkWrap]
        return (Alt con new_binders new_rhs)

{-
************************************************************************
*                                                                      *
\subsection[tryWW]{@tryWW@: attempt a worker/wrapper pair}
*                                                                      *
************************************************************************

@tryWW@ just accumulates arguments, converts strictness info from the
front-end into the proper form, then calls @mkWwBodies@ to do
the business.

The only reason this is monadised is for the unique supply.

Note [Don't w/w INLINE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very important to refrain from w/w-ing an INLINE function (ie one
with a stable unfolding) because the wrapper will then overwrite the
old stable unfolding with the wrapper code.

Furthermore, if the programmer has marked something as INLINE,
we may lose by w/w'ing it.

If the strictness analyser is run twice, this test also prevents
wrappers (which are INLINEd) from being re-done.  (You can end up with
several liked-named Ids bouncing around at the same time---absolute
mischief.)

Notice that we refrain from w/w'ing an INLINE function even if it is
in a recursive group.  It might not be the loop breaker.  (We used to
test for loop-breaker-hood, but see (CWW4) in Note [Cast worker/wrapper]
in GHC.Core.Opt.Simplify.Iteration.)

Note [Worker/wrapper for INLINABLE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
  {-# INLINABLE f #-}
  f :: Ord a => [a] -> Int -> a
  f x y = ....f....

where f is strict in y, we might get a more efficient loop by w/w'ing
f.  But that would make a new unfolding which would overwrite the old
one! So the function would no longer be INLINABLE, and in particular
will not be specialised at call sites in other modules.

This comes up in practice (#6056).

Solution:

* Do the w/w for strictness analysis, even for INLINABLE functions

* Transfer the Stable unfolding to the *worker*.  How do we "transfer
  the unfolding"? Easy: by using the old one, wrapped in work_fn! See
  GHC.Core.Unfold.Make.mkWorkerUnfolding.

* We use the /original, user-specified/ function's InlineSpec pragma
  for both the wrapper and the worker (see `mkStrWrapperInlinePrag`).
  So if f is INLINEABLE, both worker and wrapper will get an InlineSpec
  of (Inlinable "blah").

  It's important that both get this, because the specialiser uses
  the existence of a /user-specified/ INLINE/INLINABLE pragma to
  drive specialisation of imported functions.  See  GHC.Core.Opt.Specialise
  Note [Specialising imported functions]

* Remember, the subsequent inlining behaviour of the wrapper is expressed by
  (a) the stable unfolding
  (b) the unfolding guidance of UnfWhen
  (c) the inl_act activation (see Note [Wrapper activation]

For our {-# INLINEABLE f #-} example above, we will get something a
bit like like this:

  {-# Has stable unfolding, active in phase 2;
      plus InlineSpec = INLINEABLE #-}
  f :: Ord a => [a] -> Int -> a
  f d x y = case y of I# y' -> fw d x y'

  {-# Has stable unfolding, plus InlineSpec = INLINEABLE #-}
  fw :: Ord a => [a] -> Int# -> a
  fw d x y' = let y = I# y' in ...f...


(Historical note: we used to always give the wrapper an INLINE pragma,
but CSE will not happen if there is a user-specified pragma, but
should happen for w/w’ed things (#14186).  But now we simply propagate
any user-defined pragma info, so we'll defeat CSE (rightly) only when
there is a user-supplied INLINE/INLINEABLE pragma.)

Note [No worker/wrapper for record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We sometimes generate a lot of record selectors, and generally the
don't benefit from worker/wrapper.  Yes, mkWwBodies would find a w/w split,
but it is then suppressed by the certainlyWillInline test in splitFun.

The wasted effort in mkWwBodies makes a measurable difference in
compile time (see MR !2873), so although it's a terribly ad-hoc test,
we just check here for record selectors, and do a no-op in that case.

I did look for a generalisation, so that it's not just record
selectors that benefit.  But you'd need a cheap test for "this
function will definitely get a w/w split" and that's hard to predict
in advance...the logic in mkWwBodies is complex. So I've left the
super-simple test, with this Note to explain.

NB: record selectors are ordinary functions, inlined iff GHC wants to,
so won't be caught by the preceding isInlineUnfolding test in tryWW.

Note [Worker/wrapper for NOINLINE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to disable worker/wrapper for NOINLINE things, but it turns out
this can cause unnecessary reboxing of values. Consider

  {-# NOINLINE f #-}
  f :: Int -> a
  f x = error (show x)

  g :: Bool -> Bool -> Int -> Int
  g True  True  p = f p
  g False True  p = p + 1
  g b     False p = g b True p

the strictness analysis will discover f and g are strict, but because f
has no wrapper, the worker for g will rebox p. So we get

  $wg x y p# =
    let p = I# p# in  -- Yikes! Reboxing!
    case x of
      False ->
        case y of
          False -> $wg False True p#
          True -> +# p# 1#
      True ->
        case y of
          False -> $wg True True p#
          True -> case f p of { }

  g x y p = case p of (I# p#) -> $wg x y p#

Now, in this case the reboxing will float into the True branch, and so
the allocation will only happen on the error path. But it won't float
inwards if there are multiple branches that call (f p), so the reboxing
will happen on every call of g. Disaster.

Solution: do worker/wrapper even on NOINLINE things; but move the
NOINLINE pragma to the worker.

(See #13143 for a real-world example.)

It is crucial that we do this for *all* NOINLINE functions. #10069
demonstrates what happens when we promise to w/w a (NOINLINE) leaf
function, but fail to deliver:

  data C = C Int# Int#

  {-# NOINLINE c1 #-}
  c1 :: C -> Int#
  c1 (C _ n) = n

  {-# NOINLINE fc #-}
  fc :: C -> Int#
  fc c = 2 *# c1 c

Failing to w/w `c1`, but still w/wing `fc` leads to the following code:

  c1 :: C -> Int#
  c1 (C _ n) = n

  $wfc :: Int# -> Int#
  $wfc n = let c = C 0# n in 2 #* c1 c

  fc :: C -> Int#
  fc (C _ n) = $wfc n

Yikes! The reboxed `C` in `$wfc` can't cancel out, so we are in a bad place.
This generalises to any function that derives its strictness signature from
its callees, so we have to make sure that when a function announces particular
strictness properties, we have to w/w them accordingly, even if it means
splitting a NOINLINE function.

Note [Worker activation]
~~~~~~~~~~~~~~~~~~~~~~~~
Follows on from Note [Worker/wrapper for INLINABLE functions]

It is *vital* that if the worker gets an INLINABLE pragma (from the
original function), then the worker has the same phase activation as
the wrapper (or later).  That is necessary to allow the wrapper to
inline into the worker's unfolding: see GHC.Core.Opt.Simplify.Utils
Note [Simplifying inside stable unfoldings].

If the original is NOINLINE, it's important that the worker inherits the
original activation. Consider

  {-# NOINLINE expensive #-}
  expensive x = x + 1

  f y = let z = expensive y in ...

If expensive's worker inherits the wrapper's activation,
we'll get this (because of the compromise in point (2) of
Note [Wrapper activation])

  {-# NOINLINE[Final] $wexpensive #-}
  $wexpensive x = x + 1
  {-# INLINE[Final] expensive #-}
  expensive x = $wexpensive x

  f y = let z = expensive y in ...

and $wexpensive will be immediately inlined into expensive, followed by
expensive into f. This effectively removes the original NOINLINE!

Otherwise, nothing is lost by giving the worker the same activation as the
wrapper, because the worker won't have any chance of inlining until the
wrapper does; there's no point in giving it an earlier activation.

Note [Don't w/w inline small non-loop-breaker things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, we refrain from w/w-ing *small* functions, which are not
loop breakers, because they'll inline anyway.  But we must take care:
it may look small now, but get to be big later after other inlining
has happened.  So we take the precaution of adding a StableUnfolding
for any such functions.

I made this change when I observed a big function at the end of
compilation with a useful strictness signature but no w-w.  (It was
small during demand analysis, we refrained from w/w, and then got big
when something was inlined in its rhs.) When I measured it on nofib,
it didn't make much difference; just a few percent improved allocation
on one benchmark (bspt/Euclid.space).  But nothing got worse.

There is an infelicity though.  We may get something like
      f = g val
==>
      g x = case gw x of r -> I# r

      f {- InlineStable, Template = g val -}
      f = case gw x of r -> I# r

The code for f duplicates that for g, without any real benefit. It
won't really be executed, because calls to f will go via the inlining.

Note [Don't w/w join points for CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's no point in exploiting CPR info on a join point. If the whole function
is getting CPR'd, then the case expression around the worker function will get
pushed into the join point by the simplifier, which will have the same effect
that w/w'ing for CPR would have - the result will be returned in an unboxed
tuple.

  f z = let join j x y = (x+1, y+1)
        in case z of A -> j 1 2
                     B -> j 2 3

  =>

  f z = case $wf z of (# a, b #) -> (a, b)
  $wf z = case (let join j x y = (x+1, y+1)
                in case z of A -> j 1 2
                             B -> j 2 3) of (a, b) -> (# a, b #)

  =>

  f z = case $wf z of (# a, b #) -> (a, b)
  $wf z = let join j x y = (# x+1, y+1 #)
          in case z of A -> j 1 2
                       B -> j 2 3

Note that we still want to give `j` the CPR property, so that `f` has it. So
CPR *analyse* join points as regular functions, but don't *transform* them.

We could retain the CPR /signature/ on the worker after W/W, but it would
become outright wrong if the Simplifier pushes a non-trivial continuation
into it. For example:
    case (let $j x = (x,x) in ...) of alts
    ==>
    let $j x = case (x,x) of alts in case ... of alts
Before pushing the case in, `$j` has the CPR property, but not afterwards.

So we simply zap the CPR signature for join pints as part of the W/W pass.
The signature served its purpose during CPR analysis in propagating the
CPR property of `$j`.

Doing W/W for returned products on a join point would be tricky anyway, as the
worker could not be a join point because it would not be tail-called. However,
doing the *argument* part of W/W still works for join points, since the wrapper
body will make a tail call:

  f z = let join j x y = x + y
        in ...

  =>

  f z = let join $wj x# y# = x# +# y#
                 j x y = case x of I# x# ->
                         case y of I# y# ->
                         $wj x# y#
        in ...

Note [Wrapper activation]
~~~~~~~~~~~~~~~~~~~~~~~~~
When should the wrapper inlining be active?

1. It must not be active earlier than the current Activation of the Id,
   because we must give rewrite rules mentioning the wrapper and
   specialisation a chance to fire.
   See Note [Worker/wrapper for INLINABLE functions]
   and Note [Worker activation]

2. It should be active at some point, despite (1) because of
   Note [Worker/wrapper for NOINLINE functions]

3. For ordinary functions with no pragmas we want to inline the
   wrapper as early as possible (#15056).  Suppose another module
   defines    f !x xs = ... foldr k z xs ...
   and suppose we have the usual foldr/build RULE.  Then if we have
   a call `f x [1..x]`, we'd expect to inline f and the RULE will fire.
   But if f is w/w'd (which it might be), we want the inlining to
   occur just as if it hadn't been.

   (This only matters if f's RHS is big enough to w/w, but small
   enough to inline given the call site, but that can happen.)

4. We do not want to inline the wrapper before specialisation.
         module Foo where
           f :: Num a => a -> Int -> a
           f n 0 = n              -- Strict in the Int, hence wrapper
           f n x = f (n+n) (x-1)

           g :: Int -> Int
           g x = f x x    -- Provokes a specialisation for f
                          -- but not for $wf because w/w happens
                          -- after specialisation
         module Bar where
           import Foo

           h :: Int -> Int
           h x = f 3 x

   In module Bar we want to give f's specialisation a chance to fire
   before inlining f's wrapper.

   (Historical note: At one stage I tried making the wrapper inlining
   always-active, and that had a very bad effect on nofib/imaginary/x2n1;
   a wrapper was inlined before the specialisation fired.)

4a. If we have
      {-# SPECIALISE foo :: (Int,Int) -> Bool -> Int #-}
      {-# NOINLINE [n] foo #-}
    then specialisation will generate a SPEC rule active from Phase n.
    See Note [Specialise: rule activation] in GHC.Core.Opt.Specialise
    This SPEC specialisation rule will compete with inlining, but we don't
    mind that, because if inlining succeeds, it should be better.

    Now, if we w/w foo, we must ensure that the wrapper (which is very
    keen to inline) has a phase /after/ 'n', else it'll always "win" over
    the SPEC rule -- disaster (#20709).

Conclusion: the activation for the wrapper should be the /later/ of
  (a) the current activation of the function, or FinalPhase if it is NOINLINE
  (b) one phase /after/ the activation of any rules
This is implemented by mkStrWrapperInlinePrag.

Reminder: Note [Don't w/w INLINE things], so we don't need to worry
          about INLINE things here.


What if `foo` has no specialisations, is worker/wrappered (with the
wrapper inlining very early), and exported; and then in an importing
module we have {-# SPECIALISE foo : ... #-}?

Well then, we'll specialise foo's wrapper, which will expose a
specialisation for foo's worker, which we will do too.  That seems
fine.  (To work reliably, `foo` would need an INLINABLE pragma,
in which case we don't unpack dictionaries for the worker; see
see Note [Do not unbox class dictionaries].)

Note [Drop absent bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#19824):
   let t = ...big...
   in ...(f t x)...

were `f` ignores its first argument.  With luck f's wrapper will inline
thereby dropping `t`, but maybe not: the arguments to f all look boring.

So we pre-empt the problem by replacing t's RHS with an absent filler.
Simple and effective.
-}

tryWW   :: WwOpts
        -> RecFlag
        -> Id                           -- The fn binder
        -> CoreExpr                     -- The bound rhs; its innards
                                        --   are already ww'd
        -> UniqSM [(Id, CoreExpr)]      -- either *one* or *two* pairs;
                                        -- if one, then no worker (only
                                        -- the orig "wrapper" lives on);
                                        -- if two, then a worker and a
                                        -- wrapper.
tryWW ww_opts is_rec fn_id rhs
  -- See Note [Drop absent bindings]
  | isAbsDmd (demandInfo fn_info)
  , not (isJoinId fn_id)
  , Just filler <- mkAbsentFiller (wo_module ww_opts) fn_id NotMarkedStrict
  = return [(new_fn_id, filler)]

  -- See Note [Don't w/w INLINE things]
  | hasInlineUnfolding fn_info
  = return [(new_fn_id, rhs)]

  -- See Note [No worker/wrapper for record selectors]
  | isRecordSelector fn_id
  = return [ (new_fn_id, rhs ) ]

  -- Don't w/w OPAQUE things
  -- See Note [OPAQUE pragma]
  --
  -- Whilst this check might seem superfluous, since we strip boxity
  -- information in GHC.Core.Opt.DmdAnal.finaliseArgBoxities and
  -- CPR information in GHC.Core.Opt.CprAnal.cprAnalBind, it actually
  -- isn't. That is because we would still perform w/w when:
  --
  -- - An argument is used strictly, and -fworker-wrapper-cbv is
  --   enabled, or,
  -- - When demand analysis marks an argument as absent.
  --
  -- In a debug build we do assert that boxity and CPR information
  -- are actually stripped, since we want to prevent callers of OPAQUE
  -- things to do reboxing. See:
  -- - Note [The OPAQUE pragma and avoiding the reboxing of arguments]
  -- - Note [The OPAQUE pragma and avoiding the reboxing of results]
  | isOpaquePragma (inlinePragInfo fn_info)
  = assertPpr (onlyBoxedArguments (dmdSigInfo fn_info) &&
               isTopCprSig (cprSigInfo fn_info))
              (text "OPAQUE fun with boxity" $$
               ppr new_fn_id $$
               ppr (dmdSigInfo fn_info) $$
               ppr (cprSigInfo fn_info) $$
               ppr rhs) $
    return [ (new_fn_id, rhs) ]

  -- Do this even if there is a NOINLINE pragma
  -- See Note [Worker/wrapper for NOINLINE functions]
  | is_fun
  = splitFun ww_opts new_fn_id rhs

  -- See Note [Thunk splitting]
  | isNonRec is_rec     -- See (TS4)
  , is_thunk  -- All join points go via splitFun
  , not (hasRulesOrInline (fn_info))  -- See (TS9)
  = do { -- Try strictness then CPR; see (TS5)
         rhs' <- splitThunkStr ww_opts new_fn_id rhs `orElseM`
                 splitThunkCpr ww_opts new_fn_id rhs `orElseM`
                 return rhs
       ; return [ (new_fn_id, rhs') ] }

  | otherwise
  = return [ (new_fn_id, rhs) ]

  where
    is_join        = isJoinId fn_id
    fn_info        = idInfo fn_id
    (wrap_dmds, _) = splitDmdSig (dmdSigInfo fn_info)
    new_fn_id      = zap_join_cpr $ zap_usage fn_id

    zap_usage = zapIdUsedOnceInfo . zapIdUsageEnvInfo
        -- See Note [Zapping DmdEnv after Demand Analyzer] and
        -- See Note [Zapping Used Once info in WorkWrap]

    zap_join_cpr id | is_join   = id `setIdCprSig` topCprSig
                    | otherwise = id
        -- See Note [Don't w/w join points for CPR]

    is_fun     = notNull wrap_dmds || is_join
    is_thunk   = not (exprIsHNF rhs)
                 && not (isUnliftedType (idType fn_id))

{-
Note [Zapping DmdEnv after Demand Analyzer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the worker-wrapper pass we zap the DmdEnv.  Why?
 (a) it is never used again
 (b) it wastes space
 (c) it becomes incorrect as things are cloned, because
     we don't push the substitution into it

Why here?
 * Because we don’t want to do it in the Demand Analyzer, as we never know
   there when we are doing the last pass.
 * We want them to be still there at the end of DmdAnal, so that
   -ddump-str-anal contains them.
 * We don’t want a second pass just for that.
 * WorkWrap looks at all bindings anyway.

We also need to do it in TidyCore.tidyLetBndr to clean up after the
final, worker/wrapper-less run of the demand analyser (see
Note [Final Demand Analyser run] in GHC.Core.Opt.DmdAnal).

Note [Zapping Used Once info in WorkWrap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During the work/wrap pass, using zapIdUsedOnceInfo, we zap the "used once" info
* on every binder (let binders, case binders, lambda binders)
* in both demands and in strictness signatures
* recursively

Why?
 * The simplifier may happen to transform code in a way that invalidates the
   data (see #11731 for an example).
 * It is not used in later passes, up to code generation.

At first it's hard to see how the simplifier might invalidate it (and
indeed for a while I thought it couldn't: #19482), but it's not quite
as simple as I thought.  Consider this:
  {-# STRICTNESS SIG <SP(M,A)> #-}
  f p = let v = case p of (a,b) -> a
        in p `seq` (v,v)

I think we'll give `f` the strictness signature `<SP(M,A)>`, where the
`M` says that we'll evaluate the first component of the pair at most
once.  Why?  Because the RHS of the thunk `v` is evaluated at most
once.

But now let's worker/wrapper f:
  {-# STRICTNESS SIG <M> #-}
  $wf p1 = let p2 = absentError "urk" in
           let p = (p1,p2) in
           let v = case p of (a,b) -> a
           in p `seq` (v,v)

where I've gotten the demand on `p1` by decomposing the P(M,A) argument demand.
This rapidly simplifies to
  {-# STRICTNESS SIG <M> #-}
  $wf p1 = let v = p1 in
           (v,v)

and thence to `(p1,p1)` by inlining the trivial let. Now the demand on `p1` should
not be at most once!!

Conclusion: used-once info is fragile to simplification, because of
the non-monotonic behaviour of let's, which turn used-many into
used-once.  So indeed we should zap this info in worker/wrapper.

Conclusion: kill it during worker/wrapper, using `zapUsedOnceInfo`.
Both the *demand signature* of the binder, and the *demand-info* of
the binder.  Moreover, do so recursively.

You might wonder: why do we generate used-once info if we then throw
it away.  The main reason is that we do a final run of the demand analyser,
immediately before CoreTidy, which is /not/ followed by worker/wrapper; it
is there only to generate used-once info for single-entry thunks.

Note [Don't eta expand in w/w]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A binding where the manifestArity of the RHS is less than idArity of
the binder means GHC.Core.Opt.Arity didn't eta expand that binding
When this happens, it does so for a reason (see Note [Arity invariants for bindings]
in GHC.Core.Opt.Arity) and we probably have a PAP, cast or trivial expression
as RHS.

Below is a historical account of what happened when w/w still did eta expansion.
Nowadays, it doesn't do that, but will simply w/w for the wrong arity, unleashing
a demand signature meant for e.g. 2 args to be unleashed for e.g. 1 arg
(manifest arity). That's at least as terrible as doing eta expansion, so don't
do it.
---
When worker/wrapper did eta expansion, it implictly eta expanded the binding to
idArity, overriding GHC.Core.Opt.Arity's decision. Other than playing fast and loose with
divergence, it's also broken for newtypes:

  f = (\xy.blah) |> co
    where
      co :: (Int -> Int -> Char) ~ T

Then idArity is 2 (despite the type T), and it can have a DmdSig based on a
threshold of 2. But we can't w/w it without a type error.

The situation is less grave for PAPs, but the implicit eta expansion caused a
compiler allocation regression in T15164, where huge recursive instance method
groups, mostly consisting of PAPs, got w/w'd. This caused great churn in the
simplifier, when simply waiting for the PAPs to inline arrived at the same
output program.

Note there is the worry here that such PAPs and trivial RHSs might not *always*
be inlined. That would lead to reboxing, because the analysis tacitly assumes
that we W/W'd for idArity and will propagate analysis information under that
assumption. So far, this doesn't seem to matter in practice.
See https://gitlab.haskell.org/ghc/ghc/merge_requests/312#note_192064.

Note [Inline pragma for certainlyWillInline]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#19824 comment on 15 May 21):
  f _ (x,y) = ...big...
  v = ...big...
  g x = f v x + 1

So `f` will generate a worker/wrapper split; and `g` (since it is small)
will trigger the certainlyWillInline case of splitFun.  The danger is that
we end up with
  g {- StableUnfolding = \x -> f v x + 1 -}
    = ...blah...

Since (a) that unfolding for g is AlwaysActive
      (b) the unfolding for f's wrapper is ActiveAfterInitial
the call of f will never inline in g's stable unfolding, thereby
keeping `v` alive.

I thought of changing g's unfolding to be ActiveAfterInitial, but that
too is bad: it delays g's inlining into other modules, which makes fewer
specialisations happen. Example in perf/should_run/DeriveNull.

So I decided to live with the problem.  In fact v's RHS will be replaced
by LitRubbish (see Note [Drop absent bindings]) so there is no great harm.
-}


---------------------
splitFun :: WwOpts -> Id -> CoreExpr -> UniqSM [(Id, CoreExpr)]
splitFun ww_opts fn_id rhs
  | Just (arg_vars, body) <- collectNValBinders_maybe ww_arity rhs
  = warnPprTrace (not (wrap_dmds `lengthIs` (arityInfo fn_info)))
                 "splitFun"
                 (ppr fn_id <+> (ppr wrap_dmds $$ ppr cpr)) $
    do { mb_stuff <- mkWwBodies ww_opts fn_id ww_arity arg_vars (exprType body) wrap_dmds cpr
       ; case mb_stuff of
            Nothing -> -- No useful wrapper; leave the binding alone
                       return [(fn_id, rhs)]

            Just stuff
              | let opt_wwd_rhs = mkLams arg_vars $
                                  simpleOptExpr (wo_simple_opts ww_opts) body
                  -- Run the simple optimiser on the WW'd body, to get rid of
                  -- junk. Keep all the original `arg_vars` binders though: this
                  -- might be a join point, and we don't want to lose the
                  -- one-shot annotations.  At least I think that's the reason
                  -- (honestly, I have forgottne), but doing it this way
                  -- certainly does no harm and is slightly more efficient.

              , Just stable_unf <- certainlyWillInline uf_opts fn_info opt_wwd_rhs
                -- We could make a w/w split, but in fact the RHS is small
                -- See Note [Don't w/w inline small non-loop-breaker things]

              , let id_w_unf = fn_id `setIdUnfolding` stable_unf
                -- See Note [Inline pragma for certainlyWillInline]
              ->  return [ (id_w_unf, rhs) ]

              | otherwise
              -> do { work_uniq <- getUniqueM
                    ; return (mkWWBindPair ww_opts fn_id fn_info arg_vars body
                                           work_uniq div stuff) } }

  | otherwise    -- See Note [Don't eta expand in w/w]
  = return [(fn_id, rhs)]

  where
    uf_opts  = so_uf_opts (wo_simple_opts ww_opts)
    fn_info  = idInfo fn_id
    ww_arity = workWrapArity fn_id rhs
      -- workWrapArity: see (4) in Note [Worker/wrapper arity and join points] in DmdAnal

    (wrap_dmds, div) = splitDmdSig (dmdSigInfo fn_info)

    cpr_ty = cprSigType (cprSigInfo fn_info)
    -- Arity of the CPR sig should match idArity when it's not a join point.
    -- See Note [Arity trimming for CPR signatures] in GHC.Core.Opt.CprAnal
    cpr = assertPpr (isJoinId fn_id || cpr_ty == topCprType || ct_arty cpr_ty == arityInfo fn_info)
                    (ppr fn_id <> colon <+> text "ct_arty:" <+> int (ct_arty cpr_ty)
                      <+> text "arityInfo:" <+> ppr (arityInfo fn_info)) $
          ct_cpr cpr_ty

mkWWBindPair :: WwOpts -> Id -> IdInfo
             -> [Var] -> CoreExpr -> Unique -> Divergence
             -> ([Demand],JoinArity, Id -> CoreExpr, Expr CoreBndr -> CoreExpr)
             -> [(Id, CoreExpr)]
mkWWBindPair ww_opts fn_id fn_info fn_args fn_body work_uniq div
             (work_demands, join_arity, wrap_fn, work_fn)
  = -- pprTrace "mkWWBindPair" (ppr fn_id <+> ppr wrap_id <+> ppr work_id $$ ppr wrap_rhs) $
    [(work_id, work_rhs), (wrap_id, wrap_rhs)]
     -- Worker first, because wrapper mentions it
  where
    arity = arityInfo fn_info
            -- The arity is set by the simplifier using exprEtaExpandArity
            -- So it may be more than the number of top-level-visible lambdas

    simpl_opts = wo_simple_opts ww_opts

    work_rhs = work_fn (mkLams fn_args fn_body)
    work_act = case fn_inline_spec of  -- See Note [Worker activation]
                   NoInline -> inl_act fn_inl_prag
                   _        -> inl_act wrap_prag

    srcTxt = SourceText $ fsLit "{-# INLINE"
    work_prag = InlinePragma { inl_ext = XInlinePragmaGhc srcTxt AnySaturation
                             , inl_inline = fn_inline_spec
                             , inl_act    = work_act
                             , inl_rule   = FunLike }
      -- inl_inline: copy from fn_id; see Note [Worker/wrapper for INLINABLE functions]
      -- inl_act:    see Note [Worker activation]
      -- inl_rule:   it does not make sense for workers to be constructorlike.

    work_join_arity | isJoinId fn_id = JoinPoint join_arity
                    | otherwise      = NotJoinPoint
      -- worker is join point iff wrapper is join point
      -- (see Note [Don't w/w join points for CPR])

    work_id  = setCbvCandidate $
               mkWorkerId work_uniq fn_id (exprType work_rhs)
                `setIdOccInfo` occInfo fn_info
                        -- Copy over occurrence info from parent
                        -- Notably whether it's a loop breaker
                        -- Doesn't matter much, since we will simplify next, but
                        -- seems right-er to do so

                `setInlinePragma` work_prag

                `setIdUnfolding` mkWorkerUnfolding simpl_opts work_fn fn_unfolding
                        -- See Note [Worker/wrapper for INLINABLE functions]

                `setIdDmdSig` mkClosedDmdSig work_demands div
                        -- Even though we may not be at top level,
                        -- it's ok to give it an empty DmdEnv

                `setIdCprSig` topCprSig

                `setIdDemandInfo` worker_demand

                `setIdArity` work_arity
                        -- Set the arity so that the Core Lint check that the
                        -- arity is consistent with the demand type goes
                        -- through

                `asJoinId_maybe` work_join_arity

    work_arity = length work_demands :: Int

    -- See Note [Demand on the worker]
    single_call = saturatedByOneShots arity (demandInfo fn_info)
    worker_demand | single_call = mkWorkerDemand work_arity
                  | otherwise   = topDmd

    wrap_rhs  = wrap_fn work_id
    wrap_prag = mkStrWrapperInlinePrag fn_inl_prag fn_rules
    wrap_unf  = mkWrapperUnfolding simpl_opts wrap_rhs arity

    wrap_id   = fn_id `setIdUnfolding`  wrap_unf
                      `setInlinePragma` wrap_prag
                      `setIdOccInfo`    noOccInfo
                        -- Zap any loop-breaker-ness, to avoid bleating from Lint
                        -- about a loop breaker with an INLINE rule

    fn_inl_prag     = inlinePragInfo fn_info
    fn_inline_spec  = inl_inline fn_inl_prag
    fn_unfolding    = realUnfoldingInfo fn_info
    fn_rules        = ruleInfoRules (ruleInfo fn_info)

mkStrWrapperInlinePrag :: InlinePragma (GhcPass p) -> [CoreRule] -> InlinePragma GhcTc
mkStrWrapperInlinePrag (InlinePragma { inl_inline = fn_inl
                                     , inl_act    = fn_act
                                     , inl_rule   = rule_info }) rules
  = InlinePragma { inl_ext    = XInlinePragmaGhc srcTxt AnySaturation

                 , inl_inline = fn_inl
                      -- See Note [Worker/wrapper for INLINABLE functions]

                 , inl_act    = activeAfter wrapper_phase
                      -- See Note [Wrapper activation]

                 , inl_rule   = rule_info }  -- RuleMatchInfo is (and must be) unaffected
  where
    srcTxt = SourceText $ fsLit "{-# INLINE"
    -- See Note [Wrapper activation]
    wrapper_phase = foldr (latestPhase . get_rule_phase) earliest_inline_phase rules
    earliest_inline_phase = beginPhase fn_act `latestPhase` nextPhase InitialPhase
          -- latestPhase (nextPhase InitialPhase) is a temporary hack
          -- to inline no earlier than phase 2.  I got regressions in
          -- 'mate', due to changes in full laziness due to Note [Case
          -- MFEs], when I did earlier inlining.

    get_rule_phase :: CoreRule -> CompilerPhase
    -- The phase /after/ the rule is first active
    get_rule_phase rule = nextPhase (beginPhase (ruleActivation rule))

{- Note [Demand on the worker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the original function is called once, according to its demand info, then
so is the worker. This is important so that the occurrence analyser can
attach OneShot annotations to the worker’s lambda binders.


Example:

  -- Original function
  f [Demand=<L,1*C(1,U)>] :: (a,a) -> a
  f = \p -> ...

  -- Wrapper
  f [Demand=<L,1*C(1,U)>] :: a -> a -> a
  f = \p -> case p of (a,b) -> $wf a b

  -- Worker
  $wf [Demand=<L,1*C(1,C(1,U))>] :: Int -> Int
  $wf = \a b -> ...

We need to check whether the original function is called once, with
sufficiently many arguments. This is done using saturatedByOneShots, which
takes the arity of the original function (resp. the wrapper) and the demand on
the original function.

The demand on the worker is then calculated using mkWorkerDemand, and always of
the form [Demand=<L,1*(C(1,...(C(1,U))))>]

Note [Thunk splitting]
~~~~~~~~~~~~~~~~~~~~~~
The worker/wrapper split mainly affects let-bound /functions/.  But we can also use
demand and CPR information to improve let-bound /thunks/.  Specifically:

* (A) Demand-led thunk splitting:
  If a thunk is demanded strictly, perhaps nestedly, we can turn it into a
  case, perhaps nestedly.  For example
       let x :: (Int, Int)
           {- Dmd = SP(S,L) -}
           x = rhs
       in body
   We would ultimately like to get
        case rhs of x { (a,b) ->
        case a of { I# a# ->
        body

   This is much better: no thunk for `rhs`; and known-data-con unfoldings for `x`
   and `a` inside `body`.

* (B) CPR-led thunk splitting:
  If a thunk has the (possibly-nested) CPR property /and/ is demanded strictly
  at top level, we can again generate a case
       let x :: (Int, Int)
           {- Dmd = S, Cpr = (_, I# _) -}
           x = rhs
       in body
   The CPR info says that if `x` is evaluated (which it is), it'll return a pair,
   whose second component can very cheaply be evaluated to a (I# b#).  We would
   like to end up with
        case rhs of x { (a,b) ->
        case b of { I# b# ->
        body
   which is good for the same reasons as before.  See (TS3) for an example.

Implementation
~~~~~~~~~~~~~~
The worker/wrapper pass does not generate these nested case expressions directly.
Instead it generates a regular let-binding but, since the binder is marked as
strict, any `case` expressions in the RHS will float out.  Specifically:

(TS1) For (A), `splitThunkStr` generates the binding
      let x! = case rhs of { (a,b) ->
               case a of { I# a# ->
               (I# a, b)
  Now when the Simplifier floats the `case` expressions out, we get
      case rhs of { (a,b) ->
      case a of { I# a# ->
      let x = (I# a, b)
      in body
  which is equivalent to the expression in (A) above.

  `splitThunkStr` also works on /absent/ thunks: see (TS6).

  Rather cleverly, `splitThunkStr` uses `mkWWstr_one`, the /existing/ function
  argument w/w splitting function, to do the unpacking.  This automatically
  deals with nested strictness.

(TS2) For (B), `splitThunkCpr` behaves exactly analogously, again
  using `mkWWcpr_one` to do the unpacking. It works only for /demanded/ thunks.

Wrinkles:

(TS3) Here's an example of (B), thunk-splitting based on CPR information.
  Suppose we have
        let x! = case e of { T -> Just e1; F -> Just e2 }
        in body
  If the Simplifier floats the `case` out, we get
      join j2 x = body
      in case e of { T -> j2 (Just e1); F -> j2 (Just e2) }
  which is not good (boxing).

  If we first do the thunk-splitting transformation, we get
        let x! = case (case e of { T -> Just e1; F -> Just e2 }) of
                   Just a -> Just a
        in body

  Now when the Simplifier floats we will get:
      join j a = let x = Just a in body
      in case e of { T -> j e1; F -> j e2 }
  which is much better (no boxing).   But see (TS9).

(TS4) We only do thunk-splitting for thunks that are non-recursive
  For recursive thunks, the Simplifier is unable to float `rhs` out of
  x's RHS, because `x` occurs freely in `rhs`, and will just change it
  back to the original definition.

(TS5) Notice that (B) CPR-led thunk splitting can be useful:
  * when multi-constructor data types are involved, so that the argument-unboxing
    mechanism (A) is impotent.  See the example in (TS3), which uses `Maybe`.
  * when the binder is only demanded head-strictly, but CPR has nested information.

  So (A) and (B) have complementary benefits.  We might do even better by
  /combining/ (A) and (B), but at the moment we try (A) first, and then try (B)
  if (A) is impotent.  Combining them would be more work.

(TS6) We can also exploit /absence/.  Consider
         let x {-# Dmd = A #-} = rhs
         in body
   Generally speaking `x` will be absent becuase it is passed to functions that
   don't use it, in which case worker/wrapper on those calls will make `x` unused.
   But suppose `x` is absent inside a /lazy/ argument:
         let f p = (fst p, 99) in
         let x = rhs in
         f (True, x)
   Here f's argument is lazy, but `x` is definitely absent, and we could replace
   `x` with an absent filler.  Using (TS1) would give us
         let x = absentError "urk" in ...
   which is great.

   So `splitThunkStr` works /regardless/ of the demand on `x`; whether it fires is
   determined only by the  `useful` flag returned by `mkWWstr_one`.

(TS7) Adopting (TS6) leads to one other complication: a top-level binder with
   an External Name (see Note [About the NameSorts]) can be  Absent.  If so,
   it must not be used as a local binder in the `Let` created by `splitThunkStr`.
   T19180 is a test case for this rather obscure situation.

   Solution: use `localiseId` to localise the binder for the `Let`.

(TS8) Consider out poster child test case for `splitThunkCpr`, in (TS3).
         x! = case e of { True -> Just 1; False -> Just 2 }
  But suppose we have `-fworker-wrapper-cbv` enabled (reminder: see
  Note [Worker/wrapper for strict arguments]).  Then alas `splitThunkStr`
  will fire, but it'll be a no-op in this case.  But since it fires, `splitThunkCpr`
  never gets a chance.

  Hacky but effective solution: switch off `wo_unlift_strict` locally in `splitThunkStr`.

(TS9) `splitThunkStr` implicitly relies on floating a case out of a strict binding.
  But if the thunk has a RULE or INLINE/NOINLINE pragma that won't happen; see
  (SB4) in Note [StrictBind] in GHC.Core.Opt.Simplify.Iteration.  So we test
  these things before applying thunk splitting.
-}

-- | splitThunkStr converts the *non-recursive* binding
--      x = e
-- into
--      x = let x = e in
--          case x of I# y -> I# y
-- See Note [Thunk splitting].
splitThunkStr :: WwOpts -> Var -> CoreExpr -> UniqSM (Maybe CoreExpr)
splitThunkStr ww_opts x rhs
  = -- NB: does not depend on x's demand being strict; it might be absent, see (TS6)
    do { let x' = localiseId x -- See (TS7)
             ww_opts' = ww_opts { wo_unlift_strict = False }  -- See (TS8)
       ; (useful, _transit_vars, wrap_fn, fn_arg) <- mkWWstr_one ww_opts' x' NotMarkedStrict
            -- Use mkWWstr_one: see (TS1) in Note [Thunk splitting]

       ; if useful
         then return (Just (Let (NonRec x' rhs) $
                            wrap_fn fn_arg))
         else return Nothing }

splitThunkCpr :: WwOpts -> Var -> CoreExpr -> UniqSM (Maybe CoreExpr)
-- See Note [Thunk splitting]
-- Given x* = rhs, where x* is used outermost-strictly, and x* has a (nested) CPR
-- property, say Just (Just _), return this new RHS:
--     let x' = rhs in
--     case x' of { Just p -> case p of { Just q ->   -- Unpacking code
--       Just (Just q) }}                             -- Repacking code
-- Works for multi-constructor data types, where splitThunkStr will fail
splitThunkCpr ww_opts x rhs
  | wo_cpr_anal ww_opts   --  See (CPR1) in Note [Phase ordering] on GHC.Opt.CprAnal
  , isStrUsedDmd (idDemandInfo x) -- See (TS2)
  = assertPpr (cprTypeArity cpr_type == 0) (ppr x) $   -- It's a thunk
    do { (useful, _transit_vars, rebuild_result, unpacker) <- mkWWcpr_one ww_opts x cpr
            -- Use mkWWcpr_one: see (TS2) in Note [Thunk splitting]
       ; if useful
         then return (Just (Let (NonRec x rhs) $
                            unpacker rebuild_result))
         else return Nothing }

  | otherwise
  = return Nothing
  where
    cpr_type :: CprType
    cpr_type = cprSigType (idCprSig x)
    cpr :: Cpr
    cpr = cprTypeCpr cpr_type

