{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[WorkWrap]{Worker/wrapper-generating back-end of strictness analyser}
-}

{-# LANGUAGE CPP #-}
module WorkWrap ( wwTopBinds ) where

import GhcPrelude

import CoreSyn
import CoreUnfold       ( certainlyWillInline, mkWwInlineRule, mkWorkerUnfolding )
import CoreUtils        ( exprType, exprIsHNF )
import CoreFVs          ( exprFreeVars )
import Var
import Id
import IdInfo
import Type
import UniqSupply
import BasicTypes
import DynFlags
import Demand
import WwLib
import Util
import Outputable
import FamInstEnv
import MonadUtils

#include "HsVersions.h"

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

wwTopBinds :: DynFlags -> FamInstEnvs -> UniqSupply -> CoreProgram -> CoreProgram

wwTopBinds dflags fam_envs us top_binds
  = initUs_ us $ do
    top_binds' <- mapM (wwBind dflags fam_envs) top_binds
    return (concat top_binds')

{-
************************************************************************
*                                                                      *
\subsection[wwBind-wwExpr]{@wwBind@ and @wwExpr@}
*                                                                      *
************************************************************************

@wwBind@ works on a binding, trying each \tr{(binder, expr)} pair in
turn.  Non-recursive case first, then recursive...
-}

wwBind  :: DynFlags
        -> FamInstEnvs
        -> CoreBind
        -> UniqSM [CoreBind]    -- returns a WwBinding intermediate form;
                                -- the caller will convert to Expr/Binding,
                                -- as appropriate.

wwBind dflags fam_envs (NonRec binder rhs) = do
    new_rhs <- wwExpr dflags fam_envs rhs
    new_pairs <- tryWW dflags fam_envs NonRecursive binder new_rhs
    return [NonRec b e | (b,e) <- new_pairs]
      -- Generated bindings must be non-recursive
      -- because the original binding was.

wwBind dflags fam_envs (Rec pairs)
  = return . Rec <$> concatMapM do_one pairs
  where
    do_one (binder, rhs) = do new_rhs <- wwExpr dflags fam_envs rhs
                              tryWW dflags fam_envs Recursive binder new_rhs

{-
@wwExpr@ basically just walks the tree, looking for appropriate
annotations that can be used. Remember it is @wwBind@ that does the
matching by looking for strict arguments of the correct type.
@wwExpr@ is a version that just returns the ``Plain'' Tree.
-}

wwExpr :: DynFlags -> FamInstEnvs -> CoreExpr -> UniqSM CoreExpr

wwExpr _      _ e@(Type {}) = return e
wwExpr _      _ e@(Coercion {}) = return e
wwExpr _      _ e@(Lit  {}) = return e
wwExpr _      _ e@(Var  {}) = return e

wwExpr dflags fam_envs (Lam binder expr)
  = Lam new_binder <$> wwExpr dflags fam_envs expr
  where new_binder | isId binder = zapIdUsedOnceInfo binder
                   | otherwise   = binder
  -- See Note [Zapping Used Once info in WorkWrap]

wwExpr dflags fam_envs (App f a)
  = App <$> wwExpr dflags fam_envs f <*> wwExpr dflags fam_envs a

wwExpr dflags fam_envs (Tick note expr)
  = Tick note <$> wwExpr dflags fam_envs expr

wwExpr dflags fam_envs (Cast expr co) = do
    new_expr <- wwExpr dflags fam_envs expr
    return (Cast new_expr co)

wwExpr dflags fam_envs (Let bind expr)
  = mkLets <$> wwBind dflags fam_envs bind <*> wwExpr dflags fam_envs expr

wwExpr dflags fam_envs (Case expr binder ty alts) = do
    new_expr <- wwExpr dflags fam_envs expr
    new_alts <- mapM ww_alt alts
    let new_binder = zapIdUsedOnceInfo binder
      -- See Note [Zapping Used Once info in WorkWrap]
    return (Case new_expr new_binder ty new_alts)
  where
    ww_alt (con, binders, rhs) = do
        new_rhs <- wwExpr dflags fam_envs rhs
        let new_binders = [ if isId b then zapIdUsedOnceInfo b else b
                          | b <- binders ]
           -- See Note [Zapping Used Once info in WorkWrap]
        return (con, new_binders, new_rhs)

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
in a recursive group.  It might not be the loop breaker.  (We could
test for loop-breaker-hood, but I'm not sure that ever matters.)

Note [Worker-wrapper for INLINABLE functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
  {-# INLINABLE f #-}
  f :: Ord a => [a] -> Int -> a
  f x y = ....f....

where f is strict in y, we might get a more efficient loop by w/w'ing
f.  But that would make a new unfolding which would overwrite the old
one! So the function would no longer be INLNABLE, and in particular
will not be specialised at call sites in other modules.

This comes in practice (Trac #6056).

Solution: do the w/w for strictness analysis, but transfer the Stable
unfolding to the *worker*.  So we will get something like this:

  {-# INLINE[0] f #-}
  f :: Ord a => [a] -> Int -> a
  f d x y = case y of I# y' -> fw d x y'

  {-# INLINABLE[0] fw #-}
  fw :: Ord a => [a] -> Int# -> a
  fw d x y' = let y = I# y' in ...f...

How do we "transfer the unfolding"? Easy: by using the old one, wrapped
in work_fn! See CoreUnfold.mkWorkerUnfolding.

Note [Worker-wrapper for NOINLINE functions]
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

(See Trac #13143 for a real-world example.)

Note [Worker activation]
~~~~~~~~~~~~~~~~~~~~~~~~
Follows on from Note [Worker-wrapper for INLINABLE functions]

It is *vital* that if the worker gets an INLINABLE pragma (from the
original function), then the worker has the same phase activation as
the wrapper (or later).  That is necessary to allow the wrapper to
inline into the worker's unfolding: see SimplUtils
Note [Simplifying inside stable unfoldings].

If the original is NOINLINE, it's important that the work inherit the
original activation. Consider

  {-# NOINLINE expensive #-}
  expensive x = x + 1

  f y = let z = expensive y in ...

If expensive's worker inherits the wrapper's activation,
we'll get this (because of the compromise in point (2) of
Note [Wrapper activation])

  {-# NOINLINE[0] $wexpensive #-}
  $wexpensive x = x + 1
  {-# INLINE[0] expensive #-}
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
has happened.  So we take the precaution of adding an INLINE pragma to
any such functions.

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

Note [Don't CPR join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There's no point in doing CPR on a join point. If the whole function is getting
CPR'd, then the case expression around the worker function will get pushed into
the join point by the simplifier, which will have the same effect that CPR would
have - the result will be returned in an unboxed tuple.

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

Doing CPR on a join point would be tricky anyway, as the worker could not be
a join point because it would not be tail-called. However, doing the *argument*
part of W/W still works for join points, since the wrapper body will make a tail
call:

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

1. It must not be active earlier than the current Activation of the
   Id

2. It should be active at some point, despite (1) because of
   Note [Worker-wrapper for NOINLINE functions]

3. For ordinary functions with no pragmas we want to inline the
   wrapper as early as possible (Trac #15056).  Suppose another module
   defines    f x = g x x
   and suppose there is some RULE for (g True True).  Then if we have
   a call (f True), we'd expect to inline 'f' and the RULE will fire.
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
           g x = f x x            -- Provokes a specialisation for f

         module Bar where
           import Foo

           h :: Int -> Int
           h x = f 3 x

   In module Bar we want to give specialisations a chance to fire
   before inlining f's wrapper.

Reminder: Note [Don't w/w INLINE things], so we don't need to worry
          about INLINE things here.

Conclusion:
  - If the user said NOINLINE[n], respect that
  - If the user said NOINLINE, inline the wrapper as late as
    poss (phase 0). This is a compromise driven by (2) above
  - Otherwise inline wrapper in phase 2.  That allows the
    'gentle' simplification pass to apply specialisation rules

Historical note: At one stage I tried making the wrapper inlining
always-active, and that had a very bad effect on nofib/imaginary/x2n1;
a wrapper was inlined before the specialisation fired.

Note [Wrapper NoUserInline]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The use an inl_inline of NoUserInline on the wrapper distinguishes
this pragma from one that was given by the user. In particular, CSE
will not happen if there is a user-specified pragma, but should happen
for w/w’ed things (#14186).
-}

tryWW   :: DynFlags
        -> FamInstEnvs
        -> RecFlag
        -> Id                           -- The fn binder
        -> CoreExpr                     -- The bound rhs; its innards
                                        --   are already ww'd
        -> UniqSM [(Id, CoreExpr)]      -- either *one* or *two* pairs;
                                        -- if one, then no worker (only
                                        -- the orig "wrapper" lives on);
                                        -- if two, then a worker and a
                                        -- wrapper.
tryWW dflags fam_envs is_rec fn_id rhs
  -- See Note [Worker-wrapper for NOINLINE functions]

  | Just stable_unf <- certainlyWillInline dflags fn_info
  = return [ (fn_id `setIdUnfolding` stable_unf, rhs) ]
        -- See Note [Don't w/w INLINE things]
        -- See Note [Don't w/w inline small non-loop-breaker things]

  | is_fun
  = splitFun dflags fam_envs new_fn_id fn_info wrap_dmds res_info rhs

  | is_thunk                                   -- See Note [Thunk splitting]
  = splitThunk dflags fam_envs is_rec new_fn_id rhs

  | otherwise
  = return [ (new_fn_id, rhs) ]

  where
    fn_info      = idInfo fn_id
    (wrap_dmds, res_info) = splitStrictSig (strictnessInfo fn_info)

    new_fn_id = zapIdUsedOnceInfo (zapIdUsageEnvInfo fn_id)
        -- See Note [Zapping DmdEnv after Demand Analyzer] and
        -- See Note [Zapping Used Once info in WorkWrap]

    is_fun    = notNull wrap_dmds || isJoinId fn_id
    is_thunk  = not is_fun && not (exprIsHNF rhs) && not (isJoinId fn_id)
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
Note [Final Demand Analyser run] in DmdAnal).

Note [Zapping Used Once info in WorkWrap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the worker-wrapper pass we zap the used once info in demands and in
strictness signatures.

Why?
 * The simplifier may happen to transform code in a way that invalidates the
   data (see #11731 for an example).
 * It is not used in later passes, up to code generation.

So as the data is useless and possibly wrong, we want to remove it. The most
convenient place to do that is the worker wrapper phase, as it runs after every
run of the demand analyser besides the very last one (which is the one where we
want to _keep_ the info for the code generator).

We do not do it in the demand analyser for the same reasons outlined in
Note [Zapping DmdEnv after Demand Analyzer] above.
-}


---------------------
splitFun :: DynFlags -> FamInstEnvs -> Id -> IdInfo -> [Demand] -> DmdResult -> CoreExpr
         -> UniqSM [(Id, CoreExpr)]
splitFun dflags fam_envs fn_id fn_info wrap_dmds res_info rhs
  = WARN( not (wrap_dmds `lengthIs` arity), ppr fn_id <+> (ppr arity $$ ppr wrap_dmds $$ ppr res_info) ) do
    -- The arity should match the signature
    stuff <- mkWwBodies dflags fam_envs rhs_fvs fn_id wrap_dmds use_res_info
    case stuff of
      Just (work_demands, join_arity, wrap_fn, work_fn) -> do
        work_uniq <- getUniqueM
        let work_rhs = work_fn rhs
            work_act = case fn_inline_spec of  -- See Note [Worker activation]
                          NoInline -> fn_act
                          _        -> wrap_act

            work_prag = InlinePragma { inl_src = SourceText "{-# INLINE"
                                     , inl_inline = fn_inline_spec
                                     , inl_sat    = Nothing
                                     , inl_act    = work_act
                                     , inl_rule   = FunLike }
              -- inl_inline: copy from fn_id; see Note [Worker-wrapper for INLINABLE functions]
              -- inl_act:    see Note [Worker activation]
              -- inl_rule:   it does not make sense for workers to be constructorlike.

            work_join_arity | isJoinId fn_id = Just join_arity
                            | otherwise      = Nothing
              -- worker is join point iff wrapper is join point
              -- (see Note [Don't CPR join points])

            work_id  = mkWorkerId work_uniq fn_id (exprType work_rhs)
                        `setIdOccInfo` occInfo fn_info
                                -- Copy over occurrence info from parent
                                -- Notably whether it's a loop breaker
                                -- Doesn't matter much, since we will simplify next, but
                                -- seems right-er to do so

                        `setInlinePragma` work_prag

                        `setIdUnfolding` mkWorkerUnfolding dflags work_fn fn_unfolding
                                -- See Note [Worker-wrapper for INLINABLE functions]

                        `setIdStrictness` mkClosedStrictSig work_demands work_res_info
                                -- Even though we may not be at top level,
                                -- it's ok to give it an empty DmdEnv

                        `setIdDemandInfo` worker_demand

                        `setIdArity` work_arity
                                -- Set the arity so that the Core Lint check that the
                                -- arity is consistent with the demand type goes
                                -- through
                        `asJoinId_maybe` work_join_arity

            work_arity = length work_demands

            -- See Note [Demand on the Worker]
            single_call = saturatedByOneShots arity (demandInfo fn_info)
            worker_demand | single_call = mkWorkerDemand work_arity
                          | otherwise   = topDmd

            wrap_rhs  = wrap_fn work_id
            wrap_act  = case fn_act of  -- See Note [Wrapper activation]
                           ActiveAfter {} -> fn_act
                           NeverActive    -> activeDuringFinal
                           _              -> activeAfterInitial
            wrap_prag = InlinePragma { inl_src    = SourceText "{-# INLINE"
                                     , inl_inline = NoUserInline
                                     , inl_sat    = Nothing
                                     , inl_act    = wrap_act
                                     , inl_rule   = rule_match_info }
                -- inl_act:    see Note [Wrapper activation]
                -- inl_inline: see Note [Wrapper NoUserInline]
                -- inl_rule:   RuleMatchInfo is (and must be) unaffected

            wrap_id   = fn_id `setIdUnfolding`  mkWwInlineRule dflags wrap_rhs arity
                              `setInlinePragma` wrap_prag
                              `setIdOccInfo`    noOccInfo
                                -- Zap any loop-breaker-ness, to avoid bleating from Lint
                                -- about a loop breaker with an INLINE rule



        return $ [(work_id, work_rhs), (wrap_id, wrap_rhs)]
            -- Worker first, because wrapper mentions it

      Nothing -> return [(fn_id, rhs)]
  where
    rhs_fvs         = exprFreeVars rhs
    fn_inl_prag     = inlinePragInfo fn_info
    fn_inline_spec  = inl_inline fn_inl_prag
    fn_act          = inl_act fn_inl_prag
    rule_match_info = inlinePragmaRuleMatchInfo fn_inl_prag
    fn_unfolding    = unfoldingInfo fn_info
    arity           = arityInfo fn_info
                    -- The arity is set by the simplifier using exprEtaExpandArity
                    -- So it may be more than the number of top-level-visible lambdas

    use_res_info  | isJoinId fn_id = topRes -- Note [Don't CPR join points]
                  | otherwise      = res_info
    work_res_info | isJoinId fn_id = res_info -- Worker remains CPR-able
                  | otherwise
                  = case returnsCPR_maybe res_info of
                       Just _  -> topRes    -- Cpr stuff done by wrapper; kill it here
                       Nothing -> res_info  -- Preserve exception/divergence


{-
Note [Demand on the worker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the original function is called once, according to its demand info, then
so is the worker. This is important so that the occurrence analyser can
attach OneShot annotations to the worker’s lambda binders.


Example:

  -- Original function
  f [Demand=<L,1*C1(U)>] :: (a,a) -> a
  f = \p -> ...

  -- Wrapper
  f [Demand=<L,1*C1(U)>] :: a -> a -> a
  f = \p -> case p of (a,b) -> $wf a b

  -- Worker
  $wf [Demand=<L,1*C1(C1(U))>] :: Int -> Int
  $wf = \a b -> ...

We need to check whether the original function is called once, with
sufficiently many arguments. This is done using saturatedByOneShots, which
takes the arity of the original function (resp. the wrapper) and the demand on
the original function.

The demand on the worker is then calculated using mkWorkerDemand, and always of
the form [Demand=<L,1*(C1(...(C1(U))))>]


Note [Do not split void functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this rather common form of binding:
        $j = \x:Void# -> ...no use of x...

Since x is not used it'll be marked as absent.  But there is no point
in w/w-ing because we'll simply add (\y:Void#), see WwLib.mkWorerArgs.

If x has a more interesting type (eg Int, or Int#), there *is* a point
in w/w so that we don't pass the argument at all.

Note [Thunk splitting]
~~~~~~~~~~~~~~~~~~~~~~
Suppose x is used strictly (never mind whether it has the CPR
property).

      let
        x* = x-rhs
      in body

splitThunk transforms like this:

      let
        x* = case x-rhs of { I# a -> I# a }
      in body

Now simplifier will transform to

      case x-rhs of
        I# a -> let x* = I# a
                in body

which is what we want. Now suppose x-rhs is itself a case:

        x-rhs = case e of { T -> I# a; F -> I# b }

The join point will abstract over a, rather than over (which is
what would have happened before) which is fine.

Notice that x certainly has the CPR property now!

In fact, splitThunk uses the function argument w/w splitting
function, so that if x's demand is deeper (say U(U(L,L),L))
then the splitting will go deeper too.
-}

-- See Note [Thunk splitting]
-- splitThunk converts the *non-recursive* binding
--      x = e
-- into
--      x = let x = e
--          in case x of
--               I# y -> let x = I# y in x }
-- See comments above. Is it not beautifully short?
-- Moreover, it works just as well when there are
-- several binders, and if the binders are lifted
-- E.g.     x = e
--     -->  x = let x = e in
--              case x of (a,b) -> let x = (a,b)  in x

splitThunk :: DynFlags -> FamInstEnvs -> RecFlag -> Var -> Expr Var -> UniqSM [(Var, Expr Var)]
splitThunk dflags fam_envs is_rec fn_id rhs
  = ASSERT(not (isJoinId fn_id))
    do { (useful,_, wrap_fn, work_fn) <- mkWWstr dflags fam_envs False [fn_id]
       ; let res = [ (fn_id, Let (NonRec fn_id rhs) (wrap_fn (work_fn (Var fn_id)))) ]
       ; if useful then ASSERT2( isNonRec is_rec, ppr fn_id ) -- The thunk must be non-recursive
                   return res
                   else return [(fn_id, rhs)] }
