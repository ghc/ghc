--
-- Copyright (c) 2014 Joachim Breitner
--

module GHC.Core.Op.CallArity
    ( callArityAnalProgram
    , callArityRHS -- for testing
    ) where

import GhcPrelude

import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Driver.Session ( DynFlags )

import GHC.Types.Basic
import GHC.Core
import GHC.Types.Id
import GHC.Core.Arity ( typeArity )
import GHC.Core.Utils ( exprIsCheap, exprIsTrivial )
import UnVarGraph
import GHC.Types.Demand
import Util

import Control.Arrow ( first, second )


{-
%************************************************************************
%*                                                                      *
              Call Arity Analysis
%*                                                                      *
%************************************************************************

Note [Call Arity: The goal]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The goal of this analysis is to find out if we can eta-expand a local function,
based on how it is being called. The motivating example is this code,
which comes up when we implement foldl using foldr, and do list fusion:

    let go = \x -> let d = case ... of
                              False -> go (x+1)
                              True  -> id
                   in \z -> d (x + z)
    in go 1 0

If we do not eta-expand `go` to have arity 2, we are going to allocate a lot of
partial function applications, which would be bad.

The function `go` has a type of arity two, but only one lambda is manifest.
Furthermore, an analysis that only looks at the RHS of go cannot be sufficient
to eta-expand go: If `go` is ever called with one argument (and the result used
multiple times), we would be doing the work in `...` multiple times.

So `callArityAnalProgram` looks at the whole let expression to figure out if
all calls are nice, i.e. have a high enough arity. It then stores the result in
the `calledArity` field of the `IdInfo` of `go`, which the next simplifier
phase will eta-expand.

The specification of the `calledArity` field is:

    No work will be lost if you eta-expand me to the arity in `calledArity`.

What we want to know for a variable
-----------------------------------

For every let-bound variable we'd like to know:
  1. A lower bound on the arity of all calls to the variable, and
  2. whether the variable is being called at most once or possible multiple
     times.

It is always ok to lower the arity, or pretend that there are multiple calls.
In particular, "Minimum arity 0 and possible called multiple times" is always
correct.


What we want to know from an expression
---------------------------------------

In order to obtain that information for variables, we analyze expression and
obtain bits of information:

 I.  The arity analysis:
     For every variable, whether it is absent, or called,
     and if called, which what arity.

 II. The Co-Called analysis:
     For every two variables, whether there is a possibility that both are being
     called.
     We obtain as a special case: For every variables, whether there is a
     possibility that it is being called twice.

For efficiency reasons, we gather this information only for a set of
*interesting variables*, to avoid spending time on, e.g., variables from pattern matches.

The two analysis are not completely independent, as a higher arity can improve
the information about what variables are being called once or multiple times.

Note [Analysis I: The arity analysis]
------------------------------------

The arity analysis is quite straight forward: The information about an
expression is an
    VarEnv Arity
where absent variables are bound to Nothing and otherwise to a lower bound to
their arity.

When we analyze an expression, we analyze it with a given context arity.
Lambdas decrease and applications increase the incoming arity. Analysizing a
variable will put that arity in the environment. In lets or cases all the
results from the various subexpressions are lubed, which takes the point-wise
minimum (considering Nothing an infinity).


Note [Analysis II: The Co-Called analysis]
------------------------------------------

The second part is more sophisticated. For reasons explained below, it is not
sufficient to simply know how often an expression evaluates a variable. Instead
we need to know which variables are possibly called together.

The data structure here is an undirected graph of variables, which is provided
by the abstract
    UnVarGraph

It is safe to return a larger graph, i.e. one with more edges. The worst case
(i.e. the least useful and always correct result) is the complete graph on all
free variables, which means that anything can be called together with anything
(including itself).

Notation for the following:
C(e)  is the co-called result for e.
G₁∪G₂ is the union of two graphs
fv    is the set of free variables (conveniently the domain of the arity analysis result)
S₁×S₂ is the complete bipartite graph { {a,b} | a ∈ S₁, b ∈ S₂ }
S²    is the complete graph on the set of variables S, S² = S×S
C'(e) is a variant for bound expression:
      If e is called at most once, or it is and stays a thunk (after the analysis),
      it is simply C(e). Otherwise, the expression can be called multiple times
      and we return (fv e)²

The interesting cases of the analysis:
 * Var v:
   No other variables are being called.
   Return {} (the empty graph)
 * Lambda v e, under arity 0:
   This means that e can be evaluated many times and we cannot get
   any useful co-call information.
   Return (fv e)²
 * Case alternatives alt₁,alt₂,...:
   Only one can be execuded, so
   Return (alt₁ ∪ alt₂ ∪...)
 * App e₁ e₂ (and analogously Case scrut alts), with non-trivial e₂:
   We get the results from both sides, with the argument evaluated at most once.
   Additionally, anything called by e₁ can possibly be called with anything
   from e₂.
   Return: C(e₁) ∪ C(e₂) ∪ (fv e₁) × (fv e₂)
 * App e₁ x:
   As this is already in A-normal form, CorePrep will not separately lambda
   bind (and hence share) x. So we conservatively assume multiple calls to x here
   Return: C(e₁) ∪ (fv e₁) × {x} ∪ {(x,x)}
 * Let v = rhs in body:
   In addition to the results from the subexpressions, add all co-calls from
   everything that the body calls together with v to everything that is called
   by v.
   Return: C'(rhs) ∪ C(body) ∪ (fv rhs) × {v'| {v,v'} ∈ C(body)}
 * Letrec v₁ = rhs₁ ... vₙ = rhsₙ in body
   Tricky.
   We assume that it is really mutually recursive, i.e. that every variable
   calls one of the others, and that this is strongly connected (otherwise we
   return an over-approximation, so that's ok), see note [Recursion and fixpointing].

   Let V = {v₁,...vₙ}.
   Assume that the vs have been analysed with an incoming demand and
   cardinality consistent with the final result (this is the fixed-pointing).
   Again we can use the results from all subexpressions.
   In addition, for every variable vᵢ, we need to find out what it is called
   with (call this set Sᵢ). There are two cases:
    * If vᵢ is a function, we need to go through all right-hand-sides and bodies,
      and collect every variable that is called together with any variable from V:
      Sᵢ = {v' | j ∈ {1,...,n},      {v',vⱼ} ∈ C'(rhs₁) ∪ ... ∪ C'(rhsₙ) ∪ C(body) }
    * If vᵢ is a thunk, then its rhs is evaluated only once, so we need to
      exclude it from this set:
      Sᵢ = {v' | j ∈ {1,...,n}, j≠i, {v',vⱼ} ∈ C'(rhs₁) ∪ ... ∪ C'(rhsₙ) ∪ C(body) }
   Finally, combine all this:
   Return: C(body) ∪
           C'(rhs₁) ∪ ... ∪ C'(rhsₙ) ∪
           (fv rhs₁) × S₁) ∪ ... ∪ (fv rhsₙ) × Sₙ)

Using the result: Eta-Expansion
-------------------------------

We use the result of these two analyses to decide whether we can eta-expand the
rhs of a let-bound variable.

If the variable is already a function (exprIsCheap), and all calls to the
variables have a higher arity than the current manifest arity (i.e. the number
of lambdas), expand.

If the variable is a thunk we must be careful: Eta-Expansion will prevent
sharing of work, so this is only safe if there is at most one call to the
function. Therefore, we check whether {v,v} ∈ G.

    Example:

        let n = case .. of .. -- A thunk!
        in n 0 + n 1

    vs.

        let n = case .. of ..
        in case .. of T -> n 0
                      F -> n 1

    We are only allowed to eta-expand `n` if it is going to be called at most
    once in the body of the outer let. So we need to know, for each variable
    individually, that it is going to be called at most once.


Why the co-call graph?
----------------------

Why is it not sufficient to simply remember which variables are called once and
which are called multiple times? It would be in the previous example, but consider

        let n = case .. of ..
        in case .. of
            True -> let go = \y -> case .. of
                                     True -> go (y + n 1)
                                     False > n
                    in go 1
            False -> n

vs.

        let n = case .. of ..
        in case .. of
            True -> let go = \y -> case .. of
                                     True -> go (y+1)
                                     False > n
                    in go 1
            False -> n

In both cases, the body and the rhs of the inner let call n at most once.
But only in the second case that holds for the whole expression! The
crucial difference is that in the first case, the rhs of `go` can call
*both* `go` and `n`, and hence can call `n` multiple times as it recurses,
while in the second case find out that `go` and `n` are not called together.


Why co-call information for functions?
--------------------------------------

Although for eta-expansion we need the information only for thunks, we still
need to know whether functions are being called once or multiple times, and
together with what other functions.

    Example:

        let n = case .. of ..
            f x = n (x+1)
        in f 1 + f 2

    vs.

        let n = case .. of ..
            f x = n (x+1)
        in case .. of T -> f 0
                      F -> f 1

    Here, the body of f calls n exactly once, but f itself is being called
    multiple times, so eta-expansion is not allowed.


Note [Analysis type signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The work-hourse of the analysis is the function `callArityAnal`, with the
following type:

    type CallArityRes = (UnVarGraph, VarEnv Arity)
    callArityAnal ::
        Arity ->  -- The arity this expression is called with
        VarSet -> -- The set of interesting variables
        CoreExpr ->  -- The expression to analyse
        (CallArityRes, CoreExpr)

and the following specification:

  ((coCalls, callArityEnv), expr') = callArityEnv arity interestingIds expr

                            <=>

  Assume the expression `expr` is being passed `arity` arguments. Then it holds that
    * The domain of `callArityEnv` is a subset of `interestingIds`.
    * Any variable from `interestingIds` that is not mentioned in the `callArityEnv`
      is absent, i.e. not called at all.
    * Every call from `expr` to a variable bound to n in `callArityEnv` has at
      least n value arguments.
    * For two interesting variables `v1` and `v2`, they are not adjacent in `coCalls`,
      then in no execution of `expr` both are being called.
  Furthermore, expr' is expr with the callArity field of the `IdInfo` updated.


Note [Which variables are interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The analysis would quickly become prohibitive expensive if we would analyse all
variables; for most variables we simply do not care about how often they are
called, i.e. variables bound in a pattern match. So interesting are variables that are
 * top-level or let bound
 * and possibly functions (typeArity > 0)

Note [Taking boring variables into account]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we decide that the variable bound in `let x = e1 in e2` is not interesting,
the analysis of `e2` will not report anything about `x`. To ensure that
`callArityBind` does still do the right thing we have to take that into account
every time we would be lookup up `x` in the analysis result of `e2`.
  * Instead of calling lookupCallArityRes, we return (0, True), indicating
    that this variable might be called many times with no arguments.
  * Instead of checking `calledWith x`, we assume that everything can be called
    with it.
  * In the recursive case, when calclulating the `cross_calls`, if there is
    any boring variable in the recursive group, we ignore all co-call-results
    and directly go to a very conservative assumption.

The last point has the nice side effect that the relatively expensive
integration of co-call results in a recursive groups is often skipped. This
helped to avoid the compile time blowup in some real-world code with large
recursive groups (#10293).

Note [Recursion and fixpointing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a mutually recursive let, we begin by
 1. analysing the body, using the same incoming arity as for the whole expression.
 2. Then we iterate, memoizing for each of the bound variables the last
    analysis call, i.e. incoming arity, whether it is called once, and the CallArityRes.
 3. We combine the analysis result from the body and the memoized results for
    the arguments (if already present).
 4. For each variable, we find out the incoming arity and whether it is called
    once, based on the current analysis result. If this differs from the
    memoized results, we re-analyse the rhs and update the memoized table.
 5. If nothing had to be reanalyzed, we are done.
    Otherwise, repeat from step 3.


Note [Thunks in recursive groups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We never eta-expand a thunk in a recursive group, on the grounds that if it is
part of a recursive group, then it will be called multiple times.

This is not necessarily true, e.g.  it would be safe to eta-expand t2 (but not
t1) in the following code:

  let go x = t1
      t1 = if ... then t2 else ...
      t2 = if ... then go 1 else ...
  in go 0

Detecting this would require finding out what variables are only ever called
from thunks. While this is certainly possible, we yet have to see this to be
relevant in the wild.


Note [Analysing top-level binds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can eta-expand top-level-binds if they are not exported, as we see all calls
to them. The plan is as follows: Treat the top-level binds as nested lets around
a body representing “all external calls”, which returns a pessimistic
CallArityRes (the co-call graph is the complete graph, all arityies 0).

Note [Trimming arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the Call Arity papers, we are working on an untyped lambda calculus with no
other id annotations, where eta-expansion is always possible. But this is not
the case for Core!
 1. We need to ensure the invariant
      callArity e <= typeArity (exprType e)
    for the same reasons that exprArity needs this invariant (see Note
    [exprArity invariant] in GHC.Core.Arity).

    If we are not doing that, a too-high arity annotation will be stored with
    the id, confusing the simplifier later on.

 2. Eta-expanding a right hand side might invalidate existing annotations. In
    particular, if an id has a strictness annotation of <...><...>b, then
    passing two arguments to it will definitely bottom out, so the simplifier
    will throw away additional parameters. This conflicts with Call Arity! So
    we ensure that we never eta-expand such a value beyond the number of
    arguments mentioned in the strictness signature.
    See #10176 for a real-world-example.

Note [What is a thunk]
~~~~~~~~~~~~~~~~~~~~~~

Originally, everything that is not in WHNF (`exprIsWHNF`) is considered a
thunk, not eta-expanded, to avoid losing any sharing. This is also how the
published papers on Call Arity describe it.

In practice, there are thunks that do a just little work, such as
pattern-matching on a variable, and the benefits of eta-expansion likely
outweigh the cost of doing that repeatedly. Therefore, this implementation of
Call Arity considers everything that is not cheap (`exprIsCheap`) as a thunk.

Note [Call Arity and Join Points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Call Arity analysis does not care about join points, and treats them just
like normal functions. This is ok.

The analysis *could* make use of the fact that join points are always evaluated
in the same context as the join-binding they are defined in and are always
one-shot, and handle join points separately, as suggested in
https://gitlab.haskell.org/ghc/ghc/issues/13479#note_134870.
This *might* be more efficient (for example, join points would not have to be
considered interesting variables), but it would also add redundant code. So for
now we do not do that.

The simplifier never eta-expands join points (it instead pushes extra arguments from
an eta-expanded context into the join point’s RHS), so the call arity
annotation on join points is not actually used. As it would be equally valid
(though less efficient) to eta-expand join points, this is the simplifier's
choice, and hence Call Arity sets the call arity for join points as well.
-}

-- Main entry point

callArityAnalProgram :: DynFlags -> CoreProgram -> CoreProgram
callArityAnalProgram _dflags binds = binds'
  where
    (_, binds') = callArityTopLvl [] emptyVarSet binds

-- See Note [Analysing top-level-binds]
callArityTopLvl :: [Var] -> VarSet -> [CoreBind] -> (CallArityRes, [CoreBind])
callArityTopLvl exported _ []
    = ( calledMultipleTimes $ (emptyUnVarGraph, mkVarEnv $ [(v, 0) | v <- exported])
      , [] )
callArityTopLvl exported int1 (b:bs)
    = (ae2, b':bs')
  where
    int2 = bindersOf b
    exported' = filter isExportedId int2 ++ exported
    int' = int1 `addInterestingBinds` b
    (ae1, bs') = callArityTopLvl exported' int' bs
    (ae2, b')  = callArityBind (boringBinds b) ae1 int1 b


callArityRHS :: CoreExpr -> CoreExpr
callArityRHS = snd . callArityAnal 0 emptyVarSet

-- The main analysis function. See Note [Analysis type signature]
callArityAnal ::
    Arity ->  -- The arity this expression is called with
    VarSet -> -- The set of interesting variables
    CoreExpr ->  -- The expression to analyse
    (CallArityRes, CoreExpr)
        -- How this expression uses its interesting variables
        -- and the expression with IdInfo updated

-- The trivial base cases
callArityAnal _     _   e@(Lit _)
    = (emptyArityRes, e)
callArityAnal _     _   e@(Type _)
    = (emptyArityRes, e)
callArityAnal _     _   e@(Coercion _)
    = (emptyArityRes, e)
-- The transparent cases
callArityAnal arity int (Tick t e)
    = second (Tick t) $ callArityAnal arity int e
callArityAnal arity int (Cast e co)
    = second (\e -> Cast e co) $ callArityAnal arity int e

-- The interesting case: Variables, Lambdas, Lets, Applications, Cases
callArityAnal arity int e@(Var v)
    | v `elemVarSet` int
    = (unitArityRes v arity, e)
    | otherwise
    = (emptyArityRes, e)

-- Non-value lambdas are ignored
callArityAnal arity int (Lam v e) | not (isId v)
    = second (Lam v) $ callArityAnal arity (int `delVarSet` v) e

-- We have a lambda that may be called multiple times, so its free variables
-- can all be co-called.
callArityAnal 0     int (Lam v e)
    = (ae', Lam v e')
  where
    (ae, e') = callArityAnal 0 (int `delVarSet` v) e
    ae' = calledMultipleTimes ae
-- We have a lambda that we are calling. decrease arity.
callArityAnal arity int (Lam v e)
    = (ae, Lam v e')
  where
    (ae, e') = callArityAnal (arity - 1) (int `delVarSet` v) e

-- Application. Increase arity for the called expression, nothing to know about
-- the second
callArityAnal arity int (App e (Type t))
    = second (\e -> App e (Type t)) $ callArityAnal arity int e
callArityAnal arity int (App e1 e2)
    = (final_ae, App e1' e2')
  where
    (ae1, e1') = callArityAnal (arity + 1) int e1
    (ae2, e2') = callArityAnal 0           int e2
    -- If the argument is trivial (e.g. a variable), then it will _not_ be
    -- let-bound in the Core to STG transformation (CorePrep actually),
    -- so no sharing will happen here, and we have to assume many calls.
    ae2' | exprIsTrivial e2 = calledMultipleTimes ae2
         | otherwise        = ae2
    final_ae = ae1 `both` ae2'

-- Case expression.
callArityAnal arity int (Case scrut bndr ty alts)
    = -- pprTrace "callArityAnal:Case"
      --          (vcat [ppr scrut, ppr final_ae])
      (final_ae, Case scrut' bndr ty alts')
  where
    (alt_aes, alts') = unzip $ map go alts
    go (dc, bndrs, e) = let (ae, e') = callArityAnal arity int e
                        in  (ae, (dc, bndrs, e'))
    alt_ae = lubRess alt_aes
    (scrut_ae, scrut') = callArityAnal 0 int scrut
    final_ae = scrut_ae `both` alt_ae

-- For lets, use callArityBind
callArityAnal arity int (Let bind e)
  = -- pprTrace "callArityAnal:Let"
    --          (vcat [ppr v, ppr arity, ppr n, ppr final_ae ])
    (final_ae, Let bind' e')
  where
    int_body = int `addInterestingBinds` bind
    (ae_body, e') = callArityAnal arity int_body e
    (final_ae, bind') = callArityBind (boringBinds bind) ae_body int bind

-- Which bindings should we look at?
-- See Note [Which variables are interesting]
isInteresting :: Var -> Bool
isInteresting v = not $ null (typeArity (idType v))

interestingBinds :: CoreBind -> [Var]
interestingBinds = filter isInteresting . bindersOf

boringBinds :: CoreBind -> VarSet
boringBinds = mkVarSet . filter (not . isInteresting) . bindersOf

addInterestingBinds :: VarSet -> CoreBind -> VarSet
addInterestingBinds int bind
    = int `delVarSetList`    bindersOf bind -- Possible shadowing
          `extendVarSetList` interestingBinds bind

-- Used for both local and top-level binds
-- Second argument is the demand from the body
callArityBind :: VarSet -> CallArityRes -> VarSet -> CoreBind -> (CallArityRes, CoreBind)
-- Non-recursive let
callArityBind boring_vars ae_body int (NonRec v rhs)
  | otherwise
  = -- pprTrace "callArityBind:NonRec"
    --          (vcat [ppr v, ppr ae_body, ppr int, ppr ae_rhs, ppr safe_arity])
    (final_ae, NonRec v' rhs')
  where
    is_thunk = not (exprIsCheap rhs) -- see note [What is a thunk]
    -- If v is boring, we will not find it in ae_body, but always assume (0, False)
    boring = v `elemVarSet` boring_vars

    (arity, called_once)
        | boring    = (0, False) -- See Note [Taking boring variables into account]
        | otherwise = lookupCallArityRes ae_body v
    safe_arity | called_once = arity
               | is_thunk    = 0      -- A thunk! Do not eta-expand
               | otherwise   = arity

    -- See Note [Trimming arity]
    trimmed_arity = trimArity v safe_arity

    (ae_rhs, rhs') = callArityAnal trimmed_arity int rhs


    ae_rhs'| called_once     = ae_rhs
           | safe_arity == 0 = ae_rhs -- If it is not a function, its body is evaluated only once
           | otherwise       = calledMultipleTimes ae_rhs

    called_by_v = domRes ae_rhs'
    called_with_v
        | boring    = domRes ae_body
        | otherwise = calledWith ae_body v `delUnVarSet` v
    final_ae = addCrossCoCalls called_by_v called_with_v $ ae_rhs' `lubRes` resDel v ae_body

    v' = v `setIdCallArity` trimmed_arity


-- Recursive let. See Note [Recursion and fixpointing]
callArityBind boring_vars ae_body int b@(Rec binds)
  = -- (if length binds > 300 then
    -- pprTrace "callArityBind:Rec"
    --           (vcat [ppr (Rec binds'), ppr ae_body, ppr int, ppr ae_rhs]) else id) $
    (final_ae, Rec binds')
  where
    -- See Note [Taking boring variables into account]
    any_boring = any (`elemVarSet` boring_vars) [ i | (i, _) <- binds]

    int_body = int `addInterestingBinds` b
    (ae_rhs, binds') = fix initial_binds
    final_ae = bindersOf b `resDelList` ae_rhs

    initial_binds = [(i,Nothing,e) | (i,e) <- binds]

    fix :: [(Id, Maybe (Bool, Arity, CallArityRes), CoreExpr)] -> (CallArityRes, [(Id, CoreExpr)])
    fix ann_binds
        | -- pprTrace "callArityBind:fix" (vcat [ppr ann_binds, ppr any_change, ppr ae]) $
          any_change
        = fix ann_binds'
        | otherwise
        = (ae, map (\(i, _, e) -> (i, e)) ann_binds')
      where
        aes_old = [ (i,ae) | (i, Just (_,_,ae), _) <- ann_binds ]
        ae = callArityRecEnv any_boring aes_old ae_body

        rerun (i, mbLastRun, rhs)
            | i `elemVarSet` int_body && not (i `elemUnVarSet` domRes ae)
            -- No call to this yet, so do nothing
            = (False, (i, Nothing, rhs))

            | Just (old_called_once, old_arity, _) <- mbLastRun
            , called_once == old_called_once
            , new_arity == old_arity
            -- No change, no need to re-analyze
            = (False, (i, mbLastRun, rhs))

            | otherwise
            -- We previously analyzed this with a different arity (or not at all)
            = let is_thunk = not (exprIsCheap rhs) -- see note [What is a thunk]

                  safe_arity | is_thunk    = 0  -- See Note [Thunks in recursive groups]
                             | otherwise   = new_arity

                  -- See Note [Trimming arity]
                  trimmed_arity = trimArity i safe_arity

                  (ae_rhs, rhs') = callArityAnal trimmed_arity int_body rhs

                  ae_rhs' | called_once     = ae_rhs
                          | safe_arity == 0 = ae_rhs -- If it is not a function, its body is evaluated only once
                          | otherwise       = calledMultipleTimes ae_rhs

                  i' = i `setIdCallArity` trimmed_arity

              in (True, (i', Just (called_once, new_arity, ae_rhs'), rhs'))
          where
            -- See Note [Taking boring variables into account]
            (new_arity, called_once) | i `elemVarSet` boring_vars = (0, False)
                                     | otherwise                  = lookupCallArityRes ae i

        (changes, ann_binds') = unzip $ map rerun ann_binds
        any_change = or changes

-- Combining the results from body and rhs, (mutually) recursive case
-- See Note [Analysis II: The Co-Called analysis]
callArityRecEnv :: Bool -> [(Var, CallArityRes)] -> CallArityRes -> CallArityRes
callArityRecEnv any_boring ae_rhss ae_body
    = -- (if length ae_rhss > 300 then pprTrace "callArityRecEnv" (vcat [ppr ae_rhss, ppr ae_body, ppr ae_new]) else id) $
      ae_new
  where
    vars = map fst ae_rhss

    ae_combined = lubRess (map snd ae_rhss) `lubRes` ae_body

    cross_calls
        -- See Note [Taking boring variables into account]
        | any_boring               = completeGraph (domRes ae_combined)
        -- Also, calculating cross_calls is expensive. Simply be conservative
        -- if the mutually recursive group becomes too large.
        | lengthExceeds ae_rhss 25 = completeGraph (domRes ae_combined)
        | otherwise                = unionUnVarGraphs $ map cross_call ae_rhss
    cross_call (v, ae_rhs) = completeBipartiteGraph called_by_v called_with_v
      where
        is_thunk = idCallArity v == 0
        -- What rhs are relevant as happening before (or after) calling v?
        --    If v is a thunk, everything from all the _other_ variables
        --    If v is not a thunk, everything can happen.
        ae_before_v | is_thunk  = lubRess (map snd $ filter ((/= v) . fst) ae_rhss) `lubRes` ae_body
                    | otherwise = ae_combined
        -- What do we want to know from these?
        -- Which calls can happen next to any recursive call.
        called_with_v
            = unionUnVarSets $ map (calledWith ae_before_v) vars
        called_by_v = domRes ae_rhs

    ae_new = first (cross_calls `unionUnVarGraph`) ae_combined

-- See Note [Trimming arity]
trimArity :: Id -> Arity -> Arity
trimArity v a = minimum [a, max_arity_by_type, max_arity_by_strsig]
  where
    max_arity_by_type = length (typeArity (idType v))
    max_arity_by_strsig
        | isDeadEndDiv result_info = length demands
        | otherwise = a

    (demands, result_info) = splitStrictSig (idStrictness v)

---------------------------------------
-- Functions related to CallArityRes --
---------------------------------------

-- Result type for the two analyses.
-- See Note [Analysis I: The arity analysis]
-- and Note [Analysis II: The Co-Called analysis]
type CallArityRes = (UnVarGraph, VarEnv Arity)

emptyArityRes :: CallArityRes
emptyArityRes = (emptyUnVarGraph, emptyVarEnv)

unitArityRes :: Var -> Arity -> CallArityRes
unitArityRes v arity = (emptyUnVarGraph, unitVarEnv v arity)

resDelList :: [Var] -> CallArityRes -> CallArityRes
resDelList vs ae = foldr resDel ae vs

resDel :: Var -> CallArityRes -> CallArityRes
resDel v (g, ae) = (g `delNode` v, ae `delVarEnv` v)

domRes :: CallArityRes -> UnVarSet
domRes (_, ae) = varEnvDom ae

-- In the result, find out the minimum arity and whether the variable is called
-- at most once.
lookupCallArityRes :: CallArityRes -> Var -> (Arity, Bool)
lookupCallArityRes (g, ae) v
    = case lookupVarEnv ae v of
        Just a -> (a, not (g `hasLoopAt` v))
        Nothing -> (0, False)

calledWith :: CallArityRes -> Var -> UnVarSet
calledWith (g, _) v = neighbors g v

addCrossCoCalls :: UnVarSet -> UnVarSet -> CallArityRes -> CallArityRes
addCrossCoCalls set1 set2 = first (completeBipartiteGraph set1 set2 `unionUnVarGraph`)

-- Replaces the co-call graph by a complete graph (i.e. no information)
calledMultipleTimes :: CallArityRes -> CallArityRes
calledMultipleTimes res = first (const (completeGraph (domRes res))) res

-- Used for application and cases
both :: CallArityRes -> CallArityRes -> CallArityRes
both r1 r2 = addCrossCoCalls (domRes r1) (domRes r2) $ r1 `lubRes` r2

-- Used when combining results from alternative cases; take the minimum
lubRes :: CallArityRes -> CallArityRes -> CallArityRes
lubRes (g1, ae1) (g2, ae2) = (g1 `unionUnVarGraph` g2, ae1 `lubArityEnv` ae2)

lubArityEnv :: VarEnv Arity -> VarEnv Arity -> VarEnv Arity
lubArityEnv = plusVarEnv_C min

lubRess :: [CallArityRes] -> CallArityRes
lubRess = foldl' lubRes emptyArityRes
