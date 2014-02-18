--
-- Copyright (c) 2014 Joachim Breitner
--

module CallArity
    ( callArityAnalProgram
    , callArityRHS -- for testing
    ) where

import VarSet
import VarEnv
import DynFlags ( DynFlags )

import BasicTypes
import CoreSyn
import Id
import CoreArity ( exprArity, typeArity )
import CoreUtils ( exprIsHNF )
import Outputable

import Control.Arrow ( first, second )


{-
%************************************************************************
%*									*
              Call Arity Analyis
%*									*
%************************************************************************

Note [Call Arity: The goal]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The goal of this analysis is to find out if we can eta-expand a local function,
based on how it is being called. The motivating example is code this this,
which comes up when we implement foldl using foldr, and do list fusion:

    let go = \x -> let d = case ... of
                              False -> go (x+1)
                              True  -> id
                   in \z -> d (x + z)
    in go 1 0

If we do not eta-expand `go` to have arity 2, we are going to allocate a lot of
partial function applications, which would be bad.

The function `go` has a type of arity two, but only one lambda is manifest.
Further more, an analysis that only looks at the RHS of go cannot be sufficient
to eta-expand go: If `go` is ever called with one argument (and the result used
multiple times), we would be doing the work in `...` multiple times.

So `callArityAnalProgram` looks at the whole let expression to figure out if
all calls are nice, i.e. have a high enough arity. It then stores the result in
the `calledArity` field of the `IdInfo` of `go`, which the next simplifier
phase will eta-expand.

The specification of the `calledArity` field is:

    No work will be lost if you eta-expand me to the arity in `calledArity`.

The specification of the analysis
---------------------------------

The analysis only does a conservative approximation, there are plenty of
situations where eta-expansion would be ok, but we do not catch it. We are
content if all the code that foldl-via-foldr generates is being optimized
sufficiently.

The work-hourse of the analysis is the function `callArityAnal`, with the
following type:

    data Count = Many | OnceAndOnly
    type CallCount = (Count, Arity)
    type CallArityEnv = VarEnv (CallCount, Arity)
    callArityAnal ::
        Arity ->  -- The arity this expression is called with
        VarSet -> -- The set of interesting variables
        CoreExpr ->  -- The expression to analyse
        (CallArityEnv, CoreExpr)

and the following specification:

  (callArityEnv, expr') = callArityEnv arity interestingIds expr

                            <=>

  Assume the expression `expr` is being passed `arity` arguments. Then it calls
  the functions mentioned in `interestingIds` according to `callArityEnv`:
    * The domain of `callArityEnv` is a subset of `interestingIds`.
    * Any variable from interestingIds that is not mentioned in the `callArityEnv`
      is absent, i.e. not called at all.
    * Of all the variables that are mapped to OnceAndOnly by the `callArityEnv`,
      at most one is being called, at most once, with at least that many
      arguments.
    * Variables mapped to Many are called an unknown number of times, but if they
      are called, then with at least that many arguments.
  Furthermore, expr' is expr with the callArity field of the `IdInfo` updated.

The (pointwise) domain is a product domain:

          Many               0
           |         ×       |
       OneAndOnly            1
                             |
                            ...

The at-most-once is important for various reasons:

 1. Consider:

        let n = case .. of .. -- A thunk!
        in n 0 + n 1

    vs.

        let n = case .. of ..
        in case .. of T -> n 0
                      F -> n 1

    We are only allowed to eta-expand `n` if it is going to be called at most
    once in the body of the outer let. So we need to know, for each variable
    individually, that it is going to be called at most once.

 2. We need to know it for non-thunks as well, because they might call a thunk:

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

 3. We need to know that at most one of the interesting functions is being
    called, because of recursion. Consider:

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
    while in the second case it calls `go` or `n`, but not both.

Note [Which variables are interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unfortunately, the set of interesting variables is not irrelevant for the
precision of the analysis. Consider this example (and ignore the pointlessnes
of `d` recursing into itself): 

    let n = ... :: Int
    in  let d = let d = case ... of
                           False -> d
                           True  -> id
                in \z -> d (x + z)
        in d 0

Of course, `d` should be interesting. If we consider `n` as interesting as
well, then the body of the second let will return
    { go |-> (Many, 1) ,       n |-> (OnceAndOnly, 0) }
or
    { go |-> (OnceAndOnly, 1), n |-> (Many, 0)}.
Only the latter is useful, but it is hard to decide that locally.
(Returning OnceAndOnly for both would be wrong, as both are being called.)

So the heuristics is:

    Variables are interesting if their RHS has a lower exprArity than
    typeArity.

(which is precisely the those variables where this analysis can actually cause
some eta-expansion.)

But this is not uniformly a win. Consider:

    let go = \x -> let d = case ... of
                              False -> go (x+1)
                              True  -> id
                       n x = d (x+1)
                   in \z -> n (x + z)
    in go n 0

Now `n` is not going to be considered interesting (its type is `Int -> Int`).
But this will prevent us from detecting how often the body of the let calls
`d`, and we will not find out anything.

It might be possible to be smarter here; this needs find-tuning as we find more
examples.


Note [Recursion and fixpointing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a recursive let, we begin by analysing the body, using the same incoming
arity as for the whole expression.
 * We use the arity from the body on the variable as the incoming demand on the
   rhs. Then we check if the rhs calls itself with the same arity.
   - If so, we are done.
   - If not, we re-analise the rhs with the reduced arity. We do that until
     we are down to the exprArity, which then is certainly correct.
 * If the rhs calls itself many times, we must (conservatively) pass the result
   through forgetOnceCalls.
 * Similarly, if the body calls the variable many times, we must pass the
   result of the fixpointing through forgetOnceCalls.
 * Then we can `lubEnv` the results from the body and the rhs: If all mentioned
   calls are OnceAndOnly calls, then the body calls *either* the rhs *or* one
   of the other mentioned variables. Similarly, the rhs calls *either* itself
   again *or* one of the other mentioned variables. This precision is required!
   If the recursive function is called by the body, or the rhs, tagged with Many
   then we can also just `lubEnv`, because the result will no longer contain
   any OnceAndOnly values.

Note [Case and App: Which side to take?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Combining the case branches is easy, just `lubEnv` them – at most one branch is
taken.

But how to combine that with the information coming from the scrunitee? Very
similarly, how to combine the information from the callee and argument of an
`App`?

It would not be correct to just `lubEnv` then: `f n` obviously calls *both* `f`
and `n`. We need to forget about the cardinality of calls from one side using
`forgetOnceCalls`. But which one?

Both are correct, and sometimes one and sometimes the other is more precise
(also see example in [Which variables are interesting]).

So currently, we first check the scrunitee (resp. the callee) if the returned
value has any usesful information, and if so, we use that; otherwise we use the
information from the alternatives (resp. the argument).

It might be smarter to look for “more important” variables first, i.e. the
innermost recursive variable.

Note [Analysing top-level binds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can eta-expand top-level-binds if they are not exported, as we see all calls
to them. The plan is as follows: Treat the top-level binds as nested lets around
a body representing “all external calls”, which returns a CallArityEnv that calls
every exported function with the top of the lattice.

This means that the incoming arity on all top-level binds will have a Many
attached, and we will never eta-expand CAFs. Which is good.

-}

callArityAnalProgram :: DynFlags -> CoreProgram -> CoreProgram
callArityAnalProgram _dflags binds = binds'
  where
    (_, binds') = callArityTopLvl [] emptyVarSet binds

-- See Note [Analysing top-level-binds]
callArityTopLvl :: [Var] -> VarSet -> [CoreBind] -> (CallArityEnv, [CoreBind])
callArityTopLvl exported _ []
    = (mkVarEnv $ zip exported (repeat topCallCount), [])
callArityTopLvl exported int1 (b:bs)
    = (ae2, b':bs')
  where
    int2 = interestingBinds b
    exported' = filter isExportedId int2 ++ exported
    int' = int1 `addInterestingBinds` b
    (ae1, bs') = callArityTopLvl exported' int' bs
    (ae2, b')  = callArityBind ae1 int1 b


callArityRHS :: CoreExpr -> CoreExpr
callArityRHS = snd . callArityAnal 0 emptyVarSet


data Count = Many | OnceAndOnly deriving (Eq, Ord)
type CallCount = (Count, Arity)

topCallCount :: CallCount
topCallCount = (Many, 0)

type CallArityEnv = VarEnv CallCount

callArityAnal ::
    Arity ->  -- The arity this expression is called with
    VarSet -> -- The set of interesting variables
    CoreExpr ->  -- The expression to analyse
    (CallArityEnv, CoreExpr)
        -- How this expression uses its interesting variables
        -- and the expression with IdInfo updated

-- The trivial base cases
callArityAnal _     _   e@(Lit _)
    = (emptyVarEnv, e)
callArityAnal _     _   e@(Type _)
    = (emptyVarEnv, e)
callArityAnal _     _   e@(Coercion _)
    = (emptyVarEnv, e)
-- The transparent cases
callArityAnal arity int (Tick t e)
    = second (Tick t) $ callArityAnal arity int e
callArityAnal arity int (Cast e co)
    = second (\e -> Cast e co) $ callArityAnal arity int e

-- The interesting case: Variables, Lambdas, Lets, Applications, Cases
callArityAnal arity int e@(Var v)
    | v `elemVarSet` int
    = (unitVarEnv v (OnceAndOnly, arity), e)
    | otherwise
    = (emptyVarEnv, e)

-- Non-value lambdas are ignored
callArityAnal arity int (Lam v e) | not (isId v)
    = second (Lam v) $ callArityAnal arity (int `delVarSet` v) e

-- We have a lambda that we are not sure to call. Tail calls therein
-- are no longer OneAndOnly calls
callArityAnal 0     int (Lam v e)
    = (ae', Lam v e')
  where
    (ae, e') = callArityAnal 0 (int `delVarSet` v) e
    ae' = forgetOnceCalls ae
-- We have a lambda that we are calling. decrease arity.
callArityAnal arity int (Lam v e)
    = (ae, Lam v e')
  where
    (ae, e') = callArityAnal (arity - 1) (int `delVarSet` v) e

-- For lets, use callArityBind
callArityAnal arity int (Let bind e)
  = -- pprTrace "callArityAnal:Let"
    --          (vcat [ppr v, ppr arity, ppr n, ppr final_ae ])
    (final_ae, Let bind' e')
  where
    int_body = int `addInterestingBinds` bind
    (ae_body, e') = callArityAnal arity int_body e
    (final_ae, bind') = callArityBind ae_body int bind


-- Application. Increase arity for the called expresion, nothing to know about
-- the second
callArityAnal arity int (App e (Type t))
    = second (\e -> App e (Type t)) $ callArityAnal arity int e
callArityAnal arity int (App e1 e2)
    = (final_ae, App e1' e2')
  where
    (ae1, e1') = callArityAnal (arity + 1) int e1
    (ae2, e2') = callArityAnal 0           int e2
    -- See Note [Case and App: Which side to take?]
    final_ae = ae1 `useBetterOf` ae2

-- Case expression. Here we decide whether
-- we want to look at calls from the scrunitee or the alternatives;
-- one of them we set to Nothing.
-- Naive idea: If there are interesting calls in the scrunitee,
-- zap the alternatives
callArityAnal arity int (Case scrut bndr ty alts)
    = -- pprTrace "callArityAnal:Case"
      --          (vcat [ppr scrut, ppr final_ae])
      (final_ae, Case scrut' bndr ty alts')
  where
    (alt_aes, alts') = unzip $ map go alts
    go (dc, bndrs, e) = let (ae, e') = callArityAnal arity int e
                        in  (ae, (dc, bndrs, e'))
    alt_ae = foldl lubEnv emptyVarEnv alt_aes
    (scrut_ae, scrut') = callArityAnal 0 int scrut
    -- See Note [Case and App: Which side to take?]
    final_ae = scrut_ae `useBetterOf` alt_ae

-- Which bindings should we look at?
-- See Note [Which variables are interesting]
interestingBinds :: CoreBind -> [Var]
interestingBinds bind =
    map fst $ filter go $ case bind of (NonRec v e) -> [(v,e)]
                                       (Rec ves)    -> ves
  where
    go (v,e) = exprArity e < length (typeArity (idType v))

addInterestingBinds :: VarSet -> CoreBind -> VarSet
addInterestingBinds int bind
    = int `delVarSetList`    bindersOf bind -- Possible shadowing
          `extendVarSetList` interestingBinds bind

-- This function pretens a (Many 0) call for every variable bound in the binder
-- that is not interesting, as calls to these are not reported by the analysis.
fakeBoringCalls :: VarSet -> CoreBind -> CallArityEnv
fakeBoringCalls int bind
    = mkVarEnv [ (v, topCallCount) | v <- bindersOf bind, not (v `elemVarSet` int) ]

-- Used for both local and top-level binds
-- First argument is the demand from the body
callArityBind :: CallArityEnv -> VarSet -> CoreBind -> (CallArityEnv, CoreBind)

-- Non-recursive let
callArityBind ae_body int (NonRec v rhs)
  = -- pprTrace "callArityBind:NonRec"
    --          (vcat [ppr v, ppr ae_body, ppr int, ppr ae_rhs, ppr safe_arity])
    (final_ae, NonRec v' rhs')
  where
    callcount = lookupWithDefaultVarEnv ae_body topCallCount v
    (ae_rhs, safe_arity, rhs') = callArityBound callcount int rhs
    final_ae = ae_rhs `lubEnv` (ae_body `delVarEnv` v)
    v' = v `setIdCallArity` safe_arity

-- Recursive let. See Note [Recursion and fixpointing]
callArityBind ae_body int b@(Rec binds)
  = (final_ae, Rec binds')
  where
    int_body = int `addInterestingBinds` b
    -- We are ignoring calls to boring binds, so we need to pretend them here!
    ae_body' = ae_body `lubEnv` (fakeBoringCalls int_body b)
    (ae_rhs, binds') = callArityFix ae_body' int_body [(i,Nothing,e) | (i,e) <- binds]
    final_ae = ae_rhs `delVarEnvList` interestingBinds b

-- Here we do the fix-pointing for possibly mutually recursive values.  The
-- idea is that we start with the calls coming from the body, and analyize
-- every called value with that arity, adding lub these calls into the
-- environment. We also remember for each variable the CallCount we analised it
-- with.  Then we check for every variable if in the new envrionment, it is
-- called with a different (i.e. lower) arity. If so, we reanalize that, and
-- lub the result back into the environment.  If we had a change for any of the
-- variables, we repeat this step, otherwise we are done.
callArityFix ::
    CallArityEnv -> VarSet ->
    [(Id, Maybe CallCount, CoreExpr)] ->
    (CallArityEnv, [(Id, CoreExpr)])
callArityFix ae int ann_binds
    | any_change
    = callArityFix ae' int ann_binds'
    | otherwise
    = (ae', map (\(i, a, e) -> (i `setArity` a, e)) ann_binds')
  where
    (changes, ae's, ann_binds') = unzip3 $ map rerun ann_binds
    any_change = or changes
    ae' = foldl lubEnv ae ae's

    rerun (i, mbArity, rhs)

        | mb_new_arity == mbArity
        -- No change. No need to re-analize, and no need to change the arity
        -- environment
        = (False, emptyVarEnv, (i,mbArity, rhs))

        | Just new_arity <- mb_new_arity
        -- We previously analized this with a different arity (or not at all)
        = let (ae_rhs, safe_arity, rhs') = callArityBound new_arity int rhs
          in (True, ae_rhs, (i `setIdCallArity` safe_arity, mb_new_arity, rhs'))

        | otherwise
        -- No call to this yet, so do nothing
        = (False, emptyVarEnv, (i, mbArity, rhs))
      where
        mb_new_arity = lookupVarEnv ae i

    setArity i Nothing = i -- Completely absent value
    setArity i (Just (_, a)) = i `setIdCallArity` a


-- This is a variant of callArityAnal that takes a CallCount (i.e. an arity with a
-- cardinality) and adjust the resulting environment accordingly. It is to be used
-- on bound expressions that can possibly be shared.
-- It also returns the safe arity used: For a thunk that is called multiple
-- times, this will be 0!
callArityBound :: CallCount -> VarSet -> CoreExpr -> (CallArityEnv, Arity, CoreExpr)
callArityBound (count, arity) int e = (final_ae, safe_arity, e')
 where
    is_thunk = not (exprIsHNF e)

    safe_arity | OnceAndOnly <- count = arity
               | is_thunk             = 0 -- A thunk! Do not eta-expand
               | otherwise            = arity

    (ae, e') = callArityAnal safe_arity int e

    final_ae | OnceAndOnly <- count = ae
             | otherwise            = forgetOnceCalls ae


anyGoodCalls :: CallArityEnv -> Bool
anyGoodCalls = foldVarEnv ((||) . isOnceCall) False

isOnceCall :: CallCount -> Bool
isOnceCall (OnceAndOnly, _) = True
isOnceCall (Many, _)        = False

forgetOnceCalls :: CallArityEnv -> CallArityEnv
forgetOnceCalls = mapVarEnv (first (const Many))

-- See Note [Case and App: Which side to take?]
useBetterOf :: CallArityEnv -> CallArityEnv -> CallArityEnv
useBetterOf ae1 ae2 | anyGoodCalls ae1 = ae1 `lubEnv` forgetOnceCalls ae2
useBetterOf ae1 ae2 | otherwise        = forgetOnceCalls ae1 `lubEnv` ae2

lubCallCount :: CallCount -> CallCount -> CallCount
lubCallCount (count1, arity1) (count2, arity2)
    = (count1 `lubCount` count2, arity1 `min` arity2)

lubCount :: Count -> Count -> Count
lubCount OnceAndOnly OnceAndOnly = OnceAndOnly
lubCount _           _           = Many

-- Used when combining results from alternative cases; take the minimum
lubEnv :: CallArityEnv -> CallArityEnv -> CallArityEnv
lubEnv = plusVarEnv_C lubCallCount

instance Outputable Count where
    ppr Many        = text "Many"
    ppr OnceAndOnly = text "OnceAndOnly"
