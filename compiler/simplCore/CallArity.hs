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
import CoreArity

import Control.Arrow ( second )
import Data.Maybe ( isJust )


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

    type CallArityEnv = VarEnv (Maybe Arity)
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
    * Of all the variables that are mapped to a non-Nothing value by `callArityEnv`,
      at most one is being called, with at least that many arguments.
    * Nothing can be said about variables mapped to Noting.
  Furthermore, expr' is expr with the callArity field of the `IdInfo` updated.

The (pointwise) top of the domain is `Nothing`; the least upper bound coincides
with the mininum on `Maybe Int` with the usual `Ord` instance for `Maybe`.

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
precision of the analysis. Consider this example

    let n = ... :: Int
    in  let go = \x -> let d = case ... of
                                  False -> go (x+1)
                                  True  -> id
                       in \z -> d (x + z)
        in go n 0

Of course, `go` should be interesting. If we consider `n` as interesting as
well, then the body of the second let will return
    { go |-> Nothing , n |-> Just 0 }
or
    { go |-> 2, n |-> Nothing}.
Only the latter is useful, but it is hard to decide that locally.

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
But this will prevent us from detecting how the body of the let calls `d`, and
we will not find out anything.

It might be possible to be smarter here; this needs find-tuning as we find more
examples.


Note [Recursion and fixpointing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a recursive let, we begin by analysing the body, using the same incoming
arity as for the whole expression.
 * If we do not get useful information about how we are calling the rhs, we
   analyse the rhs using an incoming demand of 0 (which is always ok), and use
   `forgetGoodCalls` to ignore any information coming from the rhs.
 * If we do get useful information from the body, we use that as the incoming
   demand on the rhs. Then we check if the rhs calls itself with the same arity.
   - If so, we are done.
   - If not, we re-analise the rhs with the reduced arity. We do that until
     we are down to the exprArity, which then is certainly correct.
   We can `lubEnv` the results from the body and the rhs: The body calls *either*
   the rhs *or* one of the other mentioned variables. Similarly, the rhs calls
   *either* itself again *or* one of the other mentioned variables. This precision
   is required!

We do not analyse mutually recursive functions. This can be done once we see it
in the wild.

Note [Case and App: Which side to take?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Combining the case branches is easy, just `lubEnv` them – at most one branch is
taken.

But how to combine that with the information coming from the scrunitee? Very
similarly, how to combine the information from the callee and argument of an
`App`?

It would not be correct to just `lubEnv` then: `f n` obviously calls *both* `f`
and `n`. We need to forget about the calls from one side using `forgetGoodCalls`. But
which one?

Both are correct, and sometimes one and sometimes the other is more precise
(also see example in [Which variables are interesting]).

So currently, we first check the scrunitee (resp. the callee) if the returned
value has any usesful information, and if so, we use that; otherwise we use the
information from the alternatives (resp. the argument).

It might be smarter to look for “more important” variables first, i.e. the
innermost recursive variable.

-}

callArityAnalProgram :: DynFlags -> CoreProgram -> CoreProgram
callArityAnalProgram _dflags = map callArityBind

callArityBind :: CoreBind -> CoreBind
callArityBind (NonRec id rhs) = NonRec id (callArityRHS rhs) 
callArityBind (Rec binds) = Rec $ map (\(id,rhs) -> (id, callArityRHS rhs)) binds

callArityRHS :: CoreExpr -> CoreExpr
callArityRHS = snd . callArityAnal 0 emptyVarSet


type CallArityEnv = VarEnv (Maybe Arity)

callArityAnal ::
    Arity ->  -- The arity this expression is called with
    VarSet -> -- The set of interesting variables
    CoreExpr ->  -- The expression to analyse
    (CallArityEnv, CoreExpr)
        -- How this expression uses its interesting variables:
        --   Just n  => a tail call with that arity
        --   Nothing => other uses
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
    = (unitVarEnv v (Just arity), e)
    | otherwise
    = (emptyVarEnv, e)

-- We have a lambda that we are not sure to call. Tail calls therein
-- are no longer tail calls
callArityAnal 0     int (Lam v e)
    = (ae', Lam v e')
  where
    (ae, e') = callArityAnal 0 int e
    ae' = forgetGoodCalls ae
-- We have a lambda that we are calling. decrease arity.
callArityAnal arity int (Lam v e)
    = (ae, Lam v e')
  where
    (ae, e') = callArityAnal (arity - 1) int e

-- Boring non-recursive let, i.e. no eta expansion possible. do not be smart about this
-- See Note [Which variables are interesting]
callArityAnal arity int (Let (NonRec v rhs) e)
    | exprArity rhs >= length (typeArity (idType v))
    = (ae_final, Let (NonRec v rhs') e')
  where
    (ae_rhs, rhs') = callArityAnal 0 int rhs
    (ae_body, e')  = callArityAnal arity int e
    ae_body' = ae_body `delVarEnv` v
    ae_final = forgetGoodCalls ae_rhs `lubEnv` ae_body'

-- Non-recursive let. Find out how the body calls the rhs, analise that,
-- and combine the results, convervatively using both
callArityAnal arity int (Let (NonRec v rhs) e)

    -- We are tail-calling into the rhs. So a tail-call in the RHS is a
    -- tail-call for everything
    | Just n <- rhs_arity
    = let (ae_rhs, rhs') = callArityAnal n int rhs
          final_ae       = ae_rhs `lubEnv` ae_body'
          v'             = v `setIdCallArity` n
      in -- pprTrace "callArityAnal:LetNonRecTailCall"
         --          (vcat [ppr v, ppr arity, ppr n, ppr final_ae ])
         (final_ae, Let (NonRec v' rhs') e')

    -- We are calling the rhs in any other way (or not at all), so kill the
    -- tail-call information from there
    | otherwise
    = let (ae_rhs, rhs') = callArityAnal 0 int rhs
          final_ae = forgetGoodCalls ae_rhs `lubEnv` ae_body'
          v'             = v `setIdCallArity` 0
      in -- pprTrace "callArityAnal:LetNonRecNonTailCall"
         --          (vcat [ppr v, ppr arity, ppr final_ae ])
         (final_ae, Let (NonRec v' rhs') e')
  where
    int_body = int `extendVarSet` v
    (ae_body, e') = callArityAnal arity int_body e
    ae_body' = ae_body `delVarEnv` v
    rhs_arity = lookupWithDefaultVarEnv ae_body Nothing v

-- Boring recursive let, i.e. no eta expansion possible. do not be smart about this
callArityAnal arity int (Let (Rec [(v,rhs)]) e)
    | exprArity rhs >= length (typeArity (idType v))
    = (ae_final, Let (Rec [(v,rhs')]) e')
  where
    (ae_rhs, rhs') = callArityAnal 0 int rhs
    (ae_body, e')  = callArityAnal arity int e
    ae_final = (forgetGoodCalls ae_rhs `lubEnv` ae_body) `delVarEnv` v

-- Recursive let.
-- See Note [Recursion and fixpointing]
callArityAnal arity int (Let (Rec [(v,rhs)]) e)
    -- We are tail-calling into the rhs. So a tail-call in the RHS is a
    -- tail-call for everything
    | Just n <- rhs_arity
    = let (ae_rhs, rhs_arity', rhs') = callArityFix n int_body v rhs
          final_ae = (ae_rhs `lubEnv` ae_body) `delVarEnv` v
          v'             = v `setIdCallArity` rhs_arity'
      in -- pprTrace "callArityAnal:LetRecTailCall"
         --          (vcat [ppr v, ppr arity, ppr n, ppr rhs_arity', ppr final_ae ])
         (final_ae, Let (Rec [(v',rhs')]) e')
    -- We are calling the body in any other way (or not at all), so kill the
    -- tail-call information from there. No need to iterate there.
    | otherwise
    = let (ae_rhs, rhs') = callArityAnal 0 int_body rhs
          final_ae = (forgetGoodCalls ae_rhs `lubEnv` ae_body) `delVarEnv` v
          v'             = v `setIdCallArity` 0
      in -- pprTrace "callArityAnal:LetRecNonTailCall"
         --          (vcat [ppr v, ppr arity, ppr final_ae ])
         (final_ae, Let (Rec [(v',rhs')]) e')
  where
    int_body = int `extendVarSet` v
    (ae_body, e') = callArityAnal arity int_body e
    rhs_arity = lookupWithDefaultVarEnv ae_body Nothing v

-- Mutual recursion. Do nothing serious here, for now
callArityAnal arity int (Let (Rec binds) e)
    = (final_ae, Let (Rec binds') e')
  where
    (aes, binds') = unzip $ map go binds
    go (i,e) = let (ae,e') = callArityAnal 0 int e
               in (forgetGoodCalls ae, (i,e'))
    (ae, e') = callArityAnal arity int e
    final_ae = foldl lubEnv ae aes `delVarEnvList` map fst binds

-- Application. Increase arity for the called expresion, nothing to know about
-- the second
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

callArityFix :: Arity -> VarSet -> Id -> CoreExpr -> (CallArityEnv, Arity, CoreExpr)
callArityFix arity int v e

    | arity <= min_arity
    -- The incoming arity is already lower than the exprArity, so we can
    -- ignore the arity coming from the RHS
    = (ae `delVarEnv` v, 0, e')

    | otherwise
    = case new_arity of
        -- Not nicely recursive, rerun with arity 0
        -- (which will do at most one iteration, see above)
        -- (Or not recursive at all, but that was hopefully handled by the simplifier before)
        Nothing -> callArityFix 0 int v e

        Just n -> if n < arity
            -- RHS puts a lower arity on itself, but still a nice call, so try with that
            then callArityFix n int v e

            -- RHS calls itself with at least as many arguments as the body of
            -- the let: Great!
            else (ae `delVarEnv` v, n, e')
  where
    (ae, e') = callArityAnal arity int e
    new_arity = lookupWithDefaultVarEnv ae Nothing v
    min_arity = exprArity e


anyGoodCalls :: VarEnv (Maybe Arity) -> Bool
anyGoodCalls = foldVarEnv ((||) . isJust) False

forgetGoodCalls :: VarEnv (Maybe Arity) -> VarEnv (Maybe Arity)
forgetGoodCalls = mapVarEnv (const Nothing)

-- See Note [Case and App: Which side to take?]
useBetterOf :: CallArityEnv -> CallArityEnv -> CallArityEnv
useBetterOf ae1 ae2 | anyGoodCalls ae1 = ae1 `lubEnv` forgetGoodCalls ae2
useBetterOf ae1 ae2 | otherwise        = forgetGoodCalls ae1 `lubEnv` ae2

-- Used when combining results from alternative cases; take the minimum
lubEnv :: CallArityEnv -> CallArityEnv -> CallArityEnv
lubEnv = plusVarEnv_C min

