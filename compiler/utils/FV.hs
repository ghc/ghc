{-
(c) Bartosz Nitka, Facebook 2015

Utilities for efficiently and deterministically computing free variables.

-}

{-# LANGUAGE BangPatterns #-}

module FV (
        -- * Deterministic free vars computations
        FV, InterestingVarFun,

        -- * Running the computations
        runFV, runFVList, runFVSet, runFVDSet,

        -- ** Manipulating those computations
        oneVar,
        noVars,
        someVars,
        unionFV,
        unionsFV,
        delFV,
        delFVs,
        filterFV,
        mapUnionFV,
    ) where

import Var
import VarSet

-- | Predicate on possible free variables: returns @True@ iff the variable is
-- interesting
type InterestingVarFun = Var -> Bool

-- Note [Deterministic FV]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- When computing free variables, the order in which you get them affects
-- the results of floating and specialization. If you use UniqFM to collect
-- them and then turn that into a list, you get them in nondeterministic
-- order as described in Note [Deterministic UniqFM] in UniqDFM.

-- A naive algorithm for free variables relies on merging sets of variables.
-- Merging costs O(n+m) for UniqFM and for UniqDFM there's an additional log
-- factor. It's cheaper to incrementally add to a list and use a set to check
-- for duplicates.
type FV = InterestingVarFun
             -- Used for filtering sets as we build them
          -> VarSet
             -- Locally bound variables
          -> ([Var], VarSet)
             -- List to preserve ordering and set to check for membership,
             -- so that the list doesn't have duplicates
             -- For explanation of why using `VarSet` is not deterministic see
             -- Note [Deterministic UniqFM] in UniqDFM.
          -> ([Var], VarSet)

-- Note [FV naming conventions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- To get the performance and determinism that FV provides, FV computations
-- need to built up from smaller FV computations and then evaluated with
-- one of `runFVList`, `runFVDSet`, `runFV`. That means the functions
-- returning FV need to be exported.
--
-- The conventions are:
--
-- a) non-deterministic functions:
--   * x - a function that returns VarSet
--         e.g. `tyVarsOfType`
-- b) deterministic functions:
--   * xAcc - a worker that returns FV
--            e.g. `tyVarsOfTypeAcc`
--   * xList - a function that returns [Var]
--             e.g. `tyVarsOfTypeList`
--   * xDSet - a function that returns DVarSet
--             e.g. `tyVarsOfTypeDSet`
--
-- Where x, xList, xDSet are implemented in terms of the worker evaluated with
-- runFVSet, runFVList, runFVDSet respectively.

-- | Run a free variable computation, returning a list of distinct free
-- variables in deterministic order and a non-deterministic set containing
-- those variables.
runFV :: FV ->  ([Var], VarSet)
runFV fv = fv (const True) emptyVarSet ([], emptyVarSet)

-- | Run a free variable computation, returning a list of distinct free
-- variables in deterministic order.
runFVList :: FV -> [Var]
runFVList = fst . runFV

-- | Run a free variable computation, returning a deterministic set of free
-- variables. Note that this is just a wrapper around the version that
-- returns a deterministic list. If you need a list you should use
-- `runFVList`.
runFVDSet :: FV -> DVarSet
runFVDSet = mkDVarSet . fst . runFV

-- | Run a free variable computation, returning a non-deterministic set of
-- free variables. Don't use if the set will be later converted to a list
-- and the order of that list will impact the generated code.
runFVSet :: FV -> VarSet
runFVSet = snd . runFV

-- Note [FV eta expansion]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- Let's consider an eta-reduced implementation of freeVarsOf using FV:
--
-- freeVarsOf (App a b) = freeVarsOf a `unionFV` freeVarsOf b
--
-- If GHC doesn't eta-expand it, after inlining unionFV we end up with
--
-- freeVarsOf = \x ->
--   case x of
--     App a b -> \fv_cand in_scope acc ->
--       freeVarsOf a fv_cand in_scope $! freeVarsOf b fv_cand in_scope $! acc
--
-- which has to create a thunk, resulting in more allocations.
--
-- On the other hand if it is eta-expanded:
--
-- freeVarsOf (App a b) fv_cand in_scope acc =
--   (freeVarsOf a `unionFV` freeVarsOf b) fv_cand in_scope acc
--
-- after inlining unionFV we have:
--
-- freeVarsOf = \x fv_cand in_scope acc ->
--   case x of
--     App a b ->
--       freeVarsOf a fv_cand in_scope $! freeVarsOf b fv_cand in_scope $! acc
--
-- which saves allocations.
--
-- GHC when presented with knowledge about all the call sites, correctly
-- eta-expands in this case. Unfortunately due to the fact that freeVarsOf gets
-- exported to be composed with other functions, GHC doesn't have that
-- information and has to be more conservative here.
--
-- Hence functions that get exported and return FV need to be manually
-- eta-expanded. See also #11146.

-- | Add a variable - when free, to the returned free variables.
-- Ignores duplicates and respects the filtering function.
oneVar :: Id -> FV
oneVar var fv_cand in_scope acc@(have, haveSet)
  | var `elemVarSet` in_scope = acc
  | var `elemVarSet` haveSet = acc
  | fv_cand var = (var:have, extendVarSet haveSet var)
  | otherwise = acc
{-# INLINE oneVar #-}

-- | Return no free variables.
noVars :: FV
noVars _ _ acc = acc
{-# INLINE noVars #-}

-- | Union two free variable computations.
unionFV :: FV -> FV -> FV
unionFV fv1 fv2 fv_cand in_scope acc =
  fv1 fv_cand in_scope $! fv2 fv_cand in_scope $! acc
{-# INLINE unionFV #-}

-- | Mark the variable as not free by putting it in scope.
delFV :: Var -> FV -> FV
delFV var fv fv_cand !in_scope acc =
  fv fv_cand (extendVarSet in_scope var) acc
{-# INLINE delFV #-}

-- | Mark many free variables as not free.
delFVs :: VarSet -> FV -> FV
delFVs vars fv fv_cand !in_scope acc =
  fv fv_cand (in_scope `unionVarSet` vars) acc
{-# INLINE delFVs #-}

-- | Filter a free variable computation.
filterFV :: InterestingVarFun -> FV -> FV
filterFV fv_cand2 fv fv_cand1 in_scope acc =
  fv (\v -> fv_cand1 v && fv_cand2 v) in_scope acc
{-# INLINE filterFV #-}

-- | Map a free variable computation over a list and union the results.
mapUnionFV :: (a -> FV) -> [a] -> FV
mapUnionFV _f [] _fv_cand _in_scope acc = acc
mapUnionFV f (a:as) fv_cand in_scope acc =
  mapUnionFV f as fv_cand in_scope $! f a fv_cand in_scope $! acc
{-# INLINE mapUnionFV #-}

-- | Union many free variable computations.
unionsFV :: [FV] -> FV
unionsFV fvs fv_cand in_scope acc = mapUnionFV id fvs fv_cand in_scope acc
{-# INLINE unionsFV #-}

-- | Add multiple variables - when free, to the returned free variables.
-- Ignores duplicates and respects the filtering function.
someVars :: [Var] -> FV
someVars vars fv_cand in_scope acc =
  mapUnionFV oneVar vars fv_cand in_scope acc
{-# INLINE someVars #-}
