{-
(c) Bartosz Nitka, Facebook 2015

Utilities for efficiently and deterministically computing free variables.

-}

{-# LANGUAGE BangPatterns #-}

module NDFV (
        -- * Nondeterministic free vars computations
        NDFV, InterestingVarFun,

        -- * Running the computations
        ndfvVarListVarSet,
        -- ndfvVarList,
        ndfvVarSet,
        -- ndfvDVarSet,

        -- ** Manipulating those computations
        unitNDFV,
        emptyNDFV,
        mkNDFVs,
        unionNDFV,
        unionsNDFV,
        delNDFV,
        delNDFVs,
        filterNDFV,
        mapUnionNDFV,
    ) where

import GhcPrelude

import Var
import VarSet

-- | Predicate on possible free variables: returns @True@ iff the variable is
-- interesting
type InterestingVarFun = Var -> Bool

-- Note [Nondeterministic FV]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- When computing free variables, the order in which you get them affects
-- the results of floating and specialization. If you use UniqFM to collect
-- them and then turn that into a list, you get them in nondeterministic
-- order as described in Note [Deterministic UniqFM] in UniqDFM.

-- A naive algorithm for free variables relies on merging sets of variables.
-- Merging costs O(n+m) for UniqFM and for UniqDFM there's an additional log
-- factor. It's cheaper to incrementally add to a list and use a set to check
-- for duplicates.
type NDFV = InterestingVarFun
             -- Used for filtering sets as we build them
          -> VarSet
             -- Locally bound variables
          -> VarSet
          -> VarSet

-- Note [NDFV naming conventions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- To get the performance and determinism that NDFV provides, NDFV computations
-- need to built up from smaller NDFV computations and then evaluated with
-- one of `ndfvVarList`, `ndfvDVarSet`, `ndfvVarListVarSet`. That means the functions
-- returning NDFV need to be exported.
--
-- The conventions are:
--
-- a) non-deterministic functions:
--   * a function that returns VarSet
--       e.g. `tyVarsOfType`
-- b) deterministic functions:
--   * a worker that returns NDFV
--       e.g. `tyNDFVsOfType`
--   * a function that returns [Var]
--       e.g. `tyVarsOfTypeList`
--   * a function that returns DVarSet
--       e.g. `tyVarsOfTypeDSet`
--
-- Where tyVarsOfType, tyVarsOfTypeList, tyVarsOfTypeDSet are implemented
-- in terms of the worker evaluated with ndfvVarSet, ndfvVarList, ndfvDVarSet
-- respectively.

-- | Run a free variable computation, returning a list of distinct free
-- variables in deterministic order and a non-deterministic set containing
-- those variables.
ndfvVarListVarSet :: NDFV ->  VarSet
ndfvVarListVarSet fv = fv (const True) emptyVarSet emptyVarSet

-- | Run a free variable computation, returning a list of distinct free
-- variables in nondeterministic order.
---- ndfvVarList :: NDFV -> [Var]
---- ndfvVarList = fst . ndfvVarListVarSet

-- | Run a free variable computation, returning a deterministic set of free
-- variables. Note that this is just a wrapper around the version that
-- returns a deterministic list. If you need a list you should use
-- `ndfvVarList`.
---- ndfvDVarSet :: NDFV -> DVarSet
---- ndfvDVarSet = mkDVarSet . fst . ndfvVarListVarSet

-- | Run a free variable computation, returning a non-deterministic set of
-- free variables. Don't use if the set will be later converted to a list
-- and the order of that list will impact the generated code.
ndfvVarSet :: NDFV -> VarSet
ndfvVarSet = ndfvVarListVarSet

-- Note [NDFV eta expansion]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- Let's consider an eta-reduced implementation of freeVarsOf using NDFV:
--
-- freeVarsOf (App a b) = freeVarsOf a `unionNDFV` freeVarsOf b
--
-- If GHC doesn't eta-expand it, after inlining unionNDFV we end up with
--
-- freeVarsOf = \x ->
--   case x of
--     App a b -> \ndfv_cand in_scope acc ->
--       freeVarsOf a ndfv_cand in_scope $! freeVarsOf b ndfv_cand in_scope $! acc
--
-- which has to create a thunk, resulting in more allocations.
--
-- On the other hand if it is eta-expanded:
--
-- freeVarsOf (App a b) ndfv_cand in_scope acc =
--   (freeVarsOf a `unionNDFV` freeVarsOf b) ndfv_cand in_scope acc
--
-- after inlining unionNDFV we have:
--
-- freeVarsOf = \x ndfv_cand in_scope acc ->
--   case x of
--     App a b ->
--       freeVarsOf a ndfv_cand in_scope $! freeVarsOf b ndfv_cand in_scope $! acc
--
-- which saves allocations.
--
-- GHC when presented with knowledge about all the call sites, correctly
-- eta-expands in this case. Unfortunately due to the fact that freeVarsOf gets
-- exported to be composed with other functions, GHC doesn't have that
-- information and has to be more conservative here.
--
-- Hence functions that get exported and return NDFV need to be manually
-- eta-expanded. See also #11146.

-- | Add a variable - when free, to the returned free variables.
-- Ignores duplicates and respects the filtering function.
unitNDFV :: Id -> NDFV
unitNDFV var ndfv_cand in_scope acc
  | var `elemVarSet` in_scope = acc
  | var `elemVarSet` acc = acc
  | ndfv_cand var = extendVarSet acc var
  | otherwise = acc
{-# INLINE unitNDFV #-}

-- | Return no free variables.
emptyNDFV :: NDFV
emptyNDFV _ _ acc = acc
{-# INLINE emptyNDFV #-}

-- | Union two free variable computations.
unionNDFV :: NDFV -> NDFV -> NDFV
unionNDFV fv1 fv2 fv_cand in_scope acc =
  fv1 fv_cand in_scope $! fv2 fv_cand in_scope $! acc
{-# INLINE unionNDFV #-}

-- | Mark the variable as not free by putting it in scope.
delNDFV :: Var -> NDFV -> NDFV
delNDFV var fv fv_cand !in_scope acc =
  fv fv_cand (extendVarSet in_scope var) acc
{-# INLINE delNDFV #-}

-- | Mark many free variables as not free.
delNDFVs :: VarSet -> NDFV -> NDFV
delNDFVs vars fv fv_cand !in_scope acc =
  fv fv_cand (in_scope `unionVarSet` vars) acc
{-# INLINE delNDFVs #-}

-- | Filter a free variable computation.
filterNDFV :: InterestingVarFun -> NDFV -> NDFV
filterNDFV fv_cand2 fv fv_cand1 in_scope acc =
  fv (\v -> fv_cand1 v && fv_cand2 v) in_scope acc
{-# INLINE filterNDFV #-}

-- | Map a free variable computation over a list and union the results.
mapUnionNDFV :: (a -> NDFV) -> [a] -> NDFV
mapUnionNDFV _f [] _fv_cand _in_scope acc = acc
mapUnionNDFV f (a:as) fv_cand in_scope acc =
  f a fv_cand in_scope $! mapUnionNDFV f as fv_cand in_scope $! acc
  -- NB: preserve ordering of the input list by treating a before as
{-# INLINABLE mapUnionNDFV #-}

-- | Union many free variable computations.
unionsNDFV :: [NDFV] -> NDFV
unionsNDFV fvs fv_cand in_scope acc = mapUnionNDFV id fvs fv_cand in_scope acc
{-# INLINE unionsNDFV #-}

-- | Add multiple variables - when free, to the returned free variables.
-- Ignores duplicates and respects the filtering function.
mkNDFVs :: [Var] -> NDFV
mkNDFVs vars fv_cand in_scope acc =
  mapUnionNDFV unitNDFV vars fv_cand in_scope acc
{-# INLINE mkNDFVs #-}
