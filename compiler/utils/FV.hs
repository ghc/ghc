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
        unionFV,
        delFV,
        delFVs,
        filterFV,
    ) where

import Var
import VarSet

-- | Predicate on possible free variables: returns @True@ iff the variable is
-- interesting
type InterestingVarFun = Var -> Bool

type FV = InterestingVarFun
          -> VarSet
             -- Locally bound variables
          -> ([Var], VarSet)
             -- List to preserve ordering and set to check for membership,
             -- so that the list doesn't have duplicates
             -- For explanation of why using `VarSet` is not deterministic see
             -- Note [Deterministic UniqFM] in UniqDFM.
          -> ([Var], VarSet)

runFV :: FV ->  ([Var], VarSet)
runFV fv = fv (const True) emptyVarSet ([], emptyVarSet)

runFVList :: FV -> [Var]
runFVList = fst . runFV

runFVDSet :: FV -> DVarSet
runFVDSet = mkDVarSet . fst . runFV

runFVSet :: FV -> VarSet
runFVSet = snd . runFV

{-# INLINE oneVar #-}
oneVar :: Id -> FV
oneVar var fv_cand in_scope acc@(have, haveSet)
  = {- ASSERT( isId var ) probably not going to work -} fvs
  where
  fvs | var `elemVarSet` in_scope = acc
      | var `elemVarSet` haveSet = acc
      | fv_cand var = (var:have, extendVarSet haveSet var)
      | otherwise = acc

{-# INLINE noVars #-}
noVars :: FV
noVars _ _ acc = acc

{-# INLINE unionFV #-}
unionFV :: FV -> FV -> FV
unionFV fv1 fv2 fv_cand in_scope acc =
  fv1 fv_cand in_scope $! fv2 fv_cand in_scope $! acc

{-# INLINE delFV #-}
delFV :: Var -> FV -> FV
delFV var fv fv_cand !in_scope acc =
  fv fv_cand (extendVarSet in_scope var) acc

{-# INLINE delFVs #-}
delFVs :: VarSet -> FV -> FV
delFVs vars fv fv_cand !in_scope acc =
  fv fv_cand (in_scope `unionVarSet` vars) acc

{-# INLINE filterFV #-}
filterFV :: InterestingVarFun -> FV -> FV
filterFV fv_cand2 fv fv_cand1 in_scope acc =
  fv (\v -> fv_cand1 v && fv_cand2 v) in_scope acc
