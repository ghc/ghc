{-# LANGUAGE ExperimentalDependentTypes, GADTs, PolyKinds #-}
module DependentProjection (second) where

import DependentPair (DPair(..), first)

-- | Return the payload at this pair's witness.
second :: foreach (q :: DPair a p) -> p (first q)
second (Pair _ value) = value
