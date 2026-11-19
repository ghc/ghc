{-# LANGUAGE NoExperimentalDependentTypes #-}
module PairProjection (pick, pickRight) where

-- | Select the first field.
pick :: (a, b) -> a
pick (x, _) = x

-- | Select the second field.
pickRight :: (a, b) -> b
pickRight (_, y) = y
