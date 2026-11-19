{-# LANGUAGE NoExperimentalDependentTypes #-}

module FiniteTree (Tree (..), sample, leaves) where

import Numeric.Natural (Natural)

-- | A binary tree with a value in each leaf.
data Tree a = Leaf a | Fork (Tree a) (Tree a)

-- | Build three ordered leaves from the supplied number.
sample :: Natural -> Tree Natural
sample n = Fork (Leaf n) (Fork (Leaf (n + 1)) (Leaf (n + 2)))

-- | Read the leaves in source order.
leaves :: Tree a -> [a]
leaves (Leaf x) = [x]
leaves (Fork left right) = leaves left ++ leaves right
