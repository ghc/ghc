{-# LANGUAGE ImpredicativeTypes #-}

module PartitionChoices (partitionViews) where

import ImplicitVector (partitionRows, rowLists)
import PackedVecLibrary (Vec)

-- | Read each partition at its own pair of hidden lengths.
partitionViews :: Vec Int n -> [([Int], [Int])]
partitionViews xs = map rowLists [id (partitionRows odd xs), id (partitionRows even xs)]
