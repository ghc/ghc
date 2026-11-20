{-# LANGUAGE LinearTypes, RankNTypes #-}

module WitnessRejections where

import Filter (Hidden, filterVec)
import Vector

same :: Vec n Int -> Vec n Int -> Bool
same _ _ = True

alias :: Hidden Int -> Bool
alias p = let q = p in same p q

repeatedCall :: Vec n Int -> Bool
repeatedCall xs = same (filterVec odd xs) (filterVec odd xs)

class Pick a where
    pick :: Hidden a

sharedLength :: Vec n Bool -> Vec n Int -> Bool
sharedLength _ _ = True

polymorphic :: (forall a. Pick a => Hidden a) -> Bool
polymorphic p = sharedLength p p

escape :: Hidden Int -> Vec n Int
escape p = keep p p
  where
    keep :: Vec n Int -> Vec n Int -> Vec n Int
    keep xs _ = xs

duplicate :: (forall n m. Vec n Int %1 -> Vec m Int %1 -> Bool) -> Hidden Int %1 -> Bool
duplicate consume p = consume p p
