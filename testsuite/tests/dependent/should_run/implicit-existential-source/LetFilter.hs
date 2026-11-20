{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module LetFilter where

import Filter (Hidden, toList)
import Vector

letFilter :: (a -> Bool) -> Vec n a -> Hidden a
letFilter _ Nil = Nil
letFilter p (Cons x xs)
  | p x = let rest = letFilter p xs in Cons x rest
  | otherwise = letFilter p xs

-- Keep both existential field types when checking an unsigned pattern.
unbundle :: (Hidden a, Hidden a) -> ([a], [a])
unbundle pair = let (xs, ys) = pair in (toList xs, toList ys)
