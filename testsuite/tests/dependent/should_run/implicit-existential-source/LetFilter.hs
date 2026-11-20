{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module LetFilter where

import Filter (Hidden)
import Vector

letFilter :: (a -> Bool) -> Vec n a -> Hidden a
letFilter _ Nil = Nil
letFilter p (Cons x xs)
  | p x = let rest = letFilter p xs in Cons x rest
  | otherwise = letFilter p xs
