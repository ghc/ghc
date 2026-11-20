{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}

module LetEscape where

import Filter (Hidden)
import LetFilter (letFilter)
import Nested (partitionPair)
import Vector

copy :: Vec n a -> Vec n a
copy xs = xs

bad :: (a -> Bool) -> Vec i a -> Vec n a
bad p xs = let rest = letFilter p xs in copy rest

different :: (Hidden a, Hidden a) -> Bool
different packages = let (xs, ys) = packages in same xs ys
  where
    same :: Vec n a -> Vec n a -> Bool
    same _ _ = True

nested :: (a -> Bool) -> Vec i a -> Vec n a
nested p xs = let rows = partitionPair p xs in left rows
  where
    left :: (Vec l a, Vec r a) -> Vec l a
    left (ls, _) = ls
