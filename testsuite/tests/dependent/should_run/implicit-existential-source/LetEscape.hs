{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}

module LetEscape where

import LetFilter (letFilter)
import Vector

copy :: Vec n a -> Vec n a
copy xs = xs

bad :: (a -> Bool) -> Vec i a -> Vec n a
bad p xs = let rest = letFilter p xs in copy rest
