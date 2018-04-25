{-# LANGUAGE RankNTypes #-}

module ShouldCompile where

-- Check that we can have a forall to the right of a double-arrow

f :: forall a. (Num a) => forall b. (Ord b) => a -> b -> b -> a
f x y z = if y>z then x+1 else x

g :: (Num a) => (Ord b) => a -> b -> b -> a
g x y z = if y>z then x+1 else x
