-- Test for trac ticket #1287; ghc 6.6 and 6.6.1 panicked on this

module ShouldCompile where

{-# SPECIALIZE delta' :: Num b => Int -> Int -> b -> b -> b #-}
delta' :: Eq a => a -> a -> b -> b -> b
delta' x y e f = if (x==y) then f else e

{-# SPECIALIZE delta :: Num b => Int -> Int -> b #-}
delta :: (Eq a, Num b) => a -> a -> b
delta x y = delta' x y 0 1

