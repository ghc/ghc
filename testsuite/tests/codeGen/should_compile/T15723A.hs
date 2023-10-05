module T15723A where

{-# INLINE foo #-}
foo :: Int -> Int
foo x = {-# SCC foo1 #-} bar x

{-# NOINLINE bar #-}
bar :: Int -> Int
bar x = x
