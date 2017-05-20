module Main where

delayedId :: a -> a
delayedId x = x
{-# INLINE [0] delayedId #-}

alwaysTrue :: [Integer]-> Bool
alwaysTrue xs = xs == delayedId xs
{-# NOINLINE alwaysTrue #-}

{-# RULES
    "[Integer] Eq Refl" forall (xs :: [Integer]). xs == xs = True
#-}

main = putStrLn $ if alwaysTrue undefined then "ok" else "not ok"
