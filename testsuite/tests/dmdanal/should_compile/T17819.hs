{-# OPTIONS_GHC -O -fforce-recomp #-}

module T17819 where

{-# NOINLINE foo #-}
foo :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
foo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
    x1 + x2+ x3+ x4+ x5+ x6+ x7+ x8+ x9+ x10+ x11

{-# NOINLINE bar #-}
bar :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
bar x1 x2 x3 x4 x5 x6 x7 x8 =
    x1 + x2+ x3+ x4+ x5+ x6+ x7+x8

