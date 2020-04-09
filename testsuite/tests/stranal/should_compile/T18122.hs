{-# OPTIONS_GHC -fforce-recomp -O2 -fmax-worker-args=1 #-}
module Lib where

foo :: (Int, Int) -> Int -> Int
foo (x, y) z = x+z
{-# NOINLINE foo #-}
