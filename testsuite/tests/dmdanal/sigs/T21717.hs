module T21717 (g) where

-- This is the original reproducer from #21717.
-- See T21717b for a reproducer that exhibited a crash.

f :: Bool -> (Int, Int)
f True  = (42,error "m")
f False = (error "m",42)

g :: (Bool -> (Int, Int)) -> Int
g h = fst (h True) + snd (h False)
{-# NOINLINE g #-}
