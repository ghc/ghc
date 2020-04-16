module T10694 where

-- The point here is that 'm' should NOT have the CPR property
-- Checked by grepping in the -ddump-simpl


-- Some nonsense so that the simplifier can't see through
-- to the I# constructor
pm :: Int -> Int -> (Int, Int)
pm x y = (l !! 0, l !! 1)
  where l = [x+y, x-y]
{-# NOINLINE pm #-}

m :: Int -> Int -> Int
m x y = case pm x y of
  (pr, mr) -> mr
