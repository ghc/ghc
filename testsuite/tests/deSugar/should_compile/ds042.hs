-- !!! Guard on a tuple pattern, broke 4.01 due to the
-- !!! special handling of unboxed tuples in desugarer.
module ShouldCompile where

f :: Int -> (Int,Int)
f x = 
  case f x of
    (a,b) | a > 0 -> f (x-1)
