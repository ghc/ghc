module T19232 where

f :: Bool -> Int -> Int
f True x
  | x == 3 = 8
  | otherwise = x -- NB: the condition was flipped so that we can't substitute `x` for a constant here
f False _ = 3
{-# NOINLINE f #-}

-- See also test CaseBinderCPR

