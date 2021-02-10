module T19232 where

-- | `x` is not used strictly and hence will not be available unboxed, so
-- the `otherwise` RHS does not have the CPR property, even if it returns
-- a case binder.
f :: Bool -> Int -> Int
f True x
  | x == 3 = 8
  | otherwise = x -- NB: the condition was flipped so that we can't substitute `x` for a constant here
f False _ = 3
{-# NOINLINE f #-}

-- See also test CaseBinderCPR

