{-# LANGUAGE UnboxedTuples #-}

-- Unboxed tuples; cf tcfail115, tcfail120

module ShouldFail where

type T a = Int -> (# Int, Int #)

-- Should be ok
h t = \x -> case t x of (# r, s #) -> r

