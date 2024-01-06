module BottomFromInnerLambda where

expensive :: Int -> Int
expensive 0 = 0
expensive n = expensive n
{-# NOINLINE expensive #-}

-- We could be saying "<1P(1)><A>b"
-- but we are saying "<1P(1)>"
-- We should not be saying "<1P(1)>b"
f :: Int -> Int -> Int
f x = expensive x `seq` (\y -> error (show y))
