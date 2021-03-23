module BottomFromInnerLambda where

expensive :: Int -> Int
expensive 0 = 0
expensive n = expensive n
{-# NOINLINE expensive #-}

-- We could be saying "<1!P(SA)><A>b"
-- but we are saying "<1!P(SA)>"
-- We should not be saying "<1!P(SA)>b"
f :: Int -> Int -> Int
f x = expensive x `seq` (\y -> error (show y))
