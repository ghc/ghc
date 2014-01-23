module BottomFromInnerLambda where

expensive :: Int -> Int
expensive 0 = 0
expensive n = expensive n
{-# NOINLINE expensive #-}

-- We could be saying "<S(S),1*(U(U))><L,A>b"
-- but we are saying "<S(S),1*(U(U))>"
-- We should not be saying "<S(S),1*(U(U))>b"
f :: Int -> Int -> Int
f x = expensive x `seq` (\y -> error (show y))
