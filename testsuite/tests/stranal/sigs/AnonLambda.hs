module AnonLambda where

g :: Int -> Bool
{-# NOINLINE g #-}
g = (==0)

-- This test ensures that the CPR property of the anonymous lambda
-- Does not escape to f (which has arity 1)

f = \x -> if g x then \y -> x + y + 1
                 else \y -> x + y + 2
