module T10320 where

{-# RULES "rule" forall x. f x = 42 #-}

f :: Int -> Int
f x = x
{-# NOINLINE [1] f #-}

n = f (0 :: Int)
