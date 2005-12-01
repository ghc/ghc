{-# OPTIONS -fglasgow-exts #-}

-- This should fail, because there is no annotation on shw,
-- but it succeeds in 6.4.1

module ShouldFail where

data Term a where
   B :: Bool -> Term Bool
   I :: Int  -> Term Int

shw (I t) = ("I "++) . shows t
shw (B t) = ("B "++) . shows t

