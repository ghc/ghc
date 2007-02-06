{-# OPTIONS_GHC -fglasgow-exts #-}

-- Trac #1118

module Compose where

data Z
data S n

data List n a where
    Nil :: List Z a
    (:-) :: a -> List n a -> List (S n) a

data Hold a = Hold (forall m . a m -> a (S m))

compose' :: List n (Hold a) -> a (S Z) -> a (S n)
compose' Nil x = x
compose' ((Hold f) :- fs) x = f (compose' fs x)

compose :: List n (forall m . a m -> a (S m)) -> a (S Z) -> a (S n)
compose Nil x = x
compose (f :- fs) x = f (compose fs x)

composeS :: [forall m . a m -> a m] -> a n -> a n
composeS [] x = x
composeS (f:fs) x = f (composeS fs x)
