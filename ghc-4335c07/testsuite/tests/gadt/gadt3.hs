{-# LANGUAGE GADTs #-}

module ShouldCompile where

data T where
  T1 :: Int -> T
  T2 :: Bool -> Bool -> T

data S a where
  S1 :: a -> S a
  S2 :: Int -> S Int

f (T1 i) = i>0
f (T2 a b) = a && b

g :: S a -> a
g (S1 x) = x
g (S2 i) = i
