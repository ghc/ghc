module T15155 (a, B(..), b) where

newtype A = A Int
newtype B = B A

{-# NOINLINE a #-}
a = A 42
b = B a
