module T15155 (a, B(..), b) where

import Debug.Trace

newtype A = A Int
newtype B = B A

{-# NOINLINE a #-}
a = trace "evaluating a" A 42000

b = B a
