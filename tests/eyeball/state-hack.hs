-- The question here is whether f gets eta-expanded
-- (assuming the state hack).  It should, but
-- didn't in GHC 6.10

module Foo where

import GHC.Base

{-# NOINLINE z #-}
z :: State# a -> Bool
z s = True

{-# NOINLINE k #-}
k :: Int -> State# a -> Bool
k y s = False


f []     = z
f (x:xs) = k (length xs)
