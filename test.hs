{-# LANGUAGE MagicHash #-}

module Lib (g) where

import GHC.Exts
import Data.Int

{-# RULES
  "blah" forall x . case f x of (I# a, _) -> e = case sf x of a -> e
#-}

f :: Int -> (Int, Int)
f x = (sum [0..x], sum [1..2*x])
{-# NOINLINE f #-}

sf :: Int -> Int#
sf x = case sum [0..x] of I# a -> a
{-# NOINLINE sf #-}

g :: Int
g = case f 42 of (a, _) -> a + 1
