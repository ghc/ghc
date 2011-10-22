{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module T5573a where

import GHC.Exts

-- This is ok
foo1 x = (# x,x #)
bar y = let (# x,  _ #) = foo1 y in x

-- Nested unboxed tuple not ok
foo2 x = (# x, (# True, False #) #)

-- Unboxed tuple argument not ok
foo3 (# x,y #) = x


