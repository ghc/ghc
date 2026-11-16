{-# LANGUAGE BangPatterns #-}
module Micro.MemSize where

import Micro.Types

class MemSize a where
  memSize :: a -> Int -> Int

memSize0 :: Int -> Int
memSize2 :: (MemSize a, MemSize a1) =>
            a -> a1 -> Int -> Int

memSize0           = \ !sz -> sz
memSize2 a b       = \ !sz -> memSize a . memSize b $ 2 + sz
{-# INLINE memSize0 #-}
{-# INLINE memSize2 #-}

instance MemSize Tree where
  memSize Leaf  = memSize0
  memSize (Fork b a) = memSize2 a b

