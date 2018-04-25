{-# LANGUAGE UnboxedSums, UnboxedTuples, MagicHash #-}

module Lib where

import GHC.Prim

-- Can't unboxed tuples and sums to FFI, we should fail appropriately.

foreign import ccall "f1" f1 :: (# Int | Int #) -> IO Int
foreign import ccall "f2" f2 :: (# (# Int, Int #) | (# Float#, Float# #) #) -> IO Int
foreign import ccall "f3" f3 :: (# (# #) | Void# | (# Int# | String #) #) -> IO Int
