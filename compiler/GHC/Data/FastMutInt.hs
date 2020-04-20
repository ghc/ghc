{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected
--
-- (c) The University of Glasgow 2002-2006
--
-- Unboxed mutable Ints

module GHC.Data.FastMutInt(
        FastMutInt, newFastMutInt,
        readFastMutInt, writeFastMutInt,

        FastMutPtr, newFastMutPtr,
        readFastMutPtr, writeFastMutPtr
  ) where

import GHC.Prelude

import Data.Bits
import GHC.Base
import GHC.Ptr

newFastMutInt :: IO FastMutInt
readFastMutInt :: FastMutInt -> IO Int
writeFastMutInt :: FastMutInt -> Int -> IO ()

newFastMutPtr :: IO FastMutPtr
readFastMutPtr :: FastMutPtr -> IO (Ptr a)
writeFastMutPtr :: FastMutPtr -> Ptr a -> IO ()

data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where !(I# size) = finiteBitSize (0 :: Int)

readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }

data FastMutPtr = FastMutPtr (MutableByteArray# RealWorld)

newFastMutPtr = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutPtr arr #) }
  -- GHC assumes 'sizeof (Int) == sizeof (Ptr a)'
  where !(I# size) = finiteBitSize (0 :: Int)

readFastMutPtr (FastMutPtr arr) = IO $ \s ->
  case readAddrArray# arr 0# s of { (# s, i #) ->
  (# s, Ptr i #) }

writeFastMutPtr (FastMutPtr arr) (Ptr i) = IO $ \s ->
  case writeAddrArray# arr 0# i s of { s ->
  (# s, () #) }
