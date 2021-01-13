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
        readFastMutInt, writeFastMutInt
  ) where

import GHC.Prelude

import Data.Bits
import GHC.Base

newFastMutInt :: IO FastMutInt
readFastMutInt :: FastMutInt -> IO Int
writeFastMutInt :: FastMutInt -> Int -> IO ()

data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where !(I# size) = finiteBitSize (0 :: Int) `unsafeShiftR` 3

readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }
