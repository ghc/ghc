{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
--
-- (c) The University of Glasgow 2002
--
-- Unboxed mutable Ints

module FastMutInt(
	FastMutInt, newFastMutInt,
	readFastMutInt, writeFastMutInt,
	incFastMutInt, incFastMutIntBy
  ) where

#include "MachDeps.h"

#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif


#if __GLASGOW_HASKELL__ < 503
import GlaExts
import PrelIOBase
#else
import GHC.Base
import GHC.IOBase
#endif

#if __GLASGOW_HASKELL__ < 411
newByteArray# = newCharArray#
#endif

#ifdef __GLASGOW_HASKELL__
data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt :: IO FastMutInt
newFastMutInt = IO $ \s0 ->
  case newByteArray# size s0 of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where I# size = SIZEOF_HSINT

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s0 ->
  case readIntArray# arr 0# s0 of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s0 ->
  case writeIntArray# arr 0# i s0 of { s ->
  (# s, () #) }

incFastMutInt :: FastMutInt -> IO Int	-- Returns original value
incFastMutInt (FastMutInt arr) = IO $ \s0 ->
  case readIntArray# arr 0# s0 of { (# s1, i #) ->
  case writeIntArray# arr 0# (i +# 1#) s1 of { s ->
  (# s, I# i #) } }

incFastMutIntBy :: FastMutInt -> Int -> IO Int	-- Returns original value
incFastMutIntBy (FastMutInt arr) (I# n) = IO $ \s0 ->
  case readIntArray# arr 0# s0 of { (# s1, i #) ->
  case writeIntArray# arr 0# (i +# n) s1 of { s ->
  (# s, I# i #) } }
#endif

