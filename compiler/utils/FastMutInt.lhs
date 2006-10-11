{-# OPTIONS -cpp #-}
--
-- (c) The University of Glasgow 2002-2006
--
-- Unboxed mutable Ints

\begin{code}
module FastMutInt(
	FastMutInt, newFastMutInt,
	readFastMutInt, writeFastMutInt
  ) where

#include "MachDeps.h"

#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif


import GHC.Base
import GHC.IOBase

#if __GLASGOW_HASKELL__ < 411
newByteArray# = newCharArray#
#endif
\end{code}

\begin{code}
#ifdef __GLASGOW_HASKELL__
data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt :: IO FastMutInt
newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where I# size = SIZEOF_HSINT

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }
\end{code}
#endif

