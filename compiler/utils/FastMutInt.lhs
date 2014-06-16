\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -cpp #-}
{-# OPTIONS_GHC -O #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected
--
-- (c) The University of Glasgow 2002-2006
--
-- Unboxed mutable Ints

module FastMutInt(
        FastMutInt, newFastMutInt,
        readFastMutInt, writeFastMutInt,

        FastMutPtr, newFastMutPtr,
        readFastMutPtr, writeFastMutPtr
  ) where

#ifdef __GLASGOW_HASKELL__

#include "../includes/MachDeps.h"
#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

import GHC.Base
import GHC.Ptr

#else /* ! __GLASGOW_HASKELL__ */

import Data.IORef

#endif

newFastMutInt :: IO FastMutInt
readFastMutInt :: FastMutInt -> IO Int
writeFastMutInt :: FastMutInt -> Int -> IO ()

newFastMutPtr :: IO FastMutPtr
readFastMutPtr :: FastMutPtr -> IO (Ptr a)
writeFastMutPtr :: FastMutPtr -> Ptr a -> IO ()
\end{code}

\begin{code}
#ifdef __GLASGOW_HASKELL__
data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where !(I# size) = SIZEOF_HSINT

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
  where !(I# size) = SIZEOF_VOID_P

readFastMutPtr (FastMutPtr arr) = IO $ \s ->
  case readAddrArray# arr 0# s of { (# s, i #) ->
  (# s, Ptr i #) }

writeFastMutPtr (FastMutPtr arr) (Ptr i) = IO $ \s ->
  case writeAddrArray# arr 0# i s of { s ->
  (# s, () #) }
#else /* ! __GLASGOW_HASKELL__ */
--maybe someday we could use
--http://haskell.org/haskellwiki/Library/ArrayRef
--which has an implementation of IOURefs
--that is unboxed in GHC and just strict in all other compilers...
newtype FastMutInt = FastMutInt (IORef Int)

-- If any default value was chosen, it surely would be 0,
-- so we will use that since IORef requires a default value.
-- Or maybe it would be more interesting to package an error,
-- assuming nothing relies on being able to read a bogus Int?
-- That could interfere with its strictness for smart optimizers
-- (are they allowed to optimize a 'newtype' that way?) ...
-- Well, maybe that can be added (in DEBUG?) later.
newFastMutInt = fmap FastMutInt (newIORef 0)

readFastMutInt (FastMutInt ioRefInt) = readIORef ioRefInt

-- FastMutInt is strict in the value it contains.
writeFastMutInt (FastMutInt ioRefInt) i = i `seq` writeIORef ioRefInt i


newtype FastMutPtr = FastMutPtr (IORef (Ptr ()))

-- If any default value was chosen, it surely would be 0,
-- so we will use that since IORef requires a default value.
-- Or maybe it would be more interesting to package an error,
-- assuming nothing relies on being able to read a bogus Ptr?
-- That could interfere with its strictness for smart optimizers
-- (are they allowed to optimize a 'newtype' that way?) ...
-- Well, maybe that can be added (in DEBUG?) later.
newFastMutPtr = fmap FastMutPtr (newIORef (castPtr nullPtr))

readFastMutPtr (FastMutPtr ioRefPtr) = readIORef ioRefPtr

-- FastMutPtr is strict in the value it contains.
writeFastMutPtr (FastMutPtr ioRefPtr) i = i `seq` writeIORef ioRefPtr i
#endif
\end{code}

