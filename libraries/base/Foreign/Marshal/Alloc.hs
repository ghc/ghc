{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Alloc
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support: basic routines for memory allocation
--
-----------------------------------------------------------------------------

module Foreign.Marshal.Alloc (
  -- * Memory allocation
  -- ** Local allocation
  alloca,       -- :: Storable a =>        (Ptr a -> IO b) -> IO b
  allocaBytes,  -- ::               Int -> (Ptr a -> IO b) -> IO b

  -- ** Dynamic allocation
  malloc,       -- :: Storable a =>        IO (Ptr a)
  mallocBytes,  -- ::               Int -> IO (Ptr a)

  realloc,      -- :: Storable b => Ptr a        -> IO (Ptr b)
  reallocBytes, -- ::		    Ptr a -> Int -> IO (Ptr a)

  free,         -- :: Ptr a -> IO ()
  finalizerFree -- :: FinalizerPtr a
) where

import Data.Maybe
import Foreign.Ptr	 	( Ptr, nullPtr, FunPtr )
import Foreign.C.Types	 	( CSize )
import Foreign.Storable  	( Storable(sizeOf) )

#ifdef __GLASGOW_HASKELL__
import Foreign.ForeignPtr	( FinalizerPtr )
import GHC.IOBase
import GHC.Real
import GHC.Ptr
import GHC.Err
import GHC.Base
import GHC.Num
#elif defined(__NHC__)
import NHC.FFI			( FinalizerPtr, CInt(..) )
import IO			( bracket )
#else
import Control.Exception	( bracket )
#endif

#ifdef __HUGS__
import Hugs.Prelude		( IOException(IOError),
				  IOErrorType(ResourceExhausted) )
import Hugs.ForeignPtr		( FinalizerPtr )
#endif


-- exported functions
-- ------------------

-- |Allocate a block of memory that is sufficient to hold values of type
-- @a@.  The size of the area allocated is determined by the 'sizeOf'
-- method from the instance of 'Storable' for the appropriate type.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
malloc :: Storable a => IO (Ptr a)
malloc  = doMalloc undefined
  where
    doMalloc       :: Storable b => b -> IO (Ptr b)
    doMalloc dummy  = mallocBytes (sizeOf dummy)

-- |Allocate a block of memory of the given number of bytes.
-- The block of memory is sufficiently aligned for any of the basic
-- foreign types that fits into a memory block of the allocated size.
--
-- The memory may be deallocated using 'free' or 'finalizerFree' when
-- no longer required.
--
mallocBytes      :: Int -> IO (Ptr a)
mallocBytes size  = failWhenNULL "malloc" (_malloc (fromIntegral size))

-- |@'alloca' f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory sufficient to
-- hold values of type @a@.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca  = doAlloca undefined
  where
    doAlloca       :: Storable a' => a' -> (Ptr a' -> IO b') -> IO b'
    doAlloca dummy  = allocaBytes (sizeOf dummy)

-- |@'allocaBytes' n f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory of @n@ bytes.
-- The block of memory is sufficiently aligned for any of the basic
-- foreign types that fits into a memory block of the allocated size.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
--
#ifdef __GLASGOW_HASKELL__
allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes (I# size) action = IO $ \ s ->
     case newPinnedByteArray# size s      of { (# s, mbarr# #) ->
     case unsafeFreezeByteArray# mbarr# s of { (# s, barr#  #) ->
     let addr = Ptr (byteArrayContents# barr#) in
     case action addr    of { IO action ->
     case action s       of { (# s, r #) ->
     case touch# barr# s of { s ->
     (# s, r #)
  }}}}}
#else
allocaBytes      :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes size  = bracket (mallocBytes size) free
#endif

-- |Resize a memory area that was allocated with 'malloc' or 'mallocBytes'
-- to the size needed to store values of type @b@.  The returned pointer
-- may refer to an entirely different memory area, but will be suitably
-- aligned to hold values of type @b@.  The contents of the referenced
-- memory area will be the same as of the original pointer up to the
-- minimum of the original size and the size of values of type @b@.
--
-- If the argument to 'realloc' is 'nullPtr', 'realloc' behaves like
-- 'malloc'.
--
realloc :: Storable b => Ptr a -> IO (Ptr b)
realloc  = doRealloc undefined
  where
    doRealloc           :: Storable b' => b' -> Ptr a' -> IO (Ptr b')
    doRealloc dummy ptr  = let
			     size = fromIntegral (sizeOf dummy)
			   in
			   failWhenNULL "realloc" (_realloc ptr size)

-- |Resize a memory area that was allocated with 'malloc' or 'mallocBytes'
-- to the given size.  The returned pointer may refer to an entirely
-- different memory area, but will be sufficiently aligned for any of the
-- basic foreign types that fits into a memory block of the given size.
-- The contents of the referenced memory area will be the same as of
-- the original pointer up to the minimum of the original size and the
-- given size.
--
-- If the pointer argument to 'reallocBytes' is 'nullPtr', 'reallocBytes'
-- behaves like 'malloc'.  If the requested size is 0, 'reallocBytes'
-- behaves like 'free'.
--
reallocBytes          :: Ptr a -> Int -> IO (Ptr a)
reallocBytes ptr 0     = do free ptr; return nullPtr
reallocBytes ptr size  = 
  failWhenNULL "realloc" (_realloc ptr (fromIntegral size))

-- |Free a block of memory that was allocated with 'malloc',
-- 'mallocBytes', 'realloc', 'reallocBytes', 'Foreign.Marshal.Utils.new'
-- or any of the @new@/X/ functions in "Foreign.Marshal.Array" or
-- "Foreign.C.String".
--
free :: Ptr a -> IO ()
free  = _free


-- auxilliary routines
-- -------------------

-- asserts that the pointer returned from the action in the second argument is
-- non-null
--
failWhenNULL :: String -> IO (Ptr a) -> IO (Ptr a)
failWhenNULL name f = do
   addr <- f
   if addr == nullPtr
#if __GLASGOW_HASKELL__ || __HUGS__
      then ioError (IOError Nothing ResourceExhausted name 
					"out of memory" Nothing)
#else
      then ioError (userError (name++": out of memory"))
#endif
      else return addr

-- basic C routines needed for memory allocation
--
foreign import ccall unsafe "stdlib.h malloc"  _malloc  ::          CSize -> IO (Ptr a)
foreign import ccall unsafe "stdlib.h realloc" _realloc :: Ptr a -> CSize -> IO (Ptr b)
foreign import ccall unsafe "stdlib.h free"    _free    :: Ptr a -> IO ()

-- | A pointer to a foreign function equivalent to 'free', which may be
-- used as a finalizer (cf 'Foreign.ForeignPtr.ForeignPtr') for storage
-- allocated with 'malloc', 'mallocBytes', 'realloc' or 'reallocBytes'.
foreign import ccall unsafe "stdlib.h &free" finalizerFree :: FinalizerPtr a
