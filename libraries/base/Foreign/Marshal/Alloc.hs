{-# OPTIONS -fno-implicit-prelude #-}
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
  -- * Allocation
  malloc,       -- :: Storable a =>        IO (Ptr a)
  mallocBytes,  -- ::               Int -> IO (Ptr a)

  alloca,       -- :: Storable a =>        (Ptr a -> IO b) -> IO b
  allocaBytes,  -- ::               Int -> (Ptr a -> IO b) -> IO b

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
import Hugs.ForeignPtr		( FinalizerPtr )
#endif


-- exported functions
-- ------------------

-- |Allocate space for storable type.  The size of the area allocated
-- is determined by the 'sizeOf' method from the instance of
-- 'Storable' for the appropriate type.
--
malloc :: Storable a => IO (Ptr a)
malloc  = doMalloc undefined
  where
    doMalloc       :: Storable a => a -> IO (Ptr a)
    doMalloc dummy  = mallocBytes (sizeOf dummy)

-- |Allocate given number of bytes of storage, equivalent to C\'s @malloc()@.
--
mallocBytes      :: Int -> IO (Ptr a)
mallocBytes size  = failWhenNULL "malloc" (_malloc (fromIntegral size))

-- |Temporarily allocate space for a storable type.
--
-- * the pointer passed as an argument to the function must /not/ escape from
--   this function; in other words, in @alloca f@ the allocated storage must
--   not be used after @f@ returns
--
alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca  = doAlloca undefined
  where
    doAlloca       :: Storable a => a -> (Ptr a -> IO b) -> IO b
    doAlloca dummy  = allocaBytes (sizeOf dummy)

-- |Temporarily allocate the given number of bytes of storage.
--
-- * the pointer passed as an argument to the function must /not/ escape from
--   this function; in other words, in @allocaBytes n f@ the allocated storage
--   must not be used after @f@ returns
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

-- |Adjust a malloc\'ed storage area to the given size of the required type
-- (corresponds to C\'s @realloc()@).
--
realloc :: Storable b => Ptr a -> IO (Ptr b)
realloc  = doRealloc undefined
  where
    doRealloc           :: Storable b => b -> Ptr a -> IO (Ptr b)
    doRealloc dummy ptr  = let
			     size = fromIntegral (sizeOf dummy)
			   in
			   failWhenNULL "realloc" (_realloc ptr size)

-- |Adjust a malloc\'ed storage area to the given size (equivalent to
-- C\'s @realloc()@).
--
reallocBytes          :: Ptr a -> Int -> IO (Ptr a)
reallocBytes ptr 0     = do free ptr; return nullPtr
reallocBytes ptr size  = 
  failWhenNULL "realloc" (_realloc ptr (fromIntegral size))

-- |Free malloc\'ed storage (equivalent to
-- C\'s @free()@)
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
#ifdef __GLASGOW_HASKELL__
      then ioException (IOError Nothing ResourceExhausted name 
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

-- | A pointer to a foreign function equivalent to 'free', which may be used
-- as a finalizer for storage allocated with 'malloc' or 'mallocBytes'.
foreign import ccall unsafe "stdlib.h &free" finalizerFree :: FinalizerPtr a
