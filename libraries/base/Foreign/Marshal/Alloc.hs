{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Foreign.Marshal.Alloc
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Alloc.hs,v 1.3 2001/08/17 12:50:34 simonmar Exp $
--
-- Marshalling support: basic routines for memory allocation
--
-----------------------------------------------------------------------------

module Foreign.Marshal.Alloc (
  malloc,       -- :: Storable a =>        IO (Ptr a)
  mallocBytes,  -- ::               Int -> IO (Ptr a)

  alloca,       -- :: Storable a =>        (Ptr a -> IO b) -> IO b
  allocaBytes,  -- ::               Int -> (Ptr a -> IO b) -> IO b

  reallocBytes, -- :: Ptr a -> Int -> IO (Ptr a)

  free          -- :: Ptr a -> IO ()
) where

import Data.Maybe
import Foreign.Ptr	 	( Ptr, nullPtr )
import Foreign.C.TypesISO 	( CSize )
import Foreign.Storable  	( Storable(sizeOf) )

#ifdef __GLASGOW_HASKELL__
import GHC.Exception		( bracket )
import GHC.IOBase
import GHC.Real
import GHC.Ptr
import GHC.Err
import GHC.Base
import GHC.Prim
#endif


-- exported functions
-- ------------------

-- allocate space for storable type
--
malloc :: Storable a => IO (Ptr a)
malloc  = doMalloc undefined
  where
    doMalloc       :: Storable a => a -> IO (Ptr a)
    doMalloc dummy  = mallocBytes (sizeOf dummy)

-- allocate given number of bytes of storage
--
mallocBytes      :: Int -> IO (Ptr a)
mallocBytes size  = failWhenNULL "malloc" (_malloc (fromIntegral size))

-- temporarily allocate space for a storable type
--
-- * the pointer passed as an argument to the function must *not* escape from
--   this function; in other words, in `alloca f' the allocated storage must
--   not be used after `f' returns
--
alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca  = doAlloca undefined
  where
    doAlloca       :: Storable a => a -> (Ptr a -> IO b) -> IO b
    doAlloca dummy  = allocaBytes (sizeOf dummy)

-- temporarily allocate the given number of bytes of storage
--
-- * the pointer passed as an argument to the function must *not* escape from
--   this function; in other words, in `allocaBytes n f' the allocated storage
--   must not be used after `f' returns
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

-- adjust a malloc'ed storage area to the given size
--
reallocBytes          :: Ptr a -> Int -> IO (Ptr a)
reallocBytes ptr size  = 
  failWhenNULL "realloc" (_realloc ptr (fromIntegral size))

-- free malloc'ed storage
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
foreign import "malloc"  unsafe _malloc  ::          CSize -> IO (Ptr a)
foreign import "realloc" unsafe _realloc :: Ptr a -> CSize -> IO (Ptr a)
foreign import "free"	 unsafe _free    :: Ptr a -> IO ()
