{-# OPTIONS_GHC -#include "HsBase.h" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- Mutable boxed and unboxed arrays in the IO monad.
--
-----------------------------------------------------------------------------

module Data.Array.IO (
   -- * @IO@ arrays with boxed elements
   IOArray,		-- instance of: Eq, Typeable

   -- * @IO@ arrays with unboxed elements
   IOUArray,		-- instance of: Eq, Typeable
   castIOUArray,	-- :: IOUArray i a -> IO (IOUArray i b)

   -- * Overloaded mutable array interface
   module Data.Array.MArray,

   -- * Doing I\/O with @IOUArray@s
   hGetArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO Int
   hPutArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO ()
 ) where

import Prelude

import Data.Array.Base
import Data.Array.IO.Internals
import Data.Array		( Array )
import Data.Array.MArray
import Data.Int
import Data.Word

#ifdef __GLASGOW_HASKELL__
import Foreign
import Foreign.C

import GHC.Arr
import GHC.IOBase
import GHC.Handle
#else
import Data.Char
import System.IO
import System.IO.Error
#endif

#ifdef __GLASGOW_HASKELL__
-----------------------------------------------------------------------------
-- Freezing

freezeIOArray :: Ix ix => IOArray ix e -> IO (Array ix e)
freezeIOArray (IOArray marr) = stToIO (freezeSTArray marr)

freezeIOUArray :: Ix ix => IOUArray ix e -> IO (UArray ix e)
freezeIOUArray (IOUArray marr) = stToIO (freezeSTUArray marr)

{-# RULES
"freeze/IOArray"  freeze = freezeIOArray
"freeze/IOUArray" freeze = freezeIOUArray
    #-}

{-# INLINE unsafeFreezeIOArray #-}
unsafeFreezeIOArray :: Ix ix => IOArray ix e -> IO (Array ix e)
unsafeFreezeIOArray (IOArray marr) = stToIO (unsafeFreezeSTArray marr)

{-# INLINE unsafeFreezeIOUArray #-}
unsafeFreezeIOUArray :: Ix ix => IOUArray ix e -> IO (UArray ix e)
unsafeFreezeIOUArray (IOUArray marr) = stToIO (unsafeFreezeSTUArray marr)

{-# RULES
"unsafeFreeze/IOArray"  unsafeFreeze = unsafeFreezeIOArray
"unsafeFreeze/IOUArray" unsafeFreeze = unsafeFreezeIOUArray
    #-}

-----------------------------------------------------------------------------
-- Thawing

thawIOArray :: Ix ix => Array ix e -> IO (IOArray ix e)
thawIOArray arr = stToIO $ do
    marr <- thawSTArray arr
    return (IOArray marr)

thawIOUArray :: Ix ix => UArray ix e -> IO (IOUArray ix e)
thawIOUArray arr = stToIO $ do
    marr <- thawSTUArray arr
    return (IOUArray marr)

{-# RULES
"thaw/IOArray"  thaw = thawIOArray
"thaw/IOUArray" thaw = thawIOUArray
    #-}

{-# INLINE unsafeThawIOArray #-}
unsafeThawIOArray :: Ix ix => Array ix e -> IO (IOArray ix e)
unsafeThawIOArray arr = stToIO $ do
    marr <- unsafeThawSTArray arr
    return (IOArray marr)

{-# INLINE unsafeThawIOUArray #-}
unsafeThawIOUArray :: Ix ix => UArray ix e -> IO (IOUArray ix e)
unsafeThawIOUArray arr = stToIO $ do
    marr <- unsafeThawSTUArray arr
    return (IOUArray marr)

{-# RULES
"unsafeThaw/IOArray"  unsafeThaw = unsafeThawIOArray
"unsafeThaw/IOUArray" unsafeThaw = unsafeThawIOUArray
    #-}

-- ---------------------------------------------------------------------------
-- hGetArray

-- | Reads a number of 'Word8's from the specified 'Handle' directly
-- into an array.
hGetArray
 	:: Handle		-- ^ Handle to read from
	-> IOUArray Int Word8	-- ^ Array in which to place the values
	-> Int			-- ^ Number of 'Word8's to read
	-> IO Int
		-- ^ Returns: the number of 'Word8's actually 
		-- read, which might be smaller than the number requested
		-- if the end of file was reached.

hGetArray handle (IOUArray (STUArray l u ptr)) count
  | count == 0
  = return 0
  | count < 0 || count > rangeSize (l,u)
  = illegalBufferSize handle "hGetArray" count
  | otherwise = do
      wantReadableHandle "hGetArray" handle $ 
	\ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=is_stream } -> do
	buf@Buffer{ bufBuf=raw, bufWPtr=w, bufRPtr=r } <- readIORef ref
	if bufferEmpty buf
	   then readChunk fd is_stream ptr 0 count
	   else do 
		let avail = w - r
		copied <- if (count >= avail)
		       	    then do 
				memcpy_ba_baoff ptr raw (fromIntegral r) (fromIntegral avail)
				writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
				return avail
		     	    else do 
				memcpy_ba_baoff ptr raw (fromIntegral r) (fromIntegral count)
				writeIORef ref buf{ bufRPtr = r + count }
				return count

		let remaining = count - copied
		if remaining > 0 
		   then do rest <- readChunk fd is_stream ptr copied remaining
			   return (rest + copied)
		   else return count

readChunk :: FD -> Bool -> RawBuffer -> Int -> Int -> IO Int
readChunk fd is_stream ptr init_off bytes = loop init_off bytes 
 where
  loop :: Int -> Int -> IO Int
  loop off bytes | bytes <= 0 = return (off - init_off)
  loop off bytes = do
    r' <- readRawBuffer "readChunk" (fromIntegral fd) is_stream ptr
    				    (fromIntegral off) (fromIntegral bytes)
    let r = fromIntegral r'
    if r == 0
	then return (off - init_off)
	else loop (off + r) (bytes - r)

-- ---------------------------------------------------------------------------
-- hPutArray

-- | Writes an array of 'Word8' to the specified 'Handle'.
hPutArray
	:: Handle			-- ^ Handle to write to
	-> IOUArray Int Word8		-- ^ Array to write from
	-> Int				-- ^ Number of 'Word8's to write
	-> IO ()

hPutArray handle (IOUArray (STUArray l u raw)) count
  | count == 0
  = return ()
  | count < 0 || count > rangeSize (l,u)
  = illegalBufferSize handle "hPutArray" count
  | otherwise
   = do wantWritableHandle "hPutArray" handle $ 
          \ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=stream } -> do

          old_buf@Buffer{ bufBuf=old_raw, bufRPtr=r, bufWPtr=w, bufSize=size }
	    <- readIORef ref

          -- enough room in handle buffer?
          if (size - w > count)
		-- There's enough room in the buffer:
		-- just copy the data in and update bufWPtr.
	    then do memcpy_baoff_ba old_raw (fromIntegral w) raw (fromIntegral count)
		    writeIORef ref old_buf{ bufWPtr = w + count }
		    return ()

		-- else, we have to flush
	    else do flushed_buf <- flushWriteBuffer fd stream old_buf
		    writeIORef ref flushed_buf
		    let this_buf = 
			    Buffer{ bufBuf=raw, bufState=WriteBuffer, 
				    bufRPtr=0, bufWPtr=count, bufSize=count }
		    flushWriteBuffer fd stream this_buf
		    return ()

-- ---------------------------------------------------------------------------
-- Internal Utils

foreign import ccall unsafe "__hscore_memcpy_dst_off"
   memcpy_baoff_ba :: RawBuffer -> CInt -> RawBuffer -> CSize -> IO (Ptr ())
foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ba_baoff :: RawBuffer -> RawBuffer -> CInt -> CSize -> IO (Ptr ())

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz = 
	ioException (IOError (Just handle)
			    InvalidArgument  fn
			    ("illegal buffer size " ++ showsPrec 9 (sz::Int) [])
			    Nothing)

#else /* !__GLASGOW_HASKELL__ */
hGetArray :: Handle -> IOUArray Int Word8 -> Int -> IO Int
hGetArray handle arr count = do
	bds <- getBounds arr
	if count < 0 || count > rangeSize bds
	   then illegalBufferSize handle "hGetArray" count
	   else get 0
 where
  get i | i == count = return i
	| otherwise = do
		error_or_c <- try (hGetChar handle)
		case error_or_c of
		    Left ex
			| isEOFError ex -> return i
			| otherwise -> ioError ex
		    Right c -> do
			unsafeWrite arr i (fromIntegral (ord c))
			get (i+1)

hPutArray :: Handle -> IOUArray Int Word8 -> Int -> IO ()
hPutArray handle arr count = do
	bds <- getBounds arr
	if count < 0 || count > rangeSize bds
	   then illegalBufferSize handle "hPutArray" count
	   else put 0
 where
  put i | i == count = return ()
	| otherwise = do
		w <- unsafeRead arr i
		hPutChar handle (chr (fromIntegral w))
		put (i+1)

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize _ fn sz = ioError $
	userError (fn ++ ": illegal buffer size " ++ showsPrec 9 (sz::Int) [])
#endif /* !__GLASGOW_HASKELL__ */
