{-# OPTIONS -#include "HsCore.h" #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Array.IO
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: IO.hs,v 1.3 2002/01/02 14:40:10 simonmar Exp $
--
-- Mutable boxed/unboxed arrays in the IO monad.
--
-----------------------------------------------------------------------------

module Data.Array.IO (
   module Data.Array.MArray,
   IOArray,		-- instance of: Eq, Typeable
   IOUArray,		-- instance of: Eq, Typeable
   castIOUArray,	-- :: IOUArray i a -> IO (IOUArray i b)
   hGetArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO Int
   hPutArray,		-- :: Handle -> IOUArray Int Word8 -> Int -> IO ()
 ) where

import Prelude

import Data.Array		( Array )
import Data.Array.MArray
import Data.Int
import Data.Word
import Data.Dynamic

import Foreign.C
import Foreign.Ptr		( Ptr, FunPtr )
import Foreign.StablePtr	( StablePtr )

#ifdef __GLASGOW_HASKELL__
-- GHC only to the end of file

import Data.Array.Base
import GHC.Arr    	( STArray, freezeSTArray, unsafeFreezeSTArray,
                          thawSTArray, unsafeThawSTArray )

import GHC.ST		( ST(..) )

import GHC.IOBase
import GHC.Handle
import GHC.Conc

import GHC.Base

-----------------------------------------------------------------------------
-- Polymorphic non-strict mutable arrays (IO monad)

newtype IOArray i e = IOArray (STArray RealWorld i e) deriving Eq

iOArrayTc :: TyCon
iOArrayTc = mkTyCon "IOArray"

instance (Typeable a, Typeable b) => Typeable (IOArray a b) where
  typeOf a = mkAppTy iOArrayTc [typeOf ((undefined :: IOArray a b -> a) a),
				typeOf ((undefined :: IOArray a b -> b) a)]

instance HasBounds IOArray where
    {-# INLINE bounds #-}
    bounds (IOArray marr) = bounds marr

instance MArray IOArray e IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOArray marr) i e = stToIO (unsafeWrite marr i e)

-----------------------------------------------------------------------------
-- Flat unboxed mutable arrays (IO monad)

newtype IOUArray i e = IOUArray (STUArray RealWorld i e) deriving Eq

iOUArrayTc :: TyCon
iOUArrayTc = mkTyCon "IOUArray"

instance (Typeable a, Typeable b) => Typeable (IOUArray a b) where
  typeOf a = mkAppTy iOUArrayTc [typeOf ((undefined :: IOUArray a b -> a) a),
				 typeOf ((undefined :: IOUArray a b -> b) a)]

instance HasBounds IOUArray where
    {-# INLINE bounds #-}
    bounds (IOUArray marr) = bounds marr

instance MArray IOUArray Bool IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Char IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (Ptr a) IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (FunPtr a) IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Float IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Double IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray (StablePtr a) IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int8 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int16 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int32 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Int64 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word8 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word16 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word32 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

instance MArray IOUArray Word64 IO where
    {-# INLINE newArray #-}
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOUArray marr)
    {-# INLINE newArray_ #-}
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOUArray marr)
    {-# INLINE unsafeRead #-}
    unsafeRead (IOUArray marr) i = stToIO (unsafeRead marr i)
    {-# INLINE unsafeWrite #-}
    unsafeWrite (IOUArray marr) i e = stToIO (unsafeWrite marr i e)

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

castSTUArray :: STUArray s ix a -> ST s (STUArray s ix b)
castSTUArray (STUArray l u marr#) = return (STUArray l u marr#)

castIOUArray :: IOUArray ix a -> IO (IOUArray ix b)
castIOUArray (IOUArray marr) = stToIO $ do
    marr' <- castSTUArray marr
    return (IOUArray marr')

-- ---------------------------------------------------------------------------
-- hGetArray

hGetArray :: Handle -> IOUArray Int Word8 -> Int -> IO Int
hGetArray handle (IOUArray (STUArray l u ptr)) count
  | count <= 0 || count > rangeSize (l,u)
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
				memcpy_ba_baoff ptr raw r (fromIntegral avail)
				writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
				return avail
		     	    else do 
				memcpy_ba_baoff ptr raw r (fromIntegral count)
				writeIORef ref buf{ bufRPtr = r + count }
				return count

		let remaining = count - copied
		if remaining > 0 
		   then do rest <- readChunk fd is_stream ptr copied remaining
			   return (rest + count)
		   else return count

readChunk :: FD -> Bool -> RawBuffer -> Int -> Int -> IO Int
readChunk fd is_stream ptr init_off bytes = loop init_off bytes 
 where
  loop :: Int -> Int -> IO Int
  loop off bytes | bytes <= 0 = return (off - init_off)
  loop off bytes = do
    r' <- throwErrnoIfMinus1RetryMayBlock "readChunk"
	    (read_off (fromIntegral fd) is_stream ptr 
		(fromIntegral off) (fromIntegral bytes))
	    (threadWaitRead fd)
    let r = fromIntegral r'
    if r == 0
	then return (off - init_off)
	else loop (off + r) (bytes - r)

-- ---------------------------------------------------------------------------
-- hPutArray

hPutArray
	:: Handle			-- handle to write to
	-> IOUArray Int Word8		-- buffer
	-> Int				-- number of bytes of data to write
	-> IO ()

hPutArray handle (IOUArray (STUArray l u raw)) count
  | count <= 0 || count > rangeSize (l,u)
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
	    then do memcpy_baoff_ba old_raw w raw (fromIntegral count)
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

foreign import "__hscore_memcpy_dst_off" unsafe 
   memcpy_baoff_ba :: RawBuffer -> Int -> RawBuffer -> CSize -> IO (Ptr ())
foreign import "__hscore_memcpy_src_off" unsafe 
   memcpy_ba_baoff :: RawBuffer -> RawBuffer -> Int -> CSize -> IO (Ptr ())

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn (sz :: Int) = 
	ioException (IOError (Just handle)
			    InvalidArgument  fn
			    ("illegal buffer size " ++ showsPrec 9 sz [])
			    Nothing)

#endif /* __GLASGOW_HASKELL__ */
