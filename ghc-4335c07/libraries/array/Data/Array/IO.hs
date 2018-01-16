{-# LANGUAGE MagicHash, UnliftedFFITypes #-}

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
    IOArray,             -- instance of: Eq, Typeable

    -- * @IO@ arrays with unboxed elements
    IOUArray,            -- instance of: Eq, Typeable

    -- * Overloaded mutable array interface
    module Data.Array.MArray,

    -- * Doing I\/O with @IOUArray@s
    hGetArray,           -- :: Handle -> IOUArray Int Word8 -> Int -> IO Int
    hPutArray,           -- :: Handle -> IOUArray Int Word8 -> Int -> IO ()
  ) where

import Data.Array.Base
import Data.Array.IO.Internals
import Data.Array.MArray
import System.IO.Error

import Foreign
import Foreign.C

import GHC.Exts  (MutableByteArray#, RealWorld)
import GHC.IO.Handle
import GHC.IO.Exception

-- ---------------------------------------------------------------------------
-- hGetArray

-- | Reads a number of 'Word8's from the specified 'Handle' directly
-- into an array.
hGetArray
        :: Handle               -- ^ Handle to read from
        -> IOUArray Int Word8   -- ^ Array in which to place the values
        -> Int                  -- ^ Number of 'Word8's to read
        -> IO Int
                -- ^ Returns: the number of 'Word8's actually
                -- read, which might be smaller than the number requested
                -- if the end of file was reached.

hGetArray handle (IOUArray (STUArray _l _u n ptr)) count
  | count == 0              = return 0
  | count < 0 || count > n  = illegalBufferSize handle "hGetArray" count
  | otherwise = do
      -- we would like to read directly into the buffer, but we can't
      -- be sure that the MutableByteArray# is pinned, so we have to
      -- allocate a separate area of memory and copy.
      allocaBytes count $ \p -> do
        r <- hGetBuf handle p count
        _ <- memcpy_ba_ptr ptr p (fromIntegral r)
        return r

foreign import ccall unsafe "memcpy"
   memcpy_ba_ptr :: MutableByteArray# RealWorld -> Ptr a -> CSize -> IO (Ptr ())

-- ---------------------------------------------------------------------------
-- hPutArray

-- | Writes an array of 'Word8' to the specified 'Handle'.
hPutArray
        :: Handle                       -- ^ Handle to write to
        -> IOUArray Int Word8           -- ^ Array to write from
        -> Int                          -- ^ Number of 'Word8's to write
        -> IO ()

hPutArray handle (IOUArray (STUArray _l _u n raw)) count
  | count == 0              = return ()
  | count < 0 || count > n  = illegalBufferSize handle "hPutArray" count
  | otherwise = do
      -- as in hGetArray, we would like to use the array directly, but
      -- we can't be sure that the MutableByteArray# is pinned.
     allocaBytes count $ \p -> do
       _ <- memcpy_ptr_ba p raw (fromIntegral count)
       hPutBuf handle p count

foreign import ccall unsafe "memcpy"
   memcpy_ptr_ba :: Ptr a -> MutableByteArray# RealWorld -> CSize -> IO (Ptr ())

-- ---------------------------------------------------------------------------
-- Internal Utils

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
        ioException (ioeSetErrorString
                     (mkIOError InvalidArgument fn (Just handle) Nothing)
                     ("illegal buffer size " ++ showsPrec 9 (sz::Int) []))
