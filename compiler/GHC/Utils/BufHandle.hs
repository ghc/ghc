{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
--
-- Fast write-buffered Handles
--
-- (c) The University of Glasgow 2005-2006
--
-- This is a simple abstraction over Handles that offers very fast write
-- buffering, but without the thread safety that Handles provide.  It's used
-- to save time in GHC.Utils.Ppr.printDoc.
--
-----------------------------------------------------------------------------

module GHC.Utils.BufHandle (
        BufHandle(..),
        newBufHandle,
        bPutChar,
        bPutStr,
        bPutFS,
        bPutFZS,
        bPutPtrString,
        bPutReplicate,
        bFlush,
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Data.FastMutInt

import Control.Monad    ( when )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Char        ( ord )
import Foreign
import Foreign.C.String
import System.IO

-- -----------------------------------------------------------------------------

data BufHandle = BufHandle {-#UNPACK#-}!(Ptr Word8)
                           {-#UNPACK#-}!FastMutInt
                           Handle

newBufHandle :: Handle -> IO BufHandle
newBufHandle hdl = do
  ptr <- mallocBytes buf_size
  r <- newFastMutInt 0
  return (BufHandle ptr r hdl)

buf_size :: Int
buf_size = 8192

bPutChar :: BufHandle -> Char -> IO ()
bPutChar b@(BufHandle buf r hdl) !c = do
  i <- readFastMutInt r
  if (i >= buf_size)
        then do hPutBuf hdl buf buf_size
                writeFastMutInt r 0
                bPutChar b c
        else do pokeElemOff buf i (fromIntegral (ord c) :: Word8)
                writeFastMutInt r (i+1)

bPutStr :: BufHandle -> String -> IO ()
bPutStr (BufHandle buf r hdl) !str = do
  i <- readFastMutInt r
  loop str i
  where loop "" !i = do writeFastMutInt r i; return ()
        loop (c:cs) !i
           | i >= buf_size = do
                hPutBuf hdl buf buf_size
                loop (c:cs) 0
           | otherwise = do
                pokeElemOff buf i (fromIntegral (ord c))
                loop cs (i+1)

bPutFS :: BufHandle -> FastString -> IO ()
bPutFS b fs = bPutBS b $ bytesFS fs

bPutFZS :: BufHandle -> FastZString -> IO ()
bPutFZS b fs = bPutBS b $ fastZStringToByteString fs

bPutBS :: BufHandle -> ByteString -> IO ()
bPutBS b bs = BS.unsafeUseAsCStringLen bs $ bPutCStringLen b

bPutCStringLen :: BufHandle -> CStringLen -> IO ()
bPutCStringLen b@(BufHandle buf r hdl) cstr@(ptr, len) = do
  i <- readFastMutInt r
  if (i + len) >= buf_size
        then do hPutBuf hdl buf i
                writeFastMutInt r 0
                if (len >= buf_size)
                    then hPutBuf hdl ptr len
                    else bPutCStringLen b cstr
        else do
                copyBytes (buf `plusPtr` i) ptr len
                writeFastMutInt r (i + len)

bPutPtrString :: BufHandle -> PtrString -> IO ()
bPutPtrString b@(BufHandle buf r hdl) l@(PtrString a len) = l `seq` do
  i <- readFastMutInt r
  if (i+len) >= buf_size
        then do hPutBuf hdl buf i
                writeFastMutInt r 0
                if (len >= buf_size)
                    then hPutBuf hdl a len
                    else bPutPtrString b l
        else do
                copyBytes (buf `plusPtr` i) a len
                writeFastMutInt r (i+len)

-- | Replicate an 8-bit character
bPutReplicate :: BufHandle -> Int -> Char -> IO ()
bPutReplicate (BufHandle buf r hdl) len c = do
  i <- readFastMutInt r
  let oc = fromIntegral (ord c)
  if (i+len) < buf_size
    then do
      fillBytes (buf `plusPtr` i) oc len
      writeFastMutInt r (i+len)
    else do
      -- flush the current buffer
      when (i /= 0) $ hPutBuf hdl buf i
      if (len < buf_size)
        then do
          fillBytes buf oc len
          writeFastMutInt r len
        else do
          -- fill a full buffer
          fillBytes buf oc buf_size
          -- flush it as many times as necessary
          let go n | n >= buf_size = do
                                       hPutBuf hdl buf buf_size
                                       go (n-buf_size)
                   | otherwise     = writeFastMutInt r n
          go len

bFlush :: BufHandle -> IO ()
bFlush (BufHandle buf r hdl) = do
  i <- readFastMutInt r
  when (i > 0) $ hPutBuf hdl buf i
  free buf
  return ()
