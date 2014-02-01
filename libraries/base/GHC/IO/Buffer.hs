{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Buffer
-- Copyright   :  (c) The University of Glasgow 2008
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Buffers used in the IO system
--
-----------------------------------------------------------------------------

module GHC.IO.Buffer (
    -- * Buffers of any element
    Buffer(..), BufferState(..), CharBuffer, CharBufElem,

    -- ** Creation
    newByteBuffer,
    newCharBuffer,
    newBuffer,
    emptyBuffer,

    -- ** Insertion/removal
    bufferRemove,
    bufferAdd,
    slideContents,
    bufferAdjustL,

    -- ** Inspecting
    isEmptyBuffer,
    isFullBuffer,
    isFullCharBuffer,
    isWriteBuffer,
    bufferElems,
    bufferAvailable,
    summaryBuffer,

    -- ** Operating on the raw buffer as a Ptr
    withBuffer,
    withRawBuffer,

    -- ** Assertions
    checkBuffer,

    -- * Raw buffers
    RawBuffer,
    readWord8Buf,
    writeWord8Buf,
    RawCharBuffer,
    peekCharBuf,
    readCharBuf,
    writeCharBuf,
    readCharBufPtr,
    writeCharBufPtr,
    charSize,
 ) where

import GHC.Base
-- import GHC.IO
import GHC.Num
import GHC.Ptr
import GHC.Word
import GHC.Show
import GHC.Real
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Storable

-- Char buffers use either UTF-16 or UTF-32, with the endianness matching
-- the endianness of the host.
--
-- Invariants:
--   * a Char buffer consists of *valid* UTF-16 or UTF-32
--   * only whole characters: no partial surrogate pairs

#define CHARBUF_UTF32

-- #define CHARBUF_UTF16
--
-- NB. it won't work to just change this to CHARBUF_UTF16.  Some of
-- the code to make this work is there, and it has been tested with
-- the Iconv codec, but there are some pieces that are known to be
-- broken.  In particular, the built-in codecs
-- e.g. GHC.IO.Encoding.UTF{8,16,32} need to use isFullCharBuffer or
-- similar in place of the ow >= os comparisons.

-- ---------------------------------------------------------------------------
-- Raw blocks of data

type RawBuffer e = ForeignPtr e

readWord8Buf :: RawBuffer Word8 -> Int -> IO Word8
readWord8Buf arr ix = withForeignPtr arr $ \p -> peekByteOff p ix

writeWord8Buf :: RawBuffer Word8 -> Int -> Word8 -> IO ()
writeWord8Buf arr ix w = withForeignPtr arr $ \p -> pokeByteOff p ix w

#ifdef CHARBUF_UTF16
type CharBufElem = Word16
#else
type CharBufElem = Char
#endif

type RawCharBuffer = RawBuffer CharBufElem

peekCharBuf :: RawCharBuffer -> Int -> IO Char
peekCharBuf arr ix = withForeignPtr arr $ \p -> do
                        (c,_) <- readCharBufPtr p ix
                        return c

{-# INLINE readCharBuf #-}
readCharBuf :: RawCharBuffer -> Int -> IO (Char, Int)
readCharBuf arr ix = withForeignPtr arr $ \p -> readCharBufPtr p ix

{-# INLINE writeCharBuf #-}
writeCharBuf :: RawCharBuffer -> Int -> Char -> IO Int
writeCharBuf arr ix c = withForeignPtr arr $ \p -> writeCharBufPtr p ix c

{-# INLINE readCharBufPtr #-}
readCharBufPtr :: Ptr CharBufElem -> Int -> IO (Char, Int)
#ifdef CHARBUF_UTF16
readCharBufPtr p ix = do
  c1 <- peekElemOff p ix
  if (c1 < 0xd800 || c1 > 0xdbff)
     then return (chr (fromIntegral c1), ix+1)
     else do c2 <- peekElemOff p (ix+1)
             return (unsafeChr ((fromIntegral c1 - 0xd800)*0x400 +
                                (fromIntegral c2 - 0xdc00) + 0x10000), ix+2)
#else
readCharBufPtr p ix = do c <- peekElemOff (castPtr p) ix; return (c, ix+1)
#endif

{-# INLINE writeCharBufPtr #-}
writeCharBufPtr :: Ptr CharBufElem -> Int -> Char -> IO Int
#ifdef CHARBUF_UTF16
writeCharBufPtr p ix ch
  | c < 0x10000 = do pokeElemOff p ix (fromIntegral c)
                     return (ix+1)
  | otherwise   = do let c' = c - 0x10000
                     pokeElemOff p ix (fromIntegral (c' `div` 0x400 + 0xd800))
                     pokeElemOff p (ix+1) (fromIntegral (c' `mod` 0x400 + 0xdc00))
                     return (ix+2)
  where
    c = ord ch
#else
writeCharBufPtr p ix ch = do pokeElemOff (castPtr p) ix ch; return (ix+1)
#endif

charSize :: Int
#ifdef CHARBUF_UTF16
charSize = 2
#else
charSize = 4
#endif

-- ---------------------------------------------------------------------------
-- Buffers

-- | A mutable array of bytes that can be passed to foreign functions.
--
-- The buffer is represented by a record, where the record contains
-- the raw buffer and the start/end points of the filled portion.  The
-- buffer contents itself is mutable, but the rest of the record is
-- immutable.  This is a slightly odd mix, but it turns out to be
-- quite practical: by making all the buffer metadata immutable, we
-- can have operations on buffer metadata outside of the IO monad.
--
-- The "live" elements of the buffer are those between the 'bufL' and
-- 'bufR' offsets.  In an empty buffer, 'bufL' is equal to 'bufR', but
-- they might not be zero: for exmaple, the buffer might correspond to
-- a memory-mapped file and in which case 'bufL' will point to the
-- next location to be written, which is not necessarily the beginning
-- of the file.
data Buffer e
  = Buffer {
	bufRaw   :: !(RawBuffer e),
        bufState :: BufferState,
	bufSize  :: !Int,          -- in elements, not bytes
	bufL     :: !Int,          -- offset of first item in the buffer
	bufR     :: !Int           -- offset of last item + 1
  }

#ifdef CHARBUF_UTF16
type CharBuffer = Buffer Word16
#else
type CharBuffer = Buffer Char
#endif

data BufferState = ReadBuffer | WriteBuffer deriving (Eq)

withBuffer :: Buffer e -> (Ptr e -> IO a) -> IO a
withBuffer Buffer{ bufRaw=raw } f = withForeignPtr (castForeignPtr raw) f

withRawBuffer :: RawBuffer e -> (Ptr e -> IO a) -> IO a
withRawBuffer raw f = withForeignPtr (castForeignPtr raw) f

isEmptyBuffer :: Buffer e -> Bool
isEmptyBuffer Buffer{ bufL=l, bufR=r } = l == r

isFullBuffer :: Buffer e -> Bool
isFullBuffer Buffer{ bufR=w, bufSize=s } = s == w

-- if a Char buffer does not have room for a surrogate pair, it is "full"
isFullCharBuffer :: Buffer e -> Bool
#ifdef CHARBUF_UTF16
isFullCharBuffer buf = bufferAvailable buf < 2
#else
isFullCharBuffer = isFullBuffer
#endif

isWriteBuffer :: Buffer e -> Bool
isWriteBuffer buf = case bufState buf of
                        WriteBuffer -> True
                        ReadBuffer  -> False

bufferElems :: Buffer e -> Int
bufferElems Buffer{ bufR=w, bufL=r } = w - r

bufferAvailable :: Buffer e -> Int
bufferAvailable Buffer{ bufR=w, bufSize=s } = s - w

bufferRemove :: Int -> Buffer e -> Buffer e
bufferRemove i buf@Buffer{ bufL=r } = bufferAdjustL (r+i) buf

bufferAdjustL :: Int -> Buffer e -> Buffer e
bufferAdjustL l buf@Buffer{ bufR=w }
  | l == w    = buf{ bufL=0, bufR=0 }
  | otherwise = buf{ bufL=l, bufR=w }

bufferAdd :: Int -> Buffer e -> Buffer e
bufferAdd i buf@Buffer{ bufR=w } = buf{ bufR=w+i }

emptyBuffer :: RawBuffer e -> Int -> BufferState -> Buffer e
emptyBuffer raw sz state = 
  Buffer{ bufRaw=raw, bufState=state, bufR=0, bufL=0, bufSize=sz }

newByteBuffer :: Int -> BufferState -> IO (Buffer Word8)
newByteBuffer c st = newBuffer c c st

newCharBuffer :: Int -> BufferState -> IO CharBuffer
newCharBuffer c st = newBuffer (c * charSize) c st

newBuffer :: Int -> Int -> BufferState -> IO (Buffer e)
newBuffer bytes sz state = do
  fp <- mallocForeignPtrBytes bytes
  return (emptyBuffer fp sz state)

-- | slides the contents of the buffer to the beginning
slideContents :: Buffer Word8 -> IO (Buffer Word8)
slideContents buf@Buffer{ bufL=l, bufR=r, bufRaw=raw } = do
  let elems = r - l
  withRawBuffer raw $ \p ->
      do _ <- memmove p (p `plusPtr` l) (fromIntegral elems)
         return ()
  return buf{ bufL=0, bufR=elems }

foreign import ccall unsafe "memmove"
   memmove :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)

summaryBuffer :: Buffer a -> String
summaryBuffer buf = "buf" ++ show (bufSize buf) ++ "(" ++ show (bufL buf) ++ "-" ++ show (bufR buf) ++ ")"

-- INVARIANTS on Buffers:
--   * r <= w
--   * if r == w, and the buffer is for reading, then r == 0 && w == 0
--   * a write buffer is never full.  If an operation
--     fills up the buffer, it will always flush it before 
--     returning.
--   * a read buffer may be full as a result of hLookAhead.  In normal
--     operation, a read buffer always has at least one character of space.

checkBuffer :: Buffer a -> IO ()
checkBuffer buf@Buffer{ bufState = state, bufL=r, bufR=w, bufSize=size } = do
     check buf (
      	size > 0
      	&& r <= w
      	&& w <= size
      	&& ( r /= w || state == WriteBuffer || (r == 0 && w == 0) )
        && ( state /= WriteBuffer || w < size ) -- write buffer is never full
      )

check :: Buffer a -> Bool -> IO ()
check _   True  = return ()
check buf False = error ("buffer invariant violation: " ++ summaryBuffer buf)

