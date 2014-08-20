%
% (c) The University of Glasgow 2006
% (c) The University of Glasgow, 1997-2006
%

Buffers for scanning string input stored in external arrays.

\begin{code}
{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -O -funbox-strict-fields #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

module StringBuffer
       (
        StringBuffer(..),
        -- non-abstract for vs\/HaskellService

         -- * Creation\/destruction
        hGetStringBuffer,
        hGetStringBufferBlock,
        appendStringBuffers,
        stringToStringBuffer,

        -- * Inspection
        nextChar,
        currentChar,
        prevChar,
        atEnd,

        -- * Moving and comparison
        stepOn,
        offsetBytes,
        byteDiff,

        -- * Conversion
        lexemeToString,
        lexemeToFastString,

         -- * Parsing integers
        parseUnsignedInteger,
       ) where

#include "HsVersions.h"

import Encoding
import FastString
import FastTypes
import FastFunctions

import System.IO                ( hGetBuf, hFileSize,IOMode(ReadMode), hClose
                                , Handle, hTell, openBinaryFile )
import System.IO.Unsafe         ( unsafePerformIO )

import GHC.Exts

import Foreign.Safe

-- -----------------------------------------------------------------------------
-- The StringBuffer type

-- |A StringBuffer is an internal pointer to a sized chunk of bytes.
-- The bytes are intended to be *immutable*.  There are pure
-- operations to read the contents of a StringBuffer.
--
-- A StringBuffer may have a finalizer, depending on how it was
-- obtained.
--
data StringBuffer
 = StringBuffer {
     buf :: {-# UNPACK #-} !(ForeignPtr Word8),
     len :: {-# UNPACK #-} !Int,        -- length
     cur :: {-# UNPACK #-} !Int         -- current pos
  }
  -- The buffer is assumed to be UTF-8 encoded, and furthermore
  -- we add three '\0' bytes to the end as sentinels so that the
  -- decoder doesn't have to check for overflow at every single byte
  -- of a multibyte sequence.

instance Show StringBuffer where
        showsPrec _ s = showString "<stringbuffer("
                      . shows (len s) . showString "," . shows (cur s)
                      . showString ")>"

-- -----------------------------------------------------------------------------
-- Creation / Destruction

hGetStringBuffer :: FilePath -> IO StringBuffer
hGetStringBuffer fname = do
   h <- openBinaryFile fname ReadMode
   size_i <- hFileSize h
   let size = fromIntegral size_i
   buf <- mallocForeignPtrArray (size+3)
   withForeignPtr buf $ \ptr -> do
     r <- if size == 0 then return 0 else hGetBuf h ptr size
     hClose h
     if (r /= size)
        then ioError (userError "short read of file")
        else newUTF8StringBuffer buf ptr size

hGetStringBufferBlock :: Handle -> Int -> IO StringBuffer
hGetStringBufferBlock handle wanted
    = do size_i <- hFileSize handle
         offset_i <- hTell handle
         let size = min wanted (fromIntegral $ size_i-offset_i)
         buf <- mallocForeignPtrArray (size+3)
         withForeignPtr buf $ \ptr ->
             do r <- if size == 0 then return 0 else hGetBuf handle ptr size
                if r /= size
                   then ioError (userError $ "short read of file: "++show(r,size,size_i,handle))
                   else newUTF8StringBuffer buf ptr size

newUTF8StringBuffer :: ForeignPtr Word8 -> Ptr Word8 -> Int -> IO StringBuffer
newUTF8StringBuffer buf ptr size = do
  pokeArray (ptr `plusPtr` size :: Ptr Word8) [0,0,0]
  -- sentinels for UTF-8 decoding
  let
      sb0 = StringBuffer buf size 0
      (first_char, sb1) = nextChar sb0
        -- skip the byte-order mark if there is one (see #1744)
        -- This is better than treating #FEFF as whitespace,
        -- because that would mess up layout.  We don't have a concept
        -- of zero-width whitespace in Haskell: all whitespace codepoints
        -- have a width of one column.
  return (if first_char == '\xfeff' then sb1 else sb0)

appendStringBuffers :: StringBuffer -> StringBuffer -> IO StringBuffer
appendStringBuffers sb1 sb2
    = do newBuf <- mallocForeignPtrArray (size+3)
         withForeignPtr newBuf $ \ptr ->
          withForeignPtr (buf sb1) $ \sb1Ptr ->
           withForeignPtr (buf sb2) $ \sb2Ptr ->
             do copyArray ptr (sb1Ptr `advancePtr` cur sb1) sb1_len
                copyArray (ptr `advancePtr` sb1_len) (sb2Ptr `advancePtr` cur sb2) sb2_len
                pokeArray (ptr `advancePtr` size) [0,0,0]
                return (StringBuffer newBuf size 0)
    where sb1_len = calcLen sb1
          sb2_len = calcLen sb2
          calcLen sb = len sb - cur sb
          size =  sb1_len + sb2_len

stringToStringBuffer :: String -> StringBuffer
stringToStringBuffer str =
 unsafePerformIO $ do
  let size = utf8EncodedLength str
  buf <- mallocForeignPtrArray (size+3)
  withForeignPtr buf $ \ptr -> do
    utf8EncodeString ptr str
    pokeArray (ptr `plusPtr` size :: Ptr Word8) [0,0,0]
    -- sentinels for UTF-8 decoding
  return (StringBuffer buf size 0)

-- -----------------------------------------------------------------------------
-- Grab a character

-- Getting our fingers dirty a little here, but this is performance-critical
{-# INLINE nextChar #-}
nextChar :: StringBuffer -> (Char,StringBuffer)
nextChar (StringBuffer buf len (I# cur#)) =
  inlinePerformIO $ do
    withForeignPtr buf $ \(Ptr a#) -> do
        case utf8DecodeChar# (a# `plusAddr#` cur#) of
          (# c#, b# #) ->
             let cur' = I# (b# `minusAddr#` a#) in
             return (C# c#, StringBuffer buf len cur')

currentChar :: StringBuffer -> Char
currentChar = fst . nextChar

prevChar :: StringBuffer -> Char -> Char
prevChar (StringBuffer _   _   0)   deflt = deflt
prevChar (StringBuffer buf _   cur) _     =
  inlinePerformIO $ do
    withForeignPtr buf $ \p -> do
      p' <- utf8PrevChar (p `plusPtr` cur)
      return (fst (utf8DecodeChar p'))

-- -----------------------------------------------------------------------------
-- Moving

stepOn :: StringBuffer -> StringBuffer
stepOn s = snd (nextChar s)

offsetBytes :: Int -> StringBuffer -> StringBuffer
offsetBytes i s = s { cur = cur s + i }

byteDiff :: StringBuffer -> StringBuffer -> Int
byteDiff s1 s2 = cur s2 - cur s1

atEnd :: StringBuffer -> Bool
atEnd (StringBuffer _ l c) = l == c

-- -----------------------------------------------------------------------------
-- Conversion

lexemeToString :: StringBuffer -> Int {-bytes-} -> String
lexemeToString _ 0 = ""
lexemeToString (StringBuffer buf _ cur) bytes =
  inlinePerformIO $
    withForeignPtr buf $ \ptr ->
      utf8DecodeString (ptr `plusPtr` cur) bytes

lexemeToFastString :: StringBuffer -> Int {-bytes-} -> FastString
lexemeToFastString _ 0 = nilFS
lexemeToFastString (StringBuffer buf _ cur) len =
   inlinePerformIO $
     withForeignPtr buf $ \ptr ->
       return $! mkFastStringBytes (ptr `plusPtr` cur) len

-- -----------------------------------------------------------------------------
-- Parsing integer strings in various bases
{-
byteOff :: StringBuffer -> Int -> Char
byteOff (StringBuffer buf _ cur) i =
  inlinePerformIO $ withForeignPtr buf $ \ptr -> do
--    return $! cBox (indexWord8OffFastPtrAsFastChar
--                         (pUnbox ptr) (iUnbox (cur+i)))
--or
--    w <- peek (ptr `plusPtr` (cur+i))
--    return (unsafeChr (fromIntegral (w::Word8)))
-}
-- | XXX assumes ASCII digits only (by using byteOff)
parseUnsignedInteger :: StringBuffer -> Int -> Integer -> (Char->Int) -> Integer
parseUnsignedInteger (StringBuffer buf _ cur) len radix char_to_int
  = inlinePerformIO $ withForeignPtr buf $ \ptr -> return $! let
    --LOL, in implementations where the indexing needs slow unsafePerformIO,
    --this is less (not more) efficient than using the IO monad explicitly
    --here.
    !ptr' = pUnbox ptr
    byteOff i = cBox (indexWord8OffFastPtrAsFastChar ptr' (iUnbox (cur + i)))
    go i x | i == len  = x
           | otherwise = case byteOff i of
               char -> go (i + 1) (x * radix + toInteger (char_to_int char))
  in go 0 0

\end{code}
