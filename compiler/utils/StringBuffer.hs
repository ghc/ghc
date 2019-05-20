{-
(c) The University of Glasgow 2006
(c) The University of Glasgow, 1997-2006


Buffers for scanning string input stored in external arrays.
-}

{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

module StringBuffer
       (
        StringBuffer(..),
        -- non-abstract for vs\/HaskellService

         -- * Creation\/destruction
        hGetStringBuffer,
        hGetStringBufferBlock,
        hPutStringBuffer,
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
        atLine,

        -- * Conversion
        lexemeToString,
        lexemeToFastString,
        decodePrevNChars,

         -- * Parsing integers
        parseUnsignedInteger,
       ) where

#include "HsVersions.h"

import GhcPrelude

import Encoding
import FastString
import FastFunctions
import PlainPanic
import Util

import Data.Maybe
import Control.Exception
import System.IO
import System.IO.Unsafe         ( unsafePerformIO )
import GHC.IO.Encoding.UTF8     ( mkUTF8 )
import GHC.IO.Encoding.Failure  ( CodingFailureMode(IgnoreCodingFailure) )

import GHC.Exts

import Foreign

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
  -- we add three @\'\\0\'@ bytes to the end as sentinels so that the
  -- decoder doesn't have to check for overflow at every single byte
  -- of a multibyte sequence.

instance Show StringBuffer where
        showsPrec _ s = showString "<stringbuffer("
                      . shows (len s) . showString "," . shows (cur s)
                      . showString ")>"

-- -----------------------------------------------------------------------------
-- Creation / Destruction

-- | Read a file into a 'StringBuffer'.  The resulting buffer is automatically
-- managed by the garbage collector.
hGetStringBuffer :: FilePath -> IO StringBuffer
hGetStringBuffer fname = do
   h <- openBinaryFile fname ReadMode
   size_i <- hFileSize h
   offset_i <- skipBOM h size_i 0  -- offset is 0 initially
   let size = fromIntegral $ size_i - offset_i
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
         offset_i <- hTell handle >>= skipBOM handle size_i
         let size = min wanted (fromIntegral $ size_i-offset_i)
         buf <- mallocForeignPtrArray (size+3)
         withForeignPtr buf $ \ptr ->
             do r <- if size == 0 then return 0 else hGetBuf handle ptr size
                if r /= size
                   then ioError (userError $ "short read of file: "++show(r,size,size_i,handle))
                   else newUTF8StringBuffer buf ptr size

hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf len cur)
    = do withForeignPtr (plusForeignPtr buf cur) $ \ptr ->
             hPutBuf hdl ptr len

-- | Skip the byte-order mark if there is one (see #1744 and #6016),
-- and return the new position of the handle in bytes.
--
-- This is better than treating #FEFF as whitespace,
-- because that would mess up layout.  We don't have a concept
-- of zero-width whitespace in Haskell: all whitespace codepoints
-- have a width of one column.
skipBOM :: Handle -> Integer -> Integer -> IO Integer
skipBOM h size offset =
  -- Only skip BOM at the beginning of a file.
  if size > 0 && offset == 0
    then do
      -- Validate assumption that handle is in binary mode.
      ASSERTM( hGetEncoding h >>= return . isNothing )
      -- Temporarily select utf8 encoding with error ignoring,
      -- to make `hLookAhead` and `hGetChar` return full Unicode characters.
      bracket_ (hSetEncoding h safeEncoding) (hSetBinaryMode h True) $ do
        c <- hLookAhead h
        if c == '\xfeff'
          then hGetChar h >> hTell h
          else return offset
    else return offset
  where
    safeEncoding = mkUTF8 IgnoreCodingFailure

newUTF8StringBuffer :: ForeignPtr Word8 -> Ptr Word8 -> Int -> IO StringBuffer
newUTF8StringBuffer buf ptr size = do
  pokeArray (ptr `plusPtr` size :: Ptr Word8) [0,0,0]
  -- sentinels for UTF-8 decoding
  return $ StringBuffer buf size 0

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

-- | Encode a 'String' into a 'StringBuffer' as UTF-8.  The resulting buffer
-- is automatically managed by the garbage collector.
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

-- | Return the first UTF-8 character of a nonempty 'StringBuffer' and as well
-- the remaining portion (analogous to 'Data.List.uncons').  __Warning:__ The
-- behavior is undefined if the 'StringBuffer' is empty.  The result shares
-- the same buffer as the original.  Similar to 'utf8DecodeChar', if the
-- character cannot be decoded as UTF-8, @\'\\0\'@ is returned.
{-# INLINE nextChar #-}
nextChar :: StringBuffer -> (Char,StringBuffer)
nextChar (StringBuffer buf len (I# cur#)) =
  -- Getting our fingers dirty a little here, but this is performance-critical
  inlinePerformIO $ do
    withForeignPtr buf $ \(Ptr a#) -> do
        case utf8DecodeChar# (a# `plusAddr#` cur#) of
          (# c#, nBytes# #) ->
             let cur' = I# (cur# +# nBytes#) in
             return (C# c#, StringBuffer buf len cur')

-- | Return the first UTF-8 character of a nonempty 'StringBuffer' (analogous
-- to 'Data.List.head').  __Warning:__ The behavior is undefined if the
-- 'StringBuffer' is empty.  Similar to 'utf8DecodeChar', if the character
-- cannot be decoded as UTF-8, @\'\\0\'@ is returned.
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

-- | Return a 'StringBuffer' with the first UTF-8 character removed (analogous
-- to 'Data.List.tail').  __Warning:__ The behavior is undefined if the
-- 'StringBuffer' is empty.  The result shares the same buffer as the
-- original.
stepOn :: StringBuffer -> StringBuffer
stepOn s = snd (nextChar s)

-- | Return a 'StringBuffer' with the first @n@ bytes removed.  __Warning:__
-- If there aren't enough characters, the returned 'StringBuffer' will be
-- invalid and any use of it may lead to undefined behavior.  The result
-- shares the same buffer as the original.
offsetBytes :: Int                      -- ^ @n@, the number of bytes
            -> StringBuffer
            -> StringBuffer
offsetBytes i s = s { cur = cur s + i }

-- | Compute the difference in offset between two 'StringBuffer's that share
-- the same buffer.  __Warning:__ The behavior is undefined if the
-- 'StringBuffer's use separate buffers.
byteDiff :: StringBuffer -> StringBuffer -> Int
byteDiff s1 s2 = cur s2 - cur s1

-- | Check whether a 'StringBuffer' is empty (analogous to 'Data.List.null').
atEnd :: StringBuffer -> Bool
atEnd (StringBuffer _ l c) = l == c

-- | Computes a 'StringBuffer' which points to the first character of the
-- wanted line. Lines begin at 1.
atLine :: Int -> StringBuffer -> Maybe StringBuffer
atLine line sb@(StringBuffer buf len _) =
  inlinePerformIO $
    withForeignPtr buf $ \p -> do
      p' <- skipToLine line len p
      if p' == nullPtr
        then return Nothing
        else
          let
            delta = p' `minusPtr` p
          in return $ Just (sb { cur = delta
                               , len = len - delta
                               })

skipToLine :: Int -> Int -> Ptr Word8 -> IO (Ptr Word8)
skipToLine !line !len !op0 = go 1 op0
  where
    !opend = op0 `plusPtr` len

    go !i_line !op
      | op >= opend    = pure nullPtr
      | i_line == line = pure op
      | otherwise      = do
          w <- peek op :: IO Word8
          case w of
            10 -> go (i_line + 1) (plusPtr op 1)
            13 -> do
              -- this is safe because a 'StringBuffer' is
              -- guaranteed to have 3 bytes sentinel values.
              w' <- peek (plusPtr op 1) :: IO Word8
              case w' of
                10 -> go (i_line + 1) (plusPtr op 2)
                _  -> go (i_line + 1) (plusPtr op 1)
            _  -> go i_line (plusPtr op 1)

-- -----------------------------------------------------------------------------
-- Conversion

-- | Decode the first @n@ bytes of a 'StringBuffer' as UTF-8 into a 'String'.
-- Similar to 'utf8DecodeChar', if the character cannot be decoded as UTF-8,
-- they will be replaced with @\'\\0\'@.
lexemeToString :: StringBuffer
               -> Int                   -- ^ @n@, the number of bytes
               -> String
lexemeToString _ 0 = ""
lexemeToString (StringBuffer buf _ cur) bytes =
  utf8DecodeStringLazy buf cur bytes

lexemeToFastString :: StringBuffer
                   -> Int               -- ^ @n@, the number of bytes
                   -> FastString
lexemeToFastString _ 0 = nilFS
lexemeToFastString (StringBuffer buf _ cur) len =
   inlinePerformIO $
     withForeignPtr buf $ \ptr ->
       return $! mkFastStringBytes (ptr `plusPtr` cur) len

-- | Return the previous @n@ characters (or fewer if we are less than @n@
-- characters into the buffer.
decodePrevNChars :: Int -> StringBuffer -> String
decodePrevNChars n (StringBuffer buf _ cur) =
    inlinePerformIO $ withForeignPtr buf $ \p0 ->
      go p0 n "" (p0 `plusPtr` (cur - 1))
  where
    go :: Ptr Word8 -> Int -> String -> Ptr Word8 -> IO String
    go buf0 n acc p | n == 0 || buf0 >= p = return acc
    go buf0 n acc p = do
        p' <- utf8PrevChar p
        let (c,_) = utf8DecodeChar p'
        go buf0 (n - 1) (c:acc) p'

-- -----------------------------------------------------------------------------
-- Parsing integer strings in various bases
parseUnsignedInteger :: StringBuffer -> Int -> Integer -> (Char->Int) -> Integer
parseUnsignedInteger (StringBuffer buf _ cur) len radix char_to_int
  = inlinePerformIO $ withForeignPtr buf $ \ptr -> return $! let
    go i x | i == len  = x
           | otherwise = case fst (utf8DecodeChar (ptr `plusPtr` (cur + i))) of
               '_'  -> go (i + 1) x    -- skip "_" (#14473)
               char -> go (i + 1) (x * radix + toInteger (char_to_int char))
  in go 0 0
