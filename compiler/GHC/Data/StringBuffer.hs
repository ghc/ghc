{-
(c) The University of Glasgow 2006
(c) The University of Glasgow, 1997-2006


Buffers for scanning string input stored in external arrays.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -O2 #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

module GHC.Data.StringBuffer
       (
        StringBuffer,

         -- * Creation\/destruction
        hGetStringBuffer,
        hGetStringBufferBlock,
        hPutStringBuffer,
        appendStringBuffers,
        stringToStringBuffer,
        byteStringToStringBuffer,
        withStringBufferContents,

        -- * Inspection
        lengthStringBuffer,
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

import GHC.Prelude
import GHC.Stack

import GHC.Utils.Encoding
import GHC.Data.FastString
import GHC.Data.ByteArray
import GHC.Utils.IO.Unsafe
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import Foreign.C.String

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Maybe
import Control.Exception
import System.IO
import System.IO.Unsafe         ( unsafePerformIO )
import GHC.IO.Encoding.UTF8     ( mkUTF8 )
import GHC.IO.Encoding.Failure  ( CodingFailureMode(IgnoreCodingFailure) )

import GHC.Word
import GHC.Exts

import Foreign

-- -----------------------------------------------------------------------------
-- The StringBuffer type

-- | A 'StringBuffer' is an internal pointer to a sized chunk of bytes.
-- The bytes are intended to be *immutable*.  There are pure
-- operations to read the contents of a 'StringBuffer'.
--
data StringBuffer
 = StringBuffer {
     buf :: {-# UNPACK #-} !ByteArray,
     cur :: {-# UNPACK #-} !Int
     -- ^ Current position in bytes.
  }
  -- The buffer is assumed to be UTF-8 encoded, and furthermore
  -- we add three @\'\\0\'@ bytes to the end as sentinels so that the
  -- decoder doesn't have to check for overflow at every single byte
  -- of a multibyte sequence.

instance Show StringBuffer where
        showsPrec _ s = showString "<stringbuffer("
                      . shows (cur s)
                      . showString ")>"

isValid :: StringBuffer -> Bool
isValid sb = sizeofByteArray (buf sb) >= cur sb

checkValid :: HasCallStack => StringBuffer -> StringBuffer
checkValid sb
  | not (isValid sb) = error "isValid"
  | otherwise = sb

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
  buf <- newPinnedMutableByteArray (size+3)
  r <- if size == 0
          then return 0
          else hGetBuf h (unsafeMutableByteArrayContents buf) size
  hClose h
  if r /= size
    then ioError (userError "short read of file")
    else newUTF8StringBuffer buf size

hGetStringBufferBlock :: Handle -> Int -> IO StringBuffer
hGetStringBufferBlock handle wanted = do
  size_i <- hFileSize handle
  offset_i <- hTell handle >>= skipBOM handle size_i
  let size = min wanted (fromIntegral $ size_i-offset_i)
  buf <- newPinnedMutableByteArray (size+3)
  r <- if size == 0
          then return 0
          else hGetBuf handle (unsafeMutableByteArrayContents buf) size
  if r /= size
    then ioError (userError $ "short read of file: "++show(r,size,size_i,handle))
    else newUTF8StringBuffer buf size

hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf cur) = do
  withByteArrayContents buf $ \ptr -> hPutBuf hdl (ptr `plusPtr` cur) (sizeofByteArray buf)

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

-- | @newUTF8StringBuffer buf size@ creates a 'StringBuffer' from a
-- 'MutableByteArray' of length @size+3@ containing UTF-8 encoded text. A three
-- byte sentinel will be added to the end of the buffer.
newUTF8StringBuffer :: MutableByteArray -> Int -> IO StringBuffer
newUTF8StringBuffer buf size = do
  ASSERTM(return $ sizeofMutableByteArray buf == (size + 3))
  -- sentinels for UTF-8 decoding
  writeWord8Array buf (size+0) 0
  writeWord8Array buf (size+1) 0
  writeWord8Array buf (size+3) 0
  buf' <- unsafeFreezeByteArray buf
  return $ StringBuffer buf' 0

appendStringBuffers :: StringBuffer -> StringBuffer -> IO StringBuffer
appendStringBuffers sb1 sb2 = do
  dst <- newPinnedMutableByteArray (size+3)
  copyByteArray (buf sb1) (cur sb1) dst 0 sb1_len
  copyByteArray (buf sb2) (cur sb2) dst sb1_len sb2_len
  newUTF8StringBuffer dst size
  where
    sb1_len = lengthStringBuffer sb1
    sb2_len = lengthStringBuffer sb2
    size =  sb1_len + sb2_len

withStringBufferContents :: StringBuffer -> (CStringLen -> IO a) -> IO a
withStringBufferContents sb@(StringBuffer buf cur) action =
  withByteArrayContents buf $ \p -> action (p `plusPtr` cur, lengthStringBuffer sb)

byteStringToStringBuffer :: BS.ByteString -> StringBuffer
byteStringToStringBuffer bs = unsafePerformIO $ do
  let size = BS.length bs
  buf <- newPinnedMutableByteArray (size+3)
  BS.unsafeUseAsCString bs (\p -> copyAddrToMutableByteArray p buf 0 size)
  newUTF8StringBuffer buf size

-- | Encode a 'String' into a 'StringBuffer' as UTF-8.  The resulting buffer
-- is automatically managed by the garbage collector.
stringToStringBuffer :: String -> StringBuffer
stringToStringBuffer str = unsafePerformIO $ do
  let size = utf8EncodedLength str
  buf <- newPinnedMutableByteArray (size+3)
  utf8EncodeString (unsafeMutableByteArrayContents buf) str
  newUTF8StringBuffer buf size

-- -----------------------------------------------------------------------------
-- Grab a character

-- | Return the first UTF-8 character of a nonempty 'StringBuffer' and as well
-- the remaining portion (analogous to 'Data.List.uncons').  __Warning:__ The
-- behavior is undefined if the 'StringBuffer' is empty.  The result shares
-- the same buffer as the original.  Similar to 'utf8DecodeChar', if the
-- character cannot be decoded as UTF-8, @\'\\0\'@ is returned.
{-# INLINE nextChar #-}
nextChar :: StringBuffer -> (Char,StringBuffer)
nextChar sb@(StringBuffer buf (I# cur#)) =
  -- Getting our fingers dirty a little here, but this is performance-critical
    case utf8DecodeCharByteArray# (getByteArray buf) cur# of
      (# c#, nBytes# #) ->
        (C# c#, checkValid $ sb { cur = I# (cur# +# nBytes#) })

-- | Return the first UTF-8 character of a nonempty 'StringBuffer' (analogous
-- to 'Data.List.head').  __Warning:__ The behavior is undefined if the
-- 'StringBuffer' is empty.  Similar to 'utf8DecodeChar', if the character
-- cannot be decoded as UTF-8, @\'\\0\'@ is returned.
currentChar :: StringBuffer -> Char
currentChar = fst . nextChar

prevChar :: StringBuffer -> Char -> Char
prevChar (StringBuffer _   0)   deflt = deflt
prevChar (StringBuffer buf cur) _     =
    let !(I# p') = utf8PrevChar (getByteArray buf) cur
        !(# c, _ #) = utf8DecodeCharByteArray# (getByteArray buf) p'
    in C# c

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
offsetBytes i s = checkValid $ s { cur = cur (checkValid s) + i }

-- | Compute the difference in offset between two 'StringBuffer's that share
-- the same buffer.  __Warning:__ The behavior is undefined if the
-- 'StringBuffer's use separate buffers.
byteDiff :: StringBuffer -> StringBuffer -> Int
byteDiff s1 s2 = cur s2 - cur s1

lengthStringBuffer :: StringBuffer -> Int
lengthStringBuffer sb = sizeofByteArray (buf sb) - cur sb - 3

-- | Check whether a 'StringBuffer' is empty (analogous to 'Data.List.null').
atEnd :: StringBuffer -> Bool
atEnd sb = lengthStringBuffer sb == 0

-- | Computes a 'StringBuffer' which points to the first character of the
-- wanted line. Lines begin at 1.
atLine :: Int -> StringBuffer -> Maybe StringBuffer
atLine line sb@(StringBuffer buf _) =
  inlinePerformIO $ withByteArrayContents buf $ \p -> do
    p' <- skipToLine line (lengthStringBuffer sb) p
    if p' == nullPtr
      then return Nothing
      else
        let !delta = p' `minusPtr` p
        in return $! Just $! checkValid $ sb { cur = delta }

-- | @skipToLine line len op0@ finds the byte offset to the beginning of
-- the given line number.
skipToLine :: Int -> Int -> Ptr Word8 -> IO (Ptr Word8)
skipToLine !line !len !op0 = go 1 op0
  where
    !op_end = op0 `plusPtr` len

    go !i_line !op
      | op >= op_end   = pure nullPtr
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
lexemeToString sb bytes
  | lengthStringBuffer sb < bytes = panic "lexemeToString: overflow 1"
  | not (isValid sb)  = panic "lexemeToString: overflow 2"
lexemeToString (StringBuffer buf (I# cur#)) (I# bytes#) =
  utf8DecodeByteArrayLazy# (getByteArray buf) cur# bytes#

lexemeToFastString :: StringBuffer
                   -> Int               -- ^ @n@, the number of bytes
                   -> FastString
lexemeToFastString _ 0 = nilFS
lexemeToFastString sb len | len > lengthStringBuffer sb = panic "lexemeToFastString"
lexemeToFastString (StringBuffer buf cur) len =
  inlinePerformIO $
    withByteArrayContents buf $ \ptr ->
      return $! mkFastStringBytes (ptr `plusPtr` cur) len

-- | Return the previous @n@ characters (or fewer if we are less than @n@
-- characters into the buffer.
decodePrevNChars :: Int -> StringBuffer -> String
decodePrevNChars n (StringBuffer buf0 cur) =
    go (getByteArray buf0) (min n (cur - 1)) "" (cur - 1)
  where
    go :: ByteArray# -> Int -> String -> Int -> String
    go buf n acc ofs
      | n == 0 = acc
      | otherwise =
          let !ofs'@(I# ofs'#) = utf8PrevChar buf ofs
              !(# c,_ #) = utf8DecodeCharByteArray# buf ofs'#
          in go buf (n - 1) (C# c:acc) ofs'

-- -----------------------------------------------------------------------------
-- Parsing integer strings in various bases
parseUnsignedInteger :: StringBuffer -> Int -> Integer -> (Char->Int) -> Integer
parseUnsignedInteger (StringBuffer buf (I# cur)) (I# len) radix char_to_int
  = go (len +# cur) cur 0
  where
    go :: Int# -> Int# -> Integer -> Integer
    go end i !acc
      | isTrue# (i ==# end) = acc
      | otherwise =
        case utf8DecodeCharByteArray# (getByteArray buf) i of
          (# '_'#, _ #) -> go end (i +# 1#) acc    -- skip "_" (#14473)
          (# char, _ #) -> go end (i +# 1#) (acc * radix + toInteger (char_to_int (C# char)))
