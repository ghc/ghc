{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module StringBufferNew
       (
         StringBuffer
       , hGetStringBuffer
       , hGetStringBufferBlock
       , hPutStringBuffer
       , appendStringBuffers
       , stringToStringBuffer

       , nextChar
       , currentChar
       , prevChar
       , atEnd

       , stepOn
       , offsetBytes
       , byteDiff
       , atLine

       , lexemeToString
       , lexemeToFastString
       , decodePrevNChars

       , parseUnsignedInteger
       ) where

import GhcPrelude
import GHC.IO.Handle (Handle)
import Encoding (utf8PrevChar, utf8DecodeChar)
import Foreign (Ptr, Word8, withForeignPtr, plusPtr, minusPtr, nullPtr, peek)
import FastFunctions (inlinePerformIO)
import FastString (FastString, nilFS, mkFastStringBytes)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI (fromForeignPtr, toForeignPtr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8, decodeUtf8With)

newtype StringBuffer = StringBuffer { buffer :: B.ByteString }

-- TODO Enhance
instance Show StringBuffer where
  showsPrec _ _ = showString "<stringbuffer>"


-- -----------------------------------------------------------------------------
-- Creation / Destruction

hGetStringBuffer :: FilePath -> IO StringBuffer
hGetStringBuffer fname = do
  b <- B.readFile fname
  return . StringBuffer $ skipBOM 0 b `B.append` "\0\0\0"

hGetStringBufferBlock :: Handle -> Int -> IO StringBuffer
hGetStringBufferBlock handle wanted = do
  b <- B.hGetSome handle wanted
  return . StringBuffer $ skipBOM 0 b `B.append` "\0\0\0"

hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl = B.hPut hdl . buffer

skipBOM :: Int -> B.ByteString -> B.ByteString
skipBOM offset = stripBOMprefix . B.drop offset
  where
    bom :: B.ByteString
    bom = C.singleton '\xfeff'

    stripBOMprefix :: B.ByteString -> B.ByteString
    stripBOMprefix bs = maybe bs id $ B.stripPrefix bom bs

appendStringBuffers :: StringBuffer -> StringBuffer -> StringBuffer
appendStringBuffers sb1 sb2 = StringBuffer $ buffer sb1 `B.append` buffer sb2

-- TODO Check encoding here
stringToStringBuffer :: String -> StringBuffer
stringToStringBuffer = StringBuffer . C.pack

-- -----------------------------------------------------------------------------
-- Grab a character

-- TODO Does this do encodings correctly? Is the the default OK?
{-# INLINE nextChar #-}
nextChar :: StringBuffer -> (Char, StringBuffer)
nextChar (StringBuffer bs) = fmap StringBuffer . maybe ('\0', bs) id $ C.uncons bs

-- TODO Does this do encodings correctly? Is the the default OK?
currentChar :: StringBuffer -> Char
currentChar = C.head . buffer

-- TODO Holy fuck. Revisit this. An alternative here is to save off the
-- previous character in the type, but then we don't get a newtype
-- (can things be unpacked so this can all be put on the stack?)
-- If this remains, it at the very least needs a test...
prevChar :: StringBuffer -> Char -> Char
prevChar (StringBuffer bs) deflt = case BI.toForeignPtr bs of
  (_, 0, _) -> deflt
  (buf, off, _) -> inlinePerformIO $ do
    withForeignPtr buf $ \p -> do
      p' <- utf8PrevChar (p `plusPtr` off)
      return . fst $ utf8DecodeChar p'

-- -----------------------------------------------------------------------------
-- Moving

stepOn :: StringBuffer -> StringBuffer
stepOn = snd . nextChar

offsetBytes :: Int -> StringBuffer -> StringBuffer
offsetBytes offset = StringBuffer . B.drop offset . buffer

-- TODO Test this also
byteDiff :: StringBuffer -> StringBuffer -> Int
byteDiff s1 s2 = offset s2 - offset s1
  where
    offset s = let (_, off, _) = BI.toForeignPtr (buffer s) in off

atEnd :: StringBuffer -> Bool
atEnd = B.null . buffer

atLine :: Int -> StringBuffer -> Maybe StringBuffer
atLine line sb =
  let (buf, _, len) = BI.toForeignPtr (buffer sb) in
    inlinePerformIO $
      withForeignPtr buf $ \p -> do
        p' <- skipToLine line len p
        if p' == nullPtr
          then return Nothing
          else
            let
              delta = p' `minusPtr` p
            in return . Just . StringBuffer $ BI.fromForeignPtr buf delta (len - delta)

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
            10 -> go (i_line + 1) (op `plusPtr` 1)
            13 -> do
              -- this is safe because a 'StringBuffer' is
              -- guaranteed to have 3 bytes sentinel values.
              w' <- peek (op `plusPtr` 1) :: IO Word8
              case w' of
                10 -> go (i_line + 1) (op `plusPtr` 2)
                _ -> go (i_line + 1) (op `plusPtr` 1)
            _ -> go i_line (op `plusPtr` 1)


lexemeToString :: StringBuffer -> Int -> String
lexemeToString _ 0 = ""
lexemeToString sb bytes = stringDecode . B.take bytes $ buffer sb
  where
    nullOnError _ _ = Just '\0'

    stringDecode :: B.ByteString -> String
    stringDecode = T.unpack . T.decodeUtf8With nullOnError

lexemeToFastString :: StringBuffer -> Int -> FastString
lexemeToFastString _ 0 = nilFS
lexemeToFastString sb len =
  let
    (buf, offset, _) = BI.toForeignPtr (buffer sb)
  in
    inlinePerformIO $
      withForeignPtr buf $ \ptr ->
        return $! mkFastStringBytes (ptr `plusPtr` offset) len

decodePrevNChars :: Int -> StringBuffer -> String
decodePrevNChars n sb =
  let
    (buf, offset, _) = BI.toForeignPtr (buffer sb)
  in
    inlinePerformIO $ withForeignPtr buf $ \ptr ->
       go ptr n "" (ptr `plusPtr` (offset - 1))
  where
    go :: Ptr Word8 -> Int -> String -> Ptr Word8 -> IO String
    go buf !n acc p | n == 0 || buf >= p = return acc
    go buf !n acc p = do
      p' <- utf8PrevChar p
      let (c, _) = utf8DecodeChar p'
      go buf (n - 1) (c:acc) p'

parseUnsignedInteger :: StringBuffer -> Int -> Integer -> (Char -> Int) -> Integer
parseUnsignedInteger sb len radix char_to_int =
  T.foldl' aggregate 0 . T.decodeUtf8 . B.take len $ buffer sb
  where
    aggregate :: Integer -> Char -> Integer
    aggregate acc '_' = acc
    aggregate acc char = acc * radix * toInteger (char_to_int char)
