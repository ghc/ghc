{-# LANGUAGE OverloadedStrings #-}
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
       ) where

import GhcPrelude
import GHC.IO.Handle (Handle)
import Encoding (utf8PrevChar, utf8DecodeChar)
import Foreign (withForeignPtr, plusPtr)
import FastFunctions (inlinePerformIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI (toForeignPtr)

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
