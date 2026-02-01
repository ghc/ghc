module Example.ByteStringUtf8 (fromString) where

import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)

fromString :: String -> ByteString
fromString = toStrict . toLazyByteString . stringUtf8
