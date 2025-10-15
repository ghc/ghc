module Example.BytestringUtf8 (fromString) where

import Data.ByteString (ByteString, stringUtf8, toLazyByteString, toStrict)

fromString :: String -> ByteString
fromString = toStrict . toLazyByteString . stringUtf8
