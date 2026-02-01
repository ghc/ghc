module Example.ByteStringAscii (fromString) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

fromString :: String -> ByteString
fromString = pack
