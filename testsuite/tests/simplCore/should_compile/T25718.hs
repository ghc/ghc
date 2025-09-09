module T25718 where

import Data.Char
import Data.Word

word8ToChar :: Word8 -> Char
word8ToChar = chr . fromIntegral

isAsciiChar :: Char -> Bool
isAsciiChar = (< 256) . ord

foo :: Word8 -> Bool
foo = isAsciiChar . word8ToChar

bar :: Word8 -> Bool
bar x = fromIntegral x <= 255
