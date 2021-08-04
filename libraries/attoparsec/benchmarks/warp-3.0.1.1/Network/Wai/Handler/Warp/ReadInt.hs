{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-- Copyright     : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3

module Network.Wai.Handler.Warp.ReadInt (
    readInt
  , readInt64
  ) where

-- This function lives in its own file because the MagicHash pragma interacts
-- poorly with the CPP pragma.

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.Int (Int64)
import GHC.Prim
import GHC.Types
import GHC.Word

{-# INLINE readInt #-}
readInt :: Integral a => ByteString -> a
readInt bs = fromIntegral $ readInt64 bs

-- This function is used to parse the Content-Length field of HTTP headers and
-- is a performance hot spot. It should only be replaced with something
-- significantly and provably faster.
--
-- It needs to be able work correctly on 32 bit CPUs for file sizes > 2G so we
-- use Int64 here and then make a generic 'readInt' that allows conversion to
-- Int and Integer.

{-# NOINLINE readInt64 #-}
readInt64 :: ByteString -> Int64
readInt64 bs = S.foldl' (\ !i !c -> i * 10 + fromIntegral (mhDigitToInt c)) 0
             $ S.takeWhile isDigit bs

data Table = Table !Addr#

{-# NOINLINE mhDigitToInt #-}
mhDigitToInt :: Word8 -> Int
mhDigitToInt (W8# i) = I# (word2Int# (indexWord8OffAddr# addr (word2Int# i)))
  where
    !(Table addr) = table
    table :: Table
    table = Table
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

isDigit :: Word8 -> Bool
isDigit w = w >= 48 && w <= 57
