module Test where

import Data.Word
import Data.Int
import Data.Bits

w8 :: Word8 -> Word8
w8 x = x .&. 0xFF

w16 :: Word16 -> Word16
w16 x = x .&. 0xFFFF

w32 :: Word32 -> Word32
w32 x = x .&. 0xFFFFFFFF

w64 :: Word64 -> Word64
w64 x = x .&. 0xFFFFFFFFFFFFFFFF

w :: Word -> Word
w x = x .&. maxBound


i8 :: Int8 -> Int8
i8 x = x .&. (-1)

i16 :: Int16 -> Int16
i16 x = x .&. (-1)

i32 :: Int32 -> Int32
i32 x = x .&. (-1)

i64 :: Int64 -> Int64
i64 x = x .&. (-1)

i :: Int -> Int
i x = x .&. (-1)
