module T20203 where

import Data.Bits
import Data.Int

bitAndInt :: Int -> Int
bitAndInt x = (x .&. 0xFA) .&. 0xAF

bitOrInt :: Int -> Int
bitOrInt x = (x .|. 0xFA) .|. 0xAF

bitAndInt8 :: Int8 -> Int8
bitAndInt8 x = (x .&. 0x1) .&. 0x10

bitOrInt8 :: Int8 -> Int8
bitOrInt8 x = (x .|. 0x1) .|. 0x10

bitAndInt16 :: Int16 -> Int16
bitAndInt16 x = (x .&. 0xFA) .&. 0xAF

bitOrInt16 :: Int16 -> Int16
bitOrInt16 x = (x .|. 0xFA) .|. 0xAF

bitAndInt32 :: Int32 -> Int32
bitAndInt32 x = (x .&. 0xFA) .&. 0xAF

bitOrInt32 :: Int32 -> Int32
bitOrInt32 x = (x .|. 0xFA) .|. 0xAF

bitAndInt64 :: Int64 -> Int64
bitAndInt64 x = (x .&. 0xFA) .&. 0xAF

bitOrInt64 :: Int64 -> Int64
bitOrInt64 x = (x .|. 0xFA) .|. 0xAF

bitAndTwoVarInt :: Int -> Int -> Int
bitAndTwoVarInt x y = (x .&. 0xFA) .&. (y .&. 0xAF)

bitOrTwoVarInt :: Int -> Int -> Int
bitOrTwoVarInt x y = (x .|. 0xFA) .|. (y .|. 0xAF)

bitAndTwoVarInt8 :: Int8 -> Int8 -> Int8
bitAndTwoVarInt8 x y = (x .&. 0x1) .&. (y .&. 0x10)

bitOrTwoVarInt8 :: Int8 -> Int8 -> Int8
bitOrTwoVarInt8 x y = (x .|. 0x1) .|. (y .|. 0x10)

bitAndTwoVarInt16 :: Int16 -> Int16 -> Int16
bitAndTwoVarInt16 x y = (x .&. 0xFA) .&. (y .&. 0xAF)

bitOrTwoVarInt16 :: Int16 -> Int16 -> Int16
bitOrTwoVarInt16 x y = (x .|. 0xFA) .|. (y .|. 0xAF)

bitAndTwoVarInt32 :: Int32 -> Int32 -> Int32
bitAndTwoVarInt32 x y = (x .&. 0xFA) .&. (y .&. 0xAF)

bitOrTwoVarInt32 :: Int32 -> Int32 -> Int32
bitOrTwoVarInt32 x y = (x .|. 0xFA) .|. (y .|. 0xAF)

bitAndTwoVarInt64 :: Int64 -> Int64 -> Int64
bitAndTwoVarInt64 x y = (x .&. 0xFA) .&. (y .&. 0xAF)

bitOrTwoVarInt64 :: Int64 -> Int64 -> Int64
bitOrTwoVarInt64 x y = (x .|. 0xFA) .|. (y .|. 0xAF)
