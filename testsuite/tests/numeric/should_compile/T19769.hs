{-# LANGUAGE MagicHash #-}

module T19769 where

import GHC.Exts

wi8 :: Word8# -> Int8#
wi8 x = intToInt8# (word2Int# (word8ToWord# x))

wi16 :: Word16# -> Int16#
wi16 x = intToInt16# (word2Int# (word16ToWord# x))

wi32 :: Word32# -> Int32#
wi32 x = intToInt32# (word2Int# (word32ToWord# x))

wi64 :: Word64# -> Int64#
wi64 x = intToInt64# (word2Int# (word64ToWord# x))

iw8 :: Int8# -> Word8#
iw8 x = wordToWord8# (int2Word# (int8ToInt# x))

iw16 :: Int16# -> Word16#
iw16 x = wordToWord16# (int2Word# (int16ToInt# x))

iw32:: Int32# -> Word32#
iw32 x = wordToWord32# (int2Word# (int32ToInt# x))

iw64:: Int64# -> Word64#
iw64 x = wordToWord64# (int2Word# (int64ToInt# x))

i8 :: Int8# -> Int8#
i8 x = intToInt8# (int8ToInt# x)

i16 :: Int16# -> Int16#
i16 x = intToInt16# (int16ToInt# x)

i32 :: Int32# -> Int32#
i32 x = intToInt32# (int32ToInt# x)

i64 :: Int64# -> Int64#
i64 x = intToInt64# (int64ToInt# x)

w8 :: Word8# -> Word8#
w8 x = wordToWord8# (word8ToWord# x)

w16 :: Word16# -> Word16#
w16 x = wordToWord16# (word16ToWord# x)

w32 :: Word32# -> Word32#
w32 x = wordToWord32# (word32ToWord# x)

w64 :: Word64# -> Word64#
w64 x = wordToWord64# (word64ToWord# x)


w :: Word# -> Word#
w x = word64ToWord# (wordToWord64# x)

i :: Int# -> Int#
i x = int64ToInt# (intToInt64# x)

wiw64 :: Word64# -> Word64#
wiw64 x = int64ToWord64# (word64ToInt64# x)

iwi64 :: Int64# -> Int64#
iwi64 x = word64ToInt64# (int64ToWord64# x)

ww64i :: Word# -> Int#
ww64i x = int64ToInt# (word64ToInt64# (wordToWord64# x))

ii64w :: Int# -> Word#
ii64w x = word64ToWord# (int64ToWord64# (intToInt64# x))
