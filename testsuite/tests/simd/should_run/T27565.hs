{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Main (main) where
import GHC.Exts
import GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))

{-# OPAQUE broadcast_i8 #-}
broadcast_i8 :: Int8# -> Int8X16#
broadcast_i8 x = broadcastInt8X16# x

{-# OPAQUE broadcast_i16 #-}
broadcast_i16 :: Int16# -> Int16X8#
broadcast_i16 x = broadcastInt16X8# x

{-# OPAQUE broadcast_i32 #-}
broadcast_i32 :: Int32# -> Int32X4#
broadcast_i32 x = broadcastInt32X4# x

{-# OPAQUE broadcast_i64 #-}
broadcast_i64 :: Int64# -> Int64X2#
broadcast_i64 x = broadcastInt64X2# x

main :: IO ()
main = do
  case unpackInt8X16# (broadcast_i8 (intToInt8# 32#)) of
    (# a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15 #) ->
      print [ I8# a0, I8# a1, I8# a2, I8# a3, I8# a4, I8# a5, I8# a6, I8# a7
            , I8# a8, I8# a9, I8# a10, I8# a11, I8# a12, I8# a13, I8# a14, I8# a15 ]
  case unpackInt16X8# (broadcast_i16 (intToInt16# 32#)) of
    (# b0,b1,b2,b3,b4,b5,b6,b7 #) ->
      print [ I16# b0, I16# b1, I16# b2, I16# b3, I16# b4, I16# b5, I16# b6, I16# b7 ]
  case unpackInt32X4# (broadcast_i32 (intToInt32# 32#)) of
    (# c0,c1,c2,c3 #) ->
      print [ I32# c0, I32# c1, I32# c2, I32# c3 ]
  case unpackInt64X2# (broadcast_i64 (intToInt64# 32#)) of
    (# d0,d1 #) ->
      print [ I64# d0, I64# d1 ]
