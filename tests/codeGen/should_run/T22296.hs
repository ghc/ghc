{-# language MagicHash, UnboxedTuples, UnboxedSums #-}

module Main ( main ) where

import GHC.Exts
import GHC.Int
import GHC.Word

foo :: Word16X8# -> Integer
foo w16x8 =
  case unpackWord16X8# w16x8 of
    (# w1, w2, w3, w4, w5, w6, w7, w8 #) ->
      let
        s = sum $ map fromIntegral
             [ W16# w1, W16# w2, W16# w3, W16# w4
             , W16# w5, W16# w6, W16# w7, W16# w8 ]
      in s

bar :: Int32X4# -> Integer
bar i32x4 =
  case unpackInt32X4# i32x4 of
    (# i1, i2, i3, i4 #) ->
      let
        s = sum $ map fromIntegral
             [ I32# i1, I32# i2, I32# i3, I32# i4 ]
      in s

baz :: FloatX4# -> Float
baz fx4 =
  case unpackFloatX4# fx4 of
    (# f1, f2, f3, f4 #) ->
      let
        s = sum
             [ F# f1, F# f2, F# f3, F# f4 ]
      in s

main :: IO ()
main = do
  print ( foo ( broadcastWord16X8# ( wordToWord16# 1## ) ) )
  print ( bar ( broadcastInt32X4#  ( intToInt32# 1# ) ) )
  print ( baz ( broadcastFloatX4#  ( 1.0# ) ) )
