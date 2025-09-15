{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}

module Simd009b where

import Control.Monad ( unless )
import Data.Foldable ( for_ )
import GHC.Exts

data FloatX4  = FX4# FloatX4#

instance Show FloatX4 where
  show (FX4# f) = case (unpackFloatX4# f) of
    (# a, b, c, d #) -> show ((F# a), (F# b), (F# c), (F# d))


instance Eq FloatX4 where
  (FX4# a) == (FX4# b)
    = case (unpackFloatX4# a) of
        (# a1, a2, a3, a4 #) ->
          case (unpackFloatX4# b) of
            (# b1, b2, b3, b4 #) -> (F# a1) == (F# b1) &&
                                    (F# a2) == (F# b2) &&
                                    (F# a3) == (F# b3) &&
                                    (F# a4) == (F# b4)

data DoubleX2 = DX2# DoubleX2#

instance Show DoubleX2 where
  show (DX2# d) = case (unpackDoubleX2# d) of
    (# a, b #) -> show ((D# a), (D# b))

instance Eq DoubleX2 where
  (DX2# a) == (DX2# b)
    = case (unpackDoubleX2# a) of
        (# a1, a2 #) ->
          case (unpackDoubleX2# b) of
            (# b1, b2 #) -> (D# a1) == (D# b1) &&
                            (D# a2) == (D# b2)

myShuffleDoubleX2 :: DoubleX2# -> DoubleX2# -> (# Int#, Int# #) -> DoubleX2#
myShuffleDoubleX2 v1 v2 (# i1, i2 #) =
  case unpackDoubleX2# v1 of
    (# d1, d2 #) ->
      case unpackDoubleX2# v2 of
        (# d3, d4 #) ->
          let ds = [ D# d1, D# d2, D# d3, D# d4 ]
              D# x = ds !! I# i1
              D# y = ds !! I# i2
          in packDoubleX2# (# x, y #)

myShuffleFloatX4 :: FloatX4# -> FloatX4# -> (# Int#, Int#, Int#, Int# #) -> FloatX4#
myShuffleFloatX4 v1 v2 (# i1, i2, i3, i4 #) =
  case unpackFloatX4# v1 of
    (# f1, f2, f3, f4 #) ->
      case unpackFloatX4# v2 of
        (# f5, f6, f7, f8 #) ->
          let fs = [ F# f1, F# f2, F# f3, F# f4
                   , F# f5, F# f6, F# f7, F# f8 ]
              F# x = fs !! I# i1
              F# y = fs !! I# i2
              F# z = fs !! I# i3
              F# w = fs !! I# i4
          in packFloatX4# (# x, y, z, w #)
