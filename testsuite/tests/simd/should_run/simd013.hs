{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- test C calls with SIMD vectors

module Main where

import GHC.Exts
import GHC.Prim

foreign import ccall "sub"
  sub :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall "add6"
  add6 :: DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#

main :: IO ()
main = do
  let x1, x2 :: DoubleX2#
      x1 = packDoubleX2# (# 9.9##, 99.99## #)
      x2 = packDoubleX2# (# 1.1##, 11.11## #)
      y1, y2, y3, y4, y5, y6 :: DoubleX2#
      !y1 = packDoubleX2# (#      1.5##,           2.5## #)
      !y2 = packDoubleX2# (#     10.25##,         20.25## #)
      !y3 = packDoubleX2# (#    100.125##,       200.125## #)
      !y4 = packDoubleX2# (#   1000.0625##,     2000.0625## #)
      !y5 = packDoubleX2# (#  10000.03125##,   20000.03125## #)
      !y6 = packDoubleX2# (# 100000.015625##, 200000.015625## #)
      !(# a, b #) = unpackDoubleX2# ( sub x1 x2 )
      !(# c, d #) = unpackDoubleX2# ( add6 y1 y2 y3 y4 y5 y6 )
  print ( D# a, D# b )
  print ( D# c, D# d )
