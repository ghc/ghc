{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- Test for #26631, on simd013's source (simd/should_run).
-- The value add7 returns in %xmm0 is live across the call of sub, whose
-- argument moves write %xmm0 and %xmm1 and which clobbers every vector
-- register, so the register allocator saves it on the C stack. It should
-- spill it from %xmm0 at once, not move it to %xmm1 and then to %xmm2,
-- each move made because the next argument move writes the register.

module Main where

import GHC.Exts
import GHC.Prim

foreign import ccall unsafe "sub"
  sub :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "add7"
  add7 :: DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#

main :: IO ()
main = do
  let x1, x2 :: DoubleX2#
      x1 = packDoubleX2# (# 9.9##, 99.99## #)
      x2 = packDoubleX2# (# 1.1##, 11.11## #)
      y1, y2, y3, y4, y5, y6, y7 :: DoubleX2#
      !y1 = packDoubleX2# (#       1.5##,             2.5## #)
      !y2 = packDoubleX2# (#      10.25##,           20.25## #)
      !y3 = packDoubleX2# (#     100.125##,         200.125## #)
      !y4 = packDoubleX2# (#    1000.0625##,       2000.0625## #)
      !y5 = packDoubleX2# (#   10000.03125##,     20000.03125## #)
      !y6 = packDoubleX2# (#  100000.015625##,   200000.015625## #)
      !y7 = packDoubleX2# (# 1000000.0078125##, 2000000.0078125## #)
      !(# a, b #) = unpackDoubleX2# ( sub x1 x2 )
      !(# c, d #) = unpackDoubleX2# ( add7 y1 y2 y3 y4 y5 y6 y7 )
  print ( D# a, D# b )
  print ( D# c, D# d )
