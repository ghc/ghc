{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}
module StackAlignment64 where
import GHC.Exts

foreign import ccall unsafe add10 :: DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8#

foo :: Double -> IO ()
foo (D# x) = do
  let a = broadcastDoubleX8# x
      b = packDoubleX8# (# 1.0##, 2.0##, 3.0##, 4.0##, 5.0##, 6.0##, 7.0##, 8.0## #)
      c = add10 a a a a a a a a a b
      (# c0, c1, c2, c3, c4, c5, c6, c7 #) = unpackDoubleX8# c
  print (D# c0, D# c1, D# c2, D# c3, D# c4, D# c5, D# c6, D# c7)

foreign export ccall foo :: Double -> IO ()
