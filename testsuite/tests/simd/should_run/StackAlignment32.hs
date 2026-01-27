{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}
module StackAlignment32 where
import GHC.Exts

foreign import ccall unsafe add10 :: DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4#

foo :: Double -> IO ()
foo (D# x) = do
  let a = broadcastDoubleX4# x
      b = packDoubleX4# (# 1.0##, 2.0##, 3.0##, 4.0## #)
      c = add10 a a a a a a a a a b
      (# c0, c1, c2, c3 #) = unpackDoubleX4# c
  print (D# c0, D# c1, D# c2, D# c3)

foreign export ccall foo :: Double -> IO ()
