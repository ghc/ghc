{-# LANGUAGE MagicHash, UnboxedTuples, ExtendedLiterals, UnliftedFFITypes #-}

module Main where

import GHC.Exts
import GHC.Int

foreign import ccall unsafe
  packsi32 :: Int32X4# -> Int32X4# -> Int16X8#

main :: IO ()
main = do
  let a = broadcastInt32X4# 100#Int32
      b = broadcastInt32X4# 200#Int32
      c = packsi32 a b
      (# x0, x1, x2, x3, x4, x5, x6, x7 #) = unpackInt16X8# c
  print (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7)
