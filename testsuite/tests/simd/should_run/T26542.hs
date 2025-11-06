{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts

type D8# = (# DoubleX2#, Double#, DoubleX2#, Double#, DoubleX2# #)
type D8  = (Double, Double, Double, Double, Double, Double, Double, Double)

unD# :: Double -> Double#
unD# (D# x) = x

mkD8# :: Double -> D8#
mkD8# x =
  (# packDoubleX2# (# unD# x, unD# (x + 1) #)
   , unD# (x + 2)
   , packDoubleX2# (# unD# (x + 3), unD# (x + 4) #)
   , unD# (x + 5)
   , packDoubleX2# (# unD# (x + 6), unD# (x + 7) #)
   #)
{-# NOINLINE mkD8# #-}

unD8# :: D8# -> D8
unD8# (# v0, x2, v1, x5, v2 #) =
  case unpackDoubleX2# v0 of
    (# x0, x1 #) ->
      case unpackDoubleX2# v1 of
        (# x3, x4 #) ->
          case unpackDoubleX2# v2 of
            (# x6, x7 #) ->
              (D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7)
{-# NOINLINE unD8# #-}

type D32# = (# D8#, D8#, D8#, D8# #)
type D32  = (D8, D8, D8, D8)

mkD32# :: Double -> D32#
mkD32# x = (# mkD8# x, mkD8# (x + 8), mkD8# (x + 16), mkD8# (x + 24) #)
{-# NOINLINE mkD32# #-}

unD32# :: D32# -> D32
unD32# (# x0, x1, x2, x3 #) =
  (unD8# x0, unD8# x1, unD8# x2, unD8# x3)
{-# NOINLINE unD32# #-}

main :: IO ()
main = do
  let
    !x = mkD32# 0
    !ds = unD32# x
  print ds
