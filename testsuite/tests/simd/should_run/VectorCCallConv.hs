{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Data.Int
import GHC.Float
import GHC.Int
import GHC.Prim
import System.IO

foreign import ccall "printVecs_int64x2_c"
  printVecs_int64x2# ::
    Int64X2# -> -- v8
    Int64X2# -> -- v9
    Int64X2# -> -- v10
    Int64X2# -> -- v11
    Int64X2# -> -- v12
    Int64X2# -> -- v13
    Int64X2# -> -- v14
    Int64X2# -> -- v15
    Int64X2# -> -- v16
    Int64X2# -> -- v17
    Int64X2# -> -- v18
    Int64X2# -> -- v19
    Int64X2# -> -- v20
    Int64X2# -> -- v21
    Int64X2# -> -- v22
    Int64X2# -> -- v23
    IO ()

foreign import ccall "return_int64X2"
  return_int64X2# :: (# #) -> Int64X2#

unpackInt64X2 :: Int64X2# -> (Int64, Int64)
unpackInt64X2 v = case unpackInt64X2# v of
  (# x0, x1 #) -> (I64# x0, I64# x1)

foreign import ccall "printVecs_doublex2_c"
  printVecs_doublex2# ::
    DoubleX2# -> -- v8
    DoubleX2# -> -- v9
    DoubleX2# -> -- v10
    DoubleX2# -> -- v11
    DoubleX2# -> -- v12
    DoubleX2# -> -- v13
    DoubleX2# -> -- v14
    DoubleX2# -> -- v15
    DoubleX2# -> -- v16
    DoubleX2# -> -- v17
    DoubleX2# -> -- v18
    DoubleX2# -> -- v19
    DoubleX2# -> -- v20
    DoubleX2# -> -- v21
    DoubleX2# -> -- v22
    DoubleX2# -> -- v23
    IO ()

foreign import ccall "return_doubleX2"
  return_doubleX2# :: (# #) -> DoubleX2#

unpackDoubleX2 :: DoubleX2# -> (Double, Double)
unpackDoubleX2 v = case unpackDoubleX2# v of
  (# x0, x1 #) -> (D# x0, D# x1)

main :: IO ()
main = do
  -- Use some negative values to fill more bits and discover possible overlaps.
  let int_v8 = packInt64X2# (# 0#Int64, -1#Int64 #)
      int_v9 = packInt64X2# (# -2#Int64, 3#Int64 #)
      int_v10 = packInt64X2# (# -4#Int64, 5#Int64 #)
      int_v11 = packInt64X2# (# -6#Int64, 7#Int64 #)
      int_v12 = packInt64X2# (# -8#Int64, 9#Int64 #)
      int_v13 = packInt64X2# (# -10#Int64, 11#Int64 #)
      int_v14 = packInt64X2# (# -12#Int64, 13#Int64 #)
      int_v15 = packInt64X2# (# -14#Int64, 15#Int64 #)
      int_v16 = packInt64X2# (# -16#Int64, 17#Int64 #)
      int_v17 = packInt64X2# (# -18#Int64, 19#Int64 #)
      int_v18 = packInt64X2# (# -20#Int64, 21#Int64 #)
      int_v19 = packInt64X2# (# -22#Int64, 23#Int64 #)
      int_v20 = packInt64X2# (# -24#Int64, 25#Int64 #)
      int_v21 = packInt64X2# (# -26#Int64, 27#Int64 #)
      int_v22 = packInt64X2# (# -28#Int64, 29#Int64 #)
      int_v23 = packInt64X2# (# -30#Int64, 31#Int64 #)

      double_v8 = packDoubleX2# (# 0.0##, -1.0## #)
      double_v9 = packDoubleX2# (# -2.0##, 3.0## #)
      double_v10 = packDoubleX2# (# -4.0##, 5.0## #)
      double_v11 = packDoubleX2# (# -6.0##, 7.0## #)
      double_v12 = packDoubleX2# (# -8.0##, 9.0## #)
      double_v13 = packDoubleX2# (# -10.0##, 11.0## #)
      double_v14 = packDoubleX2# (# -12.0##, 13.0## #)
      double_v15 = packDoubleX2# (# -14.0##, 15.0## #)
      double_v16 = packDoubleX2# (# -16.0##, 17.0## #)
      double_v17 = packDoubleX2# (# -18.0##, 19.0## #)
      double_v18 = packDoubleX2# (# -20.0##, 21.0## #)
      double_v19 = packDoubleX2# (# -22.0##, 23.0## #)
      double_v20 = packDoubleX2# (# -24.0##, 25.0## #)
      double_v21 = packDoubleX2# (# -26.0##, 27.0## #)
      double_v22 = packDoubleX2# (# -28.0##, 29.0## #)
      double_v23 = packDoubleX2# (# -30.0##, 31.0## #)

  print "Arguments (int)"
  hFlush stdout
  printVecs_int64x2#
    int_v8
    int_v9
    int_v10
    int_v11
    int_v12
    int_v13
    int_v14
    int_v15
    int_v16
    int_v17
    int_v18
    int_v19
    int_v20
    int_v21
    int_v22
    int_v23

  print "Arguments (double)"
  hFlush stdout
  printVecs_doublex2#
    double_v8
    double_v9
    double_v10
    double_v11
    double_v12
    double_v13
    double_v14
    double_v15
    double_v16
    double_v17
    double_v18
    double_v19
    double_v20
    double_v21
    double_v22
    double_v23

  print "Return values (int)"
  let v = return_int64X2# (# #)
  print $ unpackInt64X2 v

  print "Return values (double)"
  let v = return_doubleX2# (# #)
  print $ unpackDoubleX2 v

  -- Check that these registers weren't messed up
  print "Initial vectors (int)"
  print $ unpackInt64X2 int_v8
  print $ unpackInt64X2 int_v9
  print $ unpackInt64X2 int_v10
  print $ unpackInt64X2 int_v11
  print $ unpackInt64X2 int_v12
  print $ unpackInt64X2 int_v13
  print $ unpackInt64X2 int_v14
  print $ unpackInt64X2 int_v15
  print $ unpackInt64X2 int_v16
  print $ unpackInt64X2 int_v17
  print $ unpackInt64X2 int_v18
  print $ unpackInt64X2 int_v19
  print $ unpackInt64X2 int_v20
  print $ unpackInt64X2 int_v21
  print $ unpackInt64X2 int_v22
  print $ unpackInt64X2 int_v23

  print "Initial vectors (double)"
  print $ unpackDoubleX2 double_v8
  print $ unpackDoubleX2 double_v9
  print $ unpackDoubleX2 double_v10
  print $ unpackDoubleX2 double_v11
  print $ unpackDoubleX2 double_v12
  print $ unpackDoubleX2 double_v13
  print $ unpackDoubleX2 double_v14
  print $ unpackDoubleX2 double_v15
  print $ unpackDoubleX2 double_v16
  print $ unpackDoubleX2 double_v17
  print $ unpackDoubleX2 double_v18
  print $ unpackDoubleX2 double_v19
  print $ unpackDoubleX2 double_v20
  print $ unpackDoubleX2 double_v21
  print $ unpackDoubleX2 double_v22
  print $ unpackDoubleX2 double_v23
