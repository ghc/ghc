{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import Data.Int
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

main :: IO ()
main = do
  -- Use some negative values to fill more bits and discover possible overlaps.
  let v8 = packInt64X2# (# 0#Int64, -1#Int64 #)
      v9 = packInt64X2# (# -2#Int64, 3#Int64 #)
      v10 = packInt64X2# (# -4#Int64, 5#Int64 #)
      v11 = packInt64X2# (# -6#Int64, 7#Int64 #)
      v12 = packInt64X2# (# -8#Int64, 9#Int64 #)
      v13 = packInt64X2# (# -10#Int64, 11#Int64 #)
      v14 = packInt64X2# (# -12#Int64, 13#Int64 #)
      v15 = packInt64X2# (# -14#Int64, 15#Int64 #)
      v16 = packInt64X2# (# -16#Int64, 17#Int64 #)
      v17 = packInt64X2# (# -18#Int64, 19#Int64 #)
      v18 = packInt64X2# (# -20#Int64, 21#Int64 #)
      v19 = packInt64X2# (# -22#Int64, 23#Int64 #)
      v20 = packInt64X2# (# -24#Int64, 25#Int64 #)
      v21 = packInt64X2# (# -26#Int64, 27#Int64 #)
      v22 = packInt64X2# (# -28#Int64, 29#Int64 #)
      v23 = packInt64X2# (# -30#Int64, 31#Int64 #)

  print "Arguments"
  hFlush stdout
  printVecs_int64x2#
    v8
    v9
    v10
    v11
    v12
    v13
    v14
    v15
    v16
    v17
    v18
    v19
    v20
    v21
    v22
    v23

  print "Return values"
  let v = return_int64X2# (# #)
  print $ unpackInt64X2 v

  print "Initial vectors" -- Check that these registers weren't messed up
  print $ unpackInt64X2 v8
  print $ unpackInt64X2 v9
  print $ unpackInt64X2 v10
  print $ unpackInt64X2 v11
  print $ unpackInt64X2 v12
  print $ unpackInt64X2 v13
  print $ unpackInt64X2 v14
  print $ unpackInt64X2 v15
  print $ unpackInt64X2 v16
  print $ unpackInt64X2 v17
  print $ unpackInt64X2 v18
  print $ unpackInt64X2 v19
  print $ unpackInt64X2 v20
  print $ unpackInt64X2 v21
  print $ unpackInt64X2 v22
  print $ unpackInt64X2 v23
