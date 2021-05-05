{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedSums               #-}
{-# LANGUAGE UnboxedTuples             #-}
module Main (main) where

import GHC.Base

data MyArray t = MyArray (# t | ByteArray# #)

getBytes :: MyArray t -> ByteArray#
-- This would work on GHC versions < 9.0 when uncommented, but not on 9.0.1!
-- getBytes (MyArray (# | arr #)) = case runRW# (\s -> (# touch# arr s, arr #)) of (# _, r #) -> r
getBytes (MyArray (# | arr #)) = arr
getBytes _ = mkByteArray 13
-- Commenting out this NOINLINE pragma also makes it work successfully
{-#  NOINLINE getBytes #-}

mkByteArray :: Double -> ByteArray#
mkByteArray (D# x) = case runRW#
  ( \s0 -> case newByteArray# 8# s0 of
      (# s1, mba #) -> unsafeFreezeByteArray# mba ( writeDoubleArray# mba 0# x s1)
  ) of (# _, ba #) -> ba

main :: IO ()
main = print $ case getBytes x of a ->  D# (indexDoubleArray# a 0#)
  where
    x :: MyArray Double
    x = MyArray (# | mkByteArray 7 #)

