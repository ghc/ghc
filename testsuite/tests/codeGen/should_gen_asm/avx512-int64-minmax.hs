{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Exts
import GHC.Prim
import GHC.Int

{-# NOINLINE f #-}
f :: Int64X2# -> Int64X2# -> Int64X2# -> Int64X2#
f x y z = minInt64X2# x (plusInt64X2# y z)

{-# NOINLINE g #-}
g :: Int64X2# -> Int64X2# -> Int64X2# -> Int64X2#
g x y z = maxInt64X2# x (plusInt64X2# y z)

main :: IO ()
main = do
  let !x = packInt64X2# (# 1#Int64, 10#Int64 #)
      !y = packInt64X2# (# 4#Int64, 2#Int64 #)
      !z = broadcastInt64X2# 5#Int64
      !w = f x y z
      (# w0, w1 #) = unpackInt64X2# w
      !v = g x y z
      (# v0, v1 #) = unpackInt64X2# v
  print (I64# w0, I64# w1)
  print (I64# v0, I64# v1)
