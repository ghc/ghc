{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Exts
import GHC.Prim
import GHC.Word

{-# NOINLINE f #-}
f :: Word64X2# -> Word64X2# -> Word64X2# -> Word64X2#
f x y z = minWord64X2# x (plusWord64X2# y z)

{-# NOINLINE g #-}
g :: Word64X2# -> Word64X2# -> Word64X2# -> Word64X2#
g x y z = maxWord64X2# x (plusWord64X2# y z)

main :: IO ()
main = do
  let !x = packWord64X2# (# 1#Word64, 10#Word64 #)
      !y = packWord64X2# (# 4#Word64, 2#Word64 #)
      !z = broadcastWord64X2# 5#Word64
      !w = f x y z
      (# w0, w1 #) = unpackWord64X2# w
      !v = g x y z
      (# v0, v1 #) = unpackWord64X2# v
  print (W64# w0, W64# w1)
  print (W64# v0, W64# v1)
