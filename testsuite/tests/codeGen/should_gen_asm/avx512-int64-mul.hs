{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Exts
import GHC.Int

{-# NOINLINE f #-}
f :: Int64X2# -> Int64X2# -> Int64X2# -> Int64X2#
f x y z = timesInt64X2# x (plusInt64X2# y z)

main :: IO ()
main = do
  let !x = packInt64X2# (# 1#Int64, 3#Int64 #)
      !y = packInt64X2# (# 4#Int64, 2#Int64 #)
      !z = broadcastInt64X2# 5#Int64
      !w = f x y z
      (# w0, w1 #) = unpackInt64X2# w
  print (I64# w0, I64# w1)
