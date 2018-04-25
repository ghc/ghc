{-# LANGUAGE MagicHash, UnboxedTuples #-}
import GHC.Prim
import GHC.IO

main = IO $ \s ->
  let (# s1, p0 #) = newByteArray# 10# s
      (# s2, p #) = unsafeFreezeByteArray# p0 s1
      (# s3, q #) = newByteArray# 10# s2
  in (# copyByteArray# p 0# q 0# 10# s, () #)
