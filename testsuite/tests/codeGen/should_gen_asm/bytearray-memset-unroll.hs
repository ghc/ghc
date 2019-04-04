{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module FillArray
  ( fill
  ) where

import GHC.Exts
import GHC.IO

data ByteArray = ByteArray ByteArray#

fill :: IO ByteArray
fill = IO $ \s0 -> case newByteArray# 24# s0 of
  (# s1, m #) -> case setByteArray# m 0# 23# 1# s1 of
    s2 -> case unsafeFreezeByteArray# m s2 of
          (# s3, r #) -> (# s3, ByteArray r #)
