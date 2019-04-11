{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module CopyArray
  ( smallCopy
  ) where

import GHC.Exts
import GHC.IO

data ByteArray = ByteArray ByteArray#

-- Does an 8 byte copy with sub-word (2 bytes) alignment
-- Should be unrolled into 4 aligned stores (MOVWs)
smallCopy :: ByteArray -> IO ByteArray
smallCopy (ByteArray ba) = IO $ \s0 -> case newByteArray# 8# s0 of
  (# s1, mut #) -> case copyByteArray# ba 2# mut 0# 8# s1 of
    s2 -> case unsafeFreezeByteArray# mut s2 of
          (# s3, frozen #) -> (# s3, ByteArray frozen #)
