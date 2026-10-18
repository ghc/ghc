{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module FillArray
  ( fill
  ) where

import GHC.Exts
import GHC.IO

data ByteArray = ByteArray ByteArray#

-- A memset of known small size is unrolled into stores, not called:
-- CHECK:      movq $72340172838076673,[[FILL:%r[a-z0-9]+]]
-- CHECK-NEXT: movq [[FILL]],0([[ARR:%r[a-z0-9]+]])
-- CHECK-NEXT: movq [[FILL]],8([[ARR]])
-- CHECK-NEXT: movl $16843009,16([[ARR]])
-- CHECK-NEXT: movw $257,20([[ARR]])
-- CHECK-NEXT: movb $1,22([[ARR]])
-- CHECK-NOT:  call{{.*}}memset
fill :: IO ByteArray
fill = IO $ \s0 -> case newByteArray# 24# s0 of
  (# s1, m #) -> case setByteArray# m 0# 23# 1# s1 of
    s2 -> case unsafeFreezeByteArray# m s2 of
          (# s3, r #) -> (# s3, ByteArray r #)
