{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

module T20376 where

import GHC.Exts
import GHC.Word
import Data.Bits

foo0 (# #) = popCnt#   123456789##
foo1 (# #) = popCnt8#         89##
foo2 (# #) = popCnt16#     56789##
foo3 (# #) = popCnt32# 123456789##

foo0' = popCount (123456789 :: Word)
foo1' = popCount (       89 :: Word8)
foo2' = popCount (    56789 :: Word16)
foo3' = popCount (123456789 :: Word32)
foo4' = popCount (123456789123456789 :: Word64)

ctz0 (# #) = ctz#   0xC0000000##
ctz1 (# #) = ctz8#        0xC0##
ctz2 (# #) = ctz16#     0xC000##
ctz3 (# #) = ctz32# 0xC0000000##

ctz0' = countTrailingZeros (0xC0000000 :: Word)
ctz1' = countTrailingZeros (      0xC0 :: Word8)
ctz2' = countTrailingZeros (    0xC000 :: Word16)
ctz3' = countTrailingZeros (0xC0000000 :: Word32)
ctz4' = countTrailingZeros (0xC000000000000000 :: Word64)

clz1 (# #) = clz8#        0x04##
clz2 (# #) = clz16#     0x0004##
clz3 (# #) = clz32# 0x00000004##

clz1' = countLeadingZeros (      0x04 :: Word8)
clz2' = countLeadingZeros (    0x0004 :: Word16)
clz3' = countLeadingZeros (0x00000004 :: Word32)
clz4' = countLeadingZeros (0x0000000000000004 :: Word64)
