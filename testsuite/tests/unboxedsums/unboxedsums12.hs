{-# LANGUAGE UnboxedSums, MagicHash, UnboxedTuples #-}

module Lib where

import GHC.Prim

data B = B1 Int# Int# Int# Int# Int# | B2 Float#

type UbxB = (# (# Int#, Int#, Int#, Int#, Int# #) | Float# #)

{-# INLINE bToSum #-}
bToSum :: B -> UbxB
bToSum (B1 i1 i2 i3 i4 i5) = (# (# i1, i2, i3, i4, i5 #) | #)
bToSum (B2 f) = (# | f #)

data C = C UbxB UbxB UbxB

mkC :: B -> C
mkC b = C (bToSum b) (bToSum b) (bToSum b)
