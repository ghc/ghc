{-# LANGUAGE MagicHash #-}

-- Check that clearing/setting/complementing a single, variable bit
-- uses the btr/bts/btc instructions (#25233).
module T25233 where

import GHC.Exts

myClearBit :: Word# -> Int# -> Word#
myClearBit x i = x `and#` not# (1## `uncheckedShiftL#` i)

mySetBit :: Word# -> Int# -> Word#
mySetBit x i = x `or#` (1## `uncheckedShiftL#` i)

myComplementBit :: Word# -> Int# -> Word#
myComplementBit x i = x `xor#` (1## `uncheckedShiftL#` i)
