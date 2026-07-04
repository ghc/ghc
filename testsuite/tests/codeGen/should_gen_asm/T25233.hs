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

myClearBit32 :: Word32# -> Int# -> Word32#
myClearBit32 x i =
  x `andWord32#` notWord32# (wordToWord32# 1## `uncheckedShiftLWord32#` i)

mySetBit32 :: Word32# -> Int# -> Word32#
mySetBit32 x i = x `orWord32#` (wordToWord32# 1## `uncheckedShiftLWord32#` i)

myComplementBit32 :: Word32# -> Int# -> Word32#
myComplementBit32 x i =
  x `xorWord32#` (wordToWord32# 1## `uncheckedShiftLWord32#` i)

-- With a constant bit index >= 32, the mask constant-folds to a literal
-- that does not fit in an imm32, so a bit-test instruction with an
-- immediate offset is used.
myClearBit40 :: Word# -> Word#
myClearBit40 x = x `and#` not# (1## `uncheckedShiftL#` 40#)

mySetBit40 :: Word# -> Word#
mySetBit40 x = x `or#` (1## `uncheckedShiftL#` 40#)

myComplementBit40 :: Word# -> Word#
myComplementBit40 x = x `xor#` (1## `uncheckedShiftL#` 40#)
