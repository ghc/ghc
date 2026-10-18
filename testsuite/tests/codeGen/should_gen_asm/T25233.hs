{-# LANGUAGE MagicHash #-}

-- Check that clearing/setting/complementing a single, variable bit
-- uses the btr/bts/btc instructions (#25233).
module T25233 where

import GHC.Exts

-- CHECK-LABEL: T25233.myClearBit_info:
-- CHECK:       btrq
myClearBit :: Word# -> Int# -> Word#
myClearBit x i = x `and#` not# (1## `uncheckedShiftL#` i)

-- CHECK-LABEL: T25233.mySetBit_info:
-- CHECK:       btsq
mySetBit :: Word# -> Int# -> Word#
mySetBit x i = x `or#` (1## `uncheckedShiftL#` i)

-- CHECK-LABEL: T25233.myComplementBit_info:
-- CHECK:       btcq
myComplementBit :: Word# -> Int# -> Word#
myComplementBit x i = x `xor#` (1## `uncheckedShiftL#` i)

-- CHECK-LABEL: T25233.myClearBit32_info:
-- CHECK:       btrl
myClearBit32 :: Word32# -> Int# -> Word32#
myClearBit32 x i =
  x `andWord32#` notWord32# (wordToWord32# 1## `uncheckedShiftLWord32#` i)

-- CHECK-LABEL: T25233.mySetBit32_info:
-- CHECK:       btsl
mySetBit32 :: Word32# -> Int# -> Word32#
mySetBit32 x i = x `orWord32#` (wordToWord32# 1## `uncheckedShiftLWord32#` i)

-- CHECK-LABEL: T25233.myComplementBit32_info:
-- CHECK:       btcl
myComplementBit32 :: Word32# -> Int# -> Word32#
myComplementBit32 x i =
  x `xorWord32#` (wordToWord32# 1## `uncheckedShiftLWord32#` i)

-- With a constant bit index >= 32, the mask constant-folds to a literal
-- that does not fit in an imm32, so a bit-test instruction with an
-- immediate offset is used.
-- CHECK-LABEL: T25233.myClearBit40_info:
-- CHECK:       btrq $40,
myClearBit40 :: Word# -> Word#
myClearBit40 x = x `and#` not# (1## `uncheckedShiftL#` 40#)

-- CHECK-LABEL: T25233.mySetBit40_info:
-- CHECK:       btsq $40,
mySetBit40 :: Word# -> Word#
mySetBit40 x = x `or#` (1## `uncheckedShiftL#` 40#)

-- CHECK-LABEL: T25233.myComplementBit40_info:
-- CHECK:       btcq $40,
myComplementBit40 :: Word# -> Word#
myComplementBit40 x = x `xor#` (1## `uncheckedShiftL#` 40#)
