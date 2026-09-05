{-# LANGUAGE MagicHash #-}

-- Check that testing a single, variable bit uses the bt instruction
-- (#27688).
module T27688 where

import GHC.Exts

testBitNe :: Word# -> Int# -> Int#
testBitNe x i =
  if isTrue# ((x `and#` (1## `uncheckedShiftL#` i)) `neWord#` 0##)
  then 1# else 2#

testBitEq :: Word# -> Int# -> Int#
testBitEq x i =
  if isTrue# ((x `and#` (1## `uncheckedShiftL#` i)) `eqWord#` 0##)
  then 1# else 2#

testBit32 :: Word32# -> Int# -> Int#
testBit32 x i =
  if isTrue# ((x `andWord32#` (wordToWord32# 1## `uncheckedShiftLWord32#` i))
                `neWord32#` wordToWord32# 0##)
  then 1# else 2#

-- With a constant bit index >= 32, the mask constant-folds to a literal
-- that does not fit in an imm32, so bt with an immediate offset is used.
testBit40 :: Word# -> Int#
testBit40 x =
  if isTrue# ((x `and#` (1## `uncheckedShiftL#` 40#)) `neWord#` 0##)
  then 1# else 2#
