{-# LANGUAGE CPP, MagicHash #-}
module T25718c where

#include "MachDeps.h"

import GHC.Exts

-- Test that value range analysis handles all conversion/narrow/arithmetic primops.
-- Each section tests a different op from `valueRange` in GHC.Core.Opt.Range.
--
-- Already tested in T25718/T25718b: Word8ToWordOp, WordAndOp.
-- Here we test the remaining ops:
--
-- (1)  Narrow8IntOp  (narrow8Int#):   result in Int8  range [-128,  127]
-- (2)  Narrow16IntOp (narrow16Int#):  result in Int16 range [-32768,32767]
-- (3)  Narrow8WordOp  (narrow8Word#): result in Word8  range [0, 255]
-- (4)  Narrow16WordOp (narrow16Word#):result in Word16 range [0, 65535]
-- (5)  Word16ToWordOp (word16ToWord#):result in Word16 range [0, 65535]
-- (6)  WordToWord8Op  (wordToWord8#): result in Word8  range [0, 255]
-- (7)  WordToWord16Op (wordToWord16#):result in Word16 range [0, 65535]
-- (8)  Word8ToInt8Op  (word8ToInt8#): rangeCastFrom -> Int8  range [-128, 127]
-- (9)  Word16ToInt16Op(word16ToInt16#):rangeCastFrom -> Int16 range [-32768,32767]
-- (10) Int8ToWord8Op  (int8ToWord8#): rangeCastFrom -> Word8  range [0, 255]
-- (11) Int16ToWord16Op(int16ToWord16#):rangeCastFrom -> Word16 range [0, 65535]
-- (12) Word8AddOp (plusWord8#): rangeCastFrom fallback -> Word8 range [0, 255]
-- (13) VRA1 for Word16 (unreachable alt removal)
-- (14) VRA1 for narrow8Int# (unreachable alt removal)
-- (15) Word8SubOp  (subWord8#):  result in Word8  range [0, 255]
-- (16) Word16AddOp (plusWord16#):result in Word16 range [0, 65535]
-- (17) Word16SubOp (subWord16#): result in Word16 range [0, 65535]
-- (18) Int8AddOp  (plusInt8#):   result in Int8  range [-128, 127]
-- (19) Int8SubOp  (subInt8#):    result in Int8  range [-128, 127]
-- (20) Int16AddOp (plusInt16#):  result in Int16 range [-32768,32767]
-- (21) Int16SubOp (subInt16#):   result in Int16 range [-32768,32767]
-- (22) Narrow32IntOp (narrow32Int#):  result in Int32 range [-2147483648,2147483647]
-- (23) Word32ToInt32Op (word32ToInt32#): rangeCastFrom -> Int32 range [-2147483648,2147483647]
-- (24) Int32ToWord32Op (int32ToWord32#): rangeCastFrom -> Word32 range [0,4294967295]
-- (25) Int32AddOp (plusInt32#):  result in Int32 range [-2147483648,2147483647]
-- (26) Int32SubOp (subInt32#):   result in Int32 range [-2147483648,2147483647]
-- (27) Word8AndOp  (andWord8#):  rangeAnd -> [0, mask]
-- (28) Word16AndOp (andWord16#): rangeAnd -> [0, mask]
-- (29) Word32AndOp (andWord32#): rangeAnd -> [0, mask]
-- (30) Word64AndOp (and64#):     rangeAnd -> [0, mask]
-- (31) IntAndOp    (andI#):      rangeAnd -> [0, mask]
-- Sections (32)-(38) require 64-bit literals; guarded with CPP:
-- (32) Narrow32WordOp (narrow32Word#): on 64-bit, result in Word32 range [0,4294967295]
-- (33) Word32ToWordOp (word32ToWord#): on 64-bit, result in Word32 range [0,4294967295]
-- (34) WordToWord32Op (wordToWord32#): on 64-bit, composed chain in [0,4294967295]
-- (35) Word32AddOp (plusWord32#): on 64-bit, result in Word32 range [0,4294967295]
-- (36) Word32SubOp (subWord32#):  on 64-bit, result in Word32 range [0,4294967295]
-- (37) VRA1 for narrow32Int# (unreachable alts > 2147483647)
-- (38) VRA1 for word32ToWord# (unreachable alts > 4294967295)

-- (1) Narrow8IntOp: narrow8Int# x is always in [-128, 127]
narrow8_ge_lb :: Int# -> Bool
narrow8_ge_lb x = isTrue# (narrow8Int# x >=# -128#)     -- True

narrow8_le_ub :: Int# -> Bool
narrow8_le_ub x = isTrue# (narrow8Int# x <=# 127#)      -- True

narrow8_gt_ub_false :: Int# -> Bool
narrow8_gt_ub_false x = isTrue# (narrow8Int# x >=# 128#) -- False

-- (2) Narrow16IntOp: narrow16Int# x is always in [-32768, 32767]
narrow16_ge_lb :: Int# -> Bool
narrow16_ge_lb x = isTrue# (narrow16Int# x >=# -32768#)  -- True

narrow16_le_ub :: Int# -> Bool
narrow16_le_ub x = isTrue# (narrow16Int# x <=# 32767#)   -- True

-- (3) Narrow8WordOp: narrow8Word# x is always in [0, 255]
narrow8w_lt_ub :: Word# -> Bool
narrow8w_lt_ub x = isTrue# (narrow8Word# x `ltWord#` 256##)   -- True

-- (4) Narrow16WordOp: narrow16Word# x is always in [0, 65535]
narrow16w_lt_ub :: Word# -> Bool
narrow16w_lt_ub x = isTrue# (narrow16Word# x `ltWord#` 65536##) -- True

-- (5) Word16ToWordOp: word16ToWord# x is always in [0, 65535]
word16_lt_ub :: Word16# -> Bool
word16_lt_ub x = isTrue# (word16ToWord# x `ltWord#` 65536##)  -- True

word16_ge_ub_false :: Word16# -> Bool
word16_ge_ub_false x = isTrue# (word16ToWord# x `geWord#` 65536##) -- False

-- (6) WordToWord8Op: word8ToWord# (wordToWord8# x) is in [0, 255]
word_to_word8_lt :: Word# -> Bool
word_to_word8_lt x = isTrue# (word8ToWord# (wordToWord8# x) `ltWord#` 256##) -- True

-- (7) WordToWord16Op: word16ToWord# (wordToWord16# x) is in [0, 65535]
word_to_word16_lt :: Word# -> Bool
word_to_word16_lt x = isTrue# (word16ToWord# (wordToWord16# x) `ltWord#` 65536##) -- True

-- (8) Word8ToInt8Op: word8ToInt8# x has range Int8 [-128, 127] (via rangeCastFrom)
word8_to_int8_ge :: Word8# -> Bool
word8_to_int8_ge x = isTrue# (geInt8# (word8ToInt8# x) (intToInt8# -128#)) -- True

-- (9) Word16ToInt16Op: word16ToInt16# x has range Int16 [-32768, 32767]
word16_to_int16_ge :: Word16# -> Bool
word16_to_int16_ge x = isTrue# (geInt16# (word16ToInt16# x) (intToInt16# -32768#)) -- True

-- (10) Int8ToWord8Op: int8ToWord8# x has range Word8 [0, 255] (via rangeCastFrom)
int8_to_word8_le :: Int8# -> Bool
int8_to_word8_le x = isTrue# (leWord8# (int8ToWord8# x) (wordToWord8# 255##)) -- True

-- (11) Int16ToWord16Op: int16ToWord16# x has range Word16 [0, 65535]
int16_to_word16_le :: Int16# -> Bool
int16_to_word16_le x = isTrue# (leWord16# (int16ToWord16# x) (wordToWord16# 65535##)) -- True

-- (12) Word8AddOp: plusWord8# result stays in [0, 255] (rangeCastFrom fallback)
word8_add_lt_256 :: Word8# -> Word8# -> Bool
word8_add_lt_256 x y = isTrue# (word8ToWord# (plusWord8# x y) `ltWord#` 256##) -- True

-- (13) VRA1: unreachable alts for Word16 range [0, 65535]
word16_alts :: Word16# -> Bool
word16_alts x = case word16ToWord# x of
  100000## -> False   -- unreachable: 100000 > 65535
  65536##  -> False   -- unreachable: 65536 > 65535
  42##     -> False   -- reachable
  _        -> True

-- (14) VRA1: unreachable alts for narrow8Int# range [-128, 127]
narrow8_alts :: Int# -> Bool
narrow8_alts x = case narrow8Int# x of
  200# -> False   -- unreachable: 200 > 127
  128# -> False   -- unreachable: 128 > 127
  42#  -> False   -- reachable
  _    -> True

-- (15) Word8SubOp: subWord8# result stays in [0, 255]
word8_sub_lt_256 :: Word8# -> Word8# -> Bool
word8_sub_lt_256 x y = isTrue# (word8ToWord# (subWord8# x y) `ltWord#` 256##) -- True

-- (16) Word16AddOp: plusWord16# result stays in [0, 65535]
word16_add_lt_ub :: Word16# -> Word16# -> Bool
word16_add_lt_ub x y = isTrue# (word16ToWord# (plusWord16# x y) `ltWord#` 65536##) -- True

-- (17) Word16SubOp: subWord16# result stays in [0, 65535]
word16_sub_lt_ub :: Word16# -> Word16# -> Bool
word16_sub_lt_ub x y = isTrue# (word16ToWord# (subWord16# x y) `ltWord#` 65536##) -- True

-- (18) Int8AddOp: plusInt8# result stays in [-128, 127]
int8_add_ge_lb :: Int8# -> Int8# -> Bool
int8_add_ge_lb x y = isTrue# (geInt8# (plusInt8# x y) (intToInt8# -128#)) -- True

-- (19) Int8SubOp: subInt8# result stays in [-128, 127]
int8_sub_le_ub :: Int8# -> Int8# -> Bool
int8_sub_le_ub x y = isTrue# (leInt8# (subInt8# x y) (intToInt8# 127#)) -- True

-- (20) Int16AddOp: plusInt16# result stays in [-32768, 32767]
int16_add_ge_lb :: Int16# -> Int16# -> Bool
int16_add_ge_lb x y = isTrue# (geInt16# (plusInt16# x y) (intToInt16# -32768#)) -- True

-- (21) Int16SubOp: subInt16# result stays in [-32768, 32767]
int16_sub_le_ub :: Int16# -> Int16# -> Bool
int16_sub_le_ub x y = isTrue# (leInt16# (subInt16# x y) (intToInt16# 32767#)) -- True

-- (22) Narrow32IntOp: narrow32Int# result stays in [-2147483648, 2147483647]
-- (literals fit on 32-bit: -2147483648 = minInt32, 2147483647 = maxInt32)
narrow32_ge_lb :: Int# -> Bool
narrow32_ge_lb x = isTrue# (narrow32Int# x >=# -2147483648#) -- True

narrow32_le_ub :: Int# -> Bool
narrow32_le_ub x = isTrue# (narrow32Int# x <=# 2147483647#) -- True

-- (23) Word32ToInt32Op: word32ToInt32# result has range Int32 [-2147483648, 2147483647]
word32_to_int32_ge :: Word32# -> Bool
word32_to_int32_ge x = isTrue# (geInt32# (word32ToInt32# x) (intToInt32# -2147483648#)) -- True

-- (24) Int32ToWord32Op: int32ToWord32# result has range Word32 [0, 4294967295]
-- (4294967295## = maxWord32 = maxWord on 32-bit, valid literal on any platform)
int32_to_word32_le :: Int32# -> Bool
int32_to_word32_le x = isTrue# (leWord32# (int32ToWord32# x) (wordToWord32# 4294967295##)) -- True

-- (25) Int32AddOp: plusInt32# result stays in [-2147483648, 2147483647]
int32_add_ge_lb :: Int32# -> Int32# -> Bool
int32_add_ge_lb x y = isTrue# (geInt32# (plusInt32# x y) (intToInt32# -2147483648#)) -- True

-- (26) Int32SubOp: subInt32# result stays in [-2147483648, 2147483647]
int32_sub_le_ub :: Int32# -> Int32# -> Bool
int32_sub_le_ub x y = isTrue# (leInt32# (subInt32# x y) (intToInt32# 2147483647#)) -- True

-- (27) Word8AndOp: andWord8# with constant mask gives bounded range [0, mask]
word8_and_lt :: Word8# -> Bool
word8_and_lt x = isTrue# (word8ToWord# (andWord8# x (wordToWord8# 0xF##)) `ltWord#` 16##) -- True

word8_and_alts :: Word8# -> Bool
word8_and_alts x = case word8ToWord# (andWord8# x (wordToWord8# 0xF##)) of
  100## -> False   -- unreachable: 100 > 15
  20##  -> False   -- unreachable: 20 > 15
  5##   -> False   -- reachable
  _     -> True

-- (28) Word16AndOp: andWord16# with constant mask gives bounded range [0, mask]
word16_and_lt :: Word16# -> Bool
word16_and_lt x = isTrue# (word16ToWord# (andWord16# x (wordToWord16# 0xFF##)) `ltWord#` 256##) -- True

-- (29) Word32AndOp: andWord32# with constant mask gives bounded range [0, mask]
-- (65536## = 2^16, valid on any platform as Word# literal)
word32_and_lt :: Word32# -> Bool
word32_and_lt x = isTrue# (word32ToWord# (andWord32# x (wordToWord32# 0xFFFF##)) `ltWord#` 65536##) -- True

-- (30) Word64AndOp (and64#): with constant mask gives bounded range [0, mask]
word64_and_le :: Word64# -> Bool
word64_and_le x = isTrue# (leWord64# (and64# x (wordToWord64# 0xFFFF##)) (wordToWord64# 65535##)) -- True

-- (31) IntAndOp (andI#): with non-negative constant mask gives range [0, mask]
int_and_ge :: Int# -> Bool
int_and_ge x = isTrue# (andI# x 0xFF# >=# 0#) -- True

int_and_alts :: Int# -> Bool
int_and_alts x = case andI# x 0xFF# of
  256# -> False   -- unreachable: 256 > 255
  42#  -> False   -- reachable
  _    -> True

#if WORD_SIZE_IN_BITS >= 64
-- On 64-bit platforms, Word# is 64-bit so 4294967296## (= 2^32) is a valid literal
-- and Word32 ops produce a proper sub-range of Word#.

-- (32) Narrow32WordOp: narrow32Word# result in [0, 4294967295] on 64-bit
narrow32w_lt_ub :: Word# -> Bool
narrow32w_lt_ub x = isTrue# (narrow32Word# x `ltWord#` 4294967296##) -- True

-- (33) Word32ToWordOp: word32ToWord# result in [0, 4294967295] on 64-bit
word32_lt_ub :: Word32# -> Bool
word32_lt_ub x = isTrue# (word32ToWord# x `ltWord#` 4294967296##) -- True

word32_ge_ub_false :: Word32# -> Bool
word32_ge_ub_false x = isTrue# (word32ToWord# x `geWord#` 4294967296##) -- False

-- (34) WordToWord32Op: word32ToWord# (wordToWord32# x) in [0, 4294967295] on 64-bit
word_to_word32_lt :: Word# -> Bool
word_to_word32_lt x = isTrue# (word32ToWord# (wordToWord32# x) `ltWord#` 4294967296##) -- True

-- (35) Word32AddOp: plusWord32# result in [0, 4294967295] on 64-bit
word32_add_lt_ub :: Word32# -> Word32# -> Bool
word32_add_lt_ub x y = isTrue# (word32ToWord# (plusWord32# x y) `ltWord#` 4294967296##) -- True

-- (36) Word32SubOp: subWord32# result in [0, 4294967295] on 64-bit
word32_sub_lt_ub :: Word32# -> Word32# -> Bool
word32_sub_lt_ub x y = isTrue# (word32ToWord# (subWord32# x y) `ltWord#` 4294967296##) -- True

-- (37) VRA1 for narrow32Int# (unreachable alts outside [-2147483648, 2147483647])
-- (literals 2147483648# and 3000000000# overflow Int# on 32-bit, so guarded by CPP)
narrow32_alts :: Int# -> Bool
narrow32_alts x = case narrow32Int# x of
  3000000000# -> False   -- unreachable: 3000000000 > 2147483647
  2147483648# -> False   -- unreachable: 2147483648 > 2147483647
  42#         -> False   -- reachable
  _           -> True

-- (38) VRA1 for word32ToWord# (unreachable alts outside [0, 4294967295])
-- (literals 4294967296## etc. overflow Word# on 32-bit, so guarded by CPP)
word32_alts :: Word32# -> Bool
word32_alts x = case word32ToWord# x of
  5000000000## -> False   -- unreachable: 5000000000 > 4294967295
  4294967296## -> False   -- unreachable: 4294967296 > 4294967295
  42##         -> False   -- reachable
  _            -> True
#endif
