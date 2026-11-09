{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
-- Stores whose address and value code interfere on i386. The x86 NCG emits
-- the address code of a store before its value code, so that the CFG
-- records the blocks that either code makes in the right order (see
-- Note [Updating the CFG during CodeGen] in GHC.CmmToAsm.Monad). Thus the
-- registers of the address must survive the value code.
--
-- * writeQuot: the quotient is fixed in eax, and the conversion of the
--   value to Int64 on i386 writes eax and edx (cltd). The amode must not
--   refer to eax.
--
-- * writeShift: both the address and the value hold a 64-bit shift by a
--   variable amount, which the i386 NCG turns into branching code. Both
--   splits must be recorded in the CFG in emission order (-dasm-lint).
--
-- Found by review of the fix for #27694.
module Main where

import GHC.Exts
import GHC.IO
import GHC.Int
import GHC.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

writeQuot :: Addr# -> Int# -> Int# -> Int# -> IO ()
writeQuot p i j k = IO $ \s ->
  case writeInt64OffAddr# p (quotInt# i j) (intToInt64# k) s of
    s' -> (# s', () #)
{-# NOINLINE writeQuot #-}

writeShift :: Addr# -> Word64# -> Int# -> Word64# -> Int# -> IO ()
writeShift p x a y b = IO $ \s ->
  case writeWord32OffAddr#
         (plusAddr# p (word2Int# (word64ToWord# (uncheckedShiftRL64# x a))))
         0#
         (wordToWord32# (word64ToWord# (uncheckedShiftL64# y b)))
         s of
    s' -> (# s', () #)
{-# NOINLINE writeShift #-}

main :: IO ()
main = allocaBytes 64 $ \ptr@(Ptr p) -> do
  fillBytes ptr 0xAA 64
  -- 13 `quot` 3 = 4: the store goes to byte 32. A clobbered amode would
  -- use the value (7) as the index and store to byte 56 instead.
  writeQuot p 13# 3# 7#
  v <- peekByteOff ptr 32 :: IO Int64
  print v
  -- lo(2^36 >> 32) = 16: the store goes to byte 16, value 0x1234 << 4.
  -- The Word64 literals are boxed: a Word# literal has 32 bits on i386.
  let !(W64# x) = 68719476736
      !(W64# y) = 4660
  writeShift p x 32# y 4#
  w <- peekByteOff ptr 16 :: IO Word32
  print w
