{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module GHC.Num.Primitives
   (
   -- * Bool#
   Bool#
   , (&&#)
   , (||#)
   , notB#
   -- * Int#
   , testBitI#
   , minI#
   , maxI#
   , sgnI#
   , absI#
   , cmpI#
   , intEncodeDouble#
   , popCntI#
   -- * Word#
   , andNot#
   , cmpW#
   , bitW#
   , maxW#
   , minW#
   , testBitW#
   , shiftRW#
   , plusWord3#
   , plusWord12#
   , quotRemWord3#
   , wordFromAbsInt#
   , wordLog2#
   , wordLogBase#
   , wordSizeInBase#
   , wordIsPowerOf2#
   , wordEncodeDouble#
   , wordReverseBits#
   , wordReverseBits32#
   , wordReverseBytes#
   -- ** Addr import/export
   , wordFromAddr#
   , wordFromAddrLE#
   , wordFromAddrBE#
   , wordToAddr#
   , wordToAddrLE#
   , wordToAddrBE#
   , wordWriteAddrLE#
   , wordWriteAddrBE#
   -- ** ByteArray import/export
   , wordFromByteArray#
   , wordFromByteArrayLE#
   , wordFromByteArrayBE#
   , wordToMutableByteArray#
   , wordToMutableByteArrayLE#
   , wordToMutableByteArrayBE#
   , wordWriteMutableByteArrayLE#
   , wordWriteMutableByteArrayBE#
   -- * Exception
   , raiseUnderflow
   , raiseUnderflow_Word#
   , raiseDivZero
   , raiseDivZero_Word#
   , unexpectedValue
   , unexpectedValue_Int#
   , unexpectedValue_Word#
   -- * IO
   , ioWord#
   , ioInt#
   , ioVoid
   , ioBool
   )
where

#include "MachDeps.h"
#include "WordSize.h"

-- Required for WORDS_BIGENDIAN
#include <ghcautoconf.h>

import GHC.Prim.Exception

import GHC.Prim
import GHC.Types

default ()

----------------------------------
-- Bool#
----------------------------------

type Bool# = Int#

(&&#) :: Bool# -> Bool# -> Bool#
(&&#) = andI#

(||#) :: Bool# -> Bool# -> Bool#
(||#) = orI#

notB# :: Bool# -> Bool#
notB# x = x `xorI#` 1#

infixr 3  &&#
infixr 2  ||#


----------------------------------
-- Int#
----------------------------------

-- | Branchless `abs`
absI# :: Int# -> Int#
absI# i# = (i# `xorI#` nsign) -# nsign
  where
    -- nsign = negateInt# (i# <# 0#)
    nsign = uncheckedIShiftRA# i# (WORD_SIZE_IN_BITS# -# 1#)

-- | Branchless `signum`
sgnI# :: Int# -> Int#
sgnI# x# = (x# ># 0#) -# (x# <# 0#)

-- | Population count
popCntI# :: Int# -> Word#
popCntI# i = popCnt# (int2Word# i)

-- | Branchless comparison
cmpI# :: Int# -> Int# -> Int#
cmpI# x# y# = (x# ># y#) -# (x# <# y#)

testBitI# :: Int# -> Word# -> Bool#
testBitI# x i = ((uncheckedIShiftL# 1# (word2Int# i)) `andI#` x) /=# 0#

minI# :: Int# -> Int# -> Int#
minI# x y | isTrue# (x <=# y) = x
          | True              = y

maxI# :: Int# -> Int# -> Int#
maxI# x y | isTrue# (x >=# y) = x
          | True              = y

-- | Encode (# Int# mantissa, Int# exponent #) into a Double#.
--
-- (provided by GHC's RTS)
foreign import ccall unsafe "__int_encodeDouble"
   intEncodeDouble# :: Int# -> Int# -> Double#

----------------------------------
-- Word#
----------------------------------

andNot# :: Word# -> Word# -> Word#
andNot# x y = x `and#` (not# y)

cmpW# :: Word# -> Word# -> Ordering
{-# INLINE cmpW# #-}
cmpW# x# y#
  | isTrue# (x# `ltWord#` y#) = LT
  | isTrue# (x# `eqWord#` y#) = EQ
  | True                      = GT

-- | Return the absolute value of the Int# in a Word#
wordFromAbsInt# :: Int# -> Word#
wordFromAbsInt# i
   | isTrue# (i >=# 0#) = int2Word# i
   | True               = int2Word# (negateInt# i)

minW# :: Word# -> Word# -> Word#
minW# x# y# | isTrue# (x# `leWord#` y#) = x#
            | True                      = y#

maxW# :: Word# -> Word# -> Word#
maxW# x# y# | isTrue# (x# `gtWord#` y#) = x#
            | True                      = y#

bitW# :: Int# -> Word#
bitW# k = 1## `uncheckedShiftL#` k

testBitW# :: Word# -> Word# -> Bool#
testBitW# w k = w `and#` (1## `uncheckedShiftL#` word2Int# k) `neWord#` 0##

-- | Safe right shift for Word#
shiftRW# :: Word# -> Word# -> Word#
shiftRW# a b
   | isTrue# (b `geWord#` WORD_SIZE_IN_BITS##) = 0##
   | True                                      = a `uncheckedShiftRL#` (word2Int# b)

-- | (h,l) <- a + (hb,lb)
plusWord12# :: Word# -> (# Word#,Word# #) -> (# Word#,Word# #)
{-# INLINABLE plusWord12# #-}
plusWord12# a0 (# b1,b0 #) = (# m1, m0 #)
   where
      !(# t, m0 #) = plusWord2# a0 b0
      !m1          = plusWord# t b1

-- | Add 3 values together
plusWord3# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
{-# INLINABLE plusWord3# #-}
plusWord3# a b c = (# r1, r0 #)
   where
      !(# t1, t0 #) = plusWord2# a b
      !(# t2, r0 #) = plusWord2# t0 c
      !r1           = plusWord# t1 t2


-- | 2-by-1 large division
--
-- Requires:
--    b0 /= 0
--    a1 >= b0 (not required, but if not q1=0)
quotRemWord3# :: (# Word#,Word# #) -> Word# -> (# (# Word#,Word# #),Word# #)
quotRemWord3# (# a1,a0 #) b0 = (# (# q1, q0 #), r0 #)
   where
      !(# q1, r' #) = quotRemWord# a1 b0
      !(# q0, r0 #) = quotRemWord2# r' a0 b0



-- | Encode (# Word# mantissa, Int# exponent #) into a Double#.
--
-- (provided by GHC's RTS)
foreign import ccall unsafe "__word_encodeDouble"
   wordEncodeDouble# :: Word# -> Int# -> Double#

-- | Compute base-2 log of 'Word#'
--
-- This is internally implemented as count-leading-zeros machine instruction.
wordLog2# :: Word# -> Word#
wordLog2# w   = (WORD_SIZE_IN_BITS## `minusWord#` 1##) `minusWord#` (clz# w)

-- | Logarithm for an arbitrary base
wordLogBase# :: Word# -> Word# -> Word#
wordLogBase# base a
   | isTrue# (base `leWord#` 1##)
   = unexpectedValue_Word# (# #)

   | 2## <- base
   = wordLog2# a

   | True
   = case go base of (# _, e' #) -> e'
   where
      goSqr pw = case timesWord2# pw pw of
         (# 0##, l #) -> go l
         (# _  , _ #) -> (# a, 0## #)
      go pw = if isTrue# (a `ltWord#` pw)
         then (# a, 0## #)
         else case goSqr pw of
            (# q, e #) -> if isTrue# (q `ltWord#` pw)
               then (# q, 2## `timesWord#` e #)
               else (# q `quotWord#` pw
                    , 2## `timesWord#` e `plusWord#` 1## #)

wordSizeInBase# :: Word# -> Word# -> Word#
wordSizeInBase# _    0## = 0##
wordSizeInBase# base w   = 1## `plusWord#` wordLogBase# base w

-- | Indicate if the value is a power of two and which one
wordIsPowerOf2# :: Word# -> (# (# #) | Word# #)
wordIsPowerOf2# w
   | isTrue# (popCnt# w `neWord#` 1##) = (# (# #) | #)
   | True                              = (# | ctz# w #)

-- | Reverse bytes in a Word#
wordReverseBytes# :: Word# -> Word#
wordReverseBytes# x0 = r
   where
#if WORD_SIZE_IN_BITS == 64
      x1 = ((x0 `and#` 0x00FF00FF00FF00FF##) `uncheckedShiftL#`  8#) `or#` ((x0 `and#` 0xFF00FF00FF00FF00##) `uncheckedShiftRL#`  8#)
      x2 = ((x1 `and#` 0x0000FFFF0000FFFF##) `uncheckedShiftL#` 16#) `or#` ((x1 `and#` 0xFFFF0000FFFF0000##) `uncheckedShiftRL#` 16#)
      r  = ((x2 `and#` 0x00000000FFFFFFFF##) `uncheckedShiftL#` 32#) `or#` ((x2 `and#` 0xFFFFFFFF00000000##) `uncheckedShiftRL#` 32#)
#else
      x1 = ((x0 `and#` 0x00FF00FF##) `uncheckedShiftL#`  8#) `or#` ((x0 `and#` 0xFF00FF00##) `uncheckedShiftRL#`  8#)
      r  = ((x1 `and#` 0x0000FFFF##) `uncheckedShiftL#` 16#) `or#` ((x1 `and#` 0xFFFF0000##) `uncheckedShiftRL#` 16#)
#endif


-- | Reverse bits in a Word#
wordReverseBits# :: Word# -> Word#
wordReverseBits# x0 = r
   where
#if WORD_SIZE_IN_BITS == 64
      x1 = ((x0 `and#` 0x5555555555555555##) `uncheckedShiftL#`  1#) `or#` ((x0 `and#` 0xAAAAAAAAAAAAAAAA##) `uncheckedShiftRL#`  1#)
      x2 = ((x1 `and#` 0x3333333333333333##) `uncheckedShiftL#`  2#) `or#` ((x1 `and#` 0xCCCCCCCCCCCCCCCC##) `uncheckedShiftRL#`  2#)
      x3 = ((x2 `and#` 0x0F0F0F0F0F0F0F0F##) `uncheckedShiftL#`  4#) `or#` ((x2 `and#` 0xF0F0F0F0F0F0F0F0##) `uncheckedShiftRL#`  4#)
      x4 = ((x3 `and#` 0x00FF00FF00FF00FF##) `uncheckedShiftL#`  8#) `or#` ((x3 `and#` 0xFF00FF00FF00FF00##) `uncheckedShiftRL#`  8#)
      x5 = ((x4 `and#` 0x0000FFFF0000FFFF##) `uncheckedShiftL#` 16#) `or#` ((x4 `and#` 0xFFFF0000FFFF0000##) `uncheckedShiftRL#` 16#)
      r  = ((x5 `and#` 0x00000000FFFFFFFF##) `uncheckedShiftL#` 32#) `or#` ((x5 `and#` 0xFFFFFFFF00000000##) `uncheckedShiftRL#` 32#)
#else
      x1 = ((x0 `and#` 0x55555555##) `uncheckedShiftL#`  1#) `or#` ((x0 `and#` 0xAAAAAAAA##) `uncheckedShiftRL#`  1#)
      x2 = ((x1 `and#` 0x33333333##) `uncheckedShiftL#`  2#) `or#` ((x1 `and#` 0xCCCCCCCC##) `uncheckedShiftRL#`  2#)
      x3 = ((x2 `and#` 0x0F0F0F0F##) `uncheckedShiftL#`  4#) `or#` ((x2 `and#` 0xF0F0F0F0##) `uncheckedShiftRL#`  4#)
      x4 = ((x3 `and#` 0x00FF00FF##) `uncheckedShiftL#`  8#) `or#` ((x3 `and#` 0xFF00FF00##) `uncheckedShiftRL#`  8#)
      r  = ((x4 `and#` 0x0000FFFF##) `uncheckedShiftL#` 16#) `or#` ((x4 `and#` 0xFFFF0000##) `uncheckedShiftRL#` 16#)
#endif

-- | Reverse bits in the Word32 subwords composing a Word#
wordReverseBits32# :: Word# -> Word#
#if WORD_SIZE_IN_BITS == 64
wordReverseBits32# x0 = r
   where
      x1 = ((x0 `and#` 0x5555555555555555##) `uncheckedShiftL#`  1#) `or#` ((x0 `and#` 0xAAAAAAAAAAAAAAAA##) `uncheckedShiftRL#`  1#)
      x2 = ((x1 `and#` 0x3333333333333333##) `uncheckedShiftL#`  2#) `or#` ((x1 `and#` 0xCCCCCCCCCCCCCCCC##) `uncheckedShiftRL#`  2#)
      x3 = ((x2 `and#` 0x0F0F0F0F0F0F0F0F##) `uncheckedShiftL#`  4#) `or#` ((x2 `and#` 0xF0F0F0F0F0F0F0F0##) `uncheckedShiftRL#`  4#)
      x4 = ((x3 `and#` 0x00FF00FF00FF00FF##) `uncheckedShiftL#`  8#) `or#` ((x3 `and#` 0xFF00FF00FF00FF00##) `uncheckedShiftRL#`  8#)
      r  = ((x4 `and#` 0x0000FFFF0000FFFF##) `uncheckedShiftL#` 16#) `or#` ((x4 `and#` 0xFFFF0000FFFF0000##) `uncheckedShiftRL#` 16#)
#else
wordReverseBits32# x0 = wordReverseBits# x0
#endif


-- | Write a Word to @/addr/@ in base-256 little-endian representation and
-- return the number of bytes written.
wordToAddrLE# :: Word# -> Addr# -> State# s -> (# State# s, Word# #)
wordToAddrLE# x addr = go x 0#
   where
      go w c s
         | 0## <- w
         = (# s, int2Word# c #)

         | True
         = case writeWord8OffAddr# addr c (wordToWord8# w) s of
            s' -> go (w `uncheckedShiftRL#` 8#) (c +# 1#) s'

-- | Write a Word to @/addr/@ in base-256 big-endian representation and
-- return the number of bytes written.
wordToAddrBE# :: Word# -> Addr# -> State# s -> (# State# s, Word# #)
wordToAddrBE# w addr = go 0# (WORD_SIZE_IN_BITS# -# clz)
   where
     !clz = word2Int# (clz# w `and#` (not# 0b0111##)) -- keep complete bytes

     go c sh s
      | 0# <- sh
      = (# s, int2Word# c #)

      | True
      , w' <- wordToWord8# (w `uncheckedShiftRL#` (sh -# 8#))
      = case writeWord8OffAddr# addr c w' s of
         s' -> go (c +# 1#) (sh -# 8#) s'

-- | Write a Word to @/addr/@ in base-256 representation and
-- return the number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
wordToAddr# :: Word# -> Addr# -> Bool# -> State# s -> (# State# s, Word# #)
wordToAddr# a addr 0# s = wordToAddrLE# a addr s
wordToAddr# a addr _  s = wordToAddrBE# a addr s


-- | Read a Word from @/addr/@ in base-256 little-endian representation.
--
-- @'n' is the number of bytes to read.
wordFromAddrLE# :: Word# -> Addr# -> State# s -> (# State# s, Word# #)
wordFromAddrLE# n addr s
   -- Optimize when we read a full word
   | WORD_SIZE_IN_BYTES## <- n
   = case readWordOffAddr# addr 0# s of
#if defined(WORDS_BIGENDIAN)
      (# s', w #) -> (# s', wordReverseBytes# w #)
#else
      (# s', w #) -> (# s', w #)
#endif

wordFromAddrLE# n addr s0 = go 0## 0# s0
   where
      go w c s
         | isTrue# (c ==# word2Int# n)
         = (# s, w #)

         | True
         = case readWord8OffAddr# addr c s of
            (# s', b #) -> go (w `or#` (word8ToWord# b `uncheckedShiftL#` (c `uncheckedIShiftL#` 3#)))
                              (c +# 1#)
                              s'

-- | Read a Word from @/addr/@ in base-256 big-endian representation.
--
-- @'n' is the number of bytes to read.
wordFromAddrBE# :: Word# -> Addr# -> State# s -> (# State# s, Word# #)
wordFromAddrBE# n addr s
   -- Optimize when we read a full word
   | WORD_SIZE_IN_BYTES## <- n
   = case readWordOffAddr# addr 0# s of
#if defined(WORDS_BIGENDIAN)
      (# s', w #) -> (# s', w #)
#else
      (# s', w #) -> (# s', wordReverseBytes# w #)
#endif

wordFromAddrBE# n addr s0 = go 0## 0# s0
   where
      go w c s
         | isTrue# (c ==# word2Int# n)
         = (# s, w #)

         | True
         = case readWord8OffAddr# addr c s of
            (# s', b #) -> go ((w `uncheckedShiftL#` 8#) `or#` word8ToWord# b)
                              (c +# 1#)
                              s'

-- | Read a Word from @/addr/@ in base-256 representation.
--
-- @'n' is the number of bytes to read.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
wordFromAddr# :: Word# -> Addr# -> Bool# -> State# s -> (# State# s, Word# #)
wordFromAddr# a addr 0# s = wordFromAddrLE# a addr s
wordFromAddr# a addr _  s = wordFromAddrBE# a addr s



-- | Write a full word with little-endian encoding
wordWriteAddrLE# :: Word# -> Addr# -> State# s -> State# s
wordWriteAddrLE# w addr = writeWordOffAddr# addr 0#
#if defined(WORDS_BIGENDIAN)
   (wordReverseBytes# w)
#else
   w
#endif

-- | Write a full word with little-endian encoding
wordWriteAddrBE# :: Word# -> Addr# -> State# s -> State# s
wordWriteAddrBE# w addr = writeWordOffAddr# addr 0#
#if defined(WORDS_BIGENDIAN)
   w
#else
   (wordReverseBytes# w)
#endif

-- | Write a Word to @/MutableByteArray/@ in base-256 little-endian
-- representation and return the number of bytes written.
--
-- The offset is in bytes.
wordToMutableByteArrayLE# :: Word# -> MutableByteArray# s -> Word# -> State# s -> (# State# s, Word# #)
wordToMutableByteArrayLE# x mba off = go x 0#
   where
      go w c s
         | 0## <- w
         = (# s, int2Word# c #)

         | True
         = case writeWord8Array# mba (word2Int# off +# c) (wordToWord8# w) s of
            s' -> go (w `uncheckedShiftRL#` 8#) (c +# 1#) s'

-- | Write a Word to @/MutableByteArray/@ in base-256 big-endian representation and
-- return the number of bytes written.
--
-- The offset is in bytes.
wordToMutableByteArrayBE# :: Word# -> MutableByteArray# s -> Word# -> State# s -> (# State# s, Word# #)
wordToMutableByteArrayBE# w mba off = go 0# (WORD_SIZE_IN_BITS# -# clz)
   where
     !clz = word2Int# (clz# w `and#` (not# 0b0111##)) -- keep complete bytes

     go c sh s
      | 0# <- sh
      = (# s, int2Word# c #)

      | True
      , w' <- wordToWord8# (w `uncheckedShiftRL#` (sh -# 8#))
      = case writeWord8Array# mba (word2Int# off +# c) w' s of
         s' -> go (c +# 1#) (sh -# 8#) s'

-- | Write a Word to @/MutableByteArray/@ in base-256 representation and
-- return the number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- The offset is in bytes.
wordToMutableByteArray# :: Word# -> MutableByteArray# s -> Word# -> Bool# -> State# s -> (# State# s, Word# #)
wordToMutableByteArray# a mba off 0# s = wordToMutableByteArrayLE# a mba off s
wordToMutableByteArray# a mba off _  s = wordToMutableByteArrayBE# a mba off s

-- | Write a full word with little-endian encoding
wordWriteMutableByteArrayLE# :: Word# -> MutableByteArray# s -> Word# -> State# s -> State# s
wordWriteMutableByteArrayLE# w mba off = writeWord8ArrayAsWord# mba (word2Int# off)
#if defined(WORDS_BIGENDIAN)
   (wordReverseBytes# w)
#else
   w
#endif

-- | Write a full word with little-endian encoding
wordWriteMutableByteArrayBE# :: Word# -> MutableByteArray# s -> Word# -> State# s -> State# s
wordWriteMutableByteArrayBE# w mba off = writeWord8ArrayAsWord# mba (word2Int# off)
#if defined(WORDS_BIGENDIAN)
   w
#else
   (wordReverseBytes# w)
#endif

-- | Read a Word from @/ByteArray/@ in base-256 little-endian representation.
--
-- @'n' is the number of bytes to read.
wordFromByteArrayLE# :: Word# -> ByteArray# -> Word# -> Word#
wordFromByteArrayLE# n ba off =
   case n of
      -- Optimize when we read a full word
      WORD_SIZE_IN_BYTES## -> case indexWord8ArrayAsWord# ba (word2Int# off) of
#if defined(WORDS_BIGENDIAN)
         w -> wordReverseBytes# w
#else
         w -> w
#endif

      _ -> let
            go w c
               | isTrue# (c ==# word2Int# n)
               = w

               | True
               = case indexWord8Array# ba (word2Int# off +# c) of
                  b -> go (w `or#` (word8ToWord# b `uncheckedShiftL#` (c `uncheckedIShiftL#` 3#)))
                          (c +# 1#)
           in go 0## 0#

-- | Read a Word from @/ByteArray/@ in base-256 big-endian representation.
--
-- @'n' is the number of bytes to read.
wordFromByteArrayBE# :: Word# -> ByteArray# -> Word# -> Word#
wordFromByteArrayBE# n ba off
   -- Optimize when we read a full word
   | WORD_SIZE_IN_BYTES## <- n
   = case indexWord8ArrayAsWord# ba (word2Int# off) of
#if defined(WORDS_BIGENDIAN)
      w -> w
#else
      w -> wordReverseBytes# w
#endif

wordFromByteArrayBE# n ba off = go 0## 0#
   where
      go w c
         | isTrue# (c ==# word2Int# n)
         = w

         | True
         = case indexWord8Array# ba (word2Int# off +# c) of
            b -> go ((w `uncheckedShiftL#` 8#) `or#` word8ToWord# b) (c +# 1#)

-- | Read a Word from @/ByteArray/@ in base-256 representation.
--
-- @'n' is the number of bytes to read.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
wordFromByteArray# :: Word# -> ByteArray# -> Word# -> Bool# -> Word#
wordFromByteArray# a ba off 0# = wordFromByteArrayLE# a ba off
wordFromByteArray# a ba off _  = wordFromByteArrayBE# a ba off

----------------------------------
-- IO
----------------------------------

ioVoid :: IO a -> State# RealWorld -> State# RealWorld
ioVoid (IO io) s = case io s of
                  (# s', _ #) -> s'

ioWord# :: IO Word -> State# RealWorld -> (# State# RealWorld, Word# #)
ioWord# (IO io) s = case io s of
   (# s', W# w #) -> (# s', w #)

ioInt# :: IO Int -> State# RealWorld -> (# State# RealWorld, Int# #)
ioInt# (IO io) s = case io s of
   (# s', I# i #) -> (# s', i #)

ioBool :: IO Bool -> State# RealWorld -> (# State# RealWorld, Bool# #)
ioBool (IO io) s = case io s of
   (# s', False #) -> (# s', 0# #)
   (# s', True #) -> (# s', 1# #)


----------------------------------
-- Exception
----------------------------------

-- Note [ghc-bignum exceptions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- `ghc-bignum` package can't depend on `base` package (it would create a cyclic
-- dependency). Hence it can't import "Control.Exception" and throw exceptions
-- the usual way. Instead it uses some wired-in functions from `ghc-prim` which
-- themselves call wired-in functions from the RTS: raiseOverflow,
-- raiseUnderflow, raiseDivZero.
--
-- We have to be careful when we want to throw an exception instead of returning
-- an unlifted value (e.g. Word#, unboxed tuple, etc.). We have to ensure the
-- evaluation of the exception throwing function before returning a dummy value,
-- otherwise it will be removed by the simplifier as dead-code.
--
--    foo :: ... -> Word#
--    foo = ... case raiseDivZero of
--                !_ -> 0## -- the bang-pattern is necessary!
--                          -- 0## is a dummy value (unreachable code)
--

unexpectedValue_Int# :: (# #) -> Int#
unexpectedValue_Int# _ = case unexpectedValue of
   !_ -> 0# -- see Note [ghc-bignum exceptions]

unexpectedValue_Word# :: (# #) -> Word#
unexpectedValue_Word# _ = case unexpectedValue of
   !_ -> 0## -- see Note [ghc-bignum exceptions]

raiseDivZero_Word# :: (# #) -> Word#
raiseDivZero_Word# _ = case raiseDivZero of
   !_ -> 0## -- see Note [ghc-bignum exceptions]

raiseUnderflow_Word# :: (# #) -> Word#
raiseUnderflow_Word# _ = case raiseUnderflow of
   !_ -> 0## -- see Note [ghc-bignum exceptions]


unexpectedValue :: a
unexpectedValue = raiseOverflow
