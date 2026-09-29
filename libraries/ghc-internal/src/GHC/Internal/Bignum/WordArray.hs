{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QualifiedDo #-}

module GHC.Internal.Bignum.WordArray where

import GHC.Internal.Prim
import GHC.Internal.Magic
import GHC.Internal.Types
import GHC.Internal.Bignum.Primitives
import qualified GHC.Internal.Unboxed.Do as Ubx

#include "MachDeps.h"
#include "WordSize.h"

-- | Unlifted array of Word
type WordArray#        = ByteArray#
type MutableWordArray# = MutableByteArray#

data WordArray          = WordArray WordArray#
data MutableWordArray s = MutableWordArray (MutableWordArray# s)

-- | Convert limb count into byte count
wordsToBytes# :: Int# -> Int#
wordsToBytes# i = i `uncheckedIShiftL#` WORD_SIZE_BYTES_SHIFT#

-- | Convert byte count into limb count
bytesToWords# :: Int# -> Int#
bytesToWords# i = i `uncheckedIShiftRL#` WORD_SIZE_BYTES_SHIFT#


-- | Create a new WordArray# of the given size (*in Word#*) and apply the
-- action to it before returning it frozen
withNewWordArray#
   :: Int#  -- ^ Size in Word
   -> (MutableWordArray# RealWorld -> State# RealWorld -> State# RealWorld)
   -> WordArray#
withNewWordArray# sz act = case runRW# io of (# _, a #) -> a
   where
      io = Ubx.do
         mwa <- newWordArray# sz
         act mwa
         unsafeFreezeByteArray# mwa

-- | Create two new WordArray# of the given sizes (*in Word#*) and apply the
-- action to them before returning them frozen
withNewWordArray2#
   :: Int# -- ^ Size in Word
   -> Int# -- ^ Ditto
   -> (MutableWordArray# RealWorld
      -> MutableWordArray# RealWorld
      -> State# RealWorld
      -> State# RealWorld)
   -> (# WordArray#, WordArray# #)
withNewWordArray2# sz1 sz2 act = case runRW# io of (# _, a #) -> a
   where
      io = Ubx.do
         mwa1 <- newWordArray# sz1
         mwa2 <- newWordArray# sz2
         act mwa1 mwa2
         wa1 <- unsafeFreezeByteArray# mwa1
         wa2 <- unsafeFreezeByteArray# mwa2
         Ubx.pure (# wa1, wa2 #)

-- | Create a new WordArray#
newWordArray# :: Int# -> State# s -> (# State# s, MutableWordArray# s #)
newWordArray# sz s = newByteArray# (wordsToBytes# sz) s

-- | Create a new WordArray# of the given size (*in Word#*), apply the action to
-- it, trim its most significant zeroes, then return it frozen
withNewWordArrayTrimmed#
   :: Int#  -- ^ Size in Word
   -> (MutableWordArray# RealWorld -> State# RealWorld -> State# RealWorld)
   -> WordArray#
withNewWordArrayTrimmed# sz act = withNewWordArray# sz \mwa -> Ubx.do
   act mwa
   mwaTrimZeroes# mwa

-- | Create two new WordArray# of the given sizes (*in Word#*), apply the action
-- to them, trim their most significant zeroes, then return them frozen
withNewWordArray2Trimmed#
   :: Int#  -- ^ Size in Word
   -> Int#  -- ^ Ditto
   -> (MutableWordArray# RealWorld
      -> MutableWordArray# RealWorld
      -> State# RealWorld
      -> State# RealWorld)
   -> (# WordArray#, WordArray# #)
withNewWordArray2Trimmed# sz1 sz2 act = withNewWordArray2# sz1 sz2 \mwa1 mwa2 -> Ubx.do
   act mwa1 mwa2
   mwaTrimZeroes# mwa1
   mwaTrimZeroes# mwa2

-- | Create a new WordArray# of the given size (*in Word#*), apply the action to
-- it. If the action returns true#, trim its most significant zeroes, then
-- return it frozen. Otherwise, return ().
withNewWordArrayTrimmedMaybe#
   :: Int#  -- ^ Size in Word
   -> (MutableWordArray# RealWorld -> State# RealWorld -> (# State# RealWorld, Bool# #))
   -> (# (# #) | WordArray# #)
withNewWordArrayTrimmedMaybe# sz act = case runRW# io of (# _, a #) -> a
   where
      io = Ubx.do
         mwa <- newWordArray# sz
         b <- act mwa
         case b of
            0# -> Ubx.pure (# (# #) | #)
            _  -> Ubx.do
               mwaTrimZeroes# mwa
               ba <- unsafeFreezeByteArray# mwa
               Ubx.pure (# | ba #)

-- | Create a WordArray# from two Word#
--
-- `wordArrayFromWord2# h l
--    where h is the most significant word
--          l is the least significant word
wordArrayFromWord2# :: Word# -> Word# -> WordArray#
wordArrayFromWord2# h l   =
   withNewWordArray# 2# \mwa -> Ubx.do
      mwaWrite# mwa 0# l
      mwaWrite# mwa 1# h

-- | Create a WordArray# from one Word#
wordArrayFromWord# :: Word# -> WordArray#
wordArrayFromWord# w   =
   withNewWordArray# 1# \mwa s ->
      mwaWrite# mwa 0# w s

-- | Word array size
wordArraySize# :: WordArray# -> Int#
wordArraySize# ba = bytesToWords# (sizeofByteArray# ba)


-- | Equality test for WordArray#

-- | Get size in Words
mwaSize# :: MutableWordArray# s-> State# s -> (# State# s, Int# #)
mwaSize# mba = Ubx.do
   sz <- getSizeofMutableByteArray# mba
   Ubx.pure (bytesToWords# sz)

-- | Get the last Word (must be non empty!)
wordArrayLast# :: WordArray# -> Word#
wordArrayLast# a = indexWordArray# a (wordArraySize# a -# 1#)

-- | Copy Words from a WordArray
--
-- Don't do anything if the number of words to copy is <= 0
mwaArrayCopy# :: MutableByteArray# s -> Int# -> WordArray# -> Int# -> Int# -> State# s -> State# s
mwaArrayCopy# dst dstIdx src srcIdx n s
   | isTrue# (n <=# 0#) = s
   | True = copyByteArray#
               src (wordsToBytes# srcIdx)
               dst (wordsToBytes# dstIdx)
               (wordsToBytes# n) s

-- | Shrink last words of a WordArray
mwaShrink# :: MutableByteArray# s -> Int# -> State# s -> State# s
mwaShrink# _mwa 0# = Ubx.nop
mwaShrink# mwa  i  = Ubx.do
   n <- mwaSize# mwa
   shrinkMutableByteArray# mwa (wordsToBytes# (n -# i))

-- | Set size
mwaSetSize# :: MutableByteArray# s -> Int# -> State# s -> State# s
mwaSetSize# mwa n s = shrinkMutableByteArray# mwa (wordsToBytes# n) s

-- | Copy the WordArray into the MWA and shrink the size of MWA to the one of
-- the WordArray
mwaInitCopyShrink# :: MutableByteArray# s -> WordArray# -> State# s -> State# s
mwaInitCopyShrink# mwa wa = Ubx.do
   mwaArrayCopy# mwa 0# wa 0# (wordArraySize# wa)
   mwaSetSize# mwa (wordArraySize# wa)

-- | Trim ending zeroes
mwaTrimZeroes# :: MutableByteArray# s -> State# s -> State# s
mwaTrimZeroes# mwa = Ubx.do
   c <- mwaClz mwa
   case c of
      0# -> Ubx.nop
      _  -> mwaShrink# mwa c

-- | Count leading zero Words
mwaClz :: MutableWordArray# s -> State# s -> (# State# s, Int# #)
mwaClz mwa = Ubx.do
   sz <- mwaSize# mwa
   mwaClzAt mwa (sz -# 1#)

-- | Count leading zero Words starting at given position
mwaClzAt :: MutableWordArray# s -> Int# -> State# s -> (# State# s, Int# #)
mwaClzAt mwa = go 0#
   where
      go c i
         | isTrue# (i <# 0#) = Ubx.pure c
         | True = Ubx.do
            w <- readWordArray# mwa i
            case w of
               0## -> go (c +# 1#) (i -# 1#)
               _   -> Ubx.pure c

-- | Count leading zero Words starting at given position
waClzAt :: WordArray# -> Int# -> Int#
waClzAt wa = go 0#
   where
      go c i
         | isTrue# (i <# 0#)
         = c

         | 0## <- indexWordArray# wa i
         = go (c +# 1#) (i -# 1#)

         | True
         = c

-- | Compare the most signiciant limbs of a and b. The comparison stops (i.e.
-- returns EQ) when there isn't enough lims in a or b to perform another
-- comparison.
wordArrayCompareMSWords :: WordArray# -> WordArray# -> Ordering
wordArrayCompareMSWords wa wb
   | 0# <- szA
   , 0# <- szB
   = EQ

   | 0# <- szA
   = LT

   | 0# <- szB
   = GT

   | True
   = go (szA -# 1#) (szB -# 1#)
   where
      szA  = wordArraySize# wa
      szB  = wordArraySize# wb

      go i j
         | isTrue# (i <# 0#) = EQ
         | isTrue# (j <# 0#) = EQ
         | True =
            let
               a = indexWordArray# wa i
               b = indexWordArray# wb j
            in if | isTrue# (a `gtWord#` b) -> GT
                  | isTrue# (b `gtWord#` a) -> LT
                  | True                    -> go (i -# 1#) (j -# 1#)


-- | Compute MutableWordArray <- WordArray + Word
--
-- The MutableWordArray may not be initialized and will be erased anyway.
--
-- Input: Size(MutableWordArray) = Size(WordArray) + 1
-- Output: Size(MutableWordArray) = Size(WordArray) [+ 1]
mwaInitArrayPlusWord :: MutableWordArray# s -> WordArray# -> Word# -> State# s -> State#s
mwaInitArrayPlusWord mwa wa = go 0#
   where
      sz = wordArraySize# wa
      go i carry
         | isTrue# (i ># sz)  = Ubx.nop
         | isTrue# (i ==# sz) = mwaWriteOrShrink mwa carry i
         | 0## <- carry       = Ubx.do -- copy higher remaining words and shrink the mwa
            mwaArrayCopy# mwa i wa i (sz -# i)
            mwaShrink# mwa 1#
         | True               = Ubx.do
            let !(# l, c #) = addWordC# (indexWordArray# wa i) carry
            mwaWrite# mwa i l
            go (i +# 1#) (int2Word# c)

-- | Write the most-significant Word:
--    * if it is 0: shrink the array of 1 Word
--    * otherwise: write it
mwaWriteOrShrink :: MutableWordArray# s -> Word# -> Int# -> State# s -> State# s
mwaWriteOrShrink mwa 0## _i s = mwaShrink# mwa 1# s
mwaWriteOrShrink mwa  w   i s = mwaWrite# mwa i w s

-- | Compute the index of the most-significant Word and write it.
mwaWriteMostSignificant :: MutableWordArray# s -> Word# -> State# s -> State# s
mwaWriteMostSignificant mwa w = Ubx.do
   sz <- mwaSize# mwa
   mwaWriteOrShrink mwa w (sz -# 1#)

-- | MutableWordArray <- zipWith op wa1 wa2
--
-- Required output: Size(MutableWordArray) = min Size(wa1) Size(wa2)
mwaInitArrayBinOp :: MutableWordArray# s -> WordArray# -> WordArray# -> (Word# -> Word# -> Word#) -> State# s -> State#s
mwaInitArrayBinOp mwa wa wb op = go 0#
   where
      !sz = minI# (wordArraySize# wa) (wordArraySize# wb)
      go i
         | isTrue# (i ==# sz) = Ubx.nop
         | True =
            case indexWordArray# wa i `op` indexWordArray# wb i of
               v -> Ubx.do
                  mwaWrite# mwa i v
                  go (i +# 1#)

-- | Write an element of the MutableWordArray
mwaWrite# :: MutableWordArray# s -> Int# -> Word# -> State# s -> State# s
mwaWrite# = writeWordArray#

-- | Fill some part of a MutableWordArray with the given Word#
mwaFill# :: MutableWordArray# s -> Word# -> Word# -> Word# -> State# s -> State# s
mwaFill# _   _ _   0## = Ubx.nop
mwaFill# mwa v off n   = Ubx.do
   mwaWrite# mwa (word2Int# off) v
   mwaFill# mwa v (off `plusWord#` 1##) (n `minusWord#` 1##)

-- | Add Word# inplace (a the specified offset) in the mwa with carry propagation.
mwaAddInplaceWord# :: MutableWordArray# d -> Int# -> Word# -> State# d -> State# d
mwaAddInplaceWord#   _ _ 0## = Ubx.nop
mwaAddInplaceWord# mwa i y   = Ubx.do
   x <- readWordArray# mwa i
   let !(# h, l #) = plusWord2# x y
   mwaWrite# mwa i l
   mwaAddInplaceWord# mwa (i +# 1#) h

-- | Sub Word# inplace (at the specified offset) in the mwa with carry
-- propagation.
--
-- Return False# on underflow
mwaSubInplaceWord#
   :: MutableWordArray# d
   -> Int#
   -> Word#
   -> State# d
   -> (# State# d, Bool# #)
mwaSubInplaceWord# mwa ii iw = Ubx.do
   sz <- mwaSize# mwa
   let
      go _ 0## = Ubx.pure 1# -- no underflow
      go i y
         | isTrue# (i >=# sz) = Ubx.pure 0# -- underflow
         | True = Ubx.do
            x <- readWordArray# mwa i
            let !(# l, h #) = subWordC# x y
            mwaWrite# mwa i l
            go (i +# 1#) (int2Word# h)
   go ii iw


-- | Trim `a` of `k` less significant limbs and then compare the result with `b`
--
-- "mwa" doesn't need to be trimmed
mwaTrimCompare :: Int# -> MutableWordArray# s -> WordArray# -> State# s -> (# State# s, Ordering #)
mwaTrimCompare k mwa wb = Ubx.do
   szA <- mwaSize# mwa
   let
      szB = wordArraySize# wb

      go i
         | isTrue# (i <# 0#) = Ubx.pure EQ
         | True = Ubx.do
            ai <- readWordArray# mwa (i +# k)
            let bi = if isTrue# (i >=# szB)
                        then 0##
                        else indexWordArray# wb i
            if | isTrue# (ai `gtWord#` bi) -> Ubx.pure GT
               | isTrue# (bi `gtWord#` ai) -> Ubx.pure LT
               | True                      -> go (i -# 1#)

      szTrimA = szA -# k

   if | isTrue# (szTrimA <# szB) -> Ubx.pure LT
      | True                     -> go (szA -# k -# 1#)


-- | Sub array inplace (at the specified offset) in the mwa with carry propagation.
--
-- We don't trim the resulting array!
--
-- Return False# on underflow.
mwaSubInplaceArray :: MutableWordArray# d -> Int# -> WordArray# -> State# d -> (# State# d, Bool# #)
mwaSubInplaceArray mwa off wb = go (wordArraySize# wb -# 1#)
   where
      go i
         | isTrue# (i <# 0#) = Ubx.pure 1# -- no underflow
         | True = Ubx.do
            b <- mwaSubInplaceWord# mwa (off +# i) (indexWordArray# wb i)
            case b of
               1# -> go (i -# 1#)
               _  -> Ubx.pure 0# -- underflow

-- | Add array inplace (a the specified offset) in the mwa with carry propagation.
--
-- Upper bound of the result mutable aray is not checked against overflow.
mwaAddInplaceArray :: MutableWordArray# d -> Int# -> WordArray# -> State# d -> State# d
mwaAddInplaceArray mwa off wb = go 0# 0##
   where
      !maxi = wordArraySize# wb
      go i c
         | isTrue# (i ==# maxi) = mwaAddInplaceWord# mwa (i +# off) c
         | True = Ubx.do
            v <- readWordArray# mwa (i +# off)
            let !(# c', v' #) = plusWord3# v (indexWordArray# wb i) c
            writeWordArray# mwa (i +# off) v'
            go (i +# 1#) c'

-- | Sub array inplace (at the specified offset) in the mwa with carry propagation.
--
-- We don't trim the resulting array!
--
-- Return False# on underflow.
mwaSubInplaceMutableArray :: MutableWordArray# d -> Int# -> MutableWordArray# d -> State# d -> (# State# d, Bool# #)
mwaSubInplaceMutableArray mwa off mwb = Ubx.do
   szB <- mwaSize# mwb
   go (szB -# 1#)
   where
      go i
         | isTrue# (i <# 0#) = Ubx.pure 1# -- no underflow
         | True = Ubx.do
            bi <- readWordArray# mwb i
            b <- mwaSubInplaceWord# mwa (off +# i) bi
            case b of
               1# -> go (i -# 1#)
               _  -> Ubx.pure 0# -- underflow

-- | Sub an array inplace and then trim zeroes
--
-- Don't check overflow. The caller must ensure that a>=b
mwaSubInplaceArrayTrim :: MutableWordArray# d -> Int# -> WordArray# -> State# d -> State# d
mwaSubInplaceArrayTrim mwa off wb = Ubx.do
   _ <- mwaSubInplaceArray mwa off wb
   mwaTrimZeroes# mwa


-- | Read an indexed Word in the MutableWordArray. If the index is out-of-bound,
-- return zero.
mwaReadOrZero :: MutableWordArray# s -> Int# -> State# s  -> (# State# s, Word# #)
mwaReadOrZero mwa i = Ubx.do
   sz <- mwaSize# mwa
   if | isTrue# (i >=# sz) -> Ubx.pure 0##
      | isTrue# (i <# 0#)  -> Ubx.pure 0##
      | True               -> readWordArray# mwa i

mwaRead# :: MutableWordArray# s -> Int# -> State# s -> (# State# s, Word# #)
mwaRead# = readWordArray#
