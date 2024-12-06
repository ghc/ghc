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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GHC.Internal.Bignum.WordArray where

import GHC.Prim
import GHC.Magic
import GHC.Types
import GHC.Internal.Bignum.Primitives

#include "MachDeps.h"
#include "WordSize.h"

default ()

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
      io s =
         case newWordArray# sz s of { (# s, mwa #) ->
         case act mwa s          of { s ->
         unsafeFreezeByteArray# mwa s
         }}

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
      io s =
         case newWordArray# sz1 s of { (# s, mwa1 #) ->
         case newWordArray# sz2 s of { (# s, mwa2 #) ->
         case act mwa1 mwa2 s     of { s ->
         case unsafeFreezeByteArray# mwa1 s of { (# s, wa1 #) ->
         case unsafeFreezeByteArray# mwa2 s of { (# s, wa2 #) ->
            (# s, (# wa1, wa2 #) #)
         }}}}}

-- | Create a new WordArray#
newWordArray# :: Int# -> State# s -> (# State# s, MutableWordArray# s #)
newWordArray# sz s = newByteArray# (wordsToBytes# sz) s

-- | Create a new WordArray# of the given size (*in Word#*), apply the action to
-- it, trim its most significant zeroes, then return it frozen
withNewWordArrayTrimmed#
   :: Int#  -- ^ Size in Word
   -> (MutableWordArray# RealWorld -> State# RealWorld -> State# RealWorld)
   -> WordArray#
withNewWordArrayTrimmed# sz act = withNewWordArray# sz \mwa s ->
   case act mwa s of
      s' -> mwaTrimZeroes# mwa s'

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
withNewWordArray2Trimmed# sz1 sz2 act = withNewWordArray2# sz1 sz2 \mwa1 mwa2 s ->
   case act mwa1 mwa2 s of
      s' -> case mwaTrimZeroes# mwa1 s' of
         s'' -> mwaTrimZeroes# mwa2 s''

-- | Create a new WordArray# of the given size (*in Word#*), apply the action to
-- it. If the action returns true#, trim its most significant zeroes, then
-- return it frozen. Otherwise, return ().
withNewWordArrayTrimmedMaybe#
   :: Int#  -- ^ Size in Word
   -> (MutableWordArray# RealWorld -> State# RealWorld -> (# State# RealWorld, Bool# #))
   -> (# (# #) | WordArray# #)
withNewWordArrayTrimmedMaybe# sz act = case runRW# io of (# _, a #) -> a
   where
      io s =
         case newWordArray# sz s of
            (# s, mwa #) -> case act mwa s of
               (# s, 0# #) -> (# s, (# (# #) | #) #)
               (# s, _  #) -> case mwaTrimZeroes# mwa s of
                  s -> case unsafeFreezeByteArray# mwa s of
                     (# s, ba #) -> (# s, (# | ba #) #)

-- | Create a WordArray# from two Word#
--
-- `wordArrayFromWord2# h l
--    where h is the most significant word
--          l is the least significant word
wordArrayFromWord2# :: Word# -> Word# -> WordArray#
wordArrayFromWord2# h l   =
   withNewWordArray# 2# \mwa s ->
      case mwaWrite# mwa 0# l s of
         s -> mwaWrite# mwa 1# h s

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
mwaSize# mba s = case getSizeofMutableByteArray# mba s of
   (# s2, sz #) -> (# s2, bytesToWords# sz #)

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
mwaShrink# _mwa 0# s = s
mwaShrink# mwa  i  s =
   case mwaSize# mwa s of
      (# s, n #) -> shrinkMutableByteArray# mwa (wordsToBytes# (n -# i)) s

-- | Set size
mwaSetSize# :: MutableByteArray# s -> Int# -> State# s -> State# s
mwaSetSize# mwa n s = shrinkMutableByteArray# mwa (wordsToBytes# n) s

-- | Copy the WordArray into the MWA and shrink the size of MWA to the one of
-- the WordArray
mwaInitCopyShrink# :: MutableByteArray# s -> WordArray# -> State# s -> State# s
mwaInitCopyShrink# mwa wa s =
   case mwaArrayCopy# mwa 0# wa 0# (wordArraySize# wa) s of
      s -> mwaSetSize# mwa (wordArraySize# wa) s

-- | Trim ending zeroes
mwaTrimZeroes# :: MutableByteArray# s -> State# s -> State# s
mwaTrimZeroes# mwa s1 =
   case mwaClz mwa s1 of
      (# s2, 0# #) -> s2
      (# s2, c  #) -> mwaShrink# mwa c s2

-- | Count leading zero Words
mwaClz :: MutableWordArray# s -> State# s -> (# State# s, Int# #)
mwaClz mwa s1 = case mwaSize# mwa s1 of
   (# s2,sz #)  -> mwaClzAt mwa (sz -# 1#) s2

-- | Count leading zero Words starting at given position
mwaClzAt :: MutableWordArray# s -> Int# -> State# s -> (# State# s, Int# #)
mwaClzAt mwa = go 0#
   where
      go c i s
         | isTrue# (i <# 0#) = (# s, c #)
         | True = case readWordArray# mwa i s of
            (# s', 0## #) -> go (c +# 1#) (i -# 1#) s'
            (# s', _   #) -> (# s', c #)

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
      go i carry s
         | isTrue# (i ># sz)  = s
         | isTrue# (i ==# sz) = mwaWriteOrShrink mwa carry i s
         | 0## <- carry       = -- copy higher remaining words and shrink the mwa
                                case mwaArrayCopy# mwa i wa i (sz -# i) s of
                                    s2 -> mwaShrink# mwa 1# s2
         | True               = let !(# l,c #) = addWordC# (indexWordArray# wa i) carry
                                in case mwaWrite# mwa i l s of
                                    s2 -> go (i +# 1#) (int2Word# c) s2

-- | Write the most-significant Word:
--    * if it is 0: shrink the array of 1 Word
--    * otherwise: write it
mwaWriteOrShrink :: MutableWordArray# s -> Word# -> Int# -> State# s -> State# s
mwaWriteOrShrink mwa 0## _i s = mwaShrink# mwa 1# s
mwaWriteOrShrink mwa  w   i s = mwaWrite# mwa i w s

-- | Compute the index of the most-significant Word and write it.
mwaWriteMostSignificant :: MutableWordArray# s -> Word# -> State# s -> State# s
mwaWriteMostSignificant mwa w s =
   case mwaSize# mwa s of
      (# s', sz #) -> mwaWriteOrShrink mwa w (sz -# 1#) s'

-- | MutableWordArray <- zipWith op wa1 wa2
--
-- Required output: Size(MutableWordArray) = min Size(wa1) Size(wa2)
mwaInitArrayBinOp :: MutableWordArray# s -> WordArray# -> WordArray# -> (Word# -> Word# -> Word#) -> State# s -> State#s
mwaInitArrayBinOp mwa wa wb op s = go 0# s
   where
      !sz = minI# (wordArraySize# wa) (wordArraySize# wb)
      go i s'
         | isTrue# (i ==# sz) = s'
         | True =
            case indexWordArray# wa i `op` indexWordArray# wb i of
               v -> case mwaWrite# mwa i v s' of
                  s'' -> go (i +# 1#) s''

-- | Write an element of the MutableWordArray
mwaWrite# :: MutableWordArray# s -> Int# -> Word# -> State# s -> State# s
mwaWrite# = writeWordArray#

-- | Fill some part of a MutableWordArray with the given Word#
mwaFill# :: MutableWordArray# s -> Word# -> Word# -> Word# -> State# s -> State# s
mwaFill# _   _ _   0## s = s
mwaFill# mwa v off n   s = case mwaWrite# mwa (word2Int# off) v s of
   s' -> mwaFill# mwa v (off `plusWord#` 1##) (n `minusWord#` 1##) s'

-- | Add Word# inplace (a the specified offset) in the mwa with carry propagation.
mwaAddInplaceWord# :: MutableWordArray# d -> Int# -> Word# -> State# d -> State# d
mwaAddInplaceWord#   _ _ 0## s = s
mwaAddInplaceWord# mwa i y   s = case readWordArray# mwa i s of
   (# s1, x #) -> let !(# h,l #) = plusWord2# x y
                  in case mwaWrite# mwa i l s1 of
                        s2 -> mwaAddInplaceWord# mwa (i +# 1#) h s2

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
mwaSubInplaceWord# mwa ii iw s1 = case mwaSize# mwa s1 of
   (# is, sz #) ->
      let
         go _ 0## s = (# s, 1# #) -- no underflow
         go i y   s
            | isTrue# (i >=# sz) = (# s, 0# #) -- underflow
            | True = case readWordArray# mwa i s of
               (# s1, x #) -> let !(# l,h #) = subWordC# x y
                  in case mwaWrite# mwa i l s1 of
                     s2 -> go (i +# 1#) (int2Word# h) s2
      in go ii iw is


-- | Trim `a` of `k` less significant limbs and then compare the result with `b`
--
-- "mwa" doesn't need to be trimmed
mwaTrimCompare :: Int# -> MutableWordArray# s -> WordArray# -> State# s -> (# State# s, Ordering #)
mwaTrimCompare k mwa wb s1
   | (# s, szA #) <- mwaSize# mwa s1
   , szB <- wordArraySize# wb
   =
     let
      go i s
         | isTrue# (i <# 0#) = (# s, EQ #)
         | True = case readWordArray# mwa (i +# k) s of
            (# s2, ai #) ->
               let bi = if isTrue# (i >=# szB)
                           then 0##
                           else indexWordArray# wb i
               in if | isTrue# (ai `gtWord#` bi) -> (# s2, GT #)
                     | isTrue# (bi `gtWord#` ai) -> (# s2, LT #)
                     | True                      -> go (i -# 1#) s2

      szTrimA = szA -# k

     in if | isTrue# (szTrimA <# szB) -> (# s, LT #)
           | True                     -> go (szA -# k -# 1#) s


-- | Sub array inplace (at the specified offset) in the mwa with carry propagation.
--
-- We don't trim the resulting array!
--
-- Return False# on underflow.
mwaSubInplaceArray :: MutableWordArray# d -> Int# -> WordArray# -> State# d -> (# State# d, Bool# #)
mwaSubInplaceArray mwa off wb = go (wordArraySize# wb -# 1#)
   where
      go i s
         | isTrue# (i <# 0#) = (# s, 1# #) -- no underflow
         | True
         = case mwaSubInplaceWord# mwa (off +# i) (indexWordArray# wb i) s of
            (# s2, 1# #) -> go (i -# 1#) s2
            (# s2, _  #) -> (# s2, 0# #) -- underflow

-- | Add array inplace (a the specified offset) in the mwa with carry propagation.
--
-- Upper bound of the result mutable aray is not checked against overflow.
mwaAddInplaceArray :: MutableWordArray# d -> Int# -> WordArray# -> State# d -> State# d
mwaAddInplaceArray mwa off wb = go 0# 0##
   where
      !maxi = wordArraySize# wb
      go i c s
         | isTrue# (i ==# maxi) = mwaAddInplaceWord# mwa (i +# off) c s
         | True
         = case readWordArray# mwa (i +# off) s of
            (# s, v #) -> case plusWord3# v (indexWordArray# wb i) c of
               (# c', v' #) -> case writeWordArray# mwa (i +# off) v' s of
                  s -> go (i +# 1#) c' s

-- | Sub array inplace (at the specified offset) in the mwa with carry propagation.
--
-- We don't trim the resulting array!
--
-- Return False# on underflow.
mwaSubInplaceMutableArray :: MutableWordArray# d -> Int# -> MutableWordArray# d -> State# d -> (# State# d, Bool# #)
mwaSubInplaceMutableArray mwa off mwb s0 =
   case mwaSize# mwb s0 of
      (# s1, szB #) -> go (szB -# 1#) s1
   where
      go i s
         | isTrue# (i <# 0#) = (# s, 1# #) -- no underflow
         | True
         = case readWordArray# mwb i s of
            (# s1, bi #) -> case mwaSubInplaceWord# mwa (off +# i) bi s1 of
               (# s2, 1# #) -> go (i -# 1#) s2
               (# s2, _  #) -> (# s2, 0# #) -- underflow

-- | Sub an array inplace and then trim zeroes
--
-- Don't check overflow. The caller must ensure that a>=b
mwaSubInplaceArrayTrim :: MutableWordArray# d -> Int# -> WordArray# -> State# d -> State# d
mwaSubInplaceArrayTrim mwa off wb s =
   case mwaSubInplaceArray mwa off wb s of
      (# s', _ #) -> mwaTrimZeroes# mwa s'


-- | Read an indexed Word in the MutableWordArray. If the index is out-of-bound,
-- return zero.
mwaReadOrZero :: MutableWordArray# s -> Int# -> State# s  -> (# State# s, Word# #)
mwaReadOrZero mwa i s = case mwaSize# mwa s of
   (# s2, sz #)
      | isTrue# (i >=# sz) -> (# s2, 0## #)
      | isTrue# (i <# 0#)  -> (# s2, 0## #)
      | True               -> readWordArray# mwa i s2

mwaRead# :: MutableWordArray# s -> Int# -> State# s -> (# State# s, Word# #)
mwaRead# = readWordArray#
