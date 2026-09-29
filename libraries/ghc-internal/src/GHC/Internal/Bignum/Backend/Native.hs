{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GHC.Internal.Bignum.Backend.Native where

#include "MachDeps.h"
#include "WordSize.h"

#if defined(BIGNUM_NATIVE)
import {-# SOURCE #-} GHC.Internal.Bignum.BigNat
import {-# SOURCE #-} GHC.Internal.Bignum.Natural
import {-# SOURCE #-} GHC.Internal.Bignum.Integer
#else
import GHC.Internal.Bignum.BigNat
import GHC.Internal.Bignum.Natural
import GHC.Internal.Bignum.Integer
#endif
import GHC.Internal.Bignum.WordArray
import GHC.Internal.Bignum.Primitives
import GHC.Internal.Prim
import GHC.Internal.Types
import qualified GHC.Internal.Unboxed.Do as Ubx

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
-- (This module uses the empty tuple () and string literals.)
import GHC.Internal.Tuple ()

-- | ghc-bignum backend name
backendName :: [Char]
backendName = "native"


count_words_bits :: Word# -> (# Word#, Word# #)
count_words_bits n = (# nw, nb #)
   where
      nw = n `uncheckedShiftRL#` WORD_SIZE_BITS_SHIFT#
      nb = n `and#` WORD_SIZE_BITS_MASK##

count_words_bits_int :: Word# -> (# Int#, Int# #)
count_words_bits_int n = case count_words_bits n of
   (# nw, nb #) -> (# word2Int# nw, word2Int# nb #)

-- | Compare two non-zero BigNat of the same length
--
-- Return:
--     < 0 ==> LT
--    == 0 ==> EQ
--     > 0 ==> GT
bignat_compare :: WordArray# -> WordArray# -> Int#
bignat_compare wa wb = go (sz -# 1#)
   where
      sz = wordArraySize# wa
      go i
         | isTrue# (i <# 0#) = 0#
         | a <- indexWordArray# wa i
         , b <- indexWordArray# wb i
         = if | isTrue# (a `eqWord#` b) -> go (i -# 1#)
              | isTrue# (a `gtWord#` b) -> 1#
              | True                    -> -1#

-- | Add two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: max (size a, size b) + 1
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_add
   :: MutableWordArray# s -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# s
   -> State# s
bignat_add mwa wa wb = addABc 0# 0##
   where
      !szA     = wordArraySize# wa
      !szB     = wordArraySize# wb
      !szMin   = minI# szA szB

      -- we have four cases:
      -- 1) we have a digit in A and in B + a potential carry
      --    => perform triple addition
      --    => result in (carry,word)
      -- 2) we have a digit only in A or B and a carry
      --    => perform double addition from a single array
      --    => result in (carry,word)
      -- 3) we have a digit only in A or B and no carry
      --    => perform array copy and shrink the array
      -- 4) We only have a potential carry
      --    => write the carry or shrink the array

      addABc i carry
         | isTrue# (i <# szMin) = Ubx.do
            let
               !(# carry', r #) = plusWord3#
                                    (indexWordArray# wa i)
                                    (indexWordArray# wb i)
                                    carry
            mwaWrite# mwa i r
            addABc (i +# 1#) carry'

         | isTrue# ((i ==# szA) &&# (i ==# szB))
         = mwaWriteOrShrink mwa carry i

         | isTrue# (i ==# szA)
         = addAoBc wb i carry

         | True
         = addAoBc wa i carry

      addAoBc wab i carry
         | isTrue# (i ==# wordArraySize# wab)
         = mwaWriteOrShrink mwa carry i

         | 0## <- carry
         = Ubx.do
            -- copy the remaining words and remove the word allocated for the
            -- potential carry
            mwaArrayCopy# mwa i wab i (wordArraySize# wab -# i)
            mwaShrink# mwa 1#

         | True
         = Ubx.do
            let !(# carry', r #) = plusWord2# (indexWordArray# wab i) carry
            mwaWrite# mwa i r
            addAoBc wab (i +# 1#) carry'

-- | Add a non-zero BigNat and a non-zero Word#
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a + 1
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_add_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_add_word mwa wa b s = mwaInitArrayPlusWord mwa wa b s

-- | Sub a non-zero word from a non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
--
-- Return False# to indicate underflow.
bignat_sub_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
bignat_sub_word mwa wa b = go b 0#
   where
      !sz = wordArraySize# wa
      go carry i
         | isTrue# (i >=# sz)
         = Ubx.pure (carry `eqWord#` 0##)

         | 0## <- carry
         = Ubx.do
            mwaArrayCopy# mwa i wa i (sz -# i)
            Ubx.pure 1# -- no underflow

         | True
         = case subWordC# (indexWordArray# wa i) carry of
            (# 0##, 0# #)
               | isTrue# (i ==# sz) -> Ubx.do
                  mwaShrink# mwa 1#
                  Ubx.pure 1# -- no underflow

            (# l  , c  #) -> Ubx.do
               mwaWrite# mwa i l
               go (int2Word# c) (i +# 1#)

-- | Multiply a non-zero BigNat and a non-zero Word#
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a + 1
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_mul_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_mul_word mwa wa b = go 0# 0##
   where
      !szA = wordArraySize# wa
      go i carry
         | isTrue# (i ==# szA) = mwaWriteOrShrink mwa carry i
         | True = Ubx.do
            let
               ai               = indexWordArray# wa i
               !(# carry', r #) = plusWord12# carry (timesWord2# ai b)
            mwaWrite# mwa i r
            go (i +# 1#) carry'


-- | Multiply two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a+size b
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_mul
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_mul mwa wa wb = Ubx.do
   -- initialize the resulting WordArray
   mwaFill# mwa 0## 0## (int2Word# sz)
   mulEachB ctzB -- loop on b Words
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      !sz  = szA +# szB

      !ctzA = word2Int# (bigNatCtzWord# wa)
      !ctzB = word2Int# (bigNatCtzWord# wb)

      -- multiply a single bj Word# to the whole wa WordArray
      mul mwa wa bj j i carry
         | isTrue# (i ==# wordArraySize# wa)
         -- write the carry
         = mwaAddInplaceWord# mwa (i +# j) carry

         | True = Ubx.do
            let
               ai           = indexWordArray# wa i
               !(# c',r' #) = timesWord2# ai bj
               !(# c'',r #) = plusWord2# r' carry
               carry'       = plusWord# c' c''
            mwaAddInplaceWord# mwa (i +# j) r
            mul mwa wa bj j (i +# 1#) carry'

      -- for each bj in wb, call `mul bj wa`
      mulEachB i
         | isTrue# (i ==# szB) = Ubx.nop
         | True = case indexWordArray# wb i of
            -- detect bj == 0## and skip the loop
            0## -> mulEachB (i +# 1#)
            bi  -> Ubx.do
               mul mwa wa bi i ctzA 0##
               mulEachB (i +# 1#)

-- | Sub two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
--
-- Return False# to indicate underflow.
bignat_sub
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
bignat_sub mwa wa wb = Ubx.do
   -- initialize the resulting WordArray
   -- Note: we could avoid the copy by subtracting the first non-zero
   -- less-significant word of b...
   mwaArrayCopy# mwa 0# wa 0# (wordArraySize# wa)
   mwaSubInplaceArray mwa 0# wb

-- | PopCount of a non-zero BigNat
bignat_popcount :: WordArray# -> Word#
bignat_popcount wa = go 0# 0##
   where
      !sz = wordArraySize# wa
      go i c
         | isTrue# (i ==# sz) = c
         | True               = go (i +# 1#) (c `plusWord#` popCnt# (indexWordArray# wa i))

-- | Left-shift a non-zero BigNat by a non-zero amount of bits
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a + required new limbs
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_shiftl
   :: MutableWordArray# s
   -> WordArray#
   -> Word#
   -> State# s
   -> State# s
bignat_shiftl mwa wa n = Ubx.do
   -- set the lower words to 0
   mwaFill# mwa 0## 0## (int2Word# nw)
   if | 0# <- nb -> mwaArrayCopy# mwa nw wa 0# szA
      | True     -> mwaBitShift 0# 0##
   where
      !szA          = wordArraySize# wa
      !(# nw, nb #) = count_words_bits_int n
      !sh           = WORD_SIZE_IN_BITS# -# nb

      -- Bit granularity (c is the carry from the previous shift)
      mwaBitShift i c
         -- write the carry
         | isTrue# (i ==# szA)
         = mwaWriteOrShrink mwa c (i +# nw)

         | True = Ubx.do
            let
               !ai = indexWordArray# wa i
               !v  = c `or#` (ai `uncheckedShiftL#` nb)
               !c' = ai `uncheckedShiftRL#` sh
            mwaWrite# mwa (i +# nw) v
            mwaBitShift (i +# 1#) c'


-- | Right-shift a non-zero BigNat by a non-zero amount of bits
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: required limbs
--
-- The potential 0 most-significant Word (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_shiftr
   :: MutableWordArray# s
   -> WordArray#
   -> Word#
   -> State# s
   -> State# s
bignat_shiftr mwa wa n
   | isTrue# (nb ==# 0#) = mwaArrayCopy# mwa 0# wa nw sz
   | True                = mwaBitShift (sz -# 1#) 0##
   where
      !szA          = wordArraySize# wa
      !(# nw, nb #) = count_words_bits_int n
      !sz           = szA -# nw
      !sh           = WORD_SIZE_IN_BITS# -# nb

      -- Bit granularity (c is the carry from the previous shift)
      mwaBitShift i c
         | isTrue# (i <# 0#) = Ubx.nop
         | True = Ubx.do
            let
               !ai = indexWordArray# wa (i +# nw)
               !v  = c `or#` (ai `uncheckedShiftRL#` nb)
               !c' = ai `uncheckedShiftL#` sh
            mwaWrite# mwa i v
            mwaBitShift (i -# 1#) c'

-- | Right-shift a non-zero BigNat by a non-zero amount of bits by first
-- converting it into its two's complement representation and then again after
-- the arithmetic shift.
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: required limbs
--
-- The potential 0 most-significant Words (i.e. the potential carry) will be
-- removed by the caller if it is not already done by the backend.
bignat_shiftr_neg
   :: MutableWordArray# s
   -> WordArray#
   -> Word#
   -> State# s
   -> State# s
bignat_shiftr_neg mwa wa n = Ubx.do
   -- initialize higher limb of mwa
   sz_mwa <- mwaSize# mwa
   mwaWrite# mwa (sz_mwa -# 1#) 0##
   bignat_shiftr mwa wa n
   if nz_shifted_out
      -- round if non-zero bits were shifted out
      then mwaAddInplaceWord# mwa 0# 1##
      else Ubx.nop
   where
      !(# nw, nb #) = count_words_bits_int n

      -- non-zero bits are shifted out?
      nz_shifted_out
         -- test nb bits
         | isTrue# (
            (nb /=# 0#)
            &&# (indexWordArray# wa nw `uncheckedShiftL#`
                  (WORD_SIZE_IN_BITS# -# nb) `neWord#` 0##))
         = True
         -- test nw words
         | True
         = let
            go j
               | isTrue# (j ==# nw)                           = False
               | isTrue# (indexWordArray# wa j `neWord#` 0##) = True
               | True                                         = go (j +# 1#)
           in go 0#


-- | OR two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: max (size a, size b)
bignat_or
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_or mwa wa wb
   | isTrue# (szA >=# szB) = go wa szA wb szB
   | True                  = go wb szB wa szA
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      -- nx >= ny
      go wx nx wy ny = Ubx.do
         mwaInitArrayBinOp mwa wx wy or#
         mwaArrayCopy# mwa ny wx ny (nx -# ny)

-- | XOR two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: max (size a, size b)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_xor
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_xor mwa wa wb
   | isTrue# (szA >=# szB) = go wa szA wb szB
   | True                  = go wb szB wa szA
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      -- nx >= ny
      go wx nx wy ny = Ubx.do
         mwaInitArrayBinOp mwa wx wy xor#
         mwaArrayCopy# mwa ny wx ny (nx -# ny)

-- | AND two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: min (size a, size b)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_and
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_and mwa wa wb s = mwaInitArrayBinOp mwa wa wb and# s

-- | ANDNOT two non-zero BigNat
--
-- Result is to be stored in the MutableWordArray#.
-- The latter has size: size a
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_and_not
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_and_not mwa wa wb = Ubx.do
   mwaInitArrayBinOp mwa wa wb (\x y -> x `and#` not# y)
   mwaArrayCopy# mwa szB wa szB (szA -# szB)
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb

-- | QuotRem of two non-zero BigNat
--
-- Result quotient and remainder are to be stored in the MutableWordArray#.
-- The first one (quotient) has size: size(A)-size(B)+1
-- The second one (remainder) has size: size(b)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_quotrem
   :: MutableWordArray# s -- ^ Quotient
   -> MutableWordArray# s -- ^ Remainder
   -> WordArray#
   -> WordArray#
   -> State# s
   -> State# s
bignat_quotrem mwq mwr uwa uwb = Ubx.do
   -- Normalization consists in left-shifting bits in B and A so that the
   -- most-significant bit of the most-significant word of B is 1. It makes
   -- quotient prediction much more efficient as we only use the two most
   -- significant words of A and the most significant word of B to make the
   -- prediction.

   -- we will left-shift A and B of "clzb" bits for normalization
   let !clzb  = clz# (indexWordArray# uwb (wordArraySize# uwb -# 1#))

   -- we use a single array initially containing A (normalized) and
   -- returning the remainder (normalized): mnwa (for "mutable normalized
   -- wordarray A")
   --
   -- We allocate it here with an additionnal Word compared to A because
   -- normalizing can left shift at most (N-1) bits (on N-bit arch).
   mnwa <- newWordArray# (wordArraySize# uwa +# 1#)

   -- normalized A in mnwa
   mwaWrite# mnwa (wordArraySize# uwa) 0## -- init potential carry
   bignat_shiftl mnwa uwa clzb              -- left shift
   mwaTrimZeroes# mnwa                      -- remove null carry if any

   -- normalize B. We don't do it in a MutableWordArray because it will remain
   -- constant during the whole computation.
   let !nwb = bigNatShiftL# uwb clzb

   -- perform quotrem on normalized inputs
   bignat_quotrem_normalized mwq mnwa nwb

   -- denormalize the remainder now stored in mnwa. We just have to right shift
   -- of "clzb" bits. We copy the result into "mwr" array.
   mwaTrimZeroes# mnwa
   wr <- unsafeFreezeByteArray# mnwa
   mwaSetSize# mwr (wordArraySize# wr)
   bignat_shiftr mwr wr clzb
   mwaTrimZeroes# mwr



-- | Quotient of two non-zero BigNat
--
-- Result quotient is to be stored in the MutableWordArray#.
-- The latter has size: size(A)-size(B)+1
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_quot
   :: MutableWordArray# RealWorld -- ^ Quotient
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_quot mwq wa wb = Ubx.do
   -- allocate a temporary array for the remainder and call quotrem
   mwr <- newWordArray# (wordArraySize# wb)
   bignat_quotrem mwq mwr wa wb

-- | Remainder of two non-zero BigNat
--
-- Result remainder is to be stored in the MutableWordArray#.
-- The latter has size: size(B)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_rem
   :: MutableWordArray# RealWorld -- ^ Remainder
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_rem mwr wa wb = Ubx.do
   -- allocate a temporary array for the quotient and call quotrem
   -- (we could avoid allocating it as it is not used to compute the result but
   -- it would require non trivial modification of bignat_quotrem)
   mwq <- newWordArray# szQ
   bignat_quotrem mwq mwr wa wb
   where
   szA = wordArraySize# wa
   szB = wordArraySize# wb
   szQ = 1# +# szA -# szB

-- | Perform quotRem on normalized inputs:
--    * highest bit of B is set
--    * A is trimmed
--    * A >= B
--    * B > 1
bignat_quotrem_normalized
   :: MutableWordArray# s
   -> MutableWordArray# s
   -> WordArray#
   -> State# s
   -> State# s
bignat_quotrem_normalized mwq mwa b = Ubx.do

   -- n is the size of B
   let !n = wordArraySize# b

   -- m+n is the size of A (m >= 0)
   szA <- mwaSize# mwa
   let !m = szA -# n

   -- Definitions:
   --    MSW(x) is the most-significant word of x
   --    MSB(x) the most-significant bit of x

   -- We first compute MSW(Q).  Thanks to the normalization of B, MSW(Q) can
   -- only be 0 or 1 so we only have to perform a prefix comparison to compute
   -- MSW(Q).
   --
   --    Proof MSW(Q) < 2:
   --       * MSB(MSW(B)) = 1 thanks to normalization.
   --       * MSW(B) * MSW(Q) <= MSW(A) by definition
   --       * suppose MSW(Q) >= 2:
   --          MSW(B) * MSW(Q) >= MSW(B) << 1    { MSW(Q) >= 2              }
   --                          >  MAX_WORD_VALUE { MSB(MSW(B)) = 1          }
   --                          >  MSW(A)         { MSW(A) <= MAX_WORD_VALUE }
   --          contradiction.
   --
   -- If A >= (B << m words)
   --    then Qm = 1
   --         A := A - (B << m words)
   --    else Qm = 0
   --         A unchanged
   let computeQm = Ubx.do
         c <- mwaTrimCompare m mwa b
         case c of
            LT -> Ubx.pure 0##
            _  -> Ubx.pure 1##

       updateQj j qj qjb = Ubx.do
         mwaWrite# mwq j qj -- write Qj
         case qj of
            0## -> Ubx.nop
            _   -> Ubx.void (mwaSubInplaceArray mwa j qjb) -- subtract (qjB << j words)

       -- update the highest word of Q
       updateQm = Ubx.do
         qm <- computeQm
         updateQj m qm b

       -- the size of Q is szA+szB+1 BEFORE normalization. Normalization may add
       -- an additional higher word to A.
       --   * If A has an additional limb:
       --      * MSW(A) < MSW(B). Because MSB(MSW(A)) can't be set (it would
       --        mean that we shifted a whole word, which we didn't)
       --      * hence MSW(Q) = 0 but we don't have to write it (and we mustn't)
       --        because of the size of Q
       --   * If A has no additional limb:
       --      * We have to check if MSW(A) >= MSW(B) and to adjust A and MSW(Q)
       --        accordingly
       --
       -- We detect if A has an additional limb by comparing the size of Q with m
       updateQmMaybe = Ubx.do
         szQ <- mwaSize# mwq
         if isTrue# (m <# szQ)
            then updateQm
            else Ubx.nop

   updateQmMaybe


   -- main loop: for j from (m-1) downto 0
   --    We estimate a one Word quotient qj:
   --       e1e0 <- a(n+j)a(n+j-1) `div` b(n-1)
   --       qj | e1 == 0   = e0
   --          | otherwise = maxBound
   --    We loop until we find the real quotient:
   --       while (A < ((qj*B) << j words)) qj--
   --    We update A and Qj:
   --       Qj := qj
   --       A  := A - (qj*B << j words)

   let bmsw = wordArrayLast# b -- most significant word of B

       estimateQj j = Ubx.do
         a1 <- mwaRead# mwa (n +# j)
         a0 <- mwaRead# mwa (n +# j -# 1#)
         case quotRemWord3# (# a1, a0 #) bmsw of
            (# (# 0##, qj #), _ #) -> Ubx.pure qj
            (# (#   _,  _ #), _ #) -> Ubx.pure WORD_MAXBOUND##

       -- we perform the qj*B multiplication once and then we subtract B from
       -- qj*B as much as needed until (qj'*B << j words) <= A
       findRealQj j qj = findRealQj' j qj (bigNatMulWord# b qj)

       findRealQj' j qj qjB = Ubx.do
         c <- mwaTrimCompare j mwa qjB
         case c of
            LT -> findRealQj' j (qj `minusWord#` 1##) (bigNatSubUnsafe qjB b)
                                                            -- TODO: we could do the sub inplace to
                                                            -- reduce allocations
            _  -> Ubx.pure (# qj, qjB #)

       loop j = Ubx.do
         qj <- estimateQj j
         (# qj', qjB #) <- findRealQj j qj
         updateQj j qj' qjB
         case j of
            0# -> Ubx.nop
            _  -> loop (j -# 1#)


   case m of
      0# -> Ubx.nop
      _  -> loop (m -# 1#)

-- | QuotRem of a non-zero BigNat and a non-zero Word
--
-- Result quotient is to be stored in the MutableWordArray#.
-- The latter has size: size(A)
--
-- The remainder is returned.
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_quotrem_word
   :: MutableWordArray# s -- ^ Quotient
   -> WordArray#
   -> Word#
   -> State# s
   -> (# State# s, Word# #)
bignat_quotrem_word mwq wa b = go (sz -# 1#) 0##
   where
      sz = wordArraySize# wa
      go i r
         | isTrue# (i <# 0#) = Ubx.pure r
         | True = Ubx.do
            let
               ai          = indexWordArray# wa i
               !(# q,r' #) = quotRemWord2# r ai b
            mwaWrite# mwq i q
            go (i -# 1#) r'

-- | Quot of a non-zero BigNat and a non-zero Word
--
-- Result quotient is to be stored in the MutableWordArray#.
-- The latter has size: size(A)
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_quot_word
   :: MutableWordArray# s -- ^ Quotient
   -> WordArray#
   -> Word#
   -> State# s
   -> State# s
bignat_quot_word mwq wa b = go (sz -# 1#) 0##
   where
      sz = wordArraySize# wa
      go i r
         | isTrue# (i <# 0#) = Ubx.nop
         | True = Ubx.do
            let
               ai          = indexWordArray# wa i
               !(# q,r' #) = quotRemWord2# r ai b
            mwaWrite# mwq i q
            go (i -# 1#) r'

-- | Remainder of a non-zero BigNat and a non-zero Word
--
-- The remainder is returned.
bignat_rem_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_rem_word wa b = go (sz -# 1#) 0##
   where
      sz = wordArraySize# wa
      go i r
         | isTrue# (i <# 0#) = r
         | True =
            let
               ai          = indexWordArray# wa i
               !(# _,r' #) = quotRemWord2# r ai b
            in go (i -# 1#) r'


-- | Greatest common divisor (GCD) of two non-zero and non-one BigNat
--
-- Result GCD is to be stored in the MutableWordArray#.
-- The latter has size: size(B)
-- The first WordArray# is greater than the second WordArray#.
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_gcd
   :: MutableWordArray# s
   -> WordArray#
   -> WordArray#
   -> State# s
   -> State# s
bignat_gcd mwr = go
   where
      go wmax wmin
         | isTrue# (wordArraySize# wmin ==# 0#)
         = mwaInitCopyShrink# mwr wmax

         | True
         = let
             wmax' = wmin
             !wmin' = bigNatRem wmax wmin
           in go wmax' wmin'

-- | Greatest common divisor (GCD) of a non-zero/non-one BigNat and a
-- non-zero/non-one Word#
--
-- Result GCD is returned
bignat_gcd_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_gcd_word a b = bignat_gcd_word_word b (bigNatRemWord# a b)

-- | Greatest common divisor (GCD) of two Word#
--
-- Result GCD is returned.
--
-- This operation doesn't really belongs here, but GMP's one is much faster
-- than this simple implementation (basic Euclid algorithm).
--
-- Ideally we should make an implementation as fast as GMP's one and put it into
-- GHC.Internal.Bignum.Primitives.
bignat_gcd_word_word
   :: Word#
   -> Word#
   -> Word#
bignat_gcd_word_word a 0## = a
bignat_gcd_word_word a b   = bignat_gcd_word_word b (a `remWord#` b)

-- | Encode (# BigNat mantissa, Int# exponent #) into a Double#
bignat_encode_double :: WordArray# -> Int# -> Double#
bignat_encode_double wa e0 = go 0.0## e0 0#
   where
      sz = wordArraySize# wa
      go acc e i
         | isTrue# (i >=# sz) = acc
         | True
         = go (acc +## wordEncodeDouble# (indexWordArray# wa i) e)
              (e +# WORD_SIZE_IN_BITS#) -- FIXME: we assume that e doesn't overflow...
              (i +# 1#)

-- | \"@'bignat_powmod_word' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- b > 1
-- e > 0
-- m > 1
bignat_powmod_word :: WordArray# -> WordArray# -> Word# -> Word#
bignat_powmod_word b0 e0 m = go (naturalFromBigNat# b0) (naturalFromBigNat# e0) (naturalFromWord# 1##)
   where
      go !b e !r
        | isTrue# (e `naturalTestBit#` 0##)
        = go b' e' ((r `naturalMul` b) `naturalRem` m')

        | naturalIsZero e
        = naturalToWord# r

        | True
        = go b' e' r
        where
          b' = (b `naturalMul` b) `naturalRem` m'
          m' = naturalFromWord# m
          e' = e `naturalShiftR#` 1## -- slightly faster than "e `div` 2"

-- | \"@'bignat_powmod' r /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- b > 1
-- e > 0
-- m > 1
--
-- Result is to be stored in the MutableWordArray# (which size is equal to the
-- one of m).
--
-- The potential 0 most-significant Words will be removed by the caller if it is
-- not already done by the backend.
bignat_powmod
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_powmod r b0 e0 m s = mwaInitCopyShrink# r r' s
   where
      !r' = go (naturalFromBigNat# b0)
               (naturalFromBigNat# e0)
               (naturalFromWord# 1##)

      go !b e !r
        | isTrue# (e `naturalTestBit#` 0##)
        = go b' e' ((r `naturalMul` b) `naturalRem` m')

        | naturalIsZero e
        = naturalToBigNat# r

        | True
        = go b' e' r
        where
          b' = (b `naturalMul` b) `naturalRem` m'
          m' = naturalFromBigNat# m
          e' = e `naturalShiftR#` 1## -- slightly faster than "e `div` 2"

-- | \"@'bignat_powmod' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- b > 1
-- e > 0
-- m > 1
bignat_powmod_words
   :: Word#
   -> Word#
   -> Word#
   -> Word#
bignat_powmod_words b e m =
   bignat_powmod_word (wordArrayFromWord# b)
                      (wordArrayFromWord# e)
                      m


-- | Return extended GCD of two non-zero integers.
--
-- I.e. integer_gcde a b returns (g,x,y) so that ax + by = g
--
-- Input: a and b are non zero.
-- Output: g must be > 0
--
integer_gcde
   :: Integer
   -> Integer
   -> (# Integer, Integer, Integer #)
integer_gcde a b = f (# a,integerOne,integerZero #) (# b,integerZero,integerOne #)
  where
    -- returned "g" must be positive
    fix (# g, x, y #)
       | integerIsNegative g = (# integerNegate g, integerNegate x, integerNegate y #)
       | True                = (# g,x,y #)

    f old@(# old_g, old_s, old_t #) new@(# g, s, t #)
      | integerIsZero g = fix old
      | True            = case integerQuotRem# old_g g of
                              !(# q, r #) -> f new (# r , old_s `integerSub` (q `integerMul` s)
                                                        , old_t `integerSub` (q `integerMul` t) #)

-- | Computes the modular inverse of two non-zero integers.
--
-- I.e. y = integer_recip_mod x m
--        = x^(-1) `mod` m
--
-- with 0 < y < abs m
integer_recip_mod
   :: Integer
   -> Natural
   -> (# Natural | () #)
integer_recip_mod x m =
   let m' = integerFromNatural m
   in case integer_gcde x m' of
      (# g, a, _b #)
         -- gcd(x,m) = ax+mb = 1
         -- ==> ax - 1 = -mb
         -- ==> ax     = 1 [m]
         | g `integerEq` integerOne -> (# integerToNatural (a `integerMod` m') | #)
                                       -- a `mod` m > 0 because m > 0
         | True                     -> (# | () #)

-- | Computes the modular exponentiation.
--
-- I.e. y = integer_powmod b e m
--        = b^e `mod` m
--
-- with 0 <= y < abs m
integer_powmod
   :: Integer
   -> Natural
   -> Natural
   -> Natural
integer_powmod b0 e0 m = go b0 e0 integerOne
   where
      !m' = integerFromNatural m

      go !b e !r
        | isTrue# (e `naturalTestBit#` 0##)
        = go b' e' ((r `integerMul` b) `integerMod` m')

        | naturalIsZero e
        = integerToNatural r -- r >= 0 by integerMod above

        | True
        = go b' e' r
        where
          b' = (b `integerMul` b) `integerRem` m'
          e' = e `naturalShiftR#` 1##
