{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GHC.Internal.Bignum.Backend.Native where

#include "MachDeps.h"
#include "WordSize.h"

#if defined(BIGNUM_NATIVE) || defined(BIGNUM_CHECK) || defined(BIGNUM_FFI)
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
import GHC.Prim
import GHC.Types

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
-- (This module uses the empty tuple () and string literals.)
import GHC.Tuple ()
import GHC.CString ()

default ()

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

      addABc i carry s
         | isTrue# (i <# szMin) =
            let
               !(# carry', r #) = plusWord3#
                                    (indexWordArray# wa i)
                                    (indexWordArray# wb i)
                                    carry
            in case mwaWrite# mwa i r s of
               s' -> addABc (i +# 1#) carry' s'

         | isTrue# ((i ==# szA) &&# (i ==# szB))
         = mwaWriteOrShrink mwa carry i s

         | isTrue# (i ==# szA)
         = addAoBc wb i carry s

         | True
         = addAoBc wa i carry s

      addAoBc wab i carry s
         | isTrue# (i ==# wordArraySize# wab)
         = mwaWriteOrShrink mwa carry i s

         | 0## <- carry
         = -- copy the remaining words and remove the word allocated for the
           -- potential carry
           case mwaArrayCopy# mwa i wab i (wordArraySize# wab -# i) s of
            s' -> mwaShrink# mwa 1# s'

         | True
         = let !(# carry', r #) = plusWord2# (indexWordArray# wab i) carry
           in case mwaWrite# mwa i r s of
               s' -> addAoBc wab (i +# 1#) carry' s'

bignat_add_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_add_word mwa wa b s = mwaInitArrayPlusWord mwa wa b s

bignat_sub_word
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
bignat_sub_word mwa wa b = go b 0#
   where
      !sz = wordArraySize# wa
      go carry i s
         | isTrue# (i >=# sz)
         = (# s, carry `eqWord#` 0## #)

         | 0## <- carry
         = case mwaArrayCopy# mwa i wa i (sz -# i) s of
            s' -> (# s', 1# #) -- no underflow

         | True
         = case subWordC# (indexWordArray# wa i) carry of
            (# 0##, 0# #)
               | isTrue# (i ==# sz) -> case mwaShrink# mwa 1# s of
                                          s' -> (# s', 1# #) -- no underflow

            (# l  , c  #) -> case mwaWrite# mwa i l s of
                              s1 -> go (int2Word# c) (i +# 1#) s1

bignat_mul_word
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> Word#
   -> State# RealWorld
   -> State# RealWorld
bignat_mul_word mwa wa b = go 0# 0##
   where
      !szA = wordArraySize# wa
      go i carry s
         | isTrue# (i ==# szA) = mwaWriteOrShrink mwa carry i s
         | True =
            let
               ai               = indexWordArray# wa i
               !(# carry', r #) = plusWord12# carry (timesWord2# ai b)
            in case mwaWrite# mwa i r s of
                  s' -> go (i +# 1#) carry' s'


bignat_mul
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_mul mwa wa wb s1 =
   -- initialize the resulting WordArray
   case mwaFill# mwa 0## 0## (int2Word# sz) s1 of
      s' -> mulEachB ctzB s' -- loop on b Words
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      !sz  = szA +# szB

      !ctzA = word2Int# (bigNatCtzWord# wa)
      !ctzB = word2Int# (bigNatCtzWord# wb)

      -- multiply a single bj Word# to the whole wa WordArray
      mul mwa wa bj j i carry s
         | isTrue# (i ==# wordArraySize# wa)
         -- write the carry
         = mwaAddInplaceWord# mwa (i +# j) carry s

         | True = let
                     ai           = indexWordArray# wa i
                     !(# c',r' #) = timesWord2# ai bj
                     !(# c'',r #) = plusWord2# r' carry
                     carry'       = plusWord# c' c''
                  in case mwaAddInplaceWord# mwa (i +# j) r s of
                        s' -> mul mwa wa bj j (i +# 1#) carry' s'

      -- for each bj in wb, call `mul bj wa`
      mulEachB i s
         | isTrue# (i ==# szB) = s
         | True = case indexWordArray# wb i of
            -- detect bj == 0## and skip the loop
            0## -> mulEachB (i +# 1#) s
            bi  -> case mul mwa wa bi i ctzA 0## s of
                     s' -> mulEachB (i +# 1#) s'

bignat_sub
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> (# State# RealWorld, Bool# #)
bignat_sub mwa wa wb s =
   -- initialize the resulting WordArray
   -- Note: we could avoid the copy by subtracting the first non-zero
   -- less-significant word of b...
   case mwaArrayCopy# mwa 0# wa 0# (wordArraySize# wa) s of
      s' -> mwaSubInplaceArray mwa 0# wb s'

bignat_popcount :: WordArray# -> Word#
bignat_popcount wa = go 0# 0##
   where
      !sz = wordArraySize# wa
      go i c
         | isTrue# (i ==# sz) = c
         | True               = go (i +# 1#) (c `plusWord#` popCnt# (indexWordArray# wa i))

bignat_shiftl
   :: MutableWordArray# s
   -> WordArray#
   -> Word#
   -> State# s
   -> State# s
bignat_shiftl mwa wa n s1 =
   -- set the lower words to 0
   case mwaFill# mwa 0## 0## (int2Word# nw) s1 of
      s2 -> if
            | 0# <- nb -> mwaArrayCopy# mwa nw wa 0# szA s2
            | True     -> mwaBitShift 0# 0## s2
   where
      !szA          = wordArraySize# wa
      !(# nw, nb #) = count_words_bits_int n
      !sh           = WORD_SIZE_IN_BITS# -# nb

      -- Bit granularity (c is the carry from the previous shift)
      mwaBitShift i c s
         -- write the carry
         | isTrue# (i ==# szA)
         = mwaWriteOrShrink mwa c (i +# nw) s

         | True =
            let
               !ai = indexWordArray# wa i
               !v  = c `or#` (ai `uncheckedShiftL#` nb)
               !c' = ai `uncheckedShiftRL#` sh
            in case mwaWrite# mwa (i +# nw) v s of
                  s' -> mwaBitShift (i +# 1#) c' s'


bignat_shiftr
   :: MutableWordArray# s
   -> WordArray#
   -> Word#
   -> State# s
   -> State# s
bignat_shiftr mwa wa n s1
   | isTrue# (nb ==# 0#) = mwaArrayCopy# mwa 0# wa nw sz s1
   | True                = mwaBitShift (sz -# 1#) 0## s1
   where
      !szA          = wordArraySize# wa
      !(# nw, nb #) = count_words_bits_int n
      !sz           = szA -# nw
      !sh           = WORD_SIZE_IN_BITS# -# nb

      -- Bit granularity (c is the carry from the previous shift)
      mwaBitShift i c s
         | isTrue# (i <# 0#) = s
         | True =
            let
               !ai = indexWordArray# wa (i +# nw)
               !v  = c `or#` (ai `uncheckedShiftRL#` nb)
               !c' = ai `uncheckedShiftL#` sh
            in case mwaWrite# mwa i v s of
                  s' -> mwaBitShift (i -# 1#) c' s'

bignat_shiftr_neg
   :: MutableWordArray# s
   -> WordArray#
   -> Word#
   -> State# s
   -> State# s
bignat_shiftr_neg mwa wa n s1
   -- initialize higher limb of mwa
   = case mwaSize# mwa s1 of
      (# s2, sz_mwa #) -> case mwaWrite# mwa (sz_mwa -# 1#) 0## s2 of
        s3 -> case bignat_shiftr mwa wa n s3 of
           s4 -> if nz_shifted_out
                    -- round if non-zero bits were shifted out
                    then mwaAddInplaceWord# mwa 0# 1## s4
                    else s4
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


bignat_or
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_or mwa wa wb s1
   | isTrue# (szA >=# szB) = go wa szA wb szB s1
   | True                  = go wb szB wa szA s1
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      -- nx >= ny
      go wx nx wy ny s =
         case mwaInitArrayBinOp mwa wx wy or# s of
            s' -> mwaArrayCopy# mwa ny wx ny (nx -# ny) s'

bignat_xor
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_xor mwa wa wb s1
   | isTrue# (szA >=# szB) = go wa szA wb szB s1
   | True                  = go wb szB wa szA s1
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb
      -- nx >= ny
      go wx nx wy ny s =
         case mwaInitArrayBinOp mwa wx wy xor# s of
            s' -> mwaArrayCopy# mwa ny wx ny (nx -# ny) s'

bignat_and
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_and mwa wa wb s = mwaInitArrayBinOp mwa wa wb and# s

bignat_and_not
   :: MutableWordArray# RealWorld -- ^ Result
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_and_not mwa wa wb s =
   case mwaInitArrayBinOp mwa wa wb (\x y -> x `and#` not# y) s of
      s' -> mwaArrayCopy# mwa szB wa szB (szA -# szB) s'
   where
      !szA = wordArraySize# wa
      !szB = wordArraySize# wb

bignat_quotrem
   :: MutableWordArray# s
   -> MutableWordArray# s
   -> WordArray#
   -> WordArray#
   -> State# s
   -> State# s
bignat_quotrem mwq mwr uwa uwb s0 =
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
   in case newWordArray# (wordArraySize# uwa +# 1#) s0 of { (# s1, mnwa #) ->

   -- normalized A in mnwa
   let normalizeA s = case mwaWrite# mnwa (wordArraySize# uwa) 0## s of -- init potential carry
                         s -> case bignat_shiftl mnwa uwa clzb s of     -- left shift
                            s -> mwaTrimZeroes# mnwa s                  -- remove null carry if any
   in case normalizeA s1 of { s2 ->

   -- normalize B. We don't do it in a MutableWordArray because it will remain
   -- constant during the whole computation.
   let !nwb = bigNatShiftL# uwb clzb in

   -- perform quotrem on normalized inputs
   case bignat_quotrem_normalized mwq mnwa nwb s2 of { s3 ->

   -- denormalize the remainder now stored in mnwa. We just have to right shift
   -- of "clzb" bits. We copy the result into "mwr" array.
   let denormalizeR s = case mwaTrimZeroes# mnwa s of
                         s -> case unsafeFreezeByteArray# mnwa s of
                            (# s, wr #) -> case mwaSetSize# mwr (wordArraySize# wr) s of
                               s -> case bignat_shiftr mwr wr clzb s of
                                 s -> mwaTrimZeroes# mwr s
   in denormalizeR s3
   }}}



bignat_quot
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_quot mwq wa wb s =
   -- allocate a temporary array for the remainder and call quotrem
   case newWordArray# (wordArraySize# wb) s of
      (# s, mwr #) -> bignat_quotrem mwq mwr wa wb s

bignat_rem
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> WordArray#
   -> State# RealWorld
   -> State# RealWorld
bignat_rem mwr wa wb s =
   -- allocate a temporary array for the quotient and call quotrem
   -- (we could avoid allocating it as it is not used to compute the result but
   -- it would require non trivial modification of bignat_quotrem)
   case newWordArray# szQ s of
      (# s, mwq #) -> bignat_quotrem mwq mwr wa wb s
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
bignat_quotrem_normalized mwq mwa b s0 =

   -- n is the size of B
   let !n = wordArraySize# b

   -- m+n is the size of A (m >= 0)
   in case mwaSize# mwa s0 of { (# s1, szA #) ->
   let !m = szA -# n in

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
   let computeQm s = case mwaTrimCompare m mwa b s of
         (# s, LT #) -> (# s, 0## #)
         (# s, _  #) -> (# s, 1## #)

       updateQj j qj qjb s = case mwaWrite# mwq j qj s of -- write Qj
               s | 0## <- qj -> s
                 | True      -> case mwaSubInplaceArray mwa j qjb s of -- subtract (qjB << j words)
                                 (# s, _ #) -> s

       -- update the highest word of Q
       updateQm s = case computeQm s of
         (# s, qm #) -> updateQj m qm b s

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
       updateQmMaybe s = case mwaSize# mwq s of
         (# s, szQ #) | isTrue# (m <# szQ) -> updateQm s
                      | True               -> s

   in case updateQmMaybe s1 of { s2 ->


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

       estimateQj j s =
         case mwaRead# mwa (n +# j) s of
           (# s, a1 #) -> case mwaRead# mwa (n +# j -# 1#) s of
             (# s, a0 #) -> case quotRemWord3# (# a1, a0 #) bmsw of
               (# (# 0##, qj #), _ #) -> (# s,              qj #)
               (# (#   _,  _ #), _ #) -> (# s, WORD_MAXBOUND## #)

       -- we perform the qj*B multiplication once and then we subtract B from
       -- qj*B as much as needed until (qj'*B << j words) <= A
       findRealQj j qj s = findRealQj' j qj (bigNatMulWord# b qj) s

       findRealQj' j qj qjB s = case mwaTrimCompare j mwa qjB s of
         (# s, LT #) -> findRealQj' j (qj `minusWord#` 1##) (bigNatSubUnsafe qjB b) s
                                                            -- TODO: we could do the sub inplace to
                                                            -- reduce allocations
         (# s, _  #) -> (# s, qj, qjB #)

       loop j s = case estimateQj j s of
         (# s, qj #) -> case findRealQj j qj s of
            (# s, qj, qjB #) -> case updateQj j qj qjB s of
               s | 0# <- j -> s
                 | True    -> loop (j -# 1#) s


   in if | 0# <- m -> s2
         | True    -> loop (m -# 1#) s2
   }}

bignat_quotrem_word
   :: MutableWordArray# s -- ^ Quotient
   -> WordArray#
   -> Word#
   -> State# s
   -> (# State# s, Word# #)
bignat_quotrem_word mwq wa b s = go (sz -# 1#) 0## s
   where
      sz = wordArraySize# wa
      go i r s
         | isTrue# (i <# 0#) = (# s, r #)
         | True =
            let
               ai          = indexWordArray# wa i
               !(# q,r' #) = quotRemWord2# r ai b
            in case mwaWrite# mwq i q s of
                  s' -> go (i -# 1#) r' s'

bignat_quot_word
   :: MutableWordArray# s -- ^ Quotient
   -> WordArray#
   -> Word#
   -> State# s
   -> State# s
bignat_quot_word mwq wa b s = go (sz -# 1#) 0## s
   where
      sz = wordArraySize# wa
      go i r s
         | isTrue# (i <# 0#) = s
         | True =
            let
               ai          = indexWordArray# wa i
               !(# q,r' #) = quotRemWord2# r ai b
            in case mwaWrite# mwq i q s of
                  s' -> go (i -# 1#) r' s'

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


bignat_gcd
   :: MutableWordArray# s
   -> WordArray#
   -> WordArray#
   -> State# s
   -> State# s
bignat_gcd mwr = go
   where
      go wmax wmin s
         | isTrue# (wordArraySize# wmin ==# 0#)
         = mwaInitCopyShrink# mwr wmax s

         | True
         = let
             wmax' = wmin
             !wmin' = bigNatRem wmax wmin
           in go wmax' wmin' s

bignat_gcd_word
   :: WordArray#
   -> Word#
   -> Word#
bignat_gcd_word a b = bignat_gcd_word_word b (bigNatRemWord# a b)

-- | This operation doesn't really belongs here, but GMP's one is much faster
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

bignat_powmod_words
   :: Word#
   -> Word#
   -> Word#
   -> Word#
bignat_powmod_words b e m =
   bignat_powmod_word (wordArrayFromWord# b)
                      (wordArrayFromWord# e)
                      m


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
