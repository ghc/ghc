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

module GHC.Num.BigNat.Native where

#include "MachDeps.h"

#if defined(BIGNUM_NATIVE) || defined(BIGNUM_CHECK)
import {-# SOURCE #-} GHC.Num.BigNat
import {-# SOURCE #-} GHC.Num.Natural
#else
import GHC.Num.BigNat
import GHC.Num.Natural
#endif
import GHC.Num.WordArray
import GHC.Num.Primitives
import GHC.Prim
import GHC.Types

default ()

#if WORD_SIZE_IN_BITS == 64
# define WORD_SIZE_IN_BYTES    8
# define WORD_SIZE_BYTES_SHIFT 3
# define WORD_SIZE_BITS_SHIFT  6
# define WORD_SIZE_BITS_MASK   0b111111
# define WORD_MAXBOUND         0xffffffffffffffff
#elif WORD_SIZE_IN_BITS == 32
# define WORD_SIZE_IN_BYTES    4
# define WORD_SIZE_BYTES_SHIFT 2
# define WORD_SIZE_BITS_SHIFT  5
# define WORD_SIZE_BITS_MASK   0b11111
# define WORD_MAXBOUND         0xffffffff
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

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
         = (# s, carry `neWord#` 0## #)

         | 0## <- carry
         = case mwaArrayCopy# mwa i wa i (sz -# i) s of
            s' -> (# s', 0# #)

         | True
         = case subWordC# (indexWordArray# wa i) carry of
            (# 0##, 0# #)
               | isTrue# (i ==# sz) -> case mwaShrink# mwa 1# s of
                                          s' -> (# s', 0# #)

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
      mul bj j i carry s
         | isTrue# (i ==# szA)
         -- write the carry
         = mwaAddInplaceWord# mwa (i +# j) carry s

         | True = let
                     ai           = indexWordArray# wa i
                     !(# c',r' #) = timesWord2# ai bj
                     !(# c'',r #) = plusWord2# r' carry
                     carry'       = plusWord# c' c''
                  in case mwaAddInplaceWord# mwa (i +# j) r s of
                        s' -> mul bj j (i +# 1#) carry' s'

      -- for each bj in wb, call `mul bj wa`
      mulEachB i s
         | isTrue# (i ==# szB) = s
         | True = case indexWordArray# wb i of
            -- detect bj == 0## and skip the loop
            0## -> mulEachB (i +# 1#) s
            bi  -> case mul bi i ctzA 0## s of
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
   -- initialize higher limb
   = case mwaWrite# mwa (szA -# 1#) 0## s1 of
      s2 -> case bignat_shiftr mwa wa n s2 of
         s3 -> if nz_shifted_out
                  -- round if non-zero bits were shifted out
                  then mwaAddInplaceWord# mwa 0# 1## s3
                  else s3
   where
      !szA          = wordArraySize# wa
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
bignat_quotrem mwq mwr uwa uwb s1 =
   case normalizeA s1     of { (# s2, mwa #) ->
   case mwaSize# mwq s2   of { (# s3, szQ #) ->
   -- normalized quotrem
   case bignat_quotrem_normalized mwq szQ mwa (normalizeB uwb) s3 of { s4 ->
   -- copy and denormalize remainder from mwa to mwr
   -- mwq is already not normalized
   denormalizeR mwr mwa s4
   }}}

   where
      -- Normalization consists in left-shifting bits in B and A so that the
      -- most-significant bit of the most-significant word of B is 1. It makes
      -- quotient prediction much more efficient as we only use the most
      -- significant words to make the prediction.
      --
      -- We also remove the common trailing zero Words in `a` and `b`
      -- during the normalization
      shl = clz# (indexWordArray# uwb (wordArraySize# uwb -# 1#))

      -- common number of zero less-significant bits (round to lower word size)
      shrW = minW# (bigNatCtz# uwa) (bigNatCtz# uwb)
               `and#` (not# WORD_SIZE_BITS_MASK##)

      isLeftShift  = isTrue# (shrW `eqWord#` 0##)

      -- right shift amount in bits if isLeftShift == False
      shr = shrW `minusWord#` shl
      !(# nw, nb #)  = count_words_bits_int shr

      normalizeB ws
         | isLeftShift = bigNatShiftL# ws shl
         | True        = bigNatShiftR# ws shr

      normalizeA s
         | isLeftShift
         , 0## <- shl
         = let szna = wordArraySize# uwa
           in case newWordArray# szna s of
               (# s, mwa #) -> case mwaArrayCopy# mwa 0# uwa 0# szna s of
                  s -> (# s, mwa #)

         | isLeftShift
         = let szna = wordArraySize# uwa +# 1# -- Normalizing A can add a newer higher limb
           in case newWordArray# szna s of
               (# s, mwa #) -> case bignat_shiftl mwa uwa shl s of
                     s -> (# s, mwa #)

         | True
         = let szna = wordArraySize# uwa -# nw
           in case newWordArray# szna s of
               (# s, mwa #) -> case bignat_shiftr mwa uwa shr s of
                     s -> case mwaTrimZeroes# mwa s of
                        s -> (# s, mwa #)

      denormalizeR mwr mws s
         = case mwaTrimZeroes# mws s of
            s -> case unsafeFreezeByteArray# mws s of
               (# s, ws #) -> if
                  | isLeftShift -> case mwaSetSize# mwr (wordArraySize# ws) s of
                                    s -> case bignat_shiftr mwr ws shl s of
                                       s -> mwaTrimZeroes# mwr s
                  | True        -> let sz = wordArraySize# ws +# nw +# (nb /=# 0#)
                                   in case mwaSetSize# mwr sz s of
                                    s -> case bignat_shiftl mwr ws shr s of
                                       s -> mwaTrimZeroes# mwr s

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

-- | On input:
--    - B is normalized
--    - A is trimed
--    - A > B
bignat_quotrem_normalized
   :: MutableWordArray# s
   -> Int#
   -> MutableWordArray# s
   -> WordArray#
   -> State# s
   -> State# s
bignat_quotrem_normalized mwq szQ mwa b s =
   case computeQMax s of
      s -> loop (szQ -# 1#) s
   where
      szB = wordArraySize# b

      -- Compute and store highest quotient word QMax. As B is normalized,
      -- QMax=0 or QMax=1.
      --
      --    Proof: if QMax>=2, then QMax*B has a least one additional
      --    higher limb compared to B because B is normalized (higher bit of higher
      --    word is set). Hence size(QMax*B)+size(A)-size(B) > size(A) (i.e.
      --    QMax*B shifted appropriately is greater than A). So QMax is too
      --    large to be a valid quotient.
      --
      -- We need to adjust the remainder depending on QMax:
      --    QMax=1: remainder = subtract B shifted appropriately from A
      --    QMax=0: remainder = A

      computeQMax s =
         case mwaSize# mwa s of
            (# s, szA #) -> if
               -- szQ is equal to szA-szB+1 BEFORE normalization. Hence, the
               -- actual szA can be larger by 1 word. In this case, the
               -- additional higher word of A is < higher word of B by
               -- construction. Hence we don't have to store do anything as
               -- don't even have a slot to store the corresponding 0 quotient
               -- word.
               | isTrue# (szQ +# 1# /=# szA -# szB) -> s
               | True -> case mwaTrimCompare (szQ -# 1#) mwa b s of
                  (# s, prefixCmp #) ->
                     let
                        qmax    = case prefixCmp of
                                    LT -> 0##
                                    _  -> 1##
                     in case mwaWrite# mwq (szQ -# 1#) qmax s of
                           s -> updateR (szQ -# 1#) b prefixCmp s

      updateR k qb c s = case c of
         LT -> s
         EQ -> mwaShrink# mwa (wordArraySize# qb) s
         GT -> mwaSubInplaceArrayTrim mwa k qb s

      -- szQ-1 (the difference between the limb counts of A and B) gives the number of
      -- steps of the algorithm. For each step k from szQ-1 to 0 we compare the
      -- remainder (= A at the beginning) with B left-shifted of k limbs and we
      -- find how many shifted Bs can be subtracted from the remainder: this is
      -- qk (the k-th limb of the quotient).

      loop k s
         | isTrue# (k <# 0#) = s
         | True              = case computeQuot k s of
                                 s -> loop (k -# 1#) s


      -- Now the loop per se. Note that if A and B have the same number of
      -- limbs, the loop isn't executed at all.
      -- We have k iterating from (szQ-2) to 0. Each time we get qk and an
      -- updated R.
      -- First we compute an upper bound qMax for qk: qk <= qMax
      -- Then the idea is to iterate qe from qMax to 0 until
      --    qe*B k-left-shifted <= R
      --  or equivalently:
      --    qe*B <= R k-right-shifted
      --
      -- As multiplication is costly, we implement it as follows:
      --    qe = qMax
      --    c  = qe*B
      --    while c > R k-right-shifted do -- we don't perform the right-shift,
      --       c  -= B                     -- we have a dedicated "compare" op
      --       qe -= 1
      --    qk = qe
      --
      -- To determine qMax we compute:
      --    (qMaxCarry,qMax') = (R{k+szB},R{k+szB-1}) `div` B{szB-1}
      --    If qMaxCarry /= 0 then
      --       qMax = maxWordValue
      --    else
      --       qMax = qMax'
      -- qk can only be a single digit number because q(k+1) is already computed
      -- and the prefix of R is < B after the previous step.

      estimateQKMax k s =
            case mwaReadOrZero mwa (szB +# k) s of
               (# s2, ro1 #) -> case mwaReadOrZero mwa (szB +# k -# 1#) s2 of
                  (# s3, ro2 #) ->
                     -- most significant limb of b
                     let bmsw = wordArrayLast# b
                     in case quotRemWord3# (# ro1, ro2 #) bmsw of
                        (# (# qMaxCarry, qMax' #), _ #) -> case qMaxCarry of
                           0## -> (# s3, qMax'           #)
                           _   -> (# s3, WORD_MAXBOUND## #)



      -- compute qj and adjust the remainder
      computeQuot k s =
         case estimateQKMax k s of
            (# s, qe #) ->
               let
                  -- Now we can compute qe*b and check whether it is < (a >> k
                  -- limbs) If not, it means we need to decrease qe and recheck
                  -- until it is true.
                  -- Note: we only compute qe*b once and then we subtract b from
                  -- it.
                  vinit = bigNatMulWord# b qe

                  go qc c s = case mwaTrimCompare k mwa c s of

                     (# s, LT #)->
                        -- retry with a smaller quotient:
                        --    q <- q-1
                        --    c <- c-b
                        -- TODO: we could do the sub inplace
                        go (qc `minusWord#` 1##) (bigNatSubUnsafe c b) s

                     (# s, cmp #)-> case updateR k c cmp s of
                        s -> (# s, qc #)

               in case go qe vinit s of
                     (# s, q #) -> mwaWrite# mwq k q s

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
-- GHC.Num.Primitives.
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
bignat_powmod_word b0 e0 m = go (naturalFromBigNat b0) (naturalFromBigNat e0) (naturalFromWord# 1##)
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
      !r' = go (naturalFromBigNat b0)
               (naturalFromBigNat e0)
               (naturalFromWord# 1##)

      go !b e !r
        | isTrue# (e `naturalTestBit#` 0##)
        = go b' e' ((r `naturalMul` b) `naturalRem` m')

        | naturalIsZero e
        = naturalToBigNat r

        | True
        = go b' e' r
        where
          b' = (b `naturalMul` b) `naturalRem` m'
          m' = naturalFromBigNat m
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
