{-# LANGUAGE CPP, MagicHash, UnboxedTuples, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"

-- Fast integer logarithms to base 2.
-- integerLog2# and wordLog2# are of general usefulness,
-- the others are only needed for a fast implementation of
-- fromRational.
-- Since they are needed in GHC.Float, we must expose this
-- module, but it should not show up in the docs.

module GHC.Integer.Logarithms.Internals
    ( integerLog2#
    , integerLog2IsPowerOf2#
    , wordLog2#
    , roundingMode#
    ) where

import GHC.Prim
import GHC.Types (isTrue#)
import GHC.Integer.Type

-- When larger word sizes become common, add support for those,
-- it is not hard, just tedious.
#if (WORD_SIZE_IN_BITS != 32) && (WORD_SIZE_IN_BITS != 64)

-- Less than ideal implementations for strange word sizes

import GHC.Integer

default ()

-- We do not know whether the word has 30 bits or 128 or even more,
-- so we cannot start from the top, although that would be much more
-- efficient.
-- Count the bits until the highest set bit is found.
wordLog2# :: Word# -> Int#
wordLog2# w = go 8# w
  where
    go acc u = case u `uncheckedShiftRL#` 8# of
                0## -> case leadingZeros of
                        BA ba -> acc -# indexInt8Array# ba (word2Int# u)
                v   -> go (acc +# 8#) v

-- Assumption: Integer is strictly positive
integerLog2# :: Integer -> Int#
integerLog2# (S# i) = wordLog2# (int2Word# i) -- that is easy
integerLog2# m = case step m (smallInteger 2#) 1# of
                    (# _, l #) -> l
  where
    -- Invariants:
    -- pw = 2 ^ lg
    -- case step n pw lg of
    --   (q, e) -> pw^(2*e) <= n < pw^(2*e+2)
    --              && q <= n/pw^(2*e) < (q+1)
    --              && q < pw^2
    step n pw lg =
      if n `ltInteger` pw
        then (# n, 0# #)
        else case step n (shiftLInteger pw lg) (2# *# lg) of
              (# q, e #) ->
                if q `ltInteger` pw
                  then (# q, 2# *# e #)
                  else (# q `shiftRInteger` lg, 2# *# e +# 1# #)

-- Calculate the log2 of a positive integer and check
-- whether it is a power of 2.
-- By coincidence, the presence of a power of 2 is
-- signalled by zero and not one.
integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)
integerLog2IsPowerOf2# m =
    case integerLog2# m of
      lg -> if m `eqInteger` (smallInteger 1# `shiftLInteger` lg)
              then (# lg, 0# #)
              else (# lg, 1# #)

-- Detect the rounding mode,
-- 0# means round to zero,
-- 1# means round to even,
-- 2# means round away from zero
roundingMode# :: Integer -> Int# -> Int#
roundingMode# m h =
    case smallInteger 1# `shiftLInteger` h of
      c -> case m `andInteger`
                ((c `plusInteger` c) `minusInteger` smallInteger 1#) of
             r ->
               if c `ltInteger` r
                 then 2#
                 else if c `gtInteger` r
                        then 0#
                        else 1#

#else

default ()

-- We have a nice word size, we can do much better now.

#if WORD_SIZE_IN_BITS == 32

#define WSHIFT 5
#define MMASK 31

#else

#define WSHIFT 6
#define MMASK 63

#endif

-- Assumption: Integer is strictly positive
-- For small integers, use wordLog#,
-- in the general case, check words from the most
-- significant down, once a nonzero word is found,
-- calculate its log2 and add the number of following bits.
integerLog2# :: Integer -> Int#
integerLog2# (S# i) = wordLog2# (int2Word# i)
integerLog2# (J# s ba) = check (s -# 1#)
  where
    check i = case indexWordArray# ba i of
                0## -> check (i -# 1#)
                w   -> wordLog2# w +# (uncheckedIShiftL# i WSHIFT#)

-- Assumption: Integer is strictly positive
-- First component is log2 n, second is 0# iff n is a power of two
integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)
-- The power of 2 test is n&(n-1) == 0, thus powers of 2
-- are indicated bythe second component being zero.
integerLog2IsPowerOf2# (S# i) =
    case int2Word# i of
      w -> (# wordLog2# w, word2Int# (w `and#` (w `minusWord#` 1##)) #)
-- Find the log2 as above, test whether that word is a power
-- of 2, if so, check whether only zero bits follow.
integerLog2IsPowerOf2# (J# s ba) = check (s -# 1#)
  where
    check :: Int# -> (# Int#, Int# #)
    check i = case indexWordArray# ba i of
                0## -> check (i -# 1#)
                w   -> (# wordLog2# w +# (uncheckedIShiftL# i WSHIFT#)
                        , case w `and#` (w `minusWord#` 1##) of
                            0## -> test (i -# 1#)
                            _   -> 1# #)
    test :: Int# -> Int#
    test i = if isTrue# (i <# 0#)
                then 0#
                else case indexWordArray# ba i of
                        0## -> test (i -# 1#)
                        _   -> 1#

-- Assumption: Integer and Int# are strictly positive, Int# is less
-- than logBase 2 of Integer, otherwise havoc ensues.
-- Used only for the numerator in fromRational when the denominator
-- is a power of 2.
-- The Int# argument is log2 n minus the number of bits in the mantissa
-- of the target type, i.e. the index of the first non-integral bit in
-- the quotient.
--
-- 0# means round down (towards zero)
-- 1# means we have a half-integer, round to even
-- 2# means round up (away from zero)
roundingMode# :: Integer -> Int# -> Int#
roundingMode# (S# i) t =
    case int2Word# i `and#` ((uncheckedShiftL# 2## t) `minusWord#` 1##) of
      k -> case uncheckedShiftL# 1## t of
            c -> if isTrue# (c `gtWord#` k)
                    then 0#
                    else if isTrue# (c `ltWord#` k)
                            then 2#
                            else 1#
roundingMode# (J# _ ba) t =
    case word2Int# (int2Word# t `and#` MMASK##) of
      j ->      -- index of relevant bit in word
        case uncheckedIShiftRA# t WSHIFT# of
          k ->  -- index of relevant word
            case indexWordArray# ba k `and#`
                    ((uncheckedShiftL# 2## j) `minusWord#` 1##) of
              r ->
                case uncheckedShiftL# 1## j of
                  c -> if isTrue# (c `gtWord#` r)
                        then 0#
                        else if isTrue# (c `ltWord#` r)
                                then 2#
                                else test (k -# 1#)
  where
    test i = if isTrue# (i <# 0#)
                then 1#
                else case indexWordArray# ba i of
                        0## -> test (i -# 1#)
                        _   -> 2#

-- wordLog2# 0## = -1#
{-# INLINE wordLog2# #-}
wordLog2# :: Word# -> Int#
wordLog2# w =
  case leadingZeros of
   BA lz ->
    let zeros u = indexInt8Array# lz (word2Int# u) in
#if WORD_SIZE_IN_BITS == 64
    case uncheckedShiftRL# w 56# of
     a ->
      if isTrue# (a `neWord#` 0##)
       then 64# -# zeros a
       else
        case uncheckedShiftRL# w 48# of
         b ->
          if isTrue# (b `neWord#` 0##)
           then 56# -# zeros b
           else
            case uncheckedShiftRL# w 40# of
             c ->
              if isTrue# (c `neWord#` 0##)
               then 48# -# zeros c
               else
                case uncheckedShiftRL# w 32# of
                 d ->
                  if isTrue# (d `neWord#` 0##)
                   then 40# -# zeros d
                   else
#endif
                    case uncheckedShiftRL# w 24# of
                     e ->
                      if isTrue# (e `neWord#` 0##)
                       then 32# -# zeros e
                       else
                        case uncheckedShiftRL# w 16# of
                         f ->
                          if isTrue# (f `neWord#` 0##)
                           then 24# -# zeros f
                           else
                            case uncheckedShiftRL# w 8# of
                             g ->
                              if isTrue# (g `neWord#` 0##)
                               then 16# -# zeros g
                               else 8# -# zeros w

#endif

-- Lookup table
data BA = BA ByteArray#

leadingZeros :: BA
leadingZeros =
    let mkArr s =
          case newByteArray# 256# s of
            (# s1, mba #) ->
              case writeInt8Array# mba 0# 9# s1 of
                s2 ->
                  let fillA lim val idx st =
                        if isTrue# (idx ==# 256#)
                          then st
                          else if isTrue# (idx <# lim)
                                then case writeInt8Array# mba idx val st of
                                        nx -> fillA lim val (idx +# 1#) nx
                                else fillA (2# *# lim) (val -# 1#) idx st
                  in case fillA 2# 8# 1# s2 of
                      s3 -> case unsafeFreezeByteArray# mba s3 of
                              (# _, ba #) -> ba
    in case mkArr realWorld# of
        b -> BA b
