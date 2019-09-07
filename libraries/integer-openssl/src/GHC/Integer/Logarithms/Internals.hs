{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  GHC.Integer.Logarithm.Internals
-- License     :  BSD3
-- Maintainer  :  sebastian.nagel@ncoding.at
--
-- Logarithm functions not part of the main GHC.Integer interface and only
-- intended to be used by 'base'. Implementation is identical to integer-gmp.
--
module GHC.Integer.Logarithms.Internals where

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 32
# define WSHIFT 5
# define MMASK 31
#elif WORD_SIZE_IN_BITS == 64
# define WSHIFT 6
# define MMASK 63
#else
# error unsupported WORD_SIZE_IN_BITS
#endif

import GHC.Integer.Type
import GHC.Integer.Logarithms

import GHC.Types
import GHC.Prim

-- | Extended version of 'integerLog2#'
--
-- Assumption: Integer is strictly positive
--
-- First component of result is @log2 n@, second is @0#@ iff /n/ is a
-- power of two.
integerLog2IsPowerOf2# :: Integer -> (# Int#, Int# #)
-- The power of 2 test is n&(n-1) == 0, thus powers of 2
-- are indicated bythe second component being zero.
integerLog2IsPowerOf2# (S# i#) = case int2Word# i# of
      w -> (# wordLog2# w, word2Int# (w `and#` (w `minusWord#` 1##)) #)
integerLog2IsPowerOf2# (Bn# _) = (# -1#, -1# #)
-- Find the log2 as above, test whether that word is a power
-- of 2, if so, check whether only zero bits follow.
integerLog2IsPowerOf2# (Bp# bn) = check (s -# 1#)
  where
    s = wordsInBigNum# bn
    check :: Int# -> (# Int#, Int# #)
    check i = case indexBigNum# bn i of
                0## -> check (i -# 1#)
                w   -> (# wordLog2# w +# (uncheckedIShiftL# i WSHIFT#)
                        , case w `and#` (w `minusWord#` 1##) of
                            0## -> test (i -# 1#)
                            _   -> 1# #)
    test :: Int# -> Int#
    test i = if isTrue# (i <# 0#)
                then 0#
                else case indexBigNum# bn i of
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
roundingMode# (S# i#) t =
    case int2Word# i# `and#` ((uncheckedShiftL# 2## t) `minusWord#` 1##) of
      k -> case uncheckedShiftL# 1## t of
            c -> if isTrue# (c `gtWord#` k)
                    then 0#
                    else if isTrue# (c `ltWord#` k)
                            then 2#
                            else 1#

roundingMode# (Bn# bn) t = roundingMode# (Bp# bn) t -- dummy
roundingMode# (Bp# bn) t
  | isTrue# (c `gtWord#` r) = 0#
  | isTrue# (c `ltWord#` r) = 2#
  | True = test (k -# 1#)
  where
    j = word2Int# (int2Word# t `and#` MMASK##) -- index of relevant bit in word
    k = uncheckedIShiftRA# t WSHIFT# -- index of relevant word
    r = indexBigNum# bn k `and#` ((uncheckedShiftL# 2## j) `minusWord#` 1##)
    c = uncheckedShiftL# 1## j
    test i = if isTrue# (i <# 0#)
             then 1#
             else case indexBigNum# bn i of
                    0## -> test (i -# 1#)
                    _   -> 2#
