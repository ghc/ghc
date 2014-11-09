{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_HADDOCK hide #-}

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

-- | Fast 'Integer' logarithms to base 2.  'integerLog2#' and
-- 'wordLog2#' are of general usefulness, the others are only needed
-- for a fast implementation of 'fromRational'.  Since they are needed
-- in "GHC.Float", we must expose this module, but it should not show
-- up in the docs.
--
-- See https://ghc.haskell.org/trac/ghc/ticket/5122
-- for the origin of the code in this module
module GHC.Integer.Logarithms.Internals
    ( wordLog2#
    , integerLog2IsPowerOf2#
    , integerLog2#
    , roundingMode#
    ) where

import GHC.Integer.Type
import GHC.Integer.Logarithms

import GHC.Types
import GHC.Prim

default ()

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
integerLog2IsPowerOf2# (Jn# _) = (# -1#, -1# #)
-- Find the log2 as above, test whether that word is a power
-- of 2, if so, check whether only zero bits follow.
integerLog2IsPowerOf2# (Jp# bn) = check (s -# 1#)
  where
    s = sizeofBigNat# bn
    check :: Int# -> (# Int#, Int# #)
    check i = case indexBigNat# bn i of
                0## -> check (i -# 1#)
                w   -> (# wordLog2# w +# (uncheckedIShiftL# i WSHIFT#)
                        , case w `and#` (w `minusWord#` 1##) of
                            0## -> test (i -# 1#)
                            _   -> 1# #)
    test :: Int# -> Int#
    test i = if isTrue# (i <# 0#)
                then 0#
                else case indexBigNat# bn i of
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

roundingMode# (Jn# bn) t = roundingMode# (Jp# bn) t -- dummy
roundingMode# (Jp# bn) t =
    case word2Int# (int2Word# t `and#` MMASK##) of
      j ->      -- index of relevant bit in word
        case uncheckedIShiftRA# t WSHIFT# of
          k ->  -- index of relevant word
            case indexBigNat# bn k `and#`
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
                else case indexBigNat# bn i of
                        0## -> test (i -# 1#)
                        _   -> 2#
