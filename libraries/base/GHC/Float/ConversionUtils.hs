{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples, NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Float.ConversionUtils
-- Copyright   :  (c) Daniel Fischer 2010
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Utilities for conversion between Double/Float and Rational
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Float.ConversionUtils ( elimZerosInteger, elimZerosInt# ) where

import GHC.Base
import GHC.Num.Integer

default ()

#if WORD_SIZE_IN_BITS < 64

#define TO64    integerToInt64#

-- Double mantissae have 53 bits, too much for Int#
elim64# :: Int64# -> Int# -> (# Integer, Int# #)
elim64# n e =
    case zeroCount (int64ToInt# n) of
      t | isTrue# (e <=# t) -> (# integerFromInt64# (uncheckedIShiftRA64# n e), 0# #)
        | isTrue# (t <# 8#) -> (# integerFromInt64# (uncheckedIShiftRA64# n t), e -# t #)
        | otherwise         -> elim64# (uncheckedIShiftRA64# n 8#) (e -# 8#)

#else

#define TO64    integerToInt#

-- Double mantissae fit it Int#
elim64# :: Int# -> Int# -> (# Integer, Int# #)
elim64# = elimZerosInt#

#endif

{-# INLINE elimZerosInteger #-}
elimZerosInteger :: Integer -> Int# -> (# Integer, Int# #)
elimZerosInteger m e = elim64# (TO64 m) e

elimZerosInt# :: Int# -> Int# -> (# Integer, Int# #)
elimZerosInt# n e =
    case zeroCount n of
      t | isTrue# (e <=# t) -> (# IS (uncheckedIShiftRA# n e), 0# #)
        | isTrue# (t <# 8#) -> (# IS (uncheckedIShiftRA# n t), e -# t #)
        | otherwise         -> elimZerosInt# (uncheckedIShiftRA# n 8#) (e -# 8#)

-- | Number of trailing zero bits in a byte
zeroCount :: Int# -> Int#
zeroCount i = int8ToInt# (indexInt8OffAddr# arr (word2Int# (narrow8Word# (int2Word# i)))) -- index must be in [0,255]
  where
    arr = "\8\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\4\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\5\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\4\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\6\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\4\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\5\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\4\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\7\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\4\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\5\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\4\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\6\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\4\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\5\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0\4\0\1\0\2\0\1\0\3\0\1\0\2\0\1\0"#
