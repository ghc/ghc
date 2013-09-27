{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples, NoImplicitPrelude #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_HADDOCK hide #-}

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
import GHC.Integer
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

default ()

#if WORD_SIZE_IN_BITS < 64

#define TO64    integerToInt64

toByte64# :: Int64# -> Int#
toByte64# i = word2Int# (and# 255## (int2Word# (int64ToInt# i)))

-- Double mantissae have 53 bits, too much for Int#
elim64# :: Int64# -> Int# -> (# Integer, Int# #)
elim64# n e =
    case zeroCount (toByte64# n) of
      t | isTrue# (e <=# t) -> (# int64ToInteger (uncheckedIShiftRA64# n e), 0# #)
        | isTrue# (t <# 8#) -> (# int64ToInteger (uncheckedIShiftRA64# n t), e -# t #)
        | otherwise         -> elim64# (uncheckedIShiftRA64# n 8#) (e -# 8#)

#else

#define TO64    integerToInt

-- Double mantissae fit it Int#
elim64# :: Int# -> Int# -> (# Integer, Int# #)
elim64# = elimZerosInt#

#endif

{-# INLINE elimZerosInteger #-}
elimZerosInteger :: Integer -> Int# -> (# Integer, Int# #)
elimZerosInteger m e = elim64# (TO64 m) e

elimZerosInt# :: Int# -> Int# -> (# Integer, Int# #)
elimZerosInt# n e =
    case zeroCount (toByte# n) of
      t | isTrue# (e <=# t) -> (# smallInteger (uncheckedIShiftRA# n e), 0# #)
        | isTrue# (t <# 8#) -> (# smallInteger (uncheckedIShiftRA# n t), e -# t #)
        | otherwise         -> elimZerosInt# (uncheckedIShiftRA# n 8#) (e -# 8#)

{-# INLINE zeroCount #-}
zeroCount :: Int# -> Int#
zeroCount i =
    case zeroCountArr of
      BA ba -> indexInt8Array# ba i

toByte# :: Int# -> Int#
toByte# i = word2Int# (and# 255## (int2Word# i))


data BA = BA ByteArray#

-- Number of trailing zero bits in a byte
zeroCountArr :: BA
zeroCountArr =
    let mkArr s =
          case newByteArray# 256# s of
            (# s1, mba #) ->
              case writeInt8Array# mba 0# 8# s1 of
                s2 ->
                  let fillA step val idx st
                        | isTrue# (idx <# 256#) =
                                        case writeInt8Array# mba idx val st of
                                          nx -> fillA step val (idx +# step) nx
                        | isTrue# (step <# 256#) =
                                        fillA (2# *# step) (val +# 1#) step  st
                        | otherwise   = st
                  in case fillA 2# 0# 1# s2 of
                       s3 -> case unsafeFreezeByteArray# mba s3 of
                                (# _, ba #) -> ba
    in case mkArr realWorld# of
        b -> BA b

