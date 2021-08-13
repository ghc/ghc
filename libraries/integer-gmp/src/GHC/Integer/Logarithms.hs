{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE GHCForeignImportPrim #-}
#endif

module GHC.Integer.Logarithms
    ( wordLog2#
    , integerLog2#
    , integerLogBase#
    ) where

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 32
# define LD_WORD_SIZE_IN_BITS 5
#elif WORD_SIZE_IN_BITS == 64
# define LD_WORD_SIZE_IN_BITS 6
#else
# error unsupported WORD_SIZE_IN_BITS
#endif

import GHC.Integer.Type

import GHC.Prim
#ifdef ghcjs_HOST_OS
import GHC.Types
#endif

default ()

-- | Calculate the integer logarithm for an arbitrary base.
--
-- The base must be greater than @1@, the second argument, the number
-- whose logarithm is sought, shall be positive, otherwise the
-- result is meaningless.
--
-- The following property holds
--
-- @base ^ 'integerLogBase#' base m <= m < base ^('integerLogBase#' base m + 1)@
--
-- for @base > 1@ and @m > 0@.
--
-- Note: Internally uses 'integerLog2#' for base 2
integerLogBase# :: Integer -> Integer -> Int#
-- #if defined(ghcjs_HOST_OS)
-- integerLogBase# x y = js_integerLogBase (unsafeCoerce x) (unsafeCoerce y)
-- foreign import javascript unsafe "h$integer_integerLogBase($1,$2)" js_integerLogBase :: Any -> Any -> Int#
-- #else
integerLogBase# (S# 2#) m = integerLog2# m
integerLogBase# b m = e'
  where
    !(# _, e' #) = go b

    go pw | m `ltInteger` pw = (# m, 0# #)
    go pw = case go (sqrInteger pw) of
              (# q, e #) | q `ltInteger` pw -> (# q, 2# *# e #)
              (# q, e #) -> (# q `quotInteger` pw, 2# *# e +# 1# #)
-- #endif

-- | Calculate the integer base 2 logarithm of an 'Integer'.  The
-- calculation is more efficient than for the general case, on
-- platforms with 32- or 64-bit words much more efficient.
--
-- The argument must be strictly positive, that condition is /not/ checked.
integerLog2# :: Integer -> Int#
#if defined(ghcjs_HOST_OS)
integerLog2# x = js_integerLog2 (unsafeCoerce# x)
foreign import javascript unsafe "h$ghcjsbn_integerLog2($1)" js_integerLog2 :: Any -> Int#
#else
integerLog2# (S# i#) = wordLog2# (int2Word# i#)
integerLog2# (Jn#  _) = -1#
integerLog2# (Jp# bn) = go (s -# 1#)
  where
    s = sizeofBigNat# bn
    go i = case indexBigNat# bn i of
               0## -> go (i -# 1#)
               w   -> wordLog2# w +# (uncheckedIShiftL# i LD_WORD_SIZE_IN_BITS#)
#endif

-- | Compute base-2 log of 'Word#'
--
-- This is internally implemented as count-leading-zeros machine instruction.
wordLog2# :: Word# -> Int#
wordLog2# w# = (WORD_SIZE_IN_BITS# -# 1#) -# (word2Int# (clz# w#))
