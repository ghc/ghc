-- |
-- Module      : Data.Memory.Internal.CompatPrim
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Compat
--
-- This module try to keep all the difference between versions of ghc primitive
-- or other needed packages, so that modules don't need to use CPP.
--
-- Note that MagicHash and CPP conflicts in places, making it "more interesting"
-- to write compat code for primitives
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
#include "MachDeps.h"
module Data.Memory.Internal.CompatPrim64
    ( Word64#
    , Int64#
    , eqInt64#
    , neInt64#
    , ltInt64#
    , leInt64#
    , gtInt64#
    , geInt64#
    , quotInt64#
    , remInt64#
    , eqWord64#
    , neWord64#
    , ltWord64#
    , leWord64#
    , gtWord64#
    , geWord64#
    , and64#
    , or64#
    , xor64#
    , not64#
    , timesWord64#
    , uncheckedShiftL64#
    , uncheckedShiftRL64#

    , int64ToWord64#
    , word64ToInt64#
    , intToInt64#
    , int64ToInt#
    , wordToWord64#
    , word64ToWord#
    , w64#
    ) where


#if WORD_SIZE_IN_BITS == 64
import GHC.Prim hiding (Word64#, Int64#)

#if __GLASGOW_HASKELL__ >= 708
type OutBool = Int#
#else
type OutBool = Bool
#endif

type Word64# = Word#
type Int64# = Int#

eqWord64# :: Word64# -> Word64# -> OutBool
eqWord64# = eqWord#

neWord64# :: Word64# -> Word64# -> OutBool
neWord64# = neWord#

ltWord64# :: Word64# -> Word64# -> OutBool
ltWord64# = ltWord#

leWord64# :: Word64# -> Word64# -> OutBool
leWord64# = leWord#

gtWord64# :: Word64# -> Word64# -> OutBool
gtWord64# = gtWord#

geWord64# :: Word64# -> Word64# -> OutBool
geWord64# = geWord#

eqInt64# :: Int64# -> Int64# -> OutBool
eqInt64# = (==#)

neInt64# :: Int64# -> Int64# -> OutBool
neInt64# = (/=#)

ltInt64# :: Int64# -> Int64# -> OutBool
ltInt64# = (<#)

leInt64# :: Int64# -> Int64# -> OutBool
leInt64# = (<=#)

gtInt64# :: Int64# -> Int64# -> OutBool
gtInt64# = (>#)

geInt64# :: Int64# -> Int64# -> OutBool
geInt64# = (<=#)

quotInt64# :: Int64# -> Int64# -> Int64#
quotInt64# = quotInt#

remInt64# :: Int64# -> Int64# -> Int64#
remInt64# = remInt#

and64# :: Word64# -> Word64# -> Word64#
and64# = and#

or64# :: Word64# -> Word64# -> Word64#
or64# = or#

xor64# :: Word64# -> Word64# -> Word64#
xor64# = xor#

not64# :: Word64# -> Word64#
not64# = not#

uncheckedShiftL64# :: Word64# -> Int# -> Word64#
uncheckedShiftL64# = uncheckedShiftL#

uncheckedShiftRL64#  :: Word64# -> Int# -> Word64#
uncheckedShiftRL64# = uncheckedShiftL#

int64ToWord64# :: Int64# -> Word64#
int64ToWord64# = int2Word#

word64ToInt64# :: Word64# -> Int64#
word64ToInt64# = word2Int#

intToInt64# :: Int# -> Int64#
intToInt64# w = w

int64ToInt# :: Int64# -> Int#
int64ToInt# w = w

wordToWord64# :: Word# -> Word64#
wordToWord64# w = w

word64ToWord# :: Word64# -> Word#
word64ToWord# w = w

timesWord64# :: Word64# -> Word64# -> Word64#
timesWord64# = timesWord#

w64# :: Word# -> Word# -> Word# -> Word64#
w64# w _ _ = w

#elif WORD_SIZE_IN_BITS == 32
import GHC.IntWord64
import GHC.Prim (Word#)

timesWord64# :: Word64# -> Word64# -> Word64#
timesWord64# a b =
    let !ai = word64ToInt64# a
        !bi = word64ToInt64# b
     in int64ToWord64# (timesInt64# ai bi)

w64# :: Word# -> Word# -> Word# -> Word64#
w64# _ hw lw =
    let !h = wordToWord64# hw
        !l = wordToWord64# lw
     in or64# (uncheckedShiftL64# h 32#) l
#else
#error "not a supported architecture. supported WORD_SIZE_IN_BITS is 32 bits or 64 bits"
#endif
