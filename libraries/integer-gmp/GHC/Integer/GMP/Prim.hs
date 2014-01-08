{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude, UnboxedTuples
           , UnliftedFFITypes, GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"
module GHC.Integer.GMP.Prim (
    MPZ#,

    cmpInteger#,
    cmpIntegerInt#,

    plusInteger#,
    plusIntegerInt#,
    minusInteger#,
    minusIntegerInt#,
    timesInteger#,
    timesIntegerInt#,

    quotRemInteger#,
    quotRemIntegerWord#,
    quotInteger#,
    quotIntegerWord#,
    remInteger#,
    remIntegerWord#,

    divModInteger#,
    divInteger#,
    modInteger#,
    divExactInteger#,

    gcdInteger#,
    gcdExtInteger#,
    gcdIntegerInt#,
    gcdInt#,

    decodeDouble#,

    int2Integer#,
    integer2Int#,

    word2Integer#,
    integer2Word#,

    andInteger#,
    orInteger#,
    xorInteger#,
    complementInteger#,

    testBitInteger#,
    mul2ExpInteger#,
    fdivQ2ExpInteger#,

    powInteger#,
    powModInteger#,
    powModSecInteger#,
    recipModInteger#,

    nextPrimeInteger#,
    testPrimeInteger#,

    sizeInBaseInteger#,
    importIntegerFromByteArray#,
    importIntegerFromAddr#,
    exportIntegerToMutableByteArray#,
    exportIntegerToAddr#,

#if WORD_SIZE_IN_BITS < 64
    int64ToInteger#,  integerToInt64#,
    word64ToInteger#, integerToWord64#,
#endif

#ifndef WORD_SIZE_IN_BITS
#error WORD_SIZE_IN_BITS not defined!!!
#endif

  ) where

import GHC.Prim
import GHC.Types

-- Double isn't available yet, and we shouldn't be using defaults anyway:
default ()

-- | This is represents a @mpz_t@ value in a heap-saving way.
--
-- The first tuple element, @/s/@, encodes the sign of the integer
-- @/i/@ (i.e. @signum /s/ == signum /i/@), and the number of /limbs/
-- used to represent the magnitude. If @abs /s/ > 1@, the 'ByteArray#'
-- contains @abs /s/@ limbs encoding the integer. Otherwise, if @abs
-- /s/ < 2@, the single limb is stored in the 'Word#' element instead
-- (and the 'ByteArray#' element is undefined and MUST NOT be accessed
-- as it doesn't point to a proper 'ByteArray#' but rather to an
-- unsafe-coerced 'Int' in order be polite to the GC -- see
-- @DUMMY_BYTE_ARR@ in gmp-wrappers.cmm)
--
-- More specifically, the following encoding is used (where `⊥` means
-- undefined/unused):
--
-- * (#  0#, ⊥, 0## #) -> value = 0
-- * (#  1#, ⊥, w   #) -> value = w
-- * (# -1#, ⊥, w   #) -> value = -w
-- * (#  s#, d, 0## #) -> value = J# s d
--
-- This representation allows to avoid temporary heap allocations
-- (-> Trac #8647) of 1-limb 'ByteArray#'s which fit into the
-- 'S#'-constructor. Moreover, this allows to delays 1-limb
-- 'ByteArray#' heap allocations, as such 1-limb `mpz_t`s can be
-- optimistically allocated on the Cmm stack and returned as a @#word@
-- in case the `mpz_t` wasn't grown beyond 1 limb by the GMP
-- operation.
--
-- See also the 'GHC.Integer.Type.mpzToInteger' function which ought
-- to be used for converting 'MPZ#'s to 'Integer's and the
-- @MP_INT_1LIMB_RETURN()@ macro in @gmp-wrappers.cmm@ which
-- constructs 'MPZ#' values in the first place for implementation
-- details.
type MPZ# = (# Int#, ByteArray#, Word# #)

-- | Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument.
--
foreign import prim "integer_cmm_cmpIntegerzh" cmpInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> Int#

-- | Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument, which
--   is an ordinary Int\#.
foreign import prim "integer_cmm_cmpIntegerIntzh" cmpIntegerInt#
  :: Int# -> ByteArray# -> Int# -> Int#

-- |
--
foreign import prim "integer_cmm_plusIntegerzh" plusInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- | Optimized version of 'plusInteger#' for summing big-ints with small-ints
--
foreign import prim "integer_cmm_plusIntegerIntzh" plusIntegerInt#
  :: Int# -> ByteArray# -> Int# -> MPZ#

-- |
--
foreign import prim "integer_cmm_minusIntegerzh" minusInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- | Optimized version of 'minusInteger#' for substracting small-ints from big-ints
--
foreign import prim "integer_cmm_minusIntegerIntzh" minusIntegerInt#
  :: Int# -> ByteArray# -> Int# -> MPZ#

-- |
--
foreign import prim "integer_cmm_timesIntegerzh" timesInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- | Optimized version of 'timesInteger#' for multiplying big-ints with small-ints
--
foreign import prim "integer_cmm_timesIntegerIntzh" timesIntegerInt#
  :: Int# -> ByteArray# -> Int# -> MPZ#

-- | Compute div and mod simultaneously, where div rounds towards negative
-- infinity and\ @(q,r) = divModInteger#(x,y)@ implies
-- @plusInteger# (timesInteger# q y) r = x@.
--
foreign import prim "integer_cmm_quotRemIntegerzh" quotRemInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# MPZ#, MPZ# #)

-- | Variant of 'quotRemInteger#'
--
foreign import prim "integer_cmm_quotRemIntegerWordzh" quotRemIntegerWord#
  :: Int# -> ByteArray# -> Word# -> (# MPZ#, MPZ# #)

-- | Rounds towards zero.
--
foreign import prim "integer_cmm_quotIntegerzh" quotInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- | Rounds towards zero.
foreign import prim "integer_cmm_quotIntegerWordzh" quotIntegerWord#
  :: Int# -> ByteArray# -> Word# -> MPZ#

-- | Satisfies \texttt{plusInteger\# (timesInteger\# (quotInteger\# x y) y) (remInteger\# x y) == x}.
--
foreign import prim "integer_cmm_remIntegerzh" remInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- | Variant of 'remInteger#'
foreign import prim "integer_cmm_remIntegerWordzh" remIntegerWord#
  :: Int# -> ByteArray# -> Word# -> MPZ#

-- | Compute div and mod simultaneously, where div rounds towards negative infinity
-- and\texttt{(q,r) = divModInteger\#(x,y)} implies \texttt{plusInteger\# (timesInteger\# q y) r = x}.
--
foreign import prim "integer_cmm_divModIntegerzh" divModInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# MPZ#, MPZ# #)
foreign import prim "integer_cmm_divIntegerzh" divInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#
foreign import prim "integer_cmm_modIntegerzh" modInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- | Divisor is guaranteed to be a factor of dividend.
--
foreign import prim "integer_cmm_divExactIntegerzh" divExactInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- | Greatest common divisor.
--
foreign import prim "integer_cmm_gcdIntegerzh" gcdInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- | Extended greatest common divisor.
--
foreign import prim "integer_cmm_gcdExtIntegerzh" gcdExtInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# MPZ#, MPZ# #)

-- | Greatest common divisor, where second argument is an ordinary {\tt Int\#}.
--
foreign import prim "integer_cmm_gcdIntegerIntzh" gcdIntegerInt#
  :: Int# -> ByteArray# -> Int# -> Int#

-- |
--
foreign import prim "integer_cmm_gcdIntzh" gcdInt#
  :: Int# -> Int# -> Int#

-- | Convert to arbitrary-precision integer.
--    First {\tt Int\#} in result is the exponent; second {\tt Int\#} and {\tt ByteArray\#}
--  represent an {\tt Integer\#} holding the mantissa.
--
foreign import prim "integer_cmm_decodeDoublezh" decodeDouble#
  :: Double# -> (# Int#, MPZ# #)

-- |
--
-- Note: This primitive doesn't use 'MPZ#' because its purpose is to instantiate a 'J#'-value.
foreign import prim "integer_cmm_int2Integerzh" int2Integer#
  :: Int# -> (# Int#, ByteArray# #)

-- |
--
-- Note: This primitive doesn't use 'MPZ#' because its purpose is to instantiate a 'J#'-value.
foreign import prim "integer_cmm_word2Integerzh" word2Integer#
  :: Word# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_andIntegerzh" andInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- |
--
foreign import prim "integer_cmm_orIntegerzh" orInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- |
--
foreign import prim "integer_cmm_xorIntegerzh" xorInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- |
--
foreign import prim "integer_cmm_testBitIntegerzh" testBitInteger#
  :: Int# -> ByteArray# -> Int# -> Int#

-- |
--
foreign import prim "integer_cmm_mul2ExpIntegerzh" mul2ExpInteger#
  :: Int# -> ByteArray# -> Int# -> MPZ#

-- |
--
foreign import prim "integer_cmm_fdivQ2ExpIntegerzh" fdivQ2ExpInteger#
  :: Int# -> ByteArray# -> Int# -> MPZ#

-- |
--
foreign import prim "integer_cmm_powIntegerzh" powInteger#
  :: Int# -> ByteArray# -> Word# -> MPZ#

-- |
--
foreign import prim "integer_cmm_powModIntegerzh" powModInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- |
--
foreign import prim "integer_cmm_powModSecIntegerzh" powModSecInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- |
--
foreign import prim "integer_cmm_recipModIntegerzh" recipModInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#

-- |
--
foreign import prim "integer_cmm_nextPrimeIntegerzh" nextPrimeInteger#
  :: Int# -> ByteArray# -> MPZ#

-- |
--
foreign import prim "integer_cmm_testPrimeIntegerzh" testPrimeInteger#
  :: Int# -> ByteArray# -> Int# -> Int#

-- |
--
foreign import prim "integer_cmm_sizeInBasezh" sizeInBaseInteger#
  :: Int# -> ByteArray# -> Int# -> Word#

-- |
--
foreign import prim "integer_cmm_importIntegerFromByteArrayzh" importIntegerFromByteArray#
  :: ByteArray# -> Word# -> Word# -> Int# -> MPZ#

-- |
--
foreign import prim "integer_cmm_importIntegerFromAddrzh" importIntegerFromAddr#
  :: Addr# -> Word# -> Int# -> State# s -> (# State# s, MPZ# #)

-- |
--
foreign import prim "integer_cmm_exportIntegerToMutableByteArrayzh" exportIntegerToMutableByteArray#
  :: Int# -> ByteArray# -> MutableByteArray# s -> Word# -> Int# -> State# s -> (# State# s, Word# #)

-- |
--
foreign import prim "integer_cmm_exportIntegerToAddrzh" exportIntegerToAddr#
  :: Int# -> ByteArray# -> Addr# -> Int# -> State# s -> (# State# s, Word# #)

-- |
--
foreign import prim "integer_cmm_complementIntegerzh" complementInteger#
  :: Int# -> ByteArray# -> MPZ#

#if WORD_SIZE_IN_BITS < 64
-- Note: This primitive doesn't use 'MPZ#' because its purpose is to instantiate a 'J#'-value.
foreign import prim "integer_cmm_int64ToIntegerzh" int64ToInteger#
  :: Int64# -> (# Int#, ByteArray# #)

-- Note: This primitive doesn't use 'MPZ#' because its purpose is to instantiate a 'J#'-value.
foreign import prim "integer_cmm_word64ToIntegerzh" word64ToInteger#
  :: Word64# -> (# Int#, ByteArray# #)

foreign import ccall unsafe "hs_integerToInt64"
    integerToInt64#  :: Int# -> ByteArray# -> Int64#

foreign import ccall unsafe "hs_integerToWord64"
    integerToWord64# :: Int# -> ByteArray# -> Word64#
#endif

-- used to be primops:
integer2Int# :: Int# -> ByteArray# -> Int#
integer2Int# s d = if isTrue# (s ==# 0#)
                       then 0#
                       else let !v = indexIntArray# d 0# in
                            if isTrue# (s <# 0#)
                               then negateInt# v
                               else v

integer2Word# :: Int# -> ByteArray# -> Word#
integer2Word# s d = int2Word# (integer2Int# s d)
