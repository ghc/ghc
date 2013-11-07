{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude, UnboxedTuples
           , UnliftedFFITypes, GHCForeignImportPrim #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"
module GHC.Integer.GMP.Prim (
    cmpInteger#,
    cmpIntegerInt#,

    plusInteger#,
    minusInteger#,
    timesInteger#,

    quotRemInteger#,
    quotInteger#,
    remInteger#,
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
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_minusIntegerzh" minusInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_timesIntegerzh" timesInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- | Compute div and mod simultaneously, where div rounds towards negative
-- infinity and\ @(q,r) = divModInteger#(x,y)@ implies
-- @plusInteger# (timesInteger# q y) r = x@.
--
foreign import prim "integer_cmm_quotRemIntegerzh" quotRemInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray#, Int#, ByteArray# #)

-- | Rounds towards zero.
--
foreign import prim "integer_cmm_quotIntegerzh" quotInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- | Satisfies \texttt{plusInteger\# (timesInteger\# (quotInteger\# x y) y) (remInteger\# x y) == x}.
--
foreign import prim "integer_cmm_remIntegerzh" remInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- | Compute div and mod simultaneously, where div rounds towards negative infinity
-- and\texttt{(q,r) = divModInteger\#(x,y)} implies \texttt{plusInteger\# (timesInteger\# q y) r = x}.
--
foreign import prim "integer_cmm_divModIntegerzh" divModInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray#, Int#, ByteArray# #)
foreign import prim "integer_cmm_divIntegerzh" divInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)
foreign import prim "integer_cmm_modIntegerzh" modInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- | Divisor is guaranteed to be a factor of dividend.
--
foreign import prim "integer_cmm_divExactIntegerzh" divExactInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- | Greatest common divisor.
--
foreign import prim "integer_cmm_gcdIntegerzh" gcdInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- | Extended greatest common divisor.
--
foreign import prim "integer_cmm_gcdExtIntegerzh" gcdExtInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray#, Int#, ByteArray# #)

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
  :: Double# -> (# Int#, Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_int2Integerzh" int2Integer#
  :: Int# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_word2Integerzh" word2Integer#
  :: Word# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_andIntegerzh" andInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_orIntegerzh" orInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_xorIntegerzh" xorInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_testBitIntegerzh" testBitInteger#
  :: Int# -> ByteArray# -> Int# -> Int#

-- |
--
foreign import prim "integer_cmm_mul2ExpIntegerzh" mul2ExpInteger#
  :: Int# -> ByteArray# -> Int# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_fdivQ2ExpIntegerzh" fdivQ2ExpInteger#
  :: Int# -> ByteArray# -> Int# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_powIntegerzh" powInteger#
  :: Int# -> ByteArray# -> Word# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_powModIntegerzh" powModInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_powModSecIntegerzh" powModSecInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_recipModIntegerzh" recipModInteger#
  :: Int# -> ByteArray# -> Int# -> ByteArray# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_nextPrimeIntegerzh" nextPrimeInteger#
  :: Int# -> ByteArray# -> (# Int#, ByteArray# #)

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
  :: ByteArray# -> Word# -> Word# -> Int# -> (# Int#, ByteArray# #)

-- |
--
foreign import prim "integer_cmm_importIntegerFromAddrzh" importIntegerFromAddr#
  :: Addr# -> Word# -> Int# -> State# s -> (# State# s, Int#, ByteArray# #)

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
  :: Int# -> ByteArray# -> (# Int#, ByteArray# #)

#if WORD_SIZE_IN_BITS < 64
foreign import prim "integer_cmm_int64ToIntegerzh" int64ToInteger#
  :: Int64# -> (# Int#, ByteArray# #)

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
