{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

#include "MachDeps.h"

-- |
-- Module      :  GHC.Integer.Type
-- Copyright   :  (c) Herbert Valerio Riedel 2014
-- License     :  BSD3
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Integer' type.
--
-- This module exposes the /portable/ 'Integer' API.  See
-- "GHC.Integer.GMP.Internals" for the @integer-gmp@-specific internal
-- representation of 'Integer' as well as optimized GMP-specific
-- operations.

module GHC.Integer (
    Integer,

    -- * Construct 'Integer's
    smallInteger, wordToInteger,
#if WORD_SIZE_IN_BITS < 64
    word64ToInteger, int64ToInteger,
#endif
    -- * Conversion to other integral types
    integerToWord, integerToInt,
#if WORD_SIZE_IN_BITS < 64
    integerToWord64, integerToInt64,
#endif

    -- * Helpers for 'RealFloat' type-class operations
    encodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,

    -- * Arithmetic operations
    plusInteger, minusInteger, timesInteger, negateInteger,
    absInteger, signumInteger,

    divModInteger, divInteger, modInteger,
    quotRemInteger, quotInteger, remInteger,

    -- * Comparison predicates
    eqInteger,  neqInteger,  leInteger,  gtInteger,  ltInteger,  geInteger,
    compareInteger,

    -- ** 'Int#'-boolean valued versions of comparison predicates
    --
    -- | These operations return @0#@ and @1#@ instead of 'False' and
    -- 'True' respectively.  See
    -- <https://gitlab.haskell.org/ghc/ghc/wikis/prim-bool PrimBool wiki-page>
    -- for more details
    eqInteger#, neqInteger#, leInteger#, gtInteger#, ltInteger#, geInteger#,


    -- * Bit-operations
    andInteger, orInteger, xorInteger,

    complementInteger,
    shiftLInteger, shiftRInteger, testBitInteger,

    popCountInteger, bitInteger,

    -- * Hashing
    hashInteger,
    ) where

import GHC.Num.Integer hiding (integerToWord,integerToInt)
import GHC.Prim
import GHC.Types

{-# DEPRECATED smallInteger "Use integerFromInt# instead" #-}
smallInteger :: Int# -> Integer
smallInteger = integerFromInt#

{-# DEPRECATED integerToInt "Use integerToInt# instead" #-}
integerToInt :: Integer -> Int#
integerToInt = integerToInt#

{-# DEPRECATED wordToInteger "Use integerFromWord# instead" #-}
wordToInteger :: Word# -> Integer
wordToInteger = integerFromWord#

{-# DEPRECATED integerToWord "Use integerToWord# instead" #-}
integerToWord :: Integer -> Word#
integerToWord = integerToWord#



#if WORD_SIZE_IN_BITS < 64

{-# DEPRECATED word64ToInteger "Use integerFromWord64# instead" #-}
word64ToInteger :: Word64# -> Integer
word64ToInteger = integerFromWord64#

{-# DEPRECATED integerToWord64 "Use integerToWord64# instead" #-}
integerToWord64 :: Integer -> Word64#
integerToWord64 = integerToWord64#

{-# DEPRECATED int64ToInteger "Use integerFromInt64# instead" #-}
int64ToInteger :: Int64# -> Integer
int64ToInteger = integerFromInt64#

{-# DEPRECATED integerToInt64 "Use integerToInt64# instead" #-}
integerToInt64 :: Integer -> Int64#
integerToInt64 = integerToInt64#

#endif


{-# DEPRECATED encodeFloatInteger "Use integerEncodeFloat# instead" #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger = integerEncodeFloat#

{-# DEPRECATED floatFromInteger "Use integerToFloat# instead" #-}
floatFromInteger :: Integer -> Float#
floatFromInteger = integerToFloat#

{-# DEPRECATED encodeDoubleInteger "Use integerEncodeDouble# instead" #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger = integerEncodeDouble#

{-# DEPRECATED doubleFromInteger "Use integerToDouble# instead" #-}
doubleFromInteger :: Integer -> Double#
doubleFromInteger = integerToDouble#

{-# DEPRECATED decodeDoubleInteger "Use integerDecodeDouble# instead" #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger = integerDecodeDouble#



{-# DEPRECATED plusInteger "Use integerAdd instead" #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger = integerAdd

{-# DEPRECATED minusInteger "Use integerSub instead" #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger = integerSub

{-# DEPRECATED timesInteger "Use integerMul instead" #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger = integerMul

{-# DEPRECATED negateInteger "Use integerNegate instead" #-}
negateInteger :: Integer -> Integer
negateInteger = integerNegate

{-# DEPRECATED absInteger "Use integerAbs instead" #-}
absInteger :: Integer -> Integer
absInteger = integerAbs

{-# DEPRECATED signumInteger "Use integerSignum instead" #-}
signumInteger :: Integer -> Integer
signumInteger = integerSignum

{-# DEPRECATED divModInteger "Use integerDivMod# instead" #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger = integerDivMod#

{-# DEPRECATED divInteger "Use integerDiv instead" #-}
divInteger :: Integer -> Integer -> Integer
divInteger = integerDiv

{-# DEPRECATED modInteger "Use integerMod instead" #-}
modInteger :: Integer -> Integer -> Integer
modInteger = integerMod

{-# DEPRECATED quotRemInteger "Use integerQuotRem# instead" #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger = integerQuotRem#

{-# DEPRECATED quotInteger "Use integerQuot instead" #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger = integerQuot

{-# DEPRECATED remInteger "Use integerRem instead" #-}
remInteger :: Integer -> Integer -> Integer
remInteger = integerRem


{-# DEPRECATED eqInteger "Use integerEq instead" #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger = integerEq

{-# DEPRECATED neqInteger "Use integerNe instead" #-}
neqInteger :: Integer -> Integer -> Bool
neqInteger = integerNe

{-# DEPRECATED leInteger "Use integerLe instead" #-}
leInteger :: Integer -> Integer -> Bool
leInteger = integerLe

{-# DEPRECATED gtInteger "Use integerGt instead" #-}
gtInteger :: Integer -> Integer -> Bool
gtInteger = integerGt

{-# DEPRECATED ltInteger "Use integerLt instead" #-}
ltInteger :: Integer -> Integer -> Bool
ltInteger = integerLt

{-# DEPRECATED geInteger "Use integerGe instead" #-}
geInteger :: Integer -> Integer -> Bool
geInteger = integerGe

{-# DEPRECATED compareInteger "Use integerCompare instead" #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger = integerCompare



{-# DEPRECATED eqInteger# "Use integerEq# instead" #-}
eqInteger# :: Integer -> Integer -> Int#
eqInteger# = integerEq#

{-# DEPRECATED neqInteger# "Use integerNe# instead" #-}
neqInteger# :: Integer -> Integer -> Int#
neqInteger# = integerNe#

{-# DEPRECATED leInteger# "Use integerLe# instead" #-}
leInteger# :: Integer -> Integer -> Int#
leInteger# = integerLe#

{-# DEPRECATED gtInteger# "Use integerGt# instead" #-}
gtInteger# :: Integer -> Integer -> Int#
gtInteger# = integerGt#

{-# DEPRECATED ltInteger# "Use integerLt# instead" #-}
ltInteger# :: Integer -> Integer -> Int#
ltInteger# = integerLt#

{-# DEPRECATED geInteger# "Use integerGe# instead" #-}
geInteger# :: Integer -> Integer -> Int#
geInteger# = integerGe#


{-# DEPRECATED andInteger "Use integerAnd instead" #-}
andInteger :: Integer -> Integer -> Integer
andInteger = integerAnd

{-# DEPRECATED orInteger "Use integerOr instead" #-}
orInteger :: Integer -> Integer -> Integer
orInteger = integerOr

{-# DEPRECATED xorInteger "Use integerXor instead" #-}
xorInteger :: Integer -> Integer -> Integer
xorInteger = integerXor

{-# DEPRECATED complementInteger "Use integerComplement instead" #-}
complementInteger :: Integer -> Integer
complementInteger = integerComplement

{-# DEPRECATED shiftLInteger "Use integerShiftL# instead" #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger n i = integerShiftL# n (int2Word# i)

{-# DEPRECATED shiftRInteger "Use integerShiftR# instead" #-}
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger n i = integerShiftR# n (int2Word# i)

{-# DEPRECATED testBitInteger "Use integerTestBit# instead" #-}
testBitInteger :: Integer -> Int# -> Bool
testBitInteger n i = isTrue# (integerTestBit# n (int2Word# i))

{-# DEPRECATED hashInteger "Use integerToInt# instead" #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt#

{-# DEPRECATED bitInteger "Use integerBit# instead" #-}
bitInteger :: Int# -> Integer
bitInteger i = integerBit# (int2Word# i)

{-# DEPRECATED popCountInteger "Use integerPopCount# instead" #-}
popCountInteger :: Integer -> Int#
popCountInteger = integerPopCount#

