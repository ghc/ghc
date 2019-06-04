{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_HADDOCK not-home #-}

#include "MachDeps.h"

-- | Compatibility module for pre ghc-bignum code.
module GHC.Integer (
    Integer,

    -- * Construct 'Integer's
    smallInteger, wordToInteger,
    word64ToInteger, int64ToInteger,

    -- * Conversion to other integral types
    integerToWord, integerToInt,
    integerToWord64, integerToInt64,

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

import GHC.Num.Integer (Integer)
import qualified GHC.Num.Integer as I
import GHC.Prim
import GHC.Types

smallInteger :: Int# -> Integer
smallInteger = I.integerFromInt#

integerToInt :: Integer -> Int#
integerToInt = I.integerToInt#

wordToInteger :: Word# -> Integer
wordToInteger = I.integerFromWord#

integerToWord :: Integer -> Word#
integerToWord = I.integerToWord#


word64ToInteger :: Word64# -> Integer
word64ToInteger = I.integerFromWord64#

integerToWord64 :: Integer -> Word64#
integerToWord64 = I.integerToWord64#

int64ToInteger :: Int64# -> Integer
int64ToInteger = I.integerFromInt64#

integerToInt64 :: Integer -> Int64#
integerToInt64 = I.integerToInt64#


encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger = I.integerEncodeFloat#

floatFromInteger :: Integer -> Float#
floatFromInteger = I.integerToFloat#

encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger = I.integerEncodeDouble#

doubleFromInteger :: Integer -> Double#
doubleFromInteger = I.integerToDouble#

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger = I.integerDecodeDouble#


plusInteger :: Integer -> Integer -> Integer
plusInteger = I.integerAdd

minusInteger :: Integer -> Integer -> Integer
minusInteger = I.integerSub

timesInteger :: Integer -> Integer -> Integer
timesInteger = I.integerMul

negateInteger :: Integer -> Integer
negateInteger = I.integerNegate

absInteger :: Integer -> Integer
absInteger = I.integerAbs

signumInteger :: Integer -> Integer
signumInteger = I.integerSignum

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger = I.integerDivMod#

divInteger :: Integer -> Integer -> Integer
divInteger = I.integerDiv

modInteger :: Integer -> Integer -> Integer
modInteger = I.integerMod

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger = I.integerQuotRem#

quotInteger :: Integer -> Integer -> Integer
quotInteger = I.integerQuot

remInteger :: Integer -> Integer -> Integer
remInteger = I.integerRem


eqInteger :: Integer -> Integer -> Bool
eqInteger = I.integerEq

neqInteger :: Integer -> Integer -> Bool
neqInteger = I.integerNe

leInteger :: Integer -> Integer -> Bool
leInteger = I.integerLe

gtInteger :: Integer -> Integer -> Bool
gtInteger = I.integerGt

ltInteger :: Integer -> Integer -> Bool
ltInteger = I.integerLt

geInteger :: Integer -> Integer -> Bool
geInteger = I.integerGe

compareInteger :: Integer -> Integer -> Ordering
compareInteger = I.integerCompare



eqInteger# :: Integer -> Integer -> Int#
eqInteger# = I.integerEq#

neqInteger# :: Integer -> Integer -> Int#
neqInteger# = I.integerNe#

leInteger# :: Integer -> Integer -> Int#
leInteger# = I.integerLe#

gtInteger# :: Integer -> Integer -> Int#
gtInteger# = I.integerGt#

ltInteger# :: Integer -> Integer -> Int#
ltInteger# = I.integerLt#

geInteger# :: Integer -> Integer -> Int#
geInteger# = I.integerGe#


andInteger :: Integer -> Integer -> Integer
andInteger = I.integerAnd

orInteger :: Integer -> Integer -> Integer
orInteger = I.integerOr

xorInteger :: Integer -> Integer -> Integer
xorInteger = I.integerXor

complementInteger :: Integer -> Integer
complementInteger = I.integerComplement

shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger n i = I.integerShiftL# n (int2Word# i)

shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger n i = I.integerShiftR# n (int2Word# i)

testBitInteger :: Integer -> Int# -> Bool
testBitInteger n i = isTrue# (I.integerTestBit# n (int2Word# i))

hashInteger :: Integer -> Int#
hashInteger = I.integerToInt#

bitInteger :: Int# -> Integer
bitInteger i = I.integerBit# (int2Word# i)

popCountInteger :: Integer -> Int#
popCountInteger = I.integerPopCount#

