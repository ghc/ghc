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

import GHC.Num.Integer (Integer)
import qualified GHC.Num.Integer as I
import GHC.Prim
import GHC.Types

smallInteger :: Int# -> Integer
{-# INLINE smallInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
smallInteger = I.integerFromInt#

integerToInt :: Integer -> Int#
{-# INLINE integerToInt #-} -- See Note [Integer constant folding] in GHC.Num.Integer
integerToInt = I.integerToInt#

wordToInteger :: Word# -> Integer
{-# INLINE wordToInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
wordToInteger = I.integerFromWord#

integerToWord :: Integer -> Word#
{-# INLINE integerToWord #-} -- See Note [Integer constant folding] in GHC.Num.Integer
integerToWord = I.integerToWord#

#if WORD_SIZE_IN_BITS < 64

word64ToInteger :: Word64# -> Integer
{-# INLINE word64ToInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
word64ToInteger = I.integerFromWord64#

integerToWord64 :: Integer -> Word64#
{-# INLINE integerToWord64 #-} -- See Note [Integer constant folding] in GHC.Num.Integer
integerToWord64 = I.integerToWord64#

int64ToInteger :: Int64# -> Integer
{-# INLINE int64ToInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
int64ToInteger = I.integerFromInt64#

integerToInt64 :: Integer -> Int64#
{-# INLINE integerToInt64 #-} -- See Note [Integer constant folding] in GHC.Num.Integer
integerToInt64 = I.integerToInt64#

#endif


encodeFloatInteger :: Integer -> Int# -> Float#
{-# INLINE encodeFloatInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
encodeFloatInteger = I.integerEncodeFloat#

floatFromInteger :: Integer -> Float#
{-# INLINE floatFromInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
floatFromInteger = I.integerToFloat#

encodeDoubleInteger :: Integer -> Int# -> Double#
{-# INLINE encodeDoubleInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
encodeDoubleInteger = I.integerEncodeDouble#

doubleFromInteger :: Integer -> Double#
{-# INLINE doubleFromInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
doubleFromInteger = I.integerToDouble#

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
{-# INLINE decodeDoubleInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
decodeDoubleInteger = I.integerDecodeDouble#


plusInteger :: Integer -> Integer -> Integer
{-# INLINE plusInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
plusInteger = I.integerAdd

minusInteger :: Integer -> Integer -> Integer
{-# INLINE minusInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
minusInteger = I.integerSub

timesInteger :: Integer -> Integer -> Integer
{-# INLINE timesInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
timesInteger = I.integerMul

negateInteger :: Integer -> Integer
{-# INLINE negateInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
negateInteger = I.integerNegate

absInteger :: Integer -> Integer
{-# INLINE absInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
absInteger = I.integerAbs

signumInteger :: Integer -> Integer
{-# INLINE signumInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
signumInteger = I.integerSignum

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
{-# INLINE divModInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
divModInteger = I.integerDivMod#

divInteger :: Integer -> Integer -> Integer
{-# INLINE divInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
divInteger = I.integerDiv

modInteger :: Integer -> Integer -> Integer
{-# INLINE modInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
modInteger = I.integerMod

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
{-# INLINE quotRemInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
quotRemInteger = I.integerQuotRem#

quotInteger :: Integer -> Integer -> Integer
{-# INLINE quotInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
quotInteger = I.integerQuot

remInteger :: Integer -> Integer -> Integer
{-# INLINE remInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
remInteger = I.integerRem


eqInteger :: Integer -> Integer -> Bool
{-# INLINE eqInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
eqInteger = I.integerEq

neqInteger :: Integer -> Integer -> Bool
{-# INLINE neqInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
neqInteger = I.integerNe

leInteger :: Integer -> Integer -> Bool
{-# INLINE leInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
leInteger = I.integerLe

gtInteger :: Integer -> Integer -> Bool
{-# INLINE gtInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
gtInteger = I.integerGt

ltInteger :: Integer -> Integer -> Bool
{-# INLINE ltInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
ltInteger = I.integerLt

geInteger :: Integer -> Integer -> Bool
{-# INLINE geInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
geInteger = I.integerGe

compareInteger :: Integer -> Integer -> Ordering
{-# INLINE compareInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
compareInteger = I.integerCompare



eqInteger# :: Integer -> Integer -> Int#
{-# INLINE eqInteger# #-} -- See Note [Integer constant folding] in GHC.Num.Integer
eqInteger# = I.integerEq#

neqInteger# :: Integer -> Integer -> Int#
{-# INLINE neqInteger# #-} -- See Note [Integer constant folding] in GHC.Num.Integer
neqInteger# = I.integerNe#

leInteger# :: Integer -> Integer -> Int#
{-# INLINE leInteger# #-} -- See Note [Integer constant folding] in GHC.Num.Integer
leInteger# = I.integerLe#

gtInteger# :: Integer -> Integer -> Int#
{-# INLINE gtInteger# #-} -- See Note [Integer constant folding] in GHC.Num.Integer
gtInteger# = I.integerGt#

ltInteger# :: Integer -> Integer -> Int#
{-# INLINE ltInteger# #-} -- See Note [Integer constant folding] in GHC.Num.Integer
ltInteger# = I.integerLt#

geInteger# :: Integer -> Integer -> Int#
{-# INLINE geInteger# #-} -- See Note [Integer constant folding] in GHC.Num.Integer
geInteger# = I.integerGe#


andInteger :: Integer -> Integer -> Integer
{-# INLINE andInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
andInteger = I.integerAnd

orInteger :: Integer -> Integer -> Integer
{-# INLINE orInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
orInteger = I.integerOr

xorInteger :: Integer -> Integer -> Integer
{-# INLINE xorInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
xorInteger = I.integerXor

complementInteger :: Integer -> Integer
{-# INLINE complementInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
complementInteger = I.integerComplement

shiftLInteger :: Integer -> Int# -> Integer
{-# INLINE shiftLInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
shiftLInteger n i = I.integerShiftL# n (int2Word# i)

shiftRInteger :: Integer -> Int# -> Integer
{-# INLINE shiftRInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
shiftRInteger n i = I.integerShiftR# n (int2Word# i)

testBitInteger :: Integer -> Int# -> Bool
{-# INLINE testBitInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
testBitInteger n i = isTrue# (I.integerTestBit# n (int2Word# i))

hashInteger :: Integer -> Int#
{-# INLINE hashInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
hashInteger = I.integerToInt#

bitInteger :: Int# -> Integer
{-# INLINE bitInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
bitInteger i = I.integerBit# (int2Word# i)

popCountInteger :: Integer -> Int#
{-# INLINE popCountInteger #-} -- See Note [Integer constant folding] in GHC.Num.Integer
popCountInteger = I.integerPopCount#
