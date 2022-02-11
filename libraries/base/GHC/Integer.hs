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
    encodeFloatInteger, encodeDoubleInteger, decodeDoubleInteger,

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

#if WORD_SIZE_IN_BITS < 64

word64ToInteger :: Word64# -> Integer
word64ToInteger = I.integerFromWord64#

integerToWord64 :: Integer -> Word64#
integerToWord64 = I.integerToWord64#

int64ToInteger :: Int64# -> Integer
int64ToInteger = I.integerFromInt64#

integerToInt64 :: Integer -> Int64#
integerToInt64 = I.integerToInt64#

#endif


encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger = I.integerEncodeFloat#

encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger = I.integerEncodeDouble#

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger = I.integerDecodeDouble#

-- | Used to implement `(+)` for the `Num` typeclass.
--   This gives the sum of two integers.
--
-- ==== __Example__
-- >>> plusInteger 3 2
-- 5
--
-- >>> (+) 3 2
-- 5
plusInteger :: Integer -> Integer -> Integer
plusInteger = I.integerAdd

-- | Used to implement `(-)` for the `Num` typeclass.
--   This gives the difference of two integers.
--
-- ==== __Example__
-- >>> minusInteger 3 2
-- 1
--
-- >>> (-) 3 2
-- 1
minusInteger :: Integer -> Integer -> Integer
minusInteger = I.integerSub

-- | Used to implement `(*)` for the `Num` typeclass.
--   This gives the product of two integers.
--
-- ==== __Example__
-- >>> timesInteger 3 2
-- 6
--
-- >>> (*) 3 2
-- 6
timesInteger :: Integer -> Integer -> Integer
timesInteger = I.integerMul

-- | Used to implement `negate` for the `Num` typeclass.
--   This changes the sign of whatever integer is passed into it.
--
-- ==== __Example__
-- >>> negateInteger (-6)
-- 6
--
-- >>> negate (-6)
-- 6
negateInteger :: Integer -> Integer
negateInteger = I.integerNegate

-- | Used to implement `abs` for the `Num` typeclass.
--   This gives the absolute value of whatever integer is passed into it.
--
-- ==== __Example__
-- >>> absInteger (-6)
-- 6
--
-- >>> abs (-6)
-- 6
absInteger :: Integer -> Integer
absInteger = I.integerAbs

-- | Used to implement `signum` for the `Num` typeclass.
--   This gives 1 for a positive integer, and -1 for a negative integer.
--
-- ==== __Example__
-- >>> signumInteger 5
-- 1
--
-- >>> signum 5
-- 1
signumInteger :: Integer -> Integer
signumInteger = I.integerSignum

-- | Used to implement `divMod` for the `Integral` typeclass.
--   This gives a tuple equivalent to
--
-- >(div x y, mod x y)
--
-- ==== __Example__
-- >>> divModInteger 10 2
-- (5,0)
--
-- >>> divMod 10 2
-- (5,0)
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger = I.integerDivMod#

-- | Used to implement `div` for the `Integral` typeclass.
--   This performs integer division on its two parameters, truncated towards negative infinity.
--
-- ==== __Example__
-- >>> 10 `divInteger` 2
-- 5
--
-- >>> 10 `div` 2
divInteger :: Integer -> Integer -> Integer
divInteger = I.integerDiv

-- | Used to implement `mod` for the `Integral` typeclass.
--   This performs the modulo operation, satisfying
--
-- > ((x `div` y) * y) + (x `mod` y) == x
--
-- ==== __Example__
-- >>> 7 `modInteger` 3
-- 1
--
-- >>> 7 `mod` 3
-- 1
modInteger :: Integer -> Integer -> Integer
modInteger = I.integerMod

-- | Used to implement `quotRem` for the `Integral` typeclass.
--   This gives a tuple equivalent to
--
-- > (quot x y, mod x y)
--
-- ==== __Example__
-- >>> quotRemInteger 10 2
-- (5,0)
--
-- >>> quotRem 10 2
-- (5,0)
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger = I.integerQuotRem#

-- | Used to implement `quot` for the `Integral` typeclass.
--   This performs integer division on its two parameters, truncated towards zero.
--
-- ==== __Example__
-- >>> quotInteger 10 2
-- 5
--
-- >>> quot 10 2
-- 5
quotInteger :: Integer -> Integer -> Integer
quotInteger = I.integerQuot

-- | Used to implement `rem` for the `Integral` typeclass.
--   This gives the remainder after integer division of its two parameters, satisfying
--
-- > ((x `quot` y) * y) + (x `rem` y) == x
--
-- ==== __Example__
-- >>> remInteger 3 2
-- 1
--
-- >>> rem 3 2
-- 1
remInteger :: Integer -> Integer -> Integer
remInteger = I.integerRem

-- | Used to implement `(==)` for the `Eq` typeclass.
--   Outputs `True` if two integers are equal to each other.
--
-- ==== __Example__
-- >>> 6 `eqInteger` 6
-- True
--
-- >>> 6 == 6
-- True
eqInteger :: Integer -> Integer -> Bool
eqInteger = I.integerEq

-- | Used to implement `(/=)` for the `Eq` typeclass.
--   Outputs `True` if two integers are not equal to each other.
--
-- ==== __Example__
-- >>> 6 `neqInteger` 7
-- True
--
-- >>> 6 /= 7
-- True
neqInteger :: Integer -> Integer -> Bool
neqInteger = I.integerNe

-- | Used to implement `(<=)` for the `Ord` typeclass.
--   Outputs `True` if the first argument is less than or equal to the second.
--
-- ==== __Example__
-- >>> 3 `leInteger` 5
-- True
--
-- >>> 3 <= 5
-- True
leInteger :: Integer -> Integer -> Bool
leInteger = I.integerLe

-- | Used to implement `(>)` for the `Ord` typeclass.
--   Outputs `True` if the first argument is greater than the second.
--
-- ==== __Example__
-- >>> 5 `gtInteger` 3
-- True
--
-- >>> 5 > 3
-- True
gtInteger :: Integer -> Integer -> Bool
gtInteger = I.integerGt

-- | Used to implement `(<)` for the `Ord` typeclass.
--   Outputs `True` if the first argument is less than the second.
--
-- ==== __Example__
-- >>> 3 `ltInteger` 5
-- True
--
-- >>> 3 < 5
-- True
ltInteger :: Integer -> Integer -> Bool
ltInteger = I.integerLt

-- | Used to implement `(>=)` for the `Ord` typeclass.
--   Outputs `True` if the first argument is greater than or equal to the second.
--
-- ==== __Example__
-- >>> 5 `geInteger` 3
-- True
--
-- >>> 5 >= 3
-- True
geInteger :: Integer -> Integer -> Bool
geInteger = I.integerGe

-- | Used to implement `compare` for the `Integral` typeclass.
--   This takes two integers, and outputs whether the first is less than, equal to, or greater than the second.
--
-- ==== __Example__
-- >>> compareInteger 2 10
-- LT
--
-- >>> compare 2 10
-- LT
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

