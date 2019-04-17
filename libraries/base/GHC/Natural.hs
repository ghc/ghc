{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans -fno-worker-wrapper #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Natural
-- Copyright   :  (C) 2014 Herbert Valerio Riedel,
--                (C) 2011 Edward Kmett
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The arbitrary-precision 'Natural' number type.
--
-- __Note__: This is an internal GHC module with an API subject to
-- change.  It's recommended use the "Numeric.Natural" module to import
-- the 'Natural' type.
--
-- @since 4.8.0.0
-----------------------------------------------------------------------------
module GHC.Natural
    ( -- * The 'Natural' number type
      --
      -- | __Warning__: The internal implementation of 'Natural'
      -- (i.e. which constructors are available) depends on the
      -- 'Integer' backend used!
      Natural(..)
    , mkNatural
    , isValidNatural
      -- * Arithmetic
    , plusNatural
    , minusNatural
    , minusNaturalMaybe
    , timesNatural
    , negateNatural
    , signumNatural
    , quotRemNatural
    , quotNatural
    , remNatural
    , gcdNatural
    , lcmNatural
      -- * Equality and Ordering
    , compareNatural
    , neqNatural
    , eqNatural
    , leNatural
    , ltNatural
    , gtNatural
    , geNatural
      -- * Bits
    , andNatural
    , orNatural
    , xorNatural
    , bitNatural
    , testBitNatural
    , popCountNatural
    , shiftLNatural
    , shiftRNatural
      -- * Conversions
    , naturalToInteger
    , naturalToWord
    , naturalToInt
    , naturalFromInteger
    , wordToNatural
    , intToNatural
    , naturalToWordMaybe
    , wordToNaturalBase
      -- * Modular arithmetic
    , powModNatural
    ) where

#include "MachDeps.h"

import GHC.Classes
import GHC.Maybe
import GHC.Types
import GHC.Prim
import {-# SOURCE #-} GHC.Exception.Type (underflowException, divZeroException)
#if defined(MIN_VERSION_integer_gmp)
import GHC.Integer.GMP.Internals
#else
import GHC.Integer
#endif

default ()

-- Most high-level operations need to be marked `NOINLINE` as
-- otherwise GHC doesn't recognize them and fails to apply constant
-- folding to `Natural`-typed expression.
--
-- To this end, the CPP hack below allows to write the pseudo-pragma
--
--   {-# CONSTANT_FOLDED plusNatural #-}
--
-- which is simply expanded into a
--
--   {-# NOINLINE plusNatural #-}
--
--
-- DONE: Note that some functions had commented CONSTANT_FOLDED annotations
-- because they did not have a built-in rule in PrelRules like their @Integer@
-- counterparts did.
-- #15821 added builtin rules for those functions on @Natural@ thereby enabling
-- folding of @Natural@ literals, uncommenting all of the disabled annotations,
-- and adding more.
-- For more information on @Natural@ constant-folding, see the following notes:
--
-- * Note [Integer and Natural constant folding]
-- * Note [Ord and Eq instances for Natural]
-- * Note [Integer division constant folding]

#define CONSTANT_FOLDED NOINLINE

-------------------------------------------------------------------------------
-- Arithmetic underflow
-------------------------------------------------------------------------------

-- We put them here because they were needed relatively early
-- in the libraries before the Exception type has been defined yet.

{-# NOINLINE underflowError #-}
underflowError :: a
underflowError = raise# underflowException

{-# NOINLINE divZeroError #-}
divZeroError :: a
divZeroError = raise# divZeroException

-------------------------------------------------------------------------------
-- Natural type
-------------------------------------------------------------------------------

#if defined(MIN_VERSION_integer_gmp)
-- TODO: if saturated arithmetic is to be used, replace 'underflowError' by '0'

-- | Type representing arbitrary-precision non-negative integers.
--
-- >>> 2^100 :: Natural
-- 1267650600228229401496703205376
--
-- Operations whose result would be negative @'Control.Exception.throw'
-- ('Control.Exception.Underflow' :: 'Control.Exception.ArithException')@,
--
-- >>> -1 :: Natural
-- *** Exception: arithmetic underflow
--
-- @since 4.8.0.0
data Natural = NatS#                 GmpLimb# -- ^ in @[0, maxBound::Word]@
             | NatJ# {-# UNPACK #-} !BigNat   -- ^ in @]maxBound::Word, +inf[@
                                              --
                                              -- __Invariant__: 'NatJ#' is used
                                              -- /iff/ value doesn't fit in
                                              -- 'NatS#' constructor.
                                              -- NB: Order of constructors *must*
                                              -- coincide with 'Ord' relation

-- Note [Ord and Eq instances for Natural]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- In order to take advantage of constant folding, the @Ord/Eq@ instances for
-- @Natural@ cannot be derived so that the @NOINLINE@ pragma can be applied to
-- comparison operators when using the @Natural@ type, just as it is done for
-- @Integer@. Deriving instances for these classes means it would not be
-- possible to do this.
--
-- Note that in case there is no version of the `integer-gmp` library in use,
-- @Natural@ falls back to wrapping an @Integer@ over a newtype. The operators
-- below are then redefined to use those from 'GHC.Integer.Type', at which point
-- the rules for @Integer@ take over.

-- | @since 4.8.0.0
instance Eq Natural where
    (==)    = eqNatural
    (/=)    = neqNatural

-- | @since 4.8.0.0
instance Ord Natural where
    compare = compareNatural
    (>)     = gtNatural
    (>=)    = geNatural
    (<)     = ltNatural
    (<=)    = leNatural

neqNatural, eqNatural, leNatural, ltNatural, gtNatural, geNatural
  :: Natural -> Natural -> Bool
eqNatural  x y = isTrue# (eqNatural#  x y)
neqNatural x y = isTrue# (neqNatural# x y)
leNatural  x y = isTrue# (leNatural#  x y)
ltNatural  x y = isTrue# (ltNatural#  x y)
gtNatural  x y = isTrue# (gtNatural#  x y)
geNatural  x y = isTrue# (geNatural#  x y)

eqNatural#, neqNatural#, leNatural#, ltNatural#, gtNatural#, geNatural#
  :: Natural -> Natural -> Int#
eqNatural# (NatS# x#) (NatS# y#) = eqWord# x# y#
eqNatural# (NatJ# x) (NatJ# y)   = eqBigNat# x y
eqNatural# _       _             = 0#
{-# CONSTANT_FOLDED eqNatural# #-}

neqNatural# (NatS# x#) (NatS# y#) = neWord# x# y#
neqNatural# (NatJ# x) (NatJ# y)   = neqBigNat# x y
neqNatural# _       _             = 1#
{-# CONSTANT_FOLDED neqNatural# #-}

gtNatural# (NatS# x#) (NatS# y#)           = gtWord# x# y#
gtNatural# x y | compareNatural x y == GT  = 1#
gtNatural# _ _                             = 0#
{-# CONSTANT_FOLDED gtNatural# #-}

leNatural# (NatS# x#) (NatS# y#)          = leWord# x# y#
leNatural# x y | compareNatural x y /= GT = 1#
leNatural# _ _                            = 0#
{-# CONSTANT_FOLDED leNatural# #-}

ltNatural# (NatS# x#) (NatS# y#)          = ltWord# x# y#
ltNatural# x y | compareNatural x y == LT = 1#
ltNatural# _ _                            = 0#
{-# CONSTANT_FOLDED ltNatural# #-}

geNatural# (NatS# x#) (NatS# y#)          = geWord# x# y#
geNatural# x y | compareNatural x y /= LT = 1#
geNatural# _ _                            = 0#
{-# CONSTANT_FOLDED geNatural# #-}

compareNatural :: Natural -> Natural -> Ordering
compareNatural (NatS# x)  (NatS# y)  = compareWord#  x y
compareNatural (NatJ# x)  (NatJ# y)  = compareBigNat x y
compareNatural (NatS# _)  (NatJ# _)  = LT
compareNatural (NatJ# _)  _          = GT
{-# CONSTANT_FOLDED compareNatural #-}

-- | Test whether all internal invariants are satisfied by 'Natural' value
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
--
-- @since 4.8.0.0
isValidNatural :: Natural -> Bool
isValidNatural (NatS# _)  = True
isValidNatural (NatJ# bn) = isTrue# (isValidBigNat# bn)
                            && isTrue# (sizeofBigNat# bn ># 0#)

signumNatural :: Natural -> Natural
signumNatural (NatS# 0##) = NatS# 0##
signumNatural _           = NatS# 1##
{-# CONSTANT_FOLDED signumNatural #-}

negateNatural :: Natural -> Natural
negateNatural (NatS# 0##) = NatS# 0##
negateNatural _           = underflowError
{-# CONSTANT_FOLDED negateNatural #-}

-- | @since 4.10.0.0
naturalFromInteger :: Integer -> Natural
naturalFromInteger (S# i#)
  | isTrue# (i# >=# 0#)     = NatS# (int2Word# i#)
naturalFromInteger (Jp# bn) = bigNatToNatural bn
naturalFromInteger _        = underflowError
{-# CONSTANT_FOLDED naturalFromInteger #-}

-- | Compute greatest common divisor.
gcdNatural :: Natural -> Natural -> Natural
gcdNatural (NatS# 0##) y       = y
gcdNatural x       (NatS# 0##) = x
gcdNatural (NatS# 1##) _       = NatS# 1##
gcdNatural _       (NatS# 1##) = NatS# 1##
gcdNatural (NatJ# x) (NatJ# y) = bigNatToNatural (gcdBigNat x y)
gcdNatural (NatJ# x) (NatS# y) = NatS# (gcdBigNatWord x y)
gcdNatural (NatS# x) (NatJ# y) = NatS# (gcdBigNatWord y x)
gcdNatural (NatS# x) (NatS# y) = NatS# (gcdWord x y)
{-# CONSTANT_FOLDED gcdNatural #-}

-- | Compute least common multiple.
lcmNatural :: Natural -> Natural -> Natural
lcmNatural (NatS# 0##) _ = NatS# 0##
lcmNatural _ (NatS# 0##) = NatS# 0##
lcmNatural (NatS# 1##) y = y
lcmNatural x (NatS# 1##) = x
lcmNatural x y           = (x `quotNatural` (gcdNatural x y)) `timesNatural` y
{-# CONSTANT_FOLDED lcmNatural #-}

----------------------------------------------------------------------------

quotRemNatural :: Natural -> Natural -> (Natural, Natural)
quotRemNatural _ (NatS# 0##) = divZeroError
quotRemNatural n (NatS# 1##) = (n,NatS# 0##)
quotRemNatural n@(NatS# _) (NatJ# _) = (NatS# 0##, n)
quotRemNatural (NatS# n) (NatS# d) = case quotRemWord# n d of
    (# q, r #) -> (NatS# q, NatS# r)
quotRemNatural (NatJ# n) (NatS# d) = case quotRemBigNatWord n d of
    (# q, r #) -> (bigNatToNatural q, NatS# r)
quotRemNatural (NatJ# n) (NatJ# d) = case quotRemBigNat n d of
    (# q, r #) -> (bigNatToNatural q, bigNatToNatural r)
{-# CONSTANT_FOLDED quotRemNatural #-}

quotNatural :: Natural -> Natural -> Natural
quotNatural _       (NatS# 0##) = divZeroError
quotNatural n       (NatS# 1##) = n
quotNatural (NatS# _) (NatJ# _) = NatS# 0##
quotNatural (NatS# n) (NatS# d) = NatS# (quotWord# n d)
quotNatural (NatJ# n) (NatS# d) = bigNatToNatural (quotBigNatWord n d)
quotNatural (NatJ# n) (NatJ# d) = bigNatToNatural (quotBigNat n d)
{-# CONSTANT_FOLDED quotNatural #-}

remNatural :: Natural -> Natural -> Natural
remNatural _         (NatS# 0##) = divZeroError
remNatural _         (NatS# 1##) = NatS# 0##
remNatural n@(NatS# _) (NatJ# _) = n
remNatural   (NatS# n) (NatS# d) = NatS# (remWord# n d)
remNatural   (NatJ# n) (NatS# d) = NatS# (remBigNatWord n d)
remNatural   (NatJ# n) (NatJ# d) = bigNatToNatural (remBigNat n d)
{-# CONSTANT_FOLDED remNatural #-}

-- | @since 4.X.0.0
naturalToInteger :: Natural -> Integer
naturalToInteger (NatS# w)  = wordToInteger w
naturalToInteger (NatJ# bn) = Jp# bn
{-# CONSTANT_FOLDED naturalToInteger #-}

andNatural :: Natural -> Natural -> Natural
andNatural (NatS# n) (NatS# m) = NatS# (n `and#` m)
andNatural (NatS# n) (NatJ# m) = NatS# (n `and#` bigNatToWord m)
andNatural (NatJ# n) (NatS# m) = NatS# (bigNatToWord n `and#` m)
andNatural (NatJ# n) (NatJ# m) = bigNatToNatural (andBigNat n m)
{-# CONSTANT_FOLDED andNatural #-}

orNatural :: Natural -> Natural -> Natural
orNatural (NatS# n) (NatS# m) = NatS# (n `or#` m)
orNatural (NatS# n) (NatJ# m) = NatJ# (orBigNat (wordToBigNat n) m)
orNatural (NatJ# n) (NatS# m) = NatJ# (orBigNat n (wordToBigNat m))
orNatural (NatJ# n) (NatJ# m) = NatJ# (orBigNat n m)
{-# CONSTANT_FOLDED orNatural #-}

xorNatural :: Natural -> Natural -> Natural
xorNatural (NatS# n) (NatS# m) = NatS# (n `xor#` m)
xorNatural (NatS# n) (NatJ# m) = NatJ# (xorBigNat (wordToBigNat n) m)
xorNatural (NatJ# n) (NatS# m) = NatJ# (xorBigNat n (wordToBigNat m))
xorNatural (NatJ# n) (NatJ# m) = bigNatToNatural (xorBigNat n m)
{-# CONSTANT_FOLDED xorNatural #-}

bitNatural :: Int# -> Natural
bitNatural i#
  | isTrue# (i# <# WORD_SIZE_IN_BITS#) = NatS# (1## `uncheckedShiftL#` i#)
  | True                               = NatJ# (bitBigNat i#)
{-# CONSTANT_FOLDED bitNatural #-}

testBitNatural :: Natural -> Int# -> Bool
testBitNatural (NatS# w) i#
  | isTrue# (i# <# WORD_SIZE_IN_BITS#) =
      isTrue# ((w `and#` (1## `uncheckedShiftL#` i#)) `neWord#` 0##)
  | True                               = False
testBitNatural (NatJ# bn) i#           = testBitBigNat bn i#
{-# CONSTANT_FOLDED testBitNatural #-}

popCountNatural :: Natural -> Int#
-- @word2Int#@ will never underflow because for this pattern match,
-- the maximum possible value it will return on an @n@-bit machine is
-- @n@, well below @maxBound :: Int@ for any realistic use.
popCountNatural (NatS# w)  = word2Int# (popCnt# w)
popCountNatural (NatJ# bn) = popCountBigNat bn
{-# CONSTANT_FOLDED popCountNatural #-}

shiftLNatural :: Natural -> Int# -> Natural
shiftLNatural n           0# = n
shiftLNatural (NatS# 0##) _  = NatS# 0##
shiftLNatural (NatS# 1##) i# = bitNatural i#
shiftLNatural (NatS# w) i#
    = bigNatToNatural (shiftLBigNat (wordToBigNat w) i#)
shiftLNatural (NatJ# bn) i#
    = bigNatToNatural (shiftLBigNat bn i#)
{-# CONSTANT_FOLDED shiftLNatural #-}

shiftRNatural :: Natural -> Int# -> Natural
shiftRNatural n         0# = n
shiftRNatural (NatS# w) i#
      | isTrue# (i# >=# WORD_SIZE_IN_BITS#) = NatS# 0##
      | True = NatS# (w `uncheckedShiftRL#` i#)
shiftRNatural (NatJ# bn) i# = bigNatToNatural (shiftRBigNat bn i#)
{-# CONSTANT_FOLDED shiftRNatural #-}

----------------------------------------------------------------------------

-- | 'Natural' Addition
plusNatural :: Natural -> Natural -> Natural
plusNatural (NatS# 0##) y         = y
plusNatural x         (NatS# 0##) = x
plusNatural (NatS# x) (NatS# y)
    = case plusWord2# x y of
       (# 0##, l #) -> NatS# l
       (# h,   l #) -> NatJ# (wordToBigNat2 h l)
plusNatural (NatS# x) (NatJ# y) = NatJ# (plusBigNatWord y x)
plusNatural (NatJ# x) (NatS# y) = NatJ# (plusBigNatWord x y)
plusNatural (NatJ# x) (NatJ# y) = NatJ# (plusBigNat     x y)
{-# CONSTANT_FOLDED plusNatural #-}

-- | 'Natural' multiplication
timesNatural :: Natural -> Natural -> Natural
timesNatural _         (NatS# 0##) = NatS# 0##
timesNatural (NatS# 0##) _         = NatS# 0##
timesNatural x         (NatS# 1##) = x
timesNatural (NatS# 1##) y         = y
timesNatural (NatS# x) (NatS# y) = case timesWord2# x y of
    (# 0##, 0## #) -> NatS# 0##
    (# 0##, xy  #) -> NatS# xy
    (# h  , l   #) -> NatJ# (wordToBigNat2 h l)
timesNatural (NatS# x) (NatJ# y) = NatJ# (timesBigNatWord y x)
timesNatural (NatJ# x) (NatS# y) = NatJ# (timesBigNatWord x y)
timesNatural (NatJ# x) (NatJ# y) = NatJ# (timesBigNat     x y)
{-# CONSTANT_FOLDED timesNatural #-}

-- | 'Natural' subtraction. May @'Control.Exception.throw'
-- 'Control.Exception.Underflow'@.
minusNatural :: Natural -> Natural -> Natural
minusNatural x         (NatS# 0##) = x
minusNatural (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> NatS# l
    _           -> underflowError
minusNatural (NatS# _) (NatJ# _) = underflowError
minusNatural (NatJ# x) (NatS# y)
    = bigNatToNatural (minusBigNatWord x y)
minusNatural (NatJ# x) (NatJ# y)
    = bigNatToNatural (minusBigNat     x y)
{-# CONSTANT_FOLDED minusNatural #-}

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- @since 4.8.0.0
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x         (NatS# 0##) = Just x
minusNaturalMaybe (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> Just (NatS# l)
    _           -> Nothing
minusNaturalMaybe (NatS# _) (NatJ# _) = Nothing
minusNaturalMaybe (NatJ# x) (NatS# y)
    = Just (bigNatToNatural (minusBigNatWord x y))
minusNaturalMaybe (NatJ# x) (NatJ# y)
  | isTrue# (isNullBigNat# res) = Nothing
  | True                        = Just (bigNatToNatural res)
  where
    res = minusBigNat x y

-- | Convert 'BigNat' to 'Natural'.
-- Throws 'Control.Exception.Underflow' if passed a 'nullBigNat'.
bigNatToNatural :: BigNat -> Natural
bigNatToNatural bn
  | isTrue# (sizeofBigNat# bn ==# 1#) = NatS# (bigNatToWord bn)
  | isTrue# (isNullBigNat# bn)        = underflowError
  | True                              = NatJ# bn

naturalToBigNat :: Natural -> BigNat
naturalToBigNat (NatS# w#) = wordToBigNat w#
naturalToBigNat (NatJ# bn) = bn

naturalToWord :: Natural -> Word#
naturalToWord (NatS# w#) = w#
naturalToWord (NatJ# bn) = bigNatToWord bn
{-# CONSTANT_FOLDED naturalToWord #-}

naturalToInt :: Natural -> Int#
naturalToInt (NatS# w#) = word2Int# w#
naturalToInt (NatJ# bn) = bigNatToInt bn
{-# CONSTANT_FOLDED naturalToInt #-}

----------------------------------------------------------------------------

-- | Convert a Word# into a Natural
--
-- Built-in rule ensures that applications of this function to literal Word# are
-- lifted into Natural literals.
wordToNatural :: Word# -> Natural
wordToNatural w# = NatS# w#
{-# CONSTANT_FOLDED wordToNatural #-}

-- | Convert a Word# into a Natural
--
-- In base we can't use wordToNatural as built-in rules transform some of them
-- into Natural literals. Use this function instead.
wordToNaturalBase :: Word# -> Natural
wordToNaturalBase w# = NatS# w#

#else /* !defined(MIN_VERSION_integer_gmp) */
----------------------------------------------------------------------------
-- Use wrapped 'Integer' as fallback; taken from Edward Kmett's nats package

-- | Type representing arbitrary-precision non-negative integers.
--
-- Operations whose result would be negative @'Control.Exception.throw'
-- ('Control.Exception.Underflow' :: 'Control.Exception.ArithException')@.
--
-- @since 4.8.0.0
newtype Natural = Natural Integer -- ^ __Invariant__: non-negative 'Integer'

-- | @since 4.8.0.0
instance Eq Natural where
    (==)    = eqNatural
    (/=)    = neqNatural

-- | @since 4.8.0.0
instance Ord Natural where
    compare = compareNatural
    (>)     = gtNatural
    (>=)    = geNatural
    (<)     = ltNatural
    (<=)    = leNatural

neqNatural, eqNatural, leNatural, ltNatural, gtNatural, geNatural
  :: Natural -> Natural -> Bool
eqNatural  x y = isTrue# (eqNatural#  x y)
neqNatural x y = isTrue# (neqNatural# x y)
leNatural  x y = isTrue# (leNatural#  x y)
ltNatural  x y = isTrue# (ltNatural#  x y)
gtNatural  x y = isTrue# (gtNatural#  x y)
geNatural  x y = isTrue# (geNatural#  x y)

eqNatural#, neqNatural#, leNatural#, ltNatural#, gtNatural#, geNatural#
  :: Natural -> Natural -> Int#
eqNatural# (Natural x) (Natural y) = eqInteger# x y
{-# CONSTANT_FOLDED eqNatural# #-}

neqNatural# (Natural x) (Natural y) = neqInteger# x y
{-# CONSTANT_FOLDED neqNatural# #-}

gtNatural# (Natural x) (Natural y)  = gtInteger# x y
{-# CONSTANT_FOLDED gtNatural# #-}

leNatural# (Natural x) (Natural y)  = leInteger# x y
{-# CONSTANT_FOLDED leNatural# #-}

ltNatural# (Natural x) (Natural y)  = ltInteger# x y
{-# CONSTANT_FOLDED ltNatural# #-}

geNatural# (Natural x) (Natural y)  = geInteger# x y
{-# CONSTANT_FOLDED geNatural# #-}

compareNatural :: Natural -> Natural -> Ordering
compareNatural (Natural x) (Natural y) = compareInteger x y
{-# CONSTANT_FOLDED compareNatural #-}

-- | Test whether all internal invariants are satisfied by 'Natural' value
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Natural' values directly.
--
-- @since 4.8.0.0
isValidNatural :: Natural -> Bool
isValidNatural (Natural i) = i >= wordToInteger 0##

-- | Convert a 'Word#' into a 'Natural'
--
-- Built-in rule ensures that applications of this function to literal 'Word#'
-- are lifted into 'Natural' literals.
wordToNatural :: Word# -> Natural
wordToNatural w## = Natural (wordToInteger w##)
{-# CONSTANT_FOLDED wordToNatural #-}

-- | Convert a 'Word#' into a Natural
--
-- In base we can't use wordToNatural as built-in rules transform some of them
-- into Natural literals. Use this function instead.
wordToNaturalBase :: Word# -> Natural
wordToNaturalBase w## = Natural (wordToInteger w##)

-- | @since 4.10.0.0
naturalFromInteger :: Integer -> Natural
naturalFromInteger n
  | n >= wordToInteger 0## = Natural n
  | True                   = underflowError
{-# CONSTANT_FOLDED naturalFromInteger #-}


-- | Compute greatest common divisor.
gcdNatural :: Natural -> Natural -> Natural
gcdNatural (Natural n) (Natural m) = Natural (n `gcdInteger` m)
{-# CONSTANT_FOLDED gcdNatural #-}

-- | Compute lowest common multiple.
lcmNatural :: Natural -> Natural -> Natural
lcmNatural (Natural n) (Natural m) = Natural (n `lcmInteger` m)
{-# CONSTANT_FOLDED lcmNatural #-}

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- @since 4.8.0.0
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe (Natural x) (Natural y)
  | x >= y  = Just (Natural (x `minusInteger` y))
  | True    = Nothing

shiftLNatural :: Natural -> Int# -> Natural
shiftLNatural (Natural n) i# = Natural (n `shiftLInteger` i#)
{-# CONSTANT_FOLDED shiftLNatural #-}

shiftRNatural :: Natural -> Int# -> Natural
shiftRNatural (Natural n) i# = Natural (n `shiftRInteger` i#)
{-# CONSTANT_FOLDED shiftRNatural #-}

plusNatural :: Natural -> Natural -> Natural
plusNatural (Natural x) (Natural y) = Natural (x `plusInteger` y)
{-# CONSTANT_FOLDED plusNatural #-}

minusNatural :: Natural -> Natural -> Natural
minusNatural (Natural x) (Natural y)
  = if z `ltInteger` wordToInteger 0## then underflowError else Natural z
  where z = x `minusInteger` y
{-# CONSTANT_FOLDED minusNatural #-}

timesNatural :: Natural -> Natural -> Natural
timesNatural (Natural x) (Natural y) = Natural (x `timesInteger` y)
{-# CONSTANT_FOLDED timesNatural #-}

orNatural :: Natural -> Natural -> Natural
orNatural (Natural x) (Natural y) = Natural (x `orInteger` y)
{-# CONSTANT_FOLDED orNatural #-}

xorNatural :: Natural -> Natural -> Natural
xorNatural (Natural x) (Natural y) = Natural (x `xorInteger` y)
{-# CONSTANT_FOLDED xorNatural #-}

andNatural :: Natural -> Natural -> Natural
andNatural (Natural x) (Natural y) = Natural (x `andInteger` y)
{-# CONSTANT_FOLDED andNatural #-}

naturalToInt :: Natural -> Int#
naturalToInt (Natural i) = integerToInt i
{-# CONSTANT_FOLDED naturalToInt #-}

naturalToWord :: Natural -> Word#
naturalToWord (Natural i) = integerToWord i
{-# CONSTANT_FOLDED naturalToWord #-}

naturalToInteger :: Natural -> Integer
naturalToInteger (Natural i) = i
{-# CONSTANT_FOLDED naturalToInteger #-}

testBitNatural :: Natural -> Int# -> Bool
testBitNatural (Natural n) i# = testBitInteger n i#
{-# CONSTANT_FOLDED testBitNatural #-}

popCountNatural :: Natural -> Int#
popCountNatural (Natural n) = popCountInteger n
{-# CONSTANT_FOLDED popCountNatural #-}

bitNatural :: Int# -> Natural
bitNatural i#
  | isTrue# (i# <# WORD_SIZE_IN_BITS#) = wordToNaturalBase (1## `uncheckedShiftL#` i#)
  | True                               = Natural (1 `shiftLInteger` i#)
{-# CONSTANT_FOLDED bitNatural #-}

quotNatural :: Natural -> Natural -> Natural
quotNatural n@(Natural x) (Natural y)
   | y == wordToInteger 0## = divZeroError
   | y == wordToInteger 1## = n
   | True                   = Natural (x `quotInteger` y)
{-# CONSTANT_FOLDED quotNatural #-}

remNatural :: Natural -> Natural -> Natural
remNatural (Natural x) (Natural y)
   | y == wordToInteger 0## = divZeroError
   | y == wordToInteger 1## = wordToNaturalBase 0##
   | True                   = Natural (x `remInteger` y)
{-# CONSTANT_FOLDED remNatural #-}

quotRemNatural :: Natural -> Natural -> (Natural, Natural)
quotRemNatural n@(Natural x) (Natural y)
   | y == wordToInteger 0## = divZeroError
   | y == wordToInteger 1## = (n,wordToNaturalBase 0##)
   | True                   = case quotRemInteger x y of
      (# k, r #) -> (Natural k, Natural r)
{-# CONSTANT_FOLDED quotRemNatural #-}

signumNatural :: Natural -> Natural
signumNatural (Natural x)
   | x == wordToInteger 0## = wordToNaturalBase 0##
   | True                   = wordToNaturalBase 1##
{-# CONSTANT_FOLDED signumNatural #-}

negateNatural :: Natural -> Natural
negateNatural (Natural x)
   | x == wordToInteger 0## = wordToNaturalBase 0##
   | True                   = underflowError
{-# CONSTANT_FOLDED negateNatural #-}

#endif

-- | Try downcasting 'Natural' to 'Word' value.
-- Returns 'Nothing' if value doesn't fit in 'Word'.
--
-- @since 4.8.0.0
naturalToWordMaybe :: Natural -> Maybe Word
#if defined(MIN_VERSION_integer_gmp)
naturalToWordMaybe (NatS# w#) = Just (W# w#)
naturalToWordMaybe (NatJ# _)  = Nothing
#else
naturalToWordMaybe (Natural i)
  | i < maxw  = Just (W# (integerToWord i))
  | True      = Nothing
  where
    maxw = 1 `shiftLInteger` WORD_SIZE_IN_BITS#
#endif

-- | \"@'powModNatural' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- @since 4.8.0.0
powModNatural :: Natural -> Natural -> Natural -> Natural
#if defined(MIN_VERSION_integer_gmp)
powModNatural _           _           (NatS# 0##) = divZeroError
powModNatural _           _           (NatS# 1##) = NatS# 0##
powModNatural _           (NatS# 0##) _           = NatS# 1##
powModNatural (NatS# 0##) _           _           = NatS# 0##
powModNatural (NatS# 1##) _           _           = NatS# 1##
powModNatural (NatS# b)   (NatS# e)   (NatS# m)   = NatS# (powModWord b e m)
powModNatural b           e           (NatS# m)
  = NatS# (powModBigNatWord (naturalToBigNat b) (naturalToBigNat e) m)
powModNatural b           e           (NatJ# m)
  = bigNatToNatural (powModBigNat (naturalToBigNat b) (naturalToBigNat e) m)
#else
-- Portable reference fallback implementation
powModNatural (Natural b0) (Natural e0) (Natural m)
   | m  == wordToInteger 0## = divZeroError
   | m  == wordToInteger 1## = wordToNaturalBase 0##
   | e0 == wordToInteger 0## = wordToNaturalBase 1##
   | b0 == wordToInteger 0## = wordToNaturalBase 0##
   | b0 == wordToInteger 1## = wordToNaturalBase 1##
   | True    = go b0 e0 (wordToInteger 1##)
  where
    go !b e !r
      | e `testBitInteger` 0#  = go b' e' ((r `timesInteger` b) `modInteger` m)
      | e == wordToInteger 0## = naturalFromInteger r
      | True                   = go b' e' r
      where
        b' = (b `timesInteger` b) `modInteger` m
        e' = e `shiftRInteger` 1# -- slightly faster than "e `div` 2"
#endif


-- | Construct 'Natural' value from list of 'Word's.
--
-- This function is used by GHC for constructing 'Natural' literals.
mkNatural :: [Word]  -- ^ value expressed in 32 bit chunks, least
                     --   significant first
          -> Natural
mkNatural [] = wordToNaturalBase 0##
mkNatural (W# i : is') = wordToNaturalBase (i `and#` 0xffffffff##) `orNatural`
                         shiftLNatural (mkNatural is') 32#
{-# CONSTANT_FOLDED mkNatural #-}

-- | Convert 'Int#' to 'Natural'.
-- Throws 'Control.Exception.Underflow' when passed a negative 'Int#'.
intToNatural :: Int# -> Natural
intToNatural i#
  | isTrue# (i# <# 0#) = underflowError
  | True               = wordToNaturalBase (int2Word# i#)
{-# CONSTANT_FOLDED intToNatural #-}