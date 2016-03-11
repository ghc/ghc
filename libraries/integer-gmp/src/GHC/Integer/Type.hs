{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ExplicitForAll #-}

-- |
-- Module      :  GHC.Integer.Type
-- Copyright   :  (c) Herbert Valerio Riedel 2014
-- License     :  BSD3
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC Extensions)
--
-- GHC needs this module to be named "GHC.Integer.Type" and provide
-- all the low-level 'Integer' operations.

module GHC.Integer.Type where

#include "MachDeps.h"

-- Sanity check as CPP defines are implicitly 0-valued when undefined
#if !(defined(SIZEOF_LONG) && defined(SIZEOF_HSWORD) \
           && defined(WORD_SIZE_IN_BITS))
# error missing defines
#endif

import GHC.Classes
import GHC.Magic
import GHC.Prim
import GHC.Types
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

default ()

-- Most high-level operations need to be marked `NOINLINE` as
-- otherwise GHC doesn't recognize them and fails to apply constant
-- folding to `Integer`-typed expression.
--
-- To this end, the CPP hack below allows to write the pseudo-pragma
--
--   {-# CONSTANT_FOLDED plusInteger #-}
--
-- which is simply expaned into a
--
--   {-# NOINLINE plusInteger #-}
--
#define CONSTANT_FOLDED NOINLINE

----------------------------------------------------------------------------
-- type definitions

-- NB: all code assumes GMP_LIMB_BITS == WORD_SIZE_IN_BITS
-- The C99 code in cbits/wrappers.c will fail to compile if this doesn't hold

-- | Type representing a GMP Limb
type GmpLimb = Word -- actually, 'CULong'
type GmpLimb# = Word#

-- | Count of 'GmpLimb's, must be positive (unless specified otherwise).
type GmpSize = Int  -- actually, a 'CLong'
type GmpSize# = Int#

narrowGmpSize# :: Int# -> Int#
#if SIZEOF_LONG == SIZEOF_HSWORD
narrowGmpSize# x = x
#elif (SIZEOF_LONG == 4) && (SIZEOF_HSWORD == 8)
-- On IL32P64 (i.e. Win64), we have to be careful with CLong not being
-- 64bit.  This is mostly an issue on values returned from C functions
-- due to sign-extension.
narrowGmpSize# = narrow32Int#
#endif


type GmpBitCnt = Word -- actually, 'CULong'
type GmpBitCnt# = Word# -- actually, 'CULong'

-- Pseudo FFI CType
type CInt = Int
type CInt# = Int#

narrowCInt# :: Int# -> Int#
narrowCInt# = narrow32Int#

-- | Bits in a 'GmpLimb'. Same as @WORD_SIZE_IN_BITS@.
gmpLimbBits :: Word -- 8 `shiftL` gmpLimbShift
gmpLimbBits = W# WORD_SIZE_IN_BITS##

#if WORD_SIZE_IN_BITS == 64
# define GMP_LIMB_SHIFT   3
# define GMP_LIMB_BYTES   8
# define GMP_LIMB_BITS    64
# define INT_MINBOUND     -0x8000000000000000
# define INT_MAXBOUND      0x7fffffffffffffff
# define ABS_INT_MINBOUND  0x8000000000000000
# define SQRT_INT_MAXBOUND 0xb504f333
#elif WORD_SIZE_IN_BITS == 32
# define GMP_LIMB_SHIFT   2
# define GMP_LIMB_BYTES   4
# define GMP_LIMB_BITS    32
# define INT_MINBOUND     -0x80000000
# define INT_MAXBOUND      0x7fffffff
# define ABS_INT_MINBOUND  0x80000000
# define SQRT_INT_MAXBOUND 0xb504
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

-- | Type representing /raw/ arbitrary-precision Naturals
--
-- This is common type used by 'Natural' and 'Integer'.  As this type
-- consists of a single constructor wrapping a 'ByteArray#' it can be
-- unpacked.
--
-- Essential invariants:
--
--  - 'ByteArray#' size is an exact multiple of 'Word#' size
--  - limbs are stored in least-significant-limb-first order,
--  - the most-significant limb must be non-zero, except for
--  - @0@ which is represented as a 1-limb.
data BigNat = BN# ByteArray#

instance Eq BigNat where
    (==) = eqBigNat

instance Ord BigNat where
    compare = compareBigNat

-- | Invariant: 'Jn#' and 'Jp#' are used iff value doesn't fit in 'S#'
--
-- Useful properties resulting from the invariants:
--
--  - @abs ('S#' _) <= abs ('Jp#' _)@
--  - @abs ('S#' _) <  abs ('Jn#' _)@
--
data Integer  = S#                !Int#
                -- ^ iff value in @[minBound::'Int', maxBound::'Int']@ range
              | Jp# {-# UNPACK #-} !BigNat
                -- ^ iff value in @]maxBound::'Int', +inf[@ range
              | Jn# {-# UNPACK #-} !BigNat
                -- ^ iff value in @]-inf, minBound::'Int'[@ range

-- TODO: experiment with different constructor-ordering

instance Eq Integer where
    (==)    = eqInteger
    (/=)    = neqInteger

instance Ord Integer where
    compare = compareInteger
    (>)     = gtInteger
    (>=)    = geInteger
    (<)     = ltInteger
    (<=)    = leInteger

----------------------------------------------------------------------------

-- | Construct 'Integer' value from list of 'Int's.
--
-- This function is used by GHC for constructing 'Integer' literals.
mkInteger :: Bool   -- ^ sign of integer ('True' if non-negative)
          -> [Int]  -- ^ absolute value expressed in 31 bit chunks, least
                    --   significant first (ideally these would be machine-word
                    --   'Word's rather than 31-bit truncated 'Int's)
          -> Integer
mkInteger nonNegative is
  | nonNegative = f is
  | True        = negateInteger (f is)
  where
    f [] = S# 0#
    f (I# i : is') = smallInteger (i `andI#` 0x7fffffff#) `orInteger`
                         shiftLInteger (f is') 31#
{-# CONSTANT_FOLDED mkInteger #-}

-- | Test whether all internal invariants are satisfied by 'Integer' value
--
-- Returns @1#@ if valid, @0#@ otherwise.
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
isValidInteger# :: Integer -> Int#
isValidInteger# (S#  _) = 1#
isValidInteger# (Jp# bn)
    = isValidBigNat# bn `andI#` (bn `gtBigNatWord#` INT_MAXBOUND##)
isValidInteger# (Jn# bn)
    = isValidBigNat# bn `andI#` (bn `gtBigNatWord#` ABS_INT_MINBOUND##)

-- | Should rather be called @intToInteger@
smallInteger :: Int# -> Integer
smallInteger i# = S# i#
{-# CONSTANT_FOLDED smallInteger #-}

----------------------------------------------------------------------------
-- Int64/Word64 specific primitives

#if WORD_SIZE_IN_BITS < 64
int64ToInteger :: Int64# -> Integer
int64ToInteger i
  | isTrue# (i `leInt64#` intToInt64#  0x7FFFFFFF#)
  , isTrue# (i `geInt64#` intToInt64# -0x80000000#)
    = S# (int64ToInt# i)
  | isTrue# (i `geInt64#` intToInt64# 0#)
    = Jp# (word64ToBigNat (int64ToWord64# i))
  | True
    = Jn# (word64ToBigNat (int64ToWord64# (negateInt64# i)))
{-# CONSTANT_FOLDED int64ToInteger #-}

word64ToInteger :: Word64# -> Integer
word64ToInteger w
  | isTrue# (w `leWord64#` wordToWord64# 0x7FFFFFFF##)
    = S# (int64ToInt# (word64ToInt64# w))
  | True
    = Jp# (word64ToBigNat w)
{-# CONSTANT_FOLDED word64ToInteger #-}

integerToInt64 :: Integer -> Int64#
integerToInt64 (S# i#)  = intToInt64# i#
integerToInt64 (Jp# bn) = word64ToInt64# (bigNatToWord64 bn)
integerToInt64 (Jn# bn) = negateInt64# (word64ToInt64# (bigNatToWord64 bn))
{-# CONSTANT_FOLDED integerToInt64 #-}

integerToWord64 :: Integer -> Word64#
integerToWord64 (S# i#)  = int64ToWord64# (intToInt64# i#)
integerToWord64 (Jp# bn) = bigNatToWord64 bn
integerToWord64 (Jn# bn)
    = int64ToWord64# (negateInt64# (word64ToInt64# (bigNatToWord64 bn)))
{-# CONSTANT_FOLDED integerToWord64 #-}

#if GMP_LIMB_BITS == 32
word64ToBigNat :: Word64# -> BigNat
word64ToBigNat w64 = wordToBigNat2 wh# wl#
  where
    wh# = word64ToWord# (uncheckedShiftRL64# w64 32#)
    wl# = word64ToWord# w64

bigNatToWord64 :: BigNat -> Word64#
bigNatToWord64 bn
  | isTrue# (sizeofBigNat# bn ># 1#)
    = let wh# = wordToWord64# (indexBigNat# bn 1#)
      in uncheckedShiftL64# wh# 32# `or64#` wl#
  | True = wl#
  where
    wl# = wordToWord64# (bigNatToWord bn)
#endif
#endif

-- End of Int64/Word64 specific primitives
----------------------------------------------------------------------------

-- | Truncates 'Integer' to least-significant 'Int#'
integerToInt :: Integer -> Int#
integerToInt (S# i#)  = i#
integerToInt (Jp# bn) = bigNatToInt bn
integerToInt (Jn# bn) = negateInt# (bigNatToInt bn)
{-# CONSTANT_FOLDED integerToInt #-}

hashInteger :: Integer -> Int#
hashInteger = integerToInt -- emulating what integer-{simple,gmp} already do

integerToWord :: Integer -> Word#
integerToWord (S# i#)  = int2Word# i#
integerToWord (Jp# bn) = bigNatToWord bn
integerToWord (Jn# bn) = int2Word# (negateInt# (bigNatToInt bn))
{-# CONSTANT_FOLDED integerToWord #-}

wordToInteger :: Word# -> Integer
wordToInteger w#
  | isTrue# (i# >=# 0#) = S# i#
  | True                = Jp# (wordToBigNat w#)
  where
    i# = word2Int# w#
{-# CONSTANT_FOLDED wordToInteger #-}

wordToNegInteger :: Word# -> Integer
wordToNegInteger w#
  | isTrue# (i# <=# 0#) = S# i#
  | True                = Jn# (wordToBigNat w#)
  where
    i# = negateInt# (word2Int# w#)

-- we could almost auto-derive Ord if it wasn't for the Jn#-Jn# case
compareInteger :: Integer -> Integer -> Ordering
compareInteger (Jn# x)  (Jn# y) = compareBigNat y x
compareInteger (S#  x)  (S#  y) = compareInt#   x y
compareInteger (Jp# x)  (Jp# y) = compareBigNat x y
compareInteger (Jn# _)  _       = LT
compareInteger (S#  _)  (Jp# _) = LT
compareInteger (S#  _)  (Jn# _) = GT
compareInteger (Jp# _)  _       = GT
{-# CONSTANT_FOLDED compareInteger #-}

isNegInteger# :: Integer -> Int#
isNegInteger# (S# i#) = i# <# 0#
isNegInteger# (Jp# _)  = 0#
isNegInteger# (Jn# _)  = 1#

-- | Not-equal predicate.
neqInteger :: Integer -> Integer -> Bool
neqInteger x y = isTrue# (neqInteger# x y)

eqInteger, leInteger, ltInteger, gtInteger, geInteger
  :: Integer -> Integer -> Bool
eqInteger  x y = isTrue# (eqInteger#  x y)
leInteger  x y = isTrue# (leInteger#  x y)
ltInteger  x y = isTrue# (ltInteger#  x y)
gtInteger  x y = isTrue# (gtInteger#  x y)
geInteger  x y = isTrue# (geInteger#  x y)

eqInteger#, neqInteger#, leInteger#, ltInteger#, gtInteger#, geInteger#
  :: Integer -> Integer -> Int#
eqInteger# (S# x#) (S# y#)   = x# ==# y#
eqInteger# (Jn# x) (Jn# y)   = eqBigNat# x y
eqInteger# (Jp# x) (Jp# y)   = eqBigNat# x y
eqInteger# _       _         = 0#
{-# CONSTANT_FOLDED eqInteger# #-}

neqInteger# (S# x#) (S# y#)  = x# /=# y#
neqInteger# (Jn# x) (Jn# y)  = neqBigNat# x y
neqInteger# (Jp# x) (Jp# y)  = neqBigNat# x y
neqInteger# _       _        = 1#
{-# CONSTANT_FOLDED neqInteger# #-}


gtInteger# (S# x#) (S# y#)   = x# ># y#
gtInteger# x y | inline compareInteger x y == GT  = 1#
gtInteger# _ _                                    = 0#
{-# CONSTANT_FOLDED gtInteger# #-}

leInteger# (S# x#) (S# y#)   = x# <=# y#
leInteger# x y | inline compareInteger x y /= GT  = 1#
leInteger# _ _                             = 0#
{-# CONSTANT_FOLDED leInteger# #-}

ltInteger# (S# x#) (S# y#)   = x# <# y#
ltInteger# x y | inline compareInteger x y == LT  = 1#
ltInteger# _ _                             = 0#
{-# CONSTANT_FOLDED ltInteger# #-}

geInteger# (S# x#) (S# y#)   = x# >=# y#
geInteger# x y | inline compareInteger x y /= LT  = 1#
geInteger# _ _                             = 0#
{-# CONSTANT_FOLDED geInteger# #-}

-- | Compute absolute value of an 'Integer'
absInteger :: Integer -> Integer
absInteger (Jn# n)                       = Jp# n
absInteger (S# INT_MINBOUND#)            = Jp# (wordToBigNat ABS_INT_MINBOUND##)
absInteger (S# i#) | isTrue# (i# <# 0#)  = S# (negateInt# i#)
absInteger i@(S# _)                      = i
absInteger i@(Jp# _)                     = i
{-# CONSTANT_FOLDED absInteger #-}

-- | Return @-1@, @0@, and @1@ depending on whether argument is
-- negative, zero, or positive, respectively
signumInteger :: Integer -> Integer
signumInteger j = S# (signumInteger# j)
{-# CONSTANT_FOLDED signumInteger #-}

-- | Return @-1#@, @0#@, and @1#@ depending on whether argument is
-- negative, zero, or positive, respectively
signumInteger# :: Integer -> Int#
signumInteger# (Jn# _)  = -1#
signumInteger# (S# i#) = sgnI# i#
signumInteger# (Jp# _ ) =  1#

-- | Negate 'Integer'
negateInteger :: Integer -> Integer
negateInteger (Jn# n)      = Jp# n
negateInteger (S# INT_MINBOUND#) = Jp# (wordToBigNat ABS_INT_MINBOUND##)
negateInteger (S# i#)             = S# (negateInt# i#)
negateInteger (Jp# bn)
  | isTrue# (eqBigNatWord# bn ABS_INT_MINBOUND##) = S# INT_MINBOUND#
  | True                                        = Jn# bn
{-# CONSTANT_FOLDED negateInteger #-}

-- one edge-case issue to take into account is that Int's range is not
-- symmetric around 0.  I.e. @minBound+maxBound = -1@
--
-- Jp# is used iff n > maxBound::Int
-- Jn# is used iff n < minBound::Int

-- | Add two 'Integer's
plusInteger :: Integer -> Integer -> Integer
plusInteger x    (S# 0#)  = x
plusInteger (S# 0#) y     = y
plusInteger (S# x#) (S# y#)
  = case addIntC# x# y# of
    (# z#, 0# #) -> S# z#
    (# 0#, _  #) -> Jn# (wordToBigNat2 1## 0##) -- 2*minBound::Int
    (# z#, _  #)
      | isTrue# (z# ># 0#) -> Jn# (wordToBigNat ( (int2Word# (negateInt# z#))))
      | True               -> Jp# (wordToBigNat ( (int2Word# z#)))
plusInteger y@(S# _) x = plusInteger x y
-- no S# as first arg from here on
plusInteger (Jp# x) (Jp# y) = Jp# (plusBigNat x y)
plusInteger (Jn# x) (Jn# y) = Jn# (plusBigNat x y)
plusInteger (Jp# x) (S# y#) -- edge-case: @(maxBound+1) + minBound == 0@
  | isTrue# (y# >=# 0#) = Jp# (plusBigNatWord x (int2Word# y#))
  | True                = bigNatToInteger (minusBigNatWord x (int2Word#
                                                              (negateInt# y#)))
plusInteger (Jn# x) (S# y#) -- edge-case: @(minBound-1) + maxBound == -2@
  | isTrue# (y# >=# 0#) = bigNatToNegInteger (minusBigNatWord x (int2Word# y#))
  | True                = Jn# (plusBigNatWord x (int2Word# (negateInt# y#)))
plusInteger y@(Jn# _) x@(Jp# _) = plusInteger x y
plusInteger (Jp# x) (Jn# y)
    = case compareBigNat x y of
      LT -> bigNatToNegInteger (minusBigNat y x)
      EQ -> S# 0#
      GT -> bigNatToInteger (minusBigNat x y)
{-# CONSTANT_FOLDED plusInteger #-}

-- TODO
-- | Subtract two 'Integer's from each other.
minusInteger :: Integer -> Integer -> Integer
minusInteger x y = inline plusInteger x (inline negateInteger y)
{-# CONSTANT_FOLDED minusInteger #-}

-- | Multiply two 'Integer's
timesInteger :: Integer -> Integer -> Integer
timesInteger !_      (S# 0#) = S# 0#
timesInteger (S# 0#) _       = S# 0#
timesInteger x       (S# 1#) = x
timesInteger (S# 1#) y       = y
timesInteger x      (S# -1#) = negateInteger x
timesInteger (S# -1#) y      = negateInteger y
timesInteger (S# x#) (S# y#)
  = case mulIntMayOflo# x# y# of
    0# -> S# (x# *# y#)
    _  -> timesInt2Integer x# y#
timesInteger x@(S# _) y      = timesInteger y x
-- no S# as first arg from here on
timesInteger (Jp# x) (Jp# y) = Jp# (timesBigNat x y)
timesInteger (Jp# x) (Jn# y) = Jn# (timesBigNat x y)
timesInteger (Jp# x) (S# y#)
  | isTrue# (y# >=# 0#) = Jp# (timesBigNatWord x (int2Word# y#))
  | True       = Jn# (timesBigNatWord x (int2Word# (negateInt# y#)))
timesInteger (Jn# x) (Jn# y) = Jp# (timesBigNat x y)
timesInteger (Jn# x) (Jp# y) = Jn# (timesBigNat x y)
timesInteger (Jn# x) (S# y#)
  | isTrue# (y# >=# 0#) = Jn# (timesBigNatWord x (int2Word# y#))
  | True       = Jp# (timesBigNatWord x (int2Word# (negateInt# y#)))
{-# CONSTANT_FOLDED timesInteger #-}

-- | Square 'Integer'
sqrInteger :: Integer -> Integer
sqrInteger (S# INT_MINBOUND#) = timesInt2Integer INT_MINBOUND# INT_MINBOUND#
sqrInteger (S# j#) | isTrue# (absI# j# <=# SQRT_INT_MAXBOUND#) = S# (j# *# j#)
sqrInteger (S# j#) = timesInt2Integer j# j#
sqrInteger (Jp# bn) = Jp# (sqrBigNat bn)
sqrInteger (Jn# bn) = Jp# (sqrBigNat bn)

-- | Construct 'Integer' from the product of two 'Int#'s
timesInt2Integer :: Int# -> Int# -> Integer
timesInt2Integer x# y# = case (# isTrue# (x# >=# 0#), isTrue# (y# >=# 0#) #) of
    (# False, False #) -> case timesWord2# (int2Word# (negateInt# x#))
                                     (int2Word# (negateInt# y#)) of
        (# 0##,l #) -> inline wordToInteger l
        (# h  ,l #) -> Jp# (wordToBigNat2 h l)

    (#  True, False #) -> case timesWord2# (int2Word# x#)
                                     (int2Word# (negateInt# y#)) of
        (# 0##,l #) -> wordToNegInteger l
        (# h  ,l #) -> Jn# (wordToBigNat2 h l)

    (# False,  True #) -> case timesWord2# (int2Word# (negateInt# x#))
                                     (int2Word# y#) of
        (# 0##,l #) -> wordToNegInteger l
        (# h  ,l #) -> Jn# (wordToBigNat2 h l)

    (#  True,  True #) -> case timesWord2# (int2Word# x#)
                                     (int2Word# y#) of
        (# 0##,l #) -> inline wordToInteger l
        (# h  ,l #) -> Jp# (wordToBigNat2 h l)

bigNatToInteger :: BigNat -> Integer
bigNatToInteger bn
  | isTrue# ((sizeofBigNat# bn ==# 1#) `andI#` (i# >=# 0#)) = S# i#
  | True                                                    = Jp# bn
  where
    i# = word2Int# (bigNatToWord bn)

bigNatToNegInteger :: BigNat -> Integer
bigNatToNegInteger bn
  | isTrue# ((sizeofBigNat# bn ==# 1#) `andI#` (i# <=# 0#)) = S# i#
  | True                                                    = Jn# bn
  where
    i# = negateInt# (word2Int# (bigNatToWord bn))

-- | Count number of set bits. For negative arguments returns negative
-- population count of negated argument.
popCountInteger :: Integer -> Int#
popCountInteger (S# i#)
  | isTrue# (i# >=# 0#) = popCntI# i#
  | True                = negateInt# (popCntI# (negateInt# i#))
popCountInteger (Jp# bn)  = popCountBigNat bn
popCountInteger (Jn# bn)  = negateInt# (popCountBigNat bn)
{-# CONSTANT_FOLDED popCountInteger #-}

-- | 'Integer' for which only /n/-th bit is set. Undefined behaviour
-- for negative /n/ values.
bitInteger :: Int# -> Integer
bitInteger i#
  | isTrue# (i# <# (GMP_LIMB_BITS# -# 1#)) = S# (uncheckedIShiftL# 1# i#)
  | True = Jp# (bitBigNat i#)
{-# CONSTANT_FOLDED bitInteger #-}

-- | Test if /n/-th bit is set.
testBitInteger :: Integer -> Int# -> Bool
testBitInteger !_  n# | isTrue# (n# <# 0#) = False
testBitInteger (S# i#) n#
  | isTrue# (n# <# GMP_LIMB_BITS#) = isTrue# (((uncheckedIShiftL# 1# n#)
                                               `andI#` i#) /=# 0#)
  | True                          = isTrue# (i# <# 0#)
testBitInteger (Jp# bn) n = testBitBigNat bn n
testBitInteger (Jn# bn) n = testBitNegBigNat bn n
{-# CONSTANT_FOLDED testBitInteger #-}

-- | Bitwise @NOT@ operation
complementInteger :: Integer -> Integer
complementInteger (S# i#) = S# (notI# i#)
complementInteger (Jp# bn) = Jn# (plusBigNatWord  bn 1##)
complementInteger (Jn# bn) = Jp# (minusBigNatWord bn 1##)
{-# CONSTANT_FOLDED complementInteger #-}

-- | Arithmetic shift-right operation
--
-- Even though the shift-amount is expressed as `Int#`, the result is
-- undefined for negative shift-amounts.
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger x        0# = x
shiftRInteger (S# i#)  n# = S# (iShiftRA# i# n#)
  where
    iShiftRA# a b
      | isTrue# (b >=# WORD_SIZE_IN_BITS#) = (a <# 0#) *# (-1#)
      | True                               = a `uncheckedIShiftRA#` b
shiftRInteger (Jp# bn) n# = bigNatToInteger (shiftRBigNat bn n#)
shiftRInteger (Jn# bn) n#
    = case bigNatToNegInteger (shiftRNegBigNat bn n#) of
        S# 0# -> S# -1#
        r           -> r
{-# CONSTANT_FOLDED shiftRInteger #-}

-- | Shift-left operation
--
-- Even though the shift-amount is expressed as `Int#`, the result is
-- undefined for negative shift-amounts.
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger x       0#  = x
shiftLInteger (S# 0#) _  = S# 0#
shiftLInteger (S# 1#) n# = bitInteger n#
shiftLInteger (S# i#) n#
  | isTrue# (i# >=# 0#)   = bigNatToInteger (shiftLBigNat
                                             (wordToBigNat (int2Word# i#)) n#)
  | True               = bigNatToNegInteger (shiftLBigNat
                                             (wordToBigNat (int2Word#
                                                         (negateInt# i#))) n#)
shiftLInteger (Jp# bn) n# = Jp# (shiftLBigNat bn n#)
shiftLInteger (Jn# bn) n# = Jn# (shiftLBigNat bn n#)
{-# CONSTANT_FOLDED shiftLInteger #-}

-- | Bitwise OR operation
orInteger :: Integer -> Integer -> Integer
-- short-cuts
orInteger  (S# 0#)     y         = y
orInteger  x           (S# 0#)   = x
orInteger  (S# -1#)    _         = S# -1#
orInteger  _           (S# -1#)  = S# -1#
-- base-cases
orInteger  (S# x#)     (S# y#)   = S# (orI# x# y#)
orInteger  (Jp# x)     (Jp# y)   = Jp# (orBigNat x y)
orInteger  (Jn# x)     (Jn# y)
    = bigNatToNegInteger (plusBigNatWord (andBigNat
                                          (minusBigNatWord x 1##)
                                          (minusBigNatWord y 1##)) 1##)
orInteger  x@(Jn# _)   y@(Jp# _)  = orInteger y x -- retry with swapped args
orInteger  (Jp# x)     (Jn# y)
    = bigNatToNegInteger (plusBigNatWord (andnBigNat (minusBigNatWord y 1##) x)
                                         1##)
-- TODO/FIXpromotion-hack
orInteger  x@(S# _)   y          = orInteger (unsafePromote x) y
orInteger  x           y {- S# -}= orInteger x (unsafePromote y)
{-# CONSTANT_FOLDED orInteger #-}

-- | Bitwise XOR operation
xorInteger :: Integer -> Integer -> Integer
-- short-cuts
xorInteger (S# 0#)     y          = y
xorInteger x           (S# 0#)    = x
-- TODO: (S# -1) cases
-- base-cases
xorInteger (S# x#)     (S# y#)    = S# (xorI# x# y#)
xorInteger (Jp# x)     (Jp# y)    = bigNatToInteger (xorBigNat x y)
xorInteger (Jn# x)     (Jn# y)
    = bigNatToInteger (xorBigNat (minusBigNatWord x 1##)
                                 (minusBigNatWord y 1##))
xorInteger x@(Jn# _)   y@(Jp# _)  = xorInteger y x -- retry with swapped args
xorInteger (Jp# x)     (Jn# y)
    = bigNatToNegInteger (plusBigNatWord (xorBigNat x (minusBigNatWord y 1##))
                                         1##)
-- TODO/FIXME promotion-hack
xorInteger x@(S# _)    y          = xorInteger (unsafePromote x) y
xorInteger x           y {- S# -} = xorInteger x (unsafePromote y)
{-# CONSTANT_FOLDED xorInteger #-}

-- | Bitwise AND operation
andInteger :: Integer -> Integer -> Integer
-- short-cuts
andInteger (S# 0#)     !_        = S# 0#
andInteger _           (S# 0#)   = S# 0#
andInteger (S# -1#)   y          = y
andInteger x           (S# -1#)  = x
-- base-cases
andInteger (S# x#)     (S# y#)   = S# (andI# x# y#)
andInteger (Jp# x)     (Jp# y)   = bigNatToInteger (andBigNat x y)
andInteger (Jn# x)     (Jn# y)
    = bigNatToNegInteger (plusBigNatWord (orBigNat (minusBigNatWord x 1##)
                                                   (minusBigNatWord y 1##)) 1##)
andInteger x@(Jn# _)   y@(Jp# _)  = andInteger y x
andInteger (Jp# x)     (Jn# y)
    = bigNatToInteger (andnBigNat x (minusBigNatWord y 1##))
-- TODO/FIXME promotion-hack
andInteger x@(S# _)   y          = andInteger (unsafePromote x) y
andInteger x           y {- S# -}= andInteger x (unsafePromote y)
{-# CONSTANT_FOLDED andInteger #-}

-- HACK warning! breaks invariant on purpose
unsafePromote :: Integer -> Integer
unsafePromote (S# x#)
    | isTrue# (x# >=# 0#) = Jp# (wordToBigNat (int2Word# x#))
    | True                = Jn# (wordToBigNat (int2Word# (negateInt# x#)))
unsafePromote x = x

-- | Simultaneous 'quotInteger' and 'remInteger'.
--
-- Divisor must be non-zero otherwise the GHC runtime will terminate
-- with a division-by-zero fault.
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger n       (S# 1#) = (# n, S# 0# #)
quotRemInteger n      (S# -1#) = let !q = negateInteger n in (# q, (S# 0#) #)
quotRemInteger !_      (S# 0#) = (# S# (quotInt# 0# 0#),S# (remInt# 0# 0#) #)
quotRemInteger (S# 0#) _       = (# S# 0#, S# 0# #)
quotRemInteger (S# n#) (S# d#) = case quotRemInt# n# d# of
    (# q#, r# #) -> (# S# q#, S# r# #)
quotRemInteger (Jp# n)  (Jp# d)  = case quotRemBigNat n d of
    (# q, r #) -> (# bigNatToInteger q, bigNatToInteger r #)
quotRemInteger (Jp# n)  (Jn# d)  = case quotRemBigNat n d of
    (# q, r #) -> (# bigNatToNegInteger q, bigNatToInteger r #)
quotRemInteger (Jn# n)  (Jn# d)  = case quotRemBigNat n d of
    (# q, r #) -> (# bigNatToInteger q, bigNatToNegInteger r #)
quotRemInteger (Jn# n)  (Jp# d)  = case quotRemBigNat n d of
    (# q, r #) -> (# bigNatToNegInteger q, bigNatToNegInteger r #)
quotRemInteger (Jp# n)  (S# d#)
  | isTrue# (d# >=# 0#) = case quotRemBigNatWord n (int2Word# d#) of
      (# q, r# #) -> (# bigNatToInteger q, inline wordToInteger r# #)
  | True               = case quotRemBigNatWord n (int2Word# (negateInt# d#)) of
      (# q, r# #) -> (# bigNatToNegInteger q, inline wordToInteger r# #)
quotRemInteger (Jn# n)  (S# d#)
  | isTrue# (d# >=# 0#) = case quotRemBigNatWord n (int2Word# d#) of
      (# q, r# #) -> (# bigNatToNegInteger q, wordToNegInteger r# #)
  | True               = case quotRemBigNatWord n (int2Word# (negateInt# d#)) of
      (# q, r# #) -> (# bigNatToInteger q, wordToNegInteger r# #)
quotRemInteger n@(S# _) (Jn# _) = (# S# 0#, n #) -- since @n < d@
quotRemInteger n@(S# n#) (Jp# d) -- need to account for (S# minBound)
    | isTrue# (n# ># 0#)                                    = (# S# 0#, n #)
    | isTrue# (gtBigNatWord# d (int2Word# (negateInt# n#))) = (# S# 0#, n #)
    | True {- abs(n) == d -}                          = (# S# -1#, S# 0# #)
{-# CONSTANT_FOLDED quotRemInteger #-}


quotInteger :: Integer -> Integer -> Integer
quotInteger n       (S# 1#) = n
quotInteger n      (S# -1#) = negateInteger n
quotInteger !_      (S# 0#) = S# (quotInt# 0# 0#)
quotInteger (S# 0#) _       = S# 0#
quotInteger (S# n#)  (S# d#) = S# (quotInt# n# d#)
quotInteger (Jp# n)   (S# d#)
  | isTrue# (d# >=# 0#) = bigNatToInteger    (quotBigNatWord n (int2Word# d#))
  | True                = bigNatToNegInteger (quotBigNatWord n
                                              (int2Word# (negateInt# d#)))
quotInteger (Jn# n)   (S# d#)
  | isTrue# (d# >=# 0#) = bigNatToNegInteger (quotBigNatWord n (int2Word# d#))
  | True                = bigNatToInteger    (quotBigNatWord n
                                              (int2Word# (negateInt# d#)))
quotInteger (Jp# n) (Jp# d) = bigNatToInteger    (quotBigNat n d)
quotInteger (Jp# n) (Jn# d) = bigNatToNegInteger (quotBigNat n d)
quotInteger (Jn# n) (Jp# d) = bigNatToNegInteger (quotBigNat n d)
quotInteger (Jn# n) (Jn# d) = bigNatToInteger    (quotBigNat n d)
-- handle remaining non-allocating cases
quotInteger n d = case inline quotRemInteger n d of (# q, _ #) -> q
{-# CONSTANT_FOLDED quotInteger #-}

remInteger :: Integer -> Integer -> Integer
remInteger !_       (S# 1#) = S# 0#
remInteger _       (S# -1#) = S# 0#
remInteger _        (S# 0#) = S# (remInt# 0# 0#)
remInteger (S# 0#) _        = S# 0#
remInteger (S# n#) (S# d#) = S# (remInt# n# d#)
remInteger (Jp# n)  (S# d#)
    = wordToInteger    (remBigNatWord n (int2Word# (absI# d#)))
remInteger (Jn# n)  (S# d#)
    = wordToNegInteger (remBigNatWord n (int2Word# (absI# d#)))
remInteger (Jp# n)  (Jp# d)  = bigNatToInteger    (remBigNat n d)
remInteger (Jp# n)  (Jn# d)  = bigNatToInteger    (remBigNat n d)
remInteger (Jn# n)  (Jp# d)  = bigNatToNegInteger (remBigNat n d)
remInteger (Jn# n)  (Jn# d)  = bigNatToNegInteger (remBigNat n d)
-- handle remaining non-allocating cases
remInteger n d = case inline quotRemInteger n d of (# _, r #) -> r
{-# CONSTANT_FOLDED remInteger #-}

-- | Simultaneous 'divInteger' and 'modInteger'.
--
-- Divisor must be non-zero otherwise the GHC runtime will terminate
-- with a division-by-zero fault.
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger n d
  | isTrue# (signumInteger# r ==# negateInt# (signumInteger# d))
     = let !q' = plusInteger q (S# -1#) -- TODO: optimize
           !r' = plusInteger r d
       in (# q', r' #)
  | True = qr
  where
    qr@(# q, r #) = quotRemInteger n d
{-# CONSTANT_FOLDED divModInteger #-}

divInteger :: Integer -> Integer -> Integer
-- same-sign ops can be handled by more efficient 'quotInteger'
divInteger n d | isTrue# (isNegInteger# n ==# isNegInteger# d) = quotInteger n d
divInteger n d = case inline divModInteger n d of (# q, _ #) -> q
{-# CONSTANT_FOLDED divInteger #-}

modInteger :: Integer -> Integer -> Integer
-- same-sign ops can be handled by more efficient 'remInteger'
modInteger n d | isTrue# (isNegInteger# n ==# isNegInteger# d) = remInteger n d
modInteger n d = case inline divModInteger n d of (# _, r #) -> r
{-# CONSTANT_FOLDED modInteger #-}

-- | Compute greatest common divisor.
gcdInteger :: Integer -> Integer -> Integer
gcdInteger (S# 0#)        b = absInteger b
gcdInteger a        (S# 0#) = absInteger a
gcdInteger (S# 1#)        _ = S# 1#
gcdInteger (S# -1#)       _ = S# 1#
gcdInteger _        (S# 1#) = S# 1#
gcdInteger _       (S# -1#) = S# 1#
gcdInteger (S# a#) (S# b#)
    = wordToInteger (gcdWord# (int2Word# (absI# a#)) (int2Word# (absI# b#)))
gcdInteger a@(S# _) b = gcdInteger b a
gcdInteger (Jn# a) b = gcdInteger (Jp# a) b
gcdInteger (Jp# a) (Jp# b) = bigNatToInteger (gcdBigNat a b)
gcdInteger (Jp# a) (Jn# b) = bigNatToInteger (gcdBigNat a b)
gcdInteger (Jp# a) (S# b#)
    = wordToInteger (gcdBigNatWord a (int2Word# (absI# b#)))
{-# CONSTANT_FOLDED gcdInteger #-}

-- | Compute least common multiple.
lcmInteger :: Integer -> Integer -> Integer
lcmInteger (S# 0#) !_  = S# 0#
lcmInteger (S# 1#)  b  = absInteger b
lcmInteger (S# -1#) b  = absInteger b
lcmInteger _ (S# 0#)   = S# 0#
lcmInteger a (S# 1#)   = absInteger a
lcmInteger a (S# -1#)  = absInteger a
lcmInteger a b = (aa `quotInteger` (aa `gcdInteger` ab)) `timesInteger` ab
  where
    aa = absInteger a
    ab = absInteger b
{-# CONSTANT_FOLDED lcmInteger #-}

-- | Compute greatest common divisor.
--
-- __Warning__: result may become negative if (at least) one argument
-- is 'minBound'
gcdInt :: Int# -> Int# -> Int#
gcdInt x# y#
    = word2Int# (gcdWord# (int2Word# (absI# x#)) (int2Word# (absI# y#)))

-- | Compute greatest common divisor.
--
-- @since 1.0.0.0
gcdWord :: Word# -> Word# -> Word#
gcdWord = gcdWord#

----------------------------------------------------------------------------
-- BigNat operations

compareBigNat :: BigNat -> BigNat -> Ordering
compareBigNat x@(BN# x#) y@(BN# y#)
  | isTrue# (nx# ==# ny#)
      = compareInt# (narrowCInt# (c_mpn_cmp x# y# nx#)) 0#
  | isTrue# (nx# <#  ny#) = LT
  | True                  = GT
  where
    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

compareBigNatWord :: BigNat -> GmpLimb# -> Ordering
compareBigNatWord bn w#
  | isTrue# (sizeofBigNat# bn ==# 1#) = cmpW# (bigNatToWord bn) w#
  | True                              = GT

gtBigNatWord# :: BigNat -> GmpLimb# -> Int#
gtBigNatWord# bn w#
    = (sizeofBigNat# bn ># 1#) `orI#` (bigNatToWord bn `gtWord#` w#)

eqBigNat :: BigNat -> BigNat -> Bool
eqBigNat x y = isTrue# (eqBigNat# x y)

eqBigNat# :: BigNat -> BigNat -> Int#
eqBigNat# x@(BN# x#) y@(BN# y#)
  | isTrue# (nx# ==# ny#) = c_mpn_cmp x# y# nx# ==# 0#
  | True                  = 0#
  where
    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

neqBigNat# :: BigNat -> BigNat -> Int#
neqBigNat# x@(BN# x#) y@(BN# y#)
  | isTrue# (nx# ==# ny#) = c_mpn_cmp x# y# nx# /=# 0#
  | True                  = 1#
  where
    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

eqBigNatWord :: BigNat -> GmpLimb# -> Bool
eqBigNatWord bn w# = isTrue# (eqBigNatWord# bn w#)

eqBigNatWord# :: BigNat -> GmpLimb# -> Int#
eqBigNatWord# bn w#
    = (sizeofBigNat# bn ==# 1#) `andI#` (bigNatToWord bn `eqWord#` w#)


-- | Same as @'indexBigNat#' bn 0\#@
bigNatToWord :: BigNat -> Word#
bigNatToWord bn = indexBigNat# bn 0#

-- | Equivalent to @'word2Int#' . 'bigNatToWord'@
bigNatToInt :: BigNat -> Int#
bigNatToInt (BN# ba#) = indexIntArray# ba# 0#

-- | CAF representing the value @0 :: BigNat@
zeroBigNat :: BigNat
zeroBigNat = runS $ do
    mbn <- newBigNat# 1#
    _ <- svoid (writeBigNat# mbn 0# 0##)
    unsafeFreezeBigNat# mbn
{-# NOINLINE zeroBigNat #-}

-- | Test if 'BigNat' value is equal to zero.
isZeroBigNat :: BigNat -> Bool
isZeroBigNat bn = eqBigNatWord bn 0##

-- | CAF representing the value @1 :: BigNat@
oneBigNat :: BigNat
oneBigNat = runS $ do
    mbn <- newBigNat# 1#
    _ <- svoid (writeBigNat# mbn 0# 1##)
    unsafeFreezeBigNat# mbn
{-# NOINLINE oneBigNat #-}

czeroBigNat :: BigNat
czeroBigNat = runS $ do
    mbn <- newBigNat# 1#
    _ <- svoid (writeBigNat# mbn 0# (not# 0##))
    unsafeFreezeBigNat# mbn
{-# NOINLINE czeroBigNat #-}

-- | Special 0-sized bigNat returned in case of arithmetic underflow
--
-- This is currently only returned by the following operations:
--
--  - 'minusBigNat'
--  - 'minusBigNatWord'
--
-- Other operations such as 'quotBigNat' may return 'nullBigNat' as
-- well as a dummy/place-holder value instead of 'undefined' since we
-- can't throw exceptions. But that behaviour should not be relied
-- upon.
--
-- NB: @isValidBigNat# nullBigNat@ is false
nullBigNat :: BigNat
nullBigNat = runS (newBigNat# 0# >>= unsafeFreezeBigNat#)
{-# NOINLINE nullBigNat #-}

-- | Test for special 0-sized 'BigNat' representing underflows.
isNullBigNat# :: BigNat -> Int#
isNullBigNat# (BN# ba#) = sizeofByteArray# ba# ==# 0#

-- | Construct 1-limb 'BigNat' from 'Word#'
wordToBigNat :: Word# -> BigNat
wordToBigNat 0## = zeroBigNat
wordToBigNat 1## = oneBigNat
wordToBigNat w#
  | isTrue# (not# w# `eqWord#` 0##) = czeroBigNat
  | True = runS $ do
    mbn <- newBigNat# 1#
    _ <- svoid (writeBigNat# mbn 0# w#)
    unsafeFreezeBigNat# mbn

-- | Construct BigNat from 2 limbs.
-- The first argument is the most-significant limb.
wordToBigNat2 :: Word# -> Word# -> BigNat
wordToBigNat2 0## lw# = wordToBigNat lw#
wordToBigNat2 hw# lw# = runS $ do
    mbn <- newBigNat# 2#
    _ <- svoid (writeBigNat# mbn 0# lw#)
    _ <- svoid (writeBigNat# mbn 1# hw#)
    unsafeFreezeBigNat# mbn

plusBigNat :: BigNat -> BigNat -> BigNat
plusBigNat x y
  | isTrue# (eqBigNatWord# x 0##) = y
  | isTrue# (eqBigNatWord# y 0##) = x
  | isTrue# (nx# >=# ny#) = go x nx# y ny#
  | True                  = go y ny# x nx#
  where
    go (BN# a#) na# (BN# b#) nb# = runS $ do
        mbn@(MBN# mba#) <- newBigNat# na#
        (W# c#) <- liftIO (c_mpn_add mba# a# na# b# nb#)
        case c# of
              0## -> unsafeFreezeBigNat# mbn
              _   -> unsafeSnocFreezeBigNat# mbn c#

    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

plusBigNatWord :: BigNat -> GmpLimb# -> BigNat
plusBigNatWord x          0## = x
plusBigNatWord x@(BN# x#) y# = runS $ do
    mbn@(MBN# mba#) <- newBigNat# nx#
    (W# c#) <- liftIO (c_mpn_add_1 mba# x# nx# y#)
    case c# of
        0## -> unsafeFreezeBigNat# mbn
        _   -> unsafeSnocFreezeBigNat# mbn c#
  where
    nx# = sizeofBigNat# x

-- | Returns 'nullBigNat' (see 'isNullBigNat#') in case of underflow
minusBigNat :: BigNat -> BigNat -> BigNat
minusBigNat x@(BN# x#) y@(BN# y#)
  | isZeroBigNat y = x
  | isTrue# (nx# >=# ny#) = runS $ do
    mbn@(MBN# mba#) <- newBigNat# nx#
    (W# b#) <- liftIO (c_mpn_sub mba# x# nx# y# ny#)
    case b# of
        0## -> unsafeRenormFreezeBigNat# mbn
        _   -> return nullBigNat

  | True = nullBigNat
  where
    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

-- | Returns 'nullBigNat' (see 'isNullBigNat#') in case of underflow
minusBigNatWord :: BigNat -> GmpLimb# -> BigNat
minusBigNatWord x 0## = x
minusBigNatWord x@(BN# x#) y# = runS $ do
    mbn@(MBN# mba#) <- newBigNat# nx#
    (W# b#) <- liftIO $ c_mpn_sub_1 mba# x# nx# y#
    case b# of
        0## -> unsafeRenormFreezeBigNat# mbn
        _   -> return nullBigNat
  where
    nx# = sizeofBigNat# x


timesBigNat :: BigNat -> BigNat -> BigNat
timesBigNat x y
  | isZeroBigNat x = zeroBigNat
  | isZeroBigNat y = zeroBigNat
  | isTrue# (nx# >=# ny#) = go x nx# y ny#
  | True                  = go y ny# x nx#
  where
    go (BN# a#) na# (BN# b#) nb# = runS $ do
        let n# = nx# +# ny#
        mbn@(MBN# mba#) <- newBigNat# n#
        (W# msl#) <- liftIO (c_mpn_mul mba# a# na# b# nb#)
        case msl# of
              0## -> unsafeShrinkFreezeBigNat# mbn (n# -# 1#)
              _   -> unsafeFreezeBigNat# mbn

    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

-- | Square 'BigNat'
sqrBigNat :: BigNat -> BigNat
sqrBigNat x
  | isZeroBigNat x = zeroBigNat
  -- TODO: 1-limb BigNats below sqrt(maxBound::GmpLimb)
sqrBigNat x = timesBigNat x x -- TODO: mpn_sqr

timesBigNatWord :: BigNat -> GmpLimb# -> BigNat
timesBigNatWord !_ 0## = zeroBigNat
timesBigNatWord x 1## = x
timesBigNatWord x@(BN# x#) y#
  | isTrue# (nx# ==# 1#) =
      let (# !h#, !l# #) = timesWord2# (bigNatToWord x) y#
      in wordToBigNat2 h# l#
  | True = runS $ do
        mbn@(MBN# mba#) <- newBigNat# nx#
        (W# msl#) <- liftIO (c_mpn_mul_1 mba# x# nx# y#)
        case msl# of
              0## -> unsafeFreezeBigNat# mbn
              _   -> unsafeSnocFreezeBigNat# mbn msl#

  where
    nx# = sizeofBigNat# x

-- | Specialised version of
--
-- > bitBigNat = shiftLBigNat (wordToBigNat 1##)
--
-- avoiding a few redundant allocations
bitBigNat :: Int# -> BigNat
bitBigNat i#
  | isTrue# (i#  <#  0#) = zeroBigNat -- or maybe 'nullBigNat'?
  | isTrue# (i# ==#  0#) = oneBigNat
  | True = runS $ do
      mbn@(MBN# mba#) <- newBigNat# (li# +# 1#)
      -- FIXME: do we really need to zero-init MBAs returned by 'newByteArray#'?
      -- clear all limbs (except for the most-significant limb)
      _ <- svoid (setByteArray# mba# 0# (li# `uncheckedIShiftL#` GMP_LIMB_SHIFT#) 0#)
      -- set single bit in most-significant limb
      _ <- svoid (writeBigNat# mbn li# (uncheckedShiftL# 1## bi#))
      unsafeFreezeBigNat# mbn
  where
    (# li#, bi# #) = quotRemInt# i# GMP_LIMB_BITS#

testBitBigNat :: BigNat -> Int# -> Bool
testBitBigNat bn i#
  | isTrue# (i#  <#  0#) = False
  | isTrue# (li# <# nx#) = isTrue# (testBitWord# (indexBigNat# bn li#) bi#)
  | True                 = False
  where
    (# li#, bi# #) = quotRemInt# i# GMP_LIMB_BITS#
    nx# = sizeofBigNat# bn

testBitNegBigNat :: BigNat -> Int# -> Bool
testBitNegBigNat bn i#
  | isTrue# (i#  <#  0#)  = False
  | isTrue# (li# >=# nx#) = True
  | allZ li# = isTrue# ((testBitWord#
                         (indexBigNat# bn li# `minusWord#` 1##) bi#) ==# 0#)
  | True     = isTrue# ((testBitWord# (indexBigNat# bn li#) bi#) ==# 0#)
  where
    (# li#, bi# #) = quotRemInt# i# GMP_LIMB_BITS#
    nx# = sizeofBigNat# bn

    allZ 0# = True
    allZ j | isTrue# (indexBigNat# bn (j -# 1#) `eqWord#` 0##) = allZ (j -# 1#)
           | True                 = False

popCountBigNat :: BigNat -> Int#
popCountBigNat bn@(BN# ba#) = word2Int# (c_mpn_popcount ba# (sizeofBigNat# bn))


shiftLBigNat :: BigNat -> Int# -> BigNat
shiftLBigNat x 0# = x
shiftLBigNat x _ | isZeroBigNat x = zeroBigNat
shiftLBigNat x@(BN# xba#) n# = runS $ do
    ymbn@(MBN# ymba#) <- newBigNat# yn#
    W# ymsl <- liftIO (c_mpn_lshift ymba# xba# xn# (int2Word# n#))
    case ymsl of
        0## -> unsafeShrinkFreezeBigNat# ymbn (yn# -# 1#)
        _   -> unsafeFreezeBigNat# ymbn
  where
    xn# = sizeofBigNat# x
    yn# = xn# +# nlimbs# +# (nbits# /=# 0#)
    (# nlimbs#, nbits# #) = quotRemInt# n# GMP_LIMB_BITS#



shiftRBigNat :: BigNat -> Int# -> BigNat
shiftRBigNat x 0# = x
shiftRBigNat x _ | isZeroBigNat x = zeroBigNat
shiftRBigNat x@(BN# xba#) n#
  | isTrue# (nlimbs# >=# xn#) = zeroBigNat
  | True = runS $ do
      ymbn@(MBN# ymba#) <- newBigNat# yn#
      W# ymsl <- liftIO (c_mpn_rshift ymba# xba# xn# (int2Word# n#))
      case ymsl of
          0## -> unsafeRenormFreezeBigNat# ymbn -- may shrink more than one
          _   -> unsafeFreezeBigNat# ymbn
  where
    xn# = sizeofBigNat# x
    yn# = xn# -# nlimbs#
    nlimbs# = quotInt# n# GMP_LIMB_BITS#

shiftRNegBigNat :: BigNat -> Int# -> BigNat
shiftRNegBigNat x 0# = x
shiftRNegBigNat x _ | isZeroBigNat x = zeroBigNat
shiftRNegBigNat x@(BN# xba#) n#
  | isTrue# (nlimbs# >=# xn#) = zeroBigNat
  | True = runS $ do
      ymbn@(MBN# ymba#) <- newBigNat# yn#
      W# ymsl <- liftIO (c_mpn_rshift_2c ymba# xba# xn# (int2Word# n#))
      case ymsl of
          0## -> unsafeRenormFreezeBigNat# ymbn -- may shrink more than one
          _   -> unsafeFreezeBigNat# ymbn
  where
    xn# = sizeofBigNat# x
    yn# = xn# -# nlimbs#
    nlimbs# = quotInt# n# GMP_LIMB_BITS#


orBigNat :: BigNat -> BigNat -> BigNat
orBigNat x@(BN# x#) y@(BN# y#)
  | isZeroBigNat x = y
  | isZeroBigNat y = x
  | isTrue# (nx# >=# ny#) = runS (ior' x# nx# y# ny#)
  | True                  = runS (ior' y# ny# x# nx#)
  where
    ior' a# na# b# nb# = do -- na >= nb
        mbn@(MBN# mba#) <- newBigNat# na#
        _ <- liftIO (c_mpn_ior_n mba# a# b# nb#)
        _ <- case isTrue# (na# ==# nb#) of
            False -> svoid (copyWordArray# a# nb# mba# nb# (na# -# nb#))
            True  -> return ()
        unsafeFreezeBigNat# mbn

    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y


xorBigNat :: BigNat -> BigNat -> BigNat
xorBigNat x@(BN# x#) y@(BN# y#)
  | isZeroBigNat x = y
  | isZeroBigNat y = x
  | isTrue# (nx# >=# ny#) = runS (xor' x# nx# y# ny#)
  | True                  = runS (xor' y# ny# x# nx#)
  where
    xor' a# na# b# nb# = do -- na >= nb
        mbn@(MBN# mba#) <- newBigNat# na#
        _ <- liftIO (c_mpn_xor_n mba# a# b# nb#)
        case isTrue# (na# ==# nb#) of
            False -> do _ <- svoid (copyWordArray# a# nb# mba# nb# (na# -# nb#))
                        unsafeFreezeBigNat# mbn
            True  -> unsafeRenormFreezeBigNat# mbn

    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

-- | aka @\x y -> x .&. (complement y)@
andnBigNat :: BigNat -> BigNat -> BigNat
andnBigNat x@(BN# x#) y@(BN# y#)
  | isZeroBigNat x = zeroBigNat
  | isZeroBigNat y = x
  | True = runS $ do
      mbn@(MBN# mba#) <- newBigNat# nx#
      _ <- liftIO (c_mpn_andn_n mba# x# y# n#)
      _ <- case isTrue# (nx# ==# n#) of
            False -> svoid (copyWordArray# x# n# mba# n# (nx# -# n#))
            True  -> return ()
      unsafeRenormFreezeBigNat# mbn
  where
    n# | isTrue# (nx# <# ny#) = nx#
       | True                 = ny#
    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y


andBigNat :: BigNat -> BigNat -> BigNat
andBigNat x@(BN# x#) y@(BN# y#)
  | isZeroBigNat x = zeroBigNat
  | isZeroBigNat y = zeroBigNat
  | True = runS $ do
      mbn@(MBN# mba#) <- newBigNat# n#
      _ <- liftIO (c_mpn_and_n mba# x# y# n#)
      unsafeRenormFreezeBigNat# mbn
  where
    n# | isTrue# (nx# <# ny#) = nx#
       | True                 = ny#
    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

-- | If divisor is zero, @(\# 'nullBigNat', 'nullBigNat' \#)@ is returned
quotRemBigNat :: BigNat -> BigNat -> (# BigNat,BigNat #)
quotRemBigNat n@(BN# nba#) d@(BN# dba#)
  | isZeroBigNat d     = (# nullBigNat, nullBigNat #)
  | eqBigNatWord d 1## = (# n, zeroBigNat #)
  | n < d              = (# zeroBigNat, n #)
  | True = case runS go of (!q,!r) -> (# q, r #)
  where
    nn# = sizeofBigNat# n
    dn# = sizeofBigNat# d
    qn# = 1# +# nn# -# dn#
    rn# = dn#

    go = do
      qmbn@(MBN# qmba#) <- newBigNat# qn#
      rmbn@(MBN# rmba#) <- newBigNat# rn#

      _ <- liftIO (c_mpn_tdiv_qr qmba# rmba# 0# nba# nn# dba# dn#)

      q <- unsafeRenormFreezeBigNat# qmbn
      r <- unsafeRenormFreezeBigNat# rmbn
      return (q, r)

quotBigNat :: BigNat -> BigNat -> BigNat
quotBigNat n@(BN# nba#) d@(BN# dba#)
  | isZeroBigNat d     = nullBigNat
  | eqBigNatWord d 1## = n
  | n < d              = zeroBigNat
  | True = runS $ do
      let nn# = sizeofBigNat# n
      let dn# = sizeofBigNat# d
      let qn# = 1# +# nn# -# dn#
      qmbn@(MBN# qmba#) <- newBigNat# qn#
      _ <- liftIO (c_mpn_tdiv_q qmba# nba# nn# dba# dn#)
      unsafeRenormFreezeBigNat# qmbn

remBigNat :: BigNat -> BigNat -> BigNat
remBigNat n@(BN# nba#) d@(BN# dba#)
  | isZeroBigNat d     = nullBigNat
  | eqBigNatWord d 1## = zeroBigNat
  | n < d              = n
  | True = runS $ do
      let nn# = sizeofBigNat# n
      let dn# = sizeofBigNat# d
      rmbn@(MBN# rmba#) <- newBigNat# dn#
      _ <- liftIO (c_mpn_tdiv_r rmba# nba# nn# dba# dn#)
      unsafeRenormFreezeBigNat# rmbn

-- | Note: Result of div/0 undefined
quotRemBigNatWord :: BigNat -> GmpLimb# -> (# BigNat, GmpLimb# #)
quotRemBigNatWord !_           0## = (# nullBigNat, 0## #)
quotRemBigNatWord n            1## = (# n,          0## #)
quotRemBigNatWord n@(BN# nba#) d# = case compareBigNatWord n d# of
    LT -> (# zeroBigNat, bigNatToWord n #)
    EQ -> (# oneBigNat, 0## #)
    GT -> case runS go of (!q,!(W# r#)) -> (# q, r# #) -- TODO: handle word/word
  where
    go = do
      let nn# = sizeofBigNat# n
      qmbn@(MBN# qmba#) <- newBigNat# nn#
      r <- liftIO (c_mpn_divrem_1 qmba# 0# nba# nn# d#)
      q <- unsafeRenormFreezeBigNat# qmbn
      return (q,r)

quotBigNatWord :: BigNat -> GmpLimb# -> BigNat
quotBigNatWord n d# = case inline quotRemBigNatWord n d# of (# q, _ #) -> q

-- | div/0 not checked
remBigNatWord :: BigNat -> GmpLimb# -> Word#
remBigNatWord n@(BN# nba#) d# = c_mpn_mod_1 nba# (sizeofBigNat# n) d#

gcdBigNatWord :: BigNat -> Word# -> Word#
gcdBigNatWord bn@(BN# ba#) = c_mpn_gcd_1# ba# (sizeofBigNat# bn)

gcdBigNat :: BigNat -> BigNat -> BigNat
gcdBigNat x@(BN# x#) y@(BN# y#)
  | isZeroBigNat x = y
  | isZeroBigNat y = x
  | isTrue# (nx# >=# ny#) = runS (gcd' x# nx# y# ny#)
  | True                  = runS (gcd' y# ny# x# nx#)
  where
    gcd' a# na# b# nb# = do -- na >= nb
        mbn@(MBN# mba#) <- newBigNat# nb#
        I# rn'# <- liftIO (c_mpn_gcd# mba# a# na# b# nb#)
        let rn# = narrowGmpSize# rn'#
        case isTrue# (rn# ==# nb#) of
            False -> unsafeShrinkFreezeBigNat# mbn rn#
            True  -> unsafeFreezeBigNat# mbn

    nx# = sizeofBigNat# x
    ny# = sizeofBigNat# y

-- | Extended euclidean algorithm.
--
-- For @/a/@ and @/b/@, compute their greatest common divisor @/g/@
-- and the coefficient @/s/@ satisfying @/a//s/ + /b//t/ = /g/@.
--
-- @since 0.5.1.0
{-# NOINLINE gcdExtInteger #-}
gcdExtInteger :: Integer -> Integer -> (# Integer, Integer #)
gcdExtInteger a b = case gcdExtSBigNat a' b' of
    (# g, s #) -> let !g' = bigNatToInteger  g
                      !s' = sBigNatToInteger s
                  in (# g', s' #)
  where
    a' = integerToSBigNat a
    b' = integerToSBigNat b

-- internal helper
gcdExtSBigNat :: SBigNat -> SBigNat -> (# BigNat, SBigNat #)
gcdExtSBigNat x y = case runS go of (g,s) -> (# g, s #)
  where
    go = do
        g@(MBN# g#) <- newBigNat# gn0#
        s@(MBN# s#) <- newBigNat# (absI# xn#)
        I# ssn_# <- liftIO (integer_gmp_gcdext# s# g# x# xn# y# yn#)
        let ssn# = narrowGmpSize# ssn_#
            sn#  = absI# ssn#
        s' <- unsafeShrinkFreezeBigNat# s sn#
        g' <- unsafeRenormFreezeBigNat# g
        case isTrue# (ssn# >=# 0#) of
            False -> return ( g', NegBN s' )
            True  -> return ( g', PosBN s' )

    !(BN# x#) = absSBigNat x
    !(BN# y#) = absSBigNat y
    xn# = ssizeofSBigNat# x
    yn# = ssizeofSBigNat# y

    gn0# = minI# (absI# xn#) (absI# yn#)

----------------------------------------------------------------------------
-- modular exponentiation

-- | \"@'powModInteger' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @abs(/m/)@.
--
-- Negative exponents are supported if an inverse modulo @/m/@
-- exists.
--
-- __Warning__: It's advised to avoid calling this primitive with
-- negative exponents unless it is guaranteed the inverse exists, as
-- failure to do so will likely cause program abortion due to a
-- divide-by-zero fault. See also 'recipModInteger'.
--
-- Future versions of @integer_gmp@ may not support negative @/e/@
-- values anymore.
--
-- @since 0.5.1.0
{-# NOINLINE powModInteger #-}
powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger (S# b#) (S# e#) (S# m#)
  | isTrue# (b# >=# 0#), isTrue# (e# >=# 0#)
  = wordToInteger (powModWord (int2Word# b#) (int2Word# e#)
                              (int2Word# (absI# m#)))
powModInteger b e m = case m of
    (S# m#) -> wordToInteger (powModSBigNatWord b' e' (int2Word# (absI# m#)))
    (Jp# m') -> bigNatToInteger (powModSBigNat b' e' m')
    (Jn# m') -> bigNatToInteger (powModSBigNat b' e' m')
  where
    b' = integerToSBigNat b
    e' = integerToSBigNat e

-- | Version of 'powModInteger' operating on 'BigNat's
--
-- @since 1.0.0.0
powModBigNat :: BigNat -> BigNat -> BigNat -> BigNat
powModBigNat b e m = inline powModSBigNat (PosBN b) (PosBN e) m

-- | Version of 'powModInteger' for 'Word#'-sized moduli
--
-- @since 1.0.0.0
powModBigNatWord :: BigNat -> BigNat -> GmpLimb# -> GmpLimb#
powModBigNatWord b e m# = inline powModSBigNatWord (PosBN b) (PosBN e) m#

-- | Version of 'powModInteger' operating on 'Word#'s
--
-- @since 1.0.0.0
foreign import ccall unsafe "integer_gmp_powm_word"
  powModWord :: GmpLimb# -> GmpLimb# -> GmpLimb# -> GmpLimb#

-- internal non-exported helper
powModSBigNat :: SBigNat -> SBigNat -> BigNat -> BigNat
powModSBigNat b e m@(BN# m#) = runS $ do
    r@(MBN# r#) <- newBigNat# mn#
    I# rn_# <- liftIO (integer_gmp_powm# r# b# bn# e# en# m# mn#)
    let rn# = narrowGmpSize# rn_#
    case isTrue# (rn# ==# mn#) of
        False -> unsafeShrinkFreezeBigNat# r rn#
        True  -> unsafeFreezeBigNat# r
  where
    !(BN# b#) = absSBigNat b
    !(BN# e#) = absSBigNat e
    bn# = ssizeofSBigNat# b
    en# = ssizeofSBigNat# e
    mn# = sizeofBigNat# m

foreign import ccall unsafe "integer_gmp_powm"
  integer_gmp_powm# :: MutableByteArray# RealWorld
                       -> ByteArray# -> GmpSize# -> ByteArray# -> GmpSize#
                       -> ByteArray# -> GmpSize# -> IO GmpSize

-- internal non-exported helper
powModSBigNatWord :: SBigNat -> SBigNat -> GmpLimb# -> GmpLimb#
powModSBigNatWord b e m# = integer_gmp_powm1# b# bn# e# en# m#
  where
    !(BN# b#) = absSBigNat b
    !(BN# e#) = absSBigNat e
    bn# = ssizeofSBigNat# b
    en# = ssizeofSBigNat# e

foreign import ccall unsafe "integer_gmp_powm1"
  integer_gmp_powm1# :: ByteArray# -> GmpSize# -> ByteArray# -> GmpSize#
                        -> GmpLimb# -> GmpLimb#


-- | \"@'recipModInteger' /x/ /m/@\" computes the inverse of @/x/@ modulo @/m/@. If
-- the inverse exists, the return value @/y/@ will satisfy @0 < /y/ <
-- abs(/m/)@, otherwise the result is @0@.
--
-- @since 0.5.1.0
{-# NOINLINE recipModInteger #-}
recipModInteger :: Integer -> Integer -> Integer
recipModInteger (S# x#) (S# m#)
  | isTrue# (x# >=# 0#)
  = wordToInteger (recipModWord (int2Word# x#) (int2Word# (absI# m#)))
recipModInteger x m = bigNatToInteger (recipModSBigNat x' m')
  where
    x' = integerToSBigNat x
    m' = absSBigNat (integerToSBigNat m)

-- | Version of 'recipModInteger' operating on 'BigNat's
--
-- @since 1.0.0.0
recipModBigNat :: BigNat -> BigNat -> BigNat
recipModBigNat x m = inline recipModSBigNat (PosBN x) m

-- | Version of 'recipModInteger' operating on 'Word#'s
--
-- @since 1.0.0.0
foreign import ccall unsafe "integer_gmp_invert_word"
  recipModWord :: GmpLimb# -> GmpLimb# -> GmpLimb#

-- internal non-exported helper
recipModSBigNat :: SBigNat -> BigNat -> BigNat
recipModSBigNat x m@(BN# m#) = runS $ do
    r@(MBN# r#) <- newBigNat# mn#
    I# rn_# <- liftIO (integer_gmp_invert# r# x# xn# m# mn#)
    let rn# = narrowGmpSize# rn_#
    case isTrue# (rn# ==# mn#) of
        False -> unsafeShrinkFreezeBigNat# r rn#
        True  -> unsafeFreezeBigNat# r
  where
    !(BN# x#) = absSBigNat x
    xn# = ssizeofSBigNat# x
    mn# = sizeofBigNat# m

foreign import ccall unsafe "integer_gmp_invert"
  integer_gmp_invert# :: MutableByteArray# RealWorld
                         -> ByteArray# -> GmpSize#
                         -> ByteArray# -> GmpSize# -> IO GmpSize

----------------------------------------------------------------------------
-- Conversions to/from floating point

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
-- decodeDoubleInteger 0.0## = (# S# 0#, 0# #)
#if WORD_SIZE_IN_BITS == 64
decodeDoubleInteger x = case decodeDouble_Int64# x of
                          (# m#, e# #) -> (# S# m#, e# #)
#elif WORD_SIZE_IN_BITS == 32
decodeDoubleInteger x = case decodeDouble_Int64# x of
                          (# m#, e# #) -> (# int64ToInteger m#, e# #)
#endif
{-# CONSTANT_FOLDED decodeDoubleInteger #-}

-- provided by GHC's RTS
foreign import ccall unsafe "__int_encodeDouble"
  int_encodeDouble# :: Int# -> Int# -> Double#

encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (S# m#) 0# = int2Double# m#
encodeDoubleInteger (S# m#) e# = int_encodeDouble# m# e#
encodeDoubleInteger (Jp# bn@(BN# bn#)) e#
    = c_mpn_get_d bn# (sizeofBigNat# bn) e#
encodeDoubleInteger (Jn# bn@(BN# bn#)) e#
    = c_mpn_get_d bn# (negateInt# (sizeofBigNat# bn)) e#
{-# CONSTANT_FOLDED encodeDoubleInteger #-}

-- double integer_gmp_mpn_get_d (const mp_limb_t sp[], const mp_size_t sn)
foreign import ccall unsafe "integer_gmp_mpn_get_d"
  c_mpn_get_d :: ByteArray# -> GmpSize# -> Int# -> Double#

doubleFromInteger :: Integer -> Double#
doubleFromInteger (S# m#) = int2Double# m#
doubleFromInteger (Jp# bn@(BN# bn#))
    = c_mpn_get_d bn# (sizeofBigNat# bn) 0#
doubleFromInteger (Jn# bn@(BN# bn#))
    = c_mpn_get_d bn# (negateInt# (sizeofBigNat# bn)) 0#
{-# CONSTANT_FOLDED doubleFromInteger #-}

-- TODO: Not sure if it's worth to write 'Float' optimized versions here
floatFromInteger :: Integer -> Float#
floatFromInteger i = double2Float# (doubleFromInteger i)

encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger m e = double2Float# (encodeDoubleInteger m e)

----------------------------------------------------------------------------
-- FFI ccall imports

foreign import ccall unsafe "integer_gmp_gcd_word"
  gcdWord# :: GmpLimb# -> GmpLimb# -> GmpLimb#

foreign import ccall unsafe "integer_gmp_mpn_gcd_1"
  c_mpn_gcd_1# :: ByteArray# -> GmpSize# -> GmpLimb# -> GmpLimb#

foreign import ccall unsafe "integer_gmp_mpn_gcd"
  c_mpn_gcd# :: MutableByteArray# s -> ByteArray# -> GmpSize#
                -> ByteArray# -> GmpSize# -> IO GmpSize

foreign import ccall unsafe "integer_gmp_gcdext"
  integer_gmp_gcdext# :: MutableByteArray# s -> MutableByteArray# s
                         -> ByteArray# -> GmpSize#
                         -> ByteArray# -> GmpSize# -> IO GmpSize

-- mp_limb_t mpn_add_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n,
--                      mp_limb_t s2limb)
foreign import ccall unsafe "gmp.h __gmpn_add_1"
  c_mpn_add_1 :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpLimb#
                 -> IO GmpLimb

-- mp_limb_t mpn_sub_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n,
--                      mp_limb_t s2limb)
foreign import ccall unsafe "gmp.h __gmpn_sub_1"
  c_mpn_sub_1 :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpLimb#
                 -> IO GmpLimb

-- mp_limb_t mpn_mul_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n,
--                      mp_limb_t s2limb)
foreign import ccall unsafe "gmp.h __gmpn_mul_1"
  c_mpn_mul_1 :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpLimb#
                 -> IO GmpLimb

-- mp_limb_t mpn_add (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n,
--                    const mp_limb_t *s2p, mp_size_t s2n)
foreign import ccall unsafe "gmp.h __gmpn_add"
  c_mpn_add :: MutableByteArray# s -> ByteArray# -> GmpSize#
               -> ByteArray# -> GmpSize# -> IO GmpLimb

-- mp_limb_t mpn_sub (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n,
--                    const mp_limb_t *s2p, mp_size_t s2n)
foreign import ccall unsafe "gmp.h __gmpn_sub"
  c_mpn_sub :: MutableByteArray# s -> ByteArray# -> GmpSize# -> ByteArray#
               -> GmpSize# -> IO GmpLimb

-- mp_limb_t mpn_mul (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n,
--                    const mp_limb_t *s2p, mp_size_t s2n)
foreign import ccall unsafe "gmp.h __gmpn_mul"
  c_mpn_mul :: MutableByteArray# s -> ByteArray# -> GmpSize# -> ByteArray#
               -> GmpSize# -> IO GmpLimb

-- int mpn_cmp (const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
foreign import ccall unsafe "gmp.h __gmpn_cmp"
  c_mpn_cmp :: ByteArray# -> ByteArray# -> GmpSize# -> CInt#

-- void mpn_tdiv_qr (mp_limb_t *qp, mp_limb_t *rp, mp_size_t qxn,
--                   const mp_limb_t *np, mp_size_t nn,
--                   const mp_limb_t *dp, mp_size_t dn)
foreign import ccall unsafe "gmp.h __gmpn_tdiv_qr"
  c_mpn_tdiv_qr :: MutableByteArray# s -> MutableByteArray# s -> GmpSize#
                   -> ByteArray# -> GmpSize# -> ByteArray# -> GmpSize# -> IO ()

foreign import ccall unsafe "integer_gmp_mpn_tdiv_q"
  c_mpn_tdiv_q :: MutableByteArray# s -> ByteArray# -> GmpSize# -> ByteArray#
                  -> GmpSize# -> IO ()

foreign import ccall unsafe "integer_gmp_mpn_tdiv_r"
  c_mpn_tdiv_r :: MutableByteArray# s -> ByteArray# -> GmpSize# -> ByteArray#
                  -> GmpSize# -> IO ()

-- mp_limb_t mpn_divrem_1 (mp_limb_t *r1p, mp_size_t qxn, mp_limb_t *s2p,
--                         mp_size_t s2n, mp_limb_t s3limb)
foreign import ccall unsafe "gmp.h __gmpn_divrem_1"
  c_mpn_divrem_1 :: MutableByteArray# s -> GmpSize# -> ByteArray# -> GmpSize#
                    -> GmpLimb# -> IO GmpLimb

-- mp_limb_t mpn_mod_1 (const mp_limb_t *s1p, mp_size_t s1n, mp_limb_t s2limb)
foreign import ccall unsafe "gmp.h __gmpn_mod_1"
  c_mpn_mod_1 :: ByteArray# -> GmpSize# -> GmpLimb# -> GmpLimb#

-- mp_limb_t integer_gmp_mpn_rshift (mp_limb_t rp[], const mp_limb_t sp[],
--                                   mp_size_t sn, mp_bitcnt_t count)
foreign import ccall unsafe "integer_gmp_mpn_rshift"
  c_mpn_rshift :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpBitCnt#
                  -> IO GmpLimb

-- mp_limb_t integer_gmp_mpn_rshift (mp_limb_t rp[], const mp_limb_t sp[],
--                                   mp_size_t sn, mp_bitcnt_t count)
foreign import ccall unsafe "integer_gmp_mpn_rshift_2c"
  c_mpn_rshift_2c :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpBitCnt#
                     -> IO GmpLimb

-- mp_limb_t integer_gmp_mpn_lshift (mp_limb_t rp[], const mp_limb_t sp[],
--                                   mp_size_t sn, mp_bitcnt_t count)
foreign import ccall unsafe "integer_gmp_mpn_lshift"
  c_mpn_lshift :: MutableByteArray# s -> ByteArray# -> GmpSize# -> GmpBitCnt#
                  -> IO GmpLimb

-- void mpn_and_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p,
--                 mp_size_t n)
foreign import ccall unsafe "integer_gmp_mpn_and_n"
  c_mpn_and_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> GmpSize#
                 -> IO ()

-- void mpn_andn_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p,
--                  mp_size_t n)
foreign import ccall unsafe "integer_gmp_mpn_andn_n"
  c_mpn_andn_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> GmpSize#
                  -> IO ()

-- void mpn_ior_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p,
--                 mp_size_t n)
foreign import ccall unsafe "integer_gmp_mpn_ior_n"
  c_mpn_ior_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> GmpSize#
                 -> IO ()

-- void mpn_xor_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p,
--                 mp_size_t n)
foreign import ccall unsafe "integer_gmp_mpn_xor_n"
  c_mpn_xor_n :: MutableByteArray# s -> ByteArray# -> ByteArray# -> GmpSize#
                 -> IO ()

-- mp_bitcnt_t mpn_popcount (const mp_limb_t *s1p, mp_size_t n)
foreign import ccall unsafe "gmp.h __gmpn_popcount"
  c_mpn_popcount :: ByteArray# -> GmpSize# -> GmpBitCnt#

----------------------------------------------------------------------------
-- BigNat-wrapped ByteArray#-primops

-- | Return number of limbs contained in 'BigNat'.
sizeofBigNat# :: BigNat -> GmpSize#
sizeofBigNat# (BN# x#)
    = sizeofByteArray# x# `uncheckedIShiftRL#` GMP_LIMB_SHIFT#

data MutBigNat s = MBN# !(MutableByteArray# s)

getSizeofMutBigNat# :: MutBigNat s -> State# s -> (# State# s, GmpSize# #)
--getSizeofMutBigNat# :: MutBigNat s -> S s GmpSize#
getSizeofMutBigNat# (MBN# x#) s =
    case getSizeofMutableByteArray# x# s of
        (# s', n# #) -> (# s', n# `uncheckedIShiftRL#` GMP_LIMB_SHIFT# #)

newBigNat# :: GmpSize# -> S s (MutBigNat s)
newBigNat# limbs# s =
    case newByteArray# (limbs# `uncheckedIShiftL#` GMP_LIMB_SHIFT#) s of
        (# s', mba# #) -> (# s', MBN# mba# #)

writeBigNat# :: MutBigNat s -> GmpSize# -> GmpLimb# -> State# s -> State# s
writeBigNat# (MBN# mba#) = writeWordArray# mba#

-- | Extract /n/-th (0-based) limb in 'BigNat'.
-- /n/ must be less than size as reported by 'sizeofBigNat#'.
indexBigNat# :: BigNat -> GmpSize# -> GmpLimb#
indexBigNat# (BN# ba#) = indexWordArray# ba#

unsafeFreezeBigNat# :: MutBigNat s -> S s BigNat
unsafeFreezeBigNat# (MBN# mba#) s = case unsafeFreezeByteArray# mba# s of
                                      (# s', ba# #) -> (# s', BN# ba# #)

resizeMutBigNat# :: MutBigNat s -> GmpSize# -> S s (MutBigNat s)
resizeMutBigNat# (MBN# mba0#) nsz# s
  | isTrue# (bsz# ==# n#) = (# s', MBN# mba0# #)
  | True =
    case resizeMutableByteArray# mba0# bsz# s' of
        (# s'', mba# #) -> (# s'', MBN# mba# #)
  where
    bsz# = nsz# `uncheckedIShiftL#` GMP_LIMB_SHIFT#
    (# s', n# #) = getSizeofMutableByteArray# mba0# s

shrinkMutBigNat# :: MutBigNat s -> GmpSize# -> State# s -> State# s
shrinkMutBigNat# (MBN# mba0#) nsz# s
  | isTrue# (bsz# ==# n#) = s' -- no-op
  | True                  = shrinkMutableByteArray# mba0# bsz# s'
  where
    bsz# = nsz# `uncheckedIShiftL#` GMP_LIMB_SHIFT#
    (# s', n# #) = getSizeofMutableByteArray# mba0# s

unsafeSnocFreezeBigNat# :: MutBigNat s -> GmpLimb# -> S s BigNat
unsafeSnocFreezeBigNat# mbn0@(MBN# mba0#) limb# s = go s'
  where
    n#   = nb0# `uncheckedIShiftRL#` GMP_LIMB_SHIFT#
    (# s', nb0# #) = getSizeofMutableByteArray# mba0# s
    go = do
        (MBN# mba#) <- resizeMutBigNat# mbn0 (n# +# 1#)
        _ <- svoid (writeWordArray# mba# n# limb#)
        unsafeFreezeBigNat# (MBN# mba#)

-- | May shrink underlyng 'ByteArray#' if needed to satisfy BigNat invariant
unsafeRenormFreezeBigNat# :: MutBigNat s -> S s BigNat
unsafeRenormFreezeBigNat# mbn s
  | isTrue# (n0# ==# 0#)  = (# s'', nullBigNat #)
  | isTrue# (n#  ==# 0#)  = (# s'', zeroBigNat #)
  | isTrue# (n#  ==# n0#) = (unsafeFreezeBigNat# mbn) s''
  | True                  = (unsafeShrinkFreezeBigNat# mbn n#) s''
  where
    (# s', n0# #) = getSizeofMutBigNat# mbn s
    (# s'', n# #) = normSizeofMutBigNat'# mbn n0# s'

-- | Shrink MBN
unsafeShrinkFreezeBigNat# :: MutBigNat s -> GmpSize# -> S s BigNat
unsafeShrinkFreezeBigNat# x@(MBN# xmba) 1#
    = \s -> case readWordArray# xmba 0# s of
        (# s', w#   #) -> freezeOneLimb w# s'
  where
    freezeOneLimb 0## = return zeroBigNat
    freezeOneLimb 1## = return oneBigNat
    freezeOneLimb w# | isTrue# (not# w# `eqWord#` 0##) = return czeroBigNat
    freezeOneLimb _   = do
        _ <- svoid (shrinkMutBigNat# x 1#)
        unsafeFreezeBigNat# x
unsafeShrinkFreezeBigNat# x y# = do
    _ <- svoid (shrinkMutBigNat# x y#)
    unsafeFreezeBigNat# x


copyWordArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int#
                  -> State# s -> State# s
copyWordArray# src src_ofs dst dst_ofs len
  = copyByteArray# src (src_ofs `uncheckedIShiftL#` GMP_LIMB_SHIFT#)
                   dst (dst_ofs `uncheckedIShiftL#` GMP_LIMB_SHIFT#)
                   (len `uncheckedIShiftL#` GMP_LIMB_SHIFT#)

-- | Version of 'normSizeofMutBigNat'#' which scans all allocated 'MutBigNat#'
normSizeofMutBigNat# :: MutBigNat s -> State# s -> (# State# s, Int# #)
normSizeofMutBigNat# mbn@(MBN# mba) s = normSizeofMutBigNat'# mbn sz# s'
  where
    (# s', n# #) = getSizeofMutableByteArray# mba s
    sz# = n# `uncheckedIShiftRA#` GMP_LIMB_SHIFT#

-- | Find most-significant non-zero limb and return its index-position
-- plus one. Start scanning downward from the initial limb-size
-- (i.e. start-index plus one) given as second argument.
--
-- NB: The 'normSizeofMutBigNat' of 'zeroBigNat' would be @0#@
normSizeofMutBigNat'# :: MutBigNat s -> GmpSize#
                         -> State# s -> (# State# s, GmpSize# #)
normSizeofMutBigNat'# (MBN# mba) = go
  where
    go  0# s = (# s, 0# #)
    go i0# s = case readWordArray# mba (i0# -# 1#) s of
        (# s', 0## #) -> go (i0# -# 1#) s'
        (# s', _  #) -> (# s', i0# #)

-- | Construct 'BigNat' from existing 'ByteArray#' containing /n/
-- 'GmpLimb's in least-significant-first order.
--
-- If possible 'ByteArray#', will be used directly (i.e. shared
-- /without/ cloning the 'ByteArray#' into a newly allocated one)
--
-- Note: size parameter (times @sizeof(GmpLimb)@) must be less or
-- equal to its 'sizeofByteArray#'.
byteArrayToBigNat# :: ByteArray# -> GmpSize# -> BigNat
byteArrayToBigNat# ba# n0#
  | isTrue# (n#  ==# 0#)    = zeroBigNat
  | isTrue# (baszr# ==# 0#) -- i.e. ba# is multiple of limb-size
  , isTrue# (baszq# ==# n#) = (BN# ba#)
  | True = runS $ \s ->
      let (# s', mbn@(MBN# mba#) #) = newBigNat# n# s
          (# s'', ba_sz# #) = getSizeofMutableByteArray# mba# s'
          go = do _ <- svoid (copyByteArray# ba# 0# mba# 0# ba_sz# )
                  unsafeFreezeBigNat# mbn
      in go s''
  where
    (# baszq#, baszr# #) = quotRemInt# (sizeofByteArray# ba#) GMP_LIMB_BYTES#

    n#  = fmssl (n0# -# 1#)

    -- find most signifcant set limb, return normalized size
    fmssl i#
      | isTrue# (i# <# 0#)                             = 0#
      | isTrue# (neWord# (indexWordArray# ba# i#) 0##) = i# +# 1#
      | True                                           = fmssl (i# -# 1#)

-- | Read 'Integer' (without sign) from memory location at @/addr/@ in
-- base-256 representation.
--
-- @'importIntegerFromAddr' /addr/ /size/ /msbf/@
--
-- See description of 'importIntegerFromByteArray' for more details.
--
-- @since 1.0.0.0
importIntegerFromAddr :: Addr# -> Word# -> Int# -> IO Integer
importIntegerFromAddr addr len msbf = IO $ do
    bn <- liftIO (importBigNatFromAddr addr len msbf)
    return (bigNatToInteger bn)

-- | Version of 'importIntegerFromAddr' constructing a 'BigNat'
importBigNatFromAddr :: Addr# -> Word# -> Int# -> IO BigNat
importBigNatFromAddr _ 0## _ = IO (\s -> (# s, zeroBigNat #))
importBigNatFromAddr addr len0 1# = IO $ do -- MSBF
    W# ofs <- liftIO (c_scan_nzbyte_addr addr 0## len0)
    let len = len0 `minusWord#` ofs
        addr' = addr `plusAddr#` (word2Int# ofs)
    importBigNatFromAddr# addr' len 1#
importBigNatFromAddr addr len0 _ = IO $ do -- LSBF
    W# len <- liftIO (c_rscan_nzbyte_addr addr 0## len0)
    importBigNatFromAddr# addr len 0#

foreign import ccall unsafe "integer_gmp_scan_nzbyte"
    c_scan_nzbyte_addr :: Addr# -> Word# -> Word# -> IO Word

foreign import ccall unsafe "integer_gmp_rscan_nzbyte"
    c_rscan_nzbyte_addr :: Addr# -> Word# -> Word# -> IO Word

-- | Helper for 'importBigNatFromAddr'
importBigNatFromAddr# :: Addr# -> Word# -> Int# -> S RealWorld BigNat
importBigNatFromAddr# _ 0## _ = return zeroBigNat
importBigNatFromAddr# addr len msbf = do
    mbn@(MBN# mba#) <- newBigNat# n#
    () <- liftIO (c_mpn_import_addr mba# addr 0## len msbf)
    unsafeFreezeBigNat# mbn
  where
    -- n = ceiling(len / SIZEOF_HSWORD), i.e. number of limbs required
    n# = (word2Int# len +# (SIZEOF_HSWORD# -# 1#)) `quotInt#` SIZEOF_HSWORD#

foreign import ccall unsafe "integer_gmp_mpn_import"
    c_mpn_import_addr :: MutableByteArray# RealWorld -> Addr# -> Word# -> Word#
                      -> Int# -> IO ()

-- | Read 'Integer' (without sign) from byte-array in base-256 representation.
--
-- The call
--
-- @'importIntegerFromByteArray' /ba/ /offset/ /size/ /msbf/@
--
-- reads
--
-- * @/size/@ bytes from the 'ByteArray#' @/ba/@ starting at @/offset/@
--
-- * with most significant byte first if @/msbf/@ is @1#@ or least
--   significant byte first if @/msbf/@ is @0#@, and
--
-- * returns a new 'Integer'
--
-- @since 1.0.0.0
importIntegerFromByteArray :: ByteArray# -> Word# -> Word# -> Int# -> Integer
importIntegerFromByteArray ba ofs len msbf
    = bigNatToInteger (importBigNatFromByteArray ba ofs len msbf)

-- | Version of 'importIntegerFromByteArray' constructing a 'BigNat'
importBigNatFromByteArray :: ByteArray# -> Word# -> Word# -> Int# -> BigNat
importBigNatFromByteArray _  _    0##  _  = zeroBigNat
importBigNatFromByteArray ba ofs0 len0 1# = runS $ do -- MSBF
    W# ofs <- liftIO (c_scan_nzbyte_bytearray ba ofs0 len0)
    let len = (len0 `plusWord#` ofs0) `minusWord#` ofs
    importBigNatFromByteArray# ba ofs len 1#
importBigNatFromByteArray ba ofs  len0 _  = runS $ do -- LSBF
    W# len <- liftIO (c_rscan_nzbyte_bytearray ba ofs len0)
    importBigNatFromByteArray# ba ofs len 0#

foreign import ccall unsafe "integer_gmp_scan_nzbyte"
    c_scan_nzbyte_bytearray :: ByteArray# -> Word# -> Word# -> IO Word

foreign import ccall unsafe "integer_gmp_rscan_nzbyte"
    c_rscan_nzbyte_bytearray :: ByteArray# -> Word# -> Word# -> IO Word

-- | Helper for 'importBigNatFromByteArray'
importBigNatFromByteArray# :: ByteArray# -> Word# -> Word# -> Int#
                           -> S RealWorld BigNat
importBigNatFromByteArray# _ _ 0## _ = return zeroBigNat
importBigNatFromByteArray# ba ofs len msbf = do
    mbn@(MBN# mba#) <- newBigNat# n#
    () <- liftIO (c_mpn_import_bytearray mba# ba ofs len msbf)
    unsafeFreezeBigNat# mbn
  where
    -- n = ceiling(len / SIZEOF_HSWORD), i.e. number of limbs required
    n# = (word2Int# len +# (SIZEOF_HSWORD# -# 1#)) `quotInt#` SIZEOF_HSWORD#

foreign import ccall unsafe "integer_gmp_mpn_import"
    c_mpn_import_bytearray :: MutableByteArray# RealWorld -> ByteArray# -> Word#
                           -> Word# -> Int# -> IO ()

-- | Test whether all internal invariants are satisfied by 'BigNat' value
--
-- Returns @1#@ if valid, @0#@ otherwise.
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
isValidBigNat# :: BigNat -> Int#
isValidBigNat# (BN# ba#)
  = (szq# ># 0#) `andI#` (szr# ==# 0#) `andI#` isNorm#
  where
    isNorm#
      | isTrue# (szq# ># 1#) = (indexWordArray# ba# (szq# -# 1#)) `neWord#` 0##
      | True                 = 1#

    sz# = sizeofByteArray# ba#

    (# szq#, szr# #) = quotRemInt# sz# GMP_LIMB_BYTES#

-- | Version of 'nextPrimeInteger' operating on 'BigNat's
--
-- @since 1.0.0.0
nextPrimeBigNat :: BigNat -> BigNat
nextPrimeBigNat bn@(BN# ba#) = runS $ do
    mbn@(MBN# mba#) <- newBigNat# n#
    (W# c#) <- liftIO (nextPrime# mba# ba# n#)
    case c# of
        0## -> unsafeFreezeBigNat# mbn
        _   -> unsafeSnocFreezeBigNat# mbn c#
  where
    n# = sizeofBigNat# bn

foreign import ccall unsafe "integer_gmp_next_prime"
  nextPrime# :: MutableByteArray# RealWorld -> ByteArray# -> GmpSize#
                -> IO GmpLimb

----------------------------------------------------------------------------
-- monadic combinators for low-level state threading

type S s a = State# s -> (# State# s, a #)

infixl 1 >>=
infixl 1 >>
infixr 0 $

{-# INLINE ($) #-}
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x

{-# INLINE (>>=) #-}
(>>=) :: S s a -> (a -> S s b) -> S s b
(>>=) m k = \s -> case m s of (# s', a #) -> k a s'

{-# INLINE (>>) #-}
(>>) :: S s a -> S s b -> S s b
(>>) m k = \s -> case m s of (# s', _ #) -> k s'

{-# INLINE svoid #-}
svoid :: (State# s -> State# s) -> S s ()
svoid m0 = \s -> case m0 s of s' -> (# s', () #)

{-# INLINE return #-}
return :: a -> S s a
return a = \s -> (# s, a #)

{-# INLINE liftIO #-}
liftIO :: IO a -> S RealWorld a
liftIO (IO m) = m

-- NB: equivalent of GHC.IO.unsafeDupablePerformIO, see notes there
runS :: S RealWorld a -> a
runS m = case runRW# m of (# _, a #) -> a

-- stupid hack
fail :: [Char] -> S s a
fail s = return (raise# s)

----------------------------------------------------------------------------

-- | Internal helper type for "signed" 'BigNat's
--
-- This is a useful abstraction for operations which support negative
-- mp_size_t arguments.
data SBigNat = NegBN !BigNat | PosBN !BigNat

-- | Absolute value of 'SBigNat'
absSBigNat :: SBigNat -> BigNat
absSBigNat (NegBN bn) = bn
absSBigNat (PosBN bn) = bn

-- | /Signed/ limb count. Negative sizes denote negative integers
ssizeofSBigNat# :: SBigNat -> GmpSize#
ssizeofSBigNat# (NegBN bn) = negateInt# (sizeofBigNat# bn)
ssizeofSBigNat# (PosBN bn) = sizeofBigNat# bn

-- | Construct 'SBigNat' from 'Int#' value
intToSBigNat# :: Int# -> SBigNat
intToSBigNat# 0#     = PosBN zeroBigNat
intToSBigNat# 1#     = PosBN oneBigNat
intToSBigNat# (-1#)  = NegBN oneBigNat
intToSBigNat# i# | isTrue# (i# ># 0#) = PosBN (wordToBigNat (int2Word# i#))
                 | True   = PosBN (wordToBigNat (int2Word# (negateInt# i#)))

-- | Convert 'Integer' into 'SBigNat'
integerToSBigNat :: Integer -> SBigNat
integerToSBigNat (S#  i#) = intToSBigNat# i#
integerToSBigNat (Jp# bn) = PosBN bn
integerToSBigNat (Jn# bn) = NegBN bn

-- | Convert 'SBigNat' into 'Integer'
sBigNatToInteger :: SBigNat -> Integer
sBigNatToInteger (NegBN bn) = bigNatToNegInteger bn
sBigNatToInteger (PosBN bn) = bigNatToInteger bn

----------------------------------------------------------------------------
-- misc helpers, some of these should rather be primitives exported by ghc-prim

cmpW# :: Word# -> Word# -> Ordering
cmpW# x# y#
  | isTrue# (x# `ltWord#` y#) = LT
  | isTrue# (x# `eqWord#` y#) = EQ
  | True                      = GT
{-# INLINE cmpW# #-}

bitWord# :: Int# -> Word#
bitWord# = uncheckedShiftL# 1##
{-# INLINE bitWord# #-}

testBitWord# :: Word# -> Int# -> Int#
testBitWord# w# i# = (bitWord# i# `and#` w#) `neWord#` 0##
{-# INLINE testBitWord# #-}

popCntI# :: Int# -> Int#
popCntI# i# = word2Int# (popCnt# (int2Word# i#))
{-# INLINE popCntI# #-}

-- branchless version
absI# :: Int# -> Int#
absI# i# = (i# `xorI#` nsign) -# nsign
  where
    -- nsign = negateInt# (i# <# 0#)
    nsign = uncheckedIShiftRA# i# (WORD_SIZE_IN_BITS# -# 1#)

-- branchless version
sgnI# :: Int# -> Int#
sgnI# x# = (x# ># 0#) -# (x# <# 0#)

cmpI# :: Int# -> Int# -> Int#
cmpI# x# y# = (x# ># y#) -# (x# <# y#)

minI# :: Int# -> Int# -> Int#
minI# x# y# | isTrue# (x# <=# y#) = x#
            | True                = y#
