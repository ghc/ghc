{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE RebindableSyntax         #-}
{-# LANGUAGE RoleAnnotations          #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}

-- Some overlapping patterns are used to avoid 'patError'
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- |
-- Module      :  GHC.Integer.Type
-- License     :  BSD3
-- Maintainer  :  sebastian.nagel@ncoding.at
--
-- Alternative implementation for the 'Integer' type using BIGNUM functions from
-- the OpenSSL's crypto/bn implementation.
--
-- Most functions were copied/adapted from integer-gmp and credit of this
-- implementation belongs to @hvr.
--
-- GHC needs this module to be named "GHC.Integer.Type" and provide all the
-- low-level 'Integer' operations.

module GHC.Integer.Type where

#include "MachDeps.h"

import GHC.Classes
import GHC.Magic
import GHC.Prim
import GHC.Types

-- * Integer functions

#if WORD_SIZE_IN_BITS == 64
# define INT_MINBOUND      -0x8000000000000000
# define INT_MAXBOUND       0x7fffffffffffffff
# define ABS_INT_MINBOUND   0x8000000000000000
# define WORD_SIZE_IN_BYTES 8
# define WORD_SHIFT         3
# define HIGH_HALF_SHIFT    32
# define LOW_HALF_MASK      0xffffffff
#elif WORD_SIZE_IN_BITS == 32
# define INT_MINBOUND       -0x80000000
# define INT_MAXBOUND       0x7fffffff
# define ABS_INT_MINBOUND   0x80000000
# define WORD_SIZE_IN_BYTES 4
# define WORD_SHIFT         2
# define HIGH_HALF_SHIFT    16
# define LOW_HALF_MASK      0xffff
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

data Integer = S# !Int#
               -- ^ small integer
             | Bp# {-# UNPACK #-} !BigNum
               -- ^ positive bignum, > maxbound(Int)
             | Bn# {-# UNPACK #-} !BigNum
               -- ^ negative bignum, < minbound(Int)

instance Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

instance Ord Integer where
    (<=) = leInteger
    (>) = gtInteger
    (<) = ltInteger
    (>=) = geInteger
    compare = compareInteger

-- ** Creation and conversion

-- | Construct 'Integer' value from list of 'Int's.
-- This function is used by GHC for constructing 'Integer' literals.
mkInteger :: Bool   -- ^ sign of integer ('True' if non-negative)
          -> [Int]  -- ^ absolute value expressed in 31 bit chunks, least
                    -- significant first
          -> Integer
mkInteger nonNegative is
  | nonNegative = f is
  | True = negateInteger (f is)
 where
  f [] = S# 0#
  f (I# i : is') = smallInteger (i `andI#` 0x7fffffff#) `orInteger` shiftLInteger (f is') 31#
{-# NOINLINE mkInteger #-}

-- | Create an Integer from a single Int#.
smallInteger :: Int# -> Integer
smallInteger i# = S# i#
{-# NOINLINE smallInteger #-}

-- | Create a (positive) Integer from a single Word#.
wordToInteger :: Word# -> Integer
wordToInteger w#
  | isTrue# (i# >=# 0#) = S# i#
  | True = Bp# (wordToBigNum w#)
  where
    i# = word2Int# w#
{-# NOINLINE wordToInteger #-}

-- | Create a (negative) Integer from a single Word#.
wordToNegInteger :: Word# -> Integer
wordToNegInteger w#
  | isTrue# (i# <=# 0#) = S# i#
  | True = Bn# (wordToBigNum w#)
  where
    i# = negateInt# (word2Int# w#)
-- inlinable as only internally used

-- | Truncates to least significant.
integerToWord :: Integer -> Word#
integerToWord (S# i#) = int2Word# i#
integerToWord (Bp# bn) = bigNumToWord bn
integerToWord (Bn# bn) = int2Word# (negateInt# (bigNumToInt bn))
{-# NOINLINE integerToWord #-}

integerToInt :: Integer -> Int#
integerToInt (S# i#) = i#
integerToInt (Bp# bn) = bigNumToInt bn
integerToInt (Bn# bn) = negateInt# (bigNumToInt bn)
{-# NOINLINE integerToInt #-}

floatFromInteger :: Integer -> Float#
floatFromInteger i = double2Float# (doubleFromInteger i)
{-# NOINLINE floatFromInteger #-}

doubleFromInteger :: Integer -> Double#
doubleFromInteger (S# i#) = int2Double# i#
doubleFromInteger (Bp# bn) = bigNumToDouble bn
doubleFromInteger (Bn# bn) = negateDouble# (bigNumToDouble bn)
{-# NOINLINE doubleFromInteger #-}

-- | Encodes the given integer into a double with the given exponent, i.e.
-- encodeDoubleInteger i e = i * 2 ^ e
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (S# 0#) _ = 0.0##
encodeDoubleInteger (S# m#) 0# = int2Double# m#
encodeDoubleInteger (S# m#) e# = int_encodeDouble# m# e#
encodeDoubleInteger (Bp# bn) e0 = f 0.0## 0# e0
 where
  f !acc !idx !e =
    let n = wordsInBigNum# bn
        newIdx = idx +# 1#
    in case isTrue# (idx ==# n) of
         True -> acc
         _ -> let d = indexBigNum# bn idx
                  newAcc = acc +## word_encodeDouble# d e
                  newE = e +# WORD_SIZE_IN_BITS#
              in  f newAcc newIdx newE
encodeDoubleInteger (Bn# bn) e0 =
  negateDouble# (encodeDoubleInteger (Bp# bn) e0)
{-# NOINLINE encodeDoubleInteger #-}

-- provided by GHC's RTS
foreign import ccall unsafe "__int_encodeDouble"
  int_encodeDouble# :: Int# -> Int# -> Double#

-- __word_encodeDouble does simply do some preparations and then calls
-- 'ldexp p1 p2' in C
foreign import ccall unsafe "__word_encodeDouble"
  word_encodeDouble# :: Word# -> Int# -> Double#

decodeDoubleInteger :: Double# -> (# Integer, Int# #)
#if WORD_SIZE_IN_BITS == 64
decodeDoubleInteger x = case decodeDouble_Int64# x of
                          (# m#, e# #) -> (# S# m#, e# #)
#elif WORD_SIZE_IN_BITS == 32
decodeDoubleInteger x = case decodeDouble_Int64# x of
                          (# m#, e# #) -> (# int64ToInteger m#, e# #)
#endif
{-# NOINLINE decodeDoubleInteger #-}

-- | Same as encodeDoubleInteger, but for Float#
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger i e = double2Float# (encodeDoubleInteger i e)
{-# NOINLINE encodeFloatInteger #-}

decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger f = case decodeFloat_Int# f of (# mant, exp #) -> (# smallInteger mant, exp #)
{-# NOINLINE decodeFloatInteger #-}

hashInteger :: Integer -> Int#
hashInteger = integerToInt
{-# NOINLINE hashInteger #-}

-- ** Arithmetic operations

plusInteger :: Integer -> Integer -> Integer
plusInteger (S# (INT_MINBOUND#)) (S# (INT_MINBOUND#)) = Bn# (wordToBigNum2 (int2Word# 1#) (int2Word# 0#))
plusInteger (S# x#) (S# y#) =
  case addIntC# x# y# of
    (# z#, 0# #) -> S# z#
    (# z#, _ #)
      | isTrue# (z# >=# 0#) -> Bn# (wordToBigNum (int2Word# (negateInt# z#)))
      | True -> Bp# (wordToBigNum (int2Word# z#))
plusInteger (S# x) (Bp# y)
  | isTrue# (x >=# 0#) = Bp# (plusBigNumWord y (int2Word# x))
  | True = bigNumToInteger (minusBigNumWord y (int2Word# (negateInt# x)))
plusInteger (S# x) (Bn# y)
  | isTrue# (x >=# 0#) = bigNumToNegInteger (minusBigNumWord y (int2Word# x))
  | True = Bn# (plusBigNumWord y (int2Word# (negateInt# x)))
plusInteger b (S# x) = plusInteger (S# x) b
plusInteger (Bp# x) (Bp# y) = Bp# (plusBigNum x y)
plusInteger (Bn# x) (Bn# y) = Bn# (plusBigNum x y)
-- TODO(SN): use compareBigNum for x == y short cut
plusInteger (Bp# x) (Bn# y) = case minusBigNum x y of
  (bn, False) -> bigNumToInteger bn
  (bn, True) -> bigNumToNegInteger bn
plusInteger (Bn# x) (Bp# y) = plusInteger (Bp# y) (Bn# x)
{-# NOINLINE plusInteger #-}

-- TODO(SN): don't define minus in terms of plusInteger (NOINLINE)
minusInteger :: Integer -> Integer -> Integer
minusInteger (S# x) (S# y) =
  case subIntC# x y of
    (# z#, 0# #) -> S# z#
    (# z#, _ #)
      | isTrue# (z# >=# 0#) -> Bn# (wordToBigNum (int2Word# (negateInt# z#)))
      | True -> Bp# (wordToBigNum (int2Word# z#))
minusInteger (Bp# x) (S# y)
  | isTrue# (y >=# 0#) = bigNumToInteger (minusBigNumWord x (int2Word# y))
  | True = Bp# (plusBigNumWord x (int2Word# (negateInt# y)))
minusInteger (Bn# x) (S# y)
  | isTrue# (y >=# 0#) = Bn# (plusBigNumWord x (int2Word# y))
  | True = bigNumToNegInteger (minusBigNumWord x (int2Word# (negateInt# y)))
minusInteger (S# x) (Bp# y) = plusInteger (S# x) (Bn# y)
minusInteger (S# x) (Bn# y) = plusInteger (S# x) (Bp# y)
minusInteger (Bp# x) (Bp# y) = plusInteger (Bp# x) (Bn# y)
minusInteger (Bp# x) (Bn# y) = Bp# (plusBigNum x y)
minusInteger (Bn# x) (Bp# y) = Bn# (plusBigNum x y)
minusInteger (Bn# x) (Bn# y) = plusInteger (Bp# y) (Bn# x)
{-# NOINLINE minusInteger #-}

-- | Switch sign of Integer.
negateInteger :: Integer -> Integer
negateInteger (Bn# n) = Bp# n
negateInteger (S# INT_MINBOUND#) = Bp# (wordToBigNum ABS_INT_MINBOUND##)
negateInteger (S# i#) = S# (negateInt# i#)
negateInteger (Bp# bn)
  | isTrue# (eqBigNumWord# bn ABS_INT_MINBOUND##) = S# INT_MINBOUND#
  | True = Bn# bn
{-# NOINLINE negateInteger #-}

-- | Absolute value of integer.
absInteger :: Integer -> Integer
absInteger (S# INT_MINBOUND#) = Bp# (wordToBigNum ABS_INT_MINBOUND##)
absInteger (S# i)
  | isTrue# (i >=# 0#) = S# i
  | True = S# (negateInt# i)
absInteger (Bp# bn) = Bp# bn
absInteger (Bn# bn) = Bp# bn
{-# NOINLINE absInteger #-}

-- | Return @-1@, @0@, and @1@ depending on whether argument is negative, zero,
-- or positive, respectively.
signumInteger :: Integer -> Integer
signumInteger i = S# (signumInteger# i)
{-# NOINLINE signumInteger #-}

-- | Return @-1#@, @0#@, and @1#@ depending on whether argument is negative,
-- zero, or positive, respectively.
signumInteger# :: Integer -> Int#
signumInteger# (Bn# _) = -1#
signumInteger# (Bp# _) = 1#
signumInteger# (S# i#) = sgnI# i#
-- inlinable as only internally used

-- | Integer multiplication.
timesInteger :: Integer -> Integer -> Integer
timesInteger _ (S# 0#) = S# 0#
timesInteger (S# 0#) _ = S# 0#
timesInteger x (S# 1#) = x
timesInteger (S# 1#) y = y
timesInteger x (S# -1#) = negateInteger x
timesInteger (S# -1#) y = negateInteger y
timesInteger (S# x#) (S# y#) =
  case mulIntMayOflo# x# y# of
    0# -> S# (x# *# y#)
    _  -> timesInt2Integer x# y#
timesInteger x@(S# _) y = timesInteger y x
timesInteger (Bp# x) (Bp# y) = Bp# (timesBigNum x y)
timesInteger (Bp# x) (Bn# y) = Bn# (timesBigNum x y)
timesInteger (Bp# x) (S# y#)
  | isTrue# (y# >=# 0#) = Bp# (timesBigNumWord x (int2Word# y#))
  | True = Bn# (timesBigNumWord x (int2Word# (negateInt# y#)))
timesInteger (Bn# x) (Bn# y) = Bp# (timesBigNum x y)
timesInteger (Bn# x) (Bp# y) = Bn# (timesBigNum x y)
timesInteger (Bn# x) (S# y#)
  | isTrue# (y# >=# 0#) =  Bn# (timesBigNumWord x (int2Word# y#))
  | True = Bp# (timesBigNumWord x (int2Word# (negateInt# y#)))
{-# NOINLINE timesInteger #-}

-- | Construct 'Integer' from the product of two 'Int#'s
timesInt2Integer :: Int# -> Int# -> Integer
timesInt2Integer x# y# =
  case (# isTrue# (x# >=# 0#), isTrue# (y# >=# 0#) #) of
    (# False, False #) -> case timesWord2# (int2Word# (negateInt# x#)) (int2Word# (negateInt# y#)) of
      (# 0##, l #) -> inline wordToInteger l
      (# h, l #) -> Bp# (wordToBigNum2 h l)

    (# True, False #) -> case timesWord2# (int2Word# x#) (int2Word# (negateInt# y#)) of
      (# 0##, l #) -> wordToNegInteger l
      (# h, l #) -> Bn# (wordToBigNum2 h l)

    (# False, True #) -> case timesWord2# (int2Word# (negateInt# x#)) (int2Word# y#) of
      (# 0##, l #) -> wordToNegInteger l
      (# h, l #) -> Bn# (wordToBigNum2 h l)

    (# True, True #) -> case timesWord2# (int2Word# x#) (int2Word# y#) of
      (# 0##, l #) -> inline wordToInteger l
      (# h, l #) -> Bp# (wordToBigNum2 h l)
-- inlinable as only internally used

-- | Integer division rounded to zero, calculating 'quotInteger' and
-- 'remInteger'. Divisor must be non-zero or a division-by-zero will be raised.
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger n (S# 1#) = (# n, S# 0# #)
quotRemInteger n (S# -1#) =
  let !q = negateInteger n
  in  (# q, S# 0# #)
quotRemInteger _ (S# 0#) = -- will raise division by zero
  (# S# (quotInt# 0# 0#), S# (remInt# 0# 0#) #)
quotRemInteger (S# 0#) _ = (# S# 0#, S# 0# #)
quotRemInteger (S# n#) (S# d#) =
  let !(# q#, r# #) = quotRemInt# n# d#
  in  (# S# q#, S# r# #)
quotRemInteger (Bp# n) (Bp# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToInteger q, bigNumToInteger r #)
quotRemInteger (Bp# n) (Bn# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToNegInteger q, bigNumToInteger r #)
quotRemInteger (Bn# n) (Bn# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToInteger q, bigNumToNegInteger r #)
quotRemInteger (Bn# n) (Bp# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToNegInteger q, bigNumToNegInteger r #)
quotRemInteger (Bp# n) (S# d#)
  | isTrue# (d# >=# 0#) = let !(# q, r# #) = quotRemBigNumWord n (int2Word# d#)
                          in  (# bigNumToInteger q, inline wordToInteger r# #)
  | True = let !(# q, r# #) = quotRemBigNumWord n (int2Word# (negateInt# d#))
           in  (# bigNumToNegInteger q, inline wordToInteger r# #)
quotRemInteger (Bn# n) (S# d#)
  | isTrue# (d# >=# 0#) = let !(# q, r# #) = quotRemBigNumWord n (int2Word# d#)
                          in  (# bigNumToNegInteger q, inline wordToNegInteger r# #)
  | True = let !(# q, r# #) = quotRemBigNumWord n (int2Word# (negateInt# d#))
           in  (# bigNumToInteger q, inline wordToNegInteger r# #)
quotRemInteger i@(S# _) (Bn# _) = (# S# 0#, i #) -- since i < d
quotRemInteger i@(S# i#) (Bp# d) -- need to account for (S# minBound)
    | isTrue# (i# ># 0#) = (# S# 0#, i #)
    | isTrue# (gtBigNumWord# d (int2Word# (negateInt# i#))) = (# S# 0#, i #)
    | True {- abs(i) == d -} = (# S# -1#, S# 0# #)
{-# NOINLINE quotRemInteger #-}

quotInteger :: Integer -> Integer -> Integer
quotInteger n d = case quotRemInteger n d of (# q, _ #) -> q
{-# NOINLINE quotInteger #-}

remInteger :: Integer -> Integer -> Integer
remInteger n d = case quotRemInteger n d of (# _, r #) -> r
{-# NOINLINE remInteger #-}

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger n d
  | isTrue# (signumInteger# r ==# negateInt# (signumInteger# d)) =
    let !q' = q `minusInteger` (S# 1#)
        !r' = r `plusInteger` d
    in (# q', r' #)
  | True = qr
 where
  !qr@(# q, r #) = quotRemInteger n d
{-# NOINLINE divModInteger #-}

divInteger :: Integer -> Integer -> Integer
-- same-sign ops can be handled by more efficient 'quotInteger'
divInteger n d | isTrue# (signumInteger# n ==# signumInteger# d) = quotInteger n d
divInteger n d = case inline divModInteger n d of (# q, _ #) -> q
{-# NOINLINE divInteger #-}

modInteger :: Integer -> Integer -> Integer
-- same-sign ops can be handled by more efficient 'remInteger'
modInteger n d | isTrue# (signumInteger# n ==# signumInteger# d) = remInteger n d
modInteger n d = case inline divModInteger n d of (# _, r #) -> r
{-# NOINLINE modInteger #-}

-- TODO(SN): untested
-- | Greatest common divisor. Implemented using 'quotRemInteger'.
gcdInteger :: Integer -> Integer -> Integer
gcdInteger (S# 0#)        b = absInteger b
gcdInteger a        (S# 0#) = absInteger a
gcdInteger (S# 1#)        _ = S# 1#
gcdInteger (S# -1#)       _ = S# 1#
gcdInteger _        (S# 1#) = S# 1#
gcdInteger _       (S# -1#) = S# 1#
gcdInteger a b = case a `remInteger` b of
                   (S# 0#) -> b
                   r -> gcdInteger b r
{-# NOINLINE gcdInteger #-}

-- TODO(SN): untested
-- | Least common multiple. Implemented using 'quotInteger', 'gcdInteger' and
-- `timesInteger'.
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
{-# NOINLINE lcmInteger #-}

-- ** Bit operations

-- | Bitwise AND of Integers.
andInteger :: Integer -> Integer -> Integer
-- short-cuts
andInteger (S# 0#) !_ = S# 0#
andInteger _ (S# 0#) = S# 0#
andInteger (S# -1#) y = y
andInteger x (S# -1#) = x
-- base-cases
andInteger (S# x#) (S# y#) = S# (andI# x# y#)
andInteger (Bp# x) (Bp# y) = bigNumToInteger (andBigNum x y)
andInteger (Bn# x) (Bn# y) =
  bigNumToNegInteger (plusBigNumWord (orBigNum (minusBigNumWord x 1##)
                                               (minusBigNumWord y 1##)) 1##)
andInteger x@(Bn# _) y@(Bp# _) = andInteger y x
andInteger (Bp# x) (Bn# y) =
  bigNumToInteger (andnBigNum x (minusBigNumWord y 1##))
-- TODO/FIXME promotion-hack
andInteger x@(S# _) y = andInteger (unsafePromote x) y
andInteger x y@(S# _) = andInteger x (unsafePromote y)
{-# NOINLINE andInteger #-}

-- | Bitwise OR of Integers.
orInteger :: Integer -> Integer -> Integer
-- short-cuts
orInteger (S# 0#) y = y
orInteger x (S# 0#) = x
orInteger x@(S# -1#) _ = x
orInteger _ y@(S# -1#) = y
-- base-cases
orInteger (S# a#) (S# b#) = S# (a# `orI#` b#)
orInteger (Bp# x) (Bp# y) = Bp# (orBigNum x y)
orInteger (Bn# x) (Bn# y) =
  bigNumToNegInteger (plusBigNumWord (andBigNum (minusBigNumWord x 1##)
                                                (minusBigNumWord y 1##)) 1##)
orInteger x@(Bn# _) y@(Bp# _) = orInteger y x
orInteger (Bp# x) (Bn# y) =
  bigNumToNegInteger (plusBigNumWord (andnBigNum (minusBigNumWord y 1##) x) 1##)
-- TODO/FIXME promotion-hack
orInteger x@(S# _) y = orInteger (unsafePromote x) y
orInteger x y@(S# _) = orInteger x (unsafePromote y)
{-# NOINLINE orInteger #-}

-- | Bitwise XOR operation
xorInteger :: Integer -> Integer -> Integer
-- short-cuts
xorInteger (S# 0#) y = y
xorInteger x (S# 0#) = x
-- TODO: (S# -1) cases
-- base-cases
xorInteger (S# x#) (S# y#) = S# (xorI# x# y#)
xorInteger (Bp# x) (Bp# y) = bigNumToInteger (xorBigNum x y)
xorInteger (Bn# x) (Bn# y) = bigNumToInteger (xorBigNum (minusBigNumWord x 1##)
                                                        (minusBigNumWord y 1##))
xorInteger x@(Bn# _) y@(Bp# _) = xorInteger y x
xorInteger (Bp# x) (Bn# y) =
  bigNumToNegInteger (plusBigNumWord (xorBigNum x (minusBigNumWord y 1##)) 1##)
-- TODO/FIXME promotion-hack
xorInteger x@(S# _) y = xorInteger (unsafePromote x) y
xorInteger x y@(S# _) = xorInteger x (unsafePromote y)
{-# NOINLINE xorInteger #-}

-- | Bitwise NOT of Integers.
complementInteger :: Integer -> Integer
complementInteger (S# i#) = S# (notI# i#)
complementInteger (Bp# bn) = Bn# (plusBigNumWord  bn 1##)
complementInteger (Bn# bn) = Bp# (minusBigNumWord bn 1##)
{-# NOINLINE complementInteger #-}

-- HACK warning! breaks invariant on purpose
unsafePromote :: Integer -> Integer
unsafePromote (S# x#)
    | isTrue# (x# >=# 0#) = Bp# (wordToBigNum (int2Word# x#))
    | True                = Bn# (wordToBigNum (int2Word# (negateInt# x#)))
unsafePromote x = x

-- | Shift-left operation. Undefined for negative shift amount ('Int#').
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger x 0# = x
shiftLInteger (S# 0#) _  = S# 0#
shiftLInteger (S# 1#) n# = bitInteger n#
shiftLInteger (S# i#) n#
  | isTrue# (i# >=# 0#) = bigNumToInteger (shiftLBigNum (wordToBigNum (int2Word# i#)) n#)
  | True = bigNumToNegInteger (shiftLBigNum (wordToBigNum (int2Word# (negateInt# i#))) n#)
shiftLInteger (Bp# bn) n# = Bp# (shiftLBigNum bn n#)
shiftLInteger (Bn# bn) n# = Bn# (shiftLBigNum bn n#)
{-# NOINLINE shiftLInteger #-}

-- | Arighmetic shift-right, i.e. the MSB is replicated.
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger x 0# = x
shiftRInteger (S# 0#) _  = S# 0#
shiftRInteger (S# i#) n#
  -- const -1 if shifted 'too far'
  | isTrue# (n# >=# WORD_SIZE_IN_BITS#) = S# ((i# <# 0#) *# (-1#))
  | True = S# (uncheckedIShiftRA# i# n#)
shiftRInteger (Bp# bn) n# = bigNumToInteger (shiftRBigNum bn n#)
-- TODO(SN): naive right shift preserving two's complement
shiftRInteger i@(Bn# _) n# = complementInteger (shiftRInteger (complementInteger i) n#)
{-# NOINLINE shiftRInteger #-}

popCountInteger :: Integer -> Int#
popCountInteger (S# i#)
  | isTrue# (i# >=# 0#) = popCntI# i#
  | True                = negateInt# (popCntI# (negateInt# i#))
popCountInteger (Bp# bn) = popCountBigNum bn
popCountInteger (Bn# bn) = negateInt# (popCountBigNum bn)
{-# NOINLINE popCountInteger #-}

-- | 'Integer' for which only 'n'-th bit is set. Undefined behaviour for
-- negative 'n' values.
bitInteger :: Int# -> Integer
bitInteger i#
  | isTrue# (i# <# (WORD_SIZE_IN_BITS# -# 1#)) = S# (uncheckedIShiftL# 1# i#)
  | True = Bp# (bitBigNum i#)
{-# NOINLINE bitInteger #-}

-- | Test if bit n is set in Integer. Note: Negative BigNums are treated as the
-- two's-complement, although they are not stored like that
testBitInteger :: Integer -> Int# -> Bool
testBitInteger (S# i#) n#
  | isTrue# (n# <# 0#) = False
  | isTrue# (n# >=# WORD_SIZE_IN_BITS#) = isTrue# (i# <# 0#)
  | True = isTrue# (i# `andI#` (uncheckedIShiftL# 1# n#) /=# 0#)
testBitInteger (Bp# bn) n# = testBitBigNum bn n#
testBitInteger (Bn# bn) n# = testBitNegBigNum bn n#

-- ** Comparison

-- | Compare given integers. Uses the S#/Bp#/Bn# constructors to efficiently
-- test corner cases.
compareInteger :: Integer -> Integer -> Ordering
compareInteger (Bp# a#) (Bp# b#) = compareBigNum a# b#
compareInteger (Bn# a#) (Bn# b#) = compareBigNum b# a#
compareInteger (S# a#) (S# b#) = compareInt# a# b#
compareInteger (S# _) (Bp# _) = LT
compareInteger (S# _) (Bn# _) = GT
compareInteger (Bp# _) _ = GT
compareInteger (Bn# _) _ = LT
{-# NOINLINE compareInteger #-}

eqInteger, neqInteger, leInteger, ltInteger, gtInteger, geInteger :: Integer -> Integer -> Bool
eqInteger x y = isTrue# (eqInteger# x y)
neqInteger x y = isTrue# (neqInteger# x y)
leInteger x y = isTrue# (leInteger# x y)
ltInteger x y = isTrue# (ltInteger# x y)
gtInteger x y = isTrue# (gtInteger# x y)
geInteger x y = isTrue# (geInteger# x y)

eqInteger# :: Integer -> Integer -> Int#
eqInteger# (S# i1) (S# i2) = i1 ==# i2
eqInteger# (Bp# bn1) (Bp# bn2) = eqBigNum# bn1 bn2
eqInteger# (Bn# bn1) (Bn# bn2) = eqBigNum# bn1 bn2
eqInteger# _ _ = 0#
{-# NOINLINE eqInteger# #-}

neqInteger# :: Integer -> Integer -> Int#
neqInteger# i1 i2 =
  case eqInteger# i1 i2 of
    0# -> 1#
    1# -> 0#
    _  -> 0#
{-# NOINLINE neqInteger# #-}

geInteger# :: Integer -> Integer -> Int#
geInteger# a b = case compareInteger a b of
  LT -> 0#
  _ -> 1#
{-# NOINLINE geInteger# #-}

gtInteger# :: Integer -> Integer -> Int#
gtInteger# a b = case compareInteger a b of
  GT -> 1#
  _ -> 0#
{-# NOINLINE gtInteger# #-}

leInteger# :: Integer -> Integer -> Int#
leInteger# a b = case compareInteger a b of
  GT -> 0#
  _ -> 1#
{-# NOINLINE leInteger# #-}

ltInteger# :: Integer -> Integer -> Int#
ltInteger# a b = case compareInteger a b of
  LT -> 1#
  _ -> 0#
{-# NOINLINE ltInteger# #-}

-- * BigNum functions

-- | OpenSSL BIGNUM represented by an absolute magnitude as 'Word#' in a
-- 'ByteArray#'. It corresponds to the 'd' array in libcrypto's bignum_st
-- structure. Length is always a multiple of Word#, least-significant first
-- (BN_BITS2 == 'WORD_SIZE_IN_BITS').
data BigNum = BN# ByteArray#

-- | Mutable variant of BigNum for internal use.
data MutableBigNum s = MBN# (MutableByteArray# s)

-- | Create a MutableBigNum with given count of words.
newBigNum :: Int# -> S s (MutableBigNum s)
newBigNum count# s =
  -- Calculate byte size using shifts, e.g. for 64bit systems:
  -- total bytes = word count * 8 = word count * 2 ^ 3 = word count << 3
  case newByteArray# (count# `uncheckedIShiftL#` WORD_SHIFT#) s of
    (# s', mba# #) -> (# s', MBN# mba# #)

-- | Freeze a MutableBigNum into a BigNum.
freezeBigNum :: MutableBigNum s -> S s BigNum
freezeBigNum (MBN# mba#) s =
  case unsafeFreezeByteArray# mba# s of
    (# s', ba# #) -> (# s', BN# ba# #)

-- | Shrink a MutableBigNum to the given count of Word#.
shrinkBigNum :: MutableBigNum s -> Int# -> S s (MutableBigNum s)
shrinkBigNum mba 0# s =
  -- BigNum always holds min one word, but clear it in this case
  case shrinkBigNum mba 1# s of
    (# s1, mba'@(MBN# mba#) #) -> case writeWordArray# mba# 0# 0## s1 of
      s2 -> (# s2, mba' #)
shrinkBigNum mba@(MBN# mba#) n# s
  | isTrue# (actual# ==# desired#) = (# s', mba #)
  | True = case shrinkMutableByteArray# mba# desired# s' of s'' -> (# s'', mba #)
 where
  !(# s', actual# #) = getSizeofMutableByteArray# mba# s
  desired# = n# `uncheckedIShiftL#` WORD_SHIFT#

-- | Find most significant word, shrink underlyng 'MutableByteArray#'
-- accordingly to satisfy BigNum invariant.
renormBigNum :: MutableBigNum s -> S s (MutableBigNum s)
renormBigNum mba@(MBN# mba#) s0
  -- TODO(SN): words# ==# 0# case?
  | isTrue# (msw# ==# 0#) = (# s2, mba #)
  | True = shrinkBigNum mba (msw# +# 1#) s2
 where
  !(# s1, words# #) = wordsInMutableBigNum# mba s0
  !(# s2, msw# #) = findMSW# (words# -# 1#) s1

  -- Finds index of the 'most-significant-word' (non 0 word)
  findMSW# 0# s = (# s, 0# #)
  findMSW# i# s = case readWordArray# mba# i# s of
    (# s', 0## #) -> findMSW# (i# -# 1#) s'
    (# s', _ #) -> (# s', i# #)

-- | Write a word to a MutableBigNum at given word index. Size is not checked!
writeBigNum :: Int# -> Word# -> MutableBigNum s -> S s (MutableBigNum s)
writeBigNum i# w# mba@(MBN# mba#) s =
  let s' = writeWordArray# mba# i# w# s
  in  (# s', mba #)

-- | Copy magnitude from given BigNum into MutableBigNum.
copyBigNum :: BigNum -> MutableBigNum s -> S s ()
copyBigNum a@(BN# ba#) (MBN# mbb#) =
  copyWordArray# ba# 0# mbb# 0# (wordsInBigNum# a)

oneBigNum :: BigNum
oneBigNum = runS (newBigNum 1# >>= writeBigNum 0# 1## >>= freezeBigNum)

zeroBigNum :: BigNum
zeroBigNum = runS (newBigNum 1# >>= writeBigNum 0# 0## >>= freezeBigNum)

-- | Create a BigNum from a single Word#.
wordToBigNum :: Word# -> BigNum
wordToBigNum w# = runS $
  newBigNum 1# >>= writeBigNum 0# w# >>= freezeBigNum

-- | Create a BigNum from two Word#.
wordToBigNum2 :: Word# -- ^ High word
              -> Word# -- ^ low word
              -> BigNum
wordToBigNum2 h# l# = runS $
  newBigNum 2# >>= writeBigNum 0# l# >>= writeBigNum 1# h# >>= freezeBigNum

-- | Get number of 'Word#' in 'BigNum'. See 'newBigNum' for shift explanation.
wordsInBigNum# :: BigNum -> Int#
wordsInBigNum# (BN# ba#) = (sizeofByteArray# ba#) `uncheckedIShiftRL#` WORD_SHIFT#

-- | Get number of 'Word#' in 'MutableBigNum'. See 'newBigNum' for shift explanation.
wordsInMutableBigNum# :: MutableBigNum s -> State# s -> (# State# s, Int# #)
wordsInMutableBigNum# (MBN# mbn#) s =
  case getSizeofMutableByteArray# mbn# s of
    (# s1, i# #) -> (# s1, i# `uncheckedIShiftRL#` WORD_SHIFT# #)

-- | Truncate a BigNum to a single Word#.
bigNumToWord :: BigNum -> Word#
bigNumToWord (BN# ba) = indexWordArray# ba 0#

indexBigNum# :: BigNum -> Int# -> Word#
indexBigNum# (BN# ba) idx = indexWordArray# ba idx

-- | Truncate a BigNum to a single Int#
bigNumToInt :: BigNum -> Int#
bigNumToInt (BN# ba) = indexIntArray# ba 0#

-- | Create a positive Integer from given BigNum. Converts to small Integer if possible.
bigNumToInteger :: BigNum -> Integer
bigNumToInteger bn
  | isTrue# ((wordsInBigNum# bn ==# 1#) `andI#` (i# >=# 0#)) = S# i#
  | True = Bp# bn
  where
    i# = word2Int# (bigNumToWord bn)

-- | Create a negative Integer from given BigNum. Converts to small Integer if possible.
bigNumToNegInteger :: BigNum -> Integer
bigNumToNegInteger bn
  | isTrue# ((wordsInBigNum# bn ==# 1#) `andI#` (i# <=# 0#)) = S# i#
  | True = Bn# bn
  where
    i# = negateInt# (word2Int# (bigNumToWord bn))

-- | Helper function to convert a BigNum into a Double#.
bigNumToDouble :: BigNum -> Double#
bigNumToDouble bn = go bn 0#
  where
    go bn' !idx =
      let n = wordsInBigNum# bn
          newIdx = idx +# 1# in
      case isTrue# (idx ==# n) of
        True ->  0.0##
        _ -> case indexBigNum# bn' idx of
                x -> case splitHalves x of
                  (# h, l #) -> (go bn' newIdx)
                      *## (2.0## **## WORD_SIZE_IN_BITS_FLOAT## )
                      +## int2Double# (word2Int# h) *## (2.0## **## int2Double# HIGH_HALF_SHIFT#)
                      +## int2Double# (word2Int# l)

-- | Splits the given Word# into a high- and a low-word.
splitHalves :: Word# -> (# {- High -} Word#, {- Low -} Word# #)
splitHalves (!x) = (# x `uncheckedShiftRL#` HIGH_HALF_SHIFT#,
                      x `and#` LOW_HALF_MASK## #)

-- ** Comparisons

compareBigNum :: BigNum -> BigNum -> Ordering
compareBigNum a@(BN# baa#) (BN# bab#)
  | isTrue# (na# ># nb#) = GT
  | isTrue# (na# <# nb#) = LT
  -- na# == nb#
  | True = go (n# -# 1#)
 where
  na# = sizeofByteArray# baa#
  nb# = sizeofByteArray# bab#
  n# = wordsInBigNum# a
  go i# =
    case i# <# 0# of
      1# -> EQ
      _  -> case (indexWordArray# baa# i#) `ltWord#` (indexWordArray# bab# i#) of
              1# ->  LT
              _  -> case (indexWordArray# baa# i#) `gtWord#` (indexWordArray# bab# i#) of
                      1# -> GT
                      _  -> go (i# -# 1#)

-- | Return '1#' iff BigNum holds one 'Word#' equal to given 'Word#'.
eqBigNumWord# :: BigNum -> Word# -> Int#
eqBigNumWord# bn w# =
  (wordsInBigNum# bn ==# 1#) `andI#` (bigNumToWord bn `eqWord#` w#)

-- | Return '1#' iff the two 'BigNum' are equal.
eqBigNum# :: BigNum -> BigNum -> Int#
eqBigNum# bn1 bn2 =
  let wib1 = wordsInBigNum# bn1
      wib2 = wordsInBigNum# bn2 in
  case isTrue# (wib1 ==# wib2) of
    True  -> go (wib1 -# 1#) (wib2 -# 1#)
    False -> 0#
  where
    go 0# 0# = indexBigNum# bn1 0# `eqWord#` indexBigNum# bn2 0#
    go 0# _ = 0#
    go _ 0# = 0#
    go !idx1 !idx2 =
      let w1 = indexBigNum# bn1 idx1
          w2 = indexBigNum# bn2 idx2 in
      case isTrue# (w1 `eqWord#` w2) of
        True  -> go (idx1 -# 1#) (idx2 -# 1#)
        False ->  0#

-- | Return @1#@ iff BigNum holds one 'Word#' equal to 0##
isZeroBigNum# :: BigNum -> Int#
isZeroBigNum# bn =
  (wordsInBigNum# bn ==# 1#) `andI#` (bigNumToWord bn `eqWord#` 0##)

-- | Return @1#@ iff BigNum is greater than a given 'Word#'.
gtBigNumWord# :: BigNum -> Word# -> Int#
gtBigNumWord# bn w# =
  (wordsInBigNum# bn ># 1#) `orI#` (bigNumToWord bn `gtWord#` w#)

-- ** Bit-operations

-- | Bitwise NOT of two BigNum.
notBigNum :: BigNum -> BigNum
notBigNum x@(BN# x#) = notBigNum' x# nx#
 where
  nx# = wordsInBigNum# x

  -- assumes n# >= m#
  notBigNum' a# n# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    _ <- mapWordArray# a# a# mba# (\a _ -> not# a) n#
    renormBigNum mbn >>= freezeBigNum

-- | Bitwise OR of two BigNum.
orBigNum :: BigNum -> BigNum -> BigNum
orBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = y
  | isTrue# (eqBigNumWord# y 0##) = x
  | isTrue# (nx# >=# ny#) = orBigNum' x# y# nx# ny#
  | True = orBigNum' y# x# ny# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m#
  orBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    _ <- mapWordArray# a# b# mba# or# m#
    _ <- case isTrue# (n# ==# m#) of
           False -> copyWordArray# a# m# mba# m# (n# -# m#)
           True  -> return ()
    freezeBigNum mbn

-- | Bitwise AND of two BigNum.
andBigNum :: BigNum -> BigNum -> BigNum
andBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
  | isTrue# (eqBigNumWord# y 0##) = zeroBigNum
  | isTrue# (nx# >=# ny#) = andBigNum' x# y# ny#
  | True = andBigNum' y# x# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m#
  andBigNum' a# b# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum m#
    _ <- mapWordArray# a# b# mba# and# m#
    freezeBigNum mbn

-- | Bitwise XOR of two BigNum.
xorBigNum :: BigNum -> BigNum -> BigNum
xorBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = y
  | isTrue# (eqBigNumWord# y 0##) = x
  | isTrue# (nx# >=# ny#) = xorBigNum' x# y# nx# ny#
  | True = xorBigNum' y# x# ny# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m#
  xorBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    _ <- mapWordArray# a# b# mba# xor# m#
    _ <- case isTrue# (n# ==# m#) of
           False -> copyWordArray# a# m# mba# m# (n# -# m#)
           True  -> return ()
    freezeBigNum mbn

-- | Bitwise ANDN of two BigNum - basically x `andBigNum` (notBigNum y). However
-- implemented differently to take care of different number of words.
andnBigNum :: BigNum -> BigNum -> BigNum
andnBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
  | isTrue# (eqBigNumWord# y 0##) = x
  | isTrue# (nx# >=# ny#) = andnBigNum' x# y# nx# ny#
  | True = andnBigNum'' x# y# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m#
  andnBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    _ <- mapWordArray# a# b# mba# (\a b -> a `and#` (not# b)) m#
    _ <- case isTrue# (n# ==# m#) of
           False -> copyWordArray# a# m# mba# m# (n# -# m#)
           True  -> return ()
    freezeBigNum mbn

  -- assumes n# < m#
  andnBigNum'' a# b# n# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    _ <- mapWordArray# a# b# mba# (\a b -> a `and#` (not# b)) n#
    freezeBigNum mbn

-- | Shift left logical, undefined for negative Int#.
shiftLBigNum :: BigNum -> Int# -> BigNum
shiftLBigNum x 0# = x
shiftLBigNum x _
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
shiftLBigNum a@(BN# ba#) n# = runS $ do
  r@(MBN# mbr#) <- newBigNum nq#
  (I# i#) <- liftIO (bn_lshift mbr# nq# ba# na# n#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nq# = na# +# nwords# +# 1#
  nwords# = quotInt# n# WORD_SIZE_IN_BITS#

-- size_t integer_bn_lshift(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, size_t n)
foreign import ccall unsafe "integer_bn_lshift"
  bn_lshift :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> IO Int

-- | Shift right logical, undefined for negative Int#.
shiftRBigNum :: BigNum -> Int# -> BigNum
shiftRBigNum x 0# = x
shiftRBigNum x _
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
shiftRBigNum a@(BN# ba#) n# = runS $ do
  r@(MBN# mbr#) <- newBigNum na#
  (I# i#) <- liftIO (bn_rshift mbr# na# ba# na# n#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a

-- size_t integer_bn_rshift(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, size_t n)
foreign import ccall unsafe "integer_bn_rshift"
  bn_rshift :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> IO Int

popCountBigNum :: BigNum -> Int#
popCountBigNum a@(BN# ba#) = word2Int# (go 0## (na# -# 1#))
 where
  go !acc# i#
    | isTrue# (i# <# 0#) = acc#
    | True = go (acc# `plusWord#` (popCnt# (indexWordArray# ba# i#))) (i# -# 1#)

  na# = wordsInBigNum# a

bitBigNum :: Int# -> BigNum
bitBigNum i#
  | isTrue# (i#  <#  0#) = zeroBigNum
  | isTrue# (i# ==#  0#) = oneBigNum
  | True = runS $ do
      mbn@(MBN# mba#) <- newBigNum (li# +# 1#)
      -- clear all limbs (except for the most-significant limb)
      _ <- clearWordArray# mba# 0# (li# +# 1#)
      -- set single bit in most-significant limb
      _ <- writeBigNum li# (uncheckedShiftL# 1## bi#) mbn
      freezeBigNum mbn
  where
    !(# li#, bi# #) = quotRemInt# i# WORD_SIZE_IN_BITS#

testBitBigNum :: BigNum -> Int# -> Bool
testBitBigNum bn i#
  | isTrue# (i# <# 0#) = False
  | isTrue# (li# <# nx#) = isTrue# (testBitWord# (indexBigNum# bn li#) bi#)
  | True = False
  where
    !(# li#, bi# #) = quotRemInt# i# WORD_SIZE_IN_BITS#
    nx# = wordsInBigNum# bn

testBitNegBigNum :: BigNum -> Int# -> Bool
testBitNegBigNum bn i#
  | isTrue# (i# <# 0#) = False
  | isTrue# (li# >=# nx#) = True
  | allZ li# = isTrue# ((testBitWord#
                         (indexBigNum# bn li# `minusWord#` 1##) bi#) ==# 0#)
  | True = isTrue# ((testBitWord# (indexBigNum# bn li#) bi#) ==# 0#)
  where
    !(# li#, bi# #) = quotRemInt# i# WORD_SIZE_IN_BITS#
    nx# = wordsInBigNum# bn

    allZ 0# = True
    allZ j | isTrue# (indexBigNum# bn (j -# 1#) `eqWord#` 0##) = allZ (j -# 1#)
           | True = False

-- ** Arithmetic operations

plusBigNum :: BigNum -> BigNum -> BigNum
plusBigNum a@(BN# a#) b@(BN# b#) = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  (I# i#) <- liftIO (bn_add mbr# nr# a# na# b# nb#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nb# = wordsInBigNum# b
  nr# = (maxInt# na# nb#) +# 1#

foreign import ccall unsafe "integer_bn_add"
  bn_add :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> IO Int

-- | Add given Word# to BigNum.
plusBigNumWord :: BigNum -> Word# -> BigNum
plusBigNumWord a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  _ <- copyBigNum a r
  (I# i#) <- liftIO (bn_add_word mbr# nr# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
   na# = wordsInBigNum# a
   nr# = na# +# 1#

-- size_t integer_bn_add_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_add_word"
  bn_add_word :: MutableByteArray# s -> Int# -> Word# -> IO Int

minusBigNum :: BigNum -> BigNum -> (BigNum, Bool)
minusBigNum a@(BN# a#) b@(BN# b#) = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  ba@(BA# negValue#) <- newByteArray 4#
  (I# i#) <- liftIO (bn_sub mbr# nr# a# na# b# nb# negValue#)
  (I# neg#) <- readInt32ByteArray ba
  bn <- shrinkBigNum r i# >>= freezeBigNum
  return (bn, isTrue# neg#)
 where
  na# = wordsInBigNum# a
  nb# = wordsInBigNum# b
  nr# = maxInt# na# nb#

foreign import ccall unsafe "integer_bn_sub"
  bn_sub :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> ByteArray# -> IO Int

-- | Subtract given Word# from BigNum.
minusBigNumWord :: BigNum -> Word# -> BigNum
minusBigNumWord a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum na#
  _ <- copyBigNum a r
  (I# i#) <- liftIO (bn_sub_word mbr# na# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
   na# = wordsInBigNum# a

-- size_t integer_bn_sub_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_sub_word"
  bn_sub_word :: MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Multiply given BigNum with given Word#.
timesBigNumWord :: BigNum -> Word# -> BigNum
timesBigNumWord a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  _ <- copyBigNum a r
  (I# i#) <- liftIO (bn_mul_word mbr# nr# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nr# = na# +# 1#

-- int integer_bn_mul_word(BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_mul_word"
  bn_mul_word :: MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Multiply two BigNums.
timesBigNum :: BigNum -> BigNum -> BigNum
timesBigNum a@(BN# a#) b@(BN# b#) = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  (I# i#) <- liftIO (bn_mul mbr# nr# a# na# b# nb#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nb# = wordsInBigNum# b

  -- OpenSSL's BN_mul requires result BigNum to be big enough (bn_mul.c:553ff)
  nr# = case isTrue# (na# >=# nb#) of
    True  -> calculateSize na#
    False -> calculateSize nb#

  calculateSize n# =
    let j = lowerPowerTwo# n#
        k = j +# j
    in case isTrue# (k ># n#) of
      True -> k *# 4#
      False -> k *# 2#

  lowerPowerTwo# i# = 1# `uncheckedIShiftL#` (numBitsWord# (int2Word# i#) -# 1#)

  numBitsWord# w# = WORD_SIZE_IN_BITS# -# (word2Int# (clz# w#))

-- int integer_bn_mul(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, BN_ULONG *bb, size_t bsize)
foreign import ccall unsafe "integer_bn_mul"
  bn_mul :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> IO Int

-- | Divide a BigNum by a Word#, returning the quotient and a remainder (rounded
-- to zero). The divisor must not be 0##.
quotRemBigNumWord :: BigNum -> Word# -> (# BigNum, Word# #)
quotRemBigNumWord a 0## = (# a, remWord# 0## 0## #) -- raises division by zero
quotRemBigNumWord a 1## = (# a, 0## #)
-- TODO(SN): use compareBigNumWord for a/1 and a < w short cuts
quotRemBigNumWord a w# = case runS divWord of (q, (I# r#)) -> (# q, int2Word# r# #)
 where
  na# = wordsInBigNum# a
  nq# = na# +# 1#
  divWord = do
    q@(MBN# mbq#) <- newBigNum nq#
    _ <- copyBigNum a q
    qtopba@(BA# qtopba#) <- newByteArray 4#
    r <- liftIO (bn_div_word mbq# nq# w# qtopba#)
    (I# qtop#) <- readInt32ByteArray qtopba
    q' <- shrinkBigNum q qtop# >>= freezeBigNum
    return (q', r)

-- int integer_bn_div_word(BN_ULONG *qb, size_t qsize, BN_ULONG w, int32_t *qtop)
foreign import ccall unsafe "integer_bn_div_word"
  bn_div_word :: MutableByteArray# s -> Int# -> Word# -> ByteArray# -> IO Int

-- | Divide a BigNum by another BigNum, returning the quotient and a remainder
-- (rounded to zero). The divisor must not be 0##.
quotRemBigNum :: BigNum -> BigNum -> (# BigNum, BigNum #)
quotRemBigNum a@(BN# ba#) d@(BN# bd#) = case runS div of (q, r) -> (# q, r #)
 where
  na# = wordsInBigNum# a
  nd# = wordsInBigNum# d
  nq# = na# +# 1#
  nr# = na#
  div = do
    q@(MBN# mbq#) <- newBigNum nq#
    r@(MBN# mbqtop#) <- newBigNum nr#
    qtopba@(BA# qtopba#) <- newByteArray 4#
    rtopba@(BA# rtopba#) <- newByteArray 4#
    _ <- liftIO (bn_div mbq# nq# mbqtop# nr# ba# na# bd# nd# qtopba# rtopba#)
    (I# qtop#) <- readInt32ByteArray qtopba
    (I# rtop#) <- readInt32ByteArray rtopba
    q' <- shrinkBigNum q qtop# >>= freezeBigNum
    r' <- shrinkBigNum r rtop# >>= freezeBigNum
    return (q', r')

-- Integer division with remaineder and rounding towards zero
-- int integer_bn_div(BN_ULONG *qb, size_t qsize,
--                    BN_ULONG *remb, size_t remsize,
--                    BN_ULONG *ab, size_t asize,
--                    BN_ULONG *db, size_t dsize,
--                    int32_t* qtop, int32_t* remtop)
foreign import ccall unsafe "integer_bn_div"
  bn_div :: MutableByteArray# s -> Int#
         -> MutableByteArray# s -> Int#
         -> ByteArray# -> Int#
         -> ByteArray# -> Int#
         -> ByteArray# -> ByteArray#
         -> IO Int

-- * Borrowed things - mostly from integer-gmp

-- ** ByteArray# utilities

data ByteArray = BA# ByteArray#

-- | Helper to allocate a freezed block of memory and return a lifted type for
-- easier handling.
newByteArray :: Int# -> S s ByteArray
newByteArray i# s =
  let !(# s1, mba# #) = newByteArray# i# s
      !(# s2, ba# #) = unsafeFreezeByteArray# mba# s1
  in  (# s2, BA# ba# #)

readInt32ByteArray :: ByteArray -> S s Int
readInt32ByteArray (BA# ba#) s = (# s, I# (indexInt32Array# ba# 0#) #)

clearWordArray# :: MutableByteArray# s -> Int# -> Int# -> S s ()
clearWordArray# mba ofs len s =
  let offsetBytes = ofs `uncheckedIShiftL#` WORD_SHIFT#
      lenBytes = len `uncheckedIShiftL#` WORD_SHIFT#
      s' = setByteArray# mba offsetBytes lenBytes 0# s
  in  (# s', () #)

-- | Copy multiples of Word# between ByteArray#s with offsets in words.
copyWordArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> S s ()
copyWordArray# src srcOffset dst dstOffset len s =
  let s' = copyByteArray# src srcOffsetBytes dst dstOffsetBytes lenBytes s
  in  (# s', () #)
 where
  srcOffsetBytes = srcOffset `uncheckedIShiftL#` WORD_SHIFT#
  dstOffsetBytes = dstOffset `uncheckedIShiftL#` WORD_SHIFT#
  lenBytes = len `uncheckedIShiftL#` WORD_SHIFT#

-- | Map over two ByteArray# for given number of words and store result in
-- MutableByteArray#.
mapWordArray# :: ByteArray# -> ByteArray# -> MutableByteArray# s
              -> (Word# -> Word# -> Word#)
              -> Int# -- ^ Number of words
              -> S s ()
mapWordArray# _ _ _ _ 0# s = (# s, () #)
mapWordArray# a# b# mba# f i# s =
  let !i'# = i# -# 1#
      w# = f (indexWordArray# a# i'#) (indexWordArray# b# i'#)
  in  case writeWordArray# mba# i'# w# s of
        s' -> mapWordArray# a# b# mba# f i'# s'

-- ** "ghc-prim" style helpers

-- branchless version
sgnI# :: Int# -> Int#
sgnI# x# = (x# ># 0#) -# (x# <# 0#)

maxInt# :: Int# -> Int# -> Int#
maxInt# x# y#
  | isTrue# (x# >=# y#) = x#
  | True = y#

bitWord# :: Int# -> Word#
bitWord# = uncheckedShiftL# 1##
{-# INLINE bitWord# #-}

testBitWord# :: Word# -> Int# -> Int#
testBitWord# w# i# = (bitWord# i# `and#` w#) `neWord#` 0##
{-# INLINE testBitWord# #-}

popCntI# :: Int# -> Int#
popCntI# i# = word2Int# (popCnt# (int2Word# i#))
{-# INLINE popCntI# #-}

-- ** From integer-gmp: monadic combinators for low-level state threading

type S s a = State# s -> (# State# s, a #)

infixl 1 >>=
infixl 1 >>
infixr 0 $

{-# INLINE ($) #-}
($) :: (a -> b) -> a -> b
f $ x = f x

{-# INLINE (>>=) #-}
(>>=) :: S s a -> (a -> S s b) -> S s b
(>>=) m k = \s -> case m s of (# s', a #) -> k a s'

{-# INLINE (>>) #-}
(>>) :: S s a -> S s b -> S s b
(>>) m k = \s -> case m s of (# s', _ #) -> k s'

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

-- ** From Base

{-# INLINE (.) #-}
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- ** From GHC.Err:

undefined :: forall a. a
undefined = runS $ fail "Prelude.undefined"
