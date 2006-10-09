{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bits
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines bitwise operations for signed and unsigned
-- integers.  Instances of the class 'Bits' for the 'Int' and
-- 'Integer' types are available from this module, and instances for
-- explicitly sized integral types are available from the
-- "Data.Int" and "Data.Word" modules.
--
-----------------------------------------------------------------------------

module Data.Bits ( 
  Bits(
    (.&.), (.|.), xor, -- :: a -> a -> a
    complement,        -- :: a -> a
    shift,             -- :: a -> Int -> a
    rotate,            -- :: a -> Int -> a
    bit,               -- :: Int -> a
    setBit,            -- :: a -> Int -> a
    clearBit,          -- :: a -> Int -> a
    complementBit,     -- :: a -> Int -> a
    testBit,           -- :: a -> Int -> Bool
    bitSize,           -- :: a -> Int
    isSigned,          -- :: a -> Bool
    shiftL, shiftR,    -- :: a -> Int -> a
    rotateL, rotateR   -- :: a -> Int -> a
  )

  -- instance Bits Int
  -- instance Bits Integer
 ) where

-- Defines the @Bits@ class containing bit-based operations.
-- See library document for details on the semantics of the
-- individual operations.

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
#include "MachDeps.h"
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Num
import GHC.Real
import GHC.Base
#endif

#ifdef __HUGS__
import Hugs.Bits
#endif

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

{-| 
The 'Bits' class defines bitwise operations over integral types.

* Bits are numbered from 0 with bit 0 being the least
  significant bit.

Minimal complete definition: '.&.', '.|.', 'xor', 'complement',
('shift' or ('shiftL' and 'shiftR')), ('rotate' or ('rotateL' and 'rotateR')),
'bitSize' and 'isSigned'.
-}
class Num a => Bits a where
    -- | Bitwise \"and\"
    (.&.) :: a -> a -> a

    -- | Bitwise \"or\"
    (.|.) :: a -> a -> a

    -- | Bitwise \"xor\"
    xor :: a -> a -> a

    {-| Reverse all the bits in the argument -}
    complement        :: a -> a

    {-| Shift the argument left by the specified number of bits.
	Right shifts (signed) are specified by giving a negative value.

	An instance can define either this unified 'shift' or 'shiftL' and
	'shiftR', depending on which is more convenient for the type in
	question. -}
    shift             :: a -> Int -> a

    x `shift`   i | i<0  = x `shiftR` (-i)
                  | i==0 = x
                  | i>0  = x `shiftL` i

    {-| Rotate the argument left by the specified number of bits.
	Right rotates are specified by giving a negative value.

        For unbounded types like 'Integer', 'rotate' is equivalent to 'shift'.

	An instance can define either this unified 'rotate' or 'rotateL' and
	'rotateR', depending on which is more convenient for the type in
	question. -}
    rotate            :: a -> Int -> a

    x `rotate`  i | i<0  = x `rotateR` (-i)
                  | i==0 = x
                  | i>0  = x `rotateL` i

    {-
    -- Rotation can be implemented in terms of two shifts, but care is
    -- needed for negative values.  This suggested implementation assumes
    -- 2's-complement arithmetic.  It is commented out because it would
    -- require an extra context (Ord a) on the signature of 'rotate'.
    x `rotate`  i | i<0 && isSigned x && x<0
                         = let left = i+bitSize x in
                           ((x `shift` i) .&. complement ((-1) `shift` left))
                           .|. (x `shift` left)
                  | i<0  = (x `shift` i) .|. (x `shift` (i+bitSize x))
                  | i==0 = x
                  | i>0  = (x `shift` i) .|. (x `shift` (i-bitSize x))
    -}

    -- | @bit i@ is a value with the @i@th bit set
    bit               :: Int -> a

    -- | @x \`setBit\` i@ is the same as @x .|. bit i@
    setBit            :: a -> Int -> a

    -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
    clearBit          :: a -> Int -> a

    -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
    complementBit     :: a -> Int -> a

    -- | Return 'True' if the @n@th bit of the argument is 1
    testBit           :: a -> Int -> Bool

    {-| Return the number of bits in the type of the argument.  The actual
	value of the argument is ignored.  The function 'bitSize' is
	undefined for types that do not have a fixed bitsize, like 'Integer'.
	-}
    bitSize           :: a -> Int

    {-| Return 'True' if the argument is a signed type.  The actual
        value of the argument is ignored -}
    isSigned          :: a -> Bool

    bit i               = 1 `shiftL` i
    x `setBit` i        = x .|. bit i
    x `clearBit` i      = x .&. complement (bit i)
    x `complementBit` i = x `xor` bit i
    x `testBit` i       = (x .&. bit i) /= 0

    {-| Shift the argument left by the specified number of bits
	(which must be non-negative).

	An instance can define either this and 'shiftR' or the unified
	'shift', depending on which is more convenient for the type in
	question. -}
    shiftL            :: a -> Int -> a
    x `shiftL`  i = x `shift`  i

    {-| Shift the argument right (signed) by the specified number of bits
	(which must be non-negative).

	An instance can define either this and 'shiftL' or the unified
	'shift', depending on which is more convenient for the type in
	question. -}
    shiftR            :: a -> Int -> a
    x `shiftR`  i = x `shift`  (-i)

    {-| Rotate the argument left by the specified number of bits
	(which must be non-negative).

	An instance can define either this and 'rotateR' or the unified
	'rotate', depending on which is more convenient for the type in
	question. -}
    rotateL           :: a -> Int -> a
    x `rotateL` i = x `rotate` i

    {-| Rotate the argument right by the specified number of bits
	(which must be non-negative).

	An instance can define either this and 'rotateL' or the unified
	'rotate', depending on which is more convenient for the type in
	question. -}
    rotateR           :: a -> Int -> a
    x `rotateR` i = x `rotate` (-i)

instance Bits Int where
    {-# INLINE shift #-}

#ifdef __GLASGOW_HASKELL__
    (I# x#) .&.   (I# y#)  = I# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I# x#) .|.   (I# y#)  = I# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I# x#) `xor` (I# y#)  = I# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I# x#)     = I# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    (I# x#) `shift` (I# i#)
        | i# >=# 0#        = I# (x# `iShiftL#` i#)
        | otherwise        = I# (x# `iShiftRA#` negateInt# i#)
    (I# x#) `rotate` (I# i#) =
        I# (word2Int# ((x'# `shiftL#` i'#) `or#`
                       (x'# `shiftRL#` (wsib -# i'#))))
        where
        x'# = int2Word# x#
        i'# = word2Int# (int2Word# i# `and#` int2Word# (wsib -# 1#))
	wsib = WORD_SIZE_IN_BITS#   {- work around preprocessor problem (??) -}
    bitSize  _             = WORD_SIZE_IN_BITS
#else /* !__GLASGOW_HASKELL__ */

#ifdef __HUGS__
    (.&.)                  = primAndInt
    (.|.)                  = primOrInt
    xor                    = primXorInt
    complement             = primComplementInt
    shift                  = primShiftInt
    bit                    = primBitInt
    testBit                = primTestInt
    bitSize _              = SIZEOF_HSINT*8
#elif defined(__NHC__)
    (.&.)                  = nhc_primIntAnd
    (.|.)                  = nhc_primIntOr
    xor                    = nhc_primIntXor
    complement             = nhc_primIntCompl
    shiftL                 = nhc_primIntLsh
    shiftR                 = nhc_primIntRsh
    bitSize _              = 32
#endif /* __NHC__ */

    x `rotate`  i
	| i<0 && x<0       = let left = i+bitSize x in
                             ((x `shift` i) .&. complement ((-1) `shift` left))
                             .|. (x `shift` left)
	| i<0              = (x `shift` i) .|. (x `shift` (i+bitSize x))
	| i==0             = x
	| i>0              = (x `shift` i) .|. (x `shift` (i-bitSize x))

#endif /* !__GLASGOW_HASKELL__ */

    isSigned _             = True

#ifdef __NHC__
foreign import ccall nhc_primIntAnd :: Int -> Int -> Int
foreign import ccall nhc_primIntOr  :: Int -> Int -> Int
foreign import ccall nhc_primIntXor :: Int -> Int -> Int
foreign import ccall nhc_primIntLsh :: Int -> Int -> Int
foreign import ccall nhc_primIntRsh :: Int -> Int -> Int
foreign import ccall nhc_primIntCompl :: Int -> Int
#endif /* __NHC__ */

instance Bits Integer where
#ifdef __GLASGOW_HASKELL__
   (S# x) .&. (S# y) = S# (word2Int# (int2Word# x `and#` int2Word# y))
   x@(S# _) .&. y = toBig x .&. y
   x .&. y@(S# _) = x .&. toBig y
   (J# s1 d1) .&. (J# s2 d2) = 
	case andInteger# s1 d1 s2 d2 of
	  (# s, d #) -> J# s d
   
   (S# x) .|. (S# y) = S# (word2Int# (int2Word# x `or#` int2Word# y))
   x@(S# _) .|. y = toBig x .|. y
   x .|. y@(S# _) = x .|. toBig y
   (J# s1 d1) .|. (J# s2 d2) = 
	case orInteger# s1 d1 s2 d2 of
	  (# s, d #) -> J# s d
   
   (S# x) `xor` (S# y) = S# (word2Int# (int2Word# x `xor#` int2Word# y))
   x@(S# _) `xor` y = toBig x `xor` y
   x `xor` y@(S# _) = x `xor` toBig y
   (J# s1 d1) `xor` (J# s2 d2) =
	case xorInteger# s1 d1 s2 d2 of
	  (# s, d #) -> J# s d
   
   complement (S# x) = S# (word2Int# (int2Word# x `xor#` int2Word# (0# -# 1#)))
   complement (J# s d) = case complementInteger# s d of (# s, d #) -> J# s d
#else
   -- reduce bitwise binary operations to special cases we can handle

   x .&. y   | x<0 && y<0 = complement (complement x `posOr` complement y)
	     | otherwise  = x `posAnd` y
   
   x .|. y   | x<0 || y<0 = complement (complement x `posAnd` complement y)
	     | otherwise  = x `posOr` y
   
   x `xor` y | x<0 && y<0 = complement x `posXOr` complement y
	     | x<0        = complement (complement x `posXOr` y)
	     |        y<0 = complement (x `posXOr` complement y)
	     | otherwise  = x `posXOr` y

   -- assuming infinite 2's-complement arithmetic
   complement a = -1 - a
#endif

   shift x i | i >= 0    = x * 2^i
	     | otherwise = x `div` 2^(-i)

   rotate x i = shift x i   -- since an Integer never wraps around

   bitSize _  = error "Data.Bits.bitSize(Integer)"
   isSigned _ = True

#ifndef __GLASGOW_HASKELL__
-- Crude implementation of bitwise operations on Integers: convert them
-- to finite lists of Ints (least significant first), zip and convert
-- back again.

-- posAnd requires at least one argument non-negative
-- posOr and posXOr require both arguments non-negative

posAnd, posOr, posXOr :: Integer -> Integer -> Integer
posAnd x y   = fromInts $ zipWith (.&.) (toInts x) (toInts y)
posOr x y    = fromInts $ longZipWith (.|.) (toInts x) (toInts y)
posXOr x y   = fromInts $ longZipWith xor (toInts x) (toInts y)

longZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
longZipWith f xs [] = xs
longZipWith f [] ys = ys
longZipWith f (x:xs) (y:ys) = f x y:longZipWith f xs ys

toInts :: Integer -> [Int]
toInts n
    | n == 0 = []
    | otherwise = mkInt (n `mod` numInts):toInts (n `div` numInts)
  where mkInt n | n > toInteger(maxBound::Int) = fromInteger (n-numInts)
		| otherwise = fromInteger n

fromInts :: [Int] -> Integer
fromInts = foldr catInt 0
    where catInt d n = (if d<0 then n+1 else n)*numInts + toInteger d

numInts = toInteger (maxBound::Int) - toInteger (minBound::Int) + 1
#endif /* !__GLASGOW_HASKELL__ */
