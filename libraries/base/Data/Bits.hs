{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, BangPatterns, MagicHash #-}

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
    (.&.), (.|.), xor,
    complement,
    shift,
    rotate,
    bit,
    setBit,
    clearBit,
    complementBit,
    testBit,
    bitSizeMaybe,
    bitSize,
    isSigned,
    shiftL, shiftR,
    unsafeShiftL, unsafeShiftR,
    rotateL, rotateR,
    popCount
  ),
  FiniteBits(finiteBitSize),

  bitDefault,
  testBitDefault,
  popCountDefault
 ) where

-- Defines the @Bits@ class containing bit-based operations.
-- See library document for details on the semantics of the
-- individual operations.

#include "MachDeps.h"

import Data.Maybe
import GHC.Enum
import GHC.Num
import GHC.Base

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

{-# DEPRECATED bitSize "Use 'bitSizeMaybe' or 'finiteBitSize' instead" #-} -- deprecated in 7.8

{-|
The 'Bits' class defines bitwise operations over integral types.

* Bits are numbered from 0 with bit 0 being the least
  significant bit.

Minimal complete definition: '.&.', '.|.', 'xor', 'complement',
('shift' or ('shiftL' and 'shiftR')), ('rotate' or ('rotateL' and 'rotateR')),
'bitSize', 'isSigned', 'testBit', 'bit', and 'popCount'.  The latter three can
be implemented using `testBitDefault', 'bitDefault, and 'popCountDefault', if
@a@ is also an instance of 'Num'.
-}
class Eq a => Bits a where
    -- | Bitwise \"and\"
    (.&.) :: a -> a -> a

    -- | Bitwise \"or\"
    (.|.) :: a -> a -> a

    -- | Bitwise \"xor\"
    xor :: a -> a -> a

    {-| Reverse all the bits in the argument -}
    complement        :: a -> a

    {-| @'shift' x i@ shifts @x@ left by @i@ bits if @i@ is positive,
        or right by @-i@ bits otherwise.
        Right shifts perform sign extension on signed number types;
        i.e. they fill the top bits with 1 if the @x@ is negative
        and with 0 otherwise.

        An instance can define either this unified 'shift' or 'shiftL' and
        'shiftR', depending on which is more convenient for the type in
        question. -}
    shift             :: a -> Int -> a

    x `shift`   i | i<0       = x `shiftR` (-i)
                  | i>0       = x `shiftL` i
                  | otherwise = x

    {-| @'rotate' x i@ rotates @x@ left by @i@ bits if @i@ is positive,
        or right by @-i@ bits otherwise.

        For unbounded types like 'Integer', 'rotate' is equivalent to 'shift'.

        An instance can define either this unified 'rotate' or 'rotateL' and
        'rotateR', depending on which is more convenient for the type in
        question. -}
    rotate            :: a -> Int -> a

    x `rotate`  i | i<0       = x `rotateR` (-i)
                  | i>0       = x `rotateL` i
                  | otherwise = x

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

    -- | @bit i@ is a value with the @i@th bit set and all other bits clear
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
        value of the argument is ignored.  Returns Nothing
        for types that do not have a fixed bitsize, like 'Integer'.

        /Since: 4.7.0.0/
        -}
    bitSizeMaybe      :: a -> Maybe Int

    {-| Return the number of bits in the type of the argument.  The actual
        value of the argument is ignored.  The function 'bitSize' is
        undefined for types that do not have a fixed bitsize, like 'Integer'.
        -}
    bitSize           :: a -> Int

    {-| Return 'True' if the argument is a signed type.  The actual
        value of the argument is ignored -}
    isSigned          :: a -> Bool

    {-# INLINE setBit #-}
    {-# INLINE clearBit #-}
    {-# INLINE complementBit #-}
    x `setBit` i        = x .|. bit i
    x `clearBit` i      = x .&. complement (bit i)
    x `complementBit` i = x `xor` bit i

    {-| Shift the argument left by the specified number of bits
        (which must be non-negative).

        An instance can define either this and 'shiftR' or the unified
        'shift', depending on which is more convenient for the type in
        question. -}
    shiftL            :: a -> Int -> a
    {-# INLINE shiftL #-}
    x `shiftL`  i = x `shift`  i

    {-| Shift the argument left by the specified number of bits.  The
        result is undefined for negative shift amounts and shift amounts
        greater or equal to the 'bitSize'.

        Defaults to 'shiftL' unless defined explicitly by an instance.

        /Since: 4.5.0.0/ -}
    unsafeShiftL            :: a -> Int -> a
    {-# INLINE unsafeShiftL #-}
    x `unsafeShiftL` i = x `shiftL` i

    {-| Shift the first argument right by the specified number of bits. The
        result is undefined for negative shift amounts and shift amounts
        greater or equal to the 'bitSize'.

        Right shifts perform sign extension on signed number types;
        i.e. they fill the top bits with 1 if the @x@ is negative
        and with 0 otherwise.

        An instance can define either this and 'shiftL' or the unified
        'shift', depending on which is more convenient for the type in
        question. -}
    shiftR            :: a -> Int -> a
    {-# INLINE shiftR #-}
    x `shiftR`  i = x `shift`  (-i)

    {-| Shift the first argument right by the specified number of bits, which
        must be non-negative an smaller than the number of bits in the type.

        Right shifts perform sign extension on signed number types;
        i.e. they fill the top bits with 1 if the @x@ is negative
        and with 0 otherwise.

        Defaults to 'shiftR' unless defined explicitly by an instance.

        /Since: 4.5.0.0/ -}
    unsafeShiftR            :: a -> Int -> a
    {-# INLINE unsafeShiftR #-}
    x `unsafeShiftR` i = x `shiftR` i

    {-| Rotate the argument left by the specified number of bits
        (which must be non-negative).

        An instance can define either this and 'rotateR' or the unified
        'rotate', depending on which is more convenient for the type in
        question. -}
    rotateL           :: a -> Int -> a
    {-# INLINE rotateL #-}
    x `rotateL` i = x `rotate` i

    {-| Rotate the argument right by the specified number of bits
        (which must be non-negative).

        An instance can define either this and 'rotateL' or the unified
        'rotate', depending on which is more convenient for the type in
        question. -}
    rotateR           :: a -> Int -> a
    {-# INLINE rotateR #-}
    x `rotateR` i = x `rotate` (-i)

    {-| Return the number of set bits in the argument.  This number is
        known as the population count or the Hamming weight.

        /Since: 4.5.0.0/ -}
    popCount          :: a -> Int

    {-# MINIMAL (.&.), (.|.), xor, complement,
                (shift | (shiftL, shiftR)),
                (rotate | (rotateL, rotateR)),
                bitSize, bitSizeMaybe, isSigned, testBit, bit, popCount #-}

-- |The 'FiniteBits' class denotes types with a finite, fixed number of bits.
--
-- /Since: 4.7.0.0/
class Bits b => FiniteBits b where
    -- | Return the number of bits in the type of the argument.
    -- The actual value of the argument is ignored. Moreover, 'finiteBitSize'
    -- is total, in contrast to the deprecated 'bitSize' function it replaces.
    --
    -- @
    -- 'finiteBitSize' = 'bitSize'
    -- 'bitSizeMaybe' = 'Just' . 'finiteBitSize'
    -- @
    --
    -- /Since: 4.7.0.0/
    finiteBitSize :: b -> Int

-- The defaults below are written with lambdas so that e.g.
--     bit = bitDefault
-- is fully applied, so inlining will happen

-- | Default implementation for 'bit'.
--
-- Note that: @bitDefault i = 1 `shiftL` i@
--
-- /Since: 4.6.0.0/
bitDefault :: (Bits a, Num a) => Int -> a
bitDefault = \i -> 1 `shiftL` i
{-# INLINE bitDefault #-}

-- | Default implementation for 'testBit'.
--
-- Note that: @testBitDefault x i = (x .&. bit i) /= 0@
--
-- /Since: 4.6.0.0/
testBitDefault ::  (Bits a, Num a) => a -> Int -> Bool
testBitDefault = \x i -> (x .&. bit i) /= 0
{-# INLINE testBitDefault #-}

-- | Default implementation for 'popCount'.
--
-- This implementation is intentionally naive. Instances are expected to provide
-- an optimized implementation for their size.
--
-- /Since: 4.6.0.0/
popCountDefault :: (Bits a, Num a) => a -> Int
popCountDefault = go 0
 where
   go !c 0 = c
   go c w = go (c+1) (w .&. (w - 1)) -- clear the least significant
{-# INLINABLE popCountDefault #-}


-- Interpret 'Bool' as 1-bit bit-field; /Since: 4.7.0.0/
instance Bits Bool where
    (.&.) = (&&)

    (.|.) = (||)

    xor = (/=)

    complement = not

    shift x 0 = x
    shift _ _ = False

    rotate x _ = x

    bit 0 = True
    bit _ = False

    testBit x 0 = x
    testBit _ _ = False

    bitSizeMaybe _ = Just 1

    bitSize _ = 1

    isSigned _ = False

    popCount False = 0
    popCount True  = 1

instance FiniteBits Bool where
    finiteBitSize _ = 1


instance Bits Int where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    bit     = bitDefault

    testBit = testBitDefault

    (I# x#) .&.   (I# y#)          = I# (x# `andI#` y#)
    (I# x#) .|.   (I# y#)          = I# (x# `orI#`  y#)
    (I# x#) `xor` (I# y#)          = I# (x# `xorI#` y#)
    complement (I# x#)             = I# (notI# x#)
    (I# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)      = I# (x# `iShiftL#` i#)
        | otherwise                = I# (x# `iShiftRA#` negateInt# i#)
    (I# x#) `shiftL` (I# i#)       = I# (x# `iShiftL#` i#)
    (I# x#) `unsafeShiftL` (I# i#) = I# (x# `uncheckedIShiftL#` i#)
    (I# x#) `shiftR` (I# i#)       = I# (x# `iShiftRA#` i#)
    (I# x#) `unsafeShiftR` (I# i#) = I# (x# `uncheckedIShiftRA#` i#)

    {-# INLINE rotate #-} 	-- See Note [Constant folding for rotate]
    (I# x#) `rotate` (I# i#) =
        I# ((x# `uncheckedIShiftL#` i'#) `orI#` (x# `uncheckedIShiftRL#` (wsib -# i'#)))
      where
        !i'# = i# `andI#` (wsib -# 1#)
        !wsib = WORD_SIZE_IN_BITS#   {- work around preprocessor problem (??) -}
    bitSizeMaybe i         = Just (finiteBitSize i)
    bitSize i              = finiteBitSize i

    popCount (I# x#) = I# (word2Int# (popCnt# (int2Word# x#)))

    isSigned _             = True

instance FiniteBits Int where
    finiteBitSize _ = WORD_SIZE_IN_BITS

instance Bits Word where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W# x#) .&.   (W# y#)    = W# (x# `and#` y#)
    (W# x#) .|.   (W# y#)    = W# (x# `or#`  y#)
    (W# x#) `xor` (W# y#)    = W# (x# `xor#` y#)
    complement (W# x#)       = W# (x# `xor#` mb#)
        where !(W# mb#) = maxBound
    (W# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)      = W# (x# `shiftL#` i#)
        | otherwise                = W# (x# `shiftRL#` negateInt# i#)
    (W# x#) `shiftL` (I# i#)       = W# (x# `shiftL#` i#)
    (W# x#) `unsafeShiftL` (I# i#) = W# (x# `uncheckedShiftL#` i#)
    (W# x#) `shiftR` (I# i#)       = W# (x# `shiftRL#` i#)
    (W# x#) `unsafeShiftR` (I# i#) = W# (x# `uncheckedShiftRL#` i#)
    (W# x#) `rotate` (I# i#)
        | isTrue# (i'# ==# 0#) = W# x#
        | otherwise  = W# ((x# `uncheckedShiftL#` i'#) `or#` (x# `uncheckedShiftRL#` (wsib -# i'#)))
        where
        !i'# = i# `andI#` (wsib -# 1#)
        !wsib = WORD_SIZE_IN_BITS#  {- work around preprocessor problem (??) -}
    bitSizeMaybe i           = Just (finiteBitSize i)
    bitSize i                = finiteBitSize i
    isSigned _               = False
    popCount (W# x#)         = I# (word2Int# (popCnt# x#))
    bit                      = bitDefault
    testBit                  = testBitDefault

instance FiniteBits Word where
    finiteBitSize _ = WORD_SIZE_IN_BITS

instance Bits Integer where
   (.&.) = andInteger
   (.|.) = orInteger
   xor = xorInteger
   complement = complementInteger
   shift x i@(I# i#) | i >= 0    = shiftLInteger x i#
                     | otherwise = shiftRInteger x (negateInt# i#)
   testBit x (I# i) = testBitInteger x i

   bit        = bitDefault
   popCount   = popCountDefault

   rotate x i = shift x i   -- since an Integer never wraps around

   bitSizeMaybe _ = Nothing
   bitSize _  = error "Data.Bits.bitSize(Integer)"
   isSigned _ = True

{- 	Note [Constant folding for rotate]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The INLINE on the Int instance of rotate enables it to be constant
folded.  For example:
     sumU . mapU (`rotate` 3) . replicateU 10000000 $ (7 :: Int)
goes to:
   Main.$wfold =
     \ (ww_sO7 :: Int#) (ww1_sOb :: Int#) ->
       case ww1_sOb of wild_XM {
         __DEFAULT -> Main.$wfold (+# ww_sO7 56) (+# wild_XM 1);
         10000000 -> ww_sO7
whereas before it was left as a call to $wrotate.

All other Bits instances seem to inline well enough on their
own to enable constant folding; for example 'shift':
     sumU . mapU (`shift` 3) . replicateU 10000000 $ (7 :: Int)
 goes to:
     Main.$wfold =
       \ (ww_sOb :: Int#) (ww1_sOf :: Int#) ->
         case ww1_sOf of wild_XM {
           __DEFAULT -> Main.$wfold (+# ww_sOb 56) (+# wild_XM 1);
           10000000 -> ww_sOb
         }
-}

