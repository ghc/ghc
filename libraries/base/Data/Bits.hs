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
    zeroBits,
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
  FiniteBits(
    finiteBitSize,
    countLeadingZeros,
    countTrailingZeros
  ),

  bitDefault,
  testBitDefault,
  popCountDefault,
  toIntegralSized
 ) where

-- Defines the @Bits@ class containing bit-based operations.
-- See library document for details on the semantics of the
-- individual operations.

#include "MachDeps.h"

#ifdef MIN_VERSION_integer_gmp
# define HAVE_INTEGER_GMP1 MIN_VERSION_integer_gmp(1,0,0)
#endif

import Data.Maybe
import GHC.Enum
import GHC.Num
import GHC.Base
import GHC.Real

#if HAVE_INTEGER_GMP1
import GHC.Integer.GMP.Internals (bitInteger, popCountInteger)
#endif

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

{-# DEPRECATED bitSize "Use 'bitSizeMaybe' or 'finiteBitSize' instead" #-} -- deprecated in 7.8

-- | The 'Bits' class defines bitwise operations over integral types.
--
-- * Bits are numbered from 0 with bit 0 being the least
--   significant bit.
class Eq a => Bits a where
    {-# MINIMAL (.&.), (.|.), xor, complement,
                (shift | (shiftL, shiftR)),
                (rotate | (rotateL, rotateR)),
                bitSize, bitSizeMaybe, isSigned, testBit, bit, popCount #-}

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

    -- | 'zeroBits' is the value with all bits unset.
    --
    -- The following laws ought to hold (for all valid bit indices @/n/@):
    --
    --   * @'clearBit' 'zeroBits' /n/ == 'zeroBits'@
    --   * @'setBit'   'zeroBits' /n/ == 'bit' /n/@
    --   * @'testBit'  'zeroBits' /n/ == False@
    --   * @'popCount' 'zeroBits'   == 0@
    --
    -- This method uses @'clearBit' ('bit' 0) 0@ as its default
    -- implementation (which ought to be equivalent to 'zeroBits' for
    -- types which possess a 0th bit).
    --
    -- @since 4.7.0.0
    zeroBits :: a
    zeroBits = clearBit (bit 0) 0

    -- | @bit /i/@ is a value with the @/i/@th bit set and all other bits clear.
    --
    -- Can be implemented using `bitDefault' if @a@ is also an
    -- instance of 'Num'.
    --
    -- See also 'zeroBits'.
    bit               :: Int -> a

    -- | @x \`setBit\` i@ is the same as @x .|. bit i@
    setBit            :: a -> Int -> a

    -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
    clearBit          :: a -> Int -> a

    -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
    complementBit     :: a -> Int -> a

    -- | Return 'True' if the @n@th bit of the argument is 1
    --
    -- Can be implemented using `testBitDefault' if @a@ is also an
    -- instance of 'Num'.
    testBit           :: a -> Int -> Bool

    {-| Return the number of bits in the type of the argument.  The actual
        value of the argument is ignored.  Returns Nothing
        for types that do not have a fixed bitsize, like 'Integer'.

        @since 4.7.0.0
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

        @since 4.5.0.0 -}
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

        @since 4.5.0.0 -}
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

        Can be implemented using `popCountDefault' if @a@ is also an
        instance of 'Num'.

        @since 4.5.0.0 -}
    popCount          :: a -> Int

-- |The 'FiniteBits' class denotes types with a finite, fixed number of bits.
--
-- @since 4.7.0.0
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
    -- @since 4.7.0.0
    finiteBitSize :: b -> Int

    -- | Count number of zero bits preceding the most significant set bit.
    --
    -- @
    -- 'countLeadingZeros' ('zeroBits' :: a) = finiteBitSize ('zeroBits' :: a)
    -- @
    --
    -- 'countLeadingZeros' can be used to compute log base 2 via
    --
    -- @
    -- logBase2 x = 'finiteBitSize' x - 1 - 'countLeadingZeros' x
    -- @
    --
    -- Note: The default implementation for this method is intentionally
    -- naive. However, the instances provided for the primitive
    -- integral types are implemented using CPU specific machine
    -- instructions.
    --
    -- @since 4.8.0.0
    countLeadingZeros :: b -> Int
    countLeadingZeros x = (w-1) - go (w-1)
      where
        go i | i < 0       = i -- no bit set
             | testBit x i = i
             | otherwise   = go (i-1)

        w = finiteBitSize x

    -- | Count number of zero bits following the least significant set bit.
    --
    -- @
    -- 'countTrailingZeros' ('zeroBits' :: a) = finiteBitSize ('zeroBits' :: a)
    -- 'countTrailingZeros' . 'negate' = 'countTrailingZeros'
    -- @
    --
    -- The related
    -- <http://en.wikipedia.org/wiki/Find_first_set find-first-set operation>
    -- can be expressed in terms of 'countTrailingZeros' as follows
    --
    -- @
    -- findFirstSet x = 1 + 'countTrailingZeros' x
    -- @
    --
    -- Note: The default implementation for this method is intentionally
    -- naive. However, the instances provided for the primitive
    -- integral types are implemented using CPU specific machine
    -- instructions.
    --
    -- @since 4.8.0.0
    countTrailingZeros :: b -> Int
    countTrailingZeros x = go 0
      where
        go i | i >= w      = i
             | testBit x i = i
             | otherwise   = go (i+1)

        w = finiteBitSize x


-- The defaults below are written with lambdas so that e.g.
--     bit = bitDefault
-- is fully applied, so inlining will happen

-- | Default implementation for 'bit'.
--
-- Note that: @bitDefault i = 1 `shiftL` i@
--
-- @since 4.6.0.0
bitDefault :: (Bits a, Num a) => Int -> a
bitDefault = \i -> 1 `shiftL` i
{-# INLINE bitDefault #-}

-- | Default implementation for 'testBit'.
--
-- Note that: @testBitDefault x i = (x .&. bit i) /= 0@
--
-- @since 4.6.0.0
testBitDefault ::  (Bits a, Num a) => a -> Int -> Bool
testBitDefault = \x i -> (x .&. bit i) /= 0
{-# INLINE testBitDefault #-}

-- | Default implementation for 'popCount'.
--
-- This implementation is intentionally naive. Instances are expected to provide
-- an optimized implementation for their size.
--
-- @since 4.6.0.0
popCountDefault :: (Bits a, Num a) => a -> Int
popCountDefault = go 0
 where
   go !c 0 = c
   go c w = go (c+1) (w .&. (w - 1)) -- clear the least significant
{-# INLINABLE popCountDefault #-}


-- Interpret 'Bool' as 1-bit bit-field; @since 4.7.0.0
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
    countTrailingZeros x = if x then 0 else 1
    countLeadingZeros  x = if x then 0 else 1

instance Bits Int where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    zeroBits = 0

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

    {-# INLINE rotate #-}       -- See Note [Constant folding for rotate]
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
    countLeadingZeros  (I# x#) = I# (word2Int# (clz# (int2Word# x#)))
    countTrailingZeros (I# x#) = I# (word2Int# (ctz# (int2Word# x#)))

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
    countLeadingZeros  (W# x#) = I# (word2Int# (clz# x#))
    countTrailingZeros (W# x#) = I# (word2Int# (ctz# x#))

instance Bits Integer where
   (.&.) = andInteger
   (.|.) = orInteger
   xor = xorInteger
   complement = complementInteger
   shift x i@(I# i#) | i >= 0    = shiftLInteger x i#
                     | otherwise = shiftRInteger x (negateInt# i#)
   testBit x (I# i) = testBitInteger x i
   zeroBits   = 0

#if HAVE_INTEGER_GMP1
   bit (I# i#) = bitInteger i#
   popCount x  = I# (popCountInteger x)
#else
   bit        = bitDefault
   popCount   = popCountDefault
#endif

   rotate x i = shift x i   -- since an Integer never wraps around

   bitSizeMaybe _ = Nothing
   bitSize _  = errorWithoutStackTrace "Data.Bits.bitSize(Integer)"
   isSigned _ = True

-----------------------------------------------------------------------------

-- | Attempt to convert an 'Integral' type @a@ to an 'Integral' type @b@ using
-- the size of the types as measured by 'Bits' methods.
--
-- A simpler version of this function is:
--
-- > toIntegral :: (Integral a, Integral b) => a -> Maybe b
-- > toIntegral x
-- >   | toInteger x == y = Just (fromInteger y)
-- >   | otherwise        = Nothing
-- >   where
-- >     y = toInteger x
--
-- This version requires going through 'Integer', which can be inefficient.
-- However, @toIntegralSized@ is optimized to allow GHC to statically determine
-- the relative type sizes (as measured by 'bitSizeMaybe' and 'isSigned') and
-- avoid going through 'Integer' for many types. (The implementation uses
-- 'fromIntegral', which is itself optimized with rules for @base@ types but may
-- go through 'Integer' for some type pairs.)
--
-- @since 4.8.0.0

toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
toIntegralSized x                 -- See Note [toIntegralSized optimization]
  | maybe True (<= x) yMinBound
  , maybe True (x <=) yMaxBound = Just y
  | otherwise                   = Nothing
  where
    y = fromIntegral x

    xWidth = bitSizeMaybe x
    yWidth = bitSizeMaybe y

    yMinBound
      | isBitSubType x y = Nothing
      | isSigned x, not (isSigned y) = Just 0
      | isSigned x, isSigned y
      , Just yW <- yWidth = Just (negate $ bit (yW-1)) -- Assumes sub-type
      | otherwise = Nothing

    yMaxBound
      | isBitSubType x y = Nothing
      | isSigned x, not (isSigned y)
      , Just xW <- xWidth, Just yW <- yWidth
      , xW <= yW+1 = Nothing -- Max bound beyond a's domain
      | Just yW <- yWidth = if isSigned y
                            then Just (bit (yW-1)-1)
                            else Just (bit yW-1)
      | otherwise = Nothing
{-# INLINEABLE toIntegralSized #-}

-- | 'True' if the size of @a@ is @<=@ the size of @b@, where size is measured
-- by 'bitSizeMaybe' and 'isSigned'.
isBitSubType :: (Bits a, Bits b) => a -> b -> Bool
isBitSubType x y
  -- Reflexive
  | xWidth == yWidth, xSigned == ySigned = True

  -- Every integer is a subset of 'Integer'
  | ySigned, Nothing == yWidth                  = True
  | not xSigned, not ySigned, Nothing == yWidth = True

  -- Sub-type relations between fixed-with types
  | xSigned == ySigned,   Just xW <- xWidth, Just yW <- yWidth = xW <= yW
  | not xSigned, ySigned, Just xW <- xWidth, Just yW <- yWidth = xW <  yW

  | otherwise = False
  where
    xWidth  = bitSizeMaybe x
    xSigned = isSigned     x

    yWidth  = bitSizeMaybe y
    ySigned = isSigned     y
{-# INLINE isBitSubType #-}

{-      Note [Constant folding for rotate]
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

-- Note [toIntegralSized optimization]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The code in 'toIntegralSized' relies on GHC optimizing away statically
-- decidable branches.
--
-- If both integral types are statically known, GHC will be able optimize the
-- code significantly (for @-O1@ and better).
--
-- For instance (as of GHC 7.8.1) the following definitions:
--
-- > w16_to_i32 = toIntegralSized :: Word16 -> Maybe Int32
-- >
-- > i16_to_w16 = toIntegralSized :: Int16 -> Maybe Word16
--
-- are translated into the following (simplified) /GHC Core/ language:
--
-- > w16_to_i32 = \x -> Just (case x of _ { W16# x# -> I32# (word2Int# x#) })
-- >
-- > i16_to_w16 = \x -> case eta of _
-- >   { I16# b1 -> case tagToEnum# (<=# 0 b1) of _
-- >       { False -> Nothing
-- >       ; True -> Just (W16# (narrow16Word# (int2Word# b1)))
-- >       }
-- >   }
