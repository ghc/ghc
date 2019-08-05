{- | Pack types of a known size into as few bytes as possible.

We do so by assigning each instance of the Packable class
a 'Width' in bits.

When combining types we add up their widths using type level computation.

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds  #-}

-- Allow constraint on result of toBits.
{-# LANGUAGE ConstrainedClassMethods  #-}

{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DerivingVia #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module PackedFlags
    (   PackedFlags(..)
    )
where

import GhcPrelude

import BasicTypes
import Binary

import Internal.Util hiding (setBits)
import Data.Bits
import Data.Word
import Data.Proxy
import Data.Kind
import GHC.TypeLits as T
import GHC.Exts (Proxy#, proxy#)

type family If c t e where
  If 'True  t e = t
  If 'False t e = e

-- | Will always return a Integral type
type family BitsFitIn (a :: Nat) :: Type where
    BitsFitIn 0 = Word8
    BitsFitIn (n ) =
      If (n T.<=? 8) (Word8)
        (If (n T.<=? 16) (Word16)
            (If (n T.<=? 32) (Word32)
                (If (n T.<=? 64) (Word64)
                    Error "PackFlags - Flags take up > 64 bits"
                )
            )
        )

type family RepType (a :: Type) :: Type where
    RepType a = BitsFitIn (BitWidth a)

type family BitWidth a :: Nat where
    BitWidth Bool = 1
    BitWidth (Either n1 n2) = 1 T.+ (Max (BitWidth n1) (BitWidth n1))
    BitWidth (Maybe a) = 1 + BitWidth a
    BitWidth (a,b) = BitWidth a + BitWidth b
    BitWidth (SizedBoundedEnum size a) = size

type family Max (n1 :: Nat) (n2 :: Nat) :: Nat where
    Max n1 n2 = If (n1 T.<=? n2) n2 n1

type BitRep a = (Integral a, Bits a)

-- | Allow packing of this type into a number of bits known at compile time.
class (RepType a ~ rep, BitRep rep) => PackedFlags rep a | a -> rep where
    -- | Store as bits
    toBits :: a -> rep
    -- | Read from bits, we do not constrain this so we can eg read a Bool out of a Int or Word8
    fromBits :: (BitRep bits) => bits -> a

-- | Set the first x bits which the given instance occupies
{-# INLINE setBits #-}
setBits :: forall rep p. (rep ~ RepType p, KnownNat (BitWidth p), PackedFlags rep p, Integral rep) => Proxy# p -> rep
setBits p = (2 ^ bitcount) - 1
    where
    bitcount = natVal' ( proxy# :: Proxy# (BitWidth p) )

-- | Get the number of bits p occupies when packed.
{-# INLINE bitWidth #-}
bitWidth :: forall a width rep. (width ~ BitWidth a, rep ~ RepType a, BitRep rep, KnownNat width)
         => Proxy# a -> Int
bitWidth p = fromIntegral $ (2 ^ bitcount) - 1
    where
    bitcount = natVal' ( proxy# :: Proxy# (BitWidth a) )

instance (rep ~ RepType Bool) => PackedFlags rep Bool where
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits x = fromIntegral . fromEnum $ x
    fromBits bits = toEnum tag
      where
        tag     = fromIntegral masked :: Int
        masked  = fromIntegral bits .&. range
        range   = setBits (proxy# :: Proxy# Bool)

instance forall rep repa a. ( RepType (Maybe a) ~ rep
                            , PackedFlags repa a
                            , BitRep rep
                            ) => PackedFlags rep (Maybe a) where
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits Nothing = fromIntegral 0 :: rep
    toBits (Just x) = 1 .|. (fromIntegral (toBits x) `unsafeShiftL` 1)
    fromBits bits
        | not (testBit bits 0) = Nothing
        | otherwise = Just . fromBits $ bits `unsafeShiftR` 1

instance forall rep repa repb a b.
                ( rep  ~ RepType (a,b), BitRep rep
                , KnownNat (BitWidth a), KnownNat (BitWidth b)
                , PackedFlags repa a, PackedFlags repb b
                ) => PackedFlags rep (a,b) where
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits (a,b) = fromIntegral (toBits a) .|. (fromIntegral (toBits b) `unsafeShiftL` (fromIntegral $ bitWidth (proxy# :: Proxy# a)))
    fromBits bits =
        let a = fromBits bits
            b = fromBits (bits `unsafeShiftR` bitWidth (proxy# :: Proxy# b))
        in (a,b)

-----------------------------------------
-- Useful newtype wrappers
-----------------------------------------

-- From (Bounded a, Enum a) and a type level size, derive a PackedFlags instance.
newtype SizedBoundedEnum (n :: Nat) a = SizedBoundedEnum a

instance forall a rep size. (Bounded a, Enum a, rep ~ BitsFitIn size, BitRep rep) => PackedFlags rep (SizedBoundedEnum size a) where
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits (SizedBoundedEnum x) = fromIntegral . fromEnum $ x
    fromBits bits = SizedBoundedEnum $ toEnum tag
      where
        tag     = fromIntegral masked :: Int
        masked  = fromIntegral bits .&. range
        range   = setBits (proxy# :: Proxy# Bool)

-----------------------------------------
-- A few basic types instances
-----------------------------------------

deriving via SizedBoundedEnum 1 TopLevelFlag instance PackedFlags rep TopLevelFlag

.a.StandaloneDeriving

asdas