{- | Pack types of a known size into as few bytes as possible.

We do so by assigning each instance of the Packable class
a 'Width' in bits.

When combining types we add up their widths using type level computation.

We do all intermediate steps on Word/Int variants, this means converting between
them once all is said and done will be zero cost.

Only up to 63 bits of flags are supported, to ensure performance.

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
    ( KnownPackable(..)
    , SizedBoundedEnum(..)
    , SizedInt(..)
    , RepType
    , BitsFitIn
    )
where

import GhcPrelude

import BasicTypes
import Binary

import Util
import Data.Bits
import Data.Word
import Data.Int
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
                    Integer
                )
            )
        )

type family RepType (a :: Type) :: Type where
    RepType a = BitsFitIn (BitWidth a)


-- type family BitWidth a :: Nat

type family Max (n1 :: Nat) (n2 :: Nat) :: Nat where
    Max n1 n2 = If (n1 T.<=? n2) n2 n1

type BitRep a = (Integral a, Bits a, Num a)

-- | Allow packing of this type into a number of bits known at compile time.
class KnownPackable rep a | a -> rep where
    type BitWidth a :: Nat
    -- | Store as bits
    toBits :: a -> rep
    -- | Read from bits, we do not constrain this so we can eg read a Bool out of a Int or Word8
    fromBits :: (BitRep bits) => bits -> a

-- | Set the first x bits which the given instance occupies
{-# INLINE setBits #-}
setBits :: forall rep p. ( rep ~ RepType p, KnownNat (BitWidth p), KnownPackable rep p, Integral rep)
        => Proxy# p -> rep
setBits _ = (2 ^ bitcount) - 1
    where
    bitcount = natVal' ( proxy# :: Proxy# (BitWidth p) )


{-# INLINE bitWidth #-}
bitWidth :: forall a width rep. (width ~ BitWidth a, rep ~ RepType a, BitRep rep, KnownNat width)
         => Proxy# a -> Int
bitWidth _ = fromIntegral $ natVal' ( proxy# :: Proxy# (BitWidth a) ) :: Int

instance (rep ~ RepType Bool) => KnownPackable rep Bool where
    type BitWidth Bool = 1
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits x = fromIntegral . fromEnum $ x
    fromBits bits = toEnum tag
      where
        tag     = fromIntegral masked :: Int
        masked  = fromIntegral bits .&. range
        range   = setBits (proxy# :: Proxy# Bool)

instance forall rep repa a. ( RepType (Maybe a) ~ rep
                            , KnownPackable repa a
                            , BitRep rep
                            , BitRep repa
                            ) => KnownPackable rep (Maybe a) where
    type BitWidth (Maybe a) = BitWidth a + 1
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits Nothing = fromIntegral (0 :: Int) :: rep
    toBits (Just x) = 1 .|. (fromIntegral (toBits x) `unsafeShiftL` 1)
    fromBits bits
        | not (testBit bits 0) = Nothing
        | otherwise = Just . fromBits $ bits `unsafeShiftR` 1


instance forall rep repa repb a b.
                ( rep  ~ BitsFitIn (BitWidth (a,b)) , repa ~ RepType a, repb ~ RepType b
                , KnownNat (BitWidth a), KnownNat (BitWidth b)
                , KnownPackable repa a, KnownPackable repb b
                , BitRep rep, BitRep repa, BitRep repb
                ) => KnownPackable rep (a,b) where
    type BitWidth (a,b) = BitWidth a + BitWidth b
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits (a,b) = fromIntegral (toBits a) .|.
                  (fromIntegral (toBits b) `unsafeShiftL` (fromIntegral $ bitWidth (proxy# :: Proxy# a)))
    fromBits bits =
        let a = fromBits bits
            b = fromBits (bits `unsafeShiftR` bitWidth (proxy# :: Proxy# b))
        in (a,b)

instance forall rep repa repb repc a b c.
                ( rep  ~ RepType (a,b,c)
                , repa ~ RepType a, repb ~ RepType b, repc ~ RepType c
                , KnownNat (BitWidth a), KnownNat (BitWidth b), KnownNat (BitWidth c)
                , KnownPackable repa a, KnownPackable repb b, KnownPackable repc c
                , BitRep rep, BitRep repa, BitRep repb, BitRep repc
                ) => KnownPackable rep (a,b,c) where
    type BitWidth (a,b,c) = BitWidth a + BitWidth b + BitWidth c
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits (a,b,c) = fromIntegral (toBits a) .|.
                    (fromIntegral (toBits b) `unsafeShiftL` widthA) .|.
                    (fromIntegral (toBits c) `unsafeShiftL` (widthA + widthB))
        where
            widthA = fromIntegral $ bitWidth (proxy# :: Proxy# a)
            widthB = fromIntegral $ bitWidth (proxy# :: Proxy# b)

    fromBits bits =
        let widthA = fromIntegral $ bitWidth (proxy# :: Proxy# a)
            widthB = fromIntegral $ bitWidth (proxy# :: Proxy# b)
            a = fromBits bits
            b = fromBits (bits `unsafeShiftR` widthA)
            c = fromBits (bits `unsafeShiftR` (widthA + widthB))
        in (a,b,c)

-----------------------------------------
-- Useful newtype wrappers
-----------------------------------------

-- | Derive a Packed instance based on Bounded,Enum and the bits required for storage.
newtype SizedBoundedEnum (n :: Nat) a = SizedBoundedEnum a deriving Show

instance forall a rep size. (Bounded a, Enum a, rep ~ BitsFitIn size, BitRep rep
                            ,KnownNat size)
                            => KnownPackable rep (SizedBoundedEnum size a) where
    type BitWidth (SizedBoundedEnum size a) = size
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits (SizedBoundedEnum x) = fromIntegral . fromEnum $ x
    fromBits bits = SizedBoundedEnum $ toEnum tag
      where
        tag     = fromIntegral masked :: Int
        masked  = fromIntegral bits .&. range
        range   = setBits (proxy# :: Proxy# (SizedBoundedEnum size a))

deriving via SizedBoundedEnum 1 TopLevelFlag instance KnownPackable Word8 TopLevelFlag
deriving via SizedBoundedEnum 1 OneShotInfo instance KnownPackable Word8 OneShotInfo

deriving via SizedBoundedEnum 8  Word8  instance KnownPackable Word8  Word8
deriving via SizedBoundedEnum 16 Word16 instance KnownPackable Word16 Word16
deriving via SizedBoundedEnum 32 Word32 instance KnownPackable Word32 Word32

deriving via SizedBoundedEnum 8  Int8  instance KnownPackable  Word8  Int8
deriving via SizedBoundedEnum 16 Int16 instance KnownPackable Word16 Int16
deriving via SizedBoundedEnum 32 Int32 instance KnownPackable Word32 Int32

-- | Packed instance only preserving the 'size' lower bits.
newtype SizedInt (size :: Nat) = SizedInt { getInt :: Int } deriving (Num,Integral,Real,Eq,Ord,Enum) via Int

instance ( KnownNat size, RepType (SizedInt size) ~ rep
         , BitRep rep)
         => KnownPackable rep (SizedInt size) where
    type BitWidth (SizedInt size) = size
    {-# INLINE toBits #-}
    {-# INLINE fromBits #-}
    toBits (SizedInt x) = fromIntegral x
    fromBits bits = SizedInt value
      where
        value   = fromIntegral masked :: Int
        masked  = fromIntegral bits .&. range
        range   = setBits (proxy# :: Proxy# (SizedInt size))
