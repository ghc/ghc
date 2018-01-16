{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 700
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
#endif
#if MIN_VERSION_base(4,7,0)
-- We need to implement bitSize for the Bits instance, but it's deprecated.
{-# OPTIONS_GHC -fno-warn-deprecations #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Identity
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The identity functor and monad.
--
-- This trivial type constructor serves two purposes:
--
-- * It can be used with functions parameterized by functor or monad classes.
--
-- * It can be used as a base monad to which a series of monad
--   transformers may be applied to construct a composite monad.
--   Most monad transformer modules include the special case of
--   applying the transformer to 'Identity'.  For example, @State s@
--   is an abbreviation for @StateT s 'Identity'@.
-----------------------------------------------------------------------------

module Data.Functor.Identity (
    Identity(..),
  ) where

import Data.Bits
import Control.Applicative
import Control.Arrow (Arrow((***)))
import Control.Monad.Fix
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith, munzip))
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(mempty, mappend))
import Data.String (IsString(fromString))
import Data.Traversable (Traversable(traverse))
#if __GLASGOW_HASKELL__ >= 700
import Data.Data
#endif
import Data.Ix (Ix(..))
import Foreign (Storable(..), castPtr)
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

-- | Identity functor and monad. (a non-strict monad)
newtype Identity a = Identity { runIdentity :: a }
    deriving ( Eq, Ord
#if __GLASGOW_HASKELL__ >= 700
             , Data, Typeable
#endif
#if __GLASGOW_HASKELL__ >= 702
             , Generic
#endif
#if __GLASGOW_HASKELL__ >= 706
             , Generic1
#endif
             )

instance (Bits a) => Bits (Identity a) where
    Identity x .&. Identity y     = Identity (x .&. y)
    Identity x .|. Identity y     = Identity (x .|. y)
    xor (Identity x) (Identity y) = Identity (xor x y)
    complement   (Identity x)     = Identity (complement x)
    shift        (Identity x) i   = Identity (shift    x i)
    rotate       (Identity x) i   = Identity (rotate   x i)
    setBit       (Identity x) i   = Identity (setBit   x i)
    clearBit     (Identity x) i   = Identity (clearBit x i)
    shiftL       (Identity x) i   = Identity (shiftL   x i)
    shiftR       (Identity x) i   = Identity (shiftR   x i)
    rotateL      (Identity x) i   = Identity (rotateL  x i)
    rotateR      (Identity x) i   = Identity (rotateR  x i)
    testBit      (Identity x) i   = testBit x i
    bitSize      (Identity x)     = bitSize x
    isSigned     (Identity x)     = isSigned x
    bit i                         = Identity (bit i)
#if MIN_VERSION_base(4,5,0)
    unsafeShiftL (Identity x) i   = Identity (unsafeShiftL x i)
    unsafeShiftR (Identity x) i   = Identity (unsafeShiftR x i)
    popCount     (Identity x)     = popCount x
#endif
#if MIN_VERSION_base(4,7,0)
    zeroBits                      = Identity zeroBits
    bitSizeMaybe (Identity x)     = bitSizeMaybe x
#endif

instance (Bounded a) => Bounded (Identity a) where
    minBound = Identity minBound
    maxBound = Identity maxBound

instance (Enum a) => Enum (Identity a) where
    succ (Identity x)     = Identity (succ x)
    pred (Identity x)     = Identity (pred x)
    toEnum i              = Identity (toEnum i)
    fromEnum (Identity x) = fromEnum x
    enumFrom (Identity x) = map Identity (enumFrom x)
    enumFromThen (Identity x) (Identity y) = map Identity (enumFromThen x y)
    enumFromTo   (Identity x) (Identity y) = map Identity (enumFromTo   x y)
    enumFromThenTo (Identity x) (Identity y) (Identity z) =
        map Identity (enumFromThenTo x y z)

#if MIN_VERSION_base(4,7,0)
instance (FiniteBits a) => FiniteBits (Identity a) where
    finiteBitSize (Identity x) = finiteBitSize x
#endif

instance (Floating a) => Floating (Identity a) where
    pi                                = Identity pi
    exp   (Identity x)                = Identity (exp x)
    log   (Identity x)                = Identity (log x)
    sqrt  (Identity x)                = Identity (sqrt x)
    sin   (Identity x)                = Identity (sin x)
    cos   (Identity x)                = Identity (cos x)
    tan   (Identity x)                = Identity (tan x)
    asin  (Identity x)                = Identity (asin x)
    acos  (Identity x)                = Identity (acos x)
    atan  (Identity x)                = Identity (atan x)
    sinh  (Identity x)                = Identity (sinh x)
    cosh  (Identity x)                = Identity (cosh x)
    tanh  (Identity x)                = Identity (tanh x)
    asinh (Identity x)                = Identity (asinh x)
    acosh (Identity x)                = Identity (acosh x)
    atanh (Identity x)                = Identity (atanh x)
    Identity x ** Identity y          = Identity (x ** y)
    logBase (Identity x) (Identity y) = Identity (logBase x y)

instance (Fractional a) => Fractional (Identity a) where
    Identity x / Identity y = Identity (x / y)
    recip (Identity x)      = Identity (recip x)
    fromRational r          = Identity (fromRational r)

instance (IsString a) => IsString (Identity a) where
    fromString s = Identity (fromString s)

instance (Ix a) => Ix (Identity a) where
    range     (Identity x, Identity y) = map Identity (range (x, y))
    index     (Identity x, Identity y) (Identity i) = index     (x, y) i
    inRange   (Identity x, Identity y) (Identity e) = inRange   (x, y) e
    rangeSize (Identity x, Identity y) = rangeSize (x, y)

instance (Integral a) => Integral (Identity a) where
    quot    (Identity x) (Identity y) = Identity (quot x y)
    rem     (Identity x) (Identity y) = Identity (rem  x y)
    div     (Identity x) (Identity y) = Identity (div  x y)
    mod     (Identity x) (Identity y) = Identity (mod  x y)
    quotRem (Identity x) (Identity y) = (Identity *** Identity) (quotRem x y)
    divMod  (Identity x) (Identity y) = (Identity *** Identity) (divMod  x y)
    toInteger (Identity x)            = toInteger x

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y)

instance (Num a) => Num (Identity a) where
    Identity x + Identity y = Identity (x + y)
    Identity x - Identity y = Identity (x - y)
    Identity x * Identity y = Identity (x * y)
    negate (Identity x)     = Identity (negate x)
    abs    (Identity x)     = Identity (abs    x)
    signum (Identity x)     = Identity (signum x)
    fromInteger n           = Identity (fromInteger n)

instance (Real a) => Real (Identity a) where
    toRational (Identity x) = toRational x

instance (RealFloat a) => RealFloat (Identity a) where
    floatRadix     (Identity x)     = floatRadix     x
    floatDigits    (Identity x)     = floatDigits    x
    floatRange     (Identity x)     = floatRange     x
    decodeFloat    (Identity x)     = decodeFloat    x
    exponent       (Identity x)     = exponent       x
    isNaN          (Identity x)     = isNaN          x
    isInfinite     (Identity x)     = isInfinite     x
    isDenormalized (Identity x)     = isDenormalized x
    isNegativeZero (Identity x)     = isNegativeZero x
    isIEEE         (Identity x)     = isIEEE         x
    significand    (Identity x)     = significand (Identity x)
    scaleFloat s   (Identity x)     = Identity (scaleFloat s x)
    encodeFloat m n                 = Identity (encodeFloat m n)
    atan2 (Identity x) (Identity y) = Identity (atan2 x y)

instance (RealFrac a) => RealFrac (Identity a) where
    properFraction (Identity x) = (id *** Identity) (properFraction x)
    truncate       (Identity x) = truncate x
    round          (Identity x) = round    x
    ceiling        (Identity x) = ceiling  x
    floor          (Identity x) = floor    x

instance (Storable a) => Storable (Identity a) where
    sizeOf    (Identity x)       = sizeOf x
    alignment (Identity x)       = alignment x
    peekElemOff p i              = fmap Identity (peekElemOff (castPtr p) i)
    pokeElemOff p i (Identity x) = pokeElemOff (castPtr p) i x
    peekByteOff p i              = fmap Identity (peekByteOff p i)
    pokeByteOff p i (Identity x) = pokeByteOff p i x
    peek p                       = fmap runIdentity (peek (castPtr p))
    poke p (Identity x)          = poke (castPtr p) x

-- These instances would be equivalent to the derived instances of the
-- newtype if the field were removed.

instance (Read a) => Read (Identity a) where
    readsPrec d = readParen (d > 10) $ \ r ->
        [(Identity x,t) | ("Identity",s) <- lex r, (x,t) <- readsPrec 11 s]

instance (Show a) => Show (Identity a) where
    showsPrec d (Identity x) = showParen (d > 10) $
        showString "Identity " . showsPrec 11 x

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance Applicative Identity where
    pure a = Identity a
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)

instance MonadFix Identity where
    mfix f = Identity (fix (runIdentity . f))

#if MIN_VERSION_base(4,4,0)
instance MonadZip Identity where
    mzipWith f (Identity x) (Identity y) = Identity (f x y)
    munzip (Identity (a, b)) = (Identity a, Identity b)
#endif
