{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      :  GHC.Internal.Data.String.Interpolate
Copyright   :  (c) The University of Glasgow 2026
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  stable
Portability :  portable

The machinery behind -XStringInterpolation
-}
module GHC.Internal.Data.String.Interpolate (
  interpolateRaw,
  interpolateValue,
  interpolateAppend,
  interpolateEmpty,
  interpolateFinalize,

  -- * StringBuilder
  StringBuilder (..),
  buildString,

  -- * Interpolate class
  Interpolation (..),
  Interpolate (..),
  interpolateValueAt,
) where

import GHC.Internal.Base
import GHC.Internal.Data.Monoid (Endo (..))
import GHC.Internal.Data.String (IsString, fromString)
import GHC.Internal.Show (show)

-- Define 'Interpolate' instances
import GHC.Internal.Int (Int16, Int32, Int64, Int8)
import GHC.Internal.Integer (Integer)
import GHC.Internal.Natural (Natural)
import GHC.Internal.Word (Word16, Word32, Word64, Word8)

{----- Interpolator functions -----}

interpolateRaw :: (IsString s) => String -> s
interpolateRaw = fromString
{-# INLINE interpolateRaw #-}

interpolateValue :: (Interpolate a) => a -> Interpolation
interpolateValue x = Interpolation (interpolateValueAt x)
{-# INLINE interpolateValue #-}

interpolateAppend :: Interpolation -> Interpolation -> Interpolation
interpolateAppend = mappend
{-# INLINE interpolateAppend #-}

interpolateEmpty :: Interpolation
interpolateEmpty = mempty
{-# INLINE interpolateEmpty #-}

interpolateFinalize :: Interpolation -> String
interpolateFinalize = buildString . unInterpolation
{-# INLINE [1] interpolateFinalize #-}

{----- StringBuilder -----}

newtype StringBuilder = StringBuilder (Endo String)
  deriving newtype (Semigroup, Monoid)
instance IsString StringBuilder where
  fromString s = StringBuilder (Endo (s <>))
  {-# INLINE fromString #-}

buildString :: StringBuilder -> String
buildString (StringBuilder (Endo f)) = f ""
{-# INLINE buildString #-}

{---- Interpolate ----}

data Interpolation = Interpolation
  { unInterpolation :: forall s. (IsString s, Monoid s) => s
  }

instance IsString Interpolation where
  fromString s = Interpolation (fromString s)
  {-# INLINE fromString #-}
instance Semigroup Interpolation where
  Interpolation s1 <> Interpolation s2 = Interpolation (s1 <> s2)
  {-# INLINE (<>) #-}
instance Monoid Interpolation where
  mempty = Interpolation mempty
  {-# INLINE mempty #-}

{- | Function that can be targeted by rewrite rules to provide more performant
implementations for specific (a -> s) conversions
-}
interpolateValueAt :: (Interpolate a, IsString s, Monoid s) => a -> s
interpolateValueAt x = unInterpolation (interpolate x)
{-# NOINLINE [1] interpolateValueAt #-}

class Interpolate a where
  interpolate :: a -> Interpolation

instance Interpolate String where
  interpolate = fromString
  {-# INLINE interpolate #-}
instance Interpolate Char where
  interpolate c = fromString [c]
  {-# INLINE interpolate #-}

#define VIA_SHOW(t) \
  instance Interpolate t where { \
    interpolate = fromString . show; \
    {-# INLINE interpolate #-} \
  }

VIA_SHOW(Int)
VIA_SHOW(Int8)
VIA_SHOW(Int16)
VIA_SHOW(Int32)
VIA_SHOW(Int64)
VIA_SHOW(Integer)
VIA_SHOW(Natural)
VIA_SHOW(Word)
VIA_SHOW(Word8)
VIA_SHOW(Word16)
VIA_SHOW(Word32)
VIA_SHOW(Word64)
VIA_SHOW(Double)
VIA_SHOW(Float)
VIA_SHOW(Bool)
