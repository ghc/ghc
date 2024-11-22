{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}

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
  InterpolateBuilder (..),
  Interpolate (..),
) where

import GHC.Internal.Base
import GHC.Internal.Data.Monoid (Endo (..))
import GHC.Internal.Data.String (IsString, fromString)
import GHC.Internal.Show (show)
import GHC.Internal.Types

{----- Interpolator functions -----}

interpolateRaw :: (IsString s) => String -> s
interpolateRaw = fromString
{-# INLINE [1] interpolateRaw #-}

interpolateValue :: (Interpolate a, IsString s, Monoid s) => a -> s
interpolateValue = unInterpolateBuilder . interpolate
{-# INLINE [1] interpolateValue #-}

interpolateAppend :: (Monoid s) => s -> s -> s
interpolateAppend = mappend
{-# INLINE [1] interpolateAppend #-}

interpolateEmpty :: (Monoid s) => s
interpolateEmpty = mempty
{-# INLINE [1] interpolateEmpty #-}

interpolateFinalize :: (forall s. (IsString s, Monoid s) => s) -> String
interpolateFinalize = buildString
{-# INLINE [1] interpolateFinalize #-}

{----- StringBuilder -----}

newtype StringBuilder = StringBuilder (Endo String)
  deriving newtype (Semigroup, Monoid)
instance IsString StringBuilder where
  fromString s = StringBuilder (Endo (s <>))
  {-# INLINE [1] fromString #-}

buildString :: StringBuilder -> String
buildString (StringBuilder (Endo f)) = f ""
{-# INLINE [1] buildString #-}

{---- Interpolate ----}

newtype InterpolateBuilder = InterpolateBuilder
  { unInterpolateBuilder :: forall s. (IsString s, Monoid s) => s
  }

instance IsString InterpolateBuilder where
  fromString s = InterpolateBuilder (fromString s)
  {-# INLINE [1] fromString #-}
instance Semigroup InterpolateBuilder where
  InterpolateBuilder s1 <> InterpolateBuilder s2 = InterpolateBuilder (s1 <> s2)
  {-# INLINE [1] (<>) #-}
instance Monoid InterpolateBuilder where
  mempty = InterpolateBuilder mempty
  {-# INLINE [1] mempty #-}

class Interpolate a where
  interpolate :: a -> InterpolateBuilder

instance Interpolate String where
  interpolate = fromString
  {-# INLINE [1] interpolate #-}
instance Interpolate Char where
  interpolate c = fromString [c]
  {-# INLINE [1] interpolate #-}

#define VIA_SHOW(t) \
  instance Interpolate t where \
    interpolate = fromString . show \
    {-# INLINE [1] interpolate #-}

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
