{- |
Module      :  Data.String.Interpolate.Builder.Experimental
Copyright   :  (c) The University of Glasgow 2026
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  stable
Portability :  portable

The interpolator for InterpolateBuilder
-}
module Data.String.Interpolate.Builder.Experimental (
  interpolateRaw,
  interpolateValue,
  interpolateAppend,
  interpolateEmpty,
  interpolateFinalize,
) where

import Data.String.Interpolate.Class.Experimental (InterpolateBuilder (..))
import Data.String.Interpolate.Class.Experimental qualified as X

interpolateRaw :: String -> InterpolateBuilder
interpolateRaw = X.interpolateRaw
{-# INLINE [1] interpolateRaw #-}

interpolateValue :: (Interpolate a) => a -> InterpolateBuilder
interpolateValue = X.interpolateValue
{-# INLINE [1] interpolateValue #-}

interpolateAppend :: InterpolateBuilder -> InterpolateBuilder -> InterpolateBuilder
interpolateAppend = X.interpolateAppend
{-# INLINE [1] interpolateAppend #-}

interpolateEmpty :: InterpolateBuilder
interpolateEmpty = X.interpolateEmpty
{-# INLINE [1] interpolateEmpty #-}

interpolateFinalize :: (forall s. (IsString s, Monoid s) => s) -> InterpolateBuilder
interpolateFinalize = InterpolateBuilder
{-# INLINE [1] interpolateFinalize #-}
