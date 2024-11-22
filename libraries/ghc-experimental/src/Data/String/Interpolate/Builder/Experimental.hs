{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      :  Data.String.Interpolate.Builder.Experimental
Copyright   :  (c) The University of Glasgow 2026
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  stable
Portability :  portable

The interpolator for Interpolation itself.
-}
module Data.String.Interpolate.Builder.Experimental (
  interpolateRaw,
  interpolateValue,
  interpolateAppend,
  interpolateEmpty,
  interpolateFinalize,
) where

import Data.String (IsString (..))
import Data.String.Interpolate.Class.Experimental (Interpolate, Interpolation (..))
import qualified Data.String.Interpolate.Default.Experimental as X

interpolateRaw :: String -> Interpolation
interpolateRaw = X.interpolateRaw
{-# INLINE interpolateRaw #-}

interpolateValue :: (Interpolate a) => a -> Interpolation
interpolateValue = X.interpolateValue
{-# INLINE interpolateValue #-}

interpolateAppend :: Interpolation -> Interpolation -> Interpolation
interpolateAppend = X.interpolateAppend
{-# INLINE interpolateAppend #-}

interpolateEmpty :: Interpolation
interpolateEmpty = X.interpolateEmpty
{-# INLINE interpolateEmpty #-}

interpolateFinalize :: (forall s. (IsString s, Monoid s) => s) -> Interpolation
interpolateFinalize = Interpolation
{-# INLINE interpolateFinalize #-}
