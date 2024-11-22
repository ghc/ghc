{- |
Module      :  Data.String.Interpolate.Default.Experimental
Copyright   :  (c) The University of Glasgow 2026
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  stable
Portability :  portable

The default interpolator for -XStringInterpolation
-}
module Data.String.Interpolate.Default.Experimental (
  interpolateRaw,
  interpolateValue,
  interpolateAppend,
  interpolateEmpty,
  interpolateFinalize,
) where

import GHC.Internal.Data.String.Interpolate
