module Data.String.Interpolate.Basic.Experimental where

import Data.String (IsString (..))

interpolateRaw :: IsString s => String -> s
interpolateRaw = fromString
{-# INLINE [1] interpolateRaw #-}

interpolateValue :: s -> s
interpolateValue = id
{-# INLINE [1] interpolateValue #-}

interpolateAppend :: Monoid s => s -> s -> s
interpolateAppend = mappend
{-# INLINE [1] interpolateAppend #-}

interpolateEmpty :: Monoid s => s
interpolateEmpty = mempty
{-# INLINE [1] interpolateEmpty #-}

interpolateFinalize :: s -> s
interpolateFinalize = id
{-# INLINE [1] interpolateFinalize #-}
