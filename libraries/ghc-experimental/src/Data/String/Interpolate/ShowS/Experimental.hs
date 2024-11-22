{- |
Module      :  Data.String.Interpolate.ShowS.Experimental
Copyright   :  (c) The University of Glasgow 2026
License     :  BSD-style (see the file libraries/base/LICENSE)

Maintainer  :  libraries@haskell.org
Stability   :  stable
Portability :  portable

An interpolator for implementing `showsPrec`

@
{-# LANGUAGE QualifiedStrings #-}
{-# LANGUAGE StringInterpolation #-}

import Data.String.Interpolate.ShowS.Experimental qualified as ShowS

instance Show a => Show (MyTree a) where
  showsPrec d (MyTree l v r) =
    showParen (d > 10) $
      ShowS.s"MyTree ${ShowS.P 11 l} ${v} ${ShowS.P 11 r}"
@
-}
module Data.String.Interpolate.ShowS.Experimental (
  -- * Interpolator
  interpolateRaw,
  interpolateValue,
  interpolateAppend,
  interpolateEmpty,
  interpolateFinalize,

  -- * Specify precedence
  P (..),
) where

import Data.String (IsString (..))

interpolateRaw :: String -> ShowS
interpolateRaw = showString
{-# INLINE [1] interpolateRaw #-}

interpolateValue :: (Show a) => a -> ShowS
interpolateValue = shows
{-# INLINE [1] interpolateValue #-}

interpolateAppend :: ShowS -> ShowS -> ShowS
interpolateAppend = (.)
{-# INLINE [1] interpolateAppend #-}

interpolateEmpty :: ShowS
interpolateEmpty = id
{-# INLINE [1] interpolateEmpty #-}

interpolateFinalize :: ShowS -> ShowS
interpolateFinalize = id
{-# INLINE [1] interpolateFinalize #-}

data P a = P !Int !a
instance Show a => Show (P a) where
  showsPrec _ (P p a) = showsPrec p a
