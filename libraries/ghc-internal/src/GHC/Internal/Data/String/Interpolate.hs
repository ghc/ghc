-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.String.Interpolate
-- Copyright   :  (c) The University of Glasgow 2024
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The machinery behind -XStringInterpolation
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

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
  Interpolate (..),
) where

import GHC.Internal.Base
import GHC.Internal.Data.Monoid (Endo (..))
import GHC.Internal.Data.String (IsString, fromString)
import GHC.Internal.Show (show)
import GHC.Internal.Types

interpolateRaw :: String -> StringBuilder
interpolateRaw = fromString
{-# INLINE [1] interpolateRaw #-}

interpolateValue :: Interpolate a => a -> StringBuilder
interpolateValue = interpolate
{-# INLINE [1] interpolateValue #-}

interpolateAppend :: StringBuilder -> StringBuilder -> StringBuilder
interpolateAppend = mappend
{-# INLINE [1] interpolateAppend #-}

interpolateEmpty :: StringBuilder
interpolateEmpty = mempty
{-# INLINE [1] interpolateEmpty #-}

interpolateFinalize :: StringBuilder -> String
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

class Interpolate a where
  interpolate :: (IsString s, Monoid s) => a -> s

instance Interpolate String where
  interpolate = fromString
  {-# INLINE [1] interpolate #-}
instance Interpolate Char where
  interpolate c = fromString [c]
  {-# INLINE [1] interpolate #-}
instance Interpolate Int where
  interpolate = fromString . show
  {-# INLINE [1] interpolate #-}
instance Interpolate Double where
  interpolate = fromString . show
  {-# INLINE [1] interpolate #-}
instance Interpolate Bool where
  interpolate = fromString . show
  {-# INLINE [1] interpolate #-}
