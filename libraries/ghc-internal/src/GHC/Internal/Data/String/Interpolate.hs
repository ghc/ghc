{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}

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

module GHC.Internal.Data.String.Interpolate (
  Buildable (..),
  Interpolate (..),

  -- * Built-in builders
  StringBuilder (..),
) where

import GHC.Internal.Base
import GHC.Internal.Data.Monoid (Endo (..))
import GHC.Internal.Show (Show, shows)

-- | @Buildable s@ allows @s@ to be built from an interpolated string.
--
-- Laws:
--   * @fromBuilder . toBuilder === id@
--   * @toBuilder . fromBuilder === id@
class Monoid (Builder s) => Buildable s where
  type Builder s = r | r -> s
  toBuilder :: s -> Builder s
  fromBuilder :: Builder s -> s

-- | @Interpolate a s@ allows a value of type @a@ to be interpolated
-- into a string interpolation of type @s@.
--
-- Laws:
--   * @interpolate \@s \@s = toBuilder@
--   * @interpolate \@(Builder s) \@s = id@
class Buildable s => Interpolate a s where
  interpolate :: a -> Builder s

newtype StringBuilder = StringBuilder (Endo String)
  deriving newtype (Semigroup, Monoid)

instance Buildable String where
  type Builder String = StringBuilder
  toBuilder = toStringBuilder
  fromBuilder = fromStringBuilder

{-# RULES
"fromStringBuilder/toStringBuilder" forall s. fromStringBuilder (toStringBuilder s) = s
"toStringBuilder/fromStringBuilder" forall s. toStringBuilder (fromStringBuilder s) = s
  #-}

toStringBuilder :: String -> StringBuilder
toStringBuilder s = StringBuilder (Endo (s ++))
{-# NOINLINE [2] toStringBuilder #-}

fromStringBuilder :: StringBuilder -> String
fromStringBuilder (StringBuilder (Endo f)) = f ""
{-# NOINLINE [2] fromStringBuilder #-}

instance Interpolate String String where
  interpolate = toBuilder
instance Interpolate StringBuilder String where
  interpolate = id
instance Interpolate Char String where
  interpolate = interpolate . (:[])
instance {-# OVERLAPPABLE #-} Show a => Interpolate a String where
  interpolate = StringBuilder . Endo . shows
