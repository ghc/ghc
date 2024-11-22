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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Internal.Data.String.Interpolate (
  SimpleStringInterpolator,
  interpolateString,
  Interpolate (..),
) where

import GHC.Internal.Base
import GHC.Internal.Data.String (IsString, fromString)
import GHC.Internal.Show (ShowS, showChar, showString, shows)

type SimpleStringInterpolator s =
  ( forall ss.
    (forall a. Interpolate a => a -> s)
    -> (s -> s)
    -> (s -> ss -> ss)
    -> ss
    -> ss
  )
  -> s

interpolateString :: (IsString s, Monoid s) => SimpleStringInterpolator s
interpolateString f = mconcat $ f (fromString . interpolate) id (:) []
{-# INLINE interpolateString #-}

class Interpolate a where
  {-# MINIMAL interpolate | interpolateS #-}

  interpolate :: a -> String
  interpolate x = interpolateS x ""

  interpolateS :: a -> ShowS
  interpolateS x s = interpolate x <> s

instance Interpolate String where
  interpolateS = showString
instance Interpolate Char where
  interpolateS = showChar

#define INTERPOLATE_WITH_SHOW(x) \
  instance Interpolate x where \
    interpolateS = shows
INTERPOLATE_WITH_SHOW(Int)
INTERPOLATE_WITH_SHOW(Double)
INTERPOLATE_WITH_SHOW(Bool)
-- FIXME(bchinn): More instances

instance Interpolate a => Interpolate (Maybe a) where
  interpolateS Nothing = showString "Nothing"
  interpolateS (Just a) = showString "Just (" . interpolateS a . showChar ')'
