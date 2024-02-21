{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.Bool
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The 'Bool' type and related functions.
--
-----------------------------------------------------------------------------

module GHC.Internal.Data.Bool (
   -- * Booleans
   Bool(..),
   -- ** Operations
   (&&),
   (||),
   not,
   otherwise,
   bool,
  ) where

import GHC.Internal.Base

-- $setup
-- >>> import Prelude

-- | Case analysis for the 'Bool' type. @'bool' f t p@ evaluates to @f@
-- when @p@ is 'False', and evaluates to @t@ when @p@ is 'True'.
--
-- This is equivalent to @if p then t else f@; that is, one can
-- think of it as an if-then-else construct with its arguments
-- reordered.
--
-- @since base-4.7.0.0
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> bool "foo" "bar" True
-- "bar"
-- >>> bool "foo" "bar" False
-- "foo"
--
-- Confirm that @'bool' f t p@ and @if p then t else f@ are
-- equivalent:
--
-- >>> let p = True; f = "bar"; t = "foo"
-- >>> bool f t p == if p then t else f
-- True
-- >>> let p = False
-- >>> bool f t p == if p then t else f
-- True
--
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
