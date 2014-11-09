{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bool
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'Bool' type and related functions.
--
-----------------------------------------------------------------------------

module Data.Bool (
   -- * Booleans
   Bool(..),
   -- ** Operations
   (&&),
   (||),
   not,
   otherwise,
   bool,
  ) where

import GHC.Base

-- | Case analysis for the 'Bool' type. @'bool' x y p@ evaluates to @x@
-- when @p@ is 'False', and evaluates to @y@ when @p@ is 'True'.
--
-- This is equivalent to @if p then y else x@; that is, one can
-- think of it as an if-then-else construct with its arguments
-- reordered.
--
-- /Since: 4.7.0.0/
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
-- Confirm that @'bool' x y p@ and @if p then y else x@ are
-- equivalent:
--
-- >>> let p = True; x = "bar"; y = "foo"
-- >>> bool x y p == if p then y else x
-- True
-- >>> let p = False
-- >>> bool x y p == if p then y else x
-- True
--
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
