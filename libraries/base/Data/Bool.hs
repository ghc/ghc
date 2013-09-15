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

-- | Case analysis for the 'Bool' type.
-- @bool a b p@ evaluates to @a@ when @p@ is @False@, and evaluates to @b@
-- when @p@ is @True@.
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
