{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

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
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#endif

