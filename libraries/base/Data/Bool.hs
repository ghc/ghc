{-# OPTIONS_GHC -fno-implicit-prelude #-}
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
   (&&),	-- :: Bool -> Bool -> Bool
   (||),	-- :: Bool -> Bool -> Bool
   not,		-- :: Bool -> Bool
   otherwise,	-- :: Bool
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#endif

#ifdef __NHC__
import Prelude
import Prelude
  ( Bool(..)
  , (&&)
  , (||)
  , not
  , otherwise
  )
#endif
