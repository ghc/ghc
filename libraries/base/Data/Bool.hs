{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bool
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Bool type and related functions.
--
-----------------------------------------------------------------------------

module Data.Bool (
   Bool(..),
   (&&),	-- :: Bool -> Bool -> Bool
   (||),	-- :: Bool -> Bool -> Bool
   not,		-- :: Bool -> Bool
   otherwise,	-- :: Bool
  ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#endif
