{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Bool
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Bool.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
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
