{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Either
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: Either.hs,v 1.2 2001/07/03 11:37:49 simonmar Exp $
--
-- The Either type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Either (
   Either(..),
   either	-- :: (a -> c) -> (b -> c) -> Either a b -> c
 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Maybe
#endif
