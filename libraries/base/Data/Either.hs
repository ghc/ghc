{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Either
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: Either.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
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
