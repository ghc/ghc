{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Either
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Either type, and associated operations.
--
-----------------------------------------------------------------------------

module Data.Either (
   Either(..),
   either	-- :: (a -> c) -> (b -> c) -> Either a b -> c
 ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#endif

data  Either a b  =  Left a | Right b	deriving (Eq, Ord )

either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
