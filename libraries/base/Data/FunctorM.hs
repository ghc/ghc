-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FunctorM
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- fmapM generalises fmap, just as mapM generalises map.
--
-- NOTE: This module is DEPRECATED.
-- The classes in "Data.Foldable" and "Data.Traversable" provide a
-- more general interface.
--
-----------------------------------------------------------------------------

module Data.FunctorM
{-# DEPRECATED "Use the more general Data.Foldable and Data.Traversable instead" #-}
  (FunctorM(..)) where

import Prelude
import Data.Array

class FunctorM f where
    fmapM  :: Monad m => (a -> m b) -> f a -> m (f b)
    fmapM_ :: Monad m => (a -> m b) -> f a -> m ()

    fmapM_ f t = fmapM f t >> return ()

instance FunctorM [] where
    fmapM  = mapM
    fmapM_ = mapM_

instance FunctorM Maybe where
    fmapM _ Nothing = return Nothing
    fmapM f (Just x) = f x >>= return . Just 

    fmapM_ _ Nothing = return ()
    fmapM_ f (Just x) = f x >> return ()

instance Ix i => FunctorM (Array i) where
    fmapM f a = do 
	a' <- sequence [ f e >>= return . (,) i | (i,e) <- assocs a]
	return (array (bounds a) a')
