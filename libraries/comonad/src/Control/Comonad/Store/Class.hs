{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Store.Class
-- Copyright   :  (C) 2008-2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Store.Class
  ( ComonadStore(..)
  , lowerPos
  , lowerPeek
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import qualified Control.Comonad.Trans.Store as Store
import Control.Comonad.Trans.Traced
import Control.Comonad.Trans.Identity
#if __GLASGOW_HASKELL__ < 710
import Data.Semigroup
#endif

class Comonad w => ComonadStore s w | w -> s where
  pos :: w a -> s
  peek :: s -> w a -> a

  peeks :: (s -> s) -> w a -> a
  peeks f w = peek (f (pos w)) w

  seek :: s -> w a -> w a
  seek s = peek s . duplicate

  seeks :: (s -> s) -> w a -> w a
  seeks f = peeks f . duplicate

  experiment :: Functor f => (s -> f s) -> w a -> f a
  experiment f w = fmap (`peek` w) (f (pos w))

instance Comonad w => ComonadStore s (Store.StoreT s w) where
  pos = Store.pos
  peek = Store.peek
  peeks = Store.peeks
  seek = Store.seek
  seeks = Store.seeks
  experiment = Store.experiment

lowerPos :: (ComonadTrans t, ComonadStore s w) => t w a -> s
lowerPos = pos . lower
{-# INLINE lowerPos #-}

lowerPeek :: (ComonadTrans t, ComonadStore s w) => s -> t w a -> a
lowerPeek s = peek s . lower
{-# INLINE lowerPeek #-}

lowerExperiment :: (ComonadTrans t, ComonadStore s w, Functor f) => (s -> f s) -> t w a -> f a
lowerExperiment f = experiment f . lower
{-# INLINE lowerExperiment #-}

instance ComonadStore s w => ComonadStore s (IdentityT w) where
  pos = lowerPos
  peek = lowerPeek
  experiment = lowerExperiment

instance ComonadStore s w => ComonadStore s (EnvT e w) where
  pos = lowerPos
  peek = lowerPeek
  experiment = lowerExperiment

instance (ComonadStore s w, Monoid m) => ComonadStore s (TracedT m w) where
  pos = lowerPos
  peek = lowerPeek
  experiment = lowerExperiment
