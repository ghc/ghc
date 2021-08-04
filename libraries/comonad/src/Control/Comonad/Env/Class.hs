{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Env.Class
-- Copyright   :  (C) 2008-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Env.Class
  ( ComonadEnv(..)
  , asks
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import qualified Control.Comonad.Trans.Env as Env
import Control.Comonad.Trans.Store
import Control.Comonad.Trans.Traced
import Control.Comonad.Trans.Identity
import Data.Semigroup

class Comonad w => ComonadEnv e w | w -> e where
  ask :: w a -> e

asks :: ComonadEnv e w => (e -> e') -> w a -> e'
asks f wa = f (ask wa)
{-# INLINE asks #-}

instance Comonad w => ComonadEnv e (Env.EnvT e w) where
  ask = Env.ask

instance ComonadEnv e ((,)e) where
  ask = fst

instance ComonadEnv e (Arg e) where
  ask (Arg e _) = e

lowerAsk :: (ComonadEnv e w, ComonadTrans t) => t w a -> e
lowerAsk = ask . lower
{-# INLINE lowerAsk #-}

instance ComonadEnv e w => ComonadEnv e (StoreT t w) where
  ask = lowerAsk

instance ComonadEnv e w => ComonadEnv e (IdentityT w) where
  ask = lowerAsk

instance (ComonadEnv e w, Monoid m) => ComonadEnv e (TracedT m w) where
  ask = lowerAsk
