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
-- Module      :  Control.Comonad.Traced.Class
-- Copyright   :  (C) 2008-2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (fundeps, MPTCs)
----------------------------------------------------------------------------
module Control.Comonad.Traced.Class
  ( ComonadTraced(..)
  , traces
  ) where

import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Store
import qualified Control.Comonad.Trans.Traced as Traced
import Control.Comonad.Trans.Identity
#if __GLASGOW_HASKELL__ < 710
import Data.Semigroup
#endif

class Comonad w => ComonadTraced m w | w -> m where
  trace :: m -> w a -> a

traces :: ComonadTraced m w => (a -> m) -> w a -> a
traces f wa = trace (f (extract wa)) wa
{-# INLINE traces #-}

instance (Comonad w, Monoid m) => ComonadTraced m (Traced.TracedT m w) where
  trace = Traced.trace

instance Monoid m => ComonadTraced m ((->) m) where
  trace m f = f m

lowerTrace :: (ComonadTrans t, ComonadTraced m w) => m -> t w a -> a
lowerTrace m = trace m . lower
{-# INLINE lowerTrace #-}

-- All of these require UndecidableInstances because they do not satisfy the coverage condition

instance ComonadTraced m w => ComonadTraced m (IdentityT w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (EnvT e w) where
  trace = lowerTrace

instance ComonadTraced m w => ComonadTraced m (StoreT s w) where
  trace = lowerTrace
