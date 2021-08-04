{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#ifdef MIN_VERSION_indexed_traversable
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans.Traced
-- Copyright   :  (C) 2008-2014 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The trace comonad builds up a result by prepending monoidal values to each
-- other.
--
-- This module specifies the traced comonad transformer (aka the cowriter or
-- exponential comonad transformer).
--
----------------------------------------------------------------------------
module Control.Comonad.Trans.Traced
  (
  -- * Traced comonad
    Traced
  , traced
  , runTraced
  -- * Traced comonad transformer
  , TracedT(..)
  -- * Operations
  , trace
  , listen
  , listens
  , censor
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

#if __GLASGOW_HASKELL__ < 707
import Control.Monad.Instances ()
#endif

import Control.Monad (ap)
import Control.Comonad
import Control.Comonad.Hoist.Class
import Control.Comonad.Trans.Class

#ifdef MIN_VERSION_distributive
import Data.Distributive
#endif

#ifdef MIN_VERSION_indexed_traversable
import Data.Functor.WithIndex
#endif

import Data.Functor.Identity

#if __GLASGOW_HASKELL__ < 710
import Data.Semigroup
#endif

import Data.Typeable


type Traced m = TracedT m Identity

traced :: (m -> a) -> Traced m a
traced f = TracedT (Identity f)

runTraced :: Traced m a -> m -> a
runTraced (TracedT (Identity f)) = f

newtype TracedT m w a = TracedT { runTracedT :: w (m -> a) }

instance Functor w => Functor (TracedT m w) where
  fmap g = TracedT . fmap (g .) . runTracedT

instance (ComonadApply w, Monoid m) => ComonadApply (TracedT m w) where
  TracedT wf <@> TracedT wa = TracedT (ap <$> wf <@> wa)

instance Applicative w => Applicative (TracedT m w) where
  pure = TracedT . pure . const
  TracedT wf <*> TracedT wa = TracedT (ap <$> wf <*> wa)

instance (Comonad w, Monoid m) => Comonad (TracedT m w) where
  extend f = TracedT . extend (\wf m -> f (TracedT (fmap (. mappend m) wf))) . runTracedT
  extract (TracedT wf) = extract wf mempty

instance Monoid m => ComonadTrans (TracedT m) where
  lower = fmap ($ mempty) . runTracedT

instance ComonadHoist (TracedT m) where
  cohoist l = TracedT . l . runTracedT

#ifdef MIN_VERSION_distributive
instance Distributive w => Distributive (TracedT m w) where
  distribute = TracedT . fmap (\tma m -> fmap ($ m) tma) . collect runTracedT
#endif

#ifdef MIN_VERSION_indexed_traversable
instance FunctorWithIndex i w => FunctorWithIndex (s, i) (TracedT s w) where
  imap f (TracedT w) = TracedT $ imap (\k' g k -> f (k, k') (g k)) w
  {-# INLINE imap #-}
#endif

trace :: Comonad w => m -> TracedT m w a -> a
trace m (TracedT wf) = extract wf m

listen :: Functor w => TracedT m w a -> TracedT m w (a, m)
listen = TracedT . fmap (\f m -> (f m, m)) . runTracedT

listens :: Functor w => (m -> b) -> TracedT m w a -> TracedT m w (a, b)
listens g = TracedT . fmap (\f m -> (f m, g m)) . runTracedT

censor :: Functor w => (m -> m) -> TracedT m w a -> TracedT m w a
censor g = TracedT . fmap (. g) . runTracedT

#ifdef __GLASGOW_HASKELL__

#if __GLASGOW_HASKELL__ >= 707
deriving instance Typeable TracedT
#else
instance (Typeable s, Typeable1 w) => Typeable1 (TracedT s w) where
  typeOf1 dswa = mkTyConApp tracedTTyCon [typeOf (s dswa), typeOf1 (w dswa)]
    where
      s :: TracedT s w a -> s
      s = undefined
      w :: TracedT s w a -> w a
      w = undefined

tracedTTyCon :: TyCon
#if __GLASGOW_HASKELL__ < 704
tracedTTyCon = mkTyCon "Control.Comonad.Trans.Traced.TracedT"
#else
tracedTTyCon = mkTyCon3 "comonad-transformers" "Control.Comonad.Trans.Traced" "TracedT"
#endif
{-# NOINLINE tracedTTyCon #-}

#endif

#endif
