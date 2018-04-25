{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module A where

import Control.Arrow (first)
import Control.Monad.Fix
import Control.Monad

-- Reduced example from rev-state package.
-- Reproduces an issue where the tuples generated when desugaring
-- mdo have nondeterministic order of components.
--
-- Consider:
--
--    do rec
--      a <- f b
--      b <- f a
--      return a
--
-- Compare:
--
--  do
--    (a, b) <- mfix $ \ ~(a, b) -> do
--      a <- f b
--      b <- f a
--      return (a, b)
--    return a
--
-- vs
--
--  do
--    (b, a) <- mfix $ \ ~(b, a) -> do
--      a <- f b
--      b <- f a
--      return (b, a)
--    return a

newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s) }

instance MonadFix m => Monad (StateT s m) where
  return x = StateT $ \s -> pure (x, s)
  m >>= f = StateT $ \s -> do
    rec
      (x, s'') <- runStateT m s'
      (x', s') <- runStateT (f x) s
    return (x', s'')

instance MonadFix m => Applicative (StateT s m) where
  (<*>) = ap
  pure = return

instance Functor m => Functor (StateT s m) where
  -- this instance is hand-written
  -- so we don't have to rely on m being MonadFix
  fmap f m = StateT $ \s -> first f `fmap` runStateT m s
