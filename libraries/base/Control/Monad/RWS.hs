-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.RWS
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Declaration of the MonadRWS class.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.RWS (
	RWS(..),
	evalRWS,
	execRWS,
	mapRWS,
	withRWS,
	RWST(..),
	evalRWST,
	execRWST,
	mapRWST,
	withRWST,
	module Control.Monad.Reader,
	module Control.Monad.Writer,
	module Control.Monad.State,
  ) where

import Prelude

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid

newtype RWS r w s a = RWS { runRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
	fmap f m = RWS $ \r s -> let
		(a, s', w) = runRWS m r s
		in (f a, s', w)

instance (Monoid w) => Monad (RWS r w s) where
	return a = RWS $ \_ s -> (a, s, mempty)
	m >>= k  = RWS $ \r s -> let
		(a, s',  w)  = runRWS m r s
		(b, s'', w') = runRWS (k a) r s'
		in (b, s'', w `mappend` w')

instance (Monoid w) => MonadFix (RWS r w s) where
	mfix f = RWS $ \r s -> let (a, s', w) = runRWS (f a) r s in (a, s', w)

instance (Monoid w) => MonadReader r (RWS r w s) where
	ask       = RWS $ \r s -> (r, s, mempty)
	local f m = RWS $ \r s -> runRWS m (f r) s

instance (Monoid w) => MonadWriter w (RWS r w s) where
	tell   w = RWS $ \_ s -> ((), s, w)
	listen m = RWS $ \r s -> let
		(a, s', w) = runRWS m r s
		in ((a, w), s', w)
	pass   m = RWS $ \r s -> let
		((a, f), s', w) = runRWS m r s
		in (a, s', f w)

instance (Monoid w) => MonadState s (RWS r w s) where
	get   = RWS $ \_ s -> (s, s, mempty)
	put s = RWS $ \_ _ -> ((), s, mempty)


evalRWS :: RWS r w s a -> r -> s -> (a, w)
evalRWS m r s = let
    (a, _, w) = runRWS m r s
    in (a, w)

execRWS :: RWS r w s a -> r -> s -> (s, w)
execRWS m r s = let
    (_, s', w) = runRWS m r s
    in (s', w)

mapRWS :: ((a, s, w) -> (b, s, w')) -> RWS r w s a -> RWS r w' s b
mapRWS f m = RWS $ \r s -> f (runRWS m r s)

withRWS :: (r' -> s -> (r, s)) -> RWS r w s a -> RWS r' w s a
withRWS f m = RWS $ \r s -> uncurry (runRWS m) (f r s)


newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

instance (Monad m) => Functor (RWST r w s m) where
	fmap f m = RWST $ \r s -> do
		(a, s', w) <- runRWST m r s
		return (f a, s', w)

instance (Monoid w, Monad m) => Monad (RWST r w s m) where
	return a = RWST $ \_ s -> return (a, s, mempty)
	m >>= k  = RWST $ \r s -> do
		(a, s', w)  <- runRWST m r s
		(b, s'',w') <- runRWST (k a) r s'
		return (b, s'', w `mappend` w')
	fail msg = RWST $ \_ _ -> fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (RWST r w s m) where
	mzero       = RWST $ \_ _ -> mzero
	m `mplus` n = RWST $ \r s -> runRWST m r s `mplus` runRWST n r s

instance (Monoid w, MonadFix m) => MonadFix (RWST r w s m) where
	mfix f = RWST $ \r s -> mfix $ \ ~(a, _, _) -> runRWST (f a) r s

instance (Monoid w, Monad m) => MonadReader r (RWST r w s m) where
	ask       = RWST $ \r s -> return (r, s, mempty)
	local f m = RWST $ \r s -> runRWST m (f r) s

instance (Monoid w, Monad m) => MonadWriter w (RWST r w s m) where
	tell   w = RWST $ \_ s -> return ((),s,w)
	listen m = RWST $ \r s -> do
		(a, s', w) <- runRWST m r s
		return ((a, w), s', w)
	pass   m = RWST $ \r s -> do
		((a, f), s', w) <- runRWST m r s
		return (a, s', f w)

instance (Monoid w, Monad m) => MonadState s (RWST r w s m) where
	get   = RWST $ \_ s -> return (s, s, mempty)
	put s = RWST $ \_ _ -> return ((), s, mempty)

instance (Monoid w) => MonadTrans (RWST r w s) where
	lift m = RWST $ \_ s -> do
		a <- m
		return (a, s, mempty)

instance (Monoid w, MonadIO m) => MonadIO (RWST r w s m) where
	liftIO = lift . liftIO


evalRWST :: (Monad m) => RWST r w s m a -> r -> s -> m (a, w)
evalRWST m r s = do
    (a, _, w) <- runRWST m r s
    return (a, w)

execRWST :: (Monad m) => RWST r w s m a -> r -> s -> m (s, w)
execRWST m r s = do
    (_, s', w) <- runRWST m r s
    return (s', w)

mapRWST :: (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST f m = RWST $ \r s -> f (runRWST m r s)

withRWST :: (r' -> s -> (r, s)) -> RWST r w s m a -> RWST r' w s m a
withRWST f m = RWST $ \r s -> uncurry (runRWST m) (f r s)
