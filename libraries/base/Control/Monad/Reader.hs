-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Reader
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Declaration of the Monoid class,and instances for list and functions
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Reader (
	MonadReader(..),
	asks,
	Reader(..),
	mapReader,
	withReader,
	ReaderT(..),
	mapReaderT,
	withReaderT,
	module Control.Monad,
	module Control.Monad.Fix,
	module Control.Monad.Trans,
	) where

import Prelude

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans

-- ----------------------------------------------------------------------------
-- class MonadReader
--  asks for the internal (non-mutable) state.

class (Monad m) => MonadReader r m | m -> r where
	ask   :: m r
	local :: (r -> r) -> m a -> m a

-- This allows you to provide a projection function.

asks :: (MonadReader r m) => (r -> a) -> m a
asks f = do
	r <- ask
	return (f r)

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance Functor ((->) r) where
	fmap = (.)

instance Monad ((->) r) where
	return  = const
	m >>= k = \r -> k (m r) r

instance MonadFix ((->) r) where
	mfix f = \r -> let a = f a r in a

instance MonadReader r ((->) r) where
	ask       = id
	local f m = m . f

-- ---------------------------------------------------------------------------
-- Our parameterizable reader monad

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
	fmap f m = Reader $ \r -> f (runReader m r)

instance Monad (Reader r) where
	return a = Reader $ \_ -> a
	m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

instance MonadFix (Reader r) where
	mfix f = Reader $ \r -> let a = runReader (f a) r in a

instance MonadReader r (Reader r) where
	ask       = Reader id
	local f m = Reader $ runReader m . f

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f m = Reader $ f . runReader m

-- This is a more general version of local.

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

-- ---------------------------------------------------------------------------
-- Our parameterizable reader monad, with an inner monad

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Monad m) => Functor (ReaderT r m) where
	fmap f m = ReaderT $ \r -> do
		a <- runReaderT m r
		return (f a)

instance (Monad m) => Monad (ReaderT r m) where
	return a = ReaderT $ \_ -> return a
	m >>= k  = ReaderT $ \r -> do
		a <- runReaderT m r
		runReaderT (k a) r
	fail msg = ReaderT $ \_ -> fail msg

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
	mzero       = ReaderT $ \_ -> mzero
	m `mplus` n = ReaderT $ \r -> runReaderT m r `mplus` runReaderT n r

instance (MonadFix m) => MonadFix (ReaderT r m) where
	mfix f = ReaderT $ \r -> mfix $ \a -> runReaderT (f a) r

instance (Monad m) => MonadReader r (ReaderT r m) where
	ask       = ReaderT return
	local f m = ReaderT $ \r -> runReaderT m (f r)

instance MonadTrans (ReaderT r) where
	lift m = ReaderT $ \_ -> m

instance (MonadIO m) => MonadIO (ReaderT r m) where
	liftIO = lift . liftIO

mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f
