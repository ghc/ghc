-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Cont
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Continuation monads.
--
-----------------------------------------------------------------------------

module Control.Monad.Cont (
	MonadCont(..),
	Cont(..),
	mapCont,
	withCont,
	ContT(..),
	mapContT,
	withContT,
	module Control.Monad,
	module Control.Monad.Trans,
  ) where

import Prelude

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS

class (Monad m) => MonadCont m where
	callCC :: ((a -> m b) -> m a) -> m a

-- ---------------------------------------------------------------------------
-- Our parameterizable continuation monad

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
	fmap f m = Cont $ \c -> runCont m (c . f)

instance Monad (Cont r) where
	return a = Cont ($ a)
	m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c

instance MonadCont (Cont r) where
	callCC f = Cont $ \c -> runCont (f (\a -> Cont $ \_ -> c a)) c

mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f m = Cont $ f . runCont m

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f m = Cont $ runCont m . f

-- ---------------------------------------------------------------------------
-- Our parameterizable continuation monad, with an inner monad

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance (Monad m) => Functor (ContT r m) where
	fmap f m = ContT $ \c -> runContT m (c . f)

instance (Monad m) => Monad (ContT r m) where
	return a = ContT ($ a)
	m >>= k  = ContT $ \c -> runContT m (\a -> runContT (k a) c)

instance (Monad m) => MonadCont (ContT r m) where
	callCC f = ContT $ \c -> runContT (f (\a -> ContT $ \_ -> c a)) c

instance MonadTrans (ContT r) where
	lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
	liftIO = lift . liftIO

instance (MonadReader r' m) => MonadReader r' (ContT r m) where
	ask       = lift ask
	local f m = ContT $ \c -> do
		r <- ask
		local f (runContT m (local (const r) . c))

instance (MonadState s m) => MonadState s (ContT r m) where
	get = lift get
	put = lift . put

-- -----------------------------------------------------------------------------
-- MonadCont instances for other monad transformers

instance (MonadCont m) => MonadCont (ReaderT r m) where
	callCC f = ReaderT $ \r ->
		callCC $ \c ->
		runReaderT (f (\a -> ReaderT $ \_ -> c a)) r

instance (MonadCont m) => MonadCont (StateT s m) where
	callCC f = StateT $ \s ->
		callCC $ \c ->
		runStateT (f (\a -> StateT $ \s' -> c (a, s'))) s

instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
	callCC f = WriterT $
		callCC $ \c ->
		runWriterT (f (\a -> WriterT $ c (a, mempty)))

instance (Monoid w, MonadCont m) => MonadCont (RWST r w s m) where
	callCC f = RWST $ \r s ->
		callCC $ \c ->
		runRWST (f (\a -> RWST $ \_ s' -> c (a, s', mempty))) r s

mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f
