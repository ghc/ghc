-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The MonadWriter class.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer (
	MonadWriter(..),
	listens,
	censor,
	Writer(..),
	execWriter,
	mapWriter,
	WriterT(..),
	execWriterT,
	mapWriterT,
	module Control.Monad,
	module Control.Monad.Fix,
	module Control.Monad.Trans,
	module Data.Monoid,
  ) where

import Prelude

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Monoid

-- ---------------------------------------------------------------------------
-- MonadWriter class
--
-- tell is like tell on the MUD's it shouts to monad
-- what you want to be heard. The monad carries this 'packet'
-- upwards, merging it if needed (hence the Monoid requirement)}
--
-- listen listens to a monad acting, and returns what the monad "said".
--
-- pass lets you provide a writer transformer which changes internals of
-- the written object.

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
	tell   :: w -> m ()
	listen :: m a -> m (a, w)
	pass   :: m (a, w -> w) -> m a

listens :: (MonadWriter w m) => (w -> b) -> m a -> m (a, b)
listens f m = do
	(a, w) <- listen m
	return (a, f w)

censor :: (MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do
	a <- m
	return (a, f)

-- ---------------------------------------------------------------------------
-- Our parameterizable writer monad

newtype Writer w a = Writer { runWriter :: (a, w) }


instance Functor (Writer w) where
	fmap f m = Writer $ let (a, w) = runWriter m in (f a, w)

instance (Monoid w) => Monad (Writer w) where
	return a = Writer (a, mempty)
	m >>= k  = Writer $ let
		(a, w)  = runWriter m
		(b, w') = runWriter (k a)
		in (b, w `mappend` w')

instance (Monoid w) => MonadFix (Writer w) where
	mfix m = Writer $ let (a, w) = runWriter (m a) in (a, w)

instance (Monoid w) => MonadWriter w (Writer w) where
	tell   w = Writer ((), w)
	listen m = Writer $ let (a, w) = runWriter m in ((a, w), w)
	pass   m = Writer $ let ((a, f), w) = runWriter m in (a, f w)


execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f m = Writer $ f (runWriter m)

-- ---------------------------------------------------------------------------
-- Our parameterizable writer monad, with an inner monad

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }


instance (Monad m) => Functor (WriterT w m) where
	fmap f m = WriterT $ do
		(a, w) <- runWriterT m
		return (f a, w)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
	return a = WriterT $ return (a, mempty)
	m >>= k  = WriterT $ do
		(a, w)  <- runWriterT m
		(b, w') <- runWriterT (k a)
		return (b, w `mappend` w')
	fail msg = WriterT $ fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
	mzero       = WriterT mzero
	m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
	mfix m = WriterT $ mfix $ \ ~(a, _) -> runWriterT (m a)

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
	tell   w = WriterT $ return ((), w)
	listen m = WriterT $ do
		(a, w) <- runWriterT m
		return ((a, w), w)
	pass   m = WriterT $ do
		((a, f), w) <- runWriterT m
		return (a, f w)

instance (Monoid w) => MonadTrans (WriterT w) where
	lift m = WriterT $ do
		a <- m
		return (a, mempty)

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
	liftIO = lift . liftIO

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
	ask       = lift ask
	local f m = WriterT $ local f (runWriterT m)


execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = do
	(_, w) <- runWriterT m
	return w

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)

-- ---------------------------------------------------------------------------
-- MonadWriter instances for other monad transformers

instance (MonadWriter w m) => MonadWriter w (ReaderT r m) where
	tell     = lift . tell
	listen m = ReaderT $ \w -> listen (runReaderT m w)
	pass   m = ReaderT $ \w -> pass   (runReaderT m w)
