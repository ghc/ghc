-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Error
-- Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de>, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- The Error monad.
--
-- Rendered by Michael Weber <mailto:michael.weber@post.rwth-aachen.de>,
--	inspired by the Haskell Monad Template Library from
--	Andy Gill (<http://www.cse.ogi.edu/~andy/>)
--
-----------------------------------------------------------------------------

module Control.Monad.Error (
	Error(..),
	MonadError(..),
	ErrorT(..),
	mapErrorT,
	module Control.Monad,
	module Control.Monad.Fix,
	module Control.Monad.Trans,
  ) where

import Prelude

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Cont

import System.IO

-- ---------------------------------------------------------------------------
-- class MonadError
--
--    throws an exception inside the monad and thus interrupts
--    normal execution order, until an error handler is reached}
--
--    catches an exception inside the monad (that was previously
--    thrown by throwError

class Error a where
	noMsg  :: a
	strMsg :: String -> a

	noMsg    = strMsg ""
	strMsg _ = noMsg

instance Error [Char] where
	noMsg  = ""
	strMsg = id

instance Error IOError where
	strMsg = userError

class (Monad m) => MonadError e m | m -> e where
	throwError :: e -> m a
	catchError :: m a -> (e -> m a) -> m a

instance MonadPlus IO where
	mzero       = ioError (userError "mzero")
	m `mplus` n = m `catch` \_ -> n

instance MonadError IOError IO where
	throwError = ioError
	catchError = catch

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

instance Functor (Either e) where
	fmap _ (Left  l) = Left  l
	fmap f (Right r) = Right (f r)

instance (Error e) => Monad (Either e) where
	return        = Right
	Left  l >>= _ = Left l
	Right r >>= k = k r
	fail msg      = Left (strMsg msg)

instance (Error e) => MonadPlus (Either e) where
	mzero            = Left noMsg
	Left _ `mplus` n = n
	m      `mplus` _ = m

instance (Error e) => MonadFix (Either e) where
	mfix f = let
		a = f $ case a of
			Right r -> r
			_       -> error "empty mfix argument"
		in a

instance (Error e) => MonadError e (Either e) where
	throwError             = Left
	Left  l `catchError` h = h l
	Right r `catchError` _ = Right r

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad, with an inner monad

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

-- The ErrorT Monad structure is parameterized over two things:
-- 	* e - The error type.
--	* m - The inner monad.

-- Here are some examples of use:
--
--   type ErrorWithIO e a = ErrorT e IO a
--	==> ErrorT (IO (Either e a))
--
--   type ErrorAndStateWithIO e s a = ErrorT e (StateT s IO) a
--	==> ErrorT (StateT s IO (Either e a))
--	==> ErrorT (StateT (s -> IO (Either e a,s)))
--

instance (Monad m) => Functor (ErrorT e m) where
	fmap f m = ErrorT $ do
		a <- runErrorT m
		case a of
			Left  l -> return (Left  l)
			Right r -> return (Right (f r))

instance (Monad m, Error e) => Monad (ErrorT e m) where
	return a = ErrorT $ return (Right a)
	m >>= k  = ErrorT $ do
		a <- runErrorT m
		case a of
			Left  l -> return (Left l)
			Right r -> runErrorT (k r)
	fail msg = ErrorT $ return (Left (strMsg msg))

instance (Monad m, Error e) => MonadPlus (ErrorT e m) where
	mzero       = ErrorT $ return (Left noMsg)
	m `mplus` n = ErrorT $ do
		a <- runErrorT m
		case a of
			Left  _ -> runErrorT n
			Right r -> return (Right r)

instance (MonadFix m, Error e) => MonadFix (ErrorT e m) where
	mfix f = ErrorT $ mfix $ \a -> runErrorT $ f $ case a of
		Right r -> r
		_       -> error "empty mfix argument"

instance (Monad m, Error e) => MonadError e (ErrorT e m) where
	throwError l     = ErrorT $ return (Left l)
	m `catchError` h = ErrorT $ do
		a <- runErrorT m
		case a of
			Left  l -> runErrorT (h l)
			Right r -> return (Right r)

instance (Error e) => MonadTrans (ErrorT e) where
	lift m = ErrorT $ do
		a <- m
		return (Right a)

instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
	liftIO = lift . liftIO

instance (Error e, MonadReader r m) => MonadReader r (ErrorT e m) where
	ask       = lift ask
	local f m = ErrorT $ local f (runErrorT m)

instance (Error e, MonadWriter w m) => MonadWriter w (ErrorT e m) where
	tell     = lift . tell
	listen m = ErrorT $ do
		(a, w) <- listen (runErrorT m)
		return $ case a of
			Left  l -> Left  l
			Right r -> Right (r, w)
	pass   m = ErrorT $ pass $ do
		a <- runErrorT m
		return $ case a of
			Left  l      -> (Left  l, id)
			Right (r, f) -> (Right r, f)

instance (Error e, MonadState s m) => MonadState s (ErrorT e m) where
	get = lift get
	put = lift . put

instance (Error e, MonadCont m) => MonadCont (ErrorT e m) where
	callCC f = ErrorT $
		callCC $ \c ->
		runErrorT (f (\a -> ErrorT $ c (Right a)))

mapErrorT :: (m (Either e a) -> n (Either e' b)) -> ErrorT e m a -> ErrorT e' n b
mapErrorT f m = ErrorT $ f (runErrorT m)

-- ---------------------------------------------------------------------------
-- MonadError instances for other monad transformers

instance (MonadError e m) => MonadError e (ReaderT r m) where
	throwError       = lift . throwError
	m `catchError` h = ReaderT $ \r -> runReaderT m r
		`catchError` \e -> runReaderT (h e) r

instance (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
	throwError       = lift . throwError
	m `catchError` h = WriterT $ runWriterT m
		`catchError` \e -> runWriterT (h e)

instance (MonadError e m) => MonadError e (StateT s m) where
	throwError       = lift . throwError
	m `catchError` h = StateT $ \s -> runStateT m s
		`catchError` \e -> runStateT (h e) s

instance (Monoid w, MonadError e m) => MonadError e (RWST r w s m) where
	throwError       = lift . throwError
	m `catchError` h = RWST $ \r s -> runRWST m r s
		`catchError` \e -> runRWST (h e) r s
