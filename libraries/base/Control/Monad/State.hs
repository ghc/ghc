-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable ( mulit-param classes, functional dependencies )
--
-- State monads.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.State (
	MonadState(..),
	modify,
	gets,
	State(..),
	evalState,
	execState,
	mapState,
	withState,
	StateT(..),
	evalStateT,
	execStateT,
	mapStateT,
	withStateT,
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

-- ---------------------------------------------------------------------------
-- MonadState class
--
--  get: returns the state from the internals of the monad.
--  put: changes (replaces) the state inside the monad.

class (Monad m) => MonadState s m | m -> s where
	get :: m s
	put :: s -> m ()

-- Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.}
--
--	  Main> :t modify ((+1) :: Int -> Int)
--	  modify (...) :: (MonadState Int a) => a ()
--
--	This says that modify (+1) acts over any
--	Monad that is a member of the MonadState class,
--	with an Int state.

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
	s <- get
	put (f s)

-- Get part of the state
--
--	gets specific component of the state,
--	using a projection function supplied.
	
gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
	s <- get
	return (f s)

-- ---------------------------------------------------------------------------
-- Our parameterizable state monad

newtype State s a = State { runState :: s -> (a, s) }

-- The State Monad structure is paramterized over just the state.

instance Functor (State s) where
	fmap f m = State $ \s -> let
		(a, s') = runState m s
		in (f a, s')

instance Monad (State s) where
	return a = State $ \s -> (a, s)
	m >>= k  = State $ \s -> let
		(a, s') = runState m s
		in runState (k a) s'

instance MonadFix (State s) where
	mfix f = State $ \s -> let (a, s') = runState (f a) s in (a, s')

instance MonadState s (State s) where
	get   = State $ \s -> (s, s)
	put s = State $ \_ -> ((), s)


evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

withState :: (s -> s) -> State s a -> State s a
withState f m = State $ runState m . f

-- ---------------------------------------------------------------------------
-- Our parameterizable state monad, with an inner monad

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

--The StateT Monad structure is parameterized over two things:
--
--   * s - The state.
--   * m - The inner monad.

-- Here are some examples of use:

-- (Parser from ParseLib with Hugs)
--   type Parser a = StateT String [] a
--      ==> StateT (String -> [(a,String)])
-- For example, item can be written as:
--	item = do (x:xs) <- get
--		  put xs
--		  return x

--   type BoringState s a = StateT s Indentity a
--	==> StateT (s -> Identity (a,s))
--
--   type StateWithIO s a = StateT s IO a
--	==> StateT (s -> IO (a,s))
--
--   type StateWithErr s a = StateT s Maybe a
--	==> StateT (s -> Maybe (a,s))

instance (Monad m) => Functor (StateT s m) where
	fmap f m = StateT $ \s -> do
		(x, s') <- runStateT m s
		return (f x, s')

instance (Monad m) => Monad (StateT s m) where
	return a = StateT $ \s -> return (a, s)
	m >>= k  = StateT $ \s -> do
		(a, s') <- runStateT m s
		runStateT (k a) s'
	fail str = StateT $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (StateT s m) where
	mzero       = StateT $ \_ -> mzero
	m `mplus` n = StateT $ \s -> runStateT m s `mplus` runStateT n s

instance (MonadFix m) => MonadFix (StateT s m) where
	mfix f = StateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s

instance (Monad m) => MonadState s (StateT s m) where
	get   = StateT $ \s -> return (s, s)
	put s = StateT $ \_ -> return ((), s)

instance MonadTrans (StateT s) where
	lift m = StateT $ \s -> do
		a <- m
		return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
	liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (StateT s m) where
	ask       = lift ask
	local f m = StateT $ \s -> local f (runStateT m s)

instance (MonadWriter w m) => MonadWriter w (StateT s m) where
	tell     = lift . tell
	listen m = StateT $ \s -> do
		((a, s'), w) <- listen (runStateT m s)
		return ((a, w), s')
	pass   m = StateT $ \s -> pass $ do
		((a, f), s') <- runStateT m s
		return ((a, s'), f)


evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
	(a, _) <- runStateT m s
	return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
	(_, s') <- runStateT m s
	return s'

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

-- ---------------------------------------------------------------------------
-- MonadState instances for other monad transformers

instance (MonadState s m) => MonadState s (ReaderT r m) where
	get = lift get
	put = lift . put

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
	get = lift get
	put = lift . put
