-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- State monads.
--
--	  This module is inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
--
-- See below for examples.

-----------------------------------------------------------------------------

module Control.Monad.State (
	-- * MonadState class
	MonadState(..),
	modify,
	gets,
	-- * The State Monad
	State(..),
	evalState,
	execState,
	mapState,
	withState,
	-- * The StateT Monad
	StateT(..),
	evalStateT,
	execStateT,
	mapStateT,
	withStateT,
	module Control.Monad,
	module Control.Monad.Fix,
	module Control.Monad.Trans,
	-- * Examples
	-- $examples
  ) where

import Prelude

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer

-- ---------------------------------------------------------------------------
-- | /get/ returns the state from the internals of the monad.
--
-- /put/ replaces the state inside the monad.

class (Monad m) => MonadState s m | m -> s where
	get :: m s
	put :: s -> m ()

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >	  Main> :t modify ((+1) :: Int -> Int)
-- >	  modify (...) :: (MonadState Int a) => a ()
--
--	This says that @modify (+1)@ acts over any
--	Monad that is a member of the @MonadState@ class,
--	with an @Int@ state.

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
	s <- get
	put (f s)

-- | Gets specific component of the state, using a projection function
-- supplied.

gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
	s <- get
	return (f s)

-- ---------------------------------------------------------------------------
-- | A parameterizable state monad where /s/ is the type of the state
-- to carry and /a/ is the type of the /return value/.

newtype State s a = State { runState :: s -> (a, s) }

-- The State Monad structure is parameterized over just the state.

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

-- |Evaluate this state monad with the given initial state,throwing
-- away the final state.  Very much like @fst@ composed with
-- @runstate@.

evalState :: State s a -- ^The state to evaluate
	  -> s         -- ^An initial value
	  -> a         -- ^The return value of the state application
evalState m s = fst (runState m s)

-- |Execute this state and return the new state, throwing away the
-- return value.  Very much like @snd@ composed with
-- @runstate@.

execState :: State s a -- ^The state to evaluate
	  -> s         -- ^An initial value
	  -> s         -- ^The new state
execState m s = snd (runState m s)

-- |Map a stateful computation from one (return value, state) pair to
-- another.  For instance, to convert numberTree from a function that
-- returns a tree to a function that returns the sum of the numbered
-- tree (see the Examples section for numberTree and sumTree) you may
-- write:
--
-- > sumNumberedTree :: (Eq a) => Tree a -> State (Table a) Int
-- > sumNumberedTree = mapState (\ (t, tab) -> (sumTree t, tab))  . numberTree

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

-- |Apply this function to this state and return the resulting state.
withState :: (s -> s) -> State s a -> State s a
withState f m = State $ runState m . f

-- ---------------------------------------------------------------------------
-- | A parameterizable state monad for encapsulating an inner
-- monad.
--
-- The StateT Monad structure is parameterized over two things:
--
--   * s - The state.
--
--   * m - The inner monad.
--
-- Here are some examples of use:
--
-- (Parser from ParseLib with Hugs)
--
-- >  type Parser a = StateT String [] a
-- >     ==> StateT (String -> [(a,String)])
--
-- For example, item can be written as:
--
-- >   item = do (x:xs) <- get
-- >          put xs
-- >          return x
-- >
-- >   type BoringState s a = StateT s Indentity a
-- >        ==> StateT (s -> Identity (a,s))
-- >
-- >   type StateWithIO s a = StateT s IO a
-- >        ==> StateT (s -> IO (a,s))
-- >
-- >   type StateWithErr s a = StateT s Maybe a
-- >        ==> StateT (s -> Maybe (a,s))

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

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

-- |Similar to 'evalState'
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
	(a, _) <- runStateT m s
	return a

-- |Similar to 'execState'
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
	(_, s') <- runStateT m s
	return s'

-- |Similar to 'mapState'
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

-- |Similar to 'withState'
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

-- ---------------------------------------------------------------------------
-- $examples
-- A function to increment a counter.  Taken from the paper
-- /Generalising Monads to Arrows/, John
-- Hughes (<http://www.math.chalmers.se/~rjmh/>), November 1998:
--
-- > tick :: State Int Int
-- > tick = do n <- get
-- >           put (n+1)
-- >           return n
--
-- Add one to the given number using the state monad:
--
-- > plusOne :: Int -> Int
-- > plusOne n = execState tick n
--
-- A contrived addition example. Works only with positive numbers:
--
-- > plus :: Int -> Int -> Int
-- > plus n x = execState (sequence $ replicate n tick) x
--
-- An example from /The Craft of Functional Programming/, Simon
-- Thompson (<http://www.cs.kent.ac.uk/people/staff/sjt/>),
-- Addison-Wesley 1999: \"Given an arbitrary tree, transform it to a
-- tree of integers in which the original elements are replaced by
-- natural numbers, starting from 0.  The same element has to be
-- replaced by the same number at every occurrence, and when we meet
-- an as-yet-unvisited element we have to find a 'new' number to match
-- it with:\"
--
-- > data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
-- > type Table a = [a]
--
-- > numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
-- > numberTree Nil = return Nil
-- > numberTree (Node x t1 t2) 
-- >        =  do num <- numberNode x
-- >              nt1 <- numberTree t1
-- >              nt2 <- numberTree t2
-- >              return (Node num nt1 nt2)
-- >     where 
-- >     numberNode :: Eq a => a -> State (Table a) Int
-- >     numberNode x
-- >        = do table <- get
-- >             (newTable, newPos) <- return (nNode x table)
-- >             put newTable
-- >             return newPos
-- >     nNode::  (Eq a) => a -> Table a -> (Table a, Int)
-- >     nNode x table
-- >        = case (findIndexInList (== x) table) of
-- >          Nothing -> (table ++ [x], length table)
-- >          Just i  -> (table, i)
-- >     findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
-- >     findIndexInList = findIndexInListHelp 0
-- >     findIndexInListHelp _ _ [] = Nothing
-- >     findIndexInListHelp count f (h:t)
-- >        = if (f h)
-- >          then Just count
-- >          else findIndexInListHelp (count+1) f t
--
-- numTree applies numberTree with an initial state:
--
-- > numTree :: (Eq a) => Tree a -> Tree Int
-- > numTree t = evalState (numberTree t) []
--
-- > testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
-- > numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil
--
-- sumTree is a little helper function that does not use the State monad:
--
-- > sumTree :: (Num a) => Tree a -> a
-- > sumTree Nil = 0
-- > sumTree (Node e t1 t2) = e + (sumTree t1) + (sumTree t2)
