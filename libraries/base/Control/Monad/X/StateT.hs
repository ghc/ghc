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

module Control.Monad.X.StateT (
	StateT,
        runState,
        runStateS,
        runStateT,
	evalStateT,
	execStateT,
	mapStateT,
	withStateT,
	module T
  ) where

import Prelude (Functor(..),Monad(..),(.),fst)

import Control.Monad
import Control.Monad.X.Trans as T
import Control.Monad.X.Utils
import Control.Monad.X.Types(StateT(..))

instance MonadTrans (StateT s) where
  lift m    = S (\s -> liftM (\a -> (a,s)) m)

instance HasBaseMonad m n => HasBaseMonad (StateT s m) n where
  inBase    = inBase'

instance (Monad m) => Functor (StateT s m) where
  fmap      = liftM

instance (Monad m) => Monad (StateT s m) where
  return    = return'
  m >>= k   = S (\s -> do (a, s') <- m $$ s
		          k a $$ s')
  fail      = fail'


runState      :: Monad m => s -> StateT s m a -> m a
runState s m  = liftM fst (runStateS s m)

runStateS     :: s -> StateT s m a -> m (a,s)
runStateS s m = m $$ s


runStateT   :: StateT s m a -> s -> m (a,s)
runStateT   = ($$)

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
	(a, _) <- m $$ s
	return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
	(_, s') <- m $$ s
	return s'

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = S (f . (m $$))

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = S ((m $$) . f)

($$)          = unS


instance (MonadReader r m) => MonadReader r (StateT s m) where
  ask         = ask'
  local       = local' mapStateT

instance (MonadWriter w m) => MonadWriter w (StateT s m) where
  tell        = tell'
  listen      = listen2' S unS (\w (a,s) -> ((a,w),s)) 

instance (Monad m) => MonadState s (StateT s m) where
  get         = S (\s -> return (s, s))
  put s       = S (\_ -> return ((), s))

instance (MonadError e m) => MonadError e (StateT s m) where
  throwError  = throwError'
  catchError  = catchError2' S ($$)

instance (MonadPlus m) => MonadPlus (StateT s m) where
  mzero       = mzero'
  mplus       = mplus2' S ($$)

-- 'findAll' does not affect the state
-- if interested in the state as well as the result, use 
-- `get` before `findAll`.
-- e.g. findAllSt m = findAll (do x <- m; y <- get; reutrn (x,y))
instance MonadNondet m => MonadNondet (StateT s m) where
  findAll m   = S (\s -> liftM (\xs -> (fmap fst xs,s)) (findAll (m $$ s)))
  commit      = mapStateT commit

instance MonadResume m => MonadResume (StateT s m) where
  delay       = mapStateT delay
  force       = mapStateT force

-- jumping undoes changes to the state state
instance MonadCont m => MonadCont (StateT s m) where
  callCC      = callCC2' S unS (\a s -> (a,s))


