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
-- The definition of the reader monad transformer.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.X.ReaderT (
	ReaderT,
        runReader,
        runReaderT,
	mapReaderT,
	withReaderT,
	module T,
	) where

import Prelude (Monad(..),Functor(..),const)
import Control.Monad (MonadPlus(..),liftM)

import Control.Monad.X.Trans as T
import Control.Monad.X.Utils
import Control.Monad.X.Types(ReaderT(..))



-- ---------------------------------------------------------------------------
-- Basic instances

instance MonadTrans (ReaderT r) where 
  lift m          = R (\_ -> m) 

instance HasBaseMonad m n => HasBaseMonad (ReaderT r m) n where
  inBase          = inBase'

instance Monad m => Functor (ReaderT r m) where
  fmap            = liftM

instance Monad m => Monad (ReaderT r m) where
  fail            = fail'
  return          = return' 
  m >>= f         = R (\r -> (m $$ r) >>= (\a -> (f a $$ r)))


-- some functions

-- | Remove a reader layer by providing a specific value for the 
-- environment.
runReader         :: r -> ReaderT r m a -> m a
runReader r m     = m $$ r

-- | Same as 'runReader' but with the arguments the other way around.
-- For backwards compatability.
runReaderT        :: ReaderT r m a -> r -> m a
runReaderT        = ($$)

-- | Apply a function to underlying monad.  
-- NOTE: SHOULD THIS BE EXPORTED?
mapReaderT        :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m    = R (\r -> f (m $$ r))

-- | A more general version of 'local' when the reader is the
-- outermost layer.
withReaderT       :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m   = R (\r -> m $$ f r)

-- sugar.
($$)              = unR


-- how the features are implemented for readers

instance (Monad m) => MonadReader r (ReaderT r m) where
  ask             = R return
  local           = withReaderT 

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
  tell            = tell'
  listen          = listen2' R unR (\w a -> (a,w))

instance MonadState s m => MonadState s (ReaderT r m) where
  get             = get'
  put             = put'

instance MonadError e m => MonadError e (ReaderT r m) where
  throwError      = throwError'
  catchError      = catchError2' R unR

instance MonadPlus m => MonadPlus (ReaderT r m) where
  mzero           = mzero'
  mplus           = mplus2' R unR

instance (MonadNondet m) => MonadNondet (ReaderT r m) where
  findAll         = mapReaderT findAll
  commit          = mapReaderT commit 

instance MonadResume m => MonadResume (ReaderT r m) where
  delay           = mapReaderT delay
  force           = mapReaderT force

instance MonadCont m => MonadCont (ReaderT r m) where
  callCC          = callCC2' R unR const 





