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
-- The implementation of the writer transformer.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.X.WriterT (
	WriterT,
        runWriter,
        runWriterT,
	execWriterT,
	mapWriterT,
	module T,
	module Monoid,
  ) where

import Prelude(Functor(..),Monad(..),fst,snd,(.))
import Control.Monad(liftM,MonadPlus(..))

import Data.Monoid as Monoid (Monoid(..))

import Control.Monad.X.Trans as T
import Control.Monad.X.Utils
import Control.Monad.X.Types(WriterT(..))


instance (Monoid w) => MonadTrans (WriterT w) where
  lift m        = W (liftM (\a -> (a,mempty)) m)

instance (Monoid w, HasBaseMonad m n) => HasBaseMonad (WriterT w m) n where
  inBase        = inBase'

instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap          = liftM

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return        = return'
  m >>= f       = W (do (a, w)  <- unW m
		        (b, w') <- unW (f a)
		        return (b, w `mappend` w'))
  fail          = fail'

 
runWriter       :: WriterT w m a -> m (a,w)
runWriter       = unW

runWriterT      :: WriterT w m a -> m (a,w)
runWriterT      = unW

execWriterT     :: Monad m => WriterT w m a -> m w
execWriterT m   = liftM snd (unW m)

mapWriterT      :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m  = W (f (unW m))


instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask           = ask'
  local         = local' mapWriterT 

-- different from before, listen produces no output
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  tell w        = W (return ((), w))
  listen        = mapWriterT (liftM (\(a,w) -> ((a,w),mempty))) 

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get           = get'
  put           = put'

instance (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
  throwError    = throwError'
  catchError    = catchError1' W unW

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
  mzero         = mzero'
  mplus         = mplus1' W unW

-- 'findAll' does not produce output
-- if interested in the output use 'listen' before calling 'findAll'.
instance (Monoid w, MonadNondet m) => MonadNondet (WriterT w m) where
  findAll       = mapWriterT (liftM (\xs -> (fmap fst xs, mempty)) . findAll) 
  commit        = mapWriterT commit

instance (Monoid w, MonadResume m) => MonadResume (WriterT w m) where
  delay         = mapWriterT delay
  force         = mapWriterT force

-- jumping undoes the output
instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
  callCC        = callCC1' W unW (\a -> (a,mempty)) 


