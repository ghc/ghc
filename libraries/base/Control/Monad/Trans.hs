-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The MonadTrans class.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Trans (
	MonadTrans(..),
	MonadIO(..),  
  ) where

import Prelude

import System.IO

-- ---------------------------------------------------------------------------
-- MonadTrans class
--
-- Monad to facilitate stackable Monads.
-- Provides a way of digging into an outer
-- monad, giving access to (lifting) the inner monad.

class MonadTrans t where
	lift :: Monad m => m a -> t m a

class (Monad m) => MonadIO m where
	liftIO :: IO a -> m a

instance MonadIO IO where
	liftIO = id
