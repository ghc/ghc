{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Control.Monad.Fix
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: Fix.hs,v 1.2 2001/07/03 11:37:49 simonmar Exp $
--
-- The Fix monad.
--
--	  Inspired by the paper:
--	  \em{Functional Programming with Overloading and
--	      Higher-Order Polymorphism},
--	    \A[HREF="http://www.cse.ogi.edu/~mpj"]{Mark P Jones},
--		  Advanced School of Functional Programming, 1995.}
--
-----------------------------------------------------------------------------

module Control.Monad.Fix (
	MonadFix(
	   mfix	-- :: (a -> m a) -> m a
         ),
	fix	-- :: (a -> a) -> a
  ) where

#ifdef __GLASGOW_HASKELL__
-- MonadFix is needed by System.IO, so it is below the Prelude.
import Control.Monad
import GHC.Base
import GHC.Err
import Data.Maybe
#endif

fix :: (a -> a) -> a
fix f = let x = f x in x

class (Monad m) => MonadFix m where
	mfix :: (a -> m a) -> m a

-- Perhaps these should live beside (the ST & IO) definition.
instance MonadFix Maybe where
	mfix f = let
		a = f $ case a of
			Just x -> x
			_      -> error "empty mfix argument"
		in a
