-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Fix
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Fix monad.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
--
-----------------------------------------------------------------------------

module Control.Monad.Fix (
	MonadFix(
	   mfix	-- :: (a -> m a) -> m a
         ),
	fix	-- :: (a -> a) -> a
  ) where

import Prelude
import System.IO

fix :: (a -> a) -> a
fix f = let x = f x in x

class (Monad m) => MonadFix m where
	mfix :: (a -> m a) -> m a

-- Instances of MonadFix for Prelude monads

-- Maybe:
instance MonadFix Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x

-- List:
instance MonadFix [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (tail . f)

-- IO:
instance MonadFix IO where
    mfix = fixIO 
