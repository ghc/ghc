-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Monoid
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable ( requires mulit-parameter type classes )
--
-- Declaration of the Monoid class,and instances for list and functions
--
--	  Inspired by the paper
--	  \em{Functional Programming with Overloading and
--	      Higher-Order Polymorphism},
--	    \A[HREF="http://www.cse.ogi.edu/~mpj"]{Mark P Jones},
--		  Advanced School of Functional Programming, 1995.}
-----------------------------------------------------------------------------

module Control.Monad.Monoid (
 	Monoid(..)
  ) where

import Prelude

-- ---------------------------------------------------------------------------
-- The Monoid class

class Monoid a where
	mempty  :: a
	mappend :: a -> a -> a
	mconcat :: [a] -> a

-- Now the default for mconcat.  For most types, this
-- default will be used, but is included in the class definition so
-- that optimized version of mconcat can be provided
-- for specific types.

	mconcat = foldr mappend mempty

-- Monoid instances.

instance Monoid [a] where
	mempty  = []
	mappend = (++)

instance Monoid (a -> a) where
	mempty  = id
	mappend = (.)

instance Monoid () where
	-- Should it be strict?
	mempty        = ()
	_ `mappend` _ = ()
	mconcat _     = ()
