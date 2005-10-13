-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.ST
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- Mutable boxed and unboxed arrays in the 'Control.Monad.ST.ST' monad.
--
-----------------------------------------------------------------------------

module Data.Array.ST (

   -- * Boxed arrays
   STArray,		-- instance of: Eq, MArray
   runSTArray,

   -- * Unboxed arrays
   STUArray,		-- instance of: Eq, MArray
   runSTUArray,
   castSTUArray,	-- :: STUArray s i a -> ST s (STUArray s i b)

   -- * Overloaded mutable array interface
   module Data.Array.MArray,
 ) where

import Prelude

import Data.Array.MArray
import Data.Array.Base	( STUArray, castSTUArray, UArray, unsafeFreezeSTUArray )
import Control.Monad.ST	( ST, runST )

#ifdef __HUGS__
import Hugs.Array	( Array )
import Hugs.ST		( STArray, unsafeFreezeSTArray )
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Arr		( STArray, Array, unsafeFreezeSTArray )
#endif

-- | A safe way to create and work with a mutable array before returning an
-- immutable array for later perusal.  This function avoids copying
-- the array before returning it - it uses 'unsafeFreeze' internally, but
-- this wrapper is a safe interface to that function.
--
runSTArray :: (Ix i)
	   => (forall s . ST s (STArray s i e))
	   -> Array i e
runSTArray st = runST (st >>= unsafeFreezeSTArray)

-- | A safe way to create and work with an unboxed mutable array before
-- returning an immutable array for later perusal.  This function
-- avoids copying the array before returning it - it uses
-- 'unsafeFreeze' internally, but this wrapper is a safe interface to
-- that function.
--
runSTUArray :: (Ix i)
	   => (forall s . ST s (STUArray s i e))
	   -> UArray i e
runSTUArray st = runST (st >>= unsafeFreezeSTUArray)


-- INTERESTING... this is the type we'd like to give to runSTUArray:
--
-- runSTUArray :: (Ix i, IArray UArray e, 
--	        forall s. MArray (STUArray s) e (ST s))
-- 	   => (forall s . ST s (STUArray s i e))
--	   -> UArray i e
--
-- Note the quantified constraint.  We dodged the problem by using
-- unsafeFreezeSTUArray directly in the defn of runSTUArray above, but
-- this essentially constrains us to a single unsafeFreeze for all STUArrays
-- (in theory we might have a different one for certain element types).
