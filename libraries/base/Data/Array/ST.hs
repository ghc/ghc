-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.ST
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mutable boxed and unboxed arrays in the 'Control.Monad.ST.ST' monad.
--
-----------------------------------------------------------------------------

module Data.Array.ST (

   -- * Boxed arrays
   STArray,		-- instance of: Eq, MArray

   -- * Unboxed arrays
   STUArray,		-- instance of: Eq, MArray
   castSTUArray,	-- :: STUArray s i a -> ST s (STUArray s i b)

   -- * Overloaded mutable array interface
   module Data.Array.MArray,
 ) where

import Prelude

import Data.Array.MArray
import Data.Array.Base	( STUArray, castSTUArray )

#ifdef __HUGS__
import Hugs.ST		( STArray )
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Arr		( STArray )
#endif
