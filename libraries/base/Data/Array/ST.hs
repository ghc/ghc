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
-- Mutable boxed and unboxed arrays in the 'ST' monad.
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
import Data.Array.Base	hiding (MArray(..))

#ifdef __GLASGOW_HASKELL__
import GHC.Arr
import GHC.ST

-- | Casts an 'STUArray' with one element type into one with a
-- different element type.  All the elements of the resulting array
-- are undefined (unless you know what you\'re doing...).
castSTUArray :: STUArray s ix a -> ST s (STUArray s ix b)
castSTUArray (STUArray l u marr#) = return (STUArray l u marr#)
#endif
