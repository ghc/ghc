-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.ST
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mutable boxed and unboxed arrays in the ST monad.
--
-----------------------------------------------------------------------------

module Data.Array.ST (
   module Data.Array.MArray,
   STArray,		-- instance of: Eq, MArray
   STUArray,		-- instance of: Eq, MArray
   castSTUArray,	-- :: STUArray s i a -> ST s (STUArray s i b)
 ) where

import Prelude

import Data.Array.MArray
import Data.Array.Base

#ifdef __GLASGOW_HASKELL__
import GHC.Arr
import GHC.ST

castSTUArray :: STUArray s ix a -> ST s (STUArray s ix b)
castSTUArray (STUArray l u marr#) = return (STUArray l u marr#)
#endif
