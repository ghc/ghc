-----------------------------------------------------------------------------
-- |
-- Module      :  Data.STRef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires non-portable module ST)
--
-- Mutable references in the (strict) ST monad.
--
-----------------------------------------------------------------------------

module Data.STRef (
	-- * STRefs
	STRef,		-- abstract, instance Eq
	newSTRef,	-- :: a -> ST s (STRef s a)
	readSTRef,	-- :: STRef s a -> ST s a
	writeSTRef,	-- :: STRef s a -> a -> ST s ()
	modifySTRef	-- :: STRef s a -> (a -> a) -> ST s ()
 ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.ST
import GHC.STRef
#endif

#ifdef __HUGS__
import Hugs.ST
#endif

-- |Mutate the contents of an 'STRef'
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = writeSTRef ref . f =<< readSTRef ref
