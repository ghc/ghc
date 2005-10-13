-----------------------------------------------------------------------------
-- |
-- Module      :  Data.STRef.Lazy
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Monad.ST.Lazy)
--
-- Mutable references in the lazy ST monad.
--
-----------------------------------------------------------------------------
module Data.STRef.Lazy (
	-- * STRefs
	ST.STRef,	-- abstract, instance Eq
	newSTRef,	-- :: a -> ST s (STRef s a)
	readSTRef,	-- :: STRef s a -> ST s a
	writeSTRef,	-- :: STRef s a -> a -> ST s ()
	modifySTRef	-- :: STRef s a -> (a -> a) -> ST s ()
 ) where

import Control.Monad.ST.Lazy
import qualified Data.STRef as ST

newSTRef    :: a -> ST s (ST.STRef s a)
readSTRef   :: ST.STRef s a -> ST s a
writeSTRef  :: ST.STRef s a -> a -> ST s ()
modifySTRef :: ST.STRef s a -> (a -> a) -> ST s ()

newSTRef   = strictToLazyST . ST.newSTRef
readSTRef  = strictToLazyST . ST.readSTRef
writeSTRef r a = strictToLazyST (ST.writeSTRef r a)
modifySTRef r f = strictToLazyST (ST.modifySTRef r f)
