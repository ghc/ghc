-----------------------------------------------------------------------------
-- |
-- Module      :  System.Mem.Weak
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Weak references, weak pairs, weak pointers, and finalizers.
--
-----------------------------------------------------------------------------

module System.Mem.Weak (
	Weak,	    		-- abstract
	-- instance Eq (Weak v)  

	mkWeak,      		-- :: k -> v -> Maybe (IO ()) -> IO (Weak v)
	deRefWeak, 		-- :: Weak v -> IO (Maybe v)
	finalize,		-- :: Weak v -> IO ()
	-- replaceFinaliser	-- :: Weak v -> IO () -> IO ()

	mkWeakPtr, 		-- :: k -> Maybe (IO ()) -> IO (Weak k)
	mkWeakPair, 		-- :: k -> v -> Maybe (IO ()) -> IO (Weak (k,v))
	addFinalizer 		-- :: key -> IO () -> IO ()
   ) where

import Prelude

import Data.Dynamic

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import GHC.Weak

#include "Dynamic.h"
INSTANCE_TYPEABLE1(Weak,weakTc,"Weak")

deRefWeak :: Weak v -> IO (Maybe v)
deRefWeak (Weak w) = IO $ \s ->
   case deRefWeak# w s of
	(# s1, flag, p #) -> case flag of
				0# -> (# s1, Nothing #)
				_  -> (# s1, Just p #)

mkWeakPair :: k -> v -> Maybe (IO ()) -> IO (Weak (k,v))
mkWeakPair key val finalizer = mkWeak key (key,val) finalizer

finalize :: Weak v -> IO ()
finalize (Weak w) = IO $ \s ->
   case finalizeWeak# w s of 
	(# s1, 0#, _ #) -> (# s1, () #)	-- already dead, or no finaliser
	(# s1, _,  f #) -> f s1
#endif

