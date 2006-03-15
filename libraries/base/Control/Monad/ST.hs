-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires universal quantification for runST)
--
-- This library provides support for /strict/ state threads, as
-- described in the PLDI \'94 paper by John Launchbury and Simon Peyton
-- Jones /Lazy Functional State Threads/.
--
-----------------------------------------------------------------------------

module Control.Monad.ST
  (
	-- * The 'ST' Monad
	ST,		-- abstract, instance of Functor, Monad, Typeable.
	runST,		-- :: (forall s. ST s a) -> a
	fixST,		-- :: (a -> ST s a) -> ST s a

	-- * Converting 'ST' to 'IO'
	RealWorld,		-- abstract
	stToIO,			-- :: ST RealWorld a -> IO a

	-- * Unsafe operations
	unsafeInterleaveST,  	-- :: ST s a -> ST s a
	unsafeIOToST,		-- :: IO a -> ST s a
	unsafeSTToIO		-- :: ST s a -> IO a
      ) where

import Prelude

import Control.Monad.Fix

#include "Typeable.h"

#ifdef __HUGS__
import Data.Typeable
import Hugs.ST
import qualified Hugs.LazyST as LazyST

INSTANCE_TYPEABLE2(ST,sTTc,"ST")
INSTANCE_TYPEABLE0(RealWorld,realWorldTc,"RealWorld")

fixST :: (a -> ST s a) -> ST s a
fixST f = LazyST.lazyToStrictST (LazyST.fixST (LazyST.strictToLazyST . f))

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST =
    LazyST.lazyToStrictST . LazyST.unsafeInterleaveST . LazyST.strictToLazyST
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.ST		( ST, runST, fixST, unsafeInterleaveST )
import GHC.Base		( RealWorld )
import GHC.IOBase 	( stToIO, unsafeIOToST, unsafeSTToIO )
#endif

instance MonadFix (ST s) where
	mfix = fixST

