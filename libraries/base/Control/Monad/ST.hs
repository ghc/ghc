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
-- Jones /Lazy State Threads/.
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
	unsafeIOToST		-- :: IO a -> ST s a
      ) where

import Prelude

import Control.Monad.Fix
import Data.Typeable

#ifdef __HUGS__
import Hugs.ST
import qualified Hugs.LazyST as LazyST

fixST :: (a -> ST s a) -> ST s a
fixST f = LazyST.lazyToStrictST (LazyST.fixST (LazyST.strictToLazyST . f))

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST =
    LazyST.lazyToStrictST . LazyST.unsafeInterleaveST . LazyST.strictToLazyST
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.ST
import GHC.Base		( unsafeCoerce#, RealWorld )
import GHC.IOBase 	( IO(..), stToIO )

-- This relies on IO and ST having the same representation modulo the
-- constraint on the type of the state
--
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s -> (unsafeCoerce# io) s
#endif

instance MonadFix (ST s) where
	mfix = fixST

-- ---------------------------------------------------------------------------
-- Typeable instance

sTTc :: TyCon
sTTc = mkTyCon "ST"

instance (Typeable a, Typeable b) => Typeable (ST a b) where
  typeOf st = mkAppTy sTTc [typeOf ((undefined :: ST a b -> a) st),
			    typeOf ((undefined :: ST a b -> b) st)]
