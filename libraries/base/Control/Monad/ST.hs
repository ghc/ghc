-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires universal quantification for runST)
--
-- The State Transformer Monad, ST
--
-----------------------------------------------------------------------------

module Control.Monad.ST
      (
	ST                  -- abstract, instance of Functor, Monad, Typeable.
      , runST		    -- :: (forall s. ST s a) -> a
      , fixST		    -- :: (a -> ST s a) -> ST s a
      , unsafeInterleaveST  -- :: ST s a -> ST s a

      , unsafeIOToST	    -- :: IO a -> ST s a

      , RealWorld	    -- abstract
      , stToIO		    -- :: ST RealWorld a -> IO a
      ) where

import Prelude

import Control.Monad.Fix
import Data.Dynamic

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
