-----------------------------------------------------------------------------
-- 
-- Module      :  Control.Monad.ST
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires universal quantification for runST)
--
-- $Id: ST.hs,v 1.3 2001/07/03 11:37:49 simonmar Exp $
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
import GHC.Prim		( unsafeCoerce#, RealWorld )
import GHC.IOBase 	( IO(..), stToIO )

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
