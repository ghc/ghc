-----------------------------------------------------------------------------
-- 
-- Module      :  Data.STRef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: STRef.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- Mutable references in the ST monad.
--
-----------------------------------------------------------------------------

module Data.STRef (
	STRef,		-- abstract, instance Eq
	newSTRef,	-- :: a -> ST s (STRef s a)
	readSTRef,	-- :: STRef s a -> ST s a
	writeSTRef	-- :: STRef s a -> a -> ST s ()
 ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.STRef
#endif

import Data.Dynamic

#include "Dynamic.h"
INSTANCE_TYPEABLE2(STRef,stRefTc,"STRef")
