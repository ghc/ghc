-----------------------------------------------------------------------------
-- 
-- Module      :  System.Mem.StableName
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: StableName.hs,v 1.1 2001/06/28 14:15:04 simonmar Exp $
--
-- Giving an object a stable (GC-invariant) name.
--
-----------------------------------------------------------------------------

module System.Mem.StableName
	( StableName {-a-}   -- abstract, instance of Eq
	, makeStableName     -- :: a -> IO (StableName a)
	, hashStableName     -- :: StableName a -> Int
	) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.Base		( Int(..) )
import GHC.IOBase	( IO(..) )
import GHC.Prim		( StableName#, makeStableName#
			, eqStableName#, stableNameToInt# )

-----------------------------------------------------------------------------
-- Stable Names

data StableName a = StableName (StableName# a)

makeStableName  :: a -> IO (StableName a)
#if defined(__PARALLEL_HASKELL__)
makeStableName a = 
  error "makeStableName not implemented in parallel Haskell"
#else
makeStableName a = IO $ \ s ->
    case makeStableName# a s of (# s', sn #) -> (# s', StableName sn #)
#endif

hashStableName :: StableName a -> Int
#if defined(__PARALLEL_HASKELL__)
hashStableName (StableName sn) = 
  error "hashStableName not implemented in parallel Haskell"
#else
hashStableName (StableName sn) = I# (stableNameToInt# sn)
#endif

instance Eq (StableName a) where 
#if defined(__PARALLEL_HASKELL__)
    (StableName sn1) == (StableName sn2) = 
      error "eqStableName not implemented in parallel Haskell"
#else
    (StableName sn1) == (StableName sn2) = 
       case eqStableName# sn1 sn2 of
	 0# -> False
	 _  -> True
#endif

#endif /* __GLASGOW_HASKELL__ */

#include "Dynamic.h"
INSTANCE_TYPEABLE1(StableName,stableNameTc,"StableName")
