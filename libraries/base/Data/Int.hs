{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Int
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: Int.hs,v 1.2 2001/07/03 11:37:49 simonmar Exp $
--
-- Sized Integer types.
--
-----------------------------------------------------------------------------

module Data.Int
	( Int8
	, Int16
	, Int32
	, Int64
	-- instances: Eq, Ord, Num, Bounded, Real, Integral, Ix, Enum, Read,
	-- Show, Bits, CCallable, CReturnable (last two are GHC specific.)
	) where

#ifdef __GLASGOW_HASKELL__
import GHC.Int
#endif

import Data.Dynamic

#include "Dynamic.h"
INSTANCE_TYPEABLE0(Int8,int8Tc, "Int8")
INSTANCE_TYPEABLE0(Int16,int16Tc,"Int16")
INSTANCE_TYPEABLE0(Int32,int32Tc,"Int32")
INSTANCE_TYPEABLE0(Int64,int64Tc,"Int64")
