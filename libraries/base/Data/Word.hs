{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- $Id: Word.hs,v 1.2 2001/07/03 11:37:50 simonmar Exp $
--
-- Sized unsigned integer types.
--
-----------------------------------------------------------------------------

module Data.Word
	( Word
	, Word8
	, Word16
	, Word32
	, Word64
	-- instances: Eq, Ord, Num, Bounded, Real, Integral, Ix, Enum, Read,
	-- Show, Bits, CCallable, CReturnable (last two are GHC specific.)
	) where

#ifdef __GLASGOW_HASKELL__
import GHC.Word
#endif

import Data.Dynamic

#include "Dynamic.h"
INSTANCE_TYPEABLE0(Word8,word8Tc, "Word8" )
INSTANCE_TYPEABLE0(Word16,word16Tc,"Word16")
INSTANCE_TYPEABLE0(Word32,word32Tc,"Word32")
INSTANCE_TYPEABLE0(Word64,word64Tc,"Word64")
