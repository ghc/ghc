{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
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
