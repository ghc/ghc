{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Foreign.Storable
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Storable.hs,v 1.2 2001/07/03 11:37:50 simonmar Exp $
--
-- A class for primitive marshaling
--
-----------------------------------------------------------------------------

module Foreign.Storable
	( Storable(
	     sizeOf,         -- :: a -> Int
	     alignment,      -- :: a -> Int
	     peekElemOff,    -- :: Ptr a -> Int      -> IO a
	     pokeElemOff,    -- :: Ptr a -> Int -> a -> IO ()
	     peekByteOff,    -- :: Ptr b -> Int      -> IO a
	     pokeByteOff,    -- :: Ptr b -> Int -> a -> IO ()
	     peek,           -- :: Ptr a             -> IO a
	     poke,           -- :: Ptr a        -> a -> IO ()
	     destruct)       -- :: Ptr a             -> IO ()
	) where

#ifdef __GLASGOW_HASKELL__
import GHC.Storable
#endif
