{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.ForeignPtr
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'ForeignPtr' type and operations.  This module is part of the
-- Foreign Function Interface (FFI) and will usually be imported via
-- the "Foreign" module.
--
-----------------------------------------------------------------------------

module Foreign.ForeignPtr
        ( 
	-- * Finalised data pointers
	  ForeignPtr
        , newForeignPtr
        , addForeignPtrFinalizer
	, withForeignPtr
	, foreignPtrToPtr
	, touchForeignPtr
	, castForeignPtr

#ifndef __NHC__
	, mallocForeignPtr
	, mallocForeignPtrBytes
	, mallocForeignPtrArray
	, mallocForeignPtrArray0
#endif
        ) 
	where

#ifdef __NHC__
import NHC.FFI
  ( ForeignPtr
  , newForeignPtr
  , addForeignPtrFinalizer
  , withForeignPtr
  , foreignPtrToPtr
  , touchForeignPtr
  , castForeignPtr
  )
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.ForeignPtr
#endif

#ifdef __HUGS__
import Hugs.ForeignPtr
#endif

#ifndef __NHC__
import Foreign.Storable	( Storable(sizeOf) )
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.IOBase
import GHC.Num
import GHC.Err		( undefined )
#endif

#ifndef __NHC__
mallocForeignPtrArray :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray  = doMalloc undefined
  where
    doMalloc            :: Storable a => a -> Int -> IO (ForeignPtr a)
    doMalloc dummy size  = mallocForeignPtrBytes (size * sizeOf dummy)

mallocForeignPtrArray0      :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray0 size  = mallocForeignPtrArray (size + 1)
#endif
