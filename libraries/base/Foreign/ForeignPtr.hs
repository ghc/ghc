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
	, FinalizerPtr
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

import Foreign.Ptr

#ifdef __NHC__
import NHC.FFI
  ( ForeignPtr
  , FinalizerPtr
  , newForeignPtr
  , addForeignPtrFinalizer
  , withForeignPtr
  , foreignPtrToPtr
  , touchForeignPtr
  , castForeignPtr
  )
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
import GHC.ForeignPtr
#endif

#if !defined(__NHC__) && !defined(__GLASGOW_HASKELL__)
import Foreign.Marshal.Alloc	( malloc, mallocBytes, finalizerFree )
import Data.Dynamic

#include "Dynamic.h"
INSTANCE_TYPEABLE1(ForeignPtr,foreignPtrTc,"ForeignPtr")

instance Eq (ForeignPtr a) where 
    p == q  =  foreignPtrToPtr p == foreignPtrToPtr q

instance Ord (ForeignPtr a) where 
    compare p q  =  compare (foreignPtrToPtr p) (foreignPtrToPtr q)

instance Show (ForeignPtr a) where
    showsPrec p f = showsPrec p (foreignPtrToPtr f)
#endif


#ifndef __NHC__
withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
-- ^This is a way to look at the pointer living inside a
-- foreign object.  This function takes a function which is
-- applied to that pointer. The resulting 'IO' action is then
-- executed. The foreign object is kept alive at least during
-- the whole action, even if it is not used directly
-- inside. Note that it is not safe to return the pointer from
-- the action and use it after the action completes. All uses
-- of the pointer should be inside the
-- 'withForeignPtr' bracket.  The reason for
-- this unsafety is the same as for
-- 'foreignPtrToPtr' below: the finalizer
-- may run earlier than expected, because the compiler can only
-- track usage of the 'ForeignPtr' object, not
-- a 'Ptr' object made from it.
--
-- This function is normally used for marshalling data to
-- or from the object pointed to by the
-- 'ForeignPtr', using the operations from the
-- 'Storable' class.
withForeignPtr fo io
  = do r <- io (foreignPtrToPtr fo)
       touchForeignPtr fo
       return r
#endif /* ! __NHC__ */

#ifdef __HUGS__
mallocForeignPtr :: Storable a => IO (ForeignPtr a)
mallocForeignPtr = do
  r <- malloc
  newForeignPtr r finalizerFree

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes n = do
  r <- mallocBytes n
  newForeignPtr r finalizerFree
#endif /* __HUGS__ */

#ifndef __NHC__
mallocForeignPtrArray :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray  = doMalloc undefined
  where
    doMalloc            :: Storable a => a -> Int -> IO (ForeignPtr a)
    doMalloc dummy size  = mallocForeignPtrBytes (size * sizeOf dummy)

mallocForeignPtrArray0      :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray0 size  = mallocForeignPtrArray (size + 1)
#endif
