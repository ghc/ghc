{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.ForeignPtr.Imp
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

module Foreign.ForeignPtr.Imp
        ( 
        -- * Finalised data pointers
          ForeignPtr
        , FinalizerPtr
        , FinalizerEnvPtr

        -- ** Basic operations
        , newForeignPtr
        , newForeignPtr_
        , addForeignPtrFinalizer
        , newForeignPtrEnv
        , addForeignPtrFinalizerEnv
        , withForeignPtr
        , finalizeForeignPtr

        -- ** Low-level operations
        , unsafeForeignPtrToPtr
        , touchForeignPtr
        , castForeignPtr
        , plusForeignPtr

        -- ** Allocating managed memory
        , mallocForeignPtr
        , mallocForeignPtrBytes
        , mallocForeignPtrArray
        , mallocForeignPtrArray0
        ) 
        where

import Foreign.Ptr
import Foreign.Storable ( Storable(sizeOf) )

import GHC.Base
import GHC.Num
import GHC.ForeignPtr

newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
-- ^Turns a plain memory reference into a foreign pointer, and
-- associates a finalizer with the reference.  The finalizer will be
-- executed after the last reference to the foreign object is dropped.
-- There is no guarantee of promptness, however the finalizer will be
-- executed before the program exits.
newForeignPtr finalizer p
  = do fObj <- newForeignPtr_ p
       addForeignPtrFinalizer finalizer fObj
       return fObj

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
-- this unsafeness is the same as for
-- 'unsafeForeignPtrToPtr' below: the finalizer
-- may run earlier than expected, because the compiler can only
-- track usage of the 'ForeignPtr' object, not
-- a 'Ptr' object made from it.
--
-- This function is normally used for marshalling data to
-- or from the object pointed to by the
-- 'ForeignPtr', using the operations from the
-- 'Storable' class.
withForeignPtr fo io
  = do r <- io (unsafeForeignPtrToPtr fo)
       touchForeignPtr fo
       return r

-- | This variant of 'newForeignPtr' adds a finalizer that expects an
-- environment in addition to the finalized pointer.  The environment
-- that will be passed to the finalizer is fixed by the second argument to
-- 'newForeignPtrEnv'.
newForeignPtrEnv ::
    FinalizerEnvPtr env a -> Ptr env -> Ptr a -> IO (ForeignPtr a)
newForeignPtrEnv finalizer env p
  = do fObj <- newForeignPtr_ p
       addForeignPtrFinalizerEnv finalizer env fObj
       return fObj

-- | This function is similar to 'Foreign.Marshal.Array.mallocArray',
-- but yields a memory area that has a finalizer attached that releases
-- the memory area.  As with 'mallocForeignPtr', it is not guaranteed that
-- the block of memory was allocated by 'Foreign.Marshal.Alloc.malloc'.
mallocForeignPtrArray :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray  = doMalloc undefined
  where
    doMalloc            :: Storable b => b -> Int -> IO (ForeignPtr b)
    doMalloc dummy size  = mallocForeignPtrBytes (size * sizeOf dummy)

-- | This function is similar to 'Foreign.Marshal.Array.mallocArray0',
-- but yields a memory area that has a finalizer attached that releases
-- the memory area.  As with 'mallocForeignPtr', it is not guaranteed that
-- the block of memory was allocated by 'Foreign.Marshal.Alloc.malloc'.
mallocForeignPtrArray0      :: Storable a => Int -> IO (ForeignPtr a)
mallocForeignPtrArray0 size  = mallocForeignPtrArray (size + 1)

