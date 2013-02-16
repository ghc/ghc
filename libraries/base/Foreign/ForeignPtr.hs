{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

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

module Foreign.ForeignPtr ( 
        -- * Finalised data pointers
          ForeignPtr
        , FinalizerPtr
#if defined(__HUGS__) || defined(__GLASGOW_HASKELL__)
        , FinalizerEnvPtr
#endif
        -- ** Basic operations
        , newForeignPtr
        , newForeignPtr_
        , addForeignPtrFinalizer
#if defined(__HUGS__) || defined(__GLASGOW_HASKELL__)
        , newForeignPtrEnv
        , addForeignPtrFinalizerEnv
#endif
        , withForeignPtr

#ifdef __GLASGOW_HASKELL__
        , finalizeForeignPtr
#endif

        -- ** Low-level operations
        , touchForeignPtr
        , castForeignPtr

        -- ** Allocating managed memory
        , mallocForeignPtr
        , mallocForeignPtrBytes
        , mallocForeignPtrArray
        , mallocForeignPtrArray0
        -- ** Unsafe low-level operations
        , unsafeForeignPtrToPtr
    ) where

import Foreign.ForeignPtr.Safe

import Foreign.Ptr ( Ptr )
import qualified Foreign.ForeignPtr.Unsafe as U

{-# DEPRECATED unsafeForeignPtrToPtr "Use Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr instead; This function will be removed in the next release" #-} -- deprecated in 7.2
{-# INLINE unsafeForeignPtrToPtr #-}
unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr = U.unsafeForeignPtrToPtr

