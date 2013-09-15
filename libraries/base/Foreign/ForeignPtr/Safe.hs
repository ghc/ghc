{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.ForeignPtr.Safe
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
-- Safe API Only.
--
-----------------------------------------------------------------------------

module Foreign.ForeignPtr.Safe (
        -- * Finalised data pointers
          ForeignPtr
        , FinalizerPtr
#ifdef __GLASGOW_HASKELL__
        , FinalizerEnvPtr
#endif
        -- ** Basic operations
        , newForeignPtr
        , newForeignPtr_
        , addForeignPtrFinalizer
#ifdef __GLASGOW_HASKELL__
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
    ) where

import Foreign.ForeignPtr.Imp

