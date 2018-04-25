{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
-- For non-portable support of Haskell finalizers, see the
-- "Foreign.Concurrent" module.
--
-----------------------------------------------------------------------------

module Foreign.ForeignPtr ( 
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
        , touchForeignPtr
        , castForeignPtr
        , plusForeignPtr

        -- ** Allocating managed memory
        , mallocForeignPtr
        , mallocForeignPtrBytes
        , mallocForeignPtrArray
        , mallocForeignPtrArray0
    ) where

import Foreign.ForeignPtr.Imp

