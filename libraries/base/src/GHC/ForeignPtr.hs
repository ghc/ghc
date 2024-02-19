{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.ForeignPtr
-- Copyright   :  (c) The University of Glasgow, 1992-2003
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- GHC's implementation of the 'ForeignPtr' data type.
--

module GHC.ForeignPtr
  (
        -- * Types
        ForeignPtr(..),
        ForeignPtrContents(..),
        Finalizers(..),
        FinalizerPtr,
        FinalizerEnvPtr,
        -- * Create
        newForeignPtr_,
        mallocForeignPtr,
        mallocPlainForeignPtr,
        mallocForeignPtrBytes,
        mallocPlainForeignPtrBytes,
        mallocForeignPtrAlignedBytes,
        mallocPlainForeignPtrAlignedBytes,
        newConcForeignPtr,
        -- * Add Finalizers
        addForeignPtrFinalizer,
        addForeignPtrFinalizerEnv,
        addForeignPtrConcFinalizer,
        -- * Conversion
        unsafeForeignPtrToPtr,
        castForeignPtr,
        plusForeignPtr,
        -- * Control over lifetype
        withForeignPtr,
        unsafeWithForeignPtr,
        touchForeignPtr,
        -- * Finalization
        finalizeForeignPtr
  ) where

import GHC.Internal.ForeignPtr
