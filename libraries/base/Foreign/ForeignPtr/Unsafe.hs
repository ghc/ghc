{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.ForeignPtr.Unsafe
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
-- Unsafe API Only.
--
-----------------------------------------------------------------------------

module Foreign.ForeignPtr.Unsafe (
        -- ** Unsafe low-level operations
        unsafeForeignPtrToPtr,
    ) where

import Foreign.ForeignPtr.Imp

