{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if sh_SAFE_DEFAULT
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Unsafe #-}
#endif
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
          module Foreign.ForeignPtr.Safe
#if !sh_SAFE_DEFAULT
        -- ** Unsafe low-level operations
        , unsafeForeignPtrToPtr
#endif
    ) where

import Foreign.ForeignPtr.Safe

#if !sh_SAFE_DEFAULT
import Foreign.Ptr ( Ptr )
import qualified Foreign.ForeignPtr.Unsafe as U

{-# DEPRECATED unsafeForeignPtrToPtr "Use Foreign.ForeignPtr.Unsafe.unsafeForeignPtrToPtr instead; This function will be removed in the next release" #-}
{-# INLINE unsafeForeignPtrToPtr #-}
unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr = U.unsafeForeignPtrToPtr
#endif

