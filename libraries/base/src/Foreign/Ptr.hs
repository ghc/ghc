{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.Ptr
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides typed pointers to foreign data.  It is part
-- of the Foreign Function Interface (FFI) and will normally be
-- imported via the "Foreign" module.
--

module Foreign.Ptr
    (-- *  Data pointers
     Ptr,
     nullPtr,
     castPtr,
     plusPtr,
     alignPtr,
     minusPtr,
     -- *  Function pointers
     FunPtr,
     nullFunPtr,
     castFunPtr,
     castFunPtrToPtr,
     castPtrToFunPtr,
     freeHaskellFunPtr,
     -- *  Integral types with lossless conversion to and from pointers
     IntPtr(..),
     ptrToIntPtr,
     intPtrToPtr,
     WordPtr(..),
     ptrToWordPtr,
     wordPtrToPtr
     ) where

import GHC.Internal.Foreign.Ptr