{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Foreign.StablePtr
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module is part of the Foreign Function Interface (FFI) and will usually
-- be imported via the module "Foreign".
--

module Foreign.StablePtr
    (-- *  Stable references to Haskell values
     StablePtr,
     newStablePtr,
     deRefStablePtr,
     freeStablePtr,
     castStablePtrToPtr,
     castPtrToStablePtr,
     -- **  The C-side interface
     -- $cinterface
     ) where

import GHC.Internal.Foreign.StablePtr