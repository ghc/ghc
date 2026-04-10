{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Foreign.StablePtr
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
-----------------------------------------------------------------------------


module GHC.Internal.Foreign.StablePtr
        ( -- * Stable references to Haskell values
          StablePtr          -- abstract
        , newStablePtr
        , deRefStablePtr
        , freeStablePtr
        , castStablePtrToPtr
        , castPtrToStablePtr
        ) where

import GHC.Internal.Stable

