{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Safe
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of data types, classes, and functions for interfacing
-- with another programming language.
--
-- Safe API Only.
--
-----------------------------------------------------------------------------

module Foreign.Safe
        ( module Data.Bits
        , module Data.Int
        , module Data.Word
        , module Foreign.Ptr
        , module Foreign.ForeignPtr.Safe
        , module Foreign.StablePtr
        , module Foreign.Storable
        , module Foreign.Marshal.Safe
        ) where

import Data.Bits
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Safe

