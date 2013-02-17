{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign
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
-----------------------------------------------------------------------------

module Foreign
        ( module Data.Bits
        , module Data.Int
        , module Data.Word
        , module Foreign.Ptr
        , module Foreign.ForeignPtr
        , module Foreign.StablePtr
        , module Foreign.Storable
        , module Foreign.Marshal
        ) where

import Data.Bits
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal

