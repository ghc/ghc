{-# OPTIONS -fno-implicit-prelude #-}
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
        ( module Data.Int
	, module Data.Word
	, module Foreign.Ptr
	, module Foreign.ForeignPtr
	, module Foreign.StablePtr
        , module Foreign.Storable
	, module Foreign.Marshal.Alloc
	, module Foreign.Marshal.Array
	, module Foreign.Marshal.Error
	, module Foreign.Marshal.Utils

	-- For compatibility with the FFI addendum only.  The recommended
	-- place to get this from is System.IO.Unsafe.
	, unsafePerformIO
        ) where

import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Foreign.Marshal.Utils

import System.IO.Unsafe (unsafePerformIO)
