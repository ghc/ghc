{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- 
-- Module      :  Foreign
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Foreign.hs,v 1.2 2001/07/03 11:37:49 simonmar Exp $
--
-- A collection of data types, classes, and functions for interfacing
-- with another programming language. This is only a convenience module
-- in the future, but currently it has the additional task of hiding
-- those entities exported from other modules, which are not part of the
-- FFI proposal.
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
