{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-- |
-- Module      : Data.Primitive
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Reexports all primitive operations
--
module Data.Primitive (
  module Data.Primitive.Types,
  module Data.Primitive.Array,
  module Data.Primitive.ByteArray,
  module Data.Primitive.Addr,

  sizeOf, alignment
) where

import Data.Primitive.Types
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Data.Primitive.Addr
