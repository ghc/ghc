-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Unboxed
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.IArray)
--
-- Unboxed immutable arrays.
--
-----------------------------------------------------------------------------

module Data.Array.Unboxed (
   -- * Arrays with unboxed elements
   UArray,

   -- * The overloaded immutable array interface
   module Data.Array.IArray,
 ) where

import Prelude

import Data.Array.IArray
import Data.Array.Base
