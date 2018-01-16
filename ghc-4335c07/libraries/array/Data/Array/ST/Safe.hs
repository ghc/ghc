{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.ST.Safe
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- Mutable boxed and unboxed arrays in the 'Control.Monad.ST.ST' monad.
--
-- Safe API only of "Data.Array.ST".
--
-- @since 0.4.0.0
-----------------------------------------------------------------------------

module Data.Array.ST.Safe (
   -- * Boxed arrays
   STArray,             -- instance of: Eq, MArray
   runSTArray,

   -- * Unboxed arrays
   STUArray,            -- instance of: Eq, MArray
   runSTUArray,

   -- * Overloaded mutable array interface
   module Data.Array.MArray.Safe,
 ) where

import Data.Array.ST
import Data.Array.MArray.Safe

