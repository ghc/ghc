-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.IArray
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Immutable arrays, with an overloaded interface.  For array types
-- which can be used with this interface, see "Data.Array",
-- "Data.Array.Unboxed", and "Data.Array.Diff".
--
-----------------------------------------------------------------------------

module Data.Array.IArray ( 
    -- * Class of immutable array types
    IArray,     -- :: (* -> * -> *) -> * -> class

    -- * Class of array types with immutable bounds
    HasBounds,  -- :: (* -> * -> *) -> class

    -- * Ordinary boxed\/lazy arrays
    Array,    

    -- * The @Ix@ class and operations
    module Data.Ix,

    -- * Array construction
    array,      -- :: (IArray a e, Ix i) => (i,i) -> [(i, e)] -> a i e
    listArray,  -- :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
    accumArray, -- :: (IArray a e, Ix i) => (e -> e' -> e) -> e -> (i,i) -> [(i, e')] -> a i e

    -- * Indexing arrays
    (!),        -- :: (IArray a e, Ix i) => a i e -> i -> e

    -- * Incremental updates
    (//),       -- :: (IArray a e, Ix i) => a i e -> [(i, e)] -> a i e
    accum,      -- :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(i, e')] -> a i e

    -- * Derived Arrays
    amap,       -- :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e
    ixmap,      -- :: (IArray a e, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> a i e

    -- * Deconstructing arrays
    bounds,     -- :: (HasBounds a, Ix i) => a i e -> (i,i)
    indices,    -- :: (HasBounds a, Ix i) => a i e -> [i]
    elems,      -- :: (IArray a e, Ix i) => a i e -> [e]
    assocs,     -- :: (IArray a e, Ix i) => a i e -> [(i, e)]
 )  where

import Prelude

import Data.Ix
import Data.Array (Array)
import Data.Array.Base
