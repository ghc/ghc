-----------------------------------------------------------------------------
-- 
-- Module      :  Data.Array.IArray
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- $Id: IArray.hs,v 1.1 2001/06/28 14:15:02 simonmar Exp $
--
-- Overloaded immutable array class.
--
-----------------------------------------------------------------------------

module Data.Array.IArray ( 
    module Data.Ix,

    -- Class of immutable array types
    IArray,     -- :: (* -> * -> *) -> * -> class
    -- Class of array types with immutable bounds
    HasBounds,  -- :: (* -> * -> *) -> class

    array,      -- :: (IArray a e, Ix i) => (i,i) -> [(i, e)] -> a i e
    listArray,  -- :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
    (!),        -- :: (IArray a e, Ix i) => a i e -> i -> e
    bounds,     -- :: (HasBounds a, Ix i) => a i e -> (i,i)
    indices,    -- :: (HasBounds a, Ix i) => a i e -> [i]
    elems,      -- :: (IArray a e, Ix i) => a i e -> [e]
    assocs,     -- :: (IArray a e, Ix i) => a i e -> [(i, e)]
    accumArray, -- :: (IArray a e, Ix i) => (e -> e' -> e) -> e -> (i,i) -> [(i, e')] -> a i e
    (//),       -- :: (IArray a e, Ix i) => a i e -> [(i, e)] -> a i e
    accum,      -- :: (IArray a e, Ix i) => (e -> e' -> e) -> a i e -> [(i, e')] -> a i e
    amap,       -- :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e
    ixmap)      -- :: (IArray a e, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> a i e
    where

import Prelude

import Data.Ix
import Data.Array.Base
