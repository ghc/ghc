{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array 
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Basic non-strict arrays.
--
-- /Note:/ The "Data.Array.IArray" module provides more general interface
-- to immutable arrays: it defines operations with the same names as
-- those defined below, but with more general types, and also defines
-- 'Array' instances of the relevant classes.  To use that more general
-- interface, import "Data.Array.IArray" but not "Data.Array".
-----------------------------------------------------------------------------

module  Data.Array 

    ( 
    -- * Immutable non-strict arrays
    -- $intro
      module Data.Ix		-- export all of Ix 
    , Array 			-- Array type is abstract

    -- * Array construction
    , array	    -- :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
    , listArray     -- :: (Ix a) => (a,a) -> [b] -> Array a b
    , accumArray    -- :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
    -- * Accessing arrays
    , (!)           -- :: (Ix a) => Array a b -> a -> b
    , bounds        -- :: (Ix a) => Array a b -> (a,a)
    , indices       -- :: (Ix a) => Array a b -> [a]
    , elems         -- :: (Ix a) => Array a b -> [b]
    , assocs        -- :: (Ix a) => Array a b -> [(a,b)]
    -- * Incremental array updates
    , (//)          -- :: (Ix a) => Array a b -> [(a,b)] -> Array a b
    , accum         -- :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
    -- * Derived arrays
    , ixmap         -- :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a b

    -- Array instances:
    --
    --   Ix a => Functor (Array a)
    --   (Ix a, Eq b)  => Eq   (Array a b)
    --   (Ix a, Ord b) => Ord  (Array a b)
    --   (Ix a, Show a, Show b) => Show (Array a b)
    --   (Ix a, Read a, Read b) => Read (Array a b)
    -- 

    -- Implementation checked wrt. Haskell 98 lib report, 1/99.

    ) where

import Data.Ix

#ifdef __GLASGOW_HASKELL__
import GHC.Arr		        -- Most of the hard work is done here
import Data.Generics.Basics     -- To provide a Data instance
import Data.Generics.Instances  -- To provide a Data instance
import GHC.Err ( error )        -- Needed for Data instance
#endif

#ifdef __HUGS__
import Hugs.Array
#endif

#ifdef __NHC__
import Array		-- Haskell'98 arrays
#endif

import Data.Typeable

{- $intro
Haskell provides indexable /arrays/, which may be thought of as functions
whose domains are isomorphic to contiguous subsets of the integers.
Functions restricted in this way can be implemented efficiently;
in particular, a programmer may reasonably expect rapid access to
the components.  To ensure the possibility of such an implementation,
arrays are treated as data, not as general functions.

Since most array functions involve the class 'Ix', this module is exported
from "Data.Array" so that modules need not import both "Data.Array" and
"Data.Ix".
-}
