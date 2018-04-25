{-# LANGUAGE CPP #-}
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- /Note:/ You should use "Data.Map.Strict" instead of this module if:
--
-- * You will eventually need all the values stored.
--
-- * The stored values don't represent large virtual data structures
-- to be lazily computed.
--
-- An efficient implementation of ordered maps from keys to values
-- (dictionaries).
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.Map as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--     Journal of Functional Programming 3(4):553-562, October 1993,
--     <http://www.swiss.ai.mit.edu/~adams/BB/>.
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
--  Bounds for 'union', 'intersection', and 'difference' are as given
--  by
--
--    * Guy Blelloch, Daniel Ferizovic, and Yihan Sun,
--      \"/Just Join for Parallel Ordered Sets/\",
--      <https://arxiv.org/abs/1602.02120v3>.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- /Warning/: The size of the map must not exceed @maxBound::Int@. Violation of
-- this condition is not detected and if the size limit is exceeded, its
-- behaviour is undefined.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation (<http://en.wikipedia.org/wiki/Big_O_notation>).
-----------------------------------------------------------------------------

module Data.Map
    ( module Data.Map.Lazy
    , insertWith'
    , insertWithKey'
    , insertLookupWithKey'
    , fold
    , foldWithKey
    ) where

import Prelude hiding (foldr)
import Data.Map.Lazy
import qualified Data.Map.Strict as Strict

-- | /O(log n)/. Same as 'insertWith', but the value being inserted to the map is
-- evaluated to WHNF beforehand.
--
-- For example, to update a counter:
--
-- > insertWith' (+) k 1 m
--
{-# DEPRECATED insertWith' "As of version 0.5, replaced by 'Data.Map.Strict.insertWith'." #-}
insertWith' :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith' = Strict.insertWith
#if __GLASGOW_HASKELL__
{-# INLINABLE insertWith' #-}
#else
{-# INLINE insertWith' #-}
#endif

-- | /O(log n)/. Same as 'insertWithKey', but the value being inserted to the map is
-- evaluated to WHNF beforehand.
{-# DEPRECATED insertWithKey' "As of version 0.5, replaced by 'Data.Map.Strict.insertWithKey'." #-}
insertWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
-- We do not reuse Data.Map.Strict.insertWithKey, because it is stricter -- it
-- forces evaluation of the given value.
insertWithKey' = Strict.insertWithKey
#if __GLASGOW_HASKELL__
{-# INLINABLE insertWithKey' #-}
#else
{-# INLINE insertWithKey' #-}
#endif

-- | /O(log n)/. Same as 'insertLookupWithKey', but the value being inserted to
-- the map is evaluated to WHNF beforehand.
{-# DEPRECATED insertLookupWithKey' "As of version 0.5, replaced by 'Data.Map.Strict.insertLookupWithKey'." #-}
insertLookupWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a
                     -> (Maybe a, Map k a)
-- We do not reuse Data.Map.Strict.insertLookupWithKey, because it is stricter -- it
-- forces evaluation of the given value.
insertLookupWithKey' = Strict.insertLookupWithKey
#if __GLASGOW_HASKELL__
{-# INLINABLE insertLookupWithKey' #-}
#else
{-# INLINE insertLookupWithKey' #-}
#endif

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
{-# DEPRECATED fold "As of version 0.5, replaced by 'foldr'." #-}
fold :: (a -> b -> b) -> b -> Map k a -> b
fold = foldr
{-# INLINE fold #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator. This function is an equivalent of 'foldrWithKey' and is present
-- for compatibility only.
{-# DEPRECATED foldWithKey "As of version 0.4, replaced by 'foldrWithKey'." #-}
foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey = foldrWithKey
{-# INLINE foldWithKey #-}
