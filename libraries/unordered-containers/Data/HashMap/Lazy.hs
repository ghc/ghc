{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.HashMap.Lazy
-- Copyright   :  2010-2012 Johan Tibell
-- License     :  BSD-style
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- A map from /hashable/ keys to values.  A map cannot contain
-- duplicate keys; each key can map to at most one value.  A 'HashMap'
-- makes no guarantees as to the order of its elements.
--
-- The implementation is based on /hash array mapped tries/.  A
-- 'HashMap' is often faster than other tree-based set types,
-- especially when key comparison is expensive, as in the case of
-- strings.
--
-- Many operations have a average-case complexity of /O(log n)/.  The
-- implementation uses a large base (i.e. 16) so in practice these
-- operations are constant time.
module Data.HashMap.Lazy
    (
      -- * Strictness properties
      -- $strictness

      HashMap

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
    , null
    , size
    , member
    , lookup
    , (!?)
    , findWithDefault
    , lookupDefault
    , (!)
    , insert
    , insertWith
    , delete
    , adjust
    , update
    , alter
    , alterF
    , isSubmapOf
    , isSubmapOfBy

      -- * Combine
      -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions

    -- ** Compose
    , compose

      -- * Transformations
    , map
    , mapWithKey
    , traverseWithKey
    , mapKeys

      -- * Difference and intersection
    , difference
    , differenceWith
    , intersection
    , intersectionWith
    , intersectionWithKey

      -- * Folds
    , foldMapWithKey
    , foldr
    , foldl
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'
    , foldrWithKey
    , foldlWithKey

      -- * Filter
    , filter
    , filterWithKey
    , mapMaybe
    , mapMaybeWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

      -- ** HashSets
    , HS.keysSet
    ) where

import Data.HashMap.Internal as HM
import qualified Data.HashSet.Internal as HS
import Prelude ()

-- $strictness
--
-- This module satisfies the following strictness property:
--
-- * Key arguments are evaluated to WHNF.
