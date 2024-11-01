
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word64Set
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Joachim Breitner 2011
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Int Sets
--
-- The @'Word64Set'@ type represents a set of elements of type @Int@.
--
-- For a walkthrough of the most commonly used functions see their
-- <https://haskell-containers.readthedocs.io/en/latest/set.html sets introduction>.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.Word64Set (Word64Set)
-- >  import qualified Data.Word64Set as Word64Set
--
--
-- == Performance information
--
-- Many operations have a worst-case complexity of \(O(\min(n,W))\).
-- This means that the operation can become linear in the number of
-- elements with a maximum of \(W\) -- the number of bits in an 'Int'
-- (32 or 64).
--
--
-- == Implementation
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced set implementation (see "Data.Set").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve Information Coded In Alphanumeric/\",
--      Journal of the ACM, 15(4), October 1968, pages 514-534.
--
-- Additionally, this implementation places bitmaps in the leaves of the tree.
-- Their size is the natural size of a machine word (32 or 64 bits) and greatly
-- reduces the memory footprint and execution times for dense sets, e.g. sets
-- where it is likely that many values lie close to each other. The asymptotics
-- are not affected by this optimization.
--
-----------------------------------------------------------------------------

module GHC.Data.Word64Set (
            -- * Strictness properties
            -- $strictness

            -- * Set type
              Word64Set          -- instance Eq,Show
            , Key

            -- * Construction
            , empty
            , singleton
            , fromList
            , fromAscList
            , fromDistinctAscList

            -- * Insertion
            , insert

            -- * Deletion
            , delete

            -- * Generalized insertion/deletion
            , alterF

            -- * Query
            , member
            , notMember
            , lookupLT
            , lookupGT
            , lookupLE
            , lookupGE
            , WS.null
            , size
            , isSubsetOf
            , isProperSubsetOf
            , disjoint

            -- * Combine
            , union
            , unions
            , difference
            , (\\)
            , intersection

            -- * Filter
            , WS.filter
            , partition

            , takeWhileAntitone
            , dropWhileAntitone
            , spanAntitone

            , split
            , splitMember
            , splitRoot

            -- * Map
            , WS.map
            , mapMonotonic

            -- * Folds
            , WS.foldr
            , WS.foldl
            -- ** Strict folds
            , foldr'
            , foldl'
            -- ** Legacy folds
            , fold

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView

            -- * Conversion

            -- ** List
            , elems
            , toList
            , toAscList
            , toDescList

            -- * Debugging
            , showTree
            , showTreeWith

            ) where

import GHC.Data.Word64Set.Internal as WS

-- $strictness
--
-- This module satisfies the following strictness property:
--
-- * Key arguments are evaluated to WHNF
--
-- Here are some examples that illustrate the property:
--
-- > delete undefined s  ==  undefined
