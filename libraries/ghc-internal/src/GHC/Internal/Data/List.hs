{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Data.List
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Operations on lists.
--
-----------------------------------------------------------------------------

module GHC.Internal.Data.List
   (
   List
   -- * Basic functions

   , (++)
   , head
   , last
   , tail
   , init
   , uncons
   , unsnoc
   , singleton
   , null
   , length

   -- * List transformations
   , map
   , reverse

   , intersperse
   , intercalate
   , transpose

   , subsequences
   , permutations

   -- * Reducing lists (folds)

   , foldl
   , foldl'
   , foldl1
   , foldl1'
   , foldr
   , foldr1

   -- ** Special folds

   , concat
   , concatMap
   , and
   , or
   , any
   , all
   , sum
   , product
   , maximum
   , minimum

   -- * Building lists

   -- ** Scans
   , scanl
   , scanl'
   , scanl1
   , scanr
   , scanr1

   -- ** Accumulating maps
   , mapAccumL
   , mapAccumR

   -- ** Infinite lists
   , iterate
   , iterate'
   , repeat
   , replicate
   , cycle

   -- ** Unfolding
   , unfoldr

   -- * Sublists

   -- ** Extracting sublists
   , take
   , drop
   , splitAt

   , takeWhile
   , dropWhile
   , dropWhileEnd
   , span
   , break

   , stripPrefix

   , group

   , inits
   , tails

   -- ** Predicates
   , isPrefixOf
   , isSuffixOf
   , isInfixOf
   , isSubsequenceOf

   -- * Searching lists

   -- ** Searching by equality
   , elem
   , notElem
   , lookup

   -- ** Searching with a predicate
   , find
   , filter
   , partition

   -- * Indexing lists
   -- | These functions treat a list @xs@ as an indexed collection,
   -- with indices ranging from 0 to @'length' xs - 1@.

   , (!?)
   , (!!)

   , elemIndex
   , elemIndices

   , findIndex
   , findIndices

   -- * Zipping and unzipping lists

   , zip
   , zip3
   , zip4, zip5, zip6, zip7

   , zipWith
   , zipWith3
   , zipWith4, zipWith5, zipWith6, zipWith7

   , unzip
   , unzip3
   , unzip4, unzip5, unzip6, unzip7

   -- * Special lists

   -- ** Functions on strings
   , lines
   , words
   , unlines
   , unwords

   -- ** \"Set\" operations

   , nub

   , delete
   , (\\)

   , union
   , intersect

   -- ** Ordered lists
   , sort
   , sortOn
   , insert

   -- * Generalized functions

   -- ** The \"@By@\" operations
   -- | By convention, overloaded functions have a non-overloaded
   -- counterpart whose name is suffixed with \`@By@\'.
   --
   -- It is often convenient to use these functions together with
   -- 'GHC.Internal.Data.Function.on', for instance @'sortBy' ('Prelude.compare'
   -- ``GHC.Internal.Data.Function.on`` 'Prelude.fst')@.

   -- *** User-supplied equality (replacing an @Eq@ context)
   -- | The predicate is assumed to define an equivalence.
   , nubBy
   , deleteBy
   , deleteFirstsBy
   , unionBy
   , intersectBy
   , groupBy

   -- *** User-supplied comparison (replacing an @Ord@ context)
   -- | The function is assumed to define a total ordering.
   , sortBy
   , insertBy
   , maximumBy
   , minimumBy

   -- ** The \"@generic@\" operations
   -- | The prefix \`@generic@\' indicates an overloaded function that
   -- is a generalized version of a "Prelude" function.

   , genericLength
   , genericTake
   , genericDrop
   , genericSplitAt
   , genericIndex
   , genericReplicate

   ) where

import GHC.Internal.Data.Foldable
import GHC.Internal.Data.Traversable

import GHC.Internal.Data.OldList hiding
    ( all, and, any, concat, concatMap, elem, find,
      foldl, foldl1, foldl', foldr, foldr1, mapAccumL,
      mapAccumR, maximum, maximumBy, minimum, minimumBy,
      length, notElem, null, or, product, sum )

import GHC.Internal.Base ( Bool(..), Eq((==)), otherwise )
import GHC.Internal.List (List)

-- | The 'isSubsequenceOf' function takes two lists and returns 'True' if all
-- the elements of the first list occur, in order, in the second. The
-- elements do not have to occur consecutively.
--
-- @'isSubsequenceOf' x y@ is equivalent to @x \`'elem'\` ('subsequences' y)@.
--
-- Note: 'isSubsequenceOf' is often used in infix form.
--
-- @since base-4.8.0.0
--
-- ==== __Examples__
--
-- >>> "GHC" `isSubsequenceOf` "The Glorious Haskell Compiler"
-- True
--
-- >>> ['a','d'..'z'] `isSubsequenceOf` ['a'..'z']
-- True
--
-- >>> [1..10] `isSubsequenceOf` [10,9..0]
-- False
--
-- For the result to be 'True', the first list must be finite;
-- for the result to be 'False', the second list must be finite:
--
-- >>> [0,2..10] `isSubsequenceOf` [0..]
-- True
--
-- >>> [0..] `isSubsequenceOf` [0,2..10]
-- False
--
-- >>> [0,2..] `isSubsequenceOf` [0..]
-- * Hangs forever*
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b
