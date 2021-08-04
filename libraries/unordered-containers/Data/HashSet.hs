{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif

------------------------------------------------------------------------
{-|
Module      :  Data.HashSet
Copyright   :  2011 Bryan O'Sullivan
License     :  BSD-style
Maintainer  :  johan.tibell@gmail.com
Stability   :  provisional
Portability :  portable

= Introduction

'HashSet' allows you to store /unique/ elements, providing efficient insertion,
lookups, and deletion. A 'HashSet' makes no guarantees as to the order of its
elements.

If you are storing sets of "Data.Int"s consider using "Data.IntSet" from the
<https://hackage.haskell.org/package/containers containers> package.


== Examples

All the examples below assume @HashSet@ is imported qualified, and uses the following @dataStructures@ set.

>>> import qualified Data.HashSet as HashSet
>>> let dataStructures = HashSet.fromList ["Set", "Map", "Graph", "Sequence"]

=== Basic Operations

Check membership in a set:

>>> -- Check if "Map" and "Trie" are in the set of data structures.
>>> HashSet.member "Map" dataStructures
True
>>> HashSet.member "Trie" dataStructures
False

Add a new entry to the set:

>>> let moreDataStructures = HashSet.insert "Trie" dataStructures
>>> HashSet.member "Trie" moreDataStructures
> True

Remove the @\"Graph\"@ entry from the set of data structures.

>>> let fewerDataStructures = HashSet.delete "Graph" dataStructures
>>> HashSet.toList fewerDataStructures
["Map","Set","Sequence"]


Create a new set and combine it with our original set.

>>> let unorderedDataStructures = HashSet.fromList ["HashSet", "HashMap"]
>>> HashSet.union dataStructures unorderedDataStructures
fromList ["Map","HashSet","Graph","HashMap","Set","Sequence"]

=== Using custom data with HashSet

To create a @HashSet@ of your custom type, the type must have instances for
'Data.Eq.Eq' and 'Data.Hashable.Hashable'. The @Hashable@ typeclass is defined in the
<https://hackage.haskell.org/package/hashable hashable> package, see the
documentation for information on how to make your type an instance of
@Hashable@.

We'll start by setting up our custom data type:

>>> :set -XDeriveGeneric
>>> import GHC.Generics (Generic)
>>> import Data.Hashable
>>> data Person = Person { name :: String, likesDogs :: Bool } deriving (Show, Eq, Generic)
>>> instance Hashable Person

And now we'll use it!

>>> let people = HashSet.fromList [Person "Lana" True, Person "Joe" False, Person "Simon" True]
>>> HashSet.filter likesDogs people
fromList [Person {name = "Simon", likesDogs = True},Person {name = "Lana", likesDogs = True}]


== Performance

The implementation is based on /hash array mapped tries/.  A
'HashSet' is often faster than other 'Data.Ord.Ord'-based set types,
especially when value comparisons are expensive, as in the case of
strings.

Many operations have a average-case complexity of /O(log n)/.  The
implementation uses a large base (i.e. 16) so in practice these
operations are constant time.
-}

module Data.HashSet
    (
      HashSet

    -- * Construction
    , empty
    , singleton

    -- * Combine
    , union
    , unions

    -- * Basic interface
    , null
    , size
    , member
    , insert
    , delete
    , isSubsetOf

    -- * Transformations
    , map

      -- * Difference and intersection
    , difference
    , intersection

    -- * Folds
    , foldl'
    , foldr

    -- * Filter
    , filter

    -- * Conversions

    -- ** Lists
    , toList
    , fromList

    -- * HashMaps
    , toMap
    , fromMap
    ) where

import Data.HashSet.Internal
import Prelude ()
