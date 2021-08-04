-- |
-- Module         : Data.ByteString.Search.KnuthMorrisPratt
-- Copyright      : Justin Bailey
--                  Chris Kuklewicz
--                  Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast non-overlapping Knuth-Morris-Pratt search of both strict and
-- lazy 'Data.ByteString.ByteString' values.
--
-- A description of the algorithm can be found at
-- <http://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm>.
--
-- Original authors: Justin Bailey (jgbailey at gmail.com) and
-- Chris Kuklewicz (haskell at list.mightyreason.com).
module Data.ByteString.Search.KnuthMorrisPratt
    {-# DEPRECATED "Use the new interface instead" #-}
    (
      -- * Overview
      -- $overview

      -- ** Changes
      -- $changes

      -- ** Deprecation
      -- $deprecation

      -- ** Parameter and return types
      -- $types

      -- ** Lazy ByteStrings
      -- $lazy

      -- * Partial application
      -- $partial

      -- * Complexity and Performance
      -- $complexity

      -- * Functions
      matchLL
    , matchLS
    , matchSS
    , matchSL
    ) where

import Data.ByteString.Search.Internal.KnuthMorrisPratt
            (matchLL, matchLS, matchSL, matchSS)

-- $overview
--
-- This module exists only for backwards compatibility. Nevertheless
-- there have been small changes in the behaviour of the functions.
-- The module exports four search functions: 'matchLL', 'matchLS',
-- 'matchSL', and 'matchSS'. All of them return the list of all
-- starting positions of non-overlapping occurrences of a pattern
-- in a string.

-- $changes
--
-- Formerly, all four functions returned an empty list when passed
-- an empty pattern. Now, in accordance with the functions from the other
-- modules, @matchXY \"\" target = [0 .. 'length' target]@.
--
-- Further, the return type of 'matchLS' and 'matchSS' has changed to
-- @['Int']@, since strict 'Data.ByteString.ByteString's are 'Int'-indexed.

-- $deprecation
--
-- This module is /deprecated/. You should use the new interface provided
-- in "Data.ByteString.Search.KMP" and "Data.ByteString.Lazy.Search.KMP"
-- or the generally faster functions from "Data.ByteString.Search" and
-- "Data.ByteString.Search.DFA", respectively the lazy versions.

-- $types
--
-- The first parameter is always the pattern string.  The second
-- parameter is always the target string to be searched.  The returned
-- list contains the offsets of all /non-overlapping/ patterns.
--
-- A returned @Int@ or @Int64@ is an index into the target string
-- which is aligned to the head of the pattern string.  Strict targets
-- return @Int@ indices and lazy targets return @Int64@ indices.  All
-- returned lists are computed and returned in a lazy fashion.

-- $lazy
--
-- 'matchLL' and 'matchLS' take lazy bytestrings as patterns.  For
-- performance, if the pattern is not a single strict chunk then all
-- the the pattern chunks will copied into a concatenated strict
-- bytestring.  This limits the patterns to a length of (maxBound ::
-- Int).
--
-- 'matchLL' and 'matchSL' take lazy bytestrings as targets.
-- These are written so that while they work they will not retain a
-- reference to all the earlier parts of the the lazy bytestring.
-- This means the garbage collector would be able to keep only a small
-- amount of the target string and free the rest.

-- $partial
--
-- These functions can all be usefully partially applied. Given only a
-- pattern, the auxiliary data will be computed only once, allowing for
-- efficient re-use.

-- $complexity
--
-- The preprocessing of the pattern is /O/(@patternLength@) in time and space.
-- The time complexity of the searching phase is /O/(@targetLength@) for all
-- functions.
--
-- In most cases, these functions are considerably slower than the
-- Boyer-Moore variants, performance is close to that of those from
-- "Data.ByteString.Search.DFA" resp. "Data.ByteString.Lazy.Search.DFA".

