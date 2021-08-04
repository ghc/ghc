-- |
-- Module         : Data.ByteString.Search.BoyerMoore
-- Copyright      : Daniel Fischer
--                  Chris Kuklewicz
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast overlapping Boyer-Moore search of both strict and lazy
-- 'ByteString' values.
--
-- Descriptions of the algorithm can be found at
-- <http://www-igm.univ-mlv.fr/~lecroq/string/node14.html#SECTION00140>
-- and
-- <http://en.wikipedia.org/wiki/Boyer-Moore_string_search_algorithm>
--
-- Original authors: Daniel Fischer (daniel.is.fischer at googlemail.com) and
-- Chris Kuklewicz (haskell at list.mightyreason.com).
module Data.ByteString.Search.BoyerMoore
    {-# DEPRECATED "Use the new interface instead" #-} (
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

                                         -- ** Performance
                                         -- $performance

                                         -- ** Complexity
                                         -- $complexity

                                         -- ** Partial application
                                         -- $currying

                                         -- ** Integer overflow
                                         -- $overflow

                                         -- * Functions
                                           matchLL
                                         , matchLS
                                         , matchSL
                                         , matchSS
                                         ) where

import Data.ByteString.Search.Internal.BoyerMoore
            (matchLS, matchSS)
import Data.ByteString.Lazy.Search.Internal.BoyerMoore
            (matchLL, matchSL)

-- $overview
--
-- This module exists only for backwards compatibility. Nevertheless
-- there have been small changes in the behaviour of the functions.
-- The module exports four search functions: 'matchLL', 'matchLS',
-- 'matchSL', and 'matchSS'. All of them return the list of all
-- starting positions of possibly overlapping occurrences of a pattern
-- in a string.

-- $changes
--
-- Formerly, all four functions returned an empty list when passed
-- an empty pattern. Now, in accordance with the functions from the other
-- modules, @matchXY \"\" target = [0 .. 'length' target]@.

-- $deprecation
--
-- This module is /deprecated/. You should use the new interface provided
-- in "Data.ByteString.Search" resp. "Data.ByteString.Lazy.Search".

-- $types
--
-- The first parameter is always the pattern string.  The second
-- parameter is always the target string to be searched.  The returned
-- list contains the offsets of all /overlapping/ patterns.
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

-- $currying
--
-- These functions can all be usefully partially applied.
-- Given only a pattern the partially applied version will compute
-- the supporting lookup tables only once, allowing for efficient re-use.
-- Similarly, the partially applied 'matchLL' and 'matchLS' will compute
-- the concatenated pattern only once.

-- $performance
--
-- In general, the Boyer-Moore algorithm is the most efficient method to
-- search for a pattern inside a string. The advantage over other algorithms
-- (e.g. Na&#239;ve, Knuth-Morris-Pratt, Horspool, Sunday) can be made
-- arbitrarily large for specially selected patterns and targets, but
-- usually, it's a factor of 2&#8211;3 versus Knuth-Morris-Pratt and of
-- 6&#8211;10 versus the na&#239;ve algorithm. The Horspool and Sunday
-- algorithms, which are simplified variants of the Boyer-Moore algorithm,
-- typically have performance between Boyer-Moore and Knuth-Morris-Pratt,
-- mostly closer to Boyer-Moore. The advantage of the Boyer-moore variants
-- over other algorithms generally becomes larger for longer patterns. For
-- very short patterns (or patterns with a very short period), other
-- algorithms, e.g. "Data.ByteString.Search.DFA" can be faster (my
-- tests suggest that \"very short\" means two, maybe three bytes).
--
-- In general, searching in a strict 'S.ByteString' is slightly faster
-- than searching in a lazy 'L.ByteString', but for long targets the
-- smaller memory footprint of lazy 'L.ByteStrings' can make searching
-- those (sometimes much) faster. On the other hand, there are cases
-- where searching in a strict target is much faster, even for long targets.
--
-- On 32-bit systems, 'Int'-arithmetic is much faster than 'Int64'-arithmetic,
-- so when there are many matches, that can make a significant difference.
--
-- Also, the modification to ameliorate the case of periodic patterns
-- is defeated by chunk-boundaries, so long patterns with a short period
-- and many matches exhibit poor behaviour (consider using @indices@ from
-- "Data.ByteString.Lazy.Search.DFA" or "Data.ByteString.Lazy.Search.KMP"
-- in those cases, the former for medium-length patterns, the latter for
-- long patterns; only 'matchLL' and 'matchSL' suffer from
-- this problem, though).

-- $complexity
--
-- Preprocessing the pattern string is O(@patternLength@).  The search
-- performance is O(@targetLength@\/@patternLength@) in the best case,
-- allowing it to go faster than a Knuth-Morris-Pratt algorithm.  With
-- a non-periodic pattern the worst case uses O(3\*@targetLength@)
-- comparisons.  The periodic pattern worst case is quadratic
-- O(@targetLength@\*@patternLength@) complexity for the original
-- Boyer-Moore algorithm.
--
-- The searching functions in this module contain a modification which
-- drastically improves the performance for periodic patterns.
-- I believe that for strict target strings, the worst case is now
-- /O/(@targetLength@) also for periodic patterns and for lazy target
-- strings, my semi-educated guess is
-- /O/(@targetLength@ * (1 + @patternLength@ \/ @chunkSize@)).

-- $overflow
--
-- The current code uses @Int@ to keep track of the locations in the
-- target string.  If the length of the pattern plus the length of any
-- strict chunk of the target string is greater or equal to
-- @'maxBound'::Int@ then this will overflow causing an error.  We try
-- to detect this and call 'error' before a segfault occurs.
