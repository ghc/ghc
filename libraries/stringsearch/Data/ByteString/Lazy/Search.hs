-- |
-- Module         : Data.ByteString.Lazy.Search
-- Copyright      : Daniel Fischer
--                  Chris Kuklewicz
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast overlapping Boyer-Moore search of lazy
-- 'L.ByteString' values. Breaking, splitting and replacing
-- using the Boyer-Moore algorithm.
--
-- Descriptions of the algorithm can be found at
-- <http://www-igm.univ-mlv.fr/~lecroq/string/node14.html#SECTION00140>
-- and
-- <http://en.wikipedia.org/wiki/Boyer-Moore_string_search_algorithm>
--
-- Original authors: Daniel Fischer (daniel.is.fischer at googlemail.com) and
-- Chris Kuklewicz (haskell at list.mightyreason.com).
module Data.ByteString.Lazy.Search( -- * Overview
                                    -- $overview

                                    -- ** Performance
                                    -- $performance

                                    -- ** Caution
                                    -- $caution

                                    -- ** Complexity
                                    -- $complexity

                                    -- ** Partial application
                                    -- $partial

                                    -- ** Integer overflow
                                    -- $overflow

                                    -- * Finding substrings
                                    indices
                                  , nonOverlappingIndices
                                    -- * Breaking on substrings
                                  , breakOn
                                  , breakAfter
                                  , breakFindAfter
                                    -- * Replacing
                                  , replace
                                    -- * Splitting
                                  , split
                                  , splitKeepEnd
                                  , splitKeepFront
                                    -- * Convenience
                                  , strictify
                                  ) where

import qualified Data.ByteString.Lazy.Search.Internal.BoyerMoore as BM
import Data.ByteString.Search.Substitution
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Int (Int64)

-- $overview
--
-- This module provides functions related to searching a substring within
-- a string, using the Boyer-Moore algorithm with minor modifications
-- to improve the overall performance and ameliorate the worst case
-- performance degradation of the original Boyer-Moore algorithm for
-- periodic patterns.
--
-- Efficiency demands that the pattern be a strict 'S.ByteString',
-- to work with a lazy pattern, convert it to a strict 'S.ByteString'
-- first via 'strictify' (provided it is not too long).
-- If support for long lazy patterns is needed, mail a feature-request.
--
-- When searching a pattern in a UTF-8-encoded 'S.ByteString', be aware that
-- these functions work on bytes, not characters, so the indices are
-- byte-offsets, not character offsets.


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
-- algorithms, e.g. "Data.ByteString.Lazy.Search.DFA" can be faster (my
-- tests suggest that \"very short\" means two, maybe three bytes).
--
-- In general, searching in a strict 'S.ByteString' is slightly faster
-- than searching in a lazy 'L.ByteString', but for long targets the
-- smaller memory footprint of lazy 'L.ByteString's can make searching
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
-- long patterns; none of the functions except 'indices' suffer from
-- this problem, though).

-- $caution
--
-- When working with a lazy target string, the relation between the pattern
-- length and the chunk size can play a big r&#244;le.
-- Crossing chunk boundaries is relatively expensive, so when that becomes
-- a frequent occurrence, as may happen when the pattern length is close
-- to or larger than the chunk size, performance is likely to degrade.
-- If it is needed, steps can be taken to ameliorate that effect, but unless
-- entirely separate functions are introduced, that would hurt the
-- performance for the more common case of patterns much shorter than
-- the default chunk size.

-- $complexity
--
-- Preprocessing the pattern is /O/(@patternLength@ + &#963;) in time and
-- space (&#963; is the alphabet size, 256 here) for all functions.
-- The time complexity of the searching phase for 'indices'
-- is /O/(@targetLength@ \/ @patternLength@) in the best case.
-- For non-periodic patterns, the worst case complexity is
-- /O/(@targetLength@), but for periodic patterns, the worst case complexity
-- is /O/(@targetLength@ * @patternLength@) for the original Boyer-Moore
-- algorithm.
--
-- The searching functions in this module contain a modification which
-- drastically improves the performance for periodic patterns, although
-- less for lazy targets than for strict ones.
-- If I'm not mistaken, the worst case complexity for periodic patterns
-- is /O/(@targetLength@ * (1 + @patternLength@ \/ @chunkSize@)).
--
-- The other functions don't have to deal with possible overlapping
-- patterns, hence the worst case complexity for the processing phase
-- is /O/(@targetLength@) (respectively /O/(@firstIndex + patternLength@)
-- for the breaking functions if the pattern occurs).

-- $partial
--
-- All functions can usefully be partially applied. Given only a pattern,
-- the pattern is preprocessed only once, allowing efficient re-use.

-- $overflow
--
-- The current code uses @Int@ to keep track of the locations in the
-- target string.  If the length of the pattern plus the length of any
-- strict chunk of the target string is greater or equal to
-- @'maxBound' :: 'Int'@ then this will overflow causing an error.  We try
-- to detect this and call 'error' before a segfault occurs.

------------------------------------------------------------------------------
--                            Exported Functions                            --
------------------------------------------------------------------------------

-- | @'indices'@ finds the starting indices of all possibly overlapping
--   occurrences of the pattern in the target string.
--   If the pattern is empty, the result is @[0 .. 'length' target]@.
{-# INLINE indices #-}
indices :: S.ByteString     -- ^ Strict pattern to find
        -> L.ByteString     -- ^ Lazy string to search
        -> [Int64]          -- ^ Offsets of matches
indices = BM.matchSL

-- | @'nonOverlappingIndices'@ finds the starting indices of all
--   non-overlapping occurrences of the pattern in the target string.
--   It is more efficient than removing indices from the list produced
--   by 'indices'.
{-# INLINE nonOverlappingIndices #-}
nonOverlappingIndices :: S.ByteString   -- ^ Strict pattern to find
                      -> L.ByteString   -- ^ Lazy string to search
                      -> [Int64]        -- ^ Offsets of matches
nonOverlappingIndices = BM.matchNOL

-- | @'breakOn' pattern target@ splits @target@ at the first occurrence
--   of @pattern@. If the pattern does not occur in the target, the
--   second component of the result is empty, otherwise it starts with
--   @pattern@. If the pattern is empty, the first component is empty.
--   For a non-empty pattern, the first component is generated lazily,
--   thus the first parts of it can be available before the pattern has
--   been found or determined to be absent.
--
-- @
--   'uncurry' 'L.append' . 'breakOn' pattern = 'id'
-- @
{-# INLINE breakOn #-}
breakOn :: S.ByteString  -- ^ Strict pattern to search for
        -> L.ByteString  -- ^ Lazy string to search in
        -> (L.ByteString, L.ByteString)
                         -- ^ Head and tail of string broken at substring
breakOn = BM.breakSubstringL

-- | @'breakAfter' pattern target@ splits @target@ behind the first occurrence
--   of @pattern@. An empty second component means that either the pattern
--   does not occur in the target or the first occurrence of pattern is at
--   the very end of target. If you need to discriminate between those cases,
--   use breakFindAfter.
--   If the pattern is empty, the first component is empty.
--   For a non-empty pattern, the first component is generated lazily,
--   thus the first parts of it can be available before the pattern has
--   been found or determined to be absent.
--
-- @
--   'uncurry' 'L.append' . 'breakAfter' pattern = 'id'
-- @
{-# INLINE breakAfter #-}
breakAfter :: S.ByteString  -- ^ Strict pattern to search for
           -> L.ByteString  -- ^ Lazy string to search in
           -> (L.ByteString, L.ByteString)
                            -- ^ Head and tail of string broken after substring
breakAfter = BM.breakAfterL

-- | @'breakFindAfter'@ does the same as 'breakAfter' but additionally indicates
--   whether the pattern is present in the target.
--
-- @
--   'fst' . 'breakFindAfter' pat = 'breakAfter' pat
-- @
{-# INLINE breakFindAfter #-}
breakFindAfter :: S.ByteString  -- ^ Strict pattern to search for
               -> L.ByteString  -- ^ Lazy string to search in
               -> ((L.ByteString, L.ByteString), Bool)
                            -- ^ Head and tail of string broken after substring
                            --   and presence of pattern
breakFindAfter = BM.breakFindAfterL

-- | @'replace' pat sub text@ replaces all (non-overlapping) occurrences of
--   @pat@ in @text@ with @sub@. If occurrences of @pat@ overlap, the first
--   occurrence that does not overlap with a replaced previous occurrence
--   is substituted. Occurrences of @pat@ arising from a substitution
--   will not be substituted. For example:
--
-- @
--   'replace' \"ana\" \"olog\" \"banana\" = \"bologna\"
--   'replace' \"ana\" \"o\" \"bananana\" = \"bono\"
--   'replace' \"aab\" \"abaa\" \"aaabb\" = \"aabaab\"
-- @
--
--   The result is a lazy 'L.ByteString',
--   which is lazily produced, without copying.
--   Equality of pattern and substitution is not checked, but
--
-- @
--   'replace' pat pat text == text
-- @
--
--   holds (the internal structure is generally different).
--   If the pattern is empty but not the substitution, the result
--   is equivalent to (were they 'String's) @cycle sub@.
--
--   For non-empty @pat@ and @sub@ a lazy 'L.ByteString',
--
-- @
--   'L.concat' . 'Data.List.intersperse' sub . 'split' pat = 'replace' pat sub
-- @
--
--   and analogous relations hold for other types of @sub@.
{-# INLINE replace #-}
replace :: Substitution rep
        => S.ByteString     -- ^ Strict pattern to replace
        -> rep              -- ^ Replacement string
        -> L.ByteString     -- ^ Lazy string to modify
        -> L.ByteString     -- ^ Lazy result
replace = BM.replaceAllL

-- | @'split' pattern target@ splits @target@ at each (non-overlapping)
--   occurrence of @pattern@, removing @pattern@. If @pattern@ is empty,
--   the result is an infinite list of empty 'L.ByteString's, if @target@
--   is empty but not @pattern@, the result is an empty list, otherwise
--   the following relations hold (where @patL@ is the lazy 'L.ByteString'
--   corresponding to @pat@):
--
-- @
--   'L.concat' . 'Data.List.intersperse' patL . 'split' pat = 'id',
--   'length' ('split' pattern target) ==
--               'length' ('nonOverlappingIndices' pattern target) + 1,
-- @
--
--   no fragment in the result contains an occurrence of @pattern@.
{-# INLINE split #-}
split :: S.ByteString   -- ^ Strict pattern to split on
      -> L.ByteString   -- ^ Lazy string to split
      -> [L.ByteString] -- ^ Fragments of string
split = BM.splitDropL

-- | @'splitKeepEnd' pattern target@ splits @target@ after each (non-overlapping)
--   occurrence of @pattern@. If @pattern@ is empty, the result is an
--   infinite list of empty 'L.ByteString's, otherwise the following
--   relations hold:
--
-- @
--   'L.concat' . 'splitKeepEnd' pattern = 'id',
-- @
--
--   all fragments in the result except possibly the last end with
--   @pattern@, no fragment contains more than one occurrence of @pattern@.
{-# INLINE splitKeepEnd #-}
splitKeepEnd :: S.ByteString    -- ^ Strict pattern to split on
             -> L.ByteString    -- ^ Lazy string to split
             -> [L.ByteString]  -- ^ Fragments of string
splitKeepEnd = BM.splitKeepEndL

-- | @'splitKeepFront'@ is like 'splitKeepEnd', except that @target@ is split
--   before each occurrence of @pattern@ and hence all fragments
--   with the possible exception of the first begin with @pattern@.
--   No fragment contains more than one non-overlapping occurrence
--   of @pattern@.
{-# INLINE splitKeepFront #-}
splitKeepFront :: S.ByteString    -- ^ Strict pattern to split on
               -> L.ByteString    -- ^ Lazy string to split
               -> [L.ByteString]  -- ^ Fragments of string
splitKeepFront = BM.splitKeepFrontL

-- | @'strictify'@ converts a lazy 'L.ByteString' to a strict 'S.ByteString'
--   to make it a suitable pattern.
strictify :: L.ByteString -> S.ByteString
strictify = S.concat . L.toChunks
