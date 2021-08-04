-- |
-- Module         : Data.ByteString.Search
-- Copyright      : Daniel Fischer (2007-2011)
--                  Chris Kuklewicz
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast overlapping Boyer-Moore search of strict
-- 'S.ByteString' values. Breaking, splitting and replacing
-- using the Boyer-Moore algorithm.
--
-- Descriptions of the algorithm can be found at
-- <http://www-igm.univ-mlv.fr/~lecroq/string/node14.html#SECTION00140>
-- and
-- <http://en.wikipedia.org/wiki/Boyer-Moore_string_search_algorithm>
--
-- Original authors: Daniel Fischer (daniel.is.fischer at googlemail.com) and
-- Chris Kuklewicz (haskell at list.mightyreason.com).
module Data.ByteString.Search ( -- * Overview
                                -- $overview

                                -- ** Performance
                                -- $performance

                                -- ** Complexity
                                -- $complexity

                                -- ** Partial application
                                -- $partial

                                -- * Finding substrings
                                indices
                              , nonOverlappingIndices
                                -- * Breaking on substrings
                              , breakOn
                              , breakAfter
                                -- * Replacing
                              , replace
                                -- * Splitting
                              , split
                              , splitKeepEnd
                              , splitKeepFront
                              ) where

import qualified Data.ByteString.Search.Internal.BoyerMoore as BM
import Data.ByteString.Search.Substitution
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- $overview
--
-- This module provides functions related to searching a substring within
-- a string, using the Boyer-Moore algorithm with minor modifications
-- to improve the overall performance and avoid the worst case
-- performance degradation of the original Boyer-Moore algorithm for
-- periodic patterns.
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
-- algorithms, e.g. "Data.ByteString.Search.DFA" can be faster (my
-- tests suggest that \"very short\" means two, maybe three bytes).
--
-- In general, searching in a strict 'S.ByteString' is slightly faster
-- than searching in a lazy 'L.ByteString', but for long targets, the
-- smaller memory footprint of lazy 'L.ByteString's can make searching
-- those (sometimes much) faster. On the other hand, there are cases
-- where searching in a strict target is much faster, even for long targets.

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
-- drastically improves the performance for periodic patterns.
-- I believe that for strict target strings, the worst case is now
-- /O/(@targetLength@) also for periodic patterns.
-- I may be wrong, though.
--
-- The other functions don't have to deal with possible overlapping
-- patterns, hence the worst case complexity for the processing phase
-- is /O/(@targetLength@) (respectively /O/(@firstIndex + patternLength@)
-- for the breaking functions if the pattern occurs).

-- $partial
--
-- All functions can usefully be partially applied. Given only a pattern,
-- the pattern is preprocessed only once, allowing efficient re-use.

------------------------------------------------------------------------------
--                            Exported Functions                            --
------------------------------------------------------------------------------

-- | @'indices'@ finds the starting indices of all possibly overlapping
--   occurrences of the pattern in the target string.
--   If the pattern is empty, the result is @[0 .. 'length' target]@.
--
--   In general, @'not' . 'null' $ 'indices' pat target@ is a much more
--   efficient version of 'S.isInfixOf'.
{-# INLINE indices #-}
indices :: S.ByteString     -- ^ Pattern to find
        -> S.ByteString     -- ^ String to search
        -> [Int]            -- ^ Offsets of matches
indices = BM.matchSS

-- | @'nonOverlappingIndices'@ finds the starting indices of all
--   non-overlapping occurrences of the pattern in the target string.
--   It is more efficient than removing indices from the list produced
--   by 'indices'.
{-# INLINE nonOverlappingIndices #-}
nonOverlappingIndices :: S.ByteString   -- ^ Pattern to find
                      -> S.ByteString   -- ^ String to search
                      -> [Int]          -- ^ Offsets of matches
nonOverlappingIndices = BM.matchNOS

-- | @'breakOn' pattern target@ splits @target@ at the first occurrence
--   of @pattern@. If the pattern does not occur in the target, the
--   second component of the result is empty, otherwise it starts with
--   @pattern@. If the pattern is empty, the first component is empty.
--
-- @
--   'uncurry' 'S.append' . 'breakOn' pattern = 'id'
-- @
{-# INLINE breakOn #-}
breakOn :: S.ByteString  -- ^ String to search for
        -> S.ByteString  -- ^ String to search in
        -> (S.ByteString, S.ByteString)
                         -- ^ Head and tail of string broken at substring
breakOn = BM.breakSubstringS

-- | @'breakAfter' pattern target@ splits @target@ behind the first occurrence
--   of @pattern@. An empty second component means that either the pattern
--   does not occur in the target or the first occurrence of pattern is at
--   the very end of target. To discriminate between those cases, use e.g.
--   'S.isSuffixOf'.
--
-- @
--   'uncurry' 'S.append' . 'breakAfter' pattern = 'id'
-- @
{-# INLINE breakAfter #-}
breakAfter :: S.ByteString  -- ^ String to search for
           -> S.ByteString  -- ^ String to search in
           -> (S.ByteString, S.ByteString)
                            -- ^ Head and tail of string broken after substring
breakAfter = BM.breakAfterS

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
--   The result is a /lazy/ 'L.ByteString',
--   which is lazily produced, without copying.
--   Equality of pattern and substitution is not checked, but
--
-- @
--   ('S.concat' . 'L.toChunks' $ 'replace' pat pat text) == text
-- @
--
--   holds. If the pattern is empty but not the substitution, the result
--   is equivalent to (were they 'String's) @'cycle' sub@.
--
--   For non-empty @pat@ and @sub@ a strict 'S.ByteString',
--
-- @
--   'L.fromChunks' . 'Data.List.intersperse' sub . 'split' pat = 'replace' pat sub
-- @
--
--   and analogous relations hold for other types of @sub@.
{-# INLINE replace #-}
replace :: Substitution rep
        => S.ByteString     -- ^ Substring to replace
        -> rep              -- ^ Replacement string
        -> S.ByteString     -- ^ String to modify
        -> L.ByteString     -- ^ Lazy result
replace = BM.replaceAllS

-- | @'split' pattern target@ splits @target@ at each (non-overlapping)
--   occurrence of @pattern@, removing @pattern@. If @pattern@ is empty,
--   the result is an infinite list of empty 'S.ByteString's, if @target@
--   is empty but not @pattern@, the result is an empty list, otherwise
--   the following relations hold:
--
-- @
--   'S.concat' . 'Data.List.intersperse' pat . 'split' pat = 'id',
--   'length' ('split' pattern target) ==
--               'length' ('nonOverlappingIndices' pattern target) + 1,
-- @
--
--   no fragment in the result contains an occurrence of @pattern@.
{-# INLINE split #-}
split :: S.ByteString   -- ^ Pattern to split on
      -> S.ByteString   -- ^ String to split
      -> [S.ByteString] -- ^ Fragments of string
split = BM.splitDropS

-- | @'splitKeepEnd' pattern target@ splits @target@ after each (non-overlapping)
--   occurrence of @pattern@. If @pattern@ is empty, the result is an
--   infinite list of empty 'S.ByteString's, otherwise the following
--   relations hold:
--
-- @
--   'S.concat' . 'splitKeepEnd' pattern = 'id',
-- @
--
--   all fragments in the result except possibly the last end with
--   @pattern@, no fragment contains more than one occurrence of @pattern@.
{-# INLINE splitKeepEnd #-}
splitKeepEnd :: S.ByteString    -- ^ Pattern to split on
             -> S.ByteString    -- ^ String to split
             -> [S.ByteString]  -- ^ Fragments of string
splitKeepEnd = BM.splitKeepEndS

-- | @'splitKeepFront'@ is like 'splitKeepEnd', except that @target@ is split
--   before each occurrence of @pattern@ and hence all fragments
--   with the possible exception of the first begin with @pattern@.
--   No fragment contains more than one non-overlapping occurrence
--   of @pattern@.
{-# INLINE splitKeepFront #-}
splitKeepFront :: S.ByteString    -- ^ Pattern to split on
               -> S.ByteString    -- ^ String to split
               -> [S.ByteString]  -- ^ Fragments of string
splitKeepFront = BM.splitKeepFrontS
