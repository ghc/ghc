-- |
-- Module         : Data.ByteString.Lazy.Search.KMP
-- Copyright      : Justin Bailey
--                  Chris Kuklewicz
--                  Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast search of lazy 'L.ByteString' values using the
-- Knuth-Morris-Pratt algorithm.
--
-- A description of the algorithm can be found at
-- <http://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm>.
--
-- Original authors: Justin Bailey (jgbailey at gmail.com) and
-- Chris Kuklewicz (haskell at list.mightyreason.com).
module Data.ByteString.Lazy.Search.KMP (-- * Overview
                                        -- $overview

                                        -- ** Complexity and Performance
                                        -- $complexity

                                        -- ** Partial application
                                        -- $partial

                                        -- * Functions
                                          indices
                                        , nonOverlappingIndices
                                        -- ** Convenience
                                        , strictify
                                        ) where

import Data.ByteString.Search.Internal.KnuthMorrisPratt (matchSL, indicesL)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Int (Int64)

-- $overview
--
-- This module provides two functions for finding the occurrences of a
-- pattern in a target string using the Knuth-Morris-Pratt algorithm.
-- It exists mostly for systematic reasons, the functions from
-- "Data.ByteString.Lazy.Search" are much faster, except for very short
-- patterns or long patterns with a short period if overlap is allowed.
-- In the latter case, 'indices' from this module may be the best choice
-- since the Boyer-Moore function's performance degrades if there are many
-- matches and the DFA function's automaton needs much space for long
-- patterns.
-- In the former case, for some pattern\/target combinations DFA has better
-- performance, for others KMP, usually the difference is small.

-- $complexity
--
-- The preprocessing of the pattern is /O/(@patternLength@) in time and space.
-- The time complexity of the searching phase is /O/(@targetLength@) for both
-- functions.
--
-- In most cases, these functions are considerably slower than the
-- Boyer-Moore variants, performance is close to that of those from
-- "Data.ByteString.Search.DFA".

-- $partial
--
-- Both functions can be usefully partially applied. Given only a
-- pattern, the auxiliary data will be computed only once, allowing for
-- efficient re-use.

-- | @'indices'@ finds the starting indices of all possibly overlapping
--   occurrences of the pattern in the target string.
--   If the pattern is empty, the result is @[0 .. 'length' target]@.
{-# INLINE indices #-}
indices :: S.ByteString     -- ^ Strict pattern to find
        -> L.ByteString     -- ^ Lazy string to search
        -> [Int64]          -- ^ Offsets of matches
indices = indicesL

-- | @'nonOverlappingIndices'@ finds the starting indices of all
--   non-overlapping occurrences of the pattern in the target string.
--   It is more efficient than removing indices from the list produced
--   by 'indices'.
{-# INLINE nonOverlappingIndices #-}
nonOverlappingIndices :: S.ByteString   -- ^ Strict pattern to find
                      -> L.ByteString   -- ^ Lazy string to search
                      -> [Int64]        -- ^ Offsets of matches
nonOverlappingIndices = matchSL

-- | @'strictify'@ transforms a lazy 'L.ByteString' into a strict
--   'S.ByteString', to make it a suitable pattern for the searching
--   functions.
strictify :: L.ByteString -> S.ByteString
strictify = S.concat . L.toChunks
