-- |
-- Module         : Data.ByteString.Search.KMP
-- Copyright      : Justin Bailey
--                  Chris Kuklewicz
--                  Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast search of strict 'S.ByteString' values using the
-- Knuth-Morris-Pratt algorithm.
--
-- A description of the algorithm can be found at
-- <http://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm>.
--
-- Original authors: Justin Bailey (jgbailey at gmail.com) and
-- Chris Kuklewicz (haskell at list.mightyreason.com).
module Data.ByteString.Search.KMP ( -- * Overview
                                    -- $overview

                                    -- ** Complexity and Performance
                                    -- $complexity

                                    -- ** Partial application
                                    -- $partial

                                    -- * Functions
                                      indices
                                    , nonOverlappingIndices
                                    ) where

import Data.ByteString.Search.Internal.KnuthMorrisPratt (matchSS, indicesS)
import qualified Data.ByteString as S

-- $overview
--
-- This module provides two functions for finding the occurrences of a
-- pattern in a target string using the Knuth-Morris-Pratt algorithm.
-- It exists only for systematic reasons, the functions from
-- "Data.ByteString.Search" are much faster, except for very short patterns,
-- in which case "Data.ByteString.Search.DFA" provides better functions.

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
indices :: S.ByteString     -- ^ Pattern to find
        -> S.ByteString     -- ^ String to search
        -> [Int]            -- ^ Offsets of matches
indices = indicesS

-- | @'nonOverlappingIndices'@ finds the starting indices of all
--   non-overlapping occurrences of the pattern in the target string.
--   It is more efficient than removing indices from the list produced
--   by 'indices'.
{-# INLINE nonOverlappingIndices #-}
nonOverlappingIndices :: S.ByteString   -- ^ Pattern to find
                      -> S.ByteString   -- ^ String to search
                      -> [Int]          -- ^ Offsets of matches
nonOverlappingIndices = matchSS
