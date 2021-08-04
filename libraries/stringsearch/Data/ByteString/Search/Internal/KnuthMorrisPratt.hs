{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide, prune #-}
-- |
-- Module         : Data.ByteString.Search.Internal.KnuthMorrisPratt
-- Copyright      : Justin Bailey
--                  Chris Kuklewicz
--                  Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast Knuth-Morris-Pratt search of both strict and
-- lazy 'S.ByteString' values.
--
-- A description of the algorithm can be found at
-- <http://en.wikipedia.org/wiki/Knuth-Morris-Pratt_algorithm>.

-- Original authors: Justin Bailey (jgbailey at gmail.com) and
-- Chris Kuklewicz (haskell at list.mightyreason.com).

module Data.ByteString.Search.Internal.KnuthMorrisPratt ( -- * Overview
                                                          -- $overview

                                                          -- * Partial application
                                                          -- $partial

                                                          -- * Complexity and Performance
                                                          -- $complexity

                                                          -- * Finding substrings
                                                          -- ** Overlapping
                                                            indicesL
                                                          , indicesS
                                                          -- ** Non-overlapping
                                                          , matchLL
                                                          , matchLS
                                                          , matchSL
                                                          , matchSS
                                                          ) where

import Data.ByteString.Search.Internal.Utils (kmpBorders, strictify)

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe (unsafeIndex)

import Data.Array.Base (unsafeAt)
--import Data.Array.Unboxed

import Data.Int (Int64)

-- $overview
--
-- This module exports 6 search functions: 'matchLL', 'matchLS',
-- 'matchSL', and 'matchSS', which find the indices of all non-overlapping
-- occurrences of a pattern in a target string, and the newly added
-- 'indicesL' and 'indicesS' which find the indices of
-- all (possibly overlapping) occurrences of the pattern in the target
-- string. The performance should be the same when the pattern can't
-- overlap, but when the pattern occurs often and can have significant
-- overlap, the search excluding the overlap is faster.
--
-- In all cases, the list of indices is produced lazily.
--
-- The behaviour of the old @matchXY@ functions for an empty pattern has
-- changed, formerly they returned an empty list, now all functions
-- return @[0 .. 'length' target]@ for an empty pattern.
--
-- The return type of the @matchXS@ functions changed to @['Int']@, since
-- strict ByteStrings are @'Int'@-indexed.
--
-- The trailing @L\/S@ in the function names indicate whether they work
-- on lazy or strict ByteStrings. Since all patterns are converted to
-- strict ByteStrings for performance reasons, the @matchLX@ add just
-- an additional bit of wrapping around the worker in comparison to
-- @matchSX@. For the new functions, no such wrapping is provided, you
-- have to 'strictify' lazy patterns before feeding them to the searcher.
-- The limit on the pattern lengths that the conversion to a strict
-- ByteString imposes should be irrelevant in practice.
--
-- The functions searching in lazy ByteStrings don't keep any references
-- to chunks already traversed. This means the garbage collector can free
-- those chunks early and only a small part of the target string needs to
-- be in memory.

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

------------------------------------------------------------------------------
--                                 Wrappers                                 --
------------------------------------------------------------------------------

-- | @'indicesL'@ finds all indices of (possibly overlapping)
--   occurrences of the pattern in the target string.
{-# INLINE indicesL #-}
indicesL :: S.ByteString     -- ^ Strict pattern
         -> L.ByteString     -- ^ Lazy target string
         -> [Int64]          -- ^ Offsets of matches
indicesL pat = search . L.toChunks
  where
    search = matcher True pat

-- | @'indicesS'@ finds all indices of (possibly overlapping)
--   occurrences of the pattern in the target string.
{-# INLINE indicesS #-}
indicesS :: S.ByteString     -- ^ Strict pattern
         -> S.ByteString     -- ^ Strict target string
         -> [Int]            -- ^ Offsets of matches
indicesS pat = search . (:[])
  where
    search = matcher True pat

-- | @'matchLL'@ finds the starting indices of all /non-overlapping/ occurrences
--   of the pattern in the target string. It is a simple wrapper around
--   'Data.ByteString.Lazy.Search.KMP.nonOverlappingIndices' strictifying
--   the pattern.
{-# INLINE matchLL #-}
matchLL :: L.ByteString           -- ^ Lazy pattern
        -> L.ByteString           -- ^ Lazy target string
        -> [Int64]                -- ^ Offsets of matches
matchLL pat = search . L.toChunks
  where
    !spat = strictify pat
    search = matcher False spat

-- | @'matchLS'@ finds the starting indices of all /non-overlapping/ occurrences
--   of the pattern in the target string. It is a simple wrapper around
--   'Data.ByteString.Search.KMP.nonOverlappingIndices' strictifying
--   the pattern.
{-# INLINE matchLS #-}
matchLS :: L.ByteString         -- ^ Lazy pattern
        -> S.ByteString         -- ^ Strict target string
        -> [Int]                -- ^ Offsets of matches
matchLS pat = search . (:[])
  where
    !spat = strictify pat
    search = matcher False spat

-- | @'matchSS'@ finds the starting indices of all /non-overlapping/ occurrences
--   of the pattern in the target string. It is an alias for
--   'Data.ByteString.Search.KMP.nonOverlappingIndices'.
{-# INLINE matchSS #-}
matchSS :: S.ByteString         -- ^ Strict pattern
        -> S.ByteString         -- ^ Strict target string
        -> [Int]                -- ^ Offsets of matches
matchSS pat = search . (:[])
  where
    search = matcher False pat

-- | @'matchSL'@ finds the starting indices of all /non-overlapping/ occurrences
--   of the pattern in the target string. It is an alias for
--   'Data.ByteString.Lazy.Search.KMP.nonOverlappingIndices'.
{-# INLINE matchSL #-}
matchSL :: S.ByteString         -- ^ Strict pattern
        -> L.ByteString         -- ^ Lazy target string
        -> [Int64]              -- ^ Offsets of matches
matchSL pat = search . L.toChunks
  where
    search = matcher False pat


------------------------------------------------------------------------------
--                                  Worker                                  --
------------------------------------------------------------------------------

{-# SPECIALISE matcher :: Bool -> S.ByteString -> [S.ByteString] -> [Int],
                          Bool -> S.ByteString -> [S.ByteString] -> [Int64] #-}
matcher :: Integral a => Bool -> S.ByteString -> [S.ByteString] -> [a]
matcher _ !pat
  | S.null pat =  (0 :) . go 0
    where
      go _ [] = []
      go !prior (!str : rest) = [prior + fromIntegral i | i <- [1 .. l]]
                                  ++ go prior' rest
        where
          !l = S.length str
          !prior' = prior + fromIntegral l
matcher !overlap pat = searcher 0 0
  where
    !patLen = S.length pat
    !bords  = kmpBorders pat
    !patH   = patAt 0
    {-# INLINE misi #-}
    misi !i = unsafeAt bords i
    {-# INLINE patAt #-}
    patAt !i = unsafeIndex pat i
    !ami    = if overlap then misi patLen else 0
    searcher _ _ [] = []
    searcher !prior !patPos (!str : rest)
      | patPos == 0 = checkHead 0
      | otherwise = findMatch patPos 0
      where
        !strLen = S.length str
        {-# INLINE strAt #-}
        strAt !i = unsafeIndex str i
        checkHead !strI
            | strI == strLen =
              searcher (prior + fromIntegral strLen) 0 rest
            | strAt strI == patH = findMatch 1 (strI + 1)
            | otherwise = checkHead (strI + 1)
        findMatch !patI !strI
            | patI == patLen =
                (prior + fromIntegral strI - fromIntegral patLen)
                    : if ami == 0 then checkHead strI else findMatch ami strI
            | strI == strLen =
                searcher (prior + fromIntegral strLen) patI rest
            | otherwise      =
                if strAt strI == patAt patI
                    then findMatch (patI + 1) (strI + 1)
                    else case misi patI of
                            0    -> checkHead strI
                            (-1) -> checkHead (strI + 1)
                            pI   -> findMatch pI strI
