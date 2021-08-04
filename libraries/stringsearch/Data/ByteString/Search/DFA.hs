{-# LANGUAGE BangPatterns #-}
-- |
-- Module         : Data.ByteString.Search.DFA
-- Copyright      : Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast search of strict 'S.ByteString' values. Breaking, splitting and
-- replacing using a deterministic finite automaton.

module Data.ByteString.Search.DFA ( -- * Overview
                                    -- $overview

                                    -- ** Complexity and performance
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

import Data.ByteString.Search.Internal.Utils (automaton)
import Data.ByteString.Search.Substitution

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Data.ByteString.Unsafe (unsafeIndex)

import Data.Array.Base (unsafeAt)
--import Data.Array.Unboxed

import Data.Bits

-- $overview
--
-- This module provides functions related to searching a substring within
-- a string. The searching algorithm uses a deterministic finite automaton
-- based on the Knuth-Morris-Pratt algorithm.
-- The automaton is implemented as an array of @(patternLength + 1) * &#963;@
-- state transitions, where &#963; is the alphabet size (256), so it is
-- only suitable for short enough patterns.
--
-- When searching a pattern in a UTF-8-encoded 'S.ByteString', be aware that
-- these functions work on bytes, not characters, so the indices are
-- byte-offsets, not character offsets.

-- $complexity
--
-- The time and space complexity of the preprocessing phase is
-- /O/(@patternLength * &#963;@).
-- The searching phase is /O/(@targetLength@), each target character is
-- inspected only once.
--
-- In general the functions in this module are slightly faster than the
-- corresponding functions using the Knuth-Morris-Pratt algorithm but
-- considerably slower than the Boyer-Moore functions. For very short
-- patterns or, in the case of 'indices', patterns with a short period
-- which occur often, however, times are close to or even below the
-- Boyer-Moore times.

-- $partial
--
-- All functions can usefully be partially applied. Given only a pattern,
-- the automaton is constructed only once, allowing efficient re-use.

------------------------------------------------------------------------------
--                            Exported Functions                            --
------------------------------------------------------------------------------

-- | @'indices'@ finds the starting indices of all possibly overlapping
--   occurrences of the pattern in the target string.
--   If the pattern is empty, the result is @[0 .. 'length' target]@.
{-# INLINE indices #-}
indices :: S.ByteString     -- ^ Pattern to find
        -> S.ByteString     -- ^ String to search
        -> [Int]            -- ^ Offsets of matches
indices = strictSearcher True

-- | @'nonOverlappingIndices'@ finds the starting indices of all
--   non-overlapping occurrences of the pattern in the target string.
--   It is more efficient than removing indices from the list produced
--   by 'indices'.
{-# INLINE nonOverlappingIndices #-}
nonOverlappingIndices :: S.ByteString   -- ^ Pattern to find
                      -> S.ByteString   -- ^ String to search
                      -> [Int]          -- ^ Offsets of matches
nonOverlappingIndices = strictSearcher False

-- | @'breakOn' pattern target@ splits @target@ at the first occurrence
--   of @pattern@. If the pattern does not occur in the target, the
--   second component of the result is empty, otherwise it starts with
--   @pattern@. If the pattern is empty, the first component is empty.
--
-- @
--   'uncurry' 'S.append' . 'breakOn' pattern = 'id'
-- @
breakOn :: S.ByteString  -- ^ String to search for
        -> S.ByteString  -- ^ String to search in
        -> (S.ByteString, S.ByteString)
                         -- ^ Head and tail of string broken at substring
breakOn pat = breaker
  where
    searcher = strictSearcher False pat
    breaker str = case searcher str of
                    []      -> (str, S.empty)
                    (i:_)   -> S.splitAt i str

-- | @'breakAfter' pattern target@ splits @target@ behind the first occurrence
--   of @pattern@. An empty second component means that either the pattern
--   does not occur in the target or the first occurrence of pattern is at
--   the very end of target. To discriminate between those cases, use e.g.
--   'S.isSuffixOf'.
--
-- @
--   'uncurry' 'S.append' . 'breakAfter' pattern = 'id'
-- @
breakAfter :: S.ByteString  -- ^ String to search for
           -> S.ByteString  -- ^ String to search in
           -> (S.ByteString, S.ByteString)
                            -- ^ Head and tail of string broken after substring
breakAfter pat = breaker
  where
    !patLen = S.length pat
    searcher = strictSearcher False pat
    breaker str = case searcher str of
                    []      -> (str, S.empty)
                    (i:_)   -> S.splitAt (i + patLen) str


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
--   'S.concat' . 'L.toChunks' $ 'replace' pat pat text == text
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
replace :: Substitution rep
        => S.ByteString     -- ^ Substring to replace
        -> rep              -- ^ Replacement string
        -> S.ByteString     -- ^ String to modify
        -> L.ByteString     -- ^ Lazy result
replace pat
  | S.null pat = \sub -> prependCycle sub . flip LI.chunk LI.Empty
  | otherwise =
    let !patLen = S.length pat
        searcher = strictSearcher False pat
        repl sub =
          let {-# NOINLINE subst #-}
              !subst = substitution sub
              replacer str
                | S.null str    = []
                | otherwise     =
                  case searcher str of
                    []              -> [str]
                    (i:_)
                        | i == 0    -> subst $ replacer (S.drop patLen str)
                        | otherwise -> S.take i str : subst
                                        (replacer (S.drop (i + patLen) str))
          in replacer
    in \sub -> L.fromChunks . repl sub

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
split :: S.ByteString   -- ^ Pattern to split on
      -> S.ByteString   -- ^ String to split
      -> [S.ByteString] -- ^ Fragments of string
split pat
  | S.null pat  = const (repeat S.empty)
split pat = splitter
  where
    !patLen = S.length pat
    searcher = strictSearcher False pat
    splitter str
      | S.null str = []
      | otherwise  = splitter' str
    splitter' str
      | S.null str = [S.empty]
      | otherwise  =
        case searcher str of
          []    -> [str]
          (i:_) -> S.take i str : splitter' (S.drop (i + patLen) str)

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
splitKeepEnd :: S.ByteString    -- ^ Pattern to split on
             -> S.ByteString    -- ^ String to split
             -> [S.ByteString]  -- ^ Fragments of string
splitKeepEnd pat
  | S.null pat = const (repeat S.empty)
splitKeepEnd pat = splitter
  where
    !patLen = S.length pat
    searcher = strictSearcher False pat
    splitter str
      | S.null str  = []
      | otherwise   =
        case searcher str of
          []    -> [str]
          (i:_) -> S.take (i + patLen) str :
                        splitter (S.drop (i + patLen) str)

-- | @'splitKeepFront'@ is like 'splitKeepEnd', except that @target@ is split
--   before each occurrence of @pattern@ and hence all fragments
--   with the possible exception of the first begin with @pattern@.
--   No fragment contains more than one non-overlapping occurrence
--   of @pattern@.
splitKeepFront :: S.ByteString    -- ^ Pattern to split on
               -> S.ByteString    -- ^ String to split
               -> [S.ByteString]  -- ^ Fragments of string
splitKeepFront pat
  | S.null pat  = const (repeat S.empty)
splitKeepFront pat = splitter
  where
    !patLen = S.length pat
    searcher = strictSearcher False pat
    splitter str
      | S.null str  = []
      | otherwise   =
        case searcher str of
          []            -> [str]
          (i:rst)
            | i == 0    -> case rst of
                             []     -> [str]
                             (j:_)  -> S.take j str : splitter' (S.drop j str)
            | otherwise -> S.take i str : splitter' (S.drop i str)
    splitter' str
      | S.null str  = []
      | otherwise   =
        case searcher (S.drop patLen str) of
          []    -> [str]
          (i:_) -> S.take (i + patLen) str :
                        splitter' (S.drop (i + patLen) str)

------------------------------------------------------------------------------
--                            Searching Function                            --
------------------------------------------------------------------------------

strictSearcher :: Bool -> S.ByteString -> S.ByteString -> [Int]
strictSearcher _ !pat
    | S.null pat = enumFromTo 0 . S.length
    | S.length pat == 1 = let !w = S.head pat in S.elemIndices w
strictSearcher !overlap pat = search
  where
    !patLen = S.length pat
    !auto   = automaton pat
    !p0     = unsafeIndex pat 0
    !ams    = if overlap then patLen else 0
    search str = match 0 0
      where
        !strLen = S.length str
        {-# INLINE strAt #-}
        strAt :: Int -> Int
        strAt !i = fromIntegral (unsafeIndex str i)
        match 0 idx
          | idx == strLen               = []
          | unsafeIndex str idx == p0   = match 1 (idx + 1)
          | otherwise                   = match 0 (idx + 1)
        match state idx
          | idx == strLen   = []
          | otherwise       =
            let !nstate = unsafeAt auto ((state `shiftL` 8) + strAt idx)
                !nxtIdx = idx + 1
            in if nstate == patLen
                then (nxtIdx - patLen) : match ams nxtIdx
                else match nstate nxtIdx

