{-# LANGUAGE BangPatterns #-}
-- |
-- Module         : Data.ByteString.Lazy.Search.DFA
-- Copyright      : Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast search of lazy 'L.ByteString' values. Breaking,
-- splitting and replacing using a deterministic finite automaton.

module Data.ByteString.Lazy.Search.DFA ( -- * Overview
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
                                       , breakFindAfter
                                         -- * Replacing
                                       , replace
                                         -- * Splitting
                                       , split
                                       , splitKeepEnd
                                       , splitKeepFront
                                       ) where

import Data.ByteString.Search.Internal.Utils (automaton, keep, ldrop, lsplit)
import Data.ByteString.Search.Substitution

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Data.ByteString.Unsafe (unsafeIndex)

import Data.Array.Base (unsafeAt)
--import Data.Array.Unboxed (UArray)

import Data.Bits
import Data.Int (Int64)

-- $overview
--
-- This module provides functions related to searching a substring within
-- a string. The searching algorithm uses a deterministic finite automaton
-- based on the Knuth-Morris-Pratt algorithm.
-- The automaton is implemented as an array of @(patternLength + 1) * &#963;@
-- state transitions, where &#963; is the alphabet size (256), so it is only
-- suitable for short enough patterns, therefore the patterns in this module
-- are required to be strict 'S.ByteString's.
--
-- When searching a pattern in a UTF-8-encoded 'L.ByteString', be aware that
-- these functions work on bytes, not characters, so the indices are
-- byte-offsets, not character offsets.

-- $complexity
--
-- The time and space complexity of the preprocessing phase is
-- /O/(@patternLength * &#963;@).
-- The searching phase is /O/(@targetLength@), each target character is
-- inspected only once.
--
-- In general the functions in this module have about the same performance as
-- the corresponding functions using the Knuth-Morris-Pratt algorithm but
-- are considerably slower than the Boyer-Moore functions. For very short
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
indices :: S.ByteString     -- ^ Strict pattern to find
        -> L.ByteString     -- ^ Lazy string to search
        -> [Int64]          -- ^ Offsets of matches
indices !pat = lazySearcher True pat . L.toChunks

-- | @'nonOverlappingIndices'@ finds the starting indices of all
--   non-overlapping occurrences of the pattern in the target string.
--   It is more efficient than removing indices from the list produced
--   by 'indices'.
{-# INLINE nonOverlappingIndices #-}
nonOverlappingIndices :: S.ByteString   -- ^ Strict pattern to find
                      -> L.ByteString   -- ^ Lazy string to search
                      -> [Int64]        -- ^ Offsets of matches
nonOverlappingIndices !pat = lazySearcher False pat . L.toChunks

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
breakOn :: S.ByteString  -- ^ Strict pattern to search for
        -> L.ByteString  -- ^ Lazy string to search in
        -> (L.ByteString, L.ByteString)
                         -- ^ Head and tail of string broken at substring
breakOn pat = breaker . L.toChunks
  where
    lbrk = lazyBreaker True pat
    breaker strs = let (f, b) = lbrk strs
                   in (L.fromChunks f, L.fromChunks b)

-- | @'breakAfter' pattern target@ splits @target@ behind the first occurrence
--   of @pattern@. An empty second component means that either the pattern
--   does not occur in the target or the first occurrence of pattern is at
--   the very end of target. If you need to discriminate between those cases,
--   use breakFindAfter.
--   If the pattern is empty, the first component is empty.
--   For a non-empty pattern, the first component is generated lazily,
--   thus the first parts of it can be available before the pattern has
--   been found or determined to be absent.
-- @
--   'uncurry' 'L.append' . 'breakAfter' pattern = 'id'
-- @
breakAfter :: S.ByteString  -- ^ Strict pattern to search for
           -> L.ByteString  -- ^ Lazy string to search in
           -> (L.ByteString, L.ByteString)
                            -- ^ Head and tail of string broken after substring
breakAfter pat = breaker . L.toChunks
  where
    lbrk = lazyBreaker False pat
    breaker strs = let (f, b) = lbrk strs
                   in (L.fromChunks f, L.fromChunks b)

-- | @'breakFindAfter'@ does the same as 'breakAfter' but additionally indicates
--   whether the pattern is present in the target.
--
-- @
--   'fst' . 'breakFindAfter' pat = 'breakAfter' pat
-- @
breakFindAfter :: S.ByteString  -- ^ Strict pattern to search for
               -> L.ByteString  -- ^ Lazy string to search in
               -> ((L.ByteString, L.ByteString), Bool)
                            -- ^ Head and tail of string broken after substring
                            --   and presence of pattern
breakFindAfter pat
  | S.null pat  = \str -> ((L.empty, str), True)
breakFindAfter pat = breaker . L.toChunks
  where
    !patLen = S.length pat
    lbrk = lazyBreaker True pat
    breaker strs = let (f, b) = lbrk strs
                       (f1, b1) = lsplit patLen b
                       mbpat = L.fromChunks f1
                   in ((foldr LI.chunk mbpat f, L.fromChunks b1), not (null b))

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
replace :: Substitution rep
        => S.ByteString     -- ^ Strict pattern to replace
        -> rep              -- ^ Replacement string
        -> L.ByteString     -- ^ Lazy string to modify
        -> L.ByteString     -- ^ Lazy result
replace pat
  | S.null pat = \sub -> prependCycle sub
  | otherwise =
    let !patLen = S.length pat
        breaker = lazyBreaker True pat
        repl subst strs
          | null strs   = []
          | otherwise   =
            let (pre, mtch) = breaker strs
            in pre ++ case mtch of
                        [] -> []
                        _  -> subst (repl subst (ldrop patLen mtch))
    in \sub -> let {-# NOINLINE subst #-}
                   !subst = substitution sub
                   repl1 = repl subst
               in L.fromChunks . repl1 . L.toChunks


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
split :: S.ByteString   -- ^ Strict pattern to split on
      -> L.ByteString   -- ^ Lazy string to split
      -> [L.ByteString] -- ^ Fragments of string
split pat
  | S.null pat  = const (repeat L.empty)
split pat = map L.fromChunks . splitter . L.toChunks
  where
    !patLen = S.length pat
    breaker = lazyBreaker True pat
    splitter strs
      | null strs  = []
      | otherwise  = splitter' strs
    splitter' strs
      | null strs  = [[]]
      | otherwise  =
        case breaker strs of
          (pre, mtch) ->
            pre : case mtch of
                    [] -> []
                    _  -> splitter' (ldrop patLen mtch)

-- | @'splitKeepEnd' pattern target@ splits @target@ after each (non-overlapping)
--   occurrence of @pattern@. If @pattern@ is empty, the result is an
--   infinite list of empty 'L.ByteString's, otherwise the following
--   relations hold:
--
-- @
--   'L.concat' . 'splitKeepEnd' pattern = 'id,'
-- @
--
--   all fragments in the result except possibly the last end with
--   @pattern@, no fragment contains more than one occurrence of @pattern@.
splitKeepEnd :: S.ByteString    -- ^ Strict pattern to split on
             -> L.ByteString    -- ^ Lazy string to split
             -> [L.ByteString]  -- ^ Fragments of string
splitKeepEnd pat
  | S.null pat = const (repeat L.empty)
splitKeepEnd pat = map L.fromChunks . splitter . L.toChunks
  where
    breaker = lazyBreaker False pat
    splitter [] = []
    splitter strs =
      case breaker strs of
        (pre, mtch) -> pre : splitter mtch

-- | @'splitKeepFront'@ is like 'splitKeepEnd', except that @target@ is split
--   before each occurrence of @pattern@ and hence all fragments
--   with the possible exception of the first begin with @pattern@.
--   No fragment contains more than one non-overlapping occurrence
--   of @pattern@.
splitKeepFront :: S.ByteString    -- ^ Strict pattern to split on
               -> L.ByteString    -- ^ Lazy string to split
               -> [L.ByteString]  -- ^ Fragments of string
splitKeepFront pat
  | S.null pat  = const (repeat L.empty)
splitKeepFront pat = map L.fromChunks . splitter . L.toChunks
  where
    !patLen = S.length pat
    breaker = lazyBreaker True pat
    splitter strs = case splitter' strs of
                      ([] : rst) -> rst
                      other -> other
    splitter' []    = []
    splitter' strs  =
      case breaker strs of
        (pre, mtch) ->
          pre : case mtch of
                  [] -> []
                  _  -> case lsplit patLen mtch of
                          (pt, rst) ->
                            if null rst
                              then [pt]
                              else let (h : t) = splitter' rst
                                   in (pt ++ h) : t

------------------------------------------------------------------------------
--                            Searching Function                            --
------------------------------------------------------------------------------

lazySearcher :: Bool -> S.ByteString -> [S.ByteString] -> [Int64]
lazySearcher _ !pat
    | S.null pat        =
      let zgo _ [] = []
          zgo !prior (!str : rest) =
              let !l = S.length str
                  !prior' = prior + fromIntegral l
              in [prior + fromIntegral i | i <- [1 .. l]] ++ zgo prior' rest
      in (0:) . zgo 0
    | S.length pat == 1 =
      let !w = S.head pat
          ixes = S.elemIndices w
          go _ [] = []
          go !prior (!str : rest)
            = let !prior' = prior + fromIntegral (S.length str)
              in map ((+ prior) . fromIntegral) (ixes str) ++ go prior' rest
      in go 0
lazySearcher !overlap pat = search 0 0
  where
    !patLen = S.length pat
    !auto   = automaton pat
    !p0     = unsafeIndex pat 0
    !ams    = if overlap then patLen else 0
    search _ _ [] = []
    search !prior st (!str:rest) = match st 0
      where
        !strLen = S.length str
        {-# INLINE strAt #-}
        strAt :: Int -> Int
        strAt i = fromIntegral (str `unsafeIndex` i)
        match 0 !idx
          | idx == strLen = search (prior + fromIntegral strLen) 0 rest
          | unsafeIndex str idx == p0   = match 1 (idx + 1)
          | otherwise     = match 0 (idx + 1)
        match state idx
          | idx == strLen = search (prior + fromIntegral strLen) state rest
          | otherwise     =
            let nstate = unsafeAt auto ((state `shiftL` 8) + strAt idx)
                !nxtIdx = idx + 1
            in if nstate == patLen
                then (prior + fromIntegral (nxtIdx - patLen)) :
                            match ams nxtIdx
                else match nstate nxtIdx

------------------------------------------------------------------------------
--                                 Breaking                                 --
------------------------------------------------------------------------------

-- Code duplication :(
-- Needed for reasonable performance.
lazyBreaker :: Bool -> S.ByteString -> [S.ByteString]
                    -> ([S.ByteString], [S.ByteString])
lazyBreaker before pat
  | S.null pat  = \strs -> ([], strs)
  | S.length pat == 1 =
    let !w = S.head pat
        !a = if before then 0 else 1
        ixes = S.elemIndices w
        scan [] = ([], [])
        scan (!str:rest) =
            let !strLen = S.length str
            in case ixes str of
                []  -> let (fr, bk) = scan rest in (str : fr, bk)
                (i:_) -> let !j = i + a
                         in if j == strLen
                              then ([str],rest)
                              else ([S.take j str], S.drop j str : rest)
    in scan
lazyBreaker !before pat = bscan [] 0
  where
    !patLen = S.length pat
    !auto   = automaton pat
    !p0     = unsafeIndex pat 0
    bscan _ _ [] = ([], [])
    bscan !past !sta (!str:rest) = match sta 0
      where
        !strLen = S.length str
        {-# INLINE strAt #-}
        strAt :: Int -> Int
        strAt i = fromIntegral (str `unsafeIndex` i)
        match 0 idx
          | idx == strLen =
            let (fr, bk) = bscan [] 0 rest
            in (foldr (flip (.) . (:)) id past (str:fr), bk)
          | unsafeIndex str idx == p0 = match 1 (idx + 1)
          | otherwise = match 0 (idx + 1)
        match state idx
          | idx == strLen =
            let (kp, !rl) = if before
                                then keep state (str:past)
                                else ([], str:past)
                (fr, bk) = bscan kp state rest
            in (foldr (flip (.) . (:)) id rl fr, bk)
          | otherwise =
            let !nstate = unsafeAt auto ((state `shiftL` 8) + strAt idx)
                !nxtIdx = idx + 1
            in if nstate == patLen
                then case if before then nxtIdx - patLen else nxtIdx of
                       0 -> (foldr (flip (.) . (:)) id past [], str:rest)
                       stIx | stIx < 0 -> rgo (-stIx) (str:rest) past
                            | stIx == strLen ->
                              (foldr (flip (.) . (:)) id past [str],rest)
                            | otherwise ->
                              (foldr (flip (.) . (:)) id past
                                    [S.take stIx str], S.drop stIx str : rest)
                else match nstate nxtIdx


-- Did I already mention that I suck at finding names?
{-# INLINE rgo #-}
rgo :: Int -> [S.ByteString] -> [S.ByteString]
    -> ([S.ByteString], [S.ByteString])
rgo !kp acc (!str:more)
  | sl == kp    = (reverse more, str:acc)
  | sl < kp     = rgo (kp - sl) (str:acc) more
  | otherwise   = case S.splitAt (sl - kp) str of
                    (fr, bk) ->
                      (foldr (flip (.) . (:)) id more [fr], bk:acc)
    where
      !sl = S.length str
rgo _ _ [] = error "Not enough past!"
-- If that error is ever encountered, I screwed up badly.
