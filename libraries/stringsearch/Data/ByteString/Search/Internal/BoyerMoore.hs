{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide, prune #-}
-- |
-- Module         : Data.ByteString.Search.Internal.BoyerMoore
-- Copyright      : Daniel Fischer
--                  Chris Kuklewicz
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Fast overlapping Boyer-Moore search of both strict and lazy
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

module Data.ByteString.Search.Internal.BoyerMoore (
                                           matchLS
                                         , matchSS

                                           --  Non-overlapping
                                         , matchNOS

                                            --  Replacing substrings
                                            -- replacing
                                         , replaceAllS
                                            --  Breaking on substrings
                                            -- breaking
                                         , breakSubstringS
                                         , breakAfterS
                                            --  Splitting on substrings
                                            -- splitting
                                         , splitKeepEndS
                                         , splitKeepFrontS
                                         , splitDropS
                                         ) where


import Data.ByteString.Search.Internal.Utils
                (occurs, suffShifts, strictify)
import Data.ByteString.Search.Substitution

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import Data.ByteString.Unsafe (unsafeIndex)

import Data.Array.Base (unsafeAt)

import Data.Word (Word8)

-- overview
--
-- This module exports three search functions for searching in strict
-- ByteStrings. One for searching non-overlapping occurrences of a strict
-- pattern and one each for possibly overlapping occurrences of a lazy
-- resp. strict pattern. The common base name is @match@, the suffix
-- indicates the type of search to perform. These functions
-- return (for a non-empty pattern) a list of all the indices of the target
-- string where an occurrence of the pattern begins, if some occurrences
-- overlap, all starting indices are reported. The list is produced lazily,
-- so not necessarily the entire target string is searched.
--
-- The behaviour of these functions when given an empty pattern has changed.
-- Formerly, the @matchXY@ functions returned an empty list then, now it's
-- @[0 .. 'length' target]@.
--
-- Newly added are functions to replace all (non-overlapping) occurrences
-- of a pattern within a string, functions to break ByteStrings at the first
-- occurrence of a pattern and functions to split ByteStrings at each
-- occurrence of a pattern. None of these functions does copying, so they
-- don't introduce large memory overhead.
--
-- Internally, a lazy pattern is always converted to a strict ByteString,
-- which is necessary for an efficient implementation of the algorithm.
-- The limit this imposes on the length of the pattern is probably
-- irrelevant in practice, but perhaps it should be mentioned.
-- This also means that the @matchL*@ functions are mere convenience wrappers.
-- Except for the initial 'strictify'ing, there's no difference between lazy
-- and strict patterns, they call the same workers. There is, however, a
-- difference between strict and lazy target strings.
-- For the new functions, no such wrappers are provided, you have to
-- 'strictify' lazy patterns yourself.

-- caution
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

-- performance
--
-- In general, the Boyer-Moore algorithm is the most efficient method to
-- search for a pattern inside a string, so most of the time, you'll want
-- to use the functions of this module, hence this is where the most work
-- has gone. Very short patterns are an exception to this, for those you
-- should consider using a finite automaton
-- ("Data.ByteString.Search.DFA.Array"). That is also often the better
-- choice for searching longer periodic patterns in a lazy ByteString
-- with many matches.
--
-- Operating on a strict target string is mostly faster than on a lazy
-- target string, but the difference is usually small (according to my
-- tests).
--
-- The known exceptions to this rule of thumb are
--
-- [long targets] Then the smaller memory footprint of a lazy target often
-- gives (much) better performance.
--
-- [high number of matches] When there are very many matches, strict target
-- strings are much faster, especially if the pattern is periodic.
--
-- If both conditions hold, either may outweigh the other.

-- complexity
--
-- Preprocessing the pattern is /O/(@patternLength@ + &#963;) in time and
-- space (&#963; is the alphabet size, 256 here) for all functions.
-- The time complexity of the searching phase for @matchXY@
-- is /O/(@targetLength@ \/ @patternLength@) in the best case.
-- For non-periodic patterns, the worst case complexity is
-- /O/(@targetLength@), but for periodic patterns, the worst case complexity
-- is /O/(@targetLength@ * @patternLength@) for the original Boyer-Moore
-- algorithm.
--
-- The searching functions in this module now contain a modification which
-- drastically improves the performance for periodic patterns.
-- I believe that for strict target strings, the worst case is now
-- /O/(@targetLength@) also for periodic patterns and for lazy target strings,
-- my semi-educated guess is
-- /O/(@targetLength@ * (1 + @patternLength@ \/ @chunkSize@)).
-- I may be very wrong, though.
--
-- The other functions don't have to deal with possible overlapping
-- patterns, hence the worst case complexity for the processing phase
-- is /O/(@targetLength@) (respectively /O/(@firstIndex + patternLength@)
-- for the breaking functions if the pattern occurs).

-- currying
--
-- These functions can all be usefully curried. Given only a pattern
-- the curried version will compute the supporting lookup tables only
-- once, allowing for efficient re-use.  Similarly, the curried
-- 'matchLL' and 'matchLS' will compute the concatenated pattern only
-- once.

-- overflow
--
-- The current code uses @Int@ to keep track of the locations in the
-- target string.  If the length of the pattern plus the length of any
-- strict chunk of the target string is greater than
-- @'maxBound' :: 'Int'@ then this will overflow causing an error.  We
-- try to detect this and call 'error' before a segfault occurs.

------------------------------------------------------------------------------
--                                 Wrappers                                 --
------------------------------------------------------------------------------

-- matching
--
-- These functions find the indices of all (possibly overlapping)
-- occurrences of a pattern in a target string.
-- If the pattern is empty, the result is @[0 .. length target]@.
-- If the pattern is much shorter than the target string
-- and the pattern does not occur very near the beginning of the target,
--
-- > not . null $ matchSS pattern target
--
-- is a much more efficient version of 'S.isInfixOf'.

-- | @'matchLS'@ finds the starting indices of all possibly overlapping
--   occurrences of the pattern in the target string.
--   It is a simple wrapper for 'Data.ByteString.Search.indices'.
--   If the pattern is empty, the result is @[0 .. 'length' target]@.
{-# INLINE matchLS #-}
matchLS :: L.ByteString     -- ^ Lazy pattern
        -> S.ByteString     -- ^ Strict target string
        -> [Int]            -- ^ Offsets of matches
matchLS pat = search
  where
    search = strictSearcher True (strictify pat)

-- | @'matchSS'@ finds the starting indices of all possibly overlapping
--   occurrences of the pattern in the target string.
--   It is an alias for 'Data.ByteString.Search.indices'.
--   If the pattern is empty, the result is @[0 .. 'length' target]@.
{-# INLINE matchSS #-}
matchSS :: S.ByteString     -- ^ Strict pattern
        -> S.ByteString     -- ^ Strict target string
        -> [Int]            -- ^ Offsets of matches
matchSS pat = search
  where
    search = strictSearcher True pat

-- | @'matchNOS'@ finds the indices of all non-overlapping occurrences
--   of the pattern in the Strict target string.
{-# INLINE matchNOS #-}
matchNOS :: S.ByteString    -- ^ Strict pattern
         -> S.ByteString    -- ^ Strict target string
         -> [Int]           -- ^ Offsets of matches
matchNOS pat = search
  where
    search = strictSearcher False pat

-- replacing
--
--   These functions replace all (non-overlapping) occurrences of a pattern
--   in the target string. If some occurrences overlap, the earliest is
--   replaced and replacing continues at the index after the replaced
--   occurrence, for example
--
-- > replaceAllL \"ana\" \"olog\" \"banana\" == \"bologna\",
-- > replaceAllS \"abacab\" \"u\" \"abacabacabacab\" == \"uacu\",
-- > replaceAllS \"aa\" \"aaa\" \"aaaa\" == \"aaaaaa\".
--
--   Equality of pattern and substitution is not checked, but
--
-- > pat == sub => 'strictify' (replaceAllS pat sub str) == str,
-- > pat == sub => replaceAllL pat sub str == str.
--
--   The result is a lazily generated lazy ByteString, the first chunks will
--   generally be available before the entire target has been scanned.
--   If the pattern is empty, but not the substitution, the result is
--   equivalent to @'cycle' sub@.

{-# INLINE replaceAllS #-}
replaceAllS :: Substitution rep
            => S.ByteString  -- ^ Pattern to replace
            -> rep           -- ^ Substitution string
            -> S.ByteString  -- ^ Target string
            -> L.ByteString  -- ^ Lazy result
replaceAllS pat
    | S.null pat = \sub -> prependCycle sub . flip LI.chunk LI.Empty
    | otherwise =
      let repl = strictRepl pat
      in \sub -> L.fromChunks . repl (substitution sub)

-- breaking
--
-- Break a string on a pattern. The first component of the result
-- contains the prefix of the string before the first occurrence of the
-- pattern, the second component contains the remainder.
-- The following relations hold:
--
-- > breakSubstringX \"\" str = (\"\", str)
-- > not (pat `isInfixOf` str) == null (snd $ breakSunbstringX pat str)
-- > True == case breakSubstringX pat str of
-- >          (x, y) -> not (pat `isInfixOf` x)
-- >                       && (null y || pat `isPrefixOf` y)

-- | This function has the same semantics as 'S.breakSubstring'
--   but is generally much faster.
{-# INLINE breakSubstringS #-}
breakSubstringS :: S.ByteString  -- ^ Pattern to break on
                -> S.ByteString  -- ^ String to break up
                -> (S.ByteString, S.ByteString)
                    -- ^ Prefix and remainder of broken string
breakSubstringS = strictBreak

breakAfterS :: S.ByteString
            -> S.ByteString
            -> (S.ByteString, S.ByteString)
breakAfterS pat
  | S.null pat = \str -> (S.empty, str)
breakAfterS pat = breaker
  where
    !patLen  = S.length pat
    searcher = strictSearcher False pat
    breaker str = case searcher str of
                    []    -> (str, S.empty)
                    (i:_) -> S.splitAt (i + patLen) str

-- splitting
--
-- These functions implement various splitting strategies.
--
-- If the pattern to split on is empty, all functions return an
-- infinite list of empty ByteStrings.
-- Otherwise, the names are rather self-explanatory.
--
-- For nonempty patterns, the following relations hold:
--
-- > concat (splitKeepXY pat str) == str
-- > concat ('Data.List.intersperse' pat (splitDropX pat str)) == str.
--
-- All fragments except possibly the last in the result of
-- @splitKeepEndX pat@ end with @pat@, none of the fragments contains
-- more than one occurrence of @pat@ or is empty.
--
-- All fragments except possibly the first in the result of
-- @splitKeepFrontX pat@ begin with @pat@, none of the fragments
-- contains more than one occurrence of @patq or is empty.
--
-- > splitDropX pat str == map dropPat (splitKeepFrontX pat str)
-- >   where
-- >     patLen = length pat
-- >     dropPat frag
-- >        | pat `isPrefixOf` frag = drop patLen frag
-- >        | otherwise             = frag
--
-- but @splitDropX@ is a little more efficient than that.


{-# INLINE splitKeepEndS #-}
splitKeepEndS :: S.ByteString    -- ^ Pattern to split on
              -> S.ByteString    -- ^ String to split
              -> [S.ByteString]  -- ^ List of fragments
splitKeepEndS = strictSplitKeepEnd

{-# INLINE splitKeepFrontS #-}
splitKeepFrontS :: S.ByteString    -- ^ Pattern to split on
                -> S.ByteString    -- ^ String to split
                -> [S.ByteString]  -- ^ List of fragments
splitKeepFrontS = strictSplitKeepFront

{-# INLINE splitDropS #-}
splitDropS :: S.ByteString    -- ^ Pattern to split on
           -> S.ByteString    -- ^ String to split
           -> [S.ByteString]  -- ^ List of fragments
splitDropS = strictSplitDrop

------------------------------------------------------------------------------
--                             Search Functions                             --
------------------------------------------------------------------------------

strictSearcher :: Bool -> S.ByteString -> S.ByteString -> [Int]
strictSearcher _ !pat
    | S.null pat = enumFromTo 0 . S.length
    | S.length pat == 1 = let !w = S.head pat in S.elemIndices w
strictSearcher !overlap pat = searcher
  where
    {-# INLINE patAt #-}
    patAt :: Int -> Word8
    patAt !i = unsafeIndex pat i

    !patLen = S.length pat
    !patEnd = patLen - 1
    !maxLen = maxBound - patLen
    !occT   = occurs pat        -- for bad-character-shift
    !suffT  = suffShifts pat    -- for good-suffix-shift
    !skip   = if overlap then unsafeAt suffT 0 else patLen
    -- shift after a complete match
    !kept   = patLen - skip     -- length of known prefix after full match
    !pe     = patAt patEnd      -- last pattern byte for fast comparison

    {-# INLINE occ #-}
    occ !w = unsafeAt occT (fromIntegral w)

    {-# INLINE suff #-}
    suff !i = unsafeAt suffT i

    searcher str
        | maxLen < strLen
            = error "Overflow in BoyerMoore.strictSearcher"
        | maxDiff < 0   = []
        | otherwise     = checkEnd patEnd
          where
            !strLen  = S.length str
            !strEnd  = strLen - 1
            !maxDiff = strLen - patLen

            {-# INLINE strAt #-}
            strAt !i = unsafeIndex str i

            -- After a full match, we know how long a prefix of the pattern
            -- still matches. Do not re-compare the prefix to prevent O(m*n)
            -- behaviour for periodic patterns.
            afterMatch !diff !patI =
              case strAt (diff + patI) of
                !c  | c == patAt patI ->
                      if patI == kept
                        then diff : let !diff' = diff + skip
                                    in if maxDiff < diff'
                                        then []
                                        else afterMatch diff' patEnd
                        else afterMatch diff (patI - 1)
                    | patI == patEnd  ->
                            checkEnd (diff + 2*patEnd + occ c)
                    | otherwise       ->
                            let {-# INLINE badShift #-}
                                badShift = patI + occ c
                                {-# INLINE goodShift #-}
                                goodShift = suff patI
                                !diff' = diff + max badShift goodShift
                            in if maxDiff < diff'
                                then []
                                else checkEnd (diff + patEnd)

            -- While comparing the last byte of the pattern, the bad-
            -- character-shift is always at least as large as the good-
            -- suffix-shift. Eliminating the unnecessary memory reads and
            -- comparison speeds things up noticeably.
            checkEnd !sI  -- index in string to compare to last of pattern
                | strEnd < sI   = []
                | otherwise     =
                  case strAt sI of
                    !c  | c == pe   -> findMatch (sI - patEnd) (patEnd - 1)
                        | otherwise -> checkEnd (sI + patEnd + occ c)

            -- Once the last byte has matched, we enter the full matcher
            -- diff is the offset of the window, patI the index of the
            -- pattern byte to compare next.
            findMatch !diff !patI =
                case strAt (diff + patI) of
                    !c  | c == patAt patI ->
                            if patI == 0    -- full match, report
                                then diff : let !diff' = diff + skip
                                            in if maxDiff < diff'
                                                then []
                                                else
                                                  if skip == patLen
                                                    then
                                                      checkEnd (diff' + patEnd)
                                                    else
                                                      afterMatch diff' patEnd
                                else findMatch diff (patI - 1)
                        | otherwise       ->
                            let !diff' = diff + max (patI + occ c) (suff patI)
                            in if maxDiff < diff'
                                then []
                                else checkEnd (diff' + patEnd)

------------------------------------------------------------------------------
--                            Breaking Functions                            --
------------------------------------------------------------------------------

strictBreak :: S.ByteString -> S.ByteString -> (S.ByteString, S.ByteString)
strictBreak pat
    | S.null pat    = \str -> (S.empty, str)
    | otherwise     = breaker
      where
        searcher = strictSearcher False pat
        breaker str = case searcher str of
                        []      -> (str, S.empty)
                        (i:_)   -> S.splitAt i str

------------------------------------------------------------------------------
--                            Splitting Functions                           --
------------------------------------------------------------------------------

strictSplitKeepFront :: S.ByteString -> S.ByteString -> [S.ByteString]
strictSplitKeepFront pat
  | S.null pat  = const (repeat S.empty)
strictSplitKeepFront pat = splitter
  where
    !patLen = S.length pat
    searcher = strictSearcher False pat
    splitter str
        | S.null str    = []
        | otherwise     =
          case searcher str of
            []            -> [str]
            (i:_)
              | i == 0    -> psplitter str
              | otherwise -> S.take i str : psplitter (S.drop i str)
    psplitter !str
        | S.null str    = []
        | otherwise     =
          case searcher (S.drop patLen str) of
            []      -> [str]
            (i:_)   -> S.take (i + patLen) str :
                        psplitter (S.drop (i + patLen) str)

strictSplitKeepEnd :: S.ByteString -> S.ByteString -> [S.ByteString]
strictSplitKeepEnd pat
  | S.null pat  = const (repeat S.empty)
strictSplitKeepEnd pat = splitter
  where
    !patLen = S.length pat
    searcher = strictSearcher False pat
    splitter str
        | S.null str    = []
        | otherwise     =
          case searcher str of
            [] -> [str]
            (i:_) -> S.take (i + patLen) str :
                        splitter (S.drop (i + patLen) str)

strictSplitDrop :: S.ByteString -> S.ByteString -> [S.ByteString]
strictSplitDrop pat
    | S.null pat    = const (repeat S.empty)
strictSplitDrop pat = splitter'
  where
    !patLen = S.length pat
    searcher = strictSearcher False pat
    splitter' str
        | S.null str    = []
        | otherwise     = splitter str
    splitter str
        | S.null str    = [S.empty]
        | otherwise     =
          case searcher str of
            []            -> [str]
            (i:_) -> S.take i str : splitter (S.drop (i + patLen) str)

------------------------------------------------------------------------------
--                            Replacing Functions                           --
------------------------------------------------------------------------------

-- replacing loop for strict ByteStrings, called only for
-- non-empty patterns and substitutions
strictRepl :: S.ByteString -> ([S.ByteString] -> [S.ByteString])
            -> S.ByteString -> [S.ByteString]
strictRepl pat  = repl
  where
    !patLen = S.length pat
    searcher = strictSearcher False pat
    repl sub = replacer
      where
        replacer str
          | S.null str    = []
          | otherwise     =
            case searcher str of
              []            -> [str]
              (i:_)
                | i == 0    -> sub $ replacer (S.drop patLen str)
                | otherwise ->
                  S.take i str : sub (replacer (S.drop (i + patLen) str))
