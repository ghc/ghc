{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide, prune #-}
-- |
-- Module         : Data.ByteString.Lazy.Search.Internal.BoyerMoore
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

module Data.ByteString.Lazy.Search.Internal.BoyerMoore (
                                           matchLL
                                         , matchSL

                                           --  Non-overlapping
                                         , matchNOL

                                            --  Replacing substrings
                                            -- replacing
                                         , replaceAllL
                                            --  Breaking on substrings
                                            -- breaking
                                         , breakSubstringL
                                         , breakAfterL
                                         , breakFindAfterL
                                            --  Splitting on substrings
                                            -- splitting
                                         , splitKeepEndL
                                         , splitKeepFrontL
                                         , splitDropL
                                         ) where


import Data.ByteString.Search.Internal.Utils
                (occurs, suffShifts, ldrop, lsplit, keep, release, strictify)
import Data.ByteString.Search.Substitution

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe (unsafeIndex)

import Data.Array.Base (unsafeAt)

import Data.Word (Word8)
import Data.Int (Int64)

-- overview
--
-- This module exports three search functions for searching in lazy
-- ByteSrings, one for searching non-overlapping occurrences of a strict
-- pattern, and one each for searchin overlapping occurrences of a strict
-- resp. lazy pattern. The common base name is @match@, the suffix
-- indicates the type of search. These functions
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

-- | @'matchLL'@ finds the starting indices of all possibly overlapping
--   occurrences of the pattern in the target string.
--   It is a simple wrapper for 'Data.ByteString.Lazy.Search.indices'.
--   If the pattern is empty, the result is @[0 .. 'length' target]@.
{-# INLINE matchLL #-}
matchLL :: L.ByteString     -- ^ Lazy pattern
        -> L.ByteString     -- ^ Lazy target string
        -> [Int64]          -- ^ Offsets of matches
matchLL pat = search . L.toChunks
  where
    search  = lazySearcher True (strictify pat)

-- | @'matchSL'@ finds the starting indices of all possibly overlapping
--   occurrences of the pattern in the target string.
--   It is an alias for 'Data.ByteString.Lazy.Search.indices'.
--   If the pattern is empty, the result is @[0 .. 'length' target]@.
{-# INLINE matchSL #-}
matchSL :: S.ByteString     -- ^ Strict pattern
        -> L.ByteString     -- ^ Lazy target string
        -> [Int64]          -- ^ Offsets of matches
matchSL pat = search . L.toChunks
  where
    search = lazySearcher True pat

-- | @'matchNOL'@ finds the indices of all non-overlapping occurrences
--   of the pattern in the lazy target string.
{-# INLINE matchNOL #-}
matchNOL :: S.ByteString    -- ^ Strict pattern
         -> L.ByteString    -- ^ Lazy target string
         -> [Int64]         -- ^ Offsets of matches
matchNOL pat = search . L.toChunks
  where
    search = lazySearcher False pat

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

{-# INLINE replaceAllL #-}
replaceAllL :: Substitution rep
            => S.ByteString  -- ^ Pattern to replace
            -> rep           -- ^ Substitution string
            -> L.ByteString  -- ^ Target string
            -> L.ByteString  -- ^ Lazy result
replaceAllL pat
    | S.null pat = \sub -> prependCycle sub
    | S.length pat == 1 =
      let breaker = lazyBreak pat
          repl subst strs
              | null strs = []
              | otherwise =
                case breaker strs of
                  (pre, mtch) ->
                        pre ++ case mtch of
                                [] -> []
                                _  -> subst (repl subst (ldrop 1 mtch))
      in \sub -> let repl1 = repl (substitution sub)
                 in L.fromChunks . repl1 . L.toChunks
    | otherwise =
      let repl = lazyRepl pat
      in \sub -> let repl1 = repl (substitution sub)
                 in L.fromChunks . repl1 . L.toChunks

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

-- | The analogous function for a lazy target string.
--   The first component is generated lazily, so parts of it can be
--   available before the pattern is detected (or found to be absent).
{-# INLINE breakSubstringL #-}
breakSubstringL :: S.ByteString  -- ^ Pattern to break on
                -> L.ByteString  -- ^ String to break up
                -> (L.ByteString, L.ByteString)
                    -- ^ Prefix and remainder of broken string
breakSubstringL pat = breaker . L.toChunks
  where
    lbrk = lazyBreak pat
    breaker strs = let (f, b) = lbrk strs
                   in (L.fromChunks f, L.fromChunks b)

breakAfterL :: S.ByteString
            -> L.ByteString
            -> (L.ByteString, L.ByteString)
breakAfterL pat
  | S.null pat      = \str -> (L.empty, str)
breakAfterL pat     = breaker' . L.toChunks
  where
    !patLen = S.length pat
    breaker = lazyBreak pat
    breaker' strs =
      let (pre, mtch) = breaker strs
          (pl, a) = if null mtch then ([],[]) else lsplit patLen mtch
      in (L.fromChunks (pre ++ pl), L.fromChunks a)

breakFindAfterL :: S.ByteString
                -> L.ByteString
                -> ((L.ByteString, L.ByteString), Bool)
breakFindAfterL pat
  | S.null pat  = \str -> ((L.empty, str), True)
breakFindAfterL pat = breaker' . L.toChunks
  where
    !patLen = S.length pat
    breaker = lazyBreak pat
    breaker' strs =
      let (pre, mtch) = breaker strs
          (pl, a) = if null mtch then ([],[]) else lsplit patLen mtch
      in ((L.fromChunks (pre ++ pl), L.fromChunks a), not (null mtch))

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

{-# INLINE splitKeepEndL #-}
splitKeepEndL :: S.ByteString    -- ^ Pattern to split on
              -> L.ByteString    -- ^ String to split
              -> [L.ByteString]  -- ^ List of fragments
splitKeepEndL pat
    | S.null pat    = const (repeat L.empty)
    | otherwise     =
      let splitter = lazySplitKeepEnd pat
      in  map L.fromChunks . splitter . L.toChunks

{-# INLINE splitKeepFrontL #-}
splitKeepFrontL :: S.ByteString    -- ^ Pattern to split on
                -> L.ByteString    -- ^ String to split
                -> [L.ByteString]  -- ^ List of fragments
splitKeepFrontL pat
    | S.null pat    = const (repeat L.empty)
    | otherwise     =
      let splitter = lazySplitKeepFront pat
      in  map L.fromChunks . splitter . L.toChunks


{-# INLINE splitDropL #-}
splitDropL :: S.ByteString    -- ^ Pattern to split on
           -> L.ByteString    -- ^ String to split
           -> [L.ByteString]  -- ^ List of fragments
splitDropL pat
    | S.null pat    = const (repeat L.empty)
    | otherwise     =
      let splitter = lazySplitDrop pat
      in map L.fromChunks . splitter . L.toChunks

------------------------------------------------------------------------------
--                             Search Functions                             --
------------------------------------------------------------------------------

lazySearcher :: Bool -> S.ByteString -> [S.ByteString] -> [Int64]
lazySearcher _ !pat
    | S.null pat        =
      let zgo !prior [] = [prior]
          zgo prior (!str : rest) =
              let !l = S.length str
                  !prior' = prior + fromIntegral l
              in [prior + fromIntegral i | i <- [0 .. l-1]] ++ zgo prior' rest
      in zgo 0
    | S.length pat == 1 =
      let !w = S.head pat
          ixes = S.elemIndices w
          go _ [] = []
          go !prior (!str : rest)
            = let !prior' = prior + fromIntegral (S.length str)
              in map ((+ prior) . fromIntegral) (ixes str) ++ go prior' rest
      in go 0
lazySearcher !overlap pat = searcher
  where
    {-# INLINE patAt #-}
    patAt :: Int -> Word8
    patAt !i = unsafeIndex pat i

    !patLen = S.length pat
    !patEnd = patLen - 1
    {-# INLINE preEnd #-}
    preEnd  = patEnd - 1
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

    searcher lst = case lst of
                    []      -> []
                    (h : t) ->
                      if maxLen < S.length h
                        then error "Overflow in BoyerMoore.lazySearcher"
                        else seek 0 [] h t 0 patEnd

    -- seek is used to position the "zipper" of (past, str, future) to the
    -- correct S.ByteString to search. This is done by ensuring that
    -- 0 <= strPos < strLen, where strPos = diffPos + patPos.
    -- Note that future is not a strict parameter. The bytes being compared
    -- will then be (strAt strPos) and (patAt patPos).
    -- Splitting this into specialised versions is possible, but it would
    -- only be useful if the pattern length is close to (or larger than)
    -- the chunk size. For ordinary patterns of at most a few hundred bytes,
    -- the overhead of yet more code-paths and larger code size will probably
    -- outweigh the small gains in the relatively rare calls to seek.
    seek :: Int64 -> [S.ByteString] -> S.ByteString
            -> [S.ByteString] -> Int -> Int -> [Int64]
    seek !prior !past !str future !diffPos !patPos
        | strPos < 0 =  -- need to look at previous chunk
            case past of
                (h : t) ->
                    let !hLen = S.length h
                    in seek (prior - fromIntegral hLen) t h (str : future)
                                (diffPos + hLen) patPos
                []      -> error "seek back too far!"
        | strEnd < strPos =  -- need to look at next chunk if there is
            case future of
                (h : t) ->
                    let {-# INLINE prior' #-}
                        prior' = prior + fromIntegral strLen
                        !diffPos' = diffPos - strLen
                        {-# INLINE past' #-}
                        past' = release (-diffPos') (str : past)
                    in if maxLen < S.length h
                        then error "Overflow in BoyerMoore.lazySearcher"
                        else seek prior' past' h t diffPos' patPos
                []      -> []
        | patPos == patEnd  = checkEnd strPos
        | diffPos < 0       = matcherN diffPos patPos
        | otherwise         = matcherP diffPos patPos
          where
            !strPos  = diffPos + patPos
            !strLen  = S.length str
            !strEnd  = strLen - 1
            !maxDiff = strLen - patLen

            {-# INLINE strAt #-}
            strAt !i = unsafeIndex str i

            -- While comparing the last byte of the pattern, the bad-
            -- character-shift is always at least as large as the good-
            -- suffix-shift. Eliminating the unnecessary memory reads and
            -- comparison speeds things up noticeably.
            checkEnd !sI  -- index in string to compare to last of pattern
              | strEnd < sI = seek prior past str future (sI - patEnd) patEnd
              | otherwise   =
                case strAt sI of
                  !c | c == pe   ->
                       if sI < patEnd
                        then case sI of
                              0 -> seek prior past str future (-patEnd) preEnd
                              _ -> matcherN (sI - patEnd) preEnd
                        else matcherP (sI - patEnd) preEnd
                     | otherwise -> checkEnd (sI + patEnd + occ c)

            -- Once the last byte has matched, we enter the full matcher
            -- diff is the offset of the window, patI the index of the
            -- pattern byte to compare next.

            -- matcherN is the tight loop that walks backwards from the end
            -- of the pattern checking for matching bytes. The offset is
            -- always negative, so no complete match can occur here.
            -- When a byte matches, we need to check whether we've reached
            -- the front of this chunk, otherwise whether we need the next.
            matcherN !diff !patI =
              case strAt (diff + patI) of
                !c  | c == patAt patI   ->
                        if diff + patI == 0
                            then seek prior past str future diff (patI - 1)
                            else matcherN diff (patI - 1)
                    | otherwise         ->
                        let {-# INLINE badShift #-}
                            badShift = patI + occ c
                            {-# INLINE goodShift #-}
                            goodShift = suff patI
                            !diff' = diff + max badShift goodShift
                        in if maxDiff < diff'
                            then seek prior past str future diff' patEnd
                            else checkEnd (diff' + patEnd)

            -- matcherP is the tight loop for non-negative offsets.
            -- When the pattern is shifted, we must check whether we leave
            -- the current chunk, otherwise we only need to check for a
            -- complete match.
            matcherP !diff !patI =
              case strAt (diff + patI) of
                !c  | c == patAt patI   ->
                      if patI == 0
                        then prior + fromIntegral diff :
                              let !diff' = diff + skip
                              in if maxDiff < diff'
                                then seek prior past str future diff' patEnd
                                else
                                  if skip == patLen
                                    then
                                      checkEnd (diff' + patEnd)
                                    else
                                      afterMatch diff' patEnd
                        else matcherP diff (patI - 1)
                    | otherwise         ->
                        let {-# INLINE badShift #-}
                            badShift = patI + occ c
                            {-# INLINE goodShift #-}
                            goodShift = suff patI
                            !diff' = diff + max badShift goodShift
                        in if maxDiff < diff'
                            then seek prior past str future diff' patEnd
                            else checkEnd (diff' + patEnd)

            -- After a full match, we know how long a prefix of the pattern
            -- still matches. Do not re-compare the prefix to prevent O(m*n)
            -- behaviour for periodic patterns.
            -- This breaks down at chunk boundaries, but except for long
            -- patterns with a short period, that shouldn't matter much.
            afterMatch !diff !patI =
              case strAt (diff + patI) of
                !c  | c == patAt patI ->
                      if patI == kept
                        then prior + fromIntegral diff :
                            let !diff' = diff + skip
                            in if maxDiff < diff'
                                then seek prior past str future diff' patEnd
                                else afterMatch diff' patEnd
                        else afterMatch diff (patI - 1)
                    | patI == patEnd  ->
                        checkEnd (diff + (2*patEnd) + occ c)
                    | otherwise       ->
                        let {-# INLINE badShift #-}
                            badShift = patI + occ c
                            {-# INLINE goodShift #-}
                            goodShift = suff patI
                            !diff' = diff + max badShift goodShift
                        in if maxDiff < diff'
                            then seek prior past str future diff' patEnd
                            else checkEnd (diff' + patEnd)

------------------------------------------------------------------------------
--                            Breaking Functions                            --
------------------------------------------------------------------------------

-- Ugh! Code duplication ahead!
-- But we want to get the first component lazily, so it's no good to find
-- the first index (if any) and then split.
-- Therefore bite the bullet and copy most of the code of lazySearcher.
-- No need for afterMatch here, fortunately.
lazyBreak ::S.ByteString -> [S.ByteString] -> ([S.ByteString], [S.ByteString])
lazyBreak !pat
  | S.null pat  = \lst -> ([],lst)
  | S.length pat == 1 =
    let !w = S.head pat
        go [] = ([], [])
        go (!str : rest) =
            case S.elemIndices w str of
                []    -> let (pre, post) = go rest in (str : pre, post)
                (i:_) -> if i == 0
                            then ([], str : rest)
                            else ([S.take i str], S.drop i str : rest)
    in go
lazyBreak pat = breaker
  where
    !patLen = S.length pat
    !patEnd = patLen - 1
    !occT   = occurs pat
    !suffT  = suffShifts pat
    !maxLen = maxBound - patLen
    !pe     = patAt patEnd

    {-# INLINE patAt #-}
    patAt !i = unsafeIndex pat i

    {-# INLINE occ #-}
    occ !w = unsafeAt occT (fromIntegral w)

    {-# INLINE suff #-}
    suff !i = unsafeAt suffT i

    breaker lst =
      case lst of
        []    -> ([],[])
        (h:t) ->
          if maxLen < S.length h
            then error "Overflow in BoyerMoore.lazyBreak"
            else seek [] h t 0 patEnd

    seek :: [S.ByteString] -> S.ByteString -> [S.ByteString]
                -> Int -> Int -> ([S.ByteString], [S.ByteString])
    seek !past !str future !offset !patPos
      | strPos < 0 =
        case past of
          [] -> error "not enough past!"
          (h : t) -> seek t h (str : future) (offset + S.length h) patPos
      | strEnd < strPos =
        case future of
          []      -> (foldr (flip (.) . (:)) id past [str], [])
          (h : t) ->
            let !off' = offset - strLen
                (past', !discharge) = keep (-off') (str : past)
            in if maxLen < S.length h
                then error "Overflow in BoyerMoore.lazyBreak (future)"
                else let (pre,post) = seek past' h t off' patPos
                     in (foldr (flip (.) . (:)) id discharge pre, post)
      | patPos == patEnd = checkEnd strPos
      | offset < 0 = matcherN offset patPos
      | otherwise  = matcherP offset patPos
      where
        {-# INLINE strAt #-}
        strAt !i = unsafeIndex str i

        !strLen = S.length str
        !strEnd = strLen - 1
        !maxOff = strLen - patLen
        !strPos = offset + patPos

        checkEnd !sI
          | strEnd < sI = seek past str future (sI - patEnd) patEnd
          | otherwise   =
            case strAt sI of
              !c  | c == pe   ->
                    if sI < patEnd
                      then (if sI == 0
                              then seek past str future (-patEnd) (patEnd - 1)
                              else matcherN (sI - patEnd) (patEnd - 1))
                      else matcherP (sI - patEnd) (patEnd - 1)
                  | otherwise -> checkEnd (sI + patEnd + occ c)

        matcherN !off !patI =
          case strAt (off + patI) of
            !c  | c == patAt patI ->
                  if off + patI == 0
                    then seek past str future off (patI - 1)
                    else matcherN off (patI - 1)
                | otherwise ->
                    let !off' = off + max (suff patI) (patI + occ c)
                    in if maxOff < off'
                        then seek past str future off' patEnd
                        else checkEnd (off' + patEnd)

        matcherP !off !patI =
          case strAt (off + patI) of
            !c  | c == patAt patI ->
                  if patI == 0
                    then let !pre = if off == 0 then [] else [S.take off str]
                             !post = S.drop off str
                         in (foldr (flip (.) . (:)) id past pre, post:future)
                    else matcherP off (patI - 1)
                | otherwise ->
                    let !off' = off + max (suff patI) (patI + occ c)
                    in if maxOff < off'
                        then seek past str future off' patEnd
                        else checkEnd (off' + patEnd)


------------------------------------------------------------------------------
--                            Splitting Functions                           --
------------------------------------------------------------------------------

-- non-empty pattern
lazySplitKeepFront :: S.ByteString -> [S.ByteString] -> [[S.ByteString]]
lazySplitKeepFront pat = splitter'
  where
    !patLen = S.length pat
    breaker = lazyBreak pat
    splitter' strs = case splitter strs of
                        ([]:rest) -> rest
                        other -> other
    splitter [] = []
    splitter strs =
      case breaker strs of
        (pre, mtch) ->
           pre : case mtch of
                    [] -> []
                    _  -> case lsplit patLen mtch of
                            (pt, rst) ->
                              if null rst
                                then [pt]
                                else let (h : t) = splitter rst
                                     in (pt ++ h) : t

-- non-empty pattern
lazySplitKeepEnd :: S.ByteString -> [S.ByteString] -> [[S.ByteString]]
lazySplitKeepEnd pat = splitter
  where
    !patLen = S.length pat
    breaker = lazyBreak pat
    splitter [] = []
    splitter strs =
      case breaker strs of
        (pre, mtch) ->
            let (h : t) = if null mtch
                            then [[]]
                            else case lsplit patLen mtch of
                                    (pt, rst) -> pt : splitter rst
            in (pre ++ h) : t

lazySplitDrop :: S.ByteString -> [S.ByteString] -> [[S.ByteString]]
lazySplitDrop pat = splitter
  where
    !patLen = S.length pat
    breaker = lazyBreak pat
    splitter [] = []
    splitter strs = splitter' strs
    splitter' [] = [[]]
    splitter' strs = case breaker strs of
                        (pre,mtch) ->
                            pre : case mtch of
                                    [] -> []
                                    _  -> splitter' (ldrop patLen mtch)

------------------------------------------------------------------------------
--                            Replacing Functions                           --
------------------------------------------------------------------------------

{-

These would be really nice.
Unfortunately they're too slow, so instead, there's another instance of
almost the same code as in lazySearcher below.

-- variant of below
lazyFRepl :: S.ByteString -> ([S.ByteString] -> [S.ByteString])
                -> [S.ByteString] -> [S.ByteString]
lazyFRepl pat = repl
  where
    !patLen = S.length pat
    breaker = lazyBreak pat
    repl sub = replacer
      where
        replacer [] = []
        replacer strs =
          let (pre, mtch) = breaker strs
          in pre ++ case mtch of
                      [] -> []
                      _  -> sub (replacer (ldrop patLen mtch))

-- This is nice and short. I really hope it's performing well!
lazyBRepl :: S.ByteString -> S.ByteString -> [S.ByteString] -> [S.ByteString]
lazyBRepl pat !sub = replacer
  where
    !patLen = S.length pat
    breaker = lazyBreak pat
    replacer [] = []
    replacer strs = let (pre, mtch) = breaker strs
                    in pre ++ case mtch of
                                [] -> []
                                _  -> sub : replacer (ldrop patLen mtch)
-}

-- Yet more code duplication.
--
-- Benchmark it against an implementation using lazyBreak and,
-- unless it's significantly faster, NUKE IT!!
--
-- Sigh, it is significantly faster. 10 - 25 %.
-- I could live with the 10, but 25 is too much.
--
-- Hmm, maybe an implementation via
-- replace pat sub = L.intercalate sub . split pat
-- would be competitive now.
-- TODO: test speed and space usage.
--
-- replacing loop for lazy ByteStrings as list of chunks,
-- called only for non-empty patterns
lazyRepl :: S.ByteString -> ([S.ByteString] -> [S.ByteString])
            -> [S.ByteString] -> [S.ByteString]
lazyRepl pat = replacer
 where
  !patLen = S.length pat
  !patEnd = patLen - 1
  !occT   = occurs pat
  !suffT  = suffShifts pat
  !maxLen = maxBound - patLen
  !pe     = patAt patEnd

  {-# INLINE patAt #-}
  patAt !i = unsafeIndex pat i

  {-# INLINE occ #-}
  occ !w = unsafeAt occT (fromIntegral w)

  {-# INLINE suff #-}
  suff !i = unsafeAt suffT i

  replacer sub lst =
      case lst of
        []    -> []
        (h:t) ->
          if maxLen < S.length h
            then error "Overflow in BoyerMoore.lazyRepl"
            else seek [] h t 0 patEnd
   where
        chop _ [] = []
        chop !k (!str : rest)
          | k < s     =
            if maxLen < (s - k)
                then error "Overflow in BoyerMoore.lazyRepl (chop)"
                else seek [] (S.drop k str) rest 0 patEnd
          | otherwise = chop (k-s) rest
            where
              !s = S.length str

        seek :: [S.ByteString] -> S.ByteString -> [S.ByteString]
                                    -> Int -> Int -> [S.ByteString]
        seek !past !str fut !offset !patPos
          | strPos < 0 =
            case past of
              [] -> error "not enough past!"
              (h : t) -> seek t h (str : fut) (offset + S.length h) patPos
          | strEnd < strPos =
            case fut of
              []      -> foldr (flip (.) . (:)) id past [str]
              (h : t) ->
                let !off' = offset - strLen
                    (past', !discharge) = keep (-off') (str : past)
                in if maxLen < S.length h
                    then error "Overflow in BoyerMoore.lazyRepl (future)"
                    else foldr (flip (.) . (:)) id discharge $
                                            seek past' h t off' patPos
          | patPos == patEnd = checkEnd strPos
          | offset < 0 = matcherN offset patPos
          | otherwise  = matcherP offset patPos
            where
              {-# INLINE strAt #-}
              strAt !i = unsafeIndex str i

              !strLen = S.length str
              !strEnd = strLen - 1
              !maxOff = strLen - patLen
              !strPos = offset + patPos

              checkEnd !sI
                | strEnd < sI = seek past str fut (sI - patEnd) patEnd
                | otherwise   =
                  case strAt sI of
                    !c  | c == pe   ->
                          if sI < patEnd
                            then (if sI == 0
                              then seek past str fut (-patEnd) (patEnd - 1)
                              else matcherN (sI - patEnd) (patEnd - 1))
                          else matcherP (sI - patEnd) (patEnd - 1)
                        | otherwise -> checkEnd (sI + patEnd + occ c)

              matcherN !off !patI =
                case strAt (off + patI) of
                  !c  | c == patAt patI ->
                        if off + patI == 0
                          then seek past str fut off (patI - 1)
                          else matcherN off (patI - 1)
                      | otherwise ->
                        let !off' = off + max (suff patI) (patI + occ c)
                        in if maxOff < off'
                            then seek past str fut off' patEnd
                            else checkEnd (off' + patEnd)

              matcherP !off !patI =
                case strAt (off + patI) of
                  !c  | c == patAt patI ->
                        if patI == 0
                          then foldr (flip (.) . (:)) id past $
                            let pre = if off == 0
                                        then id
                                        else (S.take off str :)
                            in pre . sub $
                                let !p = off + patLen
                                in if p < strLen
                                    then seek [] (S.drop p str) fut 0 patEnd
                                    else chop (p - strLen) fut
                        else matcherP off (patI - 1)
                      | otherwise ->
                        let !off' = off + max (suff patI) (patI + occ c)
                        in if maxOff < off'
                            then seek past str fut off' patEnd
                            else checkEnd (off' + patEnd)
