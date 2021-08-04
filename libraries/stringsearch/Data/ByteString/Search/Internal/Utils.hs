{-# LANGUAGE BangPatterns, FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide, prune #-}
-- |
-- Module         : Data.ByteString.Search.Internal.Utils
-- Copyright      : Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portabiltity   : non-portable
--
-- Author         : Daniel Fischer
--
-- Utilities for several searching algorithms.

module Data.ByteString.Search.Internal.Utils ( kmpBorders
                                             , automaton
                                             , occurs
                                             , suffShifts
                                             , ldrop
                                             , ltake
                                             , lsplit
                                             , release
                                             , keep
                                             , strictify
                                             ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe (unsafeIndex)

import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad (when)

import Data.Bits
import Data.Word (Word8)

------------------------------------------------------------------------------
--                              Preprocessing                               --
------------------------------------------------------------------------------

{-# INLINE automaton #-}
automaton :: S.ByteString -> UArray Int Int
automaton !pat = runSTUArray (do
    let !patLen = S.length pat
        {-# INLINE patAt #-}
        patAt !i = fromIntegral (unsafeIndex pat i)
        !bord = kmpBorders pat
    aut <- newArray (0, (patLen + 1)*256 - 1) 0
    unsafeWrite aut (patAt 0) 1
    let loop !state = do
            let !base = state `shiftL` 8
                inner j
                    | j < 0     = if state == patLen
                                    then return aut
                                    else loop (state+1)
                    | otherwise = do
                        let !i = base + patAt j
                        s <- unsafeRead aut i
                        when (s == 0) (unsafeWrite aut i (j+1))
                        inner (unsafeAt bord j)
            if state == patLen
                then inner (unsafeAt bord state)
                else inner state
    loop 1)

-- kmpBorders calculates the width of the widest borders of the prefixes
-- of the pattern which are not extensible to borders of the next
-- longer prefix. Most entries will be 0.
{-# INLINE kmpBorders #-}
kmpBorders :: S.ByteString -> UArray Int Int
kmpBorders pat = runSTUArray (do
    let !patLen = S.length pat
        {-# INLINE patAt #-}
        patAt :: Int -> Word8
        patAt i = unsafeIndex pat i
    ar <- newArray_ (0, patLen)
    unsafeWrite ar 0 (-1)
    let dec w j
            | j < 0 || w == patAt j = return $! j+1
            | otherwise = unsafeRead ar j >>= dec w
        bordLoop !i !j
            | patLen < i    = return ar
            | otherwise     = do
                let !w = patAt (i-1)
                j' <- dec w j
                if i < patLen && patAt j' == patAt i
                    then unsafeRead ar j' >>= unsafeWrite ar i
                    else unsafeWrite ar i j'
                bordLoop (i+1) j'
    bordLoop 1 (-1))

------------------------------------------------------------------------------
--                        Boyer-Moore Preprocessing                         --
------------------------------------------------------------------------------

{- Table of last occurrences of bytes in the pattern.

For each byte we record the (negated) position of its last
occurrence in the pattern except at the last position.

Thus, if byte b gives a mismatch at pattern position patPos,
we know that we can shift the window right by at least

patPos - (last occurrence of b in init pat)

or, since we negated the positions,

patPos + (occurs pat)

If the byte doesn't occur in the pattern, we can shift the window
so that the start of the pattern is aligned with the byte after this,
hence the default value of 1.

Complexity: O(patLen + size of alphabet)

-}
{- Precondition: non-empty pattern

This invariant is guaranteed by not exporting occurs,
inside this module, we don't call it for empty patterns.

-}
{-# INLINE occurs #-}
occurs :: S.ByteString -> UArray Int Int
occurs pat = runSTUArray (do
    let !patEnd = S.length pat - 1
        {-# INLINE patAt #-}
        patAt :: Int -> Int
        patAt i = fromIntegral (unsafeIndex pat i)
    ar <- newArray (0, 255) 1
    let loop !i
            | i == patEnd   = return ar
            | otherwise     = do
                unsafeWrite ar (patAt i) (-i)
                loop (i + 1)
    loop 0)

{- Table of suffix-shifts.

When a mismatch occurs at pattern position patPos, assumed to be not the
last position in the pattern, the suffix u of length (patEnd - patPos)
has been successfully matched.
Let c be the byte in the pattern at position patPos.

If the sub-pattern u also occurs in the pattern somewhere *not* preceded
by c, let uPos be the position of the last byte in u for the last of
all such occurrences. Then there can be no match if the window is shifted
less than (patEnd - uPos) places, because either the part of the string
which matched the suffix u is not aligned with an occurrence of u in the
pattern, or it is aligned with an occurrence of u which is preceded by
the same byte c as the originally matched suffix.

If the complete sub-pattern u does not occur again in the pattern, or all
of its occurrences are preceded by the byte c, then we can align the
pattern with the string so that a suffix v of u matches a prefix of the
pattern. If v is chosen maximal, no smaller shift can give a match, so
we can shift by at least (patLen - length v).

If a complete match is encountered, we can shift by at least the same
amount as if the first byte of the pattern was a mismatch, no complete
match is possible between these positions.

For non-periodic patterns, only very short suffixes will usually occur
again in the pattern, so if a longer suffix has been matched before a
mismatch, the window can then be shifted entirely past the partial
match, so that part of the string will not be re-compared.
For periodic patterns, the suffix shifts will be shorter in general,
leading to an O(strLen * patLen) worst-case performance.

To compute the suffix-shifts, we use an array containing the lengths of
the longest common suffixes of the entire pattern and its prefix ending
with position pos.

-}
{- Precondition: non-empty pattern -}
{-# INLINE suffShifts #-}
suffShifts :: S.ByteString -> UArray Int Int
suffShifts pat = runSTUArray (do
    let !patLen = S.length pat
        !patEnd = patLen - 1
        !suff   = suffLengths pat
    ar <- newArray (0,patEnd) patLen
    let preShift !idx !j
            | idx < 0   = return ()
            | suff `unsafeAt` idx == idx + 1 = do
                let !shf = patEnd - idx
                    fillToShf !i
                        | i == shf  = return ()
                        | otherwise = do
                            unsafeWrite ar i shf
                            fillToShf (i + 1)
                fillToShf j
                preShift (idx - 1) shf
            | otherwise = preShift (idx - 1) j
        sufShift !idx
            | idx == patEnd = return ar
            | otherwise     = do
                unsafeWrite ar (patEnd - unsafeAt suff idx) (patEnd - idx)
                sufShift (idx + 1)
    preShift (patEnd - 1) 0
    sufShift 0)

{- Table of suffix-lengths.

The value of this array at place i is the length of the longest common
suffix of the entire pattern and the prefix of the pattern ending at
position i.

Usually, most of the entries will be 0. Only if the byte at position i
is the same as the last byte of the pattern can the value be positive.
In any case the value at index patEnd is patLen (since the pattern is
identical to itself) and 0 <= value at i <= (i + 1).

To keep this part of preprocessing linear in the length of the pattern,
the implementation must be non-obvious (the obvious algorithm for this
is quadratic).

When the index under consideration is inside a previously identified
common suffix, we align that suffix with the end of the pattern and
check whether the suffix ending at the position corresponding to idx
is shorter than the part of the suffix up to idx. If that is the case,
the length of the suffix ending at idx is that of the suffix at the
corresponding position. Otherwise extend the suffix as far as possible.
If the index under consideration is not inside a previously identified
common suffix, compare with the last byte of the pattern. If that gives
a suffix of length > 1, for the next index we're in the previous
situation, otherwise we're back in the same situation for the next
index.

-}
{- Precondition: non-empty pattern -}
{-# INLINE suffLengths #-}
suffLengths :: S.ByteString -> UArray Int Int
suffLengths pat = runSTUArray (do
    let !patLen = S.length pat
        !patEnd = patLen - 1
        !preEnd = patEnd - 1
        {-# INLINE patAt #-}
        patAt i = unsafeIndex pat i
        -- last byte for comparisons
        !pe     = patAt patEnd
        -- find index preceding the longest suffix
        dec !diff !j
            | j < 0 || patAt j /= patAt (j + diff) = j
            | otherwise = dec diff (j - 1)
    ar <- newArray_ (0, patEnd)
    unsafeWrite ar patEnd patLen
    let noSuff !i
            | i < 0     = return ar
            | patAt i == pe = do
                let !diff  = patEnd - i
                    !nextI = i - 1
                    !prevI = dec diff nextI
                if prevI == nextI
                    then unsafeWrite ar i 1 >> noSuff nextI
                    else do unsafeWrite ar i (i - prevI)
                            suffLoop prevI preEnd nextI
            | otherwise = do
                unsafeWrite ar i 0
                noSuff (i - 1)
        suffLoop !pre !end !idx
            | idx < 0   = return ar
            | pre < idx =
              if patAt idx /= pe
                then unsafeWrite ar idx 0 >> suffLoop pre (end - 1) (idx - 1)
                else do
                    prevS <- unsafeRead ar end
                    if pre + prevS < idx
                        then do unsafeWrite ar idx prevS
                                suffLoop pre (end - 1) (idx - 1)
                        else do let !prI = dec (patEnd - idx) pre
                                unsafeWrite ar idx (idx - prI)
                                suffLoop prI preEnd (idx - 1)
            | otherwise = noSuff idx
    noSuff preEnd)

------------------------------------------------------------------------------
--                             Helper Functions                             --
------------------------------------------------------------------------------

{-# INLINE strictify #-}
strictify :: L.ByteString -> S.ByteString
strictify = S.concat . L.toChunks

-- drop k bytes from a list of strict ByteStrings
{-# INLINE ldrop #-}
ldrop :: Int -> [S.ByteString] -> [S.ByteString]
ldrop _ [] = []
ldrop k (!h : t)
  | k < l     = S.drop k h : t
  | otherwise = ldrop (k - l) t
    where
      !l = S.length h

-- take k bytes from a list of strict ByteStrings
{-# INLINE ltake #-}
ltake :: Int -> [S.ByteString] -> [S.ByteString]
ltake _ [] = []
ltake !k (!h : t)
  | l < k     = h : ltake (k - l) t
  | otherwise = [S.take k h]
    where
      !l = S.length h

-- split a list of strict ByteStrings at byte k
{-# INLINE lsplit #-}
lsplit :: Int -> [S.ByteString] -> ([S.ByteString], [S.ByteString])
lsplit _ [] = ([],[])
lsplit !k (!h : t)
  = case compare k l of
      LT -> ([S.take k h], S.drop k h : t)
      EQ -> ([h], t)
      GT -> let (u, v) = lsplit (k - l) t in (h : u, v)
  where
    !l = S.length h


-- release is used to keep the zipper in lazySearcher from remembering
-- the leading part of the searched string.  The deep parameter is the
-- number of characters that the past needs to hold.  This ensures
-- lazy streaming consumption of the searched string.
{-# INLINE release #-}
release :: Int ->  [S.ByteString] -> [S.ByteString]
release !deep _
    | deep <= 0 = []
release !deep (!x:xs) = let !rest = release (deep-S.length x) xs in x : rest
release _ [] = error "stringsearch.release could not find enough past!"

-- keep is like release, only we mustn't forget the part of the past
-- we don't need anymore for matching but have to keep it for
-- breaking, splitting and replacing.
-- The names would be more appropriate the other way round, but that's
-- a historical accident, so what?
{-# INLINE keep #-}
keep :: Int -> [S.ByteString] -> ([S.ByteString],[S.ByteString])
keep !deep xs
    | deep < 1    = ([],xs)
keep deep (!x:xs) = let (!p,d) = keep (deep - S.length x) xs in (x:p,d)
keep _ [] = error "Forgot too much"
