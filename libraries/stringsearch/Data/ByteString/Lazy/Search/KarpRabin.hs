{-# LANGUAGE BangPatterns #-}
-- |
-- Module         : Data.ByteString.Lazy.Search.KarpRabin
-- Copyright      : (c) 2010 Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Simultaneous search for multiple patterns in a lazy 'L.ByteString'
-- using the Karp-Rabin algorithm.
--
-- A description of the algorithm for a single pattern can be found at
-- <http://www-igm.univ-mlv.fr/~lecroq/string/node5.html#SECTION0050>.
module Data.ByteString.Lazy.Search.KarpRabin ( -- * Overview
                                               -- $overview

                                               -- ** Caution
                                               -- $caution

                                               -- * Function
                                               indicesOfAny
                                             ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe (unsafeIndex)

import qualified Data.IntMap as IM

import Data.Array
import Data.Array.Base (unsafeAt)

import Data.Word (Word8)
import Data.Int (Int64)
import Data.Bits
import Data.List (foldl')

-- $overview
--
-- The Karp-Rabin algorithm works by calculating a hash of the pattern and
-- comparing that hash with the hash of a slice of the target string with
-- the same length as the pattern. If the hashes are equal, the slice of the
-- target is compared to the pattern character by character (since the hash
-- function generally isn't injective).
--
-- For a single pattern, this tends to be more efficient than the na&#239;ve
-- algorithm, but it cannot compete with algorithms like
-- Knuth-Morris-Pratt or Boyer-Moore.
--
-- However, the algorithm can be generalised to search for multiple patterns
-- simultaneously. If the shortest pattern has length @k@, hash the prefix of
-- length @k@ of all patterns and compare the hash of the target's slices of
-- length @k@ to them. If there's a match, check whether the slice is part
-- of an occurrence of the corresponding pattern.
--
-- With a hash-function that
--
--   * allows to compute the hash of one slice in constant time from the hash
--     of the previous slice, the new and the dropped character, and
--
--   * produces few spurious matches,
--
-- searching for occurrences of any of @n@ patterns has a best-case complexity
-- of /O/(@targetLength@ * @lookup n@). The worst-case complexity is
-- /O/(@targetLength@ * @lookup n@ * @sum patternLengths@), the average is
-- not much worse than the best case.
--
-- The functions in this module store the hashes of the patterns in an
-- 'IM.IntMap', so the lookup is /O/(@log n@). Re-hashing is done in constant
-- time and spurious matches of the hashes /should be/ sufficiently rare.
-- The maximal length of the prefixes to be hashed is 32.

-- $caution
--
-- Unfortunately, the constant factors are high, so these functions are slow.
-- Unless the number of patterns to search for is high (larger than 50 at
-- least), repeated search for single patterns using Boyer-Moore or DFA and
-- manual merging of the indices is faster. /Much/ faster for less than 40
-- or so patterns.
--
-- 'indicesOfAny' has the advantage over multiple single-pattern searches that
-- it doesn't hold on to large parts of the string (which is likely to happen
-- for multiple searches), however, so in contrast to the strict version, it
-- may be useful for relatively few patterns already.
--
-- Nevertheless, this module seems more of an interesting curiosity than
-- anything else.

-- | @'indicesOfAny'@ finds all occurrences of any of several non-empty strict
--   patterns in a lazy target string. If no non-empty patterns are given,
--   the result is an empty list. Otherwise the result list contains
--   the pairs of all indices where any of the (non-empty) patterns start
--   and the list of all patterns starting at that index, the patterns being
--   represented by their (zero-based) position in the pattern list.
--   Empty patterns are filtered out before processing begins.
{-# INLINE indicesOfAny #-}
indicesOfAny :: [S.ByteString]  -- ^ List of non-empty patterns
             -> L.ByteString    -- ^ String to search
             -> [(Int64,[Int])]   -- ^ List of matches
indicesOfAny pats
    | null nepats   = const []
    | otherwise     = lazyMatcher nepats . L.toChunks
      where
        nepats = filter (not . S.null) pats

------------------------------------------------------------------------------
--                                 Workers                                 --
------------------------------------------------------------------------------

{-# INLINE rehash1 #-}
rehash1 :: Int -> Int -> Word8 -> Word8 -> Int
rehash1 out h o n =
    (h `shiftL` 1 - (fromIntegral o `shiftL` out)) + fromIntegral n

{-# INLINE rehash2 #-}
rehash2 :: Int -> Int -> Word8 -> Word8 -> Int
rehash2 out h o n =
    (h `shiftL` 2 - (fromIntegral o `shiftL` out)) + fromIntegral n

{-# INLINE rehash3 #-}
rehash3 :: Int -> Int -> Word8 -> Word8 -> Int
rehash3 out h o n =
    (h `shiftL` 3 - (fromIntegral o `shiftL` out)) + fromIntegral n

{-# INLINE rehash4 #-}
rehash4 :: Int -> Int -> Word8 -> Word8 -> Int
rehash4 out h o n =
    (h `shiftL` 4 - (fromIntegral o `shiftL` out)) + fromIntegral n

lazyMatcher :: [S.ByteString] -> [S.ByteString] -> [(Int64,[Int])]
lazyMatcher pats = search 0 hLen S.empty
  where
    !hLen = minimum (32 : map S.length pats)
    !shDi = case 32 `quot` hLen of
              q | q < 4 -> q
                | otherwise -> 4
    !outS = shDi*hLen
    !patNum = length pats
    !patArr = listArray (0, patNum - 1) pats
    {-# INLINE rehash #-}
    rehash :: Int -> Word8 -> Word8 -> Int
    rehash = case shDi of
                1 -> rehash1 hLen
                2 -> rehash2 outS
                3 -> rehash3 outS
                _ -> rehash4 outS
    hash :: S.ByteString -> Int
    hash = S.foldl' (\h w -> (h `shiftL` shDi) + fromIntegral w) 0 . S.take hLen
    !hashMap =
        foldl' (\mp (h,i) -> IM.insertWith (flip (++)) h [i] mp) IM.empty $
                                zip (map hash pats) [0 :: Int .. ]
    search _ _ _ [] = []
    search !h !rm !prev (!str : rest)
      | strLen < rm =
          let !h' = S.foldl' (\o w -> (o `shiftL` 1) + fromIntegral w) h str
              !prev' = S.append prev str
          in search h' (rm - strLen) prev' rest
      | otherwise =
          let !h' = S.foldl' (\o w -> (o `shiftL` 1) + fromIntegral w) h
                                                (S.take rm str)
          in if S.null prev
                then noPast 0 rest str h'
                else past 0 rest prev 0 str rm h'
        where
          !strLen = S.length str

    noPast !prior rest !str hsh = go hsh 0
      where
        !strLen = S.length str
        !maxIdx = strLen - hLen
        {-# INLINE strAt #-}
        strAt !i = unsafeIndex str i
        go !h sI =
          case IM.lookup h hashMap of
            Nothing ->
              if sI == maxIdx
                then case rest of
                        [] -> []
                        (nxt : more) ->
                          let !h' = rehash h (strAt sI) (unsafeIndex nxt 0)
                              !prior' = prior + fromIntegral strLen
                              !prev = S.drop (sI + 1) str
                          in if hLen == 1
                                then noPast prior' more nxt h'
                                else past prior' more prev 0 nxt 1 h'
                else go (rehash h (strAt sI) (strAt (sI + hLen))) (sI + 1)
            Just ps ->
              let !rst = S.drop sI str
                  !rLen = strLen - sI
                  {-# INLINE hd #-}
                  hd = strAt sI
                  {-# INLINE more #-}
                  more =
                    if sI == maxIdx
                      then case rest of
                            [] -> []
                            (nxt : fut) ->
                              let !h' = rehash h hd (unsafeIndex nxt 0)
                                  !prior' = prior + fromIntegral strLen
                              in if hLen == 1
                                    then noPast prior' fut nxt h'
                                    else past prior' fut rst 1 nxt 1 h'
                      else go (rehash h hd (strAt (sI + hLen))) (sI + 1)
                  okay bs
                    | rLen < S.length bs = S.isPrefixOf rst bs &&
                            checkFut (S.drop rLen bs) rest
                    | otherwise = S.isPrefixOf bs rst
              in case filter (okay . (patArr `unsafeAt`)) ps of
                    [] -> more
                    qs -> seq (length qs) $
                            (prior + fromIntegral sI,qs) : more

    past !prior rest !prev !pI !str !sI !hsh
      | strLen < 4040 =
        let !prior' = prior - 1 + fromIntegral (sI - hLen)
            !curr   = S.append (S.drop pI prev) str
        in noPast prior' rest curr hsh
      | otherwise = go hsh pI sI
        where
          !strLen = S.length str
          {-# INLINE strAt #-}
          strAt !i = unsafeIndex str i
          {-# INLINE prevAt #-}
          prevAt !i = unsafeIndex prev i
          go !h !p !s
            | s == hLen = noPast prior rest str h
            | otherwise =
              case IM.lookup h hashMap of
                Nothing ->
                  let {-# INLINE h' #-}
                      h' = rehash h (prevAt p) (strAt s)
                  in go h' (p + 1) (s + 1)
                Just ps ->
                  let !prst = S.drop p prev
                      {-# INLINE more #-}
                      more = go (rehash h (prevAt p) (strAt s)) (p + 1) (s + 1)
                      okay bs = checkFut bs (prst : str : rest)
                  in case filter (okay . (unsafeAt patArr)) ps of
                        [] -> more
                        qs -> seq (length qs) $
                                (prior + fromIntegral (s - hLen), qs) : more

{-# INLINE checkFut #-}
checkFut :: S.ByteString -> [S.ByteString] -> Bool
checkFut _ [] = False
checkFut !bs (!h : t)
    | hLen < S.length bs = S.isPrefixOf h bs && checkFut (S.drop hLen bs) t
    | otherwise = S.isPrefixOf bs h
      where
        !hLen = S.length h
