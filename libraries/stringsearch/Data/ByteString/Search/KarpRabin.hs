{-# LANGUAGE BangPatterns #-}
-- |
-- Module         : Data.ByteString.Search.KarpRabin
-- Copyright      : (c) 2010 Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : non-portable (BangPatterns)
--
-- Simultaneous search for multiple patterns in a strict 'S.ByteString'
-- using the Karp-Rabin algorithm.
--
-- A description of the algorithm for a single pattern can be found at
-- <http://www-igm.univ-mlv.fr/~lecroq/string/node5.html#SECTION0050>.
module Data.ByteString.Search.KarpRabin ( -- * Overview
                                          -- $overview

                                          -- ** Caution
                                          -- $caution

                                          -- * Function
                                          indicesOfAny
                                        ) where

import qualified Data.ByteString as S
import Data.ByteString.Unsafe (unsafeIndex)

import qualified Data.IntMap as IM

import Data.Array
import Data.Array.Base (unsafeAt)

import Data.Word (Word8)
import Data.Bits
import Data.List (foldl')


-- $overview
--
-- The Karp-Rabin algorithm works by calculating a hash of the pattern and
-- comparing that hash with the hash of a slice of the target string with
-- the same length as the pattern. If the hashes are equal, the slice of the
-- target is compared to the pattern byte for byte (since the hash
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
-- In summary, this module is more of an interesting curiosity than anything
-- else.

-- | @'indicesOfAny'@ finds all occurrences of any of several non-empty patterns
--   in a strict target string. If no non-empty patterns are given,
--   the result is an empty list. Otherwise the result list contains
--   the pairs of all indices where any of the (non-empty) patterns start
--   and the list of all patterns starting at that index, the patterns being
--   represented by their (zero-based) position in the pattern list.
--   Empty patterns are filtered out before processing begins.
{-# INLINE indicesOfAny #-}
indicesOfAny :: [S.ByteString]  -- ^ List of non-empty patterns
             -> S.ByteString    -- ^ String to search
             -> [(Int,[Int])]   -- ^ List of matches
indicesOfAny pats
    | null nepats   = const []
    | otherwise     = strictMatcher nepats
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

strictMatcher :: [S.ByteString] -> S.ByteString -> [(Int,[Int])]
strictMatcher pats = search
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
    search str
        | strLen < hLen   = []
        | otherwise = go 0 shash
          where
            !strLen = S.length str
            !maxIdx = strLen - hLen
            {-# INLINE strAt #-}
            strAt !i = unsafeIndex str i
            !shash = hash str
            go !sI !h =
              case IM.lookup h hashMap of
                Nothing ->
                  if sI == maxIdx
                    then []
                    else go (sI + 1) (rehash h (strAt sI) (strAt (sI + hLen)))
                Just ps ->
                  let !rst = S.drop sI str
                      {-# INLINE hd #-}
                      hd  = strAt sI
                      {-# INLINE more #-}
                      more = if sI == maxIdx then [] else
                                go (sI + 1) (rehash h hd (strAt (sI + hLen)))
                      {-# INLINE okay #-}
                      okay bs = S.isPrefixOf bs rst
                  in case filter (okay . (patArr `unsafeAt`)) ps of
                           [] -> more
                           qs -> seq (length qs) $
                                (sI,qs) : more
