{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-- |
-- Module      : Data.Text.Lazy.Search
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fast substring search for lazy 'Text', based on work by Boyer,
-- Moore, Horspool, Sunday, and Lundh.  Adapted from the strict
-- implementation.

module Data.Text.Internal.Lazy.Search
    (
      indices
    ) where

import qualified Data.Text.Array as A
import Data.Int (Int64)
import Data.Word (Word16, Word64)
import qualified Data.Text.Internal as T
import Data.Text.Internal.Fusion.Types (PairS(..))
import Data.Text.Internal.Lazy (Text(..), foldlChunks)
import Data.Bits ((.|.), (.&.))
import Data.Text.Internal.Unsafe.Shift (shiftL)

-- | /O(n+m)/ Find the offsets of all non-overlapping indices of
-- @needle@ within @haystack@.
--
-- This function is strict in @needle@, and lazy (as far as possible)
-- in the chunks of @haystack@.
--
-- In (unlikely) bad cases, this algorithm's complexity degrades
-- towards /O(n*m)/.
indices :: Text              -- ^ Substring to search for (@needle@)
        -> Text              -- ^ Text to search in (@haystack@)
        -> [Int64]
indices needle@(Chunk n ns) _haystack@(Chunk k ks)
    | nlen <= 0  = []
    | nlen == 1  = indicesOne (nindex 0) 0 k ks
    | otherwise  = advance k ks 0 0
  where
    advance x@(T.Text _ _ l) xs = scan
     where
      scan !g !i
         | i >= m = case xs of
                      Empty           -> []
                      Chunk y ys      -> advance y ys g (i-m)
         | lackingHay (i + nlen) x xs  = []
         | c == z && candidateMatch 0  = g : scan (g+nlen) (i+nlen)
         | otherwise                   = scan (g+delta) (i+delta)
       where
         m = fromIntegral l
         c = hindex (i + nlast)
         delta | nextInPattern = nlen + 1
               | c == z        = skip + 1
               | otherwise     = 1
         nextInPattern         = mask .&. swizzle (hindex (i+nlen)) == 0
         candidateMatch !j
             | j >= nlast               = True
             | hindex (i+j) /= nindex j = False
             | otherwise                = candidateMatch (j+1)
         hindex                         = index x xs
    nlen      = wordLength needle
    nlast     = nlen - 1
    nindex    = index n ns
    z         = foldlChunks fin 0 needle
        where fin _ (T.Text farr foff flen) = A.unsafeIndex farr (foff+flen-1)
    (mask :: Word64) :*: skip = buildTable n ns 0 0 0 (nlen-2)
    swizzle w = 1 `shiftL` (fromIntegral w .&. 0x3f)
    buildTable (T.Text xarr xoff xlen) xs = go
      where
        go !(g::Int64) !i !msk !skp
            | i >= xlast = case xs of
                             Empty      -> (msk .|. swizzle z) :*: skp
                             Chunk y ys -> buildTable y ys g 0 msk' skp'
            | otherwise = go (g+1) (i+1) msk' skp'
            where c                = A.unsafeIndex xarr (xoff+i)
                  msk'             = msk .|. swizzle c
                  skp' | c == z    = nlen - g - 2
                       | otherwise = skp
                  xlast = xlen - 1
    -- | Check whether an attempt to index into the haystack at the
    -- given offset would fail.
    lackingHay q = go 0
      where
        go p (T.Text _ _ l) ps = p' < q && case ps of
                                             Empty      -> True
                                             Chunk r rs -> go p' r rs
            where p' = p + fromIntegral l
indices _ _ = []

-- | Fast index into a partly unpacked 'Text'.  We take into account
-- the possibility that the caller might try to access one element
-- past the end.
index :: T.Text -> Text -> Int64 -> Word16
index (T.Text arr off len) xs !i
    | j < len   = A.unsafeIndex arr (off+j)
    | otherwise = case xs of
                    Empty
                        -- out of bounds, but legal
                        | j == len  -> 0
                        -- should never happen, due to lackingHay above
                        | otherwise -> emptyError "index"
                    Chunk c cs -> index c cs (i-fromIntegral len)
    where j = fromIntegral i

-- | A variant of 'indices' that scans linearly for a single 'Word16'.
indicesOne :: Word16 -> Int64 -> T.Text -> Text -> [Int64]
indicesOne c = chunk
  where
    chunk !i (T.Text oarr ooff olen) os = go 0
      where
        go h | h >= olen = case os of
                             Empty      -> []
                             Chunk y ys -> chunk (i+fromIntegral olen) y ys
             | on == c = i + fromIntegral h : go (h+1)
             | otherwise = go (h+1)
             where on = A.unsafeIndex oarr (ooff+h)

-- | The number of 'Word16' values in a 'Text'.
wordLength :: Text -> Int64
wordLength = foldlChunks sumLength 0
    where sumLength i (T.Text _ _ l) = i + fromIntegral l

emptyError :: String -> a
emptyError fun = error ("Data.Text.Lazy.Search." ++ fun ++ ": empty input")
