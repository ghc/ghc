{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-- |
-- Module      : Data.Text.Internal.Search
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast substring search for 'Text', based on work by Boyer, Moore,
-- Horspool, Sunday, and Lundh.
--
-- References:
--
-- * R. S. Boyer, J. S. Moore: A Fast String Searching Algorithm.
--   Communications of the ACM, 20, 10, 762-772 (1977)
--
-- * R. N. Horspool: Practical Fast Searching in Strings.  Software -
--   Practice and Experience 10, 501-506 (1980)
--
-- * D. M. Sunday: A Very Fast Substring Search Algorithm.
--   Communications of the ACM, 33, 8, 132-142 (1990)
--
-- * F. Lundh: The Fast Search Algorithm.
--   <http://effbot.org/zone/stringlib.htm> (2006)

module Data.Text.Internal.Search
    (
      indices
    ) where

import qualified Data.Text.Array as A
import Data.Word (Word64)
import Data.Text.Internal (Text(..))
import Data.Bits ((.|.), (.&.))
import Data.Text.Internal.Unsafe.Shift (shiftL)

data T = {-# UNPACK #-} !Word64 :* {-# UNPACK #-} !Int

-- | /O(n+m)/ Find the offsets of all non-overlapping indices of
-- @needle@ within @haystack@.  The offsets returned represent
-- uncorrected indices in the low-level \"needle\" array, to which its
-- offset must be added.
--
-- In (unlikely) bad cases, this algorithm's complexity degrades
-- towards /O(n*m)/.
indices :: Text                -- ^ Substring to search for (@needle@)
        -> Text                -- ^ Text to search in (@haystack@)
        -> [Int]
indices _needle@(Text narr noff nlen) _haystack@(Text harr hoff hlen)
    | nlen == 1              = scanOne (nindex 0)
    | nlen <= 0 || ldiff < 0 = []
    | otherwise              = scan 0
  where
    ldiff    = hlen - nlen
    nlast    = nlen - 1
    z        = nindex nlast
    nindex k = A.unsafeIndex narr (noff+k)
    hindex k = A.unsafeIndex harr (hoff+k)
    hindex' k | k == hlen  = 0
              | otherwise = A.unsafeIndex harr (hoff+k)
    buildTable !i !msk !skp
        | i >= nlast           = (msk .|. swizzle z) :* skp
        | otherwise            = buildTable (i+1) (msk .|. swizzle c) skp'
        where c                = nindex i
              skp' | c == z    = nlen - i - 2
                   | otherwise = skp
    swizzle k = 1 `shiftL` (fromIntegral k .&. 0x3f)
    scan !i
        | i > ldiff                  = []
        | c == z && candidateMatch 0 = i : scan (i + nlen)
        | otherwise                  = scan (i + delta)
        where c = hindex (i + nlast)
              candidateMatch !j
                    | j >= nlast               = True
                    | hindex (i+j) /= nindex j = False
                    | otherwise                = candidateMatch (j+1)
              delta | nextInPattern = nlen + 1
                    | c == z        = skip + 1
                    | otherwise     = 1
                where nextInPattern = mask .&. swizzle (hindex' (i+nlen)) == 0
              !(mask :* skip)       = buildTable 0 0 (nlen-2)
    scanOne c = loop 0
        where loop !i | i >= hlen     = []
                      | hindex i == c = i : loop (i+1)
                      | otherwise     = loop (i+1)
{-# INLINE indices #-}
