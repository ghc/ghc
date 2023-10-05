{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts
import GHC.Base

-- Try to overflow BLOCK_ROUND_UP in the computation of req_blocks in allocate()
-- Here we invoke allocate() via newByteArray#.
-- Request a number of bytes close to HS_WORD_MAX,
-- subtracting a few words for overhead in newByteArray#.
main :: IO ()
main =
    IO $ \s1# ->
           case newByteArray# (maxInt# -# 10#) s1# of
             (# s2#, _ #) -> (# s2#, () #)
  where
    maxInt# :: Int#
    !(I# maxInt#) = maxBound
