{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

-- Test that -fcheck-prim-bounds reports a failing *range* access (count > 1)
-- with the "range of N elements starting at index" wording. The source range
-- is in bounds, but the destination range [6, 10) overruns the 8-byte array.

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = IO $ \s0 ->
  case newByteArray# 8# s0 of
    (# s1, src #) ->
      case newByteArray# 8# s1 of
        (# s2, dst #) ->
          case copyMutableByteArray# src 0# dst 6# 4# s2 of
            s3 -> (# s3, () #)
