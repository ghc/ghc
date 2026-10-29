{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- The native code for this loop is a three-block cycle H -> X -> W -> H:
--
--   H, X: read v at FF64   (lane-0 extracts)
--   W:    reads v at F64x2 (the full unpack)
--
-- v is loop-invariant, so its live format on entry to every block in the
-- cycle must be F64x2. -fno-cse keeps the two syntactically identical
-- lane-0 extracts from being merged.
module T27619 where

import GHC.Exts

loop :: Int# -> DoubleX2# -> Double# -> Double#
loop i v acc =
  case unpackDoubleX2# v of
    (# a1, _ #) ->
      if isTrue# (a1 <## int2Double# i)
      then acc
      else case unpackDoubleX2# v of
        (# a2, _ #) ->
          if isTrue# (a2 *## 2.0## <## int2Double# i)
          then acc *## 2.0##
          else case unpackDoubleX2# v of
            (# x, y #) -> loop (i -# 1#) v (acc +## (x *## y))
{-# NOINLINE loop #-}
