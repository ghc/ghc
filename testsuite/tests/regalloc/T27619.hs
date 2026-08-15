{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-cse #-}

-- The loop-invariant v must be live at VecFormat 2 FmtDouble on entry to
-- every block of the loop:
--
--     +--> H: reads lane 0 of v, at FF64 -----> exit
--     |    |
--     |    v
--     |    X: reads lane 0 of v, at FF64 -----> exit
--     |    |
--     |    v
--     +--- W: reads both lanes of v, at VecFormat 2 FmtDouble
--
-- The wide read reaches X and H only through the back edge, one block per
-- sweep, so the live registers stabilise before the wide format reaches H.
-- See Note [Liveness fixpoint convergence test] in GHC.CmmToAsm.Reg.Liveness.
--
-- -fno-cse keeps the identical lane-0 extracts in H and X from being merged.
module T27619 where

import GHC.Exts

loop :: Int# -> DoubleX2# -> Double# -> Double#
loop i v acc =
  case unpackDoubleX2# v of                -- H
    (# a1, _ #) ->
      if isTrue# (a1 <## int2Double# i)
      then acc
      else case unpackDoubleX2# v of       -- X
        (# a2, _ #) ->
          if isTrue# (a2 *## 2.0## <## int2Double# i)
          then acc *## 2.0##
          else case unpackDoubleX2# v of   -- W
            (# x, y #) -> loop (i -# 1#) v (acc +## (x *## y))
{-# NOINLINE loop #-}
