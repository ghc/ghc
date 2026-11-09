-- Two word2Double# conversions in one Cmm block. The NCG gave the second
-- conversion the wrong current block, so its diamond was in the CFG before
-- the first one, but after it in the code. Branch shortcutting (on at -O2)
-- then left blocks in the CFG that no edge reached, and classifyEdges
-- panicked. -dasm-lint checks that the jumps and the CFG edges agree.
{-# LANGUAGE MagicHash #-}
module T27694d where

import GHC.Exts

f :: Double# -> Word# -> Word# -> Double#
f c x y = go (word2Double# x) (word2Double# y)
  where
    go a b = case a >## c of
      1# -> a
      _  -> go (a *## b) b
