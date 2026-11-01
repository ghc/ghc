{-# LANGUAGE MagicHash #-}
module StackLayoutTagLoop (allPos) where

import GHC.Exts

-- The loop evaluates the list and then its head, two diamonds (a tag test
-- with a call returning to a continuation) whose frames differ: nothing is
-- live across the list's evaluation, xs across the head's.  Stack layout gives
-- the loop's head the depth of its back edges (Note [Loop head stack maps] in
-- GHC.Cmm.LayoutStack) and the list's frame keeps that depth, a hole in xs's
-- place (Note [Diamond frames keep their depth]), so Sp moves where the loop
-- is entered and at its two exits, and nowhere in the loop: the test checks
-- the assembly for those three moves of %rbp, Sp's register, and no other.
-- Before, every iteration moved Sp down at each diamond and back up at the
-- loop's back edge.
allPos :: [Int] -> Int#
allPos []       = 1#
allPos (x : xs) = case x of
  I# x# -> case x# ># 0# of
    1# -> allPos xs
    _  -> 0#
