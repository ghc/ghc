{-# LANGUAGE MagicHash #-}
module T27991 (f) where

import GHC.Exts

-- #27991.  f's self-recursive tail call is a jump to a loop header.  n and i
-- are passed on the stack and loaded into registers at the entry; the loop
-- changes only i and reads neither slot.  The header took the entry's stack
-- map, in which each slot holds its argument's register, but the back edge
-- assigns every argument register, if only to itself, which leaves no slot
-- holding its register's value there, so stack layout stored n and i to
-- their slots at every iteration.  A loop head now takes the stack map its
-- back edges have in common (Note [Loop head stack maps] in
-- GHC.Cmm.LayoutStack): the test checks the assembly for a movq of a register
-- to an address off %rbp, Sp's register, of which f makes none.
f :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int#
f a b c d e n i = case i >=# n of
  1# -> a +# b +# c +# d +# e
  _ -> f a b c d e n (i +# 1#)
