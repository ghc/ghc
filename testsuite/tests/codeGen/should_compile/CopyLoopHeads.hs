{-# LANGUAGE MagicHash #-}
module CopyLoopHeads (sumTo) where

import GHC.Exts

-- go is a join point: sumTo's entry sets acc and i and jumps to the loop's
-- head, which compares i with 1000003 and branches, and the loop's back
-- edge jumps to the head too; on Int#, the loop allocates nothing, so that
-- no heap check comes before the comparison.  Of the two jumps, the one
-- block layout lets fall through is kept and the other block ends in a copy
-- of the head (Note [Copying loop heads] in GHC.CmmToAsm.X86.CodeGen): the
-- test counts the comparisons in the assembly, two by default and one under
-- -fno-asm-copy-loop-heads.
sumTo :: Int# -> Int#
sumTo n = go 0# 1#
  where
    go :: Int# -> Int# -> Int#
    go acc i = case i ># 1000003# of
      1# -> acc
      _  -> go (acc +# i *# n) (i +# 1#)
