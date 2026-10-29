{-# LANGUAGE MagicHash, UnboxedTuples #-}
module T27981 (V (..), nf) where

import GHC.Exts

-- #27981.  The loop in $wnf evaluates each element.  Where the element is
-- tagged, the block that evaluates it branches to the loop's own return
-- continuation with R1 (%rbx) still allocated: R1 is not live into the
-- continuation, but dies only at the jump that enters an untagged element.
-- The loop is entered from the continuation of the call to ping, with the
-- array in %rbx, so the continuation could not reload the array there; it
-- reloaded it and the counter into two other registers, and the edge back
-- to the loop's header paid two fixup moves at every iteration.  The test
-- checks the register allocator's statistics for fixup moves (joinRR and
-- joinRM in spills-added-total), of which the loop now needs none.

data V = V [Int] Int () (Array# Double)

{-# NOINLINE ping #-}
ping :: [Int] -> Int
ping = foldr (\_ c -> c + 1) 0

nf :: V -> ()
nf (V sh n e arr) = case ping sh of
  I# 0# -> n `seq` e `seq` ()
  I# _ -> case n of
    I# n# -> e `seq`
      let go i | isTrue# (i >=# n#) = ()
               | otherwise = case indexArray# arr i of
                   (# x #) -> case x of D# _ -> go (i +# 1#)
      in go 0#
{-# NOINLINE nf #-}
