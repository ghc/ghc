{-# LANGUAGE MagicHash, UnboxedTuples #-}
module CopyLoopHeadsCall (count) where

import GHC.Exts
import GHC.IO (unIO)

foreign import ccall unsafe "srand" c_srand :: Word -> IO ()

-- go is a join point whose head compares i with 1000003 and branches.  Three
-- blocks jump to the head, each after a C call: count's entry and the loop's
-- two back edges.  Block layout lets one of them fall through, and the other
-- two end in a copy of the head (Note [Copying loop heads] in
-- GHC.CmmToAsm.X86.CodeGen), under -g too: there a C call leaves unwinding
-- information that names the C stack pointer alone at the end of each of
-- the three blocks, which keeps none of them from taking the copy, the
-- blocks' rule for Sp being the head's.  The test counts the comparisons in
-- the assembly, three with -g as without it.
count :: Word# -> State# RealWorld -> State# RealWorld
count n s0 = case unIO (c_srand (W# n)) s0 of
  (# s1, _ #) -> go n s1
  where
    go :: Word# -> State# RealWorld -> State# RealWorld
    go i s
      | isTrue# (i `gtWord#` 1000003##) = s
      | isTrue# (and# i 1## `eqWord#` 0##)
      = case unIO (c_srand (W# i)) s of (# s', _ #) -> go (i `plusWord#` 1##) s'
      | otherwise
      = case unIO (c_srand (W# n)) s of (# s', _ #) -> go (i `plusWord#` 3##) s'
