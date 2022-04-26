
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module T21239 where

import GHC.Exts

-- This test goes wrong if we don't properly decompose
-- when unifying ConcreteTv metavariables.

atomicModifyMutVar#
  :: MutVar# s a
  -> (a -> b)
  -> State# s
  -> (# State# s, c #)
atomicModifyMutVar# mv f s =
  case unsafeCoerce# (atomicModifyMutVar2# mv f s) of
    (# s', _, ~(_, res) #) -> (# s', res #)
