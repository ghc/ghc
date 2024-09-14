{-# LANGUAGE BangPatterns, MagicHash, PolyKinds, RankNTypes, UnboxedTuples #-}

module GHC.Internal.IO.Magic (
  seq#,
  considerExceptionsPrecise#,
  evalBarrier#,
  hideEvalBarriers#,
) where

import GHC.Prim
import GHC.Magic

-- | The primitive used to implement 'GHC.IO.evaluate'.
-- Prefer to use 'GHC.IO.evaluate' whenever possible!
seq# :: forall a s. a -> State# s -> (# State# s, a #)
-- See Note [seq# magic] in GHC.Types.Id.Make
{-# NOINLINE seq# #-}  -- seq# is inlined manually in CorePrep
seq# a s = let !a' = lazy a in (# s, a' #)

considerExceptionsPrecise#
  :: forall {rep} (a :: TYPE rep).
     (State# RealWorld -> (# State# RealWorld, a #))
   -> State# RealWorld -> (# State# RealWorld, a #)
{-# NOINLINE considerExceptionsPrecise# #-}
  -- considerExceptionsPrecise# is inlined manually in CorePrep
considerExceptionsPrecise# a s = a s

hideEvalBarriers#
  :: forall {rep} (a :: TYPE rep) s. (State# s -> a)
                                   -> State# s -> a
{-# NOINLINE hideEvalBarriers# #-}
  -- hideEvalBarriers# is inlined manually in CorePrep
hideEvalBarriers# a s = a s

evalBarrier# :: State# RealWorld -> State# RealWorld
{-# NOINLINE evalBarrier# #-}
  -- evalBarrier# is inlined manually in CorePrep
evalBarrier# s = s

