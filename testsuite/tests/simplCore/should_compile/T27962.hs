{-# LANGUAGE MagicHash #-}

module T27962 where

import GHC.Exts

newtype N = MkN Int

-- After inlining myRun, we get  (runRW# @LiftedRep @Int |> co) (\_ -> ...)
-- and the simplifier pushes co into the argument of runRW#.
-- #27962: the simplifier dropped that cast, yielding ill-typed Core
myRun :: (State# RealWorld -> N) -> N
myRun = coerce (runRW# :: (State# RealWorld -> Int) -> Int)
{-# INLINE myRun #-}

foo x = myRun (\_ -> MkN (x+1))
