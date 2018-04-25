{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Solver where
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int

data Thing = Thing Int [:Thing:]

        