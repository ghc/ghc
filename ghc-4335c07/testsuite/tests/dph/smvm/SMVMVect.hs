{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module SMVMVect (smvm) where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.Prelude.Int    as I

import qualified Prelude as P

smvm :: PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
{-# NOINLINE smvm #-}
smvm m v = toPArrayP (smvm' (fromNestedPArrayP m) (fromPArrayP v))

smvm' :: [:[: (Int, Double) :]:] -> [:Double:] -> [:Double:]
smvm' m v = [: D.sumP [: x D.* (v !: i) | (i,x) <- row :] | row <- m :]

