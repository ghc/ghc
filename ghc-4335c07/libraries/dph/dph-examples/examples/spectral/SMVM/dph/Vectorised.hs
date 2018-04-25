
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised (smvmPA) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.Prelude.Int    as I
import qualified Prelude as P


smvmPA :: Int -> PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
smvmPA _ m v = toPArrayP (smvm (fromNestedPArrayP m) (fromPArrayP v))
{-# NOINLINE smvmPA #-}


smvm :: [:[: (Int, Double) :]:] -> [:Double:] -> [:Double:]
smvm m v = [: D.sumP [: x D.* (v !: i) 
                         | (i,x) <- row :] 
                                | row <- m :]

