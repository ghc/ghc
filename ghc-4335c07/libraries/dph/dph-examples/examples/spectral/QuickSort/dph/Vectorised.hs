{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
{-# OPTIONS -fno-spec-constr-count #-}
module Vectorised (quicksortPA) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude


{-# NOINLINE quicksortPA #-}
quicksortPA:: PArray Double -> PArray Double 
quicksortPA xs = toPArrayP  (qsortVect' (fromPArrayP xs))


qsortVect':: [: Double :] -> [: Double :]
qsortVect' xs | lengthP xs I.<=  1 = xs
              | otherwise =
  let p  = xs !: (lengthP xs `I.div` 2)
      ls = [:x | x <- xs, x D.< p:]
      gs = [:x | x <- xs, x D.> p:]

      ss = mapP qsortVect' [:ls, gs:]
 in
 (ss !: 0) +:+ [:x | x <- xs, x D.== p:] +:+ (ss !: 1)
