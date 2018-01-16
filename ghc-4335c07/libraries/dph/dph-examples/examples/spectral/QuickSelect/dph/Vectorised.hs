{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
{-# OPTIONS -fno-spec-constr-count #-}
module Vectorised (quickselectPA) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double        as D
import Data.Array.Parallel.Prelude.Int           as I
import qualified Prelude


{-# NOINLINE quickselectPA #-}
quickselectPA:: PArray Double -> Int -> Double 
quickselectPA xs k = qselectVect' (fromPArrayP xs) k


-- | Find the @k@th smallest element.
-- A pivot is selected and the array is partitioned into those smaller and larger than the pivot.
-- If the number of elements smaller than pivot is greater or equal to @k@, then the pivot must be larger than the
-- @k@th smallest element, and we recurse into the smaller elements.
-- Otherwise the @k@th element must be larger or equal to the pivot.
-- Since lesser elements are not in the greater array, we are no longer looking for the
-- @k@th smallest element, but the @k - (length xs - length gs)@th smallest.
qselectVect':: [: Double :] -> Int -> Double 
qselectVect' xs k =
  let p  = xs !: (lengthP xs `I.div` 2)
      ls = [:x | x <- xs, x D.< p:]
  in  if   k I.< (lengthP ls)
      then qselectVect' ls k
      else
        let gs  = [:x | x <- xs, x D.> p:]
            len = lengthP xs I.- lengthP gs
        in  if   k I.>= len
            then qselectVect' gs (k I.- len)
            else p

