module Algo.Quickhull (quickhull) where

import Data.Vector.Unboxed as V

quickhull :: (Vector Double, Vector Double) -> (Vector Double, Vector Double)
{-# NOINLINE quickhull #-}
quickhull (xs, ys) = xs' `seq` ys' `seq` (xs',ys')
    where
      (xs',ys') = V.unzip
                $ hsplit points pmin pmax V.++ hsplit points pmax pmin

      imin = V.minIndex xs
      imax = V.maxIndex xs

      points = V.zip xs ys
      pmin   = points V.! imin
      pmax   = points V.! imax


      hsplit points p1 p2
        | V.length packed < 2 = p1 `V.cons` packed
        | otherwise = hsplit packed p1 pm V.++ hsplit packed pm p2
        where
          cs     = V.map (\p -> cross p p1 p2) points
          packed = V.map fst
                 $ V.filter (\t -> snd t > 0)
                 $ V.zip points cs

          pm     = points V.! V.maxIndex cs

      cross (x,y) (x1,y1) (x2,y2) = (x1-x)*(y2-y) - (y1-y)*(x2-x)

