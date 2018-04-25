{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module QuickHullVect (quickhull) where

import Types

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double as D
import qualified Data.Array.Parallel.Prelude.Int as Int

import qualified Prelude as P

distance :: Point -> Line -> Double
distance (xo, yo) ((x1, y1), (x2, y2))
  = (x1 D.- xo) D.* (y2 D.- yo) D.- (y1 D.- yo) D.* (x2 D.- xo)

hsplit :: [:Point:] -> Line -> [:Point:]
hsplit points line@(p1, p2)
  | lengthP packed Int.< 2 = singletonP p1 +:+ packed
  | otherwise
  = concatP [: hsplit packed ends | ends <- [:(p1, pm), (pm, p2):] :]
  where
    cross  = [: distance p line | p <- points :]
    packed = [: p | (p,c) <- zipP points cross, c D.> 0.0 :]
    pm     = points !: maxIndexP cross

quickHull' :: [:Point:] -> [:Point:]
quickHull' points
  | lengthP points Int.== 0 = points
  | otherwise
  = concatP [: hsplit points ends | ends <- [: (minx, maxx), (maxx, minx) :] :]
  where
    xs   = [: x | (x, y) <- points :]
    minx = points !: minIndexP xs
    maxx = points !: maxIndexP xs

quickhull :: PArray Point -> PArray Point
{-# NOINLINE quickhull #-}
quickhull ps = toPArrayP (quickHull' (fromPArrayP ps))

