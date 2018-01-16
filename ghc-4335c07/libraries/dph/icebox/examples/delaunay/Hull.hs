{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}

module Hull ( convexHull, lowerHull ) where

import Types

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Int as Int
import Data.Array.Parallel.Prelude.Int ( Int )

import qualified Prelude as P

distance :: IPoint -> ILine -> Double
distance (_, (xo,yo)) ((_, (x1, y1)), (_, (x2, y2)))
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)

hull :: [:IPoint:] -> ILine -> [:Int:]
hull points line@((i1,p1),(i2,p2))
  | lengthP packed Int.== 0 = [:i1:]
  | otherwise
  = concatP [: hull packed ends | ends <- [:((i1,p1),pm), (pm,(i2,p2)):] :]
  where
    cross  = [: distance p line | p <- points :]
    packed = [: p | (p,c) <- zipP points cross, c > 0.0 :]
    pm     = points !: maxIndexP cross

convexHull :: [:IPoint:] -> [:Int:]
convexHull points | lengthP points Int.== 0 = [::]
convexHull points
  = concatP [: hull points ends | ends <- [: (minx,maxx), (maxx,minx) :] :]
  where
    xs = [: x | (i,(x,y)) <- points :]
    minx = points !: minIndexP xs
    maxx = points !: maxIndexP xs

lowerHull :: [:IPoint:] -> [:Int:]
lowerHull points | lengthP points Int.== 0 = [::]
lowerHull points
  = hull points (maxx,minx) +:+ [: case minx of (i,_) -> i :]
  where
    xs   = [: x | (i,(x,y)) <- points :]
    minx = points !: minIndexP xs
    maxx = points !: maxIndexP xs

