{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}

module Vectorised
    (solvePA)
where

import CommonVectorised
import Data.Array.Parallel hiding ((+), (-), (*), (/))
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude    as P


{-# NOINLINE solvePA #-}
solvePA
    :: PArray Vec3           -- ^ vertices of the surface
    -> PArray (Int,Int,Int,Colour)  -- ^ triangles, each 3 vertex indices
    -> PArray Vec3           -- ^ rays to cast
    -> Double                -- ^ time, used for rotation
    -> PArray Colour
solvePA vertices triangles rays time
 = toPArrayP (solveV (fromPArrayP vertices) (fromPArrayP triangles) (fromPArrayP rays) time)


-- | Cast all rays into triangle mesh
solveV 
    :: [:Vec3:]             -- ^ vertices of the surface
    -> [:(Int,Int,Int,Colour):]    -- ^ triangles, each 3 vertex indices
    -> [:Vec3:]             -- ^ rays to cast
    -> Double               -- ^ time
    -> [:Colour:]           -- ^ rays and their distance
solveV vertices triangles rays time
 = mapP cast' rays
 where
  matrix = rotateY 0 -- (time / 4)
  tris'  = mapP (\t -> triangleFull (tri vertices matrix t)) triangles
  cast'  = cast tris'

-- | Cast a single ray into the rotated triangle mesh
cast 
    :: [:TriangleFull:] -- ^ triangles, each 3 vertex indices
    -> Vec3              -- ^ ray
    -> Colour
cast triangles ray
 = let r' = ((0,0,0), ray)
       (dist,tri) = mincast triangles r'
   in  if dist D.< arbitraryLargeDouble
       --then colourOfTriangleF tri
       then lights triangles (ray `vsmul` (dist*0.999)) (colourOfTriangleF tri)
       else (0,0,0)

mincast :: [:TriangleFull:]
        -> Line
        -> (Double,TriangleFull)
mincast triangles ray
 = let pl = plueckerOfLine ray
       ch = mapP (check ray pl) triangles
       mi = minIndexP ch
       dist = ch !: mi
       tri  = triangles !: mi
   in (dist, tri)

lights :: [:TriangleFull:]
       -> Vec3
       -> Colour
       -> Colour
lights triangles pt colour
 = let (d1,_) = mincast triangles (pt,(D.negate 10, D.negate 10, D.negate 10))
       (d2,_) = mincast triangles (pt,(50, 50, 0))
       (d3,_) = mincast triangles (pt,(D.negate 5, 50, 0))
       fix i = if   i D.< 1
               then (0 :: Double)
               else 1
   in colour `vsmul` ((fix d1 + fix d2 + fix d3) / 3)
       


-- Get all triangle points from mesh, rotated depending on time
tri :: [:Vec3:] -> Matrix3 -> (Int,Int,Int,Colour) -> Triangle
tri v matrix (a,b,c,colour) = (get a, get b, get c, colour)
 where
  {-# INLINE get #-}
  get i = trans_disp matrix (v !: i)
