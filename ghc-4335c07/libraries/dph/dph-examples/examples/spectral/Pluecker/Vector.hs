module Vector
    (solveV)
where

import Common
import Data.Vector.Unboxed
import Prelude hiding (map,filter,minimum)


{-# NOINLINE solveV #-}
solveV 
    :: Vector Vec3             -- ^ vertices of the surface
    -> Vector (Int,Int,Int,Colour) -- ^ triangles, each 3 vertex indices
    -> Vector Vec3             -- ^ rays to cast
    -> Double                  -- ^ time
    -> Vector Colour           -- ^ the colours of the ray results - in the same order as input rays
solveV vertices triangles rays time
 = map cast' rays
 where
  matrix = rotateY 0 -- (time / 4)
  tris'  = map (triangleFull . tri vertices matrix) triangles
  cast'  = cast tris'


cast 
    :: Vector TriangleFull  -- ^ triangles with pluecker coords etc precomputed
    -> Vec3                 -- ^ ray
    -> Colour
cast triangles ray
 = let r' = ((0,0,0), ray)
       (dist,tri) = mincast triangles r'
   in  if dist < arbitraryLargeDouble
       then lights triangles (ray `vsmul` (dist*0.999)) (colourOfTriangleF tri)
       else (0,0,0)

mincast :: Vector TriangleFull
        -> Line
        -> (Double,TriangleFull)
mincast triangles ray
 = let pl = plueckerOfLine ray
       ch = map (check ray pl) triangles
       mi = minIndex ch
       dist = ch ! mi
       tri  = triangles ! mi
   in (dist, tri)

lights :: Vector TriangleFull
       -> Vec3
       -> Colour
       -> Colour
lights triangles pt colour
 = let (d1,_) = mincast triangles (pt,(-10,-10,-10))
       (d2,_) = mincast triangles (pt,(50, 50, 0))
       (d3,_) = mincast triangles (pt,((-5), 50, 0))
       fix i = if   i < 1
               then 0
               else 1
   in colour `vsmul` ((fix d1 + fix d2 + fix d3) / 3)
       

tri :: Vector Vec3 -> Matrix3 -> (Int,Int,Int,Colour) -> Triangle
tri v matrix (a,b,c,colour) = (get a, get b, get c, colour)
 where
  {-# INLINE get #-}
  get i = trans_disp matrix (v `unsafeIndex` i)

