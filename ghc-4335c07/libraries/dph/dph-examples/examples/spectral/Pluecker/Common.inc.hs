-- Common stuff between Common.hs and CommonVectorised.hs.
-- The vectorised backend needs this to be compiled with -fvectorise,
-- but strangely when I made the vector backend use a common compiled with -fvectorise
-- it was extraordinarily slow.


-- Type synonyms so they work with Data.Vector.Unboxed etc.
-- A 3-vector *or* points
type Vec3     = (Double, Double, Double)
-- Line or ray between two points in space
-- These are both points, so direction of (a,b) is the vector (b-a)
type Line     = (Vec3, Vec3)

-- No alpha, just rgb
type Colour = Vec3

-- Pl\"{u}cker coordinates of a line.
-- To convert line (a,b) into pluecker, (b-a, a X b)
type Pluecker = (Vec3,Vec3)

-- Triangles are cool because they're obviously always planar.
type Triangle = (Vec3, Vec3, Vec3, Colour)

-- Normal and distance of normal from (0,0,0)
type Plane = (Vec3,Double)

type Matrix3 = (Vec3,Vec3,Vec3)

-- Triangle with extra information: pluecker coordinates of edges and its normal plane
-- Actually we could save some memory by sharing the pluecker coords among edges, but oh well.
type TriangleFull = (Triangle,Pluecker,Pluecker,Pluecker,Plane)

pi = 3.1415926

{-# INLINE dot #-}
dot :: Vec3 -> Vec3 -> Double
dot (u,v,w) (x,y,z) = u*x + v*y + w*z

{-# INLINE cross #-}
cross :: Vec3 -> Vec3 -> Vec3
cross (u,v,w) (x,y,z) = ( v * z - w * y
                        , w * x - u * z
                        , u * y - v * x)

{-# INLINE over1 #-}
over1 :: (Double -> Double) -> Vec3 -> Vec3
over1 f (x,y,z) = (f x, f y, f z)

{-# INLINE over2 #-}
over2 :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
over2 f (u,v,w) (x,y,z) = (f u x, f v y, f w z)

{-# INLINE mag #-}
mag a = sqrt (a `dot` a)

{-# INLINE norm #-}
norm a = over1 (/m) a
 where m = mag a

{-# INLINE vsub #-}
vadd = over2 (+)
vsub = over2 (-)

vsmul v s = over1 (*s) v

-- Convert a line into pluecker coordinates
{-# INLINE plueckerOfLine #-}
plueckerOfLine :: Line -> Pluecker
plueckerOfLine (p,q) = (q `vsub` p, q `cross` p)

-- Find intersection of a line and plucker, if exists.
-- Otherwise 
{-# INLINE projectPluecker2 #-}
projectPluecker2 :: Pluecker -> Pluecker -> Double
projectPluecker2 (u1,v1) (u2,v2) = (u1 `dot` v2) + (u2 `dot` v1)

-- Check whether pluecker line intersects given triangle
{-# INLINE inside #-}
inside :: Pluecker -> Triangle -> Bool
inside p (a,b,c,_)
 = let pr l = projectPluecker2 p (plueckerOfLine l)
       ab   = pr (a,b) < 0
       bc   = pr (b,c) < 0
       ca   = pr (c,a) < 0
   in  (ab && bc && ca) || not' (ab || bc || ca)
 where
  -- TODO ISSUE
  -- Library or vectoriser bug?
  -- Before, I was just using "not (ab || ...)". It compiled fine but when running it I got this error.
  -- MainGloss: Data.Array.Parallel.Unlifted.Sequential.Flat.combine2ByTag: tags length /= sum of args length (first = 50000; second = 148577)
  not' True  = False
  not' False = True



-- Get plane from triangle, eg for projection
{-# INLINE planeOfTriangle #-}
planeOfTriangle :: Triangle -> Plane
planeOfTriangle (a,b,c,_)
 = let n  = (a `vsub` b) `cross` (c `vsub` b)
       n' = norm n
       d  = negate (n' `dot` b)
   in  (n', d)

-- should this return Maybe Double?
{-# INLINE lineOnPlane #-}
lineOnPlane :: Line -> Plane -> Double
lineOnPlane (p,q) (n,d)
 = let d1 = (n `dot` p) + d
       d2 = (n `dot` q) + d
   in  if   d1 == d2
       then arbitraryLargeDouble -- meh. big. fail.
       else d1 / (d1 - d2)

-- Project line onto triangle's plane
-- Disregard whether intersection is actually inside triangle
-- Intersection point is scale from line
{-# INLINE lineOnTriangle #-}
lineOnTriangle :: Line -> Triangle -> Double
lineOnTriangle p t
 = lineOnPlane p (planeOfTriangle t)


-- Get the pluecker coordinates of each edge and the plane, for later
{-# INLINE triangleFull #-}
triangleFull :: Triangle -> TriangleFull
triangleFull t@(a,b,c,_)
 = (t, p a b, p b c, p c a, planeOfTriangle t)
 where
  p a b = plueckerOfLine (a,b)

{-# INLINE lineOnTriangleF #-}
lineOnTriangleF :: Line -> TriangleFull -> Double
lineOnTriangleF p (_,_,_,_,norm)
 = lineOnPlane p norm

{-# INLINE insideF #-}
insideF :: Pluecker -> TriangleFull -> Bool
insideF p (_,a,b,c,_)
 = let pr l = projectPluecker2 p l
       ab   = pr a < 0
       bc   = pr b < 0
       ca   = pr c < 0
   in  (ab && bc && ca) || not' (ab || bc || ca)
 where
  -- TODO ISSUE
  -- Library or vectoriser bug?
  -- Before, I was just using "not (ab || ...)". It compiled fine but when running it I got this error.
  -- MainGloss: Data.Array.Parallel.Unlifted.Sequential.Flat.combine2ByTag: tags length /= sum of args length (first = 50000; second = 148577)
  not' True  = False
  not' False = True


-- matrix mult
{-# INLINE mvmul #-}
mvmul :: Matrix3 -> Vec3 -> Vec3
mvmul
   ((xx, xy, xz)
   ,(yx, yy, yz)
   ,(zx, zy, zz))
    (x,   y,  z)
 = ( x * xx + y * yx + z * zx
   , x * xy + y * yy + z * zy
   , x * xz + y * yz + z * zz)

{-# INLINE rotateY #-}
rotateY :: Double -> Matrix3
rotateY d
 =((         cos d,  0,  sin d)
  ,(             0,  1,      0)
  ,(negate (sin d),  0,  cos d))

{-# INLINE rotateX #-}
rotateX :: Double -> Matrix3
rotateX d
 =((             1,  0,      0)
  ,(         0, cos d,  sin d)
  ,(         0, negate (sin d),  cos d))


{-# INLINE trans_disp #-}
trans_disp :: Matrix3 -> Vec3 -> Vec3
trans_disp m v
 = (mvmul m (v `vsub` (0,0,0))) `vadd` (0,0,30)


{-# INLINE colourOfTriangle #-}
colourOfTriangle :: Triangle -> Colour
colourOfTriangle (_,_,_,col) = col

{-# INLINE colourOfTriangleF #-}
colourOfTriangleF :: TriangleFull -> Colour
colourOfTriangleF ((_,_,_,col),_,_,_,_) = col

-- | Return scale / distance along line's direction for intersection. Or a large number if there is no intersection.
--
{-# INLINE check #-}
check :: Line       -- ^ normal 3-space coordinates of line
      -> Pluecker   -- ^ pluecker coordinates of *same* line
      -> TriangleFull-- ^ triangle
      -> Double
check r pl t
  | insideF pl t =
    let dist = lineOnTriangleF r t
    in  if dist < 0 then arbitraryLargeDouble else dist
  | otherwise = arbitraryLargeDouble

arbitraryLargeDouble = 1e100


-- | Convert screen x,y coordinates into a ray from (0,0,0) centred at (0,0,1).
--   The screen coordinates are taken as Doubles because otherwise
--   CommonVectorised would need to import Int, and then (-) would be ambiguous.
--
--   This should give us a FOV of 90' in each axis, so no ratio fixup.
rayOfScreen
    :: (Double,Double) -- window size, eg 800x600
    -> (Double,Double) -- screen coordinates
    -> Vec3
rayOfScreen (mx,my) (x,y)
 = norm ((x - mx') / mx', (y - my') / my', 1)
 where
  mx' = mx / 2
  my' = my / 2
