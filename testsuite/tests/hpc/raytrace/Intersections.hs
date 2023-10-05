-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Intersections
    ( intersectRayWithObject,
      quadratic
    ) where

import Data.Maybe(isJust)

import Construct
import Geometry
import Interval
import Misc

-- This is factored into two bits.  The main function `intersections'
-- intersects a line with an object.
-- The wrapper call `intersectRayWithObject' coerces this to an intersection
-- with a ray by clamping the result to start at 0.

intersectRayWithObject ray p
  = clampIntervals is
  where is = intersections ray p

clampIntervals (True, [], True) = (False, [(0, True, undefined)], True)
clampIntervals empty@(False, [], False) = empty
clampIntervals (True, is@((i, False, p) : is'), isOpen)
  | i `near` 0 || i < 0
  = clampIntervals (False, is', isOpen)
  | otherwise
  = (False, (0, True, undefined) : is, isOpen)
clampIntervals ivals@(False, is@((i, True, p) : is'), isOpen)
  | i `near` 0 || i < 0
  -- can unify this with first case...
  = clampIntervals (True, is', isOpen)
  | otherwise
  = ivals

intersections ray (Union p q)
  = unionIntervals is js
  where is = intersections ray p
	js = intersections ray q

intersections ray (Intersect p q)
  = intersectIntervals is js
  where is = intersections ray p
	js = intersections ray q

intersections ray (Difference p q)
  = differenceIntervals is (negateSurfaces js)
  where is = intersections ray p
	js = intersections ray q

intersections ray (Transform m m' p)
  = mapI (xform m) is
  where is = intersections (m' `multMR` ray) p
	xform m (i, b, (s, p0)) = (i, b, (transformSurface m s, p0))

intersections ray (Box box p)
  | intersectWithBox ray box = intersections ray p
  | otherwise = emptyIList

intersections ray p@(Plane s)
  = intersectPlane ray s

intersections ray p@(Sphere s)
  = intersectSphere ray s

intersections ray p@(Cube s)
  = intersectCube ray s

intersections ray p@(Cylinder s)
  = intersectCylinder ray s

intersections ray p@(Cone s)
  = intersectCone ray s

negateSurfaces :: IList (Surface, Texture a) -> IList (Surface, Texture a)
negateSurfaces = mapI negSurf
  where negSurf (i, b, (s,t)) = (i, b, (negateSurface s, t))

negateSurface (Planar p0 v0 v1)
  = Planar p0 v1 v0
negateSurface (Spherical p0 v0 v1)
  = Spherical p0 v1 v0
negateSurface (Cylindrical p0 v0 v1)
  = Cylindrical p0 v1 v0
negateSurface (Conic p0 v0 v1)
  = Conic p0 v1 v0

transformSurface m (Planar p0 v0 v1)
  = Planar p0' v0' v1'
  where p0' = multMP m p0
	v0' = multMV m v0
	v1' = multMV m v1

transformSurface m (Spherical p0 v0 v1)
  = Spherical p0' v0' v1'
  where p0' = multMP m p0
	v0' = multMV m v0
	v1' = multMV m v1

-- ditto as above
transformSurface m (Cylindrical p0 v0 v1)
  = Cylindrical p0' v0' v1'
  where p0' = multMP m p0
	v0' = multMV m v0
	v1' = multMV m v1

transformSurface m (Conic p0 v0 v1)
  = Conic p0' v0' v1'
  where p0' = multMP m p0
	v0' = multMV m v0
	v1' = multMV m v1

--------------------------------
-- Plane
--------------------------------

intersectPlane :: Ray -> a -> IList (Surface, Texture a)
intersectPlane ray texture = intersectXZPlane PlaneFace ray 0.0 texture

intersectXZPlane :: Face -> Ray -> Double -> a -> IList (Surface, Texture a)
intersectXZPlane n (r,v) yoffset texture
  | b `near` 0
  = -- the ray is parallel to the plane - it's either all in, or all out
    if y `near` yoffset || y < yoffset then openIList else emptyIList

    -- The line intersects the plane. Find t such that
    -- (x + at, y + bt, z + ct) intersects the X-Z plane.
    -- t may be negative (the ray starts within the halfspace),
    -- but we'll catch that later when we clamp the intervals

  | b < 0	-- the ray is pointing downwards
  = (False, [mkEntry (t0, (Planar p0 v0 v1, (n, p0, texture)))], True)

  | otherwise	-- the ray is pointing upwards
  = (True,  [mkExit (t0, (Planar p0 v0 v1, (n, p0, texture)))],  False)

  where t0 = (yoffset-y) / b
	x0 = x + a * t0
	z0 = z + c * t0
	p0 = point x0 0 z0
	v0 = vector 0 0 1
	v1 = vector 1 0 0

	x = xCoord r
	y = yCoord r
	z = zCoord r
	a = xComponent v
	b = yComponent v
	c = zComponent v


--------------------------------
-- Sphere
--------------------------------

intersectSphere :: Ray -> a -> IList (Surface, Texture a)
intersectSphere ray@(r, v) texture
  = -- Find t such that (x + ta, y + tb, z + tc) intersects the
    -- unit sphere, that is, such that:
    --   (x + ta)^2 + (y + tb)^2 + (z + tc)^2 = 1
    -- This is a quadratic equation in t:
    --   t^2(a^2 + b^2 + c^2) + 2t(xa + yb + zc) + (x^2 + y^2 + z^2 - 1) = 0
    let c1 = sq a + sq b + sq c
	c2 = 2 * (x * a + y * b + z * c)
	c3 = sq x + sq y + sq z - 1
    in
	case quadratic c1 c2 c3 of
        Nothing -> emptyIList
        Just (t1, t2) -> entryexit (g t1) (g t2)
    where x = xCoord r
	  y = yCoord r
	  z = zCoord r
	  a = xComponent v
	  b = yComponent v
	  c = zComponent v
	  g t = (t, (Spherical origin v1 v2, (SphereFace, p0, texture)))
	      where origin = point 0 0 0
		    x0 = x + t * a
		    y0 = y + t * b
		    z0 = z + t * c
		    p0 = point  x0 y0 z0
		    v0 = vector x0 y0 z0
		    (v1, v2) = tangents v0


--------------------------------
-- Cube
--------------------------------

intersectCube :: Ray -> a -> IList (Surface, Texture a)
intersectCube ray@(r, v) texture
  = -- The set of t such that (x + at, y + bt, z + ct) lies within
    -- the unit cube satisfies:
    --    0 <= x + at <= 1,  0 <= y + bt <= 1,  0 <= z + ct <= 1
    -- The minimum and maximum such values of t give us the two
    -- intersection points.
    case intersectSlabIval (intersectCubeSlab face2 face3 x a)
	(intersectSlabIval (intersectCubeSlab face5 face4 y b)
			   (intersectCubeSlab face0 face1 z c)) of
    Nothing -> emptyIList
    Just (t1, t2) -> entryexit (g t1) (g t2)
  where g ((n, v0, v1), t)
	  = (t, (Planar p0 v0 v1, (n, p0, texture)))
	  where p0 = offsetToPoint ray t
	face0 = (CubeFront,  vectorY, vectorX)
	face1 = (CubeBack,   vectorX, vectorY)
	face2 = (CubeLeft,   vectorZ, vectorY)
	face3 = (CubeRight,  vectorY, vectorZ)
	face4 = (CubeTop,    vectorZ, vectorX)
	face5 = (CubeBottom, vectorX, vectorZ)
	vectorX = vector 1 0 0
	vectorY = vector 0 1 0
	vectorZ = vector 0 0 1
	x = xCoord r
	y = yCoord r
	z = zCoord r
	a = xComponent v
	b = yComponent v
	c = zComponent v

intersectCubeSlab n m w d
  | d `near` 0 = if (0 <= w) && (w <= 1)
		 then Just ((n, -inf), (m, inf)) else Nothing
  | d > 0      = Just ((n,  (-w)/d), (m, (1-w)/d))
  | otherwise  = Just ((m, (1-w)/d), (n,  (-w)/d))

intersectSlabIval Nothing Nothing  = Nothing
intersectSlabIval Nothing (Just i) = Nothing
intersectSlabIval (Just i) Nothing = Nothing
intersectSlabIval (Just (nu1@(n1, u1), mv1@(m1, v1)))
		  (Just (nu2@(n2, u2), mv2@(m2, v2)))
  = checkInterval (nu, mv)
  where nu = if u1 < u2 then nu2 else nu1
	mv = if v1 < v2 then mv1 else mv2
	checkInterval numv@(nu@(_, u), (m, v))
	  -- rounding error may force us to push v out a bit
	  | u `near` v = Just (nu, (m, u + epsilon))
	  | u    <   v = Just numv
	  | otherwise  = Nothing


--------------------------------
-- Cylinder
--------------------------------

intersectCylinder :: Ray -> a -> IList (Surface, Texture a)
intersectCylinder ray texture
  = isectSide `intersectIntervals` isectTop `intersectIntervals` isectBottom
  where isectSide   = intersectCylSide ray texture
	isectTop    = intersectXZPlane CylinderTop ray 1.0 texture
	isectBottom = complementIntervals $ negateSurfaces $
		      intersectXZPlane CylinderBottom ray 0.0 texture

intersectCylSide (r, v) texture
  = -- The ray (x + ta, y + tb, z + tc) intersects the sides of the
    -- cylinder if:
    --    (x + ta)^2 + (z + tc)^2 = 1  and 0 <= y + tb <= 1.
    if (sq a + sq c) `near` 0
    then -- The ray is parallel to the Y-axis, and does not intersect
	 -- the cylinder sides.  It's either all in, or all out
	if (sqxy `near` 1.0 || sqxy < 1.0) then openIList else emptyIList
   else -- Find values of t that solve the quadratic equation
	--   (a^2 + c^2)t^2 + 2(ax + cz)t + x^2 + z^2 - 1 = 0
        let c1 = sq a + sq c
            c2 = 2 * (x * a + z * c)
            c3 = sq x + sq z - 1
	in
	case quadratic c1 c2 c3 of
        Nothing -> emptyIList
        Just (t1, t2) -> entryexit (g t1) (g t2)

  where sqxy = sq x + sq y
	g t = (t, (Cylindrical origin v1 v2, (CylinderSide, p0, texture)))
	    where origin = point 0 0 0
		  x0 = x + t * a
		  y0 = y + t * b
		  z0 = z + t * c
		  p0 = point  x0 y0 z0
		  v0 = vector x0 0 z0
		  (v1, v2) = tangents v0

	x = xCoord r
	y = yCoord r
	z = zCoord r
	a = xComponent v
	b = yComponent v
	c = zComponent v


-------------------
-- Cone
-------------------

intersectCone :: Ray -> a -> IList (Surface, Texture a)
intersectCone ray texture
  = isectSide `intersectIntervals` isectTop `intersectIntervals` isectBottom
  where isectSide   = intersectConeSide ray texture
	isectTop    = intersectXZPlane ConeBase ray 1.0 texture
	isectBottom = complementIntervals $ negateSurfaces $
		      intersectXZPlane ConeBase ray 0.0 texture

intersectConeSide (r, v) texture
  = -- Find the points where the ray intersects the cond side.  At any points of
    -- intersection, we must have:
    --    (x + ta)^2 + (z + tc)^2 = (y + tb)^2
    -- which is the following quadratic equation:
    --    t^2(a^2-b^2+c^2) + 2t(xa-yb+cz) + (x^2-y^2+z^2) = 0
    let c1 = sq a - sq b + sq c
	c2 = 2 * (x * a - y * b + c * z)
	c3 = sq x - sq y + sq z
    in  case quadratic c1 c2 c3 of
	Nothing -> emptyIList
	Just (t1, t2) ->
	    -- If either intersection strikes the middle, then the other
	    -- can only be off by rounding error, so we make a tangent
	    -- strike using the "good" value.
	    -- If the intersections straddle the origin, then it's
	    -- an exit/entry pair, otherwise it's an entry/exit pair.
	    let y1 = y + t1 * b
		y2 = y + t2 * b
	    in  if y1 `near` 0                  then entryexit (g t1) (g t1)
	        else if y2 `near` 0             then entryexit (g t2) (g t2)
		else if (y1 < 0) `xor` (y2 < 0) then exitentry (g t1) (g t2)
		else                                 entryexit (g t1) (g t2)

  where g t = (t, (Conic origin v1 v2, (ConeSide, p0, texture)))
	    where origin = point 0 0 0
		  x0 = x + t * a
		  y0 = y + t * b
		  z0 = z + t * c
		  p0 = point  x0 y0 z0
		  v0 = normalize $ vector x0 (-y0) z0
		  (v1, v2) = tangents v0

	x = xCoord r
	y = yCoord r
	z = zCoord r
	a = xComponent v
	b = yComponent v
	c = zComponent v

	-- beyond me why this isn't defined in the prelude...
	xor False b = b
	xor True  b = not b


-------------------
-- Solving quadratics
-------------------

quadratic :: Double -> Double -> Double -> Maybe (Double, Double)
quadratic a b c =
  -- Solve the equation ax^2 + bx + c = 0 by using the quadratic formula.
  let d = sq b - 4 * a * c
      d' = if d `near` 0 then 0 else d
  in if d' < 0
     then Nothing -- There are no real roots.
     else
	if a > 0 then Just (((-b) - sqrt d') / (2 * a),
			    ((-b) + sqrt d') / (2 * a))
		 else Just (((-b) + sqrt d') / (2 * a),
			    ((-b) - sqrt d') / (2 * a))

-------------------
-- Bounding boxes
-------------------

data MaybeInterval = Interval !Double !Double
		   | NoInterval

isInterval (Interval _ _) = True
isInterval _              = False

intersectWithBox :: Ray -> Box -> Bool
intersectWithBox (r, v) (B x1 x2 y1 y2 z1 z2)
  = isInterval interval
  where x_interval = intersectRayWithSlab (xCoord r) (xComponent v) (x1, x2)
	y_interval = intersectRayWithSlab (yCoord r) (yComponent v) (y1, y2)
	z_interval = intersectRayWithSlab (zCoord r) (zComponent v) (z1, z2)
	interval = intersectInterval x_interval
		   (intersectInterval y_interval z_interval)

intersectInterval :: MaybeInterval -> MaybeInterval -> MaybeInterval
intersectInterval NoInterval _ = NoInterval
intersectInterval _ NoInterval = NoInterval
intersectInterval (Interval a b) (Interval c d)
  | b < c || d < a = NoInterval
  | otherwise = Interval (a `max` c) (b `min` d)

{-# INLINE intersectRayWithSlab #-}
intersectRayWithSlab :: Double -> Double -> (Double,Double) -> MaybeInterval
intersectRayWithSlab xCoord alpha (x1, x2)
  | alpha == 0 = if xCoord < x1 || xCoord > x2 then NoInterval else infInterval
  | alpha >  0 = Interval a b
  | otherwise  = Interval b a
  where a = (x1 - xCoord) / alpha
	b = (x2 - xCoord) / alpha

infInterval = Interval (-inf) inf
