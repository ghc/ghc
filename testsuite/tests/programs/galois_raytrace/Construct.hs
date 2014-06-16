-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Construct
    ( Surface (..)
    , Face (..)
    , CSG (..)
    , Texture
    , Transform
    , union, intersect, difference
    , plane, sphere, cube, cylinder, cone
    , transform
    , translate, translateX, translateY, translateZ
    , scale, scaleX, scaleY, scaleZ, uscale
    , rotateX, rotateY, rotateZ
    , eye, translateEye
    , rotateEyeX, rotateEyeY, rotateEyeZ
    ) where

import Geometry

-- In each case, we model the surface by a point and a pair of tangent vectors.
-- This gives us enough information to determine the surface
-- normal at that point, which is all that is required by the current
-- illumination model.  We can't just save the surface normal because
-- that isn't preserved by transformations.

data Surface
  = Planar Point Vector Vector
  | Spherical Point Vector Vector
  | Cylindrical Point Vector Vector
  | Conic Point Vector Vector
  deriving Show

data Face
  = PlaneFace
  | SphereFace
  | CubeFront
  | CubeBack
  | CubeLeft
  | CubeRight
  | CubeTop
  | CubeBottom
  | CylinderSide
  | CylinderTop
  | CylinderBottom
  | ConeSide
  | ConeBase
  deriving Show

data CSG a
  = Plane a
  | Sphere a
  | Cylinder a
  | Cube a
  | Cone a
  | Transform Matrix Matrix (CSG a)
  | Union (CSG a) (CSG a)
  | Intersect (CSG a) (CSG a)
  | Difference (CSG a) (CSG a)
  | Box Box (CSG a)
  deriving (Show)

-- the data returned for determining surface texture
-- the Face tells which face of a primitive this is
-- the Point is the point of intersection in object coordinates
-- the a is application-specific texture information
type Texture a = (Face, Point, a)

union, intersect, difference		:: CSG a -> CSG a -> CSG a

union p@(Box b1 _) q@(Box b2 _) = Box (mergeBox b1 b2) (Union p q)
union p q = Union p q

-- rather pessimistic
intersect p@(Box b1 _) q@(Box b2 _) = Box (mergeBox b1 b2) (Intersect p q)
intersect p q = Intersect p q

difference (Box b1 p) q = Box b1 (Difference p q)
-- no need to box again inside
-- difference p@(Box b1 _) q = Box b1 (Difference p q)
difference p q = Difference p q

mkBox b p = Box b p

plane, sphere, cube, cylinder, cone	:: a -> CSG a

plane = Plane
sphere s =
    mkBox (B (-1 - epsilon) (1 + epsilon)
	     (-1 - epsilon) (1 + epsilon)
	     (-1 - epsilon) (1 + epsilon)) (Sphere s)
cone s =
    mkBox (B (-1 - epsilon) (1 + epsilon)
	     (   - epsilon) (1 + epsilon)
	     (-1 - epsilon) (1 + epsilon)) (Cone s)
cube s =
    mkBox (B (- epsilon) (1 + epsilon)
	     (- epsilon) (1 + epsilon)
	     (- epsilon) (1 + epsilon)) (Cube s)
cylinder s =
    mkBox (B (-1 - epsilon) (1 + epsilon)
	     (   - epsilon) (1 + epsilon)
	     (-1 - epsilon) (1 + epsilon)) (Cylinder s)

----------------------------
-- Object transformations
----------------------------

type Transform = (Matrix, Matrix)

transform :: Transform -> CSG a -> CSG a

transform (m, m')   (Transform mp mp' p) = Transform  (multMM m mp)       (multMM mp' m') p
transform mm'       (Union p q)          = Union      (transform mm' p)   (transform mm' q)
transform mm'       (Intersect p q)      = Intersect  (transform mm' p)   (transform mm' q)
transform mm'       (Difference p q)     = Difference (transform mm' p)   (transform mm' q)
transform mm'@(m,_) (Box box p)          = Box        (transformBox m box) (transform mm' p)
transform (m, m')   prim                 = Transform  m m' prim

translate				:: Coords -> CSG a -> CSG a
translateX, translateY, translateZ	:: Double -> CSG a -> CSG a

translate xyz = transform $ transM xyz
translateX x = translate (x, 0, 0)
translateY y = translate (0, y, 0)
translateZ z = translate (0, 0, z)

scale      				:: Coords -> CSG a -> CSG a
scaleX, scaleY, scaleZ, uscale		:: Double -> CSG a -> CSG a

scale xyz = transform $ scaleM xyz
scaleX x = scale (x, 1, 1)
scaleY y = scale (1, y, 1)
scaleZ z = scale (1, 1, z)
uscale u = scale (u,u,u)

rotateX, rotateY, rotateZ		:: Radian -> CSG a -> CSG a

rotateX a = transform $ rotxM a
rotateY a = transform $ rotyM a
rotateZ a = transform $ rotzM a

unit = matrix
      ( ( 1.0, 0.0, 0.0, 0.0 ),
	( 0.0, 1.0, 0.0, 0.0 ),
	( 0.0, 0.0, 1.0, 0.0 ),
	( 0.0, 0.0, 0.0, 1.0 ) )

transM (x, y, z)
  = ( matrix
      ( ( 1, 0, 0, x ),
	( 0, 1, 0, y ),
	( 0, 0, 1, z ),
	( 0, 0, 0, 1 ) ),
      matrix
      ( ( 1, 0, 0, -x ),
	( 0, 1, 0, -y ),
	( 0, 0, 1, -z ),
	( 0, 0, 0,  1 ) ) )

scaleM (x, y, z)
  = ( matrix
      ( (   x',    0,    0, 0 ),
	(    0,   y',    0, 0 ),
	(    0,    0,   z', 0 ),
	(    0,    0,    0, 1 ) ),
      matrix
      ( ( 1/x',    0,    0, 0 ),
	(    0, 1/y',    0, 0 ),
	(    0,    0, 1/z', 0 ),
	(    0,    0,    0, 1 ) ) )
  where x' = nonZero x
	y' = nonZero y
	z' = nonZero z

rotxM t
  = ( matrix
      ( (      1,      0,      0, 0 ),
	(      0,  cos t, -sin t, 0 ),
	(      0,  sin t,  cos t, 0 ),
	(      0,      0,      0, 1 ) ),
      matrix
      ( (      1,      0,      0, 0 ),
	(      0,  cos t,  sin t, 0 ),
	(      0, -sin t,  cos t, 0 ),
	(      0,      0,      0, 1 ) ) )

rotyM t
  = ( matrix
      ( (  cos t,      0,  sin t, 0 ),
	(      0,      1,      0, 0 ),
	( -sin t,      0,  cos t, 0 ),
	(      0,      0,      0, 1 ) ),
      matrix
      ( (  cos t,      0, -sin t, 0 ),
	(      0,      1,      0, 0 ),
	(  sin t,      0,  cos t, 0 ),
	(      0,      0,      0, 1 ) ) )

rotzM t
  = ( matrix
      ( (  cos t, -sin t,      0, 0 ),
	(  sin t,  cos t,      0, 0 ),
	(      0,      0,      1, 0 ),
	(      0,      0,      0, 1 ) ),
      matrix
      ( (  cos t,  sin t,      0, 0 ),
	( -sin t,  cos t,      0, 0 ),
	(      0,      0,      1, 0 ),
	(      0,      0,      0, 1 ) ) )

-------------------
-- Eye transformations

-- These are used to specify placement of the eye.
-- `eye' starts out at (0, 0, -1).
-- These are implemented as inverse transforms of the model.
-------------------

eye				 	:: Transform
translateEye				:: Coords -> Transform -> Transform
rotateEyeX, rotateEyeY, rotateEyeZ	:: Radian -> Transform -> Transform

eye = (unit, unit)
translateEye xyz (eye1, eye2)
  = (multMM m1 eye1, multMM eye2 m2)
  where (m1, m2) = transM xyz
rotateEyeX t (eye1, eye2)
  = (multMM m1 eye1, multMM eye2 m2)
  where (m1, m2) = rotxM t
rotateEyeY t (eye1, eye2)
  = (multMM m1 eye1, multMM eye2 m2)
  where (m1, m2) = rotyM t
rotateEyeZ t (eye1, eye2)
  = (multMM m1 eye1, multMM eye2 m2)
  where (m1, m2) = rotzM t

-------------------
-- Bounding boxes
-------------------

mergeBox (B x11  x12  y11  y12  z11  z12) (B x21  x22  y21  y22  z21  z22) =
    B (x11 `min` x21) (x12 `max` x22)
      (y11 `min` y21) (y12 `max` y22)
      (z11 `min` z21) (z12 `max` z22)

transformBox t (B x1  x2  y1  y2  z1  z2)
  = (B (foldr1 min (map xCoord pts'))
       (foldr1 max (map xCoord pts'))
       (foldr1 min (map yCoord pts'))
       (foldr1 max (map yCoord pts'))
       (foldr1 min (map zCoord pts'))
       (foldr1 max (map zCoord pts')))
  where pts' = map (multMP t) pts
	pts =  [point x1 y1 z1,
		point x1 y1 z2,
		point x1 y2 z1,
		point x1 y2 z2,
		point x2 y1 z1,
		point x2 y1 z2,
		point x2 y2 z1,
		point x2 y2 z2]
