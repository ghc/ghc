-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Surface
    ( SurfaceFn (..)
    , Properties
    , sfun, sconst
    , prop
    , matte, shiny
    , chgColor
    , surface
    ) where

import Geometry
import CSG
import Misc

-- the surface gets passed face then u then v.
data SurfaceFn c v = SFun (Int -> Double -> Double -> Properties c v)
                   | SConst (Properties c v)

sfun :: (Int -> Double -> Double -> Properties c v) -> SurfaceFn c v
sfun = SFun
sconst :: Properties c v -> SurfaceFn c v
sconst = SConst

type Properties c v = (c, v, v, v)

prop c d s p = (c, d, s, p)

matte = (white, 1.0, 0.0, 1.0)
shiny = (white, 0.0, 1.0, 1.0)

chgColor :: c -> Properties d v -> Properties c v
chgColor c (_, d, s, p) = (c, d, s, p)

instance (Show c, Show v) => Show (SurfaceFn c v) where
  show (SFun _)   = "Surface function"
  -- show (SConst p) = "Surface constant: " ++ show p
  show (SConst p) = "Surface constant"

evalSurface :: SurfaceFn Color Double -> Int -> Double -> Double -> Properties Color Double
evalSurface (SConst p) = \_ _ _ -> p
evalSurface (SFun f)   = f

-- calculate surface properties, given the type of
-- surface, and intersection point in object coordinates

-- surface :: Surface SurfaceFn -> (Int, Point) -> (Vector, Properties)

surface (Planar _ v0 v1) (n, p0, fn)
  = (norm, evalSurface fn n' u v)
  where norm = normalize $ cross v0 v1
	(n', u, v) = planarUV n p0

surface (Spherical _ v0 v1) (_, p0, fn)
  = (norm, evalSurface fn 0 u v)
  where x = xCoord p0
	y = yCoord p0
	z = zCoord p0
	k = sqrt (1 - sq y)
	theta = adjustRadian (atan2 (x / k) (z / k))
	-- correct so that the image grows left-to-right
	-- instead of right-to-left
	u = 1.0 - clampf (theta / (2 * pi))
	v =       clampf ((y + 1) / 2)
	norm = normalize $ cross v0 v1

-- ZZ ignore the (incorrect) surface model, and estimate the normal
-- from the intersection in object space
surface (Cylindrical _ v0 v1) (_, p0, fn)
  = (norm, evalSurface fn 0 u v)
  where x = xCoord p0
	y = yCoord p0
	z = zCoord p0
	u = clampf $ adjustRadian (atan2 x z) / (2 * pi)
	v = y
	norm = normalize $ cross v0 v1

-- ZZ ignore the (incorrect) surface model, and estimate the normal
-- from the intersection in object space
surface (Conic _ v0 v1) (_, p0, fn)
  = (norm, evalSurface fn 0 u v)
  where x = xCoord p0
	y = yCoord p0
	z = zCoord p0
	u = clampf $ adjustRadian (atan2 (x / y) (z / y)) / (2 * pi)
	v = y
	norm = normalize $ cross v0 v1

planarUV face p0
  = case face of
    PlaneFace      -> (0, x, z)

    CubeFront      -> (0, x, y)
    CubeBack       -> (1, x, y)
    CubeLeft       -> (2, z, y)
    CubeRight      -> (3, z, y)
    CubeTop        -> (4, x, z)
    CubeBottom     -> (5, x, z)

    CylinderTop    -> (1, (x + 1) / 2, (z + 1) / 2)
    CylinderBottom -> (2, (x + 1) / 2, (z + 1) / 2)

    ConeBase       -> (1, (x + 1) / 2, (z + 1) / 2)
  where x = xCoord p0
	y = yCoord p0
	z = zCoord p0

-- misc

adjustRadian :: Radian -> Radian
adjustRadian r = if r > 0 then r else r + 2 * pi
