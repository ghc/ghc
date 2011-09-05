-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Geometry
    ( Coords
    , Ray
    , Point  -- abstract
    , Vector -- abstract
    , Matrix -- abstract
    , Color  -- abstract
    , Box(..)
    , Radian
    , matrix
    , coord
    , color
    , uncolor
    , xCoord , yCoord , zCoord
    , xComponent , yComponent , zComponent
    , point
    , vector
    , nearV
    , point_to_vector
    , vector_to_point
    , dot
    , cross
    , tangents
    , addVV
    , addPV
    , subVV
    , negV
    , subPP
    , norm
    , normalize
    , dist2
    , sq
    , distFrom0Sq
    , distFrom0
    , multSV
    , multMM
    , transposeM
    , multMV
    , multMP
    , multMQ
    , multMR
    , white
    , black
    , addCC
    , subCC
    , sumCC
    , multCC
    , multSC
    , nearC
    , offsetToPoint
    , epsilon
    , inf
    , nonZero
    , eqEps
    , near
    , clampf
    ) where

type Coords = (Double,Double,Double)

type Ray = (Point,Vector)    -- origin of ray, and unit vector giving direction

data Point  = P !Double !Double !Double -- implicit extra arg of 1
    deriving (Show)
data Vector = V !Double !Double !Double -- implicit extra arg of 0
    deriving (Show, Eq)
data Matrix = M !Quad   !Quad   !Quad   !Quad
    deriving (Show)

data Color  = C !Double !Double !Double
    deriving (Show, Eq)

data Box = B !Double !Double !Double !Double !Double !Double
    deriving (Show)

data Quad   = Q !Double !Double !Double !Double
    deriving (Show)

type Radian = Double

type Tup4 a = (a,a,a,a)

--{-# INLINE matrix #-}
matrix :: Tup4 (Tup4 Double) -> Matrix
matrix ((m11, m12, m13, m14),
          (m21, m22, m23, m24),
          (m31, m32, m33, m34),
          (m41, m42, m43, m44))
  = M (Q m11 m12 m13 m14)
      (Q m21 m22 m23 m24)
      (Q m31 m32 m33 m34)
      (Q m41 m42 m43 m44)

coord x y z = (x, y, z)

color r g b = C r g b

uncolor (C r g b) = (r,g,b)

{-# INLINE xCoord #-}
xCoord (P x y z) = x
{-# INLINE yCoord #-}
yCoord (P x y z) = y
{-# INLINE zCoord #-}
zCoord (P x y z) = z

{-# INLINE xComponent #-}
xComponent (V x y z) = x
{-# INLINE yComponent #-}
yComponent (V x y z) = y
{-# INLINE zComponent #-}
zComponent (V x y z) = z

point :: Double -> Double -> Double -> Point
point x y z = P x y z

vector :: Double -> Double -> Double -> Vector
vector x y z = V x y z

nearV :: Vector -> Vector -> Bool
nearV (V a b c) (V d e f) = a `near` d && b `near` e && c `near` f

point_to_vector :: Point -> Vector
point_to_vector (P x y z) = V x y z

vector_to_point :: Vector -> Point
vector_to_point (V x y z)  = P x y z

{-# INLINE vector_to_quad #-}
vector_to_quad :: Vector -> Quad
vector_to_quad (V x y z) = Q x y z 0

{-# INLINE point_to_quad #-}
point_to_quad :: Point -> Quad
point_to_quad (P x y z) = Q x y z 1

{-# INLINE quad_to_point #-}
quad_to_point :: Quad -> Point
quad_to_point (Q x y z _) = P x y z

{-# INLINE quad_to_vector #-}
quad_to_vector :: Quad -> Vector
quad_to_vector (Q x y z _) = V x y z

--{-# INLINE dot #-}
dot :: Vector -> Vector -> Double
dot (V x1 y1 z1) (V x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vector -> Vector -> Vector
cross (V x1 y1 z1) (V x2 y2 z2)
  = V (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

-- assumption: the input vector is a normal
tangents :: Vector -> (Vector, Vector)
tangents v@(V x y z)
  = (v1, v `cross` v1)
  where v1 | x == 0    = normalize (vector 0 z (-y))
	   | otherwise = normalize (vector (-y) x 0)

{-# INLINE dot4 #-}
dot4 :: Quad -> Quad -> Double
dot4 (Q x1 y1 z1 w1) (Q x2 y2 z2 w2) = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2

addVV :: Vector -> Vector -> Vector
addVV (V x1 y1 z1) (V x2 y2 z2)
    = V (x1 + x2) (y1 + y2) (z1 + z2)

addPV :: Point -> Vector -> Point
addPV (P x1 y1 z1) (V x2 y2 z2)
    = P (x1 + x2) (y1 + y2) (z1 + z2)

subVV :: Vector -> Vector -> Vector
subVV (V x1 y1 z1) (V x2 y2 z2)
    = V (x1 - x2) (y1 - y2) (z1 - z2)

negV :: Vector -> Vector
negV (V x1 y1 z1)
    = V (-x1) (-y1) (-z1)

subPP :: Point -> Point -> Vector
subPP (P x1 y1 z1) (P x2 y2 z2)
    = V (x1 - x2) (y1 - y2) (z1 - z2)

--{-# INLINE norm #-}
norm :: Vector -> Double
norm (V x y z) = sqrt (sq x + sq y + sq z)

--{-# INLINE normalize #-}
-- normalize a vector to a unit vector
normalize :: Vector -> Vector
normalize v@(V x y z)
             | norm /= 0 = multSV (1/norm) v
	     | otherwise = error "normalize empty!"
    where norm = sqrt (sq x + sq y + sq z)

-- This does computes the distance *squared*
dist2 :: Point -> Point -> Double
dist2 us vs = sq x + sq y + sq z
    where
       (V x y z) = subPP us vs

{-# INLINE sq #-}
sq :: Double -> Double
sq d = d * d

{-# INLINE distFrom0Sq #-}
distFrom0Sq :: Point -> Double  -- Distance of point from origin.
distFrom0Sq (P x y z) = sq x + sq y + sq z

{-# INLINE distFrom0 #-}
distFrom0 :: Point -> Double  -- Distance of point from origin.
distFrom0 p = sqrt (distFrom0Sq p)

--{-# INLINE multSV #-}
multSV :: Double -> Vector -> Vector
multSV k (V x y z) = V (k*x) (k*y) (k*z)

--{-# INLINE multMM #-}
multMM :: Matrix -> Matrix -> Matrix
multMM m1@(M q1 q2 q3 q4) m2
     = M (multMQ m2' q1)
         (multMQ m2' q2)
         (multMQ m2' q3)
         (multMQ m2' q4)
  where
     m2' = transposeM m2

{-# INLINE transposeM #-}
transposeM :: Matrix -> Matrix
transposeM (M (Q e11  e12  e13  e14)
              (Q e21  e22  e23  e24)
              (Q e31  e32  e33  e34)
              (Q e41  e42  e43  e44)) = (M (Q e11  e21  e31  e41)
                                           (Q e12  e22  e32  e42)
                                           (Q e13  e23  e33  e43)
                                           (Q e14  e24  e34  e44))


--multMM m1 m2 = [map (dot4 row) (transpose m2) | row <- m1]

--{-# INLINE multMV #-}
multMV :: Matrix -> Vector -> Vector
multMV m v = quad_to_vector (multMQ m (vector_to_quad v))

--{-# INLINE multMP #-}
multMP :: Matrix -> Point -> Point
multMP m p = quad_to_point (multMQ m (point_to_quad p))

-- mat vec = map (dot4 vec) mat

{-# INLINE multMQ #-}
multMQ :: Matrix -> Quad -> Quad
multMQ (M q1 q2 q3 q4) q
       = Q (dot4 q q1)
           (dot4 q q2)
           (dot4 q q3)
           (dot4 q q4)

{-# INLINE multMR #-}
multMR :: Matrix -> Ray -> Ray
multMR m (r, v) = (multMP m r, multMV m v)

white :: Color
white = C 1 1 1
black :: Color
black = C 0 0 0

addCC :: Color -> Color -> Color
addCC (C a b c) (C d e f) = C (a+d) (b+e) (c+f)

subCC :: Color -> Color -> Color
subCC (C a b c) (C d e f) = C (a-d) (b-e) (c-f)

sumCC :: [Color] -> Color
sumCC = foldr addCC black

multCC :: Color -> Color -> Color
multCC (C a b c) (C d e f) = C (a*d) (b*e) (c*f)

multSC :: Double -> Color -> Color
multSC k       (C a b c) = C (a*k) (b*k) (c*k)

nearC :: Color -> Color -> Bool
nearC (C a b c) (C d e f) = a `near` d && b `near` e && c `near` f

offsetToPoint :: Ray -> Double -> Point
offsetToPoint (r,v) i = r `addPV` (i `multSV` v)

--

epsilon, inf :: Double      -- aproximate zero and infinity
epsilon = 1.0e-10
inf = 1.0e20

nonZero :: Double -> Double         -- Use before a division. It makes definitions
nonZero x | x > epsilon  = x        -- more complete and I bet the errors that get
          | x < -epsilon = x        -- introduced will be undetectable if epsilon
          | otherwise    = epsilon  -- is small enough


eqEps x y = abs (x-y) < epsilon
near = eqEps

clampf :: Double -> Double
clampf p | p < 0 = 0
         | p > 1 = 1
         | True  = p
