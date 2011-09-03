-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

-- Modified to use stdout (for testing)

module Illumination
    ( Object
    , Light (..)
    , light, pointlight, spotlight
    , render
    ) where

import Data.Array
import Data.Char(chr)
import Data.Maybe

import Geometry
import CSG
import Surface
import Misc

type Object = CSG (SurfaceFn Color Double)

data Cxt = Cxt {ambient::Color, lights::[Light], object::Object, depth::Int}
        deriving Show

render :: (Matrix,Matrix) -> Color -> [Light] -> Object -> Int ->
          Radian -> Int -> Int -> String -> IO ()
render (m,m') amb ls obj dep fov wid ht file
  = do { debugging
       ; putStrLn (showBitmap' wid ht pixels)
       }
  where
    debugging = return ()
{-
                do { putStrLn (show cxt)
                   ; putStrLn (show (width, delta, aspect, left, top))
                   }
-}
    obj' = transform (m',m) obj
    ls'  = [ transformLight m' l | l <- ls ]
    pixelA = listArray ((1,1), (ht,wid))
                       [ illumination cxt (start,pixel i j)
                       | j <- take ht  [0.5..]
                       , i <- take wid [0.5..] ]
    antiA  = pixelA //
             [ (ix, superSample ix (pixelA ! ix))
             | j <- [2 .. ht - 1], i <- [2 .. wid - 1]
             , let ix = (j, i)
             , contrast ix pixelA ]
    pixels = [ [ illumination cxt (start,pixel i j) | i<- take wid [0.5..] ]
             | j <- take ht [0.5..]
             ]
    cxt    = Cxt {ambient=amb, lights=ls',  object=obj', depth=dep}
    start  = point  0 0 (-1)
    width  = 2 * tan (fov/2)
    delta  = width / fromIntegral wid
    aspect = fromIntegral ht / fromIntegral wid
    left   = - width / 2
    top    = - left * aspect
    pixel i j = vector (left + i*delta) (top - j*delta) 1

    superSample (y, x) col = avg $ col:
      [ illumination cxt (start, pixel (fromIntegral x - 0.5 + xd) (fromIntegral y - 0.5 + yd))
      | (xd, yd) <- [(-0.333, 0.0), (0.333, 0.0), (0.0, -0.333), (0.0, 0.333)]
      ]

avg cs = divN (fromIntegral (length cs)) (uncolor (sumCC cs))
  where divN n (r,g,b) = color (r / n) (g / n) (b / n)

contrast :: (Int, Int) -> Array (Int, Int) Color -> Bool
contrast (x, y) arr = any diffMax [ subCC cur (arr ! (x + xd, y + yd))
                                  | xd <- [-1, 1], yd <- [-1, 1]
                                  ]
  where cur = arr ! (x, y)
        diffMax col = (abs r) > 0.25 || (abs g) >  0.2 || (abs b) > 0.4
           where
                 (r,g,b) = uncolor col


illumination :: Cxt -> Ray -> Color
illumination cxt (r,v)
  | depth cxt <= 0 = black
  | otherwise     = case castRay (r,v) (object cxt) of
                      Nothing -> black
                      Just info -> illum (cxt{depth=(depth cxt)-1}) info v

illum :: Cxt -> (Point,Vector,Properties Color Double) -> Vector -> Color
illum cxt (pos,normV,(col,kd,ks,n)) v
  = ambTerm `addCC` difTerm `addCC` spcTerm `addCC` recTerm
  where
    visibleLights = unobscured pos (object cxt) (lights cxt) normV
    d = depth cxt
    amb = ambient cxt
    newV = subVV v (multSV (2 * dot normV v) normV)

    ambTerm = multSC kd (multCC amb col)
    difTerm = multSC kd (sumCC [multSC (dot normV lj) (multCC intensity col)
	       |(loc,intensity) <- visibleLights,
	       let lj = normalize ({- pos `subVV` -} loc)])
    -- ZZ might want to avoid the phong, when you can...
    spcTerm = multSC ks (sumCC [multSC ((dot normV hj) ** n ) (multCC intensity col)
	       |(loc,intensity) <- visibleLights,
	       -- ZZ note this is specific to the light at infinity
	       let lj = {- pos `subVV` -} normalize loc,
	       let hj = normalize (lj `subVV` normalize v)])
    recTerm  = if recCoeff `nearC` black then black else multCC recCoeff recRay
    recCoeff = multSC ks col
    recRay   = illumination cxt (pos,newV)

showBitmapA :: Int -> Int -> Array (Int, Int) Color -> String
showBitmapA wid ht arr
  = header ++ concatMap scaleColor (elems arr)
  where
    scaleColor col = [scalePixel r, scalePixel g, scalePixel b]
           where (r,g,b) = uncolor col
    header = "P6\n#Galois\n" ++ show wid ++ " " ++ show ht ++ "\n255\n"

showBitmap :: Int -> Int ->[[Color]] -> String
showBitmap wid ht pss
-- type of assert  | length pss == ht && all (\ ps -> length ps == wid) pss
  = header ++ concat [[scalePixel r,scalePixel g,scalePixel b]
                      | ps <- pss, (r,g,b) <- map uncolor ps]
  where
    header = "P6\n#Galois\n" ++ show wid ++ " " ++ show ht ++ "\n255\n"
showBitmap _ _ _ = error "incorrect length of bitmap string"

scalePixel :: Double -> Char
scalePixel p = chr (floor (clampf p * 255))

showBitmap' :: Int -> Int ->[[Color]] -> String
showBitmap' wid ht pss
-- type of assert  | length pss == ht && all (\ ps -> length ps == wid) pss
  = header
 ++ unlines [ unwords [unwords [scalePixel' r,scalePixel' g,scalePixel' b]
                      | (r,g,b) <- map uncolor ps]
            | ps <- pss ]
  where
    header = "P3\n#Galois\n" ++ show wid ++ " " ++ show ht ++ "\n255\n"
showBitmap' _ _ _ = error "incorrect length of bitmap string"

scalePixel' :: Double -> String
scalePixel' p = show (floor (clampf p * 255))

-- Lights

data Light = Light Vector Color
           | PointLight Point Color
           | SpotLight Point Point Color Radian Double
   deriving Show

light :: Coords -> Color -> Light
light (x,y,z) color =
  Light (normalize (vector (-x) (-y) (-z))) color
pointlight (x,y,z) color =
  PointLight (point x y z) color
spotlight (x,y,z) (p,q,r) col cutoff exp =
  SpotLight (point x y z) (point p q r) col cutoff exp

transformLight m (Light v c) = Light (multMV m v) c
transformLight m (PointLight p c) = PointLight (multMP m p) c
transformLight m (SpotLight p q c r d) = SpotLight (multMP m p) (multMP m q) c r d

unobscured :: Point -> Object -> [Light] ->  Vector -> [(Vector,Color)]
unobscured pos obj lights normV = catMaybes (map (unobscure pos obj normV) lights)

unobscure :: Point -> Object -> Vector ->  Light -> Maybe (Vector,Color)
unobscure pos obj normV (Light vec color)
  -- ZZ probably want to make this faster
  | vec `dot` normV < 0 = Nothing
  | intersects (pos `addPV` (0.0001 `multSV` vec),vec) obj = Nothing
  | otherwise               = Just (vec,color)
unobscure pos obj normV (PointLight pp color)
  | vec `dot` normV < 0     = Nothing
  | intersectWithin (pos `addPV` (0.0001 `multSV` (normalize vec)), vec) obj = Nothing
  | otherwise               = Just (vec,is)
      where vec = pp `subPP` pos
            is  = attenuate vec color
unobscure org obj normV (SpotLight pos at color cutoff exp)
  | vec `dot` normV < 0                                                 = Nothing
  | intersectWithin (org `addPV` (0.0001 `multSV` (normalize vec)), vec) obj = Nothing
  | angle > cutoff                                                      = Nothing
  | otherwise                                                           = Just (vec, is)
      where vec   = pos `subPP` org
            vec'  = pos `subPP` at
            angle = acos (normalize vec `dot` (normalize vec'))

            asp   = normalize (at `subPP` pos)
            qsp   = normalize (org `subPP` pos)
            is    = attenuate vec (((asp `dot` qsp) ** exp) `multSC` color)

attenuate :: Vector -> Color -> Color
attenuate vec color = (100 / (99 + sq (norm vec))) `multSC` color

--

castRay ray p
  = case intersectRayWithObject ray p of
    (True, _, _)                     -> Nothing -- eye is inside
    (False, [], _)                   -> Nothing -- eye is inside
    (False, (0, b, _) : _, _)        -> Nothing -- eye is inside
    (False, (i, False, _) : _, _)    -> Nothing -- eye is inside
    (False, (t, b, (s, p0)) : _, _)     ->
	let (v, prop) = surface s p0 in
	    Just (offsetToPoint ray t, v, prop)

intersects ray p
  = case intersectRayWithObject ray p of
    (True, _, _)                  -> False
    (False, [], _)                -> False
    (False, (0, b, _) : _, _)     -> False
    (False, (i, False, _) : _, _) -> False
    (False, (i, b, _) : _, _)     -> True

intersectWithin :: Ray -> Object -> Bool
intersectWithin ray p
  = case intersectRayWithObject ray p of
    (True, _, _)                  -> False -- eye is inside
    (False, [], _)                -> False -- eye is inside
    (False, (0, b, _) : _, _)     -> False -- eye is inside
    (False, (i, False, _) : _, _) -> False -- eye is inside
    (False, (t, b, _) : _, _)     -> t < 1.0
