{-# LANGUAGE BangPatterns,TypeSynonymInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Main (main) where

import Control.Parallel.Strategies
import System.Environment
import System.IO

type CFlt = Float
data Color = Color !CFlt !CFlt !CFlt deriving Show

c_black :: Color
c_black = Color 0.0 0.0 0.0
c_white :: Color
c_white = Color 1.0 1.0 1.0

get_color :: Flt -> Flt -> Scene -> Color
get_color x y scn =
 let (Scene _ (Camera pos fwd up right) _ _) = scn
     dir0 = vnorm $ vadd3 fwd (vscale right (-x)) (vscale up y)
     ray = Ray pos dir0
 in
  trace scn ray infinity

gen_pixel_list :: Flt -> Flt -> Flt -> Flt -> Flt -> Flt -> Scene
               -> [(Float,Float,Float,Float,Float)]
gen_pixel_list curx cury stopx stopy maxx maxy scene =
 [ let scx = (x - midx) / midx
       scy = (y - midy) / midy
       Color r g b = get_color scx (scy * (midy / midx)) scene
   in (scx, scy, r, g, b)
 | x <- [curx .. (stopx - 1)],
   y <- [cury .. (stopy - 1)]
 ]
    where midx = maxx / 2
          midy = maxy / 2

gen_blocks_list :: Bool -> Flt -> Flt -> Flt -> Scene -> IO ()
gen_blocks_list par maxx maxy block_size scene =
 let xblocks = maxx / block_size
     yblocks = maxy / block_size
     blocks  = [ (x*block_size, y*block_size)
               | x <- [0..xblocks-1],
                 y <- [0..yblocks-1] ]
     mapper = if par then parMap rnf else map
     pixels  = mapper
               (\(x,y) -> gen_pixel_list x y (x+block_size) (y+block_size) maxx maxy scene)
               blocks
 in
  do
   print ('A', xblocks)
   print ('B', yblocks)
   print ('C', blocks)
   rnf pixels `seq` return ()


main :: IO ()
main = do
  args <- getArgs
  let par = null args
  display par xscene
  display par xscene
  display par xscene
  display par xscene
  display par xscene

display :: Bool -> Scene -> IO ()
display par scene = do
  gen_blocks_list par 512 512 128 scene

data Rayint = RayHit !Flt !Vec !Vec !Texture | RayMiss deriving Show

data Material = Material Color !Flt !Flt !Flt !Flt !Flt deriving Show
type Texture = Rayint -> Material

showTexture :: Texture -> String
showTexture t = show $ t RayMiss

instance Show Texture where
 show = showTexture

t_white :: Rayint -> Material
t_white _ = Material c_white 0 0 0 1 2

data Solid =  Sphere !Vec !Flt !Flt !Flt
            | SNothing deriving Show

sphere :: Vec -> Flt -> Solid
sphere c r =
 Sphere c r (r*r) (1.0/r)

rayint :: Solid -> Ray -> Flt -> Texture -> Rayint

rayint (Sphere center r rsqr _) (Ray e dir0) dist t =
 let eo = vsub center e
     v  = vdot eo dir0
 in
 if (dist >= (v - r)) && (v > 0.0)
 then
  let vsqr = v*v
      csqr = vdot eo eo
      disc = rsqr - (csqr - vsqr) in
  if disc < 0.0 then
   RayMiss
  else
   let d = sqrt disc
       p = vscaleadd e dir0 (v - d)
       n = vnorm (vsub p center) in
    RayHit (v-d) p n t
 else
  RayMiss

rayint SNothing _ _ _ = RayMiss

data Camera = Camera !Vec !Vec !Vec !Vec deriving Show

camera :: Vec -> Vec -> Vec -> Flt -> Camera
camera pos at up angle =
 let fwd   = vnorm $ vsub at pos
     right = vnorm $ vcross up fwd
     up_   = vnorm $ vcross fwd right
     cam_scale = tan ((pi/180)*(angle/2))
 in
  Camera pos fwd
         (vscale up_ cam_scale)
         (vscale right cam_scale)

data Scene = Scene !Solid !Camera !Texture !Color deriving Show

cam :: Camera
cam = camera (Vec 2.1 1.3 1.7)
                        (Vec 0 0 0)
                        (Vec 0 0 1)
                        45

bgc :: Color
bgc = Color 0.078 0.361 0.753

xscene :: Scene
xscene = let prim = sphere (Vec 0.272166 0.272166 0.544331) 0.166667
         in Scene prim cam t_white bgc

shade :: Rayint -> Color
shade ri =
 case ri of
  RayHit _ _ _ _ -> c_black
  RayMiss -> c_white

trace :: Scene -> Ray -> Flt -> Color
trace scn ray depth =
    let (Scene xsld _ dtex _) = scn
        ri = rayint xsld ray depth dtex
    in shade ri

type Flt = Float

infinity :: Flt
infinity = 1.0 / 0.0

data Vec = Vec {vec_x, vec_y, vec_z :: !Flt} deriving Show
data Ray = Ray !Vec !Vec deriving Show

vdot :: Vec -> Vec -> Flt
vdot !v1 !v2 =
 ((vec_x v1) * (vec_x v2)) + ((vec_y v1) * (vec_y v2)) + ((vec_z v1) * (vec_z v2))

vcross :: Vec -> Vec -> Vec
vcross !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 Vec
  ((y1 * z2) - (z1 * y2))
  ((z1 * x2) - (x1 * z2))
  ((x1 * y2) - (y1 * x2))

vadd3 :: Vec -> Vec -> Vec -> Vec
vadd3 !(Vec x1 y1 z1) !(Vec x2 y2 z2) !(Vec x3 y3 z3) =
    Vec (x1 + x2 + x3)
        (y1 + y2 + y3)
        (z1 + z2 + z3)

vsub :: Vec -> Vec -> Vec
vsub !(Vec x1 y1 z1) !(Vec x2 y2 z2) =
 Vec (x1 - x2)
     (y1 - y2)
     (z1 - z2)

vscale :: Vec -> Flt -> Vec
vscale v1 fac =
 Vec ((vec_x v1) * fac)
     ((vec_y v1) * fac)
     ((vec_z v1) * fac)

vscaleadd :: Vec -> Vec -> Flt -> Vec
vscaleadd v1 v2 fac =
 Vec ((vec_x v1) + ((vec_x v2) * fac))
     ((vec_y v1) + ((vec_y v2) * fac))
     ((vec_z v1) + ((vec_z v2) * fac))

vnorm :: Vec -> Vec
vnorm (Vec x1 y1 z1) =
 let len = 1.0 / (sqrt ((x1*x1)+(y1*y1)+(z1*z1))) in
 Vec (x1*len) (y1*len) (z1*len)
