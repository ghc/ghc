{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module BarnesHutVect (oneStep)

where
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Int as I

import qualified Prelude



data BoundingBox   = Box Double Double Double Double

data MassPoint     = MP Double Double Double -- xpos ypos mass

data BHTree        = BHT Double Double Double -- root mass point
                         [:BHTree:]


epsilon:: Double
epsilon = 0.05

eClose::Double
eClose  = 0.5


oneStep:: Double -> Double -> Double -> Double -> PArray (Double, Double, Double)  -> PArray (Double, Double)
oneStep llx lly rux ruy mspnts = toPArrayP accs
    where  
       accs = [: calcAccel m tree | m <- ms :]
       tree = buildTree (Box llx lly rux ruy) ms
       ms   = [: MP x y m | (x,y,m) <- fromPArrayP mspnts:]

-- Phase 1: building the tree
--

buildTree:: BoundingBox -> [: MassPoint :] -> BHTree
buildTree bb particles
  | lengthP particles I.<= 1 = BHT x y m emptyP
  | otherwise                = BHT x y m subTrees
  where
    (MP x y m)           = calcCentroid particles
    (boxes, splitPnts)   = splitPoints bb particles 
    subTrees             = [:buildTree bb' ps | (bb', ps) <- zipP boxes splitPnts:]
  
-- Split massPoints according to their locations in the quadrants
-- 
splitPoints:: BoundingBox -> [: MassPoint :] -> ([:BoundingBox:], [:[: MassPoint :]:])
splitPoints b@(Box llx lly rux  ruy) particles 
  | noOfPoints I.<= 1 = (singletonP b, singletonP particles)
  | otherwise         
   = unzipP [: (b,p) | (b,p) <- zipP boxes splitPars, lengthP p I.> 0:]
      where
        noOfPoints    = lengthP particles
        lls           = [: p | p <- particles, inBox b1 p :]
        lus           = [: p | p <- particles, inBox b2 p :]
        rus           = [: p | p <- particles, inBox b3 p :]
        rls           = [: p | p <- particles, inBox b4 p :]
        b1 = Box llx  lly  midx midy
        b2 = Box llx  midy midx  ruy
        b3 = Box midx midy rux   ruy
        b4 = Box midx lly  rux  midy
        boxes         = singletonP b1 +:+ singletonP b2 +:+ singletonP b3 +:+ singletonP b4 
        splitPars     = singletonP lls +:+ singletonP lus +:+ singletonP rus +:+ singletonP rls
        (midx,  midy) = ((llx + rux) / 2.0 , (lly + ruy) / 2.0) 


-- checks if particle is in box (excluding left and lower border)
-- inBox:: BoundingBox -> MassPoint -> Bool
inBox (Box llx  lly rux  ruy) (MP px  py  _) = 
    (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)

calcCentroid:: [:MassPoint:] -> MassPoint
calcCentroid mpts = MP  ((sumP xs)/mass) ((sumP ys)/mass) mass
  where
    mass     = sumP [: m | MP _ _ m  <- mpts :]
    (xs, ys) = unzipP [: (m * x, m * y) | MP x y m <- mpts :]   


calcAccel:: MassPoint -> BHTree -> (Double, Double)
calcAccel mpt (BHT x y m subtrees)
  | isClose mpt x y = accel mpt x y m
  | otherwise       = (sumP xs, sumP ys) 
      where
        (xs, ys) = unzipP [: calcAccel mpt st | st <- subtrees:]


isClose (MP x1 y1 m) x2 y2 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) < eClose


-- accel:: MassPoint :*: MassPoint -> Accel
accel (MP x1  y1 _) x2 y2 m  
       | r < epsilon  = (0.0, 0.0) 
       | otherwise    = (aabs * dx / r , aabs * dy / r)  
                                             where 
                                               rsqr = (dx * dx) + (dy * dy) 
                                               r    = sqrt rsqr 
                                               dx   = x1 - x2 
                                               dy   = y1 - y2 
                                               aabs = m / rsqr 

