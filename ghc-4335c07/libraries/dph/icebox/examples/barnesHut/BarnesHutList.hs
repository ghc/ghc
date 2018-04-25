module BarnesHutList (oneStep)

where

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Parallel

import Debug.Trace

data Point = Point Double Double
data BoundingBox   = Box Double Double Double Double

data MassPoint = MP Double Double Double -- xpos ypos mass

data BHTree = BHT Double Double Double -- root mass point
                  [BHTree]



epsilon:: Double
epsilon = 0.05

eClose::Double
eClose  = 0.5



testList = [
       (0.3, 0.2, 5.0),
       (0.2, 0.1, 5.0),
       (0.1, 0.2, 5.0),
       (0.8, 0.8, 5.0),
       (0.7, 0.9, 5.0), 
       (0.8, 0.9, 5.0),
       (0.6, 0.6, 5.0),
       (0.7, 0.7, 5.0),
       (0.8, 0.7, 5.0),
       (0.9, 0.9, 5.0)]

{-
oneStep:: Double -> Double -> Double -> Double -> [(Double, Double, Double)]  -> ([Double], [Double]) 
oneStep  llx lly rux ruy mspnts = trace (show res) (res, res)
  where
    
    ms    = [ MP x y m | (x,y, m) <- testList]
    tree  = buildTree (Box llx lly rux ruy) ms
--    res  = [: calcCentroid (singletonP m) | m <- ms]
    res = flattenTree tree
-}



flattenTree:: BHTree -> [Double]
flattenTree (BHT x y m subtrees) =   [x,y,m] ++ concat [ flattenTree t | t <- subtrees]

--
--


--oneStep:: Double -> Double -> Double -> Double -> P(Double, Double, Double)  -> (PArray Double, PArray Double) 
oneStep llx lly rux ruy mspnts = 
    (xs, ys)
    where  
       (xs, ys) = unzip [ calcAccel m tree | m <- ms ]
       tree = buildTree (Box llx lly rux ruy) ms
       ms   = [ MP x y m | (x,y,m) <-  mspnts]

-- Phase 1: building the tree
--

buildTree:: BoundingBox -> [ MassPoint ] -> BHTree
buildTree bb particles
  | length particles <= 1 = BHT x y m []
  | otherwise             = BHT x y m subTrees
  where
    (MP x y m)           = calcCentroid particles
    (boxes, splitPnts)   = splitPoints bb particles 
    subTrees             = [buildTree bb' ps | (bb', ps) <- zip boxes splitPnts]
  
-- Split massPoints according to their locations in the quadrants
-- 
splitPoints:: BoundingBox -> [ MassPoint ] -> ([BoundingBox], [[ MassPoint ]])
splitPoints b@(Box llx lly rux  ruy) particles 
  | noOfPoints <= 1 = ([ b], [ particles])
  | otherwise         
   = unzip [ (b,p) | (b,p) <- zip boxes splitPars, length p > 0]
      where
        noOfPoints    = length particles
        lls           = [ p | p <- particles, inBox b1 p ]
        lus           = [ p | p <- particles, inBox b2 p ]
        rus           = [ p | p <- particles, inBox b3 p ]
        rls           = [ p | p <- particles, inBox b4 p ]
        b1 = Box llx  lly  midx midy
        b2 = Box llx  midy midx  ruy
        b3 = Box midx midy rux   ruy
        b4 = Box midx lly  rux  midy
        boxes         = [b1, b2,b3,b4]
        splitPars     = [lls, lus, rus, rls]
        (midx,  midy) = ((llx + rux) / 2.0 , (lly + ruy) / 2.0) 


-- checks if particle is in box (excluding left and lower border)
-- inBox:: BoundingBox -> MassPoint -> Bool
inBox (Box llx  lly rux  ruy) (MP px  py  _) = 
    (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)

calcCentroid:: [MassPoint] -> MassPoint
calcCentroid mpts = MP  ((sum xs)/mass) ((sum ys)/mass) mass
  where
    mass     = sum [ m | MP _ _ m  <- mpts ]
    (xs, ys) = unzip [ (m * x, m * y) | MP x y m <- mpts ]   


calcAccel:: MassPoint -> BHTree -> (Double, Double)
calcAccel mpt (BHT x y m subtrees)
  | isClose mpt x y = accel mpt x y m
  | otherwise       = (sum xs, sum ys) 
      where
        (xs, ys) = unzip [ calcAccel mpt st | st <- subtrees]


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

