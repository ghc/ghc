{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Solver
        (calcAccelsWithBoxPA)
where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Double as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude

data BoundingBox        
        = Box   Double          -- lower left  X
                Double          -- lower left  Y
                Double          -- upper right X
                Double          -- upper right Y

data MassPoint
        = MP    Double          -- pos X
                Double          -- pos Y
                Double          -- mass

type Accel      
        = (Double, Double)

data BHTree
        = BHT   Double          -- size of cell
                Double          -- centroid X
                Double          -- centroid Y
                Double          -- centroid mass
                [:BHTree:]      -- children


calcAccelsWithBoxPA
        :: Double
        -> Double -> Double -> Double -> Double
        -> PArray (Double, Double, Double)
        -> PArray (Double, Double)

calcAccelsWithBoxPA epsilon llx lly rux ruy mpts
 = let  mpts'   = [: MP x y m | (x, y, m) <- fromPArrayP mpts :]
        accs'   = calcAccelsWithBox epsilon llx lly rux ruy mpts'
   in   toPArrayP accs'
{-# NOINLINE calcAccelsWithBoxPA #-}    

-- | Given the extend of a bounding box containing all the points,
--   calculate the accelerations on all of them.
calcAccelsWithBox
        :: Double
        -> Double -> Double -> Double -> Double
        -> [: MassPoint :]
        -> [: Accel :]

calcAccelsWithBox epsilon llx lly rux ruy mspts
 = accs
 where  accs = [: calcAccel epsilon m tree | m <- mspts :]
        tree = buildTree (Box llx lly rux ruy) mspts


-- | Build the Barnes-Hut quadtree tree.
buildTree :: BoundingBox -> [: MassPoint :] -> BHTree
buildTree bb particles
 | lengthP particles I.<= 1     = BHT s x y m emptyP
 | otherwise                    = BHT s x y m subTrees
 where  (MP x y m)              = calcCentroid particles
        (boxes, splitPnts)      = splitPoints bb particles 
        subTrees                = [:buildTree bb' ps | (bb', ps) <- zipP boxes splitPnts:]
  
        (Box llx lly rux ruy)   = bb
        sx                      = rux D.- llx
        sy                      = ruy D.- lly
        s                       = if sx D.< sy then sx else sy


-- | Split massPoints according to their locations in the quadrants.
splitPoints
        :: BoundingBox
        -> [: MassPoint :]
        -> ([:BoundingBox:], [:[: MassPoint :]:])

splitPoints b@(Box llx lly rux  ruy) particles 
  | noOfPoints I.<= 1 = (singletonP b, singletonP particles)
  | otherwise         
  = unzipP [: (b,p) | (b,p) <- zipP boxes splitPars, lengthP p I.> 0:]
  where noOfPoints      = lengthP particles
        lls             = [: p | p <- particles, inBox b1 p :]
        lus             = [: p | p <- particles, inBox b2 p :]
        rus             = [: p | p <- particles, inBox b3 p :]
        rls             = [: p | p <- particles, inBox b4 p :]
        b1              = Box llx  lly  midx midy
        b2              = Box llx  midy midx  ruy
        b3              = Box midx midy rux   ruy
        b4              = Box midx lly  rux  midy
        boxes           = singletonP b1  +:+ singletonP b2  +:+ singletonP b3 +:+ singletonP b4 
        splitPars       = singletonP lls +:+ singletonP lus +:+ singletonP rus +:+ singletonP rls
        (midx,  midy)   = ((llx D.+ rux) D./ 2.0 , (lly D.+ ruy) D./ 2.0) 


-- | Checks if particle is in box (excluding left and lower border)
inBox :: BoundingBox -> MassPoint -> Bool
inBox (Box llx  lly rux  ruy) (MP px  py  _) 
        = (px D.> llx) && (px D.<= rux) && (py D.> lly) && (py D.<= ruy)


-- | Calculate the centroid of some points.
calcCentroid:: [:MassPoint:] -> MassPoint
calcCentroid mpts 
 = MP  (sumP xs / mass) (sumP ys / mass) mass
 where  mass     = sumP [: m | MP _ _ m  <- mpts :]
        (xs, ys) = unzipP [: (m D.* x, m D.* y) | MP x y m <- mpts :]   


-- | Calculate the accelleration of a point due to the points in the given tree.
calcAccel :: Double -> MassPoint -> BHTree -> (Double, Double)
calcAccel epsilon mpt (BHT s x y m subtrees)
        | lengthP subtrees I.== 0
        = accel epsilon mpt (MP x y m)

        | isFar mpt s x y 
        = accel epsilon mpt (MP x y m)

        | otherwise
        = let   (xs, ys) = unzipP [: calcAccel epsilon mpt st | st <- subtrees :]
          in    (sumP xs, sumP ys)


-- | Calculate the acceleration on a point due to some other point.
accel   :: Double       -- ^ If the distance between the points is smaller than this
                        --   then ignore the forces between them.
        -> MassPoint    -- ^ The point being acclerated.
        -> MassPoint    -- ^ Neibouring point.
        -> Accel

accel epsilon (MP x1 y1 _) (MP x2 y2 m)  
 = (aabs D.* dx D./ r , aabs D.* dy D./ r)  
 where  rsqr = (dx D.* dx) D.+ (dy D.* dy) D.+ epsilon D.* epsilon
        r    = sqrt rsqr 
        dx   = x1 D.- x2 
        dy   = y1 D.- y2 
        aabs = m D./ rsqr 


-- | If the point is far from a cell in the tree then we can use
--   it's centroid as an approximation of all the points in the region.
isFar   :: MassPoint    -- point being accelerated
        -> Double       -- size of region
        -> Double       -- position of center of mass of cell
        -> Double       -- position of center of mass of cell
        -> Bool

isFar (MP x1 y1 m) s x2 y2 
 = let  dx      = x2 D.- x1
        dy      = y2 D.- y1
        dist    = sqrt (dx D.* dx D.+ dy D.* dy)
   in   (s D./ dist) D.< 1

