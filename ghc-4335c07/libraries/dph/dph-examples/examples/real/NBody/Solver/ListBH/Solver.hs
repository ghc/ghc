{-# LANGUAGE BangPatterns, PatternGuards #-}

-- | The list version of the solver also builds the bounding box at every
--   node of the tree, which is good for visualisation.
module Solver.ListBH.Solver
	( MassPoint	(..)
	, BoundingBox	(..)
	, BHTree	(..)
	, calcAccels
	, buildTree)
where
import Common.Body

eClose :: Double
eClose  = square 500

square x = x * x

-- | A rectangular region in 2D space.
data BoundingBox
	= Box
	{ boxLowerLeftX	 :: {-# UNPACK #-} !Double
	, boxLowerLeftY	 :: {-# UNPACK #-} !Double
	, boxUpperRightX :: {-# UNPACK #-} !Double
	, boxUpperRightY :: {-# UNPACK #-} !Double }
	deriving Show
	
-- | The Barnes-Hut tree we use to organise the points.
data BHTree
	= BHT
	{ bhTreeBox	:: {-# UNPACK #-} !BoundingBox
	, bhTreeCenterX	:: {-# UNPACK #-} !Double
	, bhTreeCenterY	:: {-# UNPACK #-} !Double
	, bhTreeMass	:: {-# UNPACK #-} !Double
	, bhTreeBranch	:: ![BHTree] }
	deriving Show


-- | Compute the acclerations on all these points.
calcAccels :: Double -> [MassPoint] -> [Accel]
calcAccels epsilon mpts
	= map (calcAccel epsilon (buildTree mpts)) mpts
	

-- | Build a Barnes-Hut tree from these points.
buildTree :: [MassPoint] -> BHTree
buildTree mpts
 = let	(llx, lly, rux, ruy)	= findBounds mpts
	box			= Box llx lly rux ruy
   in	buildTreeWithBox box mpts


-- | Find the coordinates of the bounding box that contains these points.
findBounds :: [MassPoint] -> (Double, Double, Double, Double)
{-# INLINE findBounds #-}
findBounds ((x1, y1, _) : rest1)
 = go x1 y1 x1 y1 rest1	
 where	go !left !right !down !up pts
	 = case pts of
		[]	-> (left, down, right, up)
		(x, y, _) : rest
		 -> let	left'	= min left  x
			right'	= max right x
			down'	= min down  y
			up'	= max up    y
	   	    in	go left' right' down' up' rest


-- | Given a bounding box that contains all the points, 
--   build the Barnes-Hut tree for them.
buildTreeWithBox
	:: BoundingBox		-- ^ bounding box containing all the points.
	-> [MassPoint]		-- ^ points in the box.
	-> BHTree

buildTreeWithBox bb particles
  | length particles <= 1	= BHT bb x y m []
  | otherwise			= BHT bb x y m subTrees
  where	(x, y, m)		= calcCentroid particles
    	(boxes, splitPnts)	= splitPoints bb particles 
    	subTrees		= [buildTreeWithBox bb' ps | (bb', ps) <- zip boxes splitPnts]

  
-- | Split massPoints according to their locations in the quadrants.
splitPoints
	:: BoundingBox		-- ^ bounding box containing all the points.
	-> [MassPoint]		-- ^ points in the box.
	-> ( [BoundingBox]	-- 
	   , [[MassPoint]])

splitPoints b@(Box llx lly rux  ruy) particles 
  | noOfPoints <= 1 = ([b], [particles])
  | otherwise         
  = unzip [ (b,p) | (b,p) <- zip boxes splitPars, length p > 0]
  where
        noOfPoints	= length particles

	-- The midpoint of the parent bounding box.
        (midx,  midy)	= ((llx + rux) / 2.0 , (lly + ruy) / 2.0) 

	-- Split the parent bounding box into four quadrants.
        b1		= Box llx  lly  midx midy
        b2		= Box llx  midy midx  ruy
        b3		= Box midx midy rux   ruy
        b4		= Box midx lly  rux  midy
        boxes		= [b1,   b2,  b3,  b4]

	-- Sort the particles into the smaller boxes.
        lls		= [ p | p <- particles, inBox b1 p ]
        lus		= [ p | p <- particles, inBox b2 p ]
        rus		= [ p | p <- particles, inBox b3 p ]
        rls		= [ p | p <- particles, inBox b4 p ]
        splitPars	= [lls, lus, rus, rls]


-- | Check if a particle is in box (excluding left and lower border)
inBox:: BoundingBox -> MassPoint -> Bool
{-# INLINE inBox #-}
inBox (Box llx  lly rux  ruy) (px, py, _) 
	= (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)


-- | Calculate the centroid of some points.
calcCentroid :: [MassPoint] -> MassPoint
{-# INLINE calcCentroid #-}
calcCentroid mpts = (sum xs / mass, sum ys / mass, mass)
  where
    mass     = sum   [ m | (_, _, m)  <- mpts ]
    (xs, ys) = unzip [ (m * x, m * y) | (x, y,  m) <- mpts ]   


-- | Calculate the accelleration of a point due to the points in the given tree.
--   If the distance between the points is less then some small number
--   we set the accel to zero to avoid the acceleration going to infinity
--   and the points escaping the simulation. 
--
--   We also use this behavior as a hacky way to discard the acceleration
--   of a point due to interaction with itself.
--
calcAccel:: Double -> BHTree -> MassPoint -> (Double, Double)	
calcAccel !epsilon (BHT _ x y m subtrees) mpt
	| []	<- subtrees
	= accel epsilon mpt (x, y, m)
	
	| not $ isClose mpt x y
	= accel epsilon mpt (x, y, m)

	| otherwise
	= let	(xs, ys)  = unzip [ calcAccel epsilon st mpt | st <- subtrees]
	  in	(sum xs, sum ys) 


-- | If the a point is "close" to a region in the Barnes-Hut tree then we compute
--   the "real" acceleration on it due to all the points in the region, otherwise
--   we just use the centroid as an approximation of all the points in the region.
--
isClose :: MassPoint -> Double -> Double -> Bool
{-# INLINE isClose #-}
isClose (x1, y1, m) x2 y2 
	= (x1-x2) * (x1-x2) + (y1-y2) * (y1-y2) < eClose

