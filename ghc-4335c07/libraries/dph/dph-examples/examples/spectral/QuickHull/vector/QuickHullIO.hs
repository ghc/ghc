{-# LANGUAGE BangPatterns, PatternGuards, RankNTypes #-}

module QuickHullIO
	(quickHull)
where
import Data.Function
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.ST
import GHC.Conc
import Data.IORef
import Data.List
import Data.Ord
import Data.Vector.Unboxed			(Vector)
import qualified Data.Vector.Unboxed		as V
import qualified Data.Vector.Unboxed.Mutable	as MV
import qualified Data.Vector.Generic		as G
import Debug.Trace

type Point	= (Double, Double)
type Line	= (Point, Point)


-- | Compute the convex hull of a vector of points.
quickHull :: Vector Point -> IO (Vector Point)
quickHull !points
  | V.length points == 0	
  = return points

  | otherwise
  = do	-- Find the left and right-most points.
	let (minx, maxx) 	= minmax points

	-- Hull points get written to the vector in this IORef.
	hullRef	<- newIORef V.empty

	-- Fork off computations to handle half of the points each.
	-- For uniformly distributed points this first iteration takes most of the time.
	parIO 	[ hsplit hullRef points minx maxx
		, hsplit hullRef points maxx minx]

	-- Grab the finished hull points.
	hull	<- readIORef hullRef

	-- We've got the hull points, but they can appear in arbitrary order.
	-- Do a rubbish via-lists merge phase so that they appear clockwise around the edge.
	-- This isn't too expensive if there aren't many points on the hull.
	let (above, below) 
		= V.unstablePartition 
			(\p -> distance minx maxx p > 0)
			hull
	
	let aboveSorted	= V.fromList $ sortBy (comparing fst) $ V.toList above
	let belowSorted	= V.fromList $ sortBy (comparing fst) $ V.toList below
	let hull' = aboveSorted V.++ V.reverse belowSorted

	return hull'
	

hsplit :: IORef (Vector Point) -> Vector Point -> Point -> Point -> IO ()
{-# INLINE hsplit #-}
hsplit hullRef !points !p1@(!p1X, !p1Y) !p2@(!p2X, !p2Y)
	-- we've found one.
	| V.length packed == 0
	= addHullPoint hullRef p1
	
	-- do the two new segments in parallel.
	| V.length packed > 1000
	= parIO
		[ hsplit hullRef packed p1 pm
		, hsplit hullRef packed pm p2 ]
		
	| otherwise
	= do	hsplit hullRef packed p1 pm
		hsplit hullRef packed pm p2

	where	(packed, pm)	= parPackPoints points p1X p1Y p2X p2Y
	

-- | Copy points from the input vector that are on the left of the line into a
--	new buffer. While we're doing this, determine the point that is furthest
--	from the line.
--
--	If we have a big enough vector then split it in two and do both halves
--	in parallel. Doing this requires a copy afterwards to join the two
--	results back together. It's a trade off between decreased FP load and 
--	increased memory traffic. 
--
parPackPoints 
	:: Vector Point 
	-> Double -> Double
	-> Double -> Double
	-> ( Vector Point
	   , Point)
	
{-# INLINE parPackPoints #-}
parPackPoints !points !p1X !p1Y !p2X !p2Y
 |   numCapabilities == 1
  || V.length points < 1000
 = packPoints p1X p1Y p2X p2Y points

 | otherwise
 = let	
	numSegments	= numCapabilities

	-- Total number of points to process.
	lenPoints	= V.length points

	-- How many points to process in each segment.
	lenSeg		= lenPoints `div` numSegments

 	-- If the total number of points doesn't divide evenly into segments
	-- then there may be an odd number. Make sure to get the rest into the last segment.
	splitPacked count ixStart 
	    | count == 0	= []

	    | count == 1	
	    = let points'		= V.unsafeSlice ixStart (lenPoints - ixStart) points
	      	  result@(packed', _)	= packPoints p1X p1Y p2X p2Y points'
	      in  packed' `pseq` (result : [])

	    | otherwise	
	    = let points'		= V.unsafeSlice ixStart lenSeg points
	          result@(packed', _)	= packPoints p1X p1Y p2X p2Y points'
		  rest			= splitPacked (count - 1) (ixStart + lenSeg)
	      in  packed' `par` rest `par` (result : rest)

	results	= splitPacked numSegments 0
	vResult	= concatVectors $ map fst results
	pMax	= selectFurthest p1X p1Y p2X p2Y results
	
   in	(vResult, pMax)


selectFurthest 
 	:: Double -> Double 
	-> Double -> Double
	-> [(Vector Point, Point)] 
	-> Point
	
selectFurthest !p1X !p1Y !p2X !p2Y ps
 = go (0, 0) 0 ps

 where	go pMax !distMax []	
	 = pMax

	go pMax !distMax ((packed, pm):rest)
	 | V.length packed == 0
	 = go pMax distMax rest
	
	 | otherwise
	 , dist		<-  distance (p1X, p1Y) (p2X, p2Y) pm 
  	 = if dist > distMax
		then go pm   dist    rest
		else go pMax distMax rest


packPoints 
	:: Double -> Double 		-- First point on dividing line.
	-> Double -> Double 		-- Second point on dividing line.
	-> Vector Point 		-- Source points.
	-> ( Vector Point		-- Packed vector containing only points on the left of the line.
	   , Point)			-- The point on the left that was furthest from the line.

{-# INLINE packPoints #-}
packPoints !p1X !p1Y !p2X !p2Y !points 
 = let
	result	
	 = G.create 
 	 $ do	packed		 <- MV.new (V.length points + 1)
		(pMax, ixPacked) <- fill points packed p1X p1Y p2X p2Y 0 0

		-- We stash the maximum point on the end of the vector to get
		-- it through the create call.
		MV.unsafeWrite packed ixPacked pMax
		return $ MV.unsafeSlice 0 (ixPacked + 1) packed
	
   in	( V.unsafeSlice 0 (V.length result - 1) result
	, result `V.unsafeIndex` (V.length result - 1))
			

fill 	:: forall s
	.  Vector Point 		-- Source points.
	-> MV.MVector s Point 		-- Vector to write packed points into.
	-> Double -> Double 		-- First point on dividing line.
	-> Double -> Double		-- Second poitn on dividing line.
	-> Int 				-- Index into source points to start reading from.
	-> Int				-- Index into packed points to start writing to.
	-> ST s 
		( Point			-- Furthest point from the line that was found.
		, Int)			-- The number of packed points written.

{-# INLINE fill #-}
fill !points !packed !p1X !p1Y !p2X !p2Y !ixPoints' !ixPacked'
 = go (0, 0) 0 ixPoints' ixPacked'
 where go pMax !distMax !ixPoints !ixPacked
	| ixPoints >= V.length points	
	= do	return (pMax, ixPacked)
		
	| p	<- points `V.unsafeIndex` ixPoints
	, d	<- distance (p1X, p1Y) (p2X, p2Y) p
	, d > 0
	= do	MV.unsafeWrite packed ixPacked p
		if d > distMax
		 then	go p    d       (ixPoints + 1) (ixPacked + 1)
		 else	go pMax distMax (ixPoints + 1) (ixPacked + 1)
			
	| otherwise
	= go pMax distMax (ixPoints + 1) ixPacked


minmax :: Vector Point -> (Point, Point)
{-# INLINE minmax #-}
minmax !vec
 = go first first 0
 where	first	= vec V.! 0

	go pMin@(!minX, !minY) pMax@(!maxX, !maxY) !ix
	  | ix >= V.length vec	= (pMin, pMax)

	  | (x, y)	<- vec `V.unsafeIndex` ix
	  = if       x < minX then go (x, y) pMax   (ix + 1)
	    else if  x > maxX then go pMin   (x, y) (ix + 1)
	    else go pMin pMax (ix + 1)
	

distance :: Point -> Point -> Point -> Double
{-# INLINE distance #-}
distance (x1, y1) (x2, y2) (xo, yo)
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)


addHullPoint :: IORef (Vector Point) -> Point -> IO ()
addHullPoint hullRef p
 = atomicModifyIORef hullRef
 $ \hull -> (V.singleton p V.++ hull, ())



-- Can't find an equivalent for this in Control.Concurrent.
parIO :: [IO ()] -> IO ()
parIO stuff
 = do	mVars	<- replicateM (length stuff) newEmptyMVar
	zipWithM_ (\c v -> forkIO $ c `finally` putMVar v ()) stuff mVars
	mapM_ readMVar mVars
	

-- We really want a function in the vector library for this.
concatVectors :: [Vector Point] -> Vector Point
{-# NOINLINE concatVectors #-}
concatVectors vectors
 = G.create
 $ do	let len	= sum $ map V.length vectors
	vOut	<- MV.new len
	go vectors vOut 0
	return vOut

 where	{-# INLINE go #-}
	go [] _ _
	 = return ()

	go (vSrc:vsSrc) vDest !ixStart	
	 = do	let lenSrc	= V.length vSrc
		let vDestSlice	= MV.unsafeSlice ixStart lenSrc vDest
		V.copy vDestSlice vSrc
		go vsSrc vDest (ixStart + lenSrc) 

