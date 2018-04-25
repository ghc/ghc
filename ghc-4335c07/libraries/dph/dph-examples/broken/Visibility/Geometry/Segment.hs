
module Geometry.Segment
	( Segment
	, translateSegment
	, splitSegmentsOnY
	, splitSegmentsOnX
	, chooseSplitX
	, chooseClippingSegment
	, polarOfRectSeg)
where
import Geometry.Point
import Geometry.Intersection
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)
import Data.Maybe
import Data.Function

-- | A line segement in the 2D plane.
type Segment coord
	= (Int, (Double, Double), (Double, Double))


-- | Translate both endpoints of a segment.
translateSegment :: Double -> Double -> Segment c -> Segment c
translateSegment tx ty (n, (x1, y1), (x2, y2))
	= (n, (x1 + tx, y1 + ty), (x2 + tx, y2 + ty))
	

-- | Split segments that cross the line y = y0, for some y0.
splitSegmentsOnY :: Double -> Vector (Segment c) -> Vector (Segment c)
splitSegmentsOnY y0 segs
 = let	
	-- TODO: we only need to know IF the seg crosse the line here,
	--       not the actual intersection point. Do a faster test.
	(segsCross, segsOther)
		= V.unstablePartition 
			(\(_, p1, p2) -> isJust $ intersectSegHorzLine p1 p2 y0)
			segs

	-- TODO: going via lists here is bad.
	splitCrossingSeg :: Segment c -> Vector (Segment c)
	splitCrossingSeg (n, p1, p2)
	 = let	Just pCross	= intersectSegHorzLine p1 p2 y0
	   in	V.fromList [(n, p1, pCross), (n, pCross, p2)]
	
	-- TODO: vector append requires a copy.	
   in	segsOther V.++ (V.concat $ map splitCrossingSeg $ V.toList segsCross)


-- | Split segments that cross the line x = x0, for some x0.
splitSegmentsOnX :: Double -> Vector (Segment c) -> Vector (Segment c)
splitSegmentsOnX x0 segs
 = let	
	-- TODO: we only need to know IF the seg crosse the line here,
	--       not the actual intersection point. Do a faster test.
	(segsCross, segsOther)
		= V.unstablePartition 
			(\(_, p1, p2) -> isJust $ intersectSegVertLine p1 p2 x0)
			segs

	-- TODO: going via lists here is bad.
	splitCrossingSeg :: Segment c -> Vector (Segment c)
	splitCrossingSeg (n, p1, p2)
	 = let	Just pCross	= intersectSegVertLine p1 p2 x0
	   in	V.fromList [(n, p1, pCross), (n, pCross, p2)]
	
	-- TODO: vector append requires a copy.	
   in	segsOther V.++ (V.concat $ map splitCrossingSeg $ V.toList segsCross)


-- | Decide where to split the plane.
--   TODO: We're just taking the first point of the segment in the middle of the vector.
--	It might be better to base the split on:
--	 - the closest segment
--	 - the widest sgement
--	 - the one closes to the middle of the field.
--       - some combination of above.
--
chooseSplitX :: Vector (Segment c) -> Double
chooseSplitX segments
 = let	Just (_, (x1, _), _)	= segments V.!? (V.length segments `div` 2)
   in	x1


-- | Choose a segement to use for clipping.
chooseClippingSegment :: Vector (Segment Polar) -> Segment Polar
chooseClippingSegment segs
	= V.maximumBy (compare `on` clippingScoreOfPolarSegment) segs


-- | Heuristic to estimate how good this segement will be for clipping.
clippingScoreOfPolarSegment :: Segment Polar -> Double
clippingScoreOfPolarSegment (_, (x1, y1), (x2, y2))
	= (abs (x2 - x1)) / (y1 + y2)


-- | Convert a segment from rectangular to polar coordinates.
polarOfRectSeg :: Segment Rect -> Segment Polar
polarOfRectSeg (n, p1@(x1, y1), p2@(x2, y2))
	| y2 == 0 && x2 > 0
	= if y1 >= 0
		then (n, polarOfRectPoint p1, (magPoint p2, 0))
		else (n, polarOfRectPoint p1, (magPoint p2, 2 * pi))
	
	| y1 == 0 && x1 > 0
	= if y2 >= 0
		then (n, (magPoint p1, 0),	polarOfRectPoint p2)
		else (n, (magPoint p1, 2 * pi),	polarOfRectPoint p2)
	
	| otherwise
	= (n, polarOfRectPoint p1, polarOfRectPoint p2)


