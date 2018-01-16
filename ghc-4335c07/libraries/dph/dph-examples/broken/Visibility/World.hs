
module World
	( Segment
	, World(..), Rect, Polar
	, initialWorld
	, normaliseWorld
	, polarOfRectWorld)
where
import Geometry.Point
import Geometry.Segment
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)


-- We keep this unpacked so we can use unboxed vector.
-- index, x1, y1, x2, y2
data World coord 
	= World
	{ worldSegments	:: Vector (Segment coord) }


-- | Generate the initial world.
initialWorld :: IO (World Rect)
initialWorld
 = do	let n		= 100
	let minZ	= -400
	let maxZ	= 400
	
	let minDelta	= -100
	let maxDelta	= 100
	
	let centers	= genPointsUniform n minZ maxZ
	let deltas	= genPointsUniformWithSeed 4321 n minDelta maxDelta

	let makePoint n' (cX, cY) (dX, dY)
			= (n', (cX, cY), (cX + dX, cY + dY))

	let segs	= V.zipWith3 makePoint (V.enumFromTo 0 (n - 1)) centers deltas
	
	return $ World segs


-- | Normalise the world so that the given point is at the origin,
--   and split segements that cross the y=0 line.
normaliseWorld :: Point Rect -> World Rect -> World Rect
normaliseWorld (px, py) world
 = let	segments_trans	= V.map (translateSegment (-px) (-py)) 
			$ worldSegments world
			
	segments_split	= splitSegmentsOnY 0 segments_trans
			
   in	world { worldSegments = segments_split }


-- | Convert a world from rectangular to polar coordinates.
polarOfRectWorld :: World Rect -> World Polar
polarOfRectWorld world
	= World 
	{ worldSegments	= V.map polarOfRectSeg (worldSegments world) }


