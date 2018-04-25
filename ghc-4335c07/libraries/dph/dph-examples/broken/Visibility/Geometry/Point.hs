
module Geometry.Point
	( Rect, Polar
	, Point
	, magPoint
	, argPoint
	, normaliseAngle
	, polarOfRectPoint
	, genPointsUniform
	, genPointsUniformWithSeed)
where
import qualified Points2D.Generate	as P
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)

-- | Coordinate system.
data Rect
data Polar

-- | A 2d point in some coordinate system.
type Point coords
	= (Double, Double)


-- | Take the magnitude of a point.
magPoint :: Point Rect -> Double
magPoint (x, y)
	= sqrt (x * x + y * y)
	

-- | Take the angle of a point, between 0 and 2*pi radians.
argPoint :: Point Rect -> Double
argPoint (x, y)
	= normaliseAngle $ atan2 y x


-- | Normalise an angle to be between 0 and 2*pi radians
normaliseAngle :: Double -> Double
normaliseAngle f	
	| f < 0		= normaliseAngle (f + 2 * pi)
	| f > 2 * pi	= normaliseAngle (f - 2 * pi)
	| otherwise	= f


-- | Convert a point from rectangular to polar coordinates.
polarOfRectPoint :: Point Rect -> Point Polar
polarOfRectPoint p
 = let	r	= magPoint p
	theta	= argPoint p
   in	(r, theta)


-- | Generate some random points
genPointsUniform :: Int -> Double -> Double -> Vector (Point a)
genPointsUniform n mi mx 
 	= V.map (\(x, y) -> (x, y))
	$ P.genPointsUniform n mi mx
	
	
-- | Generate some random points using a seed value
genPointsUniformWithSeed :: Int -> Int -> Double -> Double -> Vector (Point a)
genPointsUniformWithSeed seed n mi mx 
 	= V.map (\(x, y) -> (x, y))
	$ P.genPointsUniformWithSeed seed n mi mx