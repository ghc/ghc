{-# LANGUAGE BangPatterns, PatternGuards #-}

module QuickHullVector
	(quickHull)
where
import Points2D.Types
import Data.Function
import Data.Vector.Unboxed		as V
import Data.Vector.Unboxed.Mutable	as MV
import Data.Vector.Unboxed		(Vector)
import qualified Data.Vector.Generic	as G
import System.IO.Unsafe



distance :: Point -> Point -> Point -> Double
{-# INLINE distance #-}
distance (x1, y1) (x2, y2) (xo, yo)
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)


hsplit :: Vector Point -> Point -> Point -> Vector Point
{-# INLINE hsplit #-}
hsplit !points !p1@(!p1X, !p1Y) !p2@(!p2X, !p2Y)
 = let !packed	= packPoints points p1X p1Y p2X p2Y
   in  if V.length packed == 1
	 then V.singleton p1
	 else 	let pm		= packed `V.unsafeIndex` (V.length packed - 1)
		    packed'	= V.unsafeSlice 0 (V.length packed - 1) packed
		in  hsplit packed' p1 pm V.++ hsplit packed' pm p2


packPoints :: Vector Point -> Double -> Double -> Double -> Double -> Vector Point
{-# INLINE packPoints #-}
packPoints !points !p1X !p1Y !p2X !p2Y
 = G.create 
 $ do	packed	<- MV.new (V.length points + 1)
	
	-- stash the furthest point on the end of the returned vector.	
	let fill !pMax !distMax !ixPoints !ixPacked
		| ixPoints >= V.length points	
		= do	MV.unsafeWrite packed ixPacked pMax
			return $ MV.unsafeSlice 0 (ixPacked + 1) packed

		| p	<- points `V.unsafeIndex` ixPoints
		, d	<- distance (p1X, p1Y) (p2X, p2Y) p
		, d > 0
		= do	MV.unsafeWrite packed ixPacked p
			if d > distMax
			 then	fill p    d       (ixPoints + 1) (ixPacked + 1)
			 else	fill pMax distMax (ixPoints + 1) (ixPacked + 1)
			
		| otherwise
		= fill pMax distMax (ixPoints + 1) ixPacked
			
	fill (0, 0) 0 0 0


quickHull :: Vector Point -> Vector Point
quickHull !points
  	| V.length points == 0	= points

	| (minx, maxx) 		<- minmax points
	= hsplit points minx maxx V.++ hsplit points maxx minx


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
	
