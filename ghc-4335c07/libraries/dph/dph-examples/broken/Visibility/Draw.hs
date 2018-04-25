
module Draw
	( drawState
	, drawWorld)
where
import State
import World
import Geometry.Intersection
import Geometry.Segment
import Geometry.Point
import qualified Graphics.Gloss		as G
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)
import Data.Maybe

dtof :: Double -> Float
dtof	= fromRational . toRational

drawState :: State -> G.Picture
drawState state
 	| ModeDisplayWorld 	<- stateModeDisplay state
 	= drawWorldWithViewPos (stateViewPos state) (stateWorld state)

	| ModeDisplayNormalised <- stateModeDisplay state
	= drawWorldWithViewPos (0, 0) 
	$ normaliseWorld (stateViewPos state)
	$ stateWorld state

	| ModeDisplayPolar	<- stateModeDisplay state
	= let	
		segsPolar	= V.map projectPolarSegment
				$ worldSegments
				$ polarOfRectWorld
				$ normaliseWorld (stateViewPos state)
				$ stateWorld state

		segClipping	= chooseClippingSegment segsPolar
	
	  	picWorld	= G.Color G.white $ drawSegments segsPolar
		picClipper	= G.Color G.red   $ drawSegment segClipping

	  in	G.Translate 0 (-400)
		 $ G.Scale 400 1
		 $ G.Pictures [picWorld, picClipper]
	

	| otherwise
	= G.Blank
	


drawWorldWithViewPos :: Point Rect -> World Rect -> G.Picture
drawWorldWithViewPos (px, py) world
 = let	
	-- the world 
	picWorld	= G.Color G.white
			$ drawWorld world

	-- view position indicator
	picDude		= G.Color G.green
			$ G.Translate (dtof px) (dtof py) 
			$ G.ThickCircle 2 4

	-- crossings
	ptCrossings
		= catMaybes
		$ [ intersectSegHorzLine p1 p2 py
				| (_, p1, p2) <- V.toList $ worldSegments world ]

	picCrossings	= G.Pictures
			$ [ G.Color G.red
				$ G.Translate (dtof x) (dtof y)
				$ G.ThickCircle 1 2 
				| (x, y)	<- ptCrossings]


   in	G.Pictures [picWorld, picDude, picCrossings]


projectPolarSegment :: Segment Polar -> Segment Rect
projectPolarSegment (n, p1, p2)
	= (n, projectPolarPoint p1, projectPolarPoint p2)


projectPolarPoint :: Point Polar -> Point Rect
projectPolarPoint (r, a)
	= ((a - pi) / pi, r)
		

drawWorld :: World coord -> G.Picture
drawWorld world
	= drawSegments
	$ worldSegments world


drawSegments :: Vector (Segment Rect) -> G.Picture
drawSegments segments
	= G.Pictures
	$ map drawSegment
	$ V.toList 
	$ segments


drawSegment :: Segment Rect -> G.Picture
drawSegment (_, (x1, y1), (x2, y2))
	= G.Line [(f x1, f y1), (f x2, f y2)]
	where	f	= fromRational . toRational

