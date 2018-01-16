{-# LANGUAGE PatternGuards #-}
-- | Drawing the world as a gloss picture.
module Gloss.Draw
	(drawWorld)
where
import Common.World
import Common.Body
import Graphics.Gloss
import qualified Solver.ListBH.Solver		as L
import qualified Data.Vector.Unboxed		as V


-- | Radius of the circle representing each body.
pointSize :: Float
pointSize	= 4


-- | Draw the world, and optionally show the Barnes-Hut tree.
--   NOTE: We always show the list version tree, which might not be the one
--         that's actually being used to calculate the accelerations.
--         To display the other trees we'd have to write draw functions for them,
--         or functions to convert them to the list version.
--
drawWorld :: Bool -> World -> Picture
drawWorld shouldDrawTree world
 = let	picPoints	= Color (makeColor 1 1 1 0.4)
			$ Pictures 
			$ map drawBody
			$ V.toList 
			$ worldBodies world

   	picTree		= drawBHTree
			$ L.buildTree 
			$ map massPointOfBody
			$ V.toList 
			$ worldBodies world

   in	Pictures 
		[ if shouldDrawTree 
			then Color (makeColor 0.5 1.0 0.5 0.2) $ picTree
			else Blank
			
		, picPoints ]


-- | Draw a list version Barnes-Hut tree.
drawBHTree :: L.BHTree -> Picture
drawBHTree bht
 = drawBHTree' 0 bht

drawBHTree' depth bht
 = let	
	-- The bounding box
	L.Box left down right up	= L.bhTreeBox bht
	[left', down', right', up']	= map realToFrac [left, down, right, up]

	picCell		= lineLoop [(left', down'), (left', up'), (right', up'), (right', down')]


	-- Draw a circle with an area equal to the mass of the centroid.
	centroidX	= realToFrac $ L.bhTreeCenterX bht
	centroidY	= realToFrac $ L.bhTreeCenterY bht
	
	centroidMass	= L.bhTreeMass bht
	circleRadius	= realToFrac $ sqrt (centroidMass / pi)

	midX		= (left' + right') / 2
	midY		= (up'   + down')  / 2

	picCentroid	
	 | _:_	<- L.bhTreeBranch bht
	 , depth >= 1
	 = Color (makeColor 0.5 0.5 1.0 0.4)
		$  Pictures
			[ Line [(midX, midY), (centroidX, centroidY)]
			, Translate centroidX centroidY 
			$ ThickCircle
				(circleRadius * 4 / 2) 
				(circleRadius * 4) ]
			
	 | otherwise
	 = Blank

	-- The complete picture for this cell.
	picHere		= Pictures [picCentroid, picCell]
		
	-- Pictures of children.
	picSubs		= map (drawBHTree' (depth + 1))
			$ L.bhTreeBranch bht

   in	Pictures (picHere : picSubs)


-- | Draw a single body.
drawBody :: Body -> Picture
drawBody ((x, y, _), _, _)
	= drawPoint (x, y)


-- | Draw a point using a filled circle.
drawPoint :: (Double, Double) -> Picture
drawPoint (x, y)
	= Translate (realToFrac x) (realToFrac y) 
	$ ThickCircle (pointSize / 2) pointSize
