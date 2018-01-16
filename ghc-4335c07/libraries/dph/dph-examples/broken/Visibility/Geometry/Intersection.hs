
module Geometry.Intersection
	( intersectSegHorzLine
	, intersectSegVertLine)
where
import Points2D.Types


-- | Get the point where a segment crosses a horizontal line, if any.
intersectSegHorzLine :: Point -> Point -> Double -> Maybe Point
intersectSegHorzLine (x1, y1) (x2, y2) y0
	
	-- seg is on line
	| y1 == y0, y2 == y0	= Nothing
	
	-- seg is above line
	| y1 > y0,  y2 > y0	= Nothing
	
	-- seg is below line
	| y1 < y0,  y2 < y0	= Nothing
	
	-- seg is a single point on the line.
	-- this should be caught by the first case, 
	-- but we'll test for it anyway.
	| y2 - y1 == 0		
	= Just (x1, y1)
	
	| otherwise		
	= Just ( (y0 - y1) * (x2 - x1) / (y2 - y1) + x1
	       , y0)


-- | Get the point where a segment crosses a vertical line, if any.
intersectSegVertLine :: Point -> Point -> Double -> Maybe Point
intersectSegVertLine (x1, y1) (x2, y2) x0
	
	-- seg is on line
	| x1 == x0, x2 == x0	= Nothing
	
	-- seg is to right of line
	| x1 > x0,  x2 > x0	= Nothing
	
	-- seg is to left of line
	| x1 < x0,  x2 < x0	= Nothing
	
	-- seg is a single point on the line.
	-- this should be caught by the first case, 
	-- but we'll test for it anyway.
	| x2 - x1 == 0		
	= Just (x1, y1)
	
	| otherwise		
	= Just (  x0
	       , (x0 - x1) * (y2 - y1) / (x2 - x1) + y1)
