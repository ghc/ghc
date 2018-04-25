
module SVG where

-- Making a SVG diagram of the points and hull
makeSVG :: [(Int, Int)] -> [(Int, Int)] -> String
makeSVG points hull
	= unlines
	$  [ "<svg width=\"100%\" height=\"100%\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">" ]
	++ [svgPolygon hull]
	++ map svgPoint points
	++ map svgPointHull hull
	++ ["</svg>"]

svgPolygon  :: [(Int, Int)] -> String
svgPolygon points
	=  "<polygon"
	++ " points=\"" ++ (concat [show x ++ "," ++ show y ++ " " | (x, y) <- points]) ++ "\"" 
	++ " style=\"fill:#d0d0ff;stroke:#000000;stroke-width:1\""
	++ "/>"

svgPoint :: (Int, Int) -> String
svgPoint (x, y)
	= "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"0.5\""
	++ " style=\"stroke:#000000\""
	++ "/>"

svgPointHull :: (Int, Int) -> String
svgPointHull (x, y)
	= "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"1\""
	++ " style=\"stroke:#ff0000\""
	++ "/>"
	
roundPoints :: [(Double, Double)] -> [(Int, Int)]
roundPoints ps = [(round x, round y) | (x, y) <- ps]
