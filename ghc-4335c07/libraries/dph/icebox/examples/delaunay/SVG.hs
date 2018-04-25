module SVG where

import Types

svg :: [Point] -> [(Point,Point)] -> String
svg pts edges = unlines
	$  [ "<svg width=\"100%\" height=\"100%\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">" ]
        ++ [svgEdge  (roundp p) (roundp q) | (p,q) <- edges]
        ++ [svgPoint (roundp p) | p <- pts]
	++ ["</svg>"]

svgPoint :: (Int,Int) -> String
svgPoint (x, y)
	= "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"1\""
	++ " style=\"stroke:#000000\""
	++ "/>"

svgEdge :: (Int,Int) -> (Int,Int) -> String
svgEdge (x1,y1) (x2,y2)
        = "<line x1=\"" ++ show x1 ++ "\" y1 =\"" ++ show y1
          ++ "\" x2=\"" ++ show x2 ++ "\" y2 =\"" ++ show y2
          ++ "\" style=\"stroke:#ff0000;stroke-width:1\"/>"

roundp :: Point -> (Int,Int)
roundp (x,y) = (round x, round y)

