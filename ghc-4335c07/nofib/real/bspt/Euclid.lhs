> module Euclid

	Module contains definitions of Points and Lines and related
	functions relating to Euclidean Geometry


>	(	Point(..),Halfspace(..),Line,
> 		Face(..),mkFace,getSegment,getMyLine,
>		Faces,Segment,eqn,solve,space,
>		convert,invert,triangleArea,
> 		mkPoint,mkPolygon,drawSegment)

> where
> import Stdlib (map2,splitAt_YORK,pair,between,numval)
> import GeomNum
> import MGRlib (line)
> import Params (mouseDispx,mouseDispy,gap)
> import Data.Char(isDigit)--1.3


	The data type Line is used to describe a line.
		Ln a b c represents the line : ax + by + c = 0

> data Line = Ln Numb Numb Numb deriving (Show{-was:Text-},Eq)

	The data type Point defines a point in Euclidean space
		Pt x y  represents the point x units along the
			horizontal and y units down the vertical.
	
> data Point = Pt Numb Numb deriving (Eq,Show{-was:Text-})


	The Halfspace type enumerates the possible classifications of 
	a point with respect to a line.
	
> data Halfspace = Fore | Coin | Rear deriving (Eq,Show{-was:Text-})

	The type Face defines a line segment by its two end points.
		It also stores its line equation

> data Face = Fc Segment Line deriving (Eq,Show{-was:Text-})

	The type synonym Segment defines a line segment by its two end points.
                (pt1,pt2) is the line commencing at pt1 and finishing
                          at pt2.

> type Segment = (Point,Point)

	The type synonym Faces just abbreviates a list of Face.

> type Faces = [Face]

	mkFace: function constructor generates line automatically

> mkFace :: Segment -> Face
> mkFace (x,y) = Fc (x,y) (convert x y)

> getSegment :: Face -> Segment
> getSegment (Fc segment _) = segment

> getMyLine :: Face -> Line
> getMyLine (Fc _ line) = line

	space : determines the halfspace of pt w.r.t. line
		eqn returns a value representing the dot product of point/line
		zerO,positive are GeomNum class methods. 

> space :: Line -> Point -> Halfspace
> space line pt = if zerO val then Coin else
>		  if positive val then Fore
>		  else Rear
>			where val = eqn line pt

	eqn produces the dot product value of a point in a line equation.

> eqn :: Line -> Point -> Numb
> eqn (Ln a b c) (Pt x y) = a*x + b*y + c


	convert : Takes two points and produces the line equation of the line
		those points lie on. Note that ratio produces 
		diffx and diffy at the most reduced form ensuring that
		the Line definition is a canonical form.
		This makes comparing lines for equality trivial and
		is space preserving.

> convert :: Point -> Point -> Line
> convert (Pt x1 y1) (Pt x2 y2) = Ln (diffy) (-diffx) (diffx*y1-diffy*x1)
>                                 where 
>					dy=y2-y1
>                                       dx=x2-x1
>                                       (diffx,diffy) = ratio dx dy


	invert takes a line and inverts the orientation of its normal.
	NB: The convention is that a line ax+by+c has normal (a,-b).

> invert :: Line -> Line
> invert (Ln a b c) = (Ln (negate a) (negate b) (negate c))


	solve : Takes two line equations and produces the point at which
		they intersect. The point (Pt 0 0) is currently returned
		if the two lines are parallel. 

> solve :: Line -> Line -> Point
> solve (Ln a b c) (Ln d e f) | zerO ((a*e)-(b*d)) = (Pt 0 0)
> 			    | not (zerO a) 	= solveAux (Ln a b (-c)) (Ln d e (-f))
> 			    | otherwise 	= solveAux (Ln d e (-f)) (Ln a b (-c))

> solveAux :: Line -> Line -> Point
> solveAux (Ln a b c) (Ln 0 e f) = (Pt x y) 
> 					where y = f/e
> 					      x = (c-b*y)/a

> solveAux (Ln a b c) (Ln d e f) = solveAux (Ln a b c) (Ln 0 (e-b*g) (f-c*g))
>					where g = d/a	


	triangleArea computes the area of a triangle defined by three
	points.
	
> triangleArea :: [Point] -> Numb
> triangleArea [p1,p2,p3] = abs ((1/2) * (x1*y2-y1*x2))
>				where 
>				(Pt x1 y1) = minus p2
>				(Pt x2 y2) = minus p3
>				minus x = minusPoint p1 x
	
	minusPoint performs vector subtraction
	
> minusPoint :: Point -> Point -> Point
> minusPoint (Pt x y) (Pt u v) = Pt (u-x) (v-y)


	
	mkPoint: Takes a string of the form "x y" and creates
			the point (Pt x y) where x and y are 
			number conversions of the strings x and y.
			If x or y are not digits the Null point is returned.
			Note use of mouse displacement corrections.

> mkPoint :: String -> Point
> mkPoint l = if and (map isDigit (a++b)) then
>                  Pt (fromIntegral (numval a-mouseDispx))
> 				(fromIntegral (numval b-mouseDispy))
>                 else (Pt 0 0) -- Null Point
>                                         where
>                                         (a,b)= splitAt_YORK gap l


	mkPolygon: converts a list of points of the form
			[p1,p2,p3..pn] representing the vertices of 
		a polygon,
		to the B-rep form [(p1,p2),(p2,p3),..,(pn,p1)]
	 
> mkPolygon :: [Point] -> Faces 
> mkPolygon [] = []
> mkPolygon (a:l) = map2 f (a:l) (l++[a])
>			where f x y = mkFace (x,y)




	drawface: Returns the escape string required to plot the 
			face on the screen. Note that each of the
			co-ords is rounded to the nearest pixel
			position.

> drawSegment :: Segment -> String
> drawSegment ((Pt x1 y1),(Pt x2 y2)) = line (map rnd [x1,y1,x2,y2])


	inRange: tests whether the point a b lives in the box with
		origin (top left corner) at (x,y) with height h and 
		width w.

> {- UNUSED: (an illegal name, too)
> inRange :: [Integer] -> Point -> Bool
> inRange [x,y,w,h] (Pt a b) = between (fromInteger x) (fromInteger w) a
>                                         && between (fromInteger y) (fromInteger h) b
> -}
