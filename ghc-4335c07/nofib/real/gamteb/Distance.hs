-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module Distance (distSurf) where

import GamtebType
import Consts

-- find distance of a particle to a surface

distSurf :: Point -> Point -> (Value, Indx)
distSurf (p1@(x,y,z)) (p2@(u,v,w)) =
	(dSurf+small, surf)
	where
	    (d1, s1) = ((distPlane y v 0), 1)
	    (d2, s2) = ((distCyl p1 p2), 2)
	    (d3, s3) = ((distPlane y v cylLen), 3)
	    (d4, s4) = ((distPlane y v cylLen2), 4)
	    (dSurf, surf) = minP (minP (d1,s1) (d2,s2)) (minP (d3,s3) (d4,s4))
	    minP (d, j) (d', j') =
		if (d < d')
		    then (d, j)
		    else (d', j')


-- find distance to a cylinder

distCyl :: Point -> Point -> Value
distCyl (x,y,z) (u,v,w)
	| (u*u + w*w) == 0	= big		-- w*w used to be v*v  LA
	| (u /= 0)		= 
		let
		    m = w/u
		    b = z - m*x
		    s = m*m + 1
		    r = sqrt (s - b*b)
		    x' = if (u > 0)
			    then (-m*b + r) / s
			    else (-m*b - r) / s
		in
		(x'-x) / u
	| (u == 0 && v /= 0)	= 
		let
		    m = w/v
		    b = z - m*y
		    r = sqrt (1 - x*x)
		    y' = if (v > 0)
			    then (r-b) / m
			    else (-r-b) / m
		in
		(y'-y) / v
	| w > 0			= (sqrt (1 - x*x)) - z
	| otherwise		= -(sqrt (1 - x*x)) - z


-- find distance of a particle to a plane

distPlane :: Coord -> Coord -> Value -> Value
distPlane y v yPlane 
	| v == 0	= big
	| y >= yPlane	= big
	| otherwise	= (yPlane-y) / v
