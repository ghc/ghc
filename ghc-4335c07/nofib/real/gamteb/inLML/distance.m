-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module

#include "gamtebType.t"
#include "consts.t"

export distSurf;

rec	-- find distance of a particle to a surface
	-- distSurf :: Point -> Point -> (Float, Int)

    distSurf (p1 as Pt x y z) (p2 as Pt u v w) =
	let (d1, s1) = ((distPlane y v (0.0)), 1) in
	let (d2, s2) = ((distCyl p1 p2), 2) in
	let (d3, s3) = ((distPlane y v cl), 3) in
	let (d4, s4) = ((distPlane y v cl2), 4) in
	let (dSurf, surf) = (minpair (minpair (d1, s1) (d2, s2)) 
				       (minpair (d3, s3) (d4, s4))
	    where
		minpair (d, j) (d', j') =
		    if (d < d')
			then (d, j)
			else (d', j')) in
	(dSurf+.small, surf)


and	-- find distance to a cylinder
	-- distCyl :: Point -> Point -> Float

    distCyl (Pt x y z) (Pt u v w) =
	if ((u*.u +. w*.w) = 0.0)
	    then big
	    else (distC u v
		where
		    distC u v & (u ~= 0.0) =
			let m = w/.u in
			let b = z -. m*.x in
			let s = m*.m +. 1.0 in
			let r = sqrt (s -. b*.b) in
			let x' = (if (u > 0.0)
				    then ((0.0-.m)*.b +. r) /. s
				    else ((0.0-.m)*.b -. r) /. s) in
			(x'-.x) /. u
		||  distC u v & (u = 0.0 & v ~= 0.0) =
			let m = w/.v in
			let b = z -. m*.y in
			let r = sqrt (1.0 -. x*.x) in
			let y' = (if (v > 0.0)
				    then (r-.b) /. m
				    else (0.0-.r-.b) /. m) in
			(y'-.y) /. v
		||  distC u v =
			let z' = sqrt (1.0 -. x*.x) in
			(if (w > 0.0)
			    then z'-.z
			    else 0.0-.z'-.z))


and	-- find distance of a particle to a plane
	-- distPlane :: Float -> Float -> Float -> Float

    distPlane y v yPlane & (v = 0.0) = big
||  distPlane y v yPlane =
	let d = (yPlane-.y) /. v in
	if (d <= 0.0)
	    then big
	    else d

end
