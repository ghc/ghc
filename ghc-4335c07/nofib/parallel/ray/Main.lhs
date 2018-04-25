The Ray tracer algorithm taken from Paul Kelly's book, adapted by Greg
Michaelson for SML, converted to (parallel) Haskell by Kevin Hammond!

> {-# LANGUAGE BangPatterns,CPP #-}
> import Control.Parallel
> import Control.Parallel.Strategies (Strategy, withStrategy, rseq, parBuffer)
> import System.Environment

> main = do
>   [detail] <- fmap (map read) getArgs
>   putStr (top detail 10.0 7.0 6.0 sc)

> type Coord = (Double,Double,Double)

> type Vector = (Double,Double,Double)

> type Ray = (Coord,Coord)

Objects: just polygons at present.  Polygon(i,N,Vs) is a polygon with
identifier i, with normal vector N and vertices Vs.

> data Object = Polygon Int Vector [Coord]

> data Impact = Impact Double Int
>	    | NoImpact

Polygon inside-outside testing.

The function in_poly_test tests whether a point lies inside
or outside a polygon (it is assumed the point is in the plane of the polygon).
The test is performed in two parts:
1. A quick range test excludes points far outside the polygon
2. An accurate test for points passing the range test
The range test checks that the x, y and z components of the point
lie within the range of x, y and z values in the polygon vertices.
The accurate test calculates the cross product of the vector from
the point to each vertex with the polygon edge at that vertex.
This cross product yields a vector at right angles to
the polygon.
If the point is inside the polygon then all these right-angle vectors
point the same way, say 'up' from the polygon.
If the point is outside the polygon then some will point up and some down.

> jot :: Double
> jot = 1.0e-8 		-- small value used to prevent real rounding errors

in_poly_range (p,q,r) Vs
   where Vs is a list of the vertices of a Polygon,
   tests whether the point (p,q,r) lies within the 'range' of the polygon.
   This means p must be less than the largest x component within Vs
   and greater than the smallest.  Similarly for q and r. 
   The fn returns a 6-tuple of bools: (xbig,xsmall,ybig,ysmall,zbig,zsmall)
   where xbig is true if p is greater than all polygon x components
         xsmall is true if p is smaller than all polygon x components 
         etc 

> in_poly_range :: Coord -> [Vector] -> (Bool,Bool,Bool,Bool,Bool,Bool)
> in_poly_range (p,q,r) [] = (True,True,True,True,True,True)
> in_poly_range (p,q,r) ((u,v,w):vs) = 
>	        (xbig && p>u+jot,xsmall && p<u-jot,
>		 ybig && q>v+jot,ysmall && q<v-jot,
>		 zbig && r>w+jot,zsmall && r<w-jot)
>    where (xbig,xsmall,ybig,ysmall,zbig,zsmall) = in_poly_range (p,q,r) vs


 cross_dot_sign (a,b,c) (d,e,f) (A,B,C)
   returns ~1 or 1 according to the sign of the dot product of
   (P,Q,R) & (A,B,C) where (P,Q,R) is the cross product of (a,b,c) & (d,e,f) 
   (A,B,C) is the normal to a polygon so this test returns 1 if
   the cross product point 'up' from the polygon and ~1 if it points 'down'

> cross_dot_sign :: Vector -> Vector -> Vector -> Int
> cross_dot_sign (a,b,c) (d,e,f) (a',b',c') =
>	if cd<0 then -1 else 1
> 	where	p = b*f-e*c
>		q = d*c-a*f
>		r = a*e-d*b
>           	cd = p*a'+q*b'+r*c'


really_in_poly (p,q,r) (A,B,C) Vs 
   tests is point (p,q,r) is inside the polygon with normal (A,B,C)
   and vertices Vs
   test that cross_dot_sign returns the same sign for all edges *)

> really_in_poly :: Vector -> Vector -> [Coord] -> (Bool,Int)
> really_in_poly (p,q,r) (a,b,c) [(x1,y1,z1),(x2,y2,z2)] =
>	(True,cross_dot_sign (x2-p,y2-q,z2-r) (x2-x1,y2-y1,z2-z1) (a,b,c))
> really_in_poly (p,q,r) (a,b,c) ((x1,y1,z1):(x2,y2,z2):vs) 
>	| in_poly =   if s1 == s then (True,s1) else (False,0)
>	| otherwise = is
>	where is@(in_poly,s) = really_in_poly (p,q,r) (a,b,c) ((x2,y2,z2):vs)
> 	      s1 =             cross_dot_sign (x2-p,y2-q,z2-r) (x2-x1,y2-y1,z2-z1) (a,b,c)

in_poly_test (p,q,r) (A,B,C) Vs
   tests if point (p,q,r) is inside the polygon with vertices Vs & normal
   vector (A,B,C)
   first test if p,q,r are inside 'range' of polygon vertices
   if passes this test, do accurate test

> in_poly_test (p,q,r) (a,b,c) vs = if b1 || b2 || b3 || b4 || b5 || b6 then False else in_poly
>	where (b1,b2,b3,b4,b5,b6) = in_poly_range (p,q,r) vs
>	      (in_poly,s) = 	    really_in_poly (p,q,r) (a,b,c) vs

(*** following functions are after Kelly's ray-tracing example in
  'Functional Programming for Loosely Coupled Multiprocessors' ***)

(* return impact for ray and polygon:
   calculate point (p,q,r) where ray intersects place of poygon
   then test if point lies inside or outside polygon:
   If inside, return IMPACT(distance,i) giving distance along ray
   and polygon id, otherwise return NOIMPACT *)

> testForImpact :: Ray -> Object -> Impact
> testForImpact ((u,v,w),(l,m,n)) (Polygon i (a,b,c) vs@((px,py,pz):_))
>	| in_poly_test (p,q,r) (a,b,c) vs = Impact distance i
>	| otherwise = 			    NoImpact
>	    where distance = (a*(px-u)+b*(py-v)+c*(pz-w))/(a*l+b*m+c*n)
>		  p = u+distance*l
>		  q = v+distance*m
>		  r = w+distance*n


(* return impact with small distance *)

> earlier :: Impact -> 	   Impact -> 	      Impact
> earlier NoImpact         NoImpact =         NoImpact
> earlier i1               NoImpact = 	      i1
> earlier NoImpact         i2 = 	      i2
> earlier i1@(Impact d1 _) i2@(Impact d2 _) = if d1 <= d2 then i1 else i2

> insert :: (Impact -> Impact -> Impact) -> Impact -> [Impact] -> Impact
> insert f d [] = d
> insert f d (x:xs) = f x (insert f d xs)

> firstImpact :: [Object] -> Ray -> Impact
> firstImpact os r =  earliest (map (testForImpact r) os)
> 	where earliest = insert earlier NoImpact

> findImpacts :: [Ray] -> [Object] -> [Impact]
> findImpacts rays objects  = parallel $      
>                               map (firstImpact objects) rays
>  where
#ifdef STRATEGIES_2
>    parallel = parBuffer 200 rwhnf
#else
>    parallel = withStrategy (parBuffer 200 rseq)
#endif


(*** Functions to generate a list of rays ******
     GenerateRays Detail X Y Z
     generates a list of Detail*Detail rays emanating from the point X, Y, Z.
     The rays are formed by projecting from the view point (X,Y,Z)
     through a grid of points (of side Detail) held at a distance of 4 units
     from the viewpoint.
     The grid is positioned so that rays in the centre of the view
     are directed toward the origin. *)


> root :: Double -> Double -> Double
> root 0 _ = 0
> root x r | abs ((r*r-x)/x) < 0.0000001 = r
>	   | otherwise =                   root x ((r+x/r)/2.0)

> vadd :: Coord -> Vector -> Vector
> vadd (a,b,c) (d,e,f) = (a+d,b+e,c+f)

> vmult :: Double -> Vector -> Vector
> vmult n      (u,v,w) = (n*u,n*v,n*w)

> ray_points :: (Int,Int) -> Int -> Coord -> Vector -> Vector -> [Vector]
> ray_points (i,j) detail (p,q,r) vx vy | j == detail = []
>				        | i == detail = ray_points (0,j+1) detail (p,q,r) vx vy
>					| otherwise = ps : ray_points (i+1,j) detail (p,q,r) vx vy
>	where ivx = vmult (fromIntegral i/fromIntegral (detail-1)) vx
> 	      jvy = vmult (fromIntegral j/fromIntegral (detail-1)) vy
>	      ps =  vadd (vadd (p,q,r) ivx) jvy 

> generateRays det x y z =
> 	map (\ (x',y',z') -> ((x,y,z),(x'-x,y'-y,z'-z))) rps
>	where
>	        d = root (x*x+y*y+z*z) 1.0
>	        (vza,vzb,vzc) = ((-4.0)*x/d,(-4.0)*y/d,(-4.0)*z/d)
>	        ab = root (vza*vza + vzb*vzb) 1.0
>	        (vxa,vxb,vxc) = (vzb/ab,(-vza)/ab,0)
>		(ya,yb,yc) = (vzb*vxc-vxb*vzc,vxa*vzc-vza*vxc,vza*vxb-vxa*vzb)
>             	ysize = root (ya*ya+yb*yb+yc*yc) 1.0
> 	    	(vya,vyb,vyc)  = (ya/ysize,yb/ysize,yc/ysize)
>		((vxa',vxb',vxc'),(vya',vyb',vyc')) | vyc > 0 =    ((-vxa,-vxb,-vxc),(-vya,-vyb,-vyc))
>						    | otherwise =  ((vxa,vxb,vxc),(vya,vyb,vyc))
>	    	(p,q,r) = (x+vza-(vxa'+vya')/2.0,
> 			   y+vzb-(vxb'+vyb')/2.0,
>			   z+vzc-(vxc'+vyc')/2.0)
>	        rps = ray_points (0,0) det (p,q,r) (vxa',vxb',vxc') (vya',vyb',vyc')

> show_imps :: Int -> Int -> [Impact] -> String
> show_imps dv i [] = ""
> show_imps dv 0 imps = '\n' : show_imps dv dv imps
> show_imps dv i (imp:imps) = simp imp ++ ' ' :  show_imps dv (i-1) imps
> 	where simp NoImpact = "."
> 	      simp (Impact _ p) = show p

(*** top level function ***)

> top :: Int -> Double -> Double -> Double -> [Object] -> String
> top detail viewx viewy viewz scene =
>	show_imps detail detail imps
>	where rays = generateRays detail viewx viewy viewz
>	      imps = findImpacts rays scene


(*** Example scene : consists of 6 squares, arranged something like:

               z
               |
	    /\ |
	   /  \| /\
	  |\ 5/|/  \
          | \/ |\ 2/|
          |6|4 | \/ |
          \ | / \3|1/
           \|/   \|/
            /     \
           /       \
         y          x

> 
> sc = [
>       Polygon 1 (1.0,0.0,0.0) [(1.0,0.0,-0.5), (1.0,0.0,0.5),   (1.0,-1.0,0.5), (1.0,-1.0,-0.5), (1.0,0.0,-0.5)],
>       Polygon 2 (0.0,0.0,1.0) [(1.0,0.0,0.5),  (0.0,0.0,0.5),   (0.0,-1.0,0.5), (1.0,-1.0,0.5),  (1.0,0.0,0.5)],
> 	Polygon 3 (0.0,1.0,0.0) [(0.0,0.0,-0.5), (0.0,0.0,0.5),   (1.0,0.0,0.5),  (1.0,0.0,-0.5),  (0.0,0.0,-0.5)],
> 	Polygon 4 (1.0,0.0,0.0) [(0.0,0.0,-0.5), (0.0,1.3,-0.5),  (0.0,1.3,0.8),  (0.0,0.0,0.8),   (0.0,0.0,-0.5)],
> 	Polygon 5 (0.0,0.0,1.0) [(0.0,0.0,0.8),  (0.0,1.3,0.8),   (-1.3,1.3,0.8), (-1.3,0.0,0.8),  (0.0,0.0,0.8)],
> 	Polygon 6 (0.0,1.0,0.0) [(0.0,1.3,-0.5), (-1.3,1.3,-0.5), (-1.3,1.3,0.8), (0.0,1.3,0.8),   (0.0,1.3,-0.5)]
>      ]
> {-
> sc = [
>       Polygon 1 (1.0,0.0,0.0) [(1.0,0.0,-0.5), (1.0,0.0,0.5),   (1.0,-1.0,0.5), (1.0,-1.0,-0.5), (1.0,0.0,-0.5)],
>       Polygon 2 (0.0,0.0,1.0) [(1.0,0.0,0.5),  (0.0,0.0,0.5),   (0.0,-1.0,0.5), (1.0,-1.0,0.5),  (1.0,0.0,0.5)],
> 	Polygon 3 (0.0,1.0,0.0) [(0.0,0.0,-0.5), (0.0,0.0,0.5),   (1.0,0.0,0.5),  (1.0,0.0,-0.5),  (0.0,0.0,-0.5)],

> 	Polygon 4 (1.0,0.0,0.0) [(0.0,0.0,-0.5), (0.0,1.3,-0.5),  (0.0,1.3,0.8),  (0.0,0.0,0.8),   (0.0,0.0,-0.5)],
> 	Polygon 5 (0.0,0.0,1.0) [(0.0,0.0,0.8),  (0.0,1.3,0.8),   (-1.3,1.3,0.8), (-1.3,0.0,0.8),  (0.0,0.0,0.8)],
> 	Polygon 6 (0.0,1.0,0.0) [(0.0,1.3,-0.5), (-1.3,1.3,-0.5), (-1.3,1.3,0.8), (0.0,1.3,0.8),   (0.0,1.3,-0.5)],

> 	Polygon 7 (1.0,0.0,0.0) [(1.7,0.6,-0.5), (1.7,0.6,0.5),   (1.7,-0.4,0.5), (1.7,-0.4,-0.5), (1.7,0.6,-0.5)],
>       Polygon 8 (0.0,0.0,1.0) [(1.7,0.6,0.5),  (0.7,0.6,0.5),   (0.7,-0.4,0.5), (1.7,-0.4,0.5),  (1.7,0.6,0.5)],
> 	Polygon 9 (0.0,1.0,0.0) [(0.7,0.6,-0.5), (0.7,0.6,0.5),   (1.7,0.6,0.5),  (1.7,0.6,-0.5),  (0.7,0.6,-0.5)],

> 	Polygon 10 (1.0,0.0,0.0) [(-0.2,0.5,0.2), (-0.2,1.4,0.2), (-0.2,1.4,1.2), (-0.2,0.5,1.2),  (-0.2,0.5,0.2)],
> 	Polygon 11 (0.0,0.0,1.0) [(-0.2,0.5,1.2), (-0.2,1.4,1.2), (-1.5,1.4,1.2), (-1.5,0.5,1.2),  (-0.2,0.5,1.2)],
> 	Polygon 12 (0.0,1.0,0.0) [(-0.2,1.4,0.2), (-1.5,1.4,0.2), (-1.5,1.4,1.2), (-0.2,1.4,1.2),  (-0.2,1.4,0.2)],

>	-- A pyramid with four surfaces, each angled at 45 degrees
>	Polygon 13 (0,0,1) [(0.0,0.0,1.0), (0.0,1.0,1.0), (1.0,1.0,1.0), (1.0,0.0,1.0)],
>	Polygon 14 (0,1,1) [(0.0,0.0,1.0), (0.5,0.5,2.0), (1.0,0.0,1.0), (0.0,0.0,1.0)],
>	Polygon 15 (1,0,1) [(0.0,0.0,1.0), (0.0,1.0,1.0), (0.5,0.5,2.0), (0.0,0.0,1.0)],
>	Polygon 16 (0,1,1) [(0.0,1.0,1.0), (1.0,1.0,1.0), (0.5,0.5,2.0), (0.0,1.0,1.0)],
>	Polygon 17 (1,0,1) [(1.0,1.0,1.0), (0.5,0.5,2.0), (1.0,0.0,1.0), (1.0,1.0,1.0)]

>      ]
> -}
