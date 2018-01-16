Time-stamp: <Wed May 22 1996 19:50:29 Stardate: [-31]7543.92 hwloidl>

Computing the convex  hull  of a  given set  of  points using   a bisecting
divide-and-conquer approach (similar to  quicksort in its structure). Based
on the NESL code presented in:
   Programming Parallel Algorithms
   by Guy E. Blelloch 
   in CACM 39(3), March 1996
   URL: http://www.cs.cmu.edu/afs/cs.cmu.edu/project/scandal/public/www/nesl/alg-geometry.html

As data types we only need points and lines.

\begin{code}
#if defined(GRAN)
import Strategies
#endif
#if defined(ARGS)
import GranRandom
import LibSystem       -- for getArgs
#endif

infixl 5 *.         -- infix function for cross-product

type Coor = Double             -- type of coordinates that are used
type Point = (Coor,Coor)
type Line  = (Point,Point)
\end{code}

These function implement the  NESL functions with the same  name. If I grab
more NESL  programs it would  be a Good  Idea to make a  NESL module out of
this part.  For this algorithm, however, these functions are not needed any
more (they would be quite inefficient in our model of computation anyway).

index x []     = error "index: element not in list"
index x (x:xs) = 1
index x (_:xs) = 1 + index x xs

min_index xs  = index (minimum xs) xs
max_index xs  = index (maximum xs) xs

plusp = (>0)

\begin{code}
get_maximum :: Ord a => (a->a->Bool) -> [a] -> a
get_maximum _ [] = error "get_maximum: []"
get_maximum gt (x:xs) = get_maximum' x xs
                       where get_maximum' max [] = max
                             get_maximum' max (x:xs)
                                | x `gt` max = get_maximum' x xs
                                | otherwise  = get_maximum' max xs
\end{code}

@cross_product@ is used to find the distance of a point (o) from a line (line).

\begin{code}
cross_product :: Point -> Line -> Coor
cross_product pt@(xo,yo) line@((x1,y1),(x2,y2))  = 
	      (x1-xo)*(y2-yo) - (y1-yo)*(x2-xo)

(*.) = cross_product
\end{code}

Given two points on the convex hull (p1 and p2), hsplit finds
all the points on the hull between p1 and p2 (clockwise), 
inclusive of p1 but not of p2.

\begin{code}
hsplit :: [Point] -> Point -> Point -> [Point]
hsplit points p1 p2 =
  let pts_cross = filter (\ (p, c) -> c>0) [ (p, p *. (p1,p2)) | p <- points ]
      packed = map fst pts_cross
  in if (length packed < 2) then p1:packed
     else
       let pm = fst (get_maximum (\ (p, c) (p', c') -> c>c') pts_cross)
           left_res  = hsplit packed p1 pm 
           right_res = hsplit packed pm p2
           result = left_res ++ right_res 
#if defined(GRAN)
           strategy x = rnf pm `par`
	                parList rnf pts_cross `par`
                        parList rnf left_res  `par`
                        parList rnf right_res `par`
                        rwhnf x	
       in
       strategy $ result
#else
       in
       result	
#endif
\end{code}

The main function:

\begin{code}
quick_hull :: [Point] -> [Point]
quick_hull points =
  let x_coors = [ x | (x,y) <- points ]
      minx = get_maximum (\ (x,y) (x',y') -> x<x') points
      maxx = get_maximum (\ (x,y) (x',y') -> x>x') points
      left_res  = (hsplit points minx maxx)
      right_res = (hsplit points maxx minx)      
#if defined(GRAN) 
      -- _parGlobal_ 21# 21# 0# 0# left_res $
      -- _parGlobal_ 22# 22# 0# 0# right_res $ 
      strategy x = rnf minx `par`
                   rnf maxx `par`
		   rnf left_res `par`
                   rnf right_res `par`
                   x	   
  in
  strategy $ (left_res ++ right_res)
#else
  in
  (left_res ++ right_res) 
#endif

\end{code}

Test data:

\begin{code}
points0 :: [Point]
points0 = [(3.0,3.0),(2.0,7.0),(0.0,0.0),(8.0,5.0),
          (4.0,6.0),(5.0,3.0),(9.0,6.0),(10.0,0.0)]

-- 100 pts with both x and y coor between 0 and 500
points1 :: [Point]
points1 = [ (498.700012, 302.399994), (188.399994, 232.399994), (382.200012, 492.200012), (145.899994, 469.799988), (312.000000, 449.899994), (296.899994, 46.299999), (97.699997, 381.899994), (38.700001, 78.500000), (304.899994, 348.000000), (109.800003, 136.000000), (430.299988, 102.500000), (154.699997, 260.899994), (392.799988, 344.500000), (137.199997, 108.800003), (245.600006, 272.100006), (54.799999, 404.200012), (44.900002, 470.399994), (429.600006, 152.000000), (82.099998, 442.000000), (149.399994, 231.899994), (309.700012, 263.500000), (475.100006, 28.500000), (462.399994, 15.800000), (303.000000, 304.600006), (324.799988, 165.899994), (477.600006, 435.600006), (374.200012, 56.200001), (251.100006, 190.100006), (20.900000, 385.200012), (131.100006, 260.600006), (281.299988, 371.899994), (459.500000, 168.600006), (423.100006, 99.800003), (91.699997, 384.500000), (85.599998, 366.100006), (7.300000, 27.500000), (240.800003, 480.299988), (462.000000, 438.100006), (182.399994, 41.599998), (247.000000, 113.699997), (54.700001, 119.800003), (39.299999, 130.699997), (11.400000, 249.899994), (403.700012, 137.699997), (107.500000, 150.000000), (263.399994, 418.500000), (67.699997, 102.199997), (298.500000, 309.000000), (141.100006, 56.599998), (284.000000, 415.899994), (346.600006, 132.100006), (424.000000, 177.600006), (75.300003, 211.699997), (160.399994, 306.700012), (320.399994, 369.399994), (177.500000, 340.799988), (358.899994, 36.400002), (26.600000, 177.800003), (422.399994, 493.000000), (245.800003, 473.700012), (425.600006, 17.000000), (159.399994, 377.200012), (159.500000, 157.899994), (185.800003, 336.299988), (215.899994, 486.000000), (3.600000, 425.100006), (19.200001, 69.099998), (137.399994, 318.700012), (238.399994, 23.000000), (422.500000, 361.100006), (271.299988, 448.299988), (246.899994, 456.500000), (98.599998, 320.299988), (313.500000, 412.299988), (236.300003, 336.799988), (379.200012, 88.000000), (413.399994, 328.799988), (411.500000, 83.500000), (396.399994, 41.799999), (433.799988, 316.500000), (408.899994, 418.100006), (248.500000, 90.000000), (310.200012, 396.700012), (441.700012, 102.400002), (171.300003, 177.899994), (269.700012, 338.600006), (386.700012, 312.399994), (384.399994, 359.700012), (149.399994, 355.799988), (302.700012, 130.800003), (152.000000, 234.800003), (294.799988, 349.899994), (117.599998, 160.300003), (354.299988, 229.000000), (106.800003, 242.199997), (70.099998, 248.399994), (254.100006, 241.899994), (107.199997, 84.599998), (31.200001, 12.200000), (43.599998, 229.800003) ]

\end{code}

The main function:

\begin{code}
args_to_IntList a = if length a < 2
		      then error "Usage: qh <lst-length> <max-elem>\n"
		      else map (\ a1 -> fst ((readDec a1) !! 0)) a

#if defined(ARGS)
munch_args = 	getArgs >>= \ a ->
                return (args_to_IntList a) >>= \[n,m] ->
                getRandomDoubles (fromIntegral m) >>= \ random_list -> 
	        let 
                  (l1, random_list') = splitAt n random_list
                  (l2, random_list'') = splitAt n random_list'
                  x = zip l1 l2
		in
		return (x)
#else
munch_args = return (points)
#endif

#ifdef PRINT
main = munch_args >>= \ x -> print (quick_hull x)
#else
main = munch_args >>= \ x -> (rnf $ quick_hull x) `seq` putStr "Done\n"
#endif
\end{code}

----------------------------------------------------------------------

This is the original NESL code for this algorithm:

% Used to find the distance of a point (o) from a line (line). %
function cross_product(o,line) =
let (xo,yo) = o; 
    ((x1,y1),(x2,y2)) = line;
in (x1-xo)*(y2-yo) - (y1-yo)*(x2-xo);

% Given two points on the convex hull (p1 and p2), hsplit finds
  all the points on the hull between p1 and p2 (clockwise), 
  inclusive of p1 but not of p2. %
function hsplit(points,(p1,p2)) =
let cross = {cross_product(p,(p1,p2)): p in points};
    packed = {p in points; c in cross | plusp(c)};
in if (#packed < 2) then [p1] ++ packed
   else
     let pm = points[max_index(cross)];
     in flatten({hsplit(packed,ends): ends in [(p1,pm),(pm,p2)]});

function quick_hull(points) =
let x = {x : (x,y) in points};
    minx = points[min_index(x)];
    maxx = points[max_index(x)];
in hsplit(points,minx,maxx) ++ hsplit(points,maxx,minx);

points = [(3.,3.),(2.,7.),(0.,0.),(8.,5.),
          (4.,6.),(5.,3.),(9.,6.),(10.,0.)];

quick_hull(points);

