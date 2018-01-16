module ListDelaunay where

import Data.List hiding ( sort, union )
import qualified Data.List
import SVG

maxIndex :: Ord a => [a] -> Int
maxIndex xs = fst $ maximumBy cmp $ zip [0..length xs - 1] xs
  where
    cmp (_,x) (_,y) = compare x y

minIndex :: Ord a => [a] -> Int
minIndex xs = fst $ minimumBy cmp $ zip [0..length xs - 1] xs
  where
    cmp (_,x) (_,y) = compare x y

slice :: Int -> Int -> [a] -> [a]
slice i n xs = take n (drop i xs)

update :: [a] -> [(Int,a)] -> [a]
update xs ps = go xs 0 (sortBy cmp ps)
  where
    cmp (i,_) (j,_) = compare i j

    go xs i [] = xs
    go (x:xs) i ps@((j,y):ps')
      | i < j  = x : go xs (i+1) ps
      | i == j = go (y:xs) i ps'

bpermute :: [a] -> [Int] -> [a]
bpermute xs = map (xs!!)

type Point = (Double,Double)
type Line  = (Point,Point)

type IPoint = (Int,Point)
type ILine  = (IPoint,IPoint)

xOf :: IPoint -> Double
xOf (_,(x,_)) = x

yOf :: IPoint -> Double
yOf (_,(_,y)) = y

iOf :: IPoint -> Int
iOf (i,_) = i


distance :: IPoint -> ILine -> Double
distance (_, (xo,yo)) ((_, (x1, y1)), (_, (x2, y2)))
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)

hull :: [IPoint] -> ILine -> [Int]
hull points line@((i1,p1),(i2,p2))
  -- | length packed == 0 = [i1]
  | length packed < 2 = i1 : map fst packed
  | otherwise
  = concat [hull packed ends | ends <- [((i1,p1),pm), (pm,(i2,p2))]]
  where
    cross  = [distance p line | p <- points]
    packed = [p | (p,c) <- zip points cross, c > 0.0]
    pm     = points !! maxIndex cross

convexHull :: [IPoint] -> [Int]
-- convexHull points | length points == 0 = []
convexHull points
  = concat [hull points ends | ends <- [(minx,maxx), (maxx,minx)]]
  where
    xs = [x | (i,(x,y)) <- points]
    minx = points !! minIndex xs
    maxx = points !! maxIndex xs

lowerHull :: [IPoint] -> [Int]
-- lowerHull points | length points == 0 = []
lowerHull points
  = hull points (maxx,minx) ++ [case minx of (i,_) -> i]
  where
    xs   = [x | (i,(x,y)) <- points]
    minx = points !! minIndex xs
    maxx = points !! maxIndex xs



kthSmallest :: [Double] -> Int -> Double
kthSmallest xs i = Data.List.sort xs !! i

kthSmallestIndex :: [(Int,Double)] -> Int -> Int
kthSmallestIndex xs k
  | k >= length lts && k < n - length gts = i
  | otherwise = kthSmallestIndex ys k'
  where
    n = length xs
    (i,x) = xs !! (n `div` 2)

    lts = [(j,y) | (j,y) <- xs, y < x]
    gts = [(j,y) | (j,y) <- xs, y > x]

    (ys, k') | k < length lts = (lts, k)
             | otherwise      = (gts, k - (n - length gts))

median :: [Double] -> Double
median xs = kthSmallest xs (length xs `div` 2)

medianIndex :: [Double] -> Int
medianIndex xs = kthSmallestIndex (zip [0..] xs) (length xs `div` 2)

union :: [(Int,[Int])] -> [(Int,[Int])]
union ps
  | length ps <= 1 = ps
  | otherwise =
  let
    (pivot,_) = ps !! (length ps `div` 2)
    ls        = [(p,xs) | (p,xs) <- ps, p < pivot]
    gs        = [(p,xs) | (p,xs) <- ps, p > pivot]
    eqs       = [xs     | (p,xs) <- ps, p == pivot]

    ss        = map union [ls,gs]
  in
  (ss!!0) ++ [(pivot, concat eqs)] ++ (ss!!1)

collect :: [(Int,Int)] -> [(Int,[Int])]
{-
collect ps = parts (sortBy cmp ps)
  where
    cmp (i,_) (j,_) = compare i j

    parts [] = []
    parts ((i,k):ps) = let (qs,rs) = span ((i==) . fst) ps
                       in
                       (i, k : map snd qs) : parts rs
-}
collect ps = union [(i,[j]) | (i,j) <- ps]

sort :: [Int] -> [Int]
sort = Data.List.sort
{-
sort xs | length xs <= 1 = xs
sort xs = (ss!!0) ++ [pivot] ++ (ss!!1)
  where
    pivot = xs !! (length xs `div` 2)
    ls    = [x | x <- xs, x < pivot]
    gs    = [x | x <- xs, x > pivot]
    ss    = map sort [ls,gs]
-}


lineFromPoint :: Point -> Point
lineFromPoint (x,y) = (x/q,y/q)
  where
    q = sq x + sq y

sq :: Double -> Double
sq d = d*d

neighbours :: IPoint -> [IPoint] -> [(Int,Int)]
neighbours (i,(x0,y0)) points = [(i,j) | j <- hull, j /= i]
  where
    npts = [(j,lineFromPoint ((x-x0),(y-y0))) | (j,(x,y)) <- points]
    hull = convexHull ([(i,(0.0,0.0))] ++ npts)

nestedGet :: [IPoint] -> [[Int]] -> [[IPoint]]
nestedGet a i = [bpermute a k | k <- i]

removeDuplicates :: [(Int,Int)] -> [(Int,Int)]
removeDuplicates = nub
{-
removeDuplicates edges
  = [e | (i,e) <- iedges, and [e `neq` e' | e' <- slice 0 i edges ]]
  where
    iedges = zip (enumFromTo 0 (length edges - 1)) edges

    neq (i1,j1) (i2,j2) = i1 /= i2 || j1 /= j2
-}

delaunayFromEdgelist :: [Point] -> [(Int,Int)] -> [[(Int,Int)]]
delaunayFromEdgelist points edges
  = [neighbours p ps | (p,ps) <- zip ipoints (nestedGet ipoints adj_lists)]
  where
    edges1 = removeDuplicates [(max i j, min i j) | (i,j) <- edges]
    edges2 = edges1 ++ [(j,i) | (i,j) <- edges1]
    adj_lists = [sort js | (i,js) <- collect edges2]
    ipoints   = zip (enumFromTo 0 (length points - 1)) points

slowDelaunay :: [IPoint] -> [(Int,Int)]
slowDelaunay points | length points == 0 = []
slowDelaunay points = neighbours (points !! 0) rest ++ slowDelaunay rest
  where
    rest = slice 1 (length points - 1) points

delaunayDivide :: [IPoint] -> Int -> [[IPoint]]
delaunayDivide points prev_n
  | length points <= 4 || length points == prev_n = [points]
delaunayDivide points prev_n
  = concat [ delaunayDivide x n | x <- [ down_points, up_points ] ]
  where
    n = length points
    
    points1 = [(i,(y,x)) | (i,(x,y)) <- points]
    (i,(xm,ym)) = points1 !! medianIndex [x | (_, (x,y)) <- points1]
    -- med     = median (map xOf points1)
    -- (i,(xm,ym)) = [p | p <- points1, xOf p == med] !! 0

    proj = [(j,(y, sq (x-xm) + sq (y-ym)))
                | ((i,(x,y)),j) <- zip points1 (enumFromTo 0 (n - 1))]

    lower_hull_indices = lowerHull proj
    hull_flags = update (replicate n 0) [(i,1) | i <- lower_hull_indices]

    down_points = [p | (p,fl) <- zip points1 hull_flags, xOf p < xm || fl /= 0]
    up_points   = [p | (p,fl) <- zip points1 hull_flags, xOf p >= ym || fl /= 0]

delaunay' :: [Point] -> [(Int,Int)]
delaunay' points = concat (delaunayFromEdgelist points all_edges)
  where
    ipoints = zip (enumFromTo 0 (length points - 1)) points

    point_groups = delaunayDivide ipoints (length ipoints + 1)

    all_edges = concat [slowDelaunay group | group <- point_groups]

{-
delaunay :: PArray Point -> PArray (Int,Int)
{-# NOINLINE delaunay #-}
delaunay ps = toPArrayP (delaunay' (fromPArrayP ps))
-}

delaunayPoints' :: [Point] -> [(Point,Point)]
delaunayPoints' points = zip (bpermute points is)
                             (bpermute points js)
  where
    (is,js) = unzip (delaunay' points)

{-
delaunayPoints :: PArray Point -> PArray (Point,Point)
{-# NOINLINE delaunayPoints #-}
delaunayPoints ps = toPArrayP (delaunayPoints' (fromPArrayP ps))
-}


example :: [Point]
example = [(187.2283470049284,64.15476546210951),(155.62136380600523,108.84028008492088),(149.26053167049645,149.5598807211091),(79.4118261942465,127.50225107978828),(150.106766302773,238.44232700214994),(153.787784299575,233.58629672994178),(64.64021599338429,196.07873397263373),(158.9519012687203,181.1545990824634),(142.0960792283161,161.37135509853204),(106.43568181290043,174.18403207724552),(227.6704027469097,142.82256220439206)]

exampleSVG :: String
exampleSVG = svg example (delaunayPoints' example)

