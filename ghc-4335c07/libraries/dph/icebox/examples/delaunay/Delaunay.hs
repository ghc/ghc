{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}

module Delaunay ( delaunay, delaunayPoints ) where

import Types
import Hull
import Sort

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Int as Int
import Data.Array.Parallel.Prelude.Int ( Int )

import qualified Prelude as P

xOf :: IPoint -> Double
xOf (_,(x,_)) = x

yOf :: IPoint -> Double
yOf (_,(_,y)) = y

iOf :: IPoint -> Int
iOf (i,_) = i

lineFromPoint :: Point -> Point
lineFromPoint (x,y) = (x/q,y/q)
  where
    q = sq x + sq y

sq :: Double -> Double
sq d = d*d

neighbours :: IPoint -> [:IPoint:] -> [:(Int,Int):]
neighbours (i,(x0,y0)) points = [:(i,j) | j <- hull, j Int./= i:]
  where
    npts = [:(j,lineFromPoint ((x-x0),(y-y0))) | (j,(x,y)) <- points:]
    hull = convexHull ([:(i,(0.0,0.0)):] +:+ npts)

nestedGet :: [:IPoint:] -> [:[:Int:]:] -> [:[:IPoint:]:]
nestedGet a i = [:bpermuteP a k | k <- i:]

removeDuplicates :: [:(Int,Int):] -> [:(Int,Int):]
removeDuplicates edges
  = [:e | (i,e) <- iedges, andP [: e `neq` e' | e' <- sliceP 0 i edges :] :]
  where
    iedges = indexedP edges
    neq (i1,j1) (i2,j2) = i1 Int./= i2 || j1 Int./= j2

delaunayFromEdgelist :: [:Point:] -> [:(Int,Int):] -> [:[:(Int,Int):]:]
delaunayFromEdgelist points edges
  = [:neighbours p ps | (p,ps) <- zipP ipoints (nestedGet ipoints adj_lists):]
  where
    edges1 = removeDuplicates [:(Int.max i j, Int.min i j) | (i,j) <- edges:]
    edges2 = edges1 +:+ [:(j,i) | (i,j) <- edges1:]
    adj_lists = [:js | (i,js) <- collect edges2:]
    ipoints   = indexedP points
{-
% Given a set of points and a set of edges (pairs of indices to the points),
  this returns for each point its delaunay edges sorted clockwise.
  It assumes that all Delaunay edges are included in the input edgelist 
  but that they don't have to appear in both directions %
FUNCTION delaunay_from_edgelist(points,edges) =
let
    % orient the edges and remove duplicates %
    edges = remove_duplicates({max(i,j),min(i,j): (i,j) in edges});
    % put in the back edges %
    edges = edges ++ {j,i : (i,j) in edges};
    % create an adjacency list for each node %
    adj_lists = {e : i,e in int_collect(edges)};
    % tag the points with indices %
    pts = zip([0:#points],points);
    % for each point subselect the delaunay edges and sort clockwise %
    adj_lists = {delaunay_neighbors(pt,npts): 
		 pt in pts; npts in nested_get(pts,adj_lists)};
in adj_lists $

function slow_delaunay(pts) =
if #pts == 0 then [] (int,int)
else 
    let rest = drop(pts,1);
    in delaunay_neighbors(pts[0],rest)++slow_delaunay(rest) $
-}

slowDelaunay :: [:IPoint:] -> [:(Int,Int):]
slowDelaunay points | lengthP points Int.== 0 = [::]
slowDelaunay points = neighbours (points !: 0) rest +:+ slowDelaunay rest
  where
    rest = sliceP 1 (lengthP points Int.- 1) points

delaunayDivide :: [:IPoint:] -> Int -> [:[:IPoint:]:]
delaunayDivide points prev_n
  | lengthP points Int.<= 4 || lengthP points Int.== prev_n = [:points:]
delaunayDivide points prev_n
  = concatP [: delaunayDivide x n | x <- [: down_points, up_points :] :]
  where
    n = lengthP points

    (_, pts) = unzipP points
    
    (xm,ym) = pts !: medianIndex [:y | (x,y) <- pts:]

    proj = [:(x, sq (x-xm) + sq (y-ym)) | (x,y) <- pts:]

    lower_hull_indices = lowerHull (indexedP proj)
    hull_flags = updateP (replicateP n 0) [:(i,1) | i <- lower_hull_indices:]

    down_points = [:(i,(y,x)) | ((i,(x,y)),fl) <- zipP points hull_flags, y < ym || fl Int./= 0:]
    up_points   = [:(i,(y,x)) | ((i,(x,y)),fl) <- zipP points hull_flags, y >= ym || fl Int./= 0:]

{-
function delaunay_divide(points,previous_n) =
if (#points <= block_size) or #points == previous_n 
% terminate if either points is smaller than block_size or if no progress
  was made in the previous step %
then [points]
else let
    n = #points;

    % flip x and y coordinates -- this makes it so that we alternate between
      cutting in x and in y %
    points = {i,y,x : i,x,y in points};    

    % find x median %
    med = median({x : i,x,y in points});   
    (i,xm,ym) = {i,x,y in points | x == med}[0];

    % project points onto a parabola around median point %
    proj = {j,y,(x-xm)^2+(y-ym)^2: i,x,y in points; j in index(n)};

    % find the lower hull of this parabola and mark these points %
    lower_hull_indices = lower_hull(proj);
    hull_flags = dist(f,n)<-{i,t: i in lower_hull_indices};

    % divide points into two sets based on median and such that the hull 
      points belong to both sets %
    down_points = {i,x,y in points; fl in hull_flags | x < med or fl};
    up_points = {i,x,y in points; fl in hull_flags | x >= med or fl};

% Recurse %
in flatten({delaunay_divide(x,n) : x in [down_points,up_points]}) $

-}

delaunay' :: [:Point:] -> [:(Int,Int):]
delaunay' points = concatP (delaunayFromEdgelist points all_edges)
  where
    ipoints = indexedP points
    point_groups = delaunayDivide ipoints (lengthP ipoints Int.+ 1)

    all_edges = concatP [:slowDelaunay group | group <- point_groups:]

delaunay :: PArray Point -> PArray (Int,Int)
{-# NOINLINE delaunay #-}
delaunay ps = toPArrayP (delaunay' (fromPArrayP ps))

delaunayPoints' :: [:Point:] -> [:(Point,Point):]
delaunayPoints' points = zipP (bpermuteP points is)
                              (bpermuteP points js)
  where
    (is,js) = unzipP (delaunay' points)

delaunayPoints :: PArray Point -> PArray (Point,Point)
{-# NOINLINE delaunayPoints #-}
delaunayPoints ps = toPArrayP (delaunayPoints' (fromPArrayP ps))

{-
function delaunay(pts) =
let
    % Tag the points with an index %
    ipts = {i,p : i in index(#pts) ; p in pts};

    % Break into components using divide and conquer until
      point sets are of size block_size %
    point_groups = delaunay_divide(ipts,#ipts+1);

    % Now find delaunay edges within each block %
    all_edges = flatten({slow_delaunay(group) : group in point_groups});

    % Finally put all blocks together and remove redundant edges %
    edges = delaunay_from_edgelist(pts,all_edges);
in edges $
-}

