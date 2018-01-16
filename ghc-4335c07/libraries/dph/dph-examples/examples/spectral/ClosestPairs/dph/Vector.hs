{-# LANGUAGE BangPatterns #-}
module Vector (closestV, closeststupidV) where
import Points2D.Types
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U

-- removed the sqrt here - only some users actually need it
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2)
  = ( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) )

-- Distance between two points, but return a very large number if they're the same...
distancex :: Point -> Point -> Double
distancex a b
 = let d = distance a b
   in  if d == 0 then 1e100 else d


-- An n^2 algorithm for finding closest pairs.
-- Our divide and conquer drops back to this once there are few enough points
closeststupid :: U.Vector Point -> U.Vector (Point,Point)
closeststupid pts
 = U.map m1 pts
 where
  m1 a = (a, U.minimumBy (m2 a) pts)
  m2 a b c = distancex a b `compare` distancex a c


-- | Find the points within distance @d@ of the edge along x=@x0@.
-- Only the first element of each pair is checked.
-- Returns pairs with indices within original input array.
near_boundary :: U.Vector (Point,Point) -> Double -> Double -> U.Vector (Int,(Point,Point))
near_boundary a x0 d
 = U.filter check (U.indexed a)
 where
  check (_,((x1,_),_)) = abs (x1 - x0) < d


-- | Given two arrays of pairs where the firsts are both near some boundary,
-- update the first array with any closer points in the second array.
new_nearest :: U.Vector (Int,(Point,Point)) -> U.Vector (Int,(Point,Point)) -> U.Vector (Int,(Point,Point))
new_nearest a b
 | U.length b == 0 = a
 | otherwise
 = let bp = U.map (\(_,(pt1,_)) -> pt1) b
   in  U.map (m1 bp) a
 where
  m1 bp (k,(pt1,pt2))
   = let pn = U.minimumBy (\b c -> distancex pt1 b `compare` distancex pt1 c) bp
     in  (k, (pt1, check pt1 pn pt2))
  check pt1 pn pt2
   = if   distancex pt1 pn < distancex pt1 pt2
     then pn
     else pt2

-- | Merge two arrays of pairs that have been split on the x axis.
-- To do this, we find the maximum distance between any two points,
-- then find the points in each array within that distance of the split.
-- We check each of these boundary points against the other boundary
-- to see if they are closer.
merge_pairs :: Double -> U.Vector (Point,Point) -> U.Vector (Point,Point) -> U.Vector (Point,Point)
merge_pairs x0 a b
 = let d  = sqrt (max (U.maximum (U.map dist a)) (U.maximum (U.map dist b)))
       an = near_boundary a x0 d
       bn = near_boundary b x0 d
       a' = a `U.update` new_nearest an bn
       b' = b `U.update` new_nearest bn an
   in  a' U.++ b'
 where
  dist (a,b) = distancex a b


-- | For each point, find its closest neighbour.
-- Once there are few enough points, we use a naive n^2 algorithm.
-- Otherwise, the median x is found and the array is split into
-- those below and those above.
-- The two halves are recursed upon and then the results merged (see @merge_pairs@).
closest :: U.Vector Point -> U.Vector (Point,Point)
closest pts
 | U.length pts < 250 = closeststupid pts
 | otherwise        =
   let (xs,ys)   = U.unzip pts

       xd   = U.maximum xs - U.minimum xs
       yd   = U.maximum ys - U.minimum ys

       mid  = median xs
       
       top  = U.filter (\(x,_) -> x >= mid) pts
       bot  = U.filter (\(x,_) -> x <  mid) pts

       top' = closest top
       bot' = closest bot

   in  merge_pairs mid top' bot'





closestV :: U.Vector Point -> U.Vector (Point,Point)
closestV = closest

closeststupidV :: U.Vector Point -> U.Vector (Point,Point)
closeststupidV = closeststupid


-- | Find median of an array, using quickselect
median :: U.Vector Double -> Double
median xs = median' xs (U.length xs `div` 2)

-- | Find the @k@th smallest element.
-- A pivot is selected and the array is partitioned into those smaller and larger than the pivot.
-- If the number of elements smaller than pivot is greater or equal to @k@, then the pivot must be larger than the
-- @k@th smallest element, and we recurse into the smaller elements.
-- Otherwise the @k@th element must be larger or equal to the pivot.
-- Since lesser elements are not in the greater array, we are no longer looking for the
-- @k@th smallest element, but the @k - (length xs - length gs)@th smallest.
median':: U.Vector Double -> Int -> Double 
median' xs k =
  let p  = xs U.! (U.length xs `div` 2)
      ls = U.filter (<p) xs
  in  if   k < (U.length ls)
      then median' ls k
      else
        let gs  = U.filter (>p) xs
            len = U.length xs - U.length gs
        in  if   k >= len
            then median' gs (k - len)
            else p


