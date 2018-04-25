{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
{-# OPTIONS -fno-spec-constr-count #-}
module Vectorised1 (closest1PA, closeststupid1PA) where
import Points2D.Types
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude as P

-- removed the sqrt here - only some users actually need it
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2)
  =      ( (x2 D.- x1) D.* (x2 D.- x1)
       D.+ (y2 D.- y1) D.* (y2 D.- y1) )

-- Distance between two points, but return a very large number if they're the same...
distancex :: Point -> Point -> Double
distancex a b
 = let d = distance a b
   in  if d D.== 0 then 1e100 else d


-- An n^2 algorithm for finding closest pair.
-- Our divide and conquer drops back to this once there are few enough points
closeststupid :: [: Point :] -> (Point,Point)
closeststupid pts
 = let i = minIndexP [: distancex a b | a <- pts, b <- pts :]
   in  [: (a,b) | a <- pts, b <- pts :] !: i


-- | Find the points within distance @d@ of the edge along x=@x0@.
near_boundary
    :: [:Point:]    -- ^ array of points
    -> Double       -- ^ split / x boundary
    -> Double       -- ^ maximum distance
    -> [:Point:]
near_boundary pts x0 d
 = filterP check pts
 where
  check (x1,_) = D.abs (x1 D.- x0) D.< d


-- | Find pair with minimum distance between tops * bots
merge :: [:Point:] -> [:Point:] -> (Point,Point)
merge tops bots
 = let i = minIndexP [: distancex a b | a <- tops, b <- bots :]
   in  [: (a,b) | a <- tops, b <- bots :] !: i


-- | Given closest pairs in points above and below split,
-- we want to find the minimum of all points.
-- To do this, we find the points within a certain distance
-- from the split boundary, and check them against each other.
-- We then take the minimum of all points.
merge_pairs
    :: Double           -- ^ split / x boundary
    -> [:Point:]        -- ^ points above split
    -> [:Point:]        -- ^ points below split
    -> (Point,Point)    -- ^ closest pair in points above
    -> (Point,Point)    -- ^ closest pair in points below
    -> (Point,Point)
merge_pairs x0 top bot (a1,a2) (b1,b2)
 = let da   = distancex a1 a2
       db   = distancex b1 b2
       min2 = if da D.< db then (a1,a2) else (b1,b2)
       mind = D.min da db
       d    = sqrt mind

       topn = near_boundary top x0 d
       botn = near_boundary bot x0 d

   in  if   lengthP topn I.* lengthP botn I.== 0
       then min2
       else let 
               (m,n)= merge topn botn
               dm   = distancex m n
            in if dm D.< mind then (m,n) else min2


-- | Find closest two points in array of points.
-- Use naive n^2 algorithm when there are few points,
-- otherwise split along median X and do each half recursively.
-- And merge the result of the two halves.
closest :: [:Point:] -> (Point,Point)
closest pts
 | lengthP pts I.< 250 = closeststupid pts
 | otherwise       =
   let (xs,ys)   = unzipP pts

       xd   = maximumP xs D.- minimumP xs
       yd   = maximumP ys D.- minimumP ys

       mid  = median xs
       
       top  = filterP (\(x,_) -> x D.>= mid) pts
       -- NOTE was error here "combine2SPack" when "not (x >= mid)"
       bot  = filterP (\(x,_) -> x D.< mid) pts

       top' = closest top
       bot' = closest bot

       pair = merge_pairs mid top bot top' bot'
   in  pair


closest1PA :: PArray Point -> (Point,Point)
closest1PA ps = closest (fromPArrayP ps)

closeststupid1PA :: PArray Point -> (Point,Point)
closeststupid1PA ps = closeststupid (fromPArrayP ps)


median :: [: Double :] -> Double
median xs = median' xs (lengthP xs `I.div` 2)

median':: [: Double :] -> P.Int -> Double 
median' xs k =
  let p  = xs !: (lengthP xs `I.div` 2)
      ls = [:x | x <- xs, x D.< p:]
  in  if   k I.< (lengthP ls)
      then median' ls k
      else
        let gs  = [:x | x <- xs, x D.> p:]
            len = lengthP xs I.- lengthP gs
        in  if   k I.>= len
            then median' gs (k I.- len)
            else p

