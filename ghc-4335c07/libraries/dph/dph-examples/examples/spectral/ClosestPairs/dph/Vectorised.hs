-- Vectorised "all closest pairs":
-- This was

{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
{-# OPTIONS -fno-spec-constr-count #-}
module Vectorised (closestPA, closeststupidPA) where
import Points2D.Types
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude as P

--------- dph-lifted-copy implementations
-- These functions are in -copy but not in -vseg.
-- I've tried to implement them but got a vectorisation error:
--  "Can't vectorise expression GHC.Prim.Int#"
--
bpermuteP :: [:a:] -> [:Int:] -> [:a:]
bpermuteP as is = mapP (\i -> as !: i) is

indexedP :: [:a:] -> [:(Int,a):]
indexedP xs = zipP (I.enumFromToP 0 (lengthP xs I.- 1)) xs

updateP :: [:a:] -> [:(Int,a):] -> [:a:]
updateP as is = snd' (unzipP (mapP find (indexedP as)))
 where
  find (i,a)
   | [:v:] <- filterP (\(i',_) -> i I.== i') is
   = v
   | otherwise
   = (i,a)

  snd' (_,b) = b



-- | Distance squared.
-- Because most uses of this are just finding the minimum, the sqrt is unnecessary.
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2)
  =   (x2 D.- x1) D.* (x2 D.- x1)
  D.+ (y2 D.- y1) D.* (y2 D.- y1)


-- | Distance squared.
-- Distance between two points, but return a very large number if they're the same...
distancex :: Point -> Point -> Double
distancex a b
 = let d = distance a b
   in  if d D.== 0 then 1e100 else d


-- | Naive closest pairs.
-- An n^2 algorithm for finding closest pairs.
-- Our divide and conquer drops back to this once there are few enough points
closeststupid :: [: Point :] -> [:(Point,Point):]
closeststupid pts
 = let is = [: minIndexP [: distancex a b | b <- pts :] | a <- pts :]
   in  bpermuteP [: (a,b) | a <- pts, b <- pts :] is


-- | Find the points within distance @d@ of the edge along x=@x0@.
-- Only the first element of each pair is checked.
-- Returns pairs with indices within original input array.
near_boundary
    :: [:(Point,Point):]        -- ^ Input pairs
    -> Double                   -- ^ X dimension of boundary
    -> Double                   -- ^ Maximum distance from X boundary
    -> [:(Int,(Point,Point)):]
near_boundary a x0 d
 = filterP check (indexedP a)
 where
  check (_,((x1,_),_)) = D.abs (x1 D.- x0) D.< d


-- | Given two arrays of pairs where the firsts are both near some boundary,
-- update the first array with any closer points in the second array.
new_nearest
    :: [:(Int,(Point,Point)):] -- ^ Input pairs
    -> [:(Int,(Point,Point)):] -- ^ Check each input pair against these to find any closer pairs
    -> [:(Int,(Point,Point)):]
new_nearest a b
 | lengthP b I.== 0 = a
 | otherwise
 = let bp = mapP (\(_,(pt1,_)) -> pt1) b
       is = [: minIndexP [: distance pt1 pt2 | pt2 <- bp :] | (_,(pt1,_)) <- a :]
       na = [: (k,(pt1,check pt1 pn pt2)) | (k,(pt1,pt2)) <- a, pn <- bpermuteP bp is :]
   in  na
 where
  check pt1 pn pt2
   = if   distance pt1 pn D.< distance pt1 pt2
     then pn
     else pt2

-- | Merge two arrays of pairs that have been split on the x axis.
-- To do this, we find the maximum distance between any two points,
-- then find the points in each array within that distance of the split.
-- We check each of these boundary points against the other boundary
-- to see if they are closer.
merge_pairs
    :: Double            -- ^ Split point
    -> [:(Point,Point):] -- ^ `Above' pairs
    -> [:(Point,Point):] -- ^ `Below' pairs
    -> [:(Point,Point):]
merge_pairs x0 a b
 = let d  = sqrt (D.max (maximumP (mapP dist a)) (maximumP (mapP dist b)))
       an = near_boundary a x0 d
       bn = near_boundary b x0 d
       a' = a `updateP` new_nearest an bn
       b' = b `updateP` new_nearest bn an
   in  a' +:+ b'
 where
  dist (a,b) = distance a b


-- | For each point, find its closest neighbour.
-- Once there are few enough points, we use a naive n^2 algorithm.
-- Otherwise, the median x is found and the array is split into
-- those below and those above.
-- The two halves are recursed upon and then the results merged (see @merge_pairs@).
closest :: [:Point:] -> [:(Point,Point):]
closest pts
 | lengthP pts I.< 250 = closeststupid pts
 | otherwise       =
   let (xs,ys)   = unzipP pts

       xd   = maximumP xs D.- minimumP xs
       yd   = maximumP ys D.- minimumP ys

       mid  = median xs
       
       top  = filterP (\(x,_) -> x D.>= mid) pts
       bot  = filterP (\(x,_) -> x D.<  mid) pts

       top' = closest top
       bot' = closest bot

       pair = merge_pairs mid top' bot'
   in  pair


closestPA :: PArray Point -> PArray (Point,Point)
closestPA ps = toPArrayP (closest (fromPArrayP ps))

closeststupidPA :: PArray Point -> PArray (Point,Point)
closeststupidPA ps = toPArrayP (closeststupid (fromPArrayP ps))


-- | Find median of an array, using quickselect
median :: [: Double :] -> Double
median xs = median' xs (lengthP xs `I.div` 2)

-- | Find the @k@th smallest element.
-- A pivot is selected and the array is partitioned into those smaller and larger than the pivot.
-- If the number of elements smaller than pivot is greater or equal to @k@, then the pivot must be larger than the
-- @k@th smallest element, and we recurse into the smaller elements.
-- Otherwise the @k@th element must be larger or equal to the pivot.
-- Since lesser elements are not in the greater array, we are no longer looking for the
-- @k@th smallest element, but the @k - (length xs - length gs)@th smallest.
median':: [: Double :] -> Int -> Double 
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

