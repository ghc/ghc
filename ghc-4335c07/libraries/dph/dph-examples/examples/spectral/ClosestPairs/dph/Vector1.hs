{-# LANGUAGE BangPatterns #-}
module Vector1 (closest1V, closeststupid1V) where
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


-- An n^2 algorithm for finding closest pair.
-- Our divide and conquer drops back to this once there are few enough points
closeststupid :: U.Vector Point -> (Point,Point)
closeststupid pts = merge pts pts

near_boundary :: U.Vector Point -> Double -> Double -> U.Vector Point
near_boundary pts x0 d
 = U.filter check pts
 where
  check (x1,_) = abs (x1 - x0) < d


merge :: U.Vector Point -> U.Vector Point -> (Point,Point)
merge tops bots
 = case U.foldl m1 Nothing tops of
    Nothing    -> error "merge empty vector!"
    Just (p,_) -> p
 where
  m1 s a = U.foldl (m2 a) s bots

  m2 a Nothing b
   = Just ((a,b),distancex a b)
  m2 a (Just (pts,d)) b
   = let d' = distancex a b
     in  if   d' < d
         then Just ((a,b),d')
         else Just (pts,  d)


merge_pairs :: Double -> U.Vector Point -> U.Vector Point -> (Point,Point) -> (Point,Point) -> (Point,Point)
merge_pairs x0 top bot (a1,a2) (b1,b2)
 = let da   = distancex a1 a2
       db   = distancex b1 b2
       min2 = if da < db then (a1,a2) else (b1,b2)
       mind = min da db
       d    = sqrt mind

       topn = near_boundary top x0 d
       botn = near_boundary bot x0 d

   in  if   U.length topn * U.length botn == 0
       then min2
       else let 
               (m,n)= merge topn botn
               dm   = distancex m n
            in if dm < mind then (m,n) else min2


closest :: U.Vector Point -> (Point,Point)
closest pts
 | U.length pts < 250 = closeststupid pts
 | otherwise       =
   let (xs,ys)   = U.unzip pts

       xd   = U.maximum xs - U.minimum xs
       yd   = U.maximum ys - U.minimum ys

       mid  = median xs
       
       (top,bot)  = U.partition (\(x,_) -> x >= mid) pts

       top' = closest top
       bot' = closest bot

   in  merge_pairs mid top bot top' bot'

flip pts
 = let (xs,ys) = U.unzip pts
   in  U.zip xs ys

flip2 ((x1,y1),(x2,y2)) = ((y1,x1),(y2,x2))


closest1V :: U.Vector Point -> (Point,Point)
closest1V = closest

closeststupid1V :: U.Vector Point -> (Point,Point)
closeststupid1V = closeststupid


median :: U.Vector Double -> Double
median xs = median' xs (U.length xs `div` 2)

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


