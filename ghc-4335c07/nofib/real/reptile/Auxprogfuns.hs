-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Auxprogfuns(
nearx,neary, deline, orient, display, cs,
  wwscale, wscale, wline, showoris) where

import Data.List ( (\\) ) -- 1.3

import Mgrfuns
import Diff
import Drawfuns
import Geomfuns
import Layout
import Rational

concmap3 :: (a -> b -> c -> [d]) -> [a] -> [b] -> [c] -> [d]
concmap3 f (x:xs) (y:ys) (z:zs) = f x y z ++ concmap3 f xs ys zs
concmap3 f _      _       _     = []

-- see the pic related definitions for where the numbers come from

--CR numeric literals here not acceptable - needs abstraction 
display :: [(a, [[Int]])] -> [Char]
display slist = concmap3 place [624,624,624,624,724,724,724,724]
			      [676,776,876,976,676,776,876,976]
				       (map snd slist)

-- These codings are used for the eight pictures,
-- for the program state,
-- and for the postscript file
-- CR replaced multiclause defn by case

orient :: Int -> Int -> [[Int]] -> [[Int]]
orient m n = case n of
             0 -> (\_ -> [[0,0,0,0]])
             1 -> (\x -> x)
             2 -> rotatecw m
             3 -> rotatecw m . rotatecw m
             4 -> antirotate m
             5 -> tbinvert m
             6 -> tbinvert m . rotatecw m
             7 -> lrinvert m
             8 -> lrinvert m . rotatecw m

pixdist :: Int
pixdist = 10

--CR removed old 'rmin2' definition - now use rmin from Rational module

between :: Int -> Int -> Int -> Bool
between n1 n2 n = (n1 <= n && n2 >= n) || (n1 >= n && n2 <= n)

--CR now uses Rational's rmin instead of old rmin2
--CR k1 redefined to avoid explicit use of norm
online :: [Int] -> Int -> Int -> Bool
online [x0,y0,x1,y1] xp yp =
	if y0 == y1 then between x0 x1 xp && abs (y0 - yp) < pixdist
	else if x0 == x1 then between y0 y1 yp && abs (x0 - xp) < pixdist
        else b2 <= a2 + c2 && c2 <= a2 + b2 && intval (rmin dx dy) < pixdist
        where
	k1 = rdiv (torat (x0 - x1)) (torat (y0 - y1))
	k0 = rsub (torat x0) (rmul k1 (torat y0))
	xp' = radd k0 (rmul k1 (torat yp))
	yp' = rdiv (rsub (torat xp)  k0) k1
	a2 = square (diff x0 x1) + square (diff y0 y1)
	b2 = square (diff x1 xp) + square (diff y1 yp)
	c2 = square (diff x0 xp) + square (diff y0 yp)
        dx = rabs (rsub (torat xp) xp')
        dy = rabs (rsub (torat yp) yp') 

--CR renamed firstline as thisline, firstcircs as thesecircs
--CR note allowance for argument order bug using \\ instead of difference
deline :: [([Int],[Int])] -> [Int] -> ([Char], [([Int],[Int])])
deline ls [px,py] =
    deline' ls
    where
    deline' [] = ("",ls)
    deline' (pl:pls) =  
      if online thisline px py then 
        (undraw thisline ++ (undo . wline) thisline ++ decircs, remove1 ls pl)
      else deline' pls
      where
      (thisline, thesecircs) = pl
      restcircs = listremove1 (concat (map snd ls)) thesecircs
      decircs = (concat . map decirc) (restcircs \\ thesecircs)

--CR remove1 xs y is xs with 1st occurrence (if any) of y removed
remove1 :: (Eq a) => [a] -> a -> [a]
remove1 (l:ls) i = if i==l then ls else l : remove1 ls i
remove1 []     i = []

--CR replaced explicit recursion with foldl application
listremove1 :: (Eq a) => [a] -> [a] -> [a]
listremove1 = foldl remove1

-- functions to do with the drawing of lines and marking of circles
-- in the design phase

-- as the x and y lists for the design area are the same, the function 
-- onedge can be defined without specifying onedgex and onedgey

onedge :: Int -> Bool
onedge n = n == dpxyorig || n == dpxyorig + (dpxynum -1) * dpxygap

-- similarly the method of finding the nearest x or y points
-- on the grid are equivalent

nearest :: Int -> Int
nearest n = if n - n1 < n2 - n then n1 else n2
            where
            n1 = dpxyorig + ((n - dpxyorig) `div` dpxygap) * dpxygap
            n2 = n1 + dpxygap

-- but the cursor is not symmetrical in its deficiencies, so we have:

nearx, neary :: Int -> Int
nearx x = nearest (x - 4)
neary y = nearest (y - 5)

-- numassoc is to give points on the edge an associated number

numassoc :: Int -> Int
numassoc n = if n1 <= 9 then n1 else 18 - n1
             where
             n1 = (n - dpxyorig) `div` dpxygap

-- circ6 for drawing the little circles

circ6 :: Int -> Int -> [Char]
circ6 x y = circle [x,y,6]

-- circsym for identifying symmetrically placed dots and
-- drawing circles round them. It assumes that the x and y
-- have been adjusted to allow for the dicky cursor.

circsym :: Int -> Int -> ([Char], [Int]) 
circsym xn yn = if onedge xn then (symcircs yn,[numassoc yn])
                else if onedge yn then (symcircs xn,[numassoc xn])
                else ("",[])

--CR explanation of numeric literals?
sympat :: Int -> [Int]
sympat n = [n, 400-n, 380, 380, 400-n, n, 20, 20]

symcircs :: Int -> [Char]
symcircs n = concat (zipWith circ6 (sympat n) (reverse (sympat n)))

-- assumes the coordinates have already been corrected to allow
-- for the deficiencies of the cursor, and to fit into the grid
cs :: [Int] -> ([Char], [Int])
cs [x0,y0,x1,y1] = 
	(line [x0,y0,x1,y1] ++ circles0 ++ circles1, ids0++ids1)
	where  
	(circles0,ids0) = circsym x0 y0
	(circles1,ids1) = circsym x1 y1

decirc :: Int -> [Char]
decirc n  = (undo . symcircs) (n * dpxygap + dpxyorig)

-- wscale for the lines in the wee square
wscale :: Int -> Int
wscale n = (n - dpxyorig) `div` 5

-- wwscale for the lines in postscript
wwscale :: Int -> Int
wwscale n = (n - dpxyorig) `div` 10

wline :: [Int] -> [Char]
wline = line .
        mapx (\x -> x + picxorig) .
        mapy (\y -> y + picyorig) .
        map wscale

showoris :: [[Int]] -> Int -> [Char]
showoris coords n = place x y (((orient xymax) n . map (map wscale)) coords)
                    where
                    [x,y,w,h] = picbox n



