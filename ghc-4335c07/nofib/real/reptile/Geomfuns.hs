-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Geomfuns(
mapx, mapy, col, row, lrinvert, antirotate, place, rotatecw, 
       tbinvert, tile, t4, xymax) where

import Mgrfuns
import Drawfuns

--CR strange instructions here!
-- xymax should be in layout.m, and the functions like t4 in
-- a module specific to the program that #includes "layout.t"

swapxy :: [Int] -> [Int]

--xs [x1,y1,x2,y2] = [x1,x2]
--ys [x1,y1,x2,y2] = [y1,y2]
swapxy [x1,y1,x2,y2] = [y1,x1,y2,x2]

mapx, mapy :: (Int -> Int) -> [Int] -> [Int] 

mapx f [x1,y1,x2,y2] = [f x1, y1, f x2, y2]
mapy f [x1,y1,x2,y2] = [x1, f y1, x2, f y2]

toright, down :: Int -> [[Int]] -> [[Int]]

toright = map . mapx . (+) 
down = map . mapy . (+)

origin :: Int -> Int -> [[Int]] -> [[Int]]
origin x y = (toright x) . (down y)

-- place x y takes a print and outputs a string that
-- is interpreted by MGR with the result that
-- the print is drawn at x y

place :: Int -> Int -> [[Int]] -> [Char]
place x y = drawlines . (origin x y)

-- 72 is the size of the square in the big tile
xymax :: Int
xymax = 72

-- lrinvert etc still need the size of the square in which to do it
-- so have not yet reverted to their original generality

lrinvert, tbinvert, rotatecw, antirotate :: Int -> [[Int]] -> [[Int]]

lrinvert m   = map (mapx (\x -> m-x))
tbinvert m   = map (mapy (\x -> m-x))
rotatecw m   = map (swapxy . (mapy (\x -> m-x)))
antirotate m = map (swapxy . (mapx (\x -> m-x)))

--CR this doesn't really belong here - redefinition (cf postscript)!
-- a function specifically for the potatoprinting program
-- ss is the square size
t4 :: [[[Int]]] -> [[Int]]
t4 [c1,c2,c3,c4] = c1 ++
                   toright ss c2 ++
                   down ss c3 ++
                   (down ss . toright ss) c4
                   where
                   ss = xymax

-- a tile function specifically for use with t4
--CR ditto
tile :: Int -> Int -> Int -> Int -> [[Int]] -> [Char]
tile _ _ _ 0 coords = ""
tile _ _ 0 _ coords = ""
tile x y c r coords = col x y r coords ++
                      row (x + 2*xymax) y (c-1) coords ++
              	      tile (x + 2*xymax) (y + 2*xymax) (c-1)(r-1) coords

col, row :: Int -> Int -> Int -> [[Int]] -> [Char]

col x y 0 coords = ""
col x y n coords = place x y coords ++ col x y' (n-1) coords
	           where
                   y' = y + (2 * xymax)

row x y 0 coords = ""
row x y n coords = place x y coords ++ row x' y (n-1) coords
		   where
	           x' = x + (2 * xymax)



