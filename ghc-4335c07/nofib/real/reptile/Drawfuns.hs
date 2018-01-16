-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Drawfuns(
drawdot, grid, squ, circ, gowin, rectangle,
fillrect, undo, undraw, drawlines) where

import Mgrfuns
import Diff

gowin :: Int -> [Char]
gowin n = selectwin n ++ setmode 7 ++ setmode 8

rectangle :: [Int] -> [Char]
rectangle [x1,y1,x2,y2] = line [x1,y1,x2,y1] ++
                          line [x2,y1,x2,y2] ++
			  line [x1,y1,x1,y2] ++
			  line [x1,y2,x2,y2]

fillrect :: [Int] -> [Char]
fillrect [x0,y0,x1,y1] = shade (diff x0 x1)
                         where
                         m = min x0 x1
			 vline n = line [n,y0,n,y1]
			 shade 0 = vline m
		         shade n = vline (m+n) ++ shade (n-1)

squ :: Int -> Int -> Int -> [Char]
squ n x y = rectangle [x, y, x+n, y+n]

circ :: Int -> Int -> Int -> [Char]
circ n x y = circle [x,y,n]

drawdot :: Int -> Int -> [Char]
drawdot x y = fillrect [x-1, y-1, x+1, y+1]

undo :: [Char] -> [Char]
undo f = func 0 ++ f ++ func 15

undraw :: [Int] -> [Char]
undraw = undo . line 

drawlines :: [[Int]] -> [Char]
drawlines = concat . map line

allpairs _ [] _ = []
allpairs _ _ [] = []
allpairs f (x:xs) ys = map (f x) ys ++ allpairs f xs ys

-- grid -- a function that draws a grid. 
-- The function drawf is applied to each x y pair in the grid

grid :: Int -> Int -> Int -> Int -> Int -> Int -> (Int -> Int -> [a]) -> [a]
grid xor yor xgap ygap xlength ylength drawf = 
	concat (allpairs drawf x0list y0list)
        where
	x0list = gridlist xor xgap xlength
	y0list = gridlist yor ygap ylength
	gridlist orig gap len =
		take len (gridlist' orig)
                where
		gridlist' n = n : gridlist' (n + gap)



