-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Tilefuns(
alistind, initalist, mark, unmark, sqid, sqas, btlocate, newas, 
       pam, put, ineights, tpatformat, rot, inv, turn, squas, inbox) where

import Layout
import Drawfuns
import Geomfuns

-- to get the (0,0)..(7,7) part of the state of
-- the tiling area

nextoct :: Int -> Int
nextoct n = (n + 1) `mod` 8

nop :: (Int, Int) -> (Int, Int)
nop (n1,n2) = if n2 == 7 then (nextoct n1, 0) else (n1, nextoct n2)

indlist :: (Int, Int) -> [(Int, Int)]
indlist n1n2 = n1n2 : (indlist . nop) n1n2

alistind :: [(Int,Int)]
alistind = take 64 (indlist (0,0))

initalist :: [((Int,Int),Int)]
initalist = map (\x -> (x,0)) alistind

-- the mark to show the current selection

unmark :: Int -> [Char]
unmark = undo . mark

mark :: Int -> [Char]
mark 0 = ""
mark n = rectangle [x-3, y-3, x + w + 3, y + h + 3]  --CR why the 3's?
         where
         [x,y,w,h] = picbox n

-- to find the x of the top left corner of 
-- the square in which the middle button is pressed

tlx, tly :: Int -> Int
tlx = \x -> tpxorig + (((x - tpxorig) `div` tpxygap) * tpxygap)
tly = \y -> tpyorig + (((y - tpyorig) `div` tpxygap) * tpxygap)

-- counting squares to give it an id

tlidx, tlidy :: Int -> Int
tlidx = \x -> ((x-tpxorig) `div` tpxygap)
tlidy = \y -> ((y-tpyorig) `div` tpxygap)

-- sqas -- square associated with
-- refers to tiling area
-- gives top left coordinates of the square

sqas :: Int -> Int -> [Int]
sqas x y = [tlx x, tly y]

-- sqid -- square id
-- refers to tiling area
-- gives id of the square as reflected in the state

sqid :: [Int] -> (Int,Int)
sqid [x,y] = (tlidy y, tlidx x)

-- squas returns the coordinates associated with a particular
-- tilist square.

squas :: (Int,Int) -> [Int]
squas (ln1,ln2) = [tpxorig + ln2 * tpxygap, tpyorig + ln1 * tpxygap]

-- btlocate -- locate in the big tile
-- if it's not there gives a default [0,0]

btlocate :: [Int] -> [Int]
btlocate [x,y] = if inbigtile x y then sqas x y else [0,0]

put :: [Int] -> [[Int]] -> [Char]
put [x,y] = place x y

-- for grouping tiles in rows for printing them out

ineights :: [a] -> [[a]]
ineights [] = []
ineights ns = take 8 ns : ineights (drop 8 ns)

rot :: Int -> Int
rot n = case n of
	  0 -> 0
	  4 -> 1
	  8 -> 7
	  7 -> 6
	  6 -> 5
	  5 -> 8
	  n -> n + 1

turn :: Int -> Int
turn n = if n==0 then 0 else 
                   (if n == 4 then 8 else (n + 4) `mod` 8)

-- Because of the arrangement of the 8 pictures
-- inv is effectively tbinvert in this version

inv :: Int -> Int
inv = turn --CR
--CR inv n = if n==0 then 0 else
--CR              (if n == 4 then 8 else (n + 4) `mod` 8)

-- CR removed apparently redundant x' and y' and restructured conditional
inbox :: [Int] -> Int
inbox [xp,yp] = inbox' 1
	        where
                inbox' n =
                  if n > 8 then 0 
	          else if inrect x y w h xp yp then n
                  else inbox' (n+1)
                  where
                  [x,y,w,h] = picbox n 

tpatformat :: [[Int]] -> [Char]
tpatformat [] = ""
tpatformat (ln:lns) = formline ln ++ "\n" ++ tpatformat lns
	              where
                      formline (n:ns) = if (ns /= []) then
                                          show n ++ " " ++ formline ns
				        else show n

pam :: (a -> b -> c) -> [a] -> b -> [c]
pam f xs y = map (\x -> f x y) xs --CR
--CR pam f []  _ = []
--CR pam f (x:xs) y = f x y : pam f xs y

newas :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
newas i e [] = [(i,e)]
newas i e ((g1,g2):gs) = if g1 == i then (i,e) : gs
                         else (g1,g2) : newas i e gs



