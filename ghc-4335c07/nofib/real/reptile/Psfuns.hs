-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Psfuns(pos8head, introline, lf, tile4, concrep) where

import Geomfuns
import Auxprogfuns

concrep :: Int -> [a] -> [a]
concrep x y = concat (take x (repeat y))

--CR headN, fN, ff and f replaced by newf - still needs cleaning up!
pos8head :: [[Int]] -> [Char]
pos8head coords =  header ++ pamcat (map newf [1 .. 8]) coords 
                   where
		   header = "%!PS-Adobe-1.0\n0.75 setlinewidth\n" ++
			    "/print0\n{\n} def\n"
		   topos [x1,y1,x2,y2] = show x1++" "++show y1++" moveto\n"++
				         show x2++" "++show y2++" lineto\n"
		   pamcat (f:fs) a = f a ++ pamcat fs a
		   pamcat [] a = []
		   fpat h f coords = h ++
                                     (concat . map topos . f) coords ++
                                     "stroke} def\n"
                   newf n = fpat ("/print" ++ show n ++ "\n{") (orient psmax n)
                   

introline, rowline, ss :: [Char]
introline = "400 400 translate"
rowline = "\n-288 36 translate"
ss = "\n36 0 translate\nprint"

sq :: Int -> [Char]
sq num = ss ++ show num

lf :: [Int] -> [Char]
lf list = rowline ++ concat (map sq list)

--CR this shouldn't be here :-)
tile4 :: [Int] -> [Char]
tile4 [n1,n2,n3,n4] = introline ++
		      concrep 4 (posrow n1 n2 ++ posrow n3 n4) ++
                      "\nshowpage\n"
		      where
		      posrow i j = rowline ++ concrep 4 (sq i ++ sq j)

-- 36 is the size of the postscript square
psmax :: Int
psmax = 36



