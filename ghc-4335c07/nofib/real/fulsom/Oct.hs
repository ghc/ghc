{-
 -  Fulsom (The Solid Modeller, written in Haskell)
 -
 -  Copyright 1990,1991,1992,1993 Duncan Sinclair
 -
 - Permissiom to use, copy, modify, and distribute this software for any 
 - purpose and without fee is hereby granted, provided that the above
 - copyright notice and this permission notice appear in all copies, and
 - that my name not be used in advertising or publicity pertaining to this
 - software without specific, written prior permission.  I makes no
 - representations about the suitability of this software for any purpose.
 - It is provided ``as is'' without express or implied warranty.
 - 
 - Duncan Sinclair 1993.
 - 
 - Csg to Oct-tree processing.
 -
 -}

module Oct (octcsg) where

import Csg
import Interval
import Types
import Kolor
import Vector

startx = -2
endx = 2

starty = -2
endy = 2

startz = -2
endz = 2

makeoct :: Csg -> Oct
makeoct csg = octer 1 csg xyz
  where
   xyz = (x,y,z)
   x = startx # endx
   y = starty # endy
   z = startz # endz


-- octer :: Int -> Csg -> (R3 BI) -> Oct
octer nn csg xyz
    = case (calc csg white xyz) of
	(res,newc',rgb,new) -> 
         let
          newc = if new then newc' else csg
          c = light rgb (calcn newc xyz)
          (x,y,z) = xyz
          bhx = bothalf x ; thx = tophalf x
          bhy = bothalf y ; thy = tophalf y
          tbz = topbit  z ; bhz = bothalf z
	  os  = if nn == 1 then osb else osa
	  n1  = nn + 1
          osa = map (octer n1 newc)
               [ (bhx,bhy,tbz) , (bhx,bhy,bhz) ,
		 (thx,bhy,tbz) , (thx,bhy,bhz) ,
		 (bhx,thy,tbz) , (bhx,thy,bhz) ,
		 (thx,thy,tbz) , (thx,thy,bhz) ]
          osb = [(octer n1 newc (bhx,bhy,tbz)) ,
		 (octer n1 newc (bhx,bhy,bhz)) ,
		 (octer n1 newc (thx,bhy,tbz)) ,
		 (octer n1 newc (thx,bhy,bhz)) ,
		 (octer n1 newc (bhx,thy,tbz)) ,
		 (octer n1 newc (bhx,thy,bhz)) ,
		 (octer n1 newc (thx,thy,tbz)) ,
		 (octer n1 newc (thx,thy,bhz)) ]
         in
	     if res < (pt 0) then
	      O_Full c
	     else if res > (pt 0) then
	      O_Empty
	     else
	      O_Sub c os

{-
          os = map (octer newc)
               [ (bhx,bhy,tbz) , (bhx,bhy,bhz) ,
		 (thx,bhy,tbz) , (thx,bhy,bhz) ,
		 (bhx,thy,tbz) , (bhx,thy,bhz) ,
		 (thx,thy,tbz) , (thx,thy,bhz) ]
-}

calcn csg xyz = normalise (makevector f0 f1 f2 f3)
  where
    (f0,_,_,_) = calc csg black (mid1 x,mid1 y,mid2 z)
    (f1,_,_,_) = calc csg black (mid2 x,mid1 y,mid2 z)
    (f2,_,_,_) = calc csg black (mid1 x,mid2 y,mid2 z)
    (f3,_,_,_) = calc csg black (mid1 x,mid1 y, up  z)
    (x,y,z) = xyz


pruneoct :: Int -> Oct -> Oct
pruneoct 0 (O_Sub c os) = O_Full c
pruneoct n (O_Sub c os) = O_Sub c (map (pruneoct (n-1)) os)
pruneoct n o            = o

octcsg :: Int -> Csg -> Oct
octcsg depth = (pruneoct depth) . makeoct


