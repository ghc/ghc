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
 - Quad-tree to raster-format processing.
 -
 -}

module Raster(draw,cdraw) where

import Interval
import Kolor
import Quad
import Types

{-

Description of raster protocol:

Each value is a "Word", which is a two byte value, MSB, then LSB.

Flags, Dimensions, (Location, Value)*

Flags      :   F        (defined below.)
Dimensions :   XX,YY    (If not square.)
           :   S        (If square.)
Location   :   X,Y,H,W  (If not square.)
           :   X,Y,D    (If square.)
Value      :   V        (If mono.)
           :   R,G,B    (If colour.)

Flags: 0x0001 = square - expect only one dimension.
       0x0002 = colour - expect triples.

Background is defined as values unassigned at end.

-}


-- square, colour...

cdraw :: Int -> Quad -> [Char]
cdraw depth q = (wordy (3:w:(cout (0,0) w q [])) )
  where
    w :: Int
    w = 2 ^ (depth::Int)

cout :: (Int,Int) -> Int -> Quad -> [Int] -> [Int]
cout xy w       (Q_Empty  ) = \ints -> ints
cout xy w       (Q_Full a ) = \ints -> (coutlines xy w a) ints
-- cout xy@(x,y) w (Q_Sub a l) = (coutlines xy w a) . e . f . g . h
cout (x,y) w (Q_Sub a l) = e . f . g . h
  where
    (l0:ll1) = l   ; (l1:ll2) = ll1
    (l2:ll3) = ll2 ; (l3:ll4) = ll3
    e = cout (x  ,y  ) n (l0)
    f = cout (x+n,y  ) n (l1)
    g = cout (x  ,y+n) n (l2)
    h = cout (x+n,y+n) n (l3)
    n = w `div` 2

coutlines :: (Int,Int) -> Int -> Color -> [Int] -> [Int]
coutlines (x,y) l colour = \next -> x:y:l:r:g:b:next
  where
    (r,g,b) = unmkcolor colour

-- non-square, monochrome...

draw :: Int -> Quad -> [Char]
draw depth q = (wordy (0:w:w:(out (0,0) w q [])) )
  where
    w :: Int
    w = 2 ^ (depth::Int)

out :: (Int,Int) -> Int -> Quad -> [Int] -> [Int]
out xy w       (Q_Empty  ) = \ints -> ints
out xy w       (Q_Full a ) = \ints -> (outlines xy w a) ints
-- out xy@(x,y) w (Q_Sub a l) = (outlines xy w a) . e . f . g . h
out (x,y) w (Q_Sub a l) = e . f . g . h
  where
    (l0:ll1) = l   ; (l1:ll2) = ll1
    (l2:ll3) = ll2 ; (l3:ll4) = ll3
    e = out (x  ,y  ) n (l0)
    f = out (x+n,y  ) n (l1)
    g = out (x  ,y+n) n (l2)
    h = out (x+n,y+n) n (l3)
    n = w `div` 2

outlines :: (Int,Int) -> Int -> Color -> [Int] -> [Int]
outlines (x,y) l s = \n -> x:y:l:l:(shade s):n

shade :: Color -> Int
shade (RGB r g b) = round ((sqrt r)*255)

-- and (<256) (wordy x) = True

wordy :: [Int] -> [Char]
wordy  []  = []
wordy (a:bs) = (toEnum b):(toEnum c):(wordy bs)
  where
     (b,c) = a `divMod` 256

