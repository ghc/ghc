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
 - Test main module.
 -
 -}

module Main(main,blah) where

import Shapes
import Raster
import Quad
import Oct
import Csg
import Interval
import Types
import Vector
import Kolor
import Matrix

-- This is a test module

main = print blah

-- blah = go 5 plane  -- do a profile plot on this...
-- blah = go 5 (Geom g (RotY 0.1))
-- blah = go 7 pic
-- blah = go 7 rotxyz
blah = gone 9 pic
-- blah = gone 8 (Colour pink sphere)

go :: Int -> Csg -> [Char]
go n = (draw n) . quadoct . (octcsg n)

gone :: Int -> Csg -> [Char]
gone n = (cdraw n) . quadoct . (octcsg n)



