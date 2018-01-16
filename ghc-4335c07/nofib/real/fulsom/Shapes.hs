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
 - Library of CSG shapes.
 -
 -}

module Shapes where

import Interval
import Kolor
import Types

cube = Object (Cube 0 0 0 1.2)

sphere = Object (Sphere 0 0 0 1.2)

x = Object X
y = Object Y
z = Object Z

x' = Comp (Object X)
y' = Comp (Object Y)
z' = Comp (Object Z)

rot1   = Geom cube  (RotX  0.1)
rot2   = Geom cube  (RotX  0.2)
rot3   = Geom cube  (RotX  0.3)

rotz   = Geom cube  (RotZ  0.3)
roty   = Geom cube  (RotY  0.3)
rotx   = Geom cube  (RotX  0.3)
rotxy  = Geom rotx  (RotY  0.3)
rotxyz = Geom rotxy (RotZ  0.3)

ex1 = cube `Sub` sphere
ex2 = cube `Union` sphere

plane = Object (Plane 0.0 0.0 1 0)

complex = Geom ex1 (Trans  0 0 (-1.5))

st1 = plane `Union` complex
st2 = complex


bin = Inter x z
cin = Inter (Comp x) z

a = Geom x' (Trans   0.8  0 0)
b = Geom x  (Trans (-0.8) 0 0)
c = a `Inter` b
d = a `Union` b
g = c `Inter` z
h = d `Inter` z

bigs = Object (Sphere 0 0 0 1.2)
smls = Object (Sphere 0 0.3 0.3 1.0)
bite = bigs `Sub` smls

obja = Colour honeydew ( Object (Sphere (-0.5) (-1) 1 0.8) )
objb = Colour violet   ( Object (Sphere 0.5 (-1) 0 0.6) )
objc = Colour green_yellow ( Object (Sphere (-0.2) 0.5 (-0.8) 1.2) )
pic = foldr1 Union [obja,objb,objc]

pic2 =
  Union  (Object (Sphere 0.0 1.0 0.0 1.0))
         (Sub    (Object (Cube 0.0 0.0 0.0 3.0))
                 (Union (Object (Sphere 0.0 7.0   3.0  5.0))
                        (Object (Sphere 0.0 7.0 (-3.0) 5.0))))

