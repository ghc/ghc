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
 - CSG evaluation engine.
 -
 -}

module Csg(calc) where

import Matrix
import Types
import Interval

-- no is returned when there is "no" change to the csg.
no = error ("Evaluated dead csg.")

calc :: Csg -> Calc

calc (Func f) rgb xyz = f rgb xyz

calc (Matrix a mat) rgb xyz
    = (ans,newc,newr,prune)
     where
      newc         = if prune then (if b then newc' else newc'') else (no)
      (newc',b)    = if prune then (reduceM newc'' mat) else (no)
      xyz'         = mat4x1 mat xyz
      (ans,newc'',newr,prune) = calc a rgb xyz'

calc (Object X) rgb (x,y,z) = (x,no,rgb,False)
calc (Object Y) rgb (x,y,z) = (y,no,rgb,False)
calc (Object Z) rgb (x,y,z) = (z,no,rgb,False)

calc (Object (Plane a b c d)) rgb xyz
    = (ans,(no),rgb,False)
     where
      ans = dorow (a,b,c,d) xyz

calc (Object (Sphere a b c r)) rgb xyz
    = (ans,newc,rgb,True)
      where
       (ans,_,_,_) = calc newc rgb xyz
       newc = Func f
       f rgb zyx = (sphere zyx,no,rgb,False)
       sphere :: (R3 BI) -> BI
       sphere (x,y,z) = sqr (x-a') + sqr (y-b') + sqr (z-c') - sqr r'
       a' = realToFrac a ; b' = realToFrac b ; c' = realToFrac c
       r' = realToFrac r

calc (Object (Cube a b c r)) rgb xyz
    = (ans,newc',rgb,bool)
      where
       newc'' = if bool then newc else newc'
       (ans,newc,_,bool) = calc newc' rgb xyz
       newc' = Inter xx (Inter yy zz)
       xx = Inter x1 x2
       yy = Inter y1 y2
       zz = Inter z1 z2
       x1 = Object (Plane ( 1) 0 0 (-(a+r)))
       y1 = Object (Plane 0 ( 1) 0 (-(b+r)))
       z1 = Object (Plane 0 0 ( 1) (-(c+r)))
       x2 = Object (Plane (-1) 0 0 ( (a-r)))
       y2 = Object (Plane 0 (-1) 0 ( (b-r)))
       z2 = Object (Plane 0 0 (-1) ( (c-r)))

calc (Union a b) rgb xyz
    = (min an1 an2,newc,newr,bool)
     where
      (an1,c1,rgb1,b1) = calc a rgb xyz
      (an2,c2,rgb2,b2) = calc b rgb xyz
      bool = b1 || b2
      ca = if b1 then c1 else a
      cb = if b2 then c2 else b
      newr | an1 < an2       = rgb1
           | an1 > an2       = rgb2
           | otherwise       = rgb
      newc | an1 < an2       = ca
           | an1 > an2       = cb
           | not bool        = (no)
           | otherwise       = Union ca cb

calc (Inter a b) rgb xyz
    = (max an1 an2,newc,newr,bool)
     where
      (an1,c1,rgb1,b1) = calc a rgb xyz
      (an2,c2,rgb2,b2) = calc b rgb xyz
      bool = b1 || b2
      ca = if b1 then c1 else a
      cb = if b2 then c2 else b
      newr | an1 > an2       = rgb1
           | an1 < an2       = rgb2
           | otherwise       = rgb
      newc | an1 > an2       = ca
           | an1 < an2       = cb
           | not bool        = (no)
           | otherwise       = Inter ca cb

calc (Comp a) rgb xyz
    = (ans,newc'',newr,True)
     where
      (ans,newc,newr,b) = calc newc' rgb xyz
      newc'' = if b then newc else newc'
      newc' = Matrix a mat
      mat = (m1,m2,m3)
      m1  = (-1, 0, 0, 0)
      m2  = ( 0,-1, 0, 0)
      m3  = ( 0, 0,-1, 0)

calc (Colour c a) rgb xyz
    = (ans,newc,c,bool)
     where
      newc = if bool then (Colour c newc') else (no)
      (ans,newc',_,bool) = calc a c xyz

calc (Sub a b) rgb xyz
    = (ans,newc'',newr,True)
     where
      newc' = (a `Inter` (Comp b))
      newc'' = if bool then newc else newc'
      (ans,newc,newr,bool) = calc newc' rgb xyz

calc (Geom a (Trans h w d)) rgb xyz
    = (ans,newc'',newr,True)
     where
      (ans,newc,newr,b) = calc newc' rgb xyz
      newc'' = if b then newc else newc'
      newc' = Matrix a mat
      mat = (m1,m2,m3)
      m1  = ( 1, 0, 0, h)
      m2  = ( 0, 1, 0, w)
      m3  = ( 0, 0, 1, d)

calc (Geom a (Scale h w d)) rgb xyz
    = (ans,newc'',newr,True)
     where
      (ans,newc,newr,b) = calc newc' rgb xyz
      newc'' = if b then newc else newc'
      newc' = Matrix a mat
      mat = (m1,m2,m3)
      m1  = ( h, 0, 0, 0)
      m2  = ( 0, w, 0, 0)
      m3  = ( 0, 0, d, 0)

calc (Geom a (RotX rad)) rgb xyz
    = (ans,newc'',newr,True)
     where
      (ans,newc,newr,b) = calc newc' rgb xyz
      newc'' = if b then newc else newc'
      newc' = Matrix a mat
      mat = (m1,m2,m3)
      c = cos rad
      s = sin rad
      m1  = ( 1, 0, 0, 0)
      m2  = ( 0, c,-s, 0)
      m3  = ( 0, s, c, 0)

calc (Geom a (RotY rad)) rgb xyz
    = (ans,newc'',newr,True)
     where
      (ans,newc,newr,b) = calc newc' rgb xyz
      newc'' = if b then newc else newc'
      newc' = Matrix a mat
      mat = (m1,m2,m3)
      c = cos rad
      s = sin rad
      m1  = ( c, 0, s, 0)
      m2  = ( 0, 1, 0, 0)
      m3  = (-s, 0, c, 0)

calc (Geom a (RotZ rad)) rgb xyz
    = (ans,newc'',newr,True)
     where
      (ans,newc,newr,b) = calc newc' rgb xyz
      newc'' = if b then newc else newc'
      newc' = Matrix a mat
      mat = (m1,m2,m3)
      c = cos rad
      s = sin rad
      m1  = ( c, s, 0, 0)
      m2  = (-s, c, 0, 0)
      m3  = ( 0, 0, 1, 0)


-- conflate matrices together and into planes planes...
reduceM (Object X)               mata
      =  case (mat1x4 (1,0,0,0) mata) of
	  (x,y,z,w) -> (Object (Plane x y z w),True)
reduceM (Object Y)               mata
      =  case (mat1x4 (0,1,0,0) mata) of
	  (x,y,z,w) -> (Object (Plane x y z w),True)
reduceM (Object Z)               mata
      =  case (mat1x4 (0,0,1,0) mata) of
	  (x,y,z,w) -> (Object (Plane x y z w),True)
reduceM (Object (Plane a b c d)) mata
      =  case (mat1x4 (a,b,c,d) mata) of
	  (x,y,z,w) -> (Object (Plane x y z w),True)
reduceM (Matrix b matb)          mata
      =  case (mat4x4 mata matb)      of
	  matc -> (Matrix b matc,True)
reduceM _                        _    = (no,False)



