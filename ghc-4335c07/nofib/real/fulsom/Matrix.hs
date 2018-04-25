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
 - Matrix arithmetic functions.
 -
 -}

module Matrix where

import Types
import Interval -- not used.

mat4x1 :: (Fractional a) => Arr -> R3 a -> R3 a
mat4x1 (r1,r2,r3) xyz = (x,y,z)
                          where
                             x = dorow r1 xyz
                             y = dorow r2 xyz
                             z = dorow r3 xyz

dorow :: (Fractional a) => Row -> R3 a -> a
dorow (m11,m12,m13,m14) (x,y,z) 
    = case  (m1 * x) + (m2 * y) + (m3 * z) + m4 of n -> n
       where
	 m1 = realToFrac m11
	 m2 = realToFrac m12
	 m3 = realToFrac m13
	 m4 = realToFrac m14

mat4x1' :: (Fractional a) => Arr -> R3 a -> R3 a
mat4x1' (r1,r2,r3) xyz = (x,y,z)
                          where
                             x = dorow r1 xyz
                             y = dorow r2 xyz
                             z = dorow r3 xyz

dorow' :: (Fractional a) => Row -> R3 a -> a
dorow' (m11,m12,m13,m14) (x,y,z) 
    = case (m1 * x) + (m2 * y) + (m3 * z) of n -> n
       where
	 m1 = realToFrac m11
	 m2 = realToFrac m12
	 m3 = realToFrac m13

mat1x4 :: Row -> Arr -> Row
mat1x4 a (b1,b2,b3) = (c1,c2,c3,c4)
   where
     c1 = dorow' a (b11,b21,b31)
     c2 = dorow' a (b12,b22,b32)
     c3 = dorow' a (b13,b23,b33)
     c4 = dorow  a (b14,b24,b34)
     (b11,b12,b13,b14) = b1
     (b21,b22,b23,b24) = b2
     (b31,b32,b33,b34) = b3

mat4x4 :: Arr -> Arr -> Arr
mat4x4 a (b1,b2,b3) = (c1,c2,c3)
   where
     c1 = (c11,c12,c13,c14)
     c2 = (c21,c22,c23,c24)
     c3 = (c31,c32,c33,c34)
     (b11,b12,b13,b14) = b1
     (b21,b22,b23,b24) = b2
     (b31,b32,b33,b34) = b3
     (c11,c21,c31) = mat4x1' a (b11,b21,b31)
     (c12,c22,c32) = mat4x1' a (b12,b22,b32)
     (c13,c23,c33) = mat4x1' a (b13,b23,b33)
     (c14,c24,c34) = mat4x1  a (b14,b24,b34)




