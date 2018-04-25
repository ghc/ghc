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
 - Vector arithmetic routines.
 -
 -}

module Vector where

import Interval
import Types

makevector :: In -> In -> In -> In -> Vector
makevector a' b' c' d' = (x,y,z)
  where
    a = unpt a' ; b = unpt b' ; c = unpt c' ; d = unpt d'
    x = b - a
    y = c - a
    z = d - a

normalise :: Vector -> Vector
normalise xyz@(x,y,z) = (x/l,y/l,z/l)
  where l = len xyz

len :: Vector -> FType
len (x,y,z) = ans
  where
    ans | sqs /= 0.0 = sqrt sqs
	| True       = 1
    sqs :: FType
    sqs = (x2 + y2 + z2)
    x2  = x * x
    y2  = y * y
    z2  = z * z

light :: Color -> Vector -> Color
light (RGB r g b) (x,y,z) = RGB (a*r) (a*g) (a*b)
    where a = (max ((0.5773*x + 0.5773*y + 0.5773*z) * (1-amb)) 0) + amb

-- amb = (0.05 :: FType)
amb = (0.50 :: FType)
