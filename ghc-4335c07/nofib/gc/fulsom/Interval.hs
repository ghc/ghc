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
 - Interval arithmetic package.
 -
 -}

module Interval(Interval, (#), pt, sqr,
		tophalf, bothalf, topbit,
		lo, hi, mid1, mid2,
		up,down,unpt)
		where

infix 4 #,:#:

data Interval a = Pt a | a :#: a deriving (Show{-was:Text-})


pt a  = Pt a
a # b = a :#: b

instance (Ord a, Eq a) => Eq (Interval a) where
  a == b        = a >= b && a <= b  -- Not correct - but it will do.
  a /= b        = a >  b || a <  b


instance (Ord a) => Ord (Interval a) where
  (<)  = ivLess
  (<=) = ivLeEq
  (>)  = ivGreat
  (>=) = ivGrEq
  min  = ivMin
  max  = ivMax


instance (Num a,Ord a,Eq a,Show{-was:Text-} a) => Num (Interval a) where
  (+)		= ivPlus
  (*)		= ivMult
  negate	= ivNegate
  abs		= ivAbs
  signum	= ivSignum
  fromInteger	= ivFromInteger


instance (Show a,Num a,Ord a,Fractional a) => Fractional (Interval a) where
  (/)		= ivDiv
  fromRational	= ivFromRational

-- instance (Fractional a,Ord a,Floating a) =>  - not this ?
instance (Show a,RealFloat a) =>
			Floating (Interval a) where
  pi		= Pt pi
  exp		= ivExp
  log		= ivLog
  sqrt		= ivSqrt
  (**)		= ivPower
  sin		= ivSin
  cos		= ivCos
  tan		= ivTan
  asin		= ivAsin
  acos		= ivAcos
  atan		= ivAtan
  sinh		= ivSinh
  cosh		= ivCosh
  tanh		= ivTanh
  asinh		= ivAsinh
  acosh		= ivAcosh
  atanh		= ivAtanh


-- Error functions - un-used.

error0 = error "Not implemented."
error1 a = error "Not implemented."
error2 a b = error "Not implemented."
error3 a b c = error "Not implemented."
error4 a b c d = error "Not implemented."


--  Eq class functions


--  Ord class functions

ivLess (Pt b)    (Pt c)    = b < c
ivLess (a :#: b) (c :#: d) = b < c
ivLess (Pt b)    (c :#: d) = b < c
ivLess (a :#: b) (Pt c)    = b < c

ivLeEq (Pt b)    (Pt d)    = b <= d
ivLeEq (a :#: b) (c :#: d) = b <= d
ivLeEq (Pt b)    (c :#: d) = b <= d
ivLeEq (a :#: b) (Pt d)    = b <= d

ivGreat (Pt a)    (Pt d)    = a > d
ivGreat (a :#: b) (c :#: d) = a > d
ivGreat (Pt a)    (c :#: d) = a > d
ivGreat (a :#: b) (Pt d)    = a > d

ivGrEq (Pt a)    (Pt c)    = a >= c
ivGrEq (a :#: b) (c :#: d) = a >= c
ivGrEq (Pt a)    (c :#: d) = a >= c
ivGrEq (a :#: b) (Pt c)    = a >= c

ivMin (Pt a)    (Pt c)    = Pt (min a c)
ivMin (a :#: b) (c :#: d) = (min a c) :#: (min b d)
ivMin (Pt a)    (c :#: d) | a < c     = Pt a
                          | otherwise = c :#: min a d
ivMin (a :#: b) (Pt c)    | c < a     = Pt c
                          | otherwise = a :#: min c b

ivMax (Pt a)    (Pt c)    = Pt (max a c)
ivMax (a :#: b) (c :#: d) = (max a c) :#: (max b d)
ivMax (Pt a)    (c :#: d) | a > d     = Pt a
                          | otherwise = max a c :#: d
ivMax (a :#: b) (Pt c)    | c > b     = Pt c
                          | otherwise = max c a :#: b

--  Num class functions

ivPlus   (Pt a)    (Pt c)    = Pt (a+c)
ivPlus   (a :#: b) (c :#: d) = a+c :#: b+d
ivPlus   (Pt a)    (c :#: d) = a+c :#: a+d
ivPlus   (a :#: b) (Pt c)    = a+c :#: b+c

ivNegate (Pt a)              = Pt (negate a)
ivNegate (a :#: b)           = negate b :#: negate a

ivMult   (Pt a)    (Pt c)    = Pt (a*c)
ivMult   (a :#: b) (c :#: d) | (min a c) > 0 = a*c :#: b*d
                             | (max b d) < 0 = b*d :#: a*c
			     | otherwise      = minmax [e,f,g,h]
			       where
				 e = b * c
				 f = a * d
				 g = a * c
				 h = b * d
ivMult   (Pt a)    (c :#: d) | a > 0     = a*c :#: a*d
			     | a < 0     = a*d :#: a*c
			     | otherwise = (Pt 0)
ivMult   (c :#: d) (Pt a)    | a > 0     = a*c :#: a*d
			     | a < 0     = a*d :#: a*c
			     | otherwise = (Pt 0)

-- minmax finds the lowest, and highest in a list - used for mult.
-- Should use foldl rather than foldr

minmax [a] = a :#: a
minmax (a:as)  = case True of
		  True | (a > s) -> f :#: a
		  True | (a < f) -> a :#: s
		  otherwise      -> f :#: s
                 where
                     (f :#: s) = minmax as

ivAbs (Pt a)    = Pt (abs a)
ivAbs (a :#: b) | a<=0 && 0<=b   = 0 :#: (max (abs a) (abs b))
		| a<=b && b<0    = b :#: a
		| 0<a && a<=b    = a :#: b
		| otherwise = error "abs doesny work!"

ivSignum (Pt a)    = Pt (signum a)
ivSignum (a :#: b) = (signum a) :#: (signum b)

ivFromInteger a = Pt (fromInteger a)

--  Fractional class functions

ivDiv a (Pt c)    = ivMult a (Pt (1/c))
ivDiv a (c :#: d) = ivMult a (1/c :#: 1/d)
ivFromRational a  = Pt (fromRational a)

--  Floating class functions

-- ivPi () = fromRational pi

ivExp (Pt a)    = Pt (exp a)
ivExp (a :#: b) = (exp a) :#: (exp b)

ivLog (Pt a)    = Pt (log a)
ivLog (a :#: b) = (log a) :#: (log b)

ivSqrt (Pt a)    = Pt (sqrt a)
ivSqrt (a :#: b) = (sqrt a) :#: (sqrt b)

ivPower x y = exp (log x * y)		-- Optimise for x ** 2


ivSin :: (Floating a) => (Interval a) -> (Interval a)
ivSin a = error "Floating op not defined."
ivCos :: (Floating a) => (Interval a) -> (Interval a)
ivCos a = error "Floating op not defined."
ivTan :: (Floating a) => (Interval a) -> (Interval a)
ivTan a = error "Floating op not defined."
ivAsin :: (Floating a) => (Interval a) -> (Interval a)
ivAsin a = error "Floating op not defined."
ivAcos :: (Floating a) => (Interval a) -> (Interval a)
ivAcos a = error "Floating op not defined."
ivAtan :: (Floating a) => (Interval a) -> (Interval a)
ivAtan a = error "Floating op not defined."
ivSinh :: (Floating a) => (Interval a) -> (Interval a)
ivSinh a = error "Floating op not defined."
ivCosh :: (Floating a) => (Interval a) -> (Interval a)
ivCosh a = error "Floating op not defined."
ivTanh :: (Floating a) => (Interval a) -> (Interval a)
ivTanh a = error "Floating op not defined."
ivAsinh :: (Floating a) => (Interval a) -> (Interval a)
ivAsinh a = error "Floating op not defined."
ivAcosh :: (Floating a) => (Interval a) -> (Interval a)
ivAcosh a = error "Floating op not defined."
ivAtanh :: (Floating a) => (Interval a) -> (Interval a)
ivAtanh a = error "Floating op not defined."

-- Extra math functions not part of classes

sqr (Pt a)    = Pt (a*a)
sqr (a :#: b) | a > 0     = a*a :#: b*b
              | b < 0     = b*b :#: a*a
              | otherwise = 0 :#: (max e f)
                 where
                   e = a * a
                   f = b * b


-- Other Functions specific to interval type

tophalf (a :#: b) = (a+b)/2 :#: b
bothalf (a :#: b) = a :#: (a+b)/2
topbit  (a :#: b) = (a+b)/2-0.001 :#: b

lo (a :#: b) = a
hi (a :#: b) = b

down (a :#: b) = Pt a
up   (a :#: b) = Pt b

unpt (Pt a) = a

mid1 (a :#: b) = Pt (a + (b-a)/3)
mid2 (a :#: b) = Pt (b - (b-a)/3)


-- END --
