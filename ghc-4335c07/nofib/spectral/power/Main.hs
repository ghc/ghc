{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{- 	Taken from Doug McIlroy's paper
	"Power series, power serious"
	JFP 9(3) May 1999
-}

module Main where

import System.IO
import Data.Ratio
import System.Environment (getArgs)

infixl 7 .*
infixr 5 :+: 

default (Integer, Rational, Double)

main = do { (n:_) <- getArgs ;
	    let { p = read n :: Int } ;
	    putStrLn (show (extract p (sinx - sqrt (1-cosx^2)))) ;
	    putStrLn (show (extract p (sinx/cosx - revert (integral (1/(1+x^2)))))) ;
	    putStrLn (show (extract p ts)) ;
	    putStrLn (show (extract p tree)) 
	  }
	

-- From Section 6
tree   = 0 :+: forest
forest = compose list tree
list   = 1 :+: list

ts = 1 :+: ts^2
	

-- The main implementation follows
data Ps a = Pz | a :+: Ps a

extract :: Int -> Ps a -> [a]
extract 0 ps 	     = []
extract n Pz 	     = []
extract n (x :+: ps) = x : extract (n-1) ps

deriv:: Num a => Ps a -> Ps a
integral:: Fractional a => Ps a -> Ps a
compose:: (Eq a, Num a) => Ps a -> Ps a -> Ps a
revert:: (Eq a, Fractional a) => Ps a -> Ps a
toList:: Num a => Ps a -> [a]
takePs:: Num a => Int -> Ps a -> [a]
(.*):: Num a => a -> Ps a -> Ps a
x:: Num a => Ps a
expx, sinx, cosx:: Fractional a => Ps a

c .* Pz = Pz
c .* (f :+: fs) = c*f :+: c.*fs

x = 0 :+: 1 :+: Pz

toList Pz = []						--(0)
toList (f :+: fs) = f : (toList fs)

takePs n fs = take n (toList fs)

instance (Eq a, Num a) => Eq (Ps a) where			--(1)
	Pz == Pz = True
	Pz == (f :+: fs) = f==0 && Pz==fs
	fs == Pz = Pz==fs
	(f :+: fs) == (g :+: gs) = f==g && fs==gs

instance (Show a, Num a) => Show (Ps a) where			--(2)
	showsPrec p Pz = showsPrec p [0]
	showsPrec p fs = showsPrec p (toList fs)

instance Num a => Num (Ps a) where
	negate Pz = Pz
	negate (f :+: fs) = -f :+: -fs

	Pz + gs = gs
	fs + Pz = fs
	(f :+: fs) + (g :+: gs) = f+g :+: fs+gs

	Pz * _ = Pz
	_ * Pz = Pz
	(f :+: fs) * (g :+: gs) =
		f*g :+: f.*gs + g.*fs + x*fs*gs		--(3)

	fromInteger 0 = Pz
	fromInteger c = fromInteger c :+: Pz

instance (Eq a, Fractional a) => Fractional (Ps a) where
	recip fs = 1/fs

	Pz/Pz = error "power series 0/0"
	Pz / (0 :+: gs) = Pz / gs
	Pz / _ = Pz
	(0 :+: fs) / (0 :+: gs) = fs / gs
	(f :+: fs) / (g :+: gs) = let q = f/g in
		q :+: (fs - q.*gs)/(g :+: gs)

compose Pz _ = Pz
compose (f :+: _) Pz = f :+: Pz
compose (f :+: fs) (0 :+: gs) = f :+: gs*(compose fs (0 :+: gs))
compose (f :+: fs) gs = (f :+: Pz) + gs*(compose fs gs)	--(4)

revert (0 :+: fs) = rs where
	rs = 0 :+: 1/(compose fs rs)
revert (f0 :+: f1 :+: Pz) = -1/f1 :+: 1/f1 :+: Pz	--(5)

deriv Pz = Pz
deriv (_ :+: fs) = deriv1 fs 1 where
	deriv1 Pz _ = Pz
	deriv1 (f :+: fs) n = n*f :+: (deriv1 fs (n+1))

integral fs = 0 :+: (int1 fs 1) where			--(6)
	int1 Pz _ = Pz
	int1 (f :+: fs) n = f/n :+: (int1 fs (n+1))

instance (Eq a, Fractional a) => Floating (Ps a) where
	sqrt Pz = Pz
	sqrt (0 :+: 0 :+: fs) = 0 :+: (sqrt fs)
	sqrt (1 :+: fs) = qs where
		qs = 1 + integral((deriv (1:+:fs))/(2.*qs))

expx = 1 + (integral expx)
sinx = integral cosx
cosx  = 1 - (integral sinx)

--(0) Convert power series to a list; used in printing.
--(1) Equality works on polynomials; diverges otherwise.
--(2) Specifies how to print the new data type.
--(3) x*fs*gs replaces 0:fs*gs to avoid extra zero
--    at end of product of polynomials; it works
--    because x is a finite series.
--(4) This extra production works for the composition
--    of polynomials with non-zero constant term,
--    but not for infinite series.
--(5) Special case for reverting a linear function.
--(6) There is no special case for (integral Pz)
--    because this would defeat the property that
--    (integral) emits one term before evaluating
--    its operand--a property used in solving
--    differential equations.

