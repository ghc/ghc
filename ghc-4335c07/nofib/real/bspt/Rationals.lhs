> module Rationals 

>		(Rationals(..),rndNR)

> where
 
> import Data.Ratio
> infix 7 :%%


	Data declaration for Rationals. 
	Rationals are defined in terms of Ints.
  
> data Rationals = Int :%% Int deriving (Show{-was:Text-},Eq)

	LazyRationals instance of Ord declared with
	simple degeneration to lazyInteger where
	numerators are multiplied by opposite denumerators.

> instance Ord Rationals where
>	(p :%% q) <= (r :%% s) = p*s <= q*r

> instance Num Rationals where
	
	(+): rational addition is performed by converting
	the arguments to a form where they share a common
	denominator. 
	The function simplify ensures that the answer
	is in normal form. 
	Unit denominators are treated as special cases.

>	(+) (p :%% 1) (r :%% 1) = (p+r) :%% 1
>	(+) (p :%% 1) (r :%% s) = simplify (p*s +r) s
>	(+) (p :%% q) (r :%% 1) = simplify (p+ q*r) q
>	(+) (p :%% q) (r :%% s) = simplify (p*s+q*r) (q*s)
 

	Multiplication of rationals provided by degeneration
	to Ints. Unit denominators are treated as special
	cases.

>	(*) (p :%% 1) (r :%% 1) = (p*r) :%% 1
>	(*) (p :%% 1) (r :%% s) = simplify (p*r) s
>	(*) (p :%% q) (r :%% 1) = simplify (p*r) q
>	(*) (p :%% q) (r :%% s) = simplify (p*r) (q*s)


	negate: Simply change the sign of the numerator to negate

>	negate (x :%% y) = (negate x :%% y)

	abs: Take the abs value of the numerator and place over the
		denominator 

>	abs (x :%% y) = (abs x :%% y)

	signum: sign simply determined by sign of numerator

>	signum (x :%% _) = (signum x:%%1)

	fromInteger: Change to an Integer and stick it over one.

>	fromInteger x = (fromInteger x) :%% 1

	defines LazyRational as a instance of the class Fractional 

> instance Fractional Rationals where

	divideLazyNum: performed by turning over second Rational
		and multiplying.
	Zero cases handled appropriately.

> 	(/) x y@(r :%% s)	| r==0 = error "Attempt to divide by Zero" 
>  				| otherwise = x * (s :%% r) 


	fromRational : defines conversion of a Rational
	(Ratio Integer) to  Rationals. NB Rational
	is supposedly in normal form.
  
>	fromRational r = (fromInteger (numerator r)) :%% (fromInteger (denominator r))


	simplify: produces a Rational in normal
	form from the Ints given. Note normal
	form means a positive denominator. Sign
	of numerator therefore determines sign of number.
 
> simplify :: Int -> Int -> Rationals
> simplify x y  = (signum bottom*top) :%% abs bottom
>			where 
>			top = x `div` d
>			bottom = y `div` d
>			d = gcd x y


	Defines Rationals as a member of the class Real. 

> instance Real Rationals where
> 	toRational (x:%%y) = toInteger x % toInteger y

> instance Enum Rationals where	-- partain
>    enumFrom		= error "Enum.Rationals.enumFrom"
>    enumFromThen	= error "Enum.Rationals.enumFromThen"
>    enumFromTo		= error "Enum.Rationals.enumFromTo"
>    enumFromThenTo	= error "Enum.Rationals.enumFromThenTo"


	top : extracts the numerator part of a rational

> top ::  Rationals -> Int
> top (a :%% b) = a


	bottom : extract the denominator part of a rational  

> bottom :: Rationals -> Int
> bottom (a :%% b) = b
 

	rndNR : Converts a Rational to an Int. Rounding to the
		nearest. NB: Safe to use Int. Only results
		that lie on screen are arguments to rndNR. 
		NB: Also no need to worry about rounding 
		negative strategy for same reason.
 
> rndNR ::  Rationals -> Int
> rndNR (a :%% 1) = fromInteger (toInteger a)
> rndNR a =  fromInteger (toInteger (div n d))
>                         where r = (1/2) + a
>                               n = top r
>                               d = bottom r
 

