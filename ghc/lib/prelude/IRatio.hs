--*** All of PreludeRatio, except the actual data/type decls.
--*** data Ratio ... is builtin (no need to import TyRatio)

module PreludeRatio where

import Cls
import Core
import IChar
import IDouble
import IFloat
import IInt
import IInteger
import IList
import List		( iterate, (++), foldr, takeWhile )
import Prel		( (&&), (||), (.), otherwise, gcd, fromIntegral, id )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

--infixl 7  %, :%

prec = (7 :: Int)

{-# GENERATE_SPECS (%) a{Integer} #-}
(%)		:: (Integral a) => a -> a -> Ratio a

numerator :: Ratio a -> a
numerator (x:%y)	=  x

denominator :: Ratio a -> a
denominator (x:%y)	=  y


x % y			=  reduce (x * signum y) (abs y)

reduce x y | y == __i0	=  error "(%){PreludeRatio}: zero denominator\n"
           | otherwise	=  (x `quot` d) :% (y `quot` d)
			   where d = gcd x y

instance (Integral a) => Eq (Ratio a) where
    {- works because Ratios held in reduced form -}
    (x :% y) == (x2 :% y2)  =  x == x2 && y == y2
    (x :% y) /= (x2 :% y2)  =  x /= x2 || y /= y2

instance (Integral a) => Ord (Ratio a) where
    (x1:%y1) <= (x2:%y2) =  x1 * y2 <= x2 * y1
    (x1:%y1) <  (x2:%y2) =  x1 * y2 <  x2 * y1
    (x1:%y1) >= (x2:%y2) =  x1 * y2 >= x2 * y1
    (x1:%y1) >  (x2:%y2) =  x1 * y2 >  x2 * y1
    min x y | x <= y    = x
    min x y | otherwise = y
    max x y | x >= y    = x
    max x y | otherwise = y
    _tagCmp (x1:%y1) (x2:%y2)
      = if x1y2 == x2y1 then _EQ else if x1y2 < x2y1 then _LT else _GT
      where x1y2 = x1 * y2
	    x2y1 = x2 * y1

instance (Integral a) => Num (Ratio a) where
    (x1:%y1) + (x2:%y2)	=  reduce (x1*y2 + x2*y1) (y1*y2)
    (x1:%y1) - (x2:%y2)	=  reduce (x1*y2 - x2*y1) (y1*y2)
    (x1:%y1) * (x2:%y2)	=  reduce (x1 * x2) (y1 * y2)
    negate (x:%y)	=  (-x) :% y
    abs (x:%y)		=  abs x :% y
    signum (x:%y)	=  signum x :% __i1
    fromInteger x	=  fromInteger x :% __i1
    fromInt x		=  fromInt x :% __i1

instance (Integral a) => Real (Ratio a) where
    toRational (x:%y)	=  toInteger x :% toInteger y

instance (Integral a) => Fractional (Ratio a) where
    (x1:%y1) / (x2:%y2)	=  (x1*y2) % (y1*x2)
    recip (x:%y)	=  if x < __i0 then (-y) :% (-x) else y :% x
    fromRational (x:%y) =  fromInteger x :% fromInteger y


instance (Integral a) => Enum (Ratio a) where
    enumFrom		 =  iterate ((+) __i1)
    enumFromThen n m	 =  iterate ((+) (m-n)) n
    enumFromTo n m	 =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

instance  (Integral a) => Text (Ratio a)  where
    readsPrec p  =  readParen (p > prec)
			      (\r -> [(x%y,u) | (x,s)   <- readsPrec 0 r,
					        ("%",t) <- lex s,
						(y,u)   <- readsPrec 0 t ])

    showsPrec p (x:%y)	=  showParen (p > prec)
    	    	    	       (showsPrec 0 x . showString " % " . showsPrec 0 y)

    readList	= _readList (readsPrec 0)
    showList	= _showList (showsPrec 0) 

{-# SPECIALIZE instance Eq  	    (Ratio Integer) #-}
{-# SPECIALIZE instance Ord 	    (Ratio Integer) #-}
{-# SPECIALIZE instance Num 	    (Ratio Integer) #-}
{-# SPECIALIZE instance Real	    (Ratio Integer) #-}
{-# SPECIALIZE instance Fractional  (Ratio Integer) #-}
{-# SPECIALIZE instance Enum	    (Ratio Integer) #-}
{-# SPECIALIZE instance Text	    (Ratio Integer) #-}

-- We have to give a real overlapped instance for RealFrac (Ratio Integer)
-- since we need to give SPECIALIZE pragmas

-- ToDo: Allow (ignored) SPEC pragmas in poly instance]
--       and substitute for tyvars in a SPECIALIZED instance

instance RealFrac (Ratio Integer) where

    {-# SPECIALIZE properFraction :: Rational -> (Int, Rational) #-}
    {-# SPECIALIZE truncate :: Rational -> Int #-}
    {-# SPECIALIZE round    :: Rational -> Int #-}
    {-# SPECIALIZE ceiling  :: Rational -> Int #-}
    {-# SPECIALIZE floor    :: Rational -> Int #-}

    {-# SPECIALIZE properFraction :: Rational -> (Integer, Rational) #-}
    {-# SPECIALIZE truncate :: Rational -> Integer #-}
    {-# SPECIALIZE round    :: Rational -> Integer #-}
    {-# SPECIALIZE ceiling  :: Rational -> Integer #-}
    {-# SPECIALIZE floor    :: Rational -> Integer #-}

    properFraction (x:%y) = case quotRem x y of
			      (q,r) -> (fromIntegral q, r:%y)

    truncate x	= case properFraction x of
		     (n,_) -> n

    round x	= case properFraction x of
		     (n,r) -> let
			      	m         = if r < __i0 then n - __i1 else n + __i1
		  	      	half_down = abs r - __rhalf
    		   	      in
    		   	      case (_tagCmp half_down __i0) of
      		     		_LT -> n
      		     		_EQ -> if even n then n else m
      		     		_GT -> m

    ceiling x   = case properFraction x of
		    (n,r) -> if r > __i0 then n + __i1 else n

    floor x	= case properFraction x of
		    (n,r) -> if r < __i0 then n - __i1 else n


-- approxRational, applied to two real fractional numbers x and epsilon,
-- returns the simplest rational number within epsilon of x.  A rational
-- number n%d in reduced form is said to be simpler than another n'%d' if
-- abs n <= abs n' && d <= d'.  Any real interval contains a unique
-- simplest rational; here, for simplicity, we assume a closed rational
-- interval.  If such an interval includes at least one whole number, then
-- the simplest rational is the absolutely least whole number.  Otherwise,
-- the bounds are of the form q%1 + r%d and q%1 + r'%d', where abs r < d
-- and abs r' < d', and the simplest rational is q%1 + the reciprocal of
-- the simplest rational between d'%r' and d%r.

{-# GENERATE_SPECS approxRational a{Double#,Double} #-}
approxRational	:: (RealFrac a) => a -> a -> Rational

approxRational x eps	=  simplest (x-eps) (x+eps)
	where simplest x y | y < x	=  simplest y x
			   | x == y	=  xr
			   | x > 0	=  simplest' n d n' d'
			   | y < 0	=  - simplest' (-n') d' (-n) d
			   | otherwise	=  __i0
					where xr@(n:%d) = toRational x
					      (n':%d')	= toRational y

	      simplest' n d n' d'	-- assumes 0 < n%d < n'%d'
			| r == __i0  =	q :% __i1
			| q /= q'    =	(q + __i1) :% __i1
			| otherwise  =	(q*n''+d'') :% n''
				     where (q,r)      =	 quotRem n d
					   (q',r')    =	 quotRem n' d'
					   (n'':%d'') =	 simplest' d' r' d r
