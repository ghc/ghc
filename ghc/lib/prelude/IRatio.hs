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

--infixl 7  %, :%

prec = (7 :: Int)

{-# GENERATE_SPECS (%) a{Integer} #-}
(%)		:: (Integral a) => a -> a -> Ratio a

numerator :: Ratio a -> a
numerator (x:%y)	=  x

denominator :: Ratio a -> a
denominator (x:%y)	=  y


x % y			=  reduce (x * signum y) (abs y)

reduce _ 0		=  error "(%){PreludeRatio}: zero denominator\n"
reduce x y		=  (x `quot` d) :% (y `quot` d)
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
    signum (x:%y)	=  signum x :% 1
    fromInteger x	=  fromInteger x :% 1
    fromInt x		=  fromInt x :% 1

instance (Integral a) => Real (Ratio a) where
    toRational (x:%y)	=  toInteger x :% toInteger y

instance (Integral a) => Fractional (Ratio a) where
    (x1:%y1) / (x2:%y2)	=  (x1*y2) % (y1*x2)
    recip (x:%y)	=  if x < 0 then (-y) :% (-x) else y :% x
    fromRational (x:%y) =  fromInteger x :% fromInteger y

instance (Integral a) => RealFrac (Ratio a) where
    properFraction (x:%y) = (fromIntegral q, r:%y)
			    where (q,r) = quotRem x y

    -- just call the versions in Core.hs
    truncate x	=  _truncate x
    round x	=  _round x
    ceiling x	=  _ceiling x
    floor x	=  _floor x

instance (Integral a) => Enum (Ratio a) where
    enumFrom		 = iterate ((+)1)
    enumFromThen n m	 = iterate ((+)(m-n)) n
    enumFromTo n m	 = takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p = takeWhile (if m >= n then (<= p) else (>= p))
				     (enumFromThen n m)

instance  (Integral a) => Text (Ratio a)  where
    readsPrec p  =  readParen (p > prec)
			      (\r -> [(x%y,u) | (x,s)   <- reads r,
					        ("%",t) <- lex s,
						(y,u)   <- reads t ])

    showsPrec p (x:%y)	=  showParen (p > prec)
    	    	    	       (shows x . showString " % " . shows y)

{-# SPECIALIZE instance Eq  	    (Ratio Integer) #-}
{-# SPECIALIZE instance Ord 	    (Ratio Integer) #-}
{-# SPECIALIZE instance Num 	    (Ratio Integer) #-}
{-# SPECIALIZE instance Real	    (Ratio Integer) #-}
{-# SPECIALIZE instance Fractional  (Ratio Integer) #-}
{-# SPECIALIZE instance RealFrac    (Ratio Integer) #-}
{-# SPECIALIZE instance Enum	    (Ratio Integer) #-}
{-# SPECIALIZE instance Text	    (Ratio Integer) #-}

{- ToDo: Ratio Int# ???
#if defined(__UNBOXED_INSTANCES__)

{-# SPECIALIZE instance Eq  	    (Ratio Int#) #-}
{-# SPECIALIZE instance Ord 	    (Ratio Int#) #-}
{-# SPECIALIZE instance Num 	    (Ratio Int#) #-}
{-# SPECIALIZE instance Real	    (Ratio Int#) #-}
{-# SPECIALIZE instance Fractional  (Ratio Int#) #-}
{-# SPECIALIZE instance RealFrac    (Ratio Int#) #-}
{-# SPECIALIZE instance Enum	    (Ratio Int#) #-}
{-# SPECIALIZE instance Text	    (Ratio Int#) #-}

#endif
-}

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

--{-# GENERATE_SPECS approxRational a{Double#,Double} #-}
{-# GENERATE_SPECS approxRational a{Double} #-}
approxRational	:: (RealFrac a) => a -> a -> Rational

approxRational x eps	=  simplest (x-eps) (x+eps)
	where simplest x y | y < x	=  simplest y x
			   | x == y	=  xr
			   | x > 0	=  simplest' n d n' d'
			   | y < 0	=  - simplest' (-n') d' (-n) d
			   | otherwise	=  0 :% 1
					where xr@(n:%d) = toRational x
					      (n':%d')	= toRational y

	      simplest' n d n' d'	-- assumes 0 < n%d < n'%d'
			| r == 0     =	q :% 1
			| q /= q'    =	(q+1) :% 1
			| otherwise  =	(q*n''+d'') :% n''
				     where (q,r)      =	 quotRem n d
					   (q',r')    =	 quotRem n' d'
					   (n'':%d'') =	 simplest' d' r' d r
