-- Standard functions on rational numbers

module	Ratio (
    Ratio, Rational, (%), numerator, denominator, approxRational ) where

infixl 7  %
--partain:infixl 7  :%

prec = 7

data  (Integral a)	=> Ratio a = a :% a  deriving (Eq)
type  Rational		=  Ratio Integer

(%)			:: (Integral a) => a -> a -> Ratio a
numerator, denominator	:: (Integral a) => Ratio a -> a
approxRational		:: (RealFrac a) => a -> a -> Rational


reduce _ 0		=  error "{Ratio.%}: zero denominator"
reduce x y		=  (x `quot` d) :% (y `quot` d)
			   where d = gcd x y

x % y			=  reduce (x * signum y) (abs y)

numerator (x:%y)	=  x

denominator (x:%y)	=  y


instance  (Integral a)	=> Ord (Ratio a)  where
    (x:%y) <= (x':%y')	=  x * y' <= x' * y
    (x:%y) <  (x':%y')	=  x * y' <  x' * y

instance  (Integral a)	=> Num (Ratio a)  where
    (x:%y) + (x':%y')	=  reduce (x*y' + x'*y) (y*y')
    (x:%y) * (x':%y')	=  reduce (x * x') (y * y')
    negate (x:%y)	=  (-x) :% y
    abs (x:%y)		=  abs x :% y
    signum (x:%y)	=  signum x :% 1
    fromInteger x	=  fromInteger x :% 1

instance  (Integral a)	=> Real (Ratio a)  where
    toRational (x:%y)	=  toInteger x :% toInteger y

instance  (Integral a)	=> Fractional (Ratio a)  where
    (x:%y) / (x':%y')	=  (x*y') % (y*x')
    recip (x:%y)	=  if x < 0 then (-y) :% (-x) else y :% x
    fromRational (x:%y) =  fromInteger x :% fromInteger y

instance  (Integral a)	=> RealFrac (Ratio a)  where
    properFraction (x:%y) = (fromIntegral q, r:%y)
			    where (q,r) = quotRem x y

instance  (Integral a)	=> Enum (Ratio a)  where
    enumFrom		=  iterate ((+)1)
    enumFromThen n m	=  iterate ((+)(m-n)) n
    toEnum n            =  fromIntegral n :% 1
    fromEnum            =  fromInteger . truncate

instance  (Integral a, Read a)  => Read (Ratio a)  where
    readsPrec p  =  readParen (p > prec)
			      (\r -> [(x%y,u) | (x,s)   <- reads r,
					        ("%",t) <- lex s,
						(y,u)   <- reads t ])

instance  (Integral a)  => Show (Ratio a)  where
    showsPrec p (x:%y)	=  showParen (p > prec)
    	    	    	       (shows x . showString " % " . shows y)


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
