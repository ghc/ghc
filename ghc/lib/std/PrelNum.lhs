%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelNum]{Module @PrelNum@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelNum where

import {-# SOURCE #-} PrelErr
import PrelBase
import PrelList
import PrelEnum
import PrelShow

infixr 8  ^, ^^, **
infixl 7  %, /, `quot`, `rem`, `div`, `mod`
infixl 7  *
infixl 6  +, -

\end{code}

%*********************************************************
%*							*
\subsection{Standard numeric classes}
%*							*
%*********************************************************

\begin{code}
class  (Eq a, Show a) => Num a  where
    (+), (-), (*)	:: a -> a -> a
    negate		:: a -> a
    abs, signum		:: a -> a
    fromInteger		:: Integer -> a
    fromInt		:: Int -> a -- partain: Glasgow extension

    x - y		= x + negate y
    negate x		= 0 - x
    fromInt (I# i#)	= fromInteger (S# i#)
					-- Go via the standard class-op if the
					-- non-standard one ain't provided

class  (Num a, Ord a) => Real a  where
    toRational		::  a -> Rational

class  (Real a, Enum a) => Integral a  where
    quot, rem, div, mod	:: a -> a -> a
    quotRem, divMod	:: a -> a -> (a,a)
    toInteger		:: a -> Integer
    toInt		:: a -> Int -- partain: Glasgow extension

    n `quot` d		=  q  where (q,_) = quotRem n d
    n `rem` d		=  r  where (_,r) = quotRem n d
    n `div` d		=  q  where (q,_) = divMod n d
    n `mod` d		=  r  where (_,r) = divMod n d
    divMod n d 		=  if signum r == negate (signum d) then (q-1, r+d) else qr
			   where qr@(q,r) = quotRem n d

class  (Num a) => Fractional a  where
    (/)			:: a -> a -> a
    recip		:: a -> a
    fromRational	:: Rational -> a

    recip x		=  1 / x
    x / y		= x * recip y

class  (Fractional a) => Floating a  where
    pi			:: a
    exp, log, sqrt	:: a -> a
    (**), logBase	:: a -> a -> a
    sin, cos, tan	:: a -> a
    asin, acos, atan	:: a -> a
    sinh, cosh, tanh	:: a -> a
    asinh, acosh, atanh :: a -> a

    x ** y		=  exp (log x * y)
    logBase x y		=  log y / log x
    sqrt x		=  x ** 0.5
    tan  x		=  sin  x / cos  x
    tanh x		=  sinh x / cosh x

class  (Real a, Fractional a) => RealFrac a  where
    properFraction	:: (Integral b) => a -> (b,a)
    truncate, round	:: (Integral b) => a -> b
    ceiling, floor	:: (Integral b) => a -> b

    truncate x		=  m  where (m,_) = properFraction x
    
    round x		=  let (n,r) = properFraction x
    			       m     = if r < 0 then n - 1 else n + 1
    			   in case signum (abs r - 0.5) of
    				-1 -> n
    			 	0  -> if even n then n else m
    				1  -> m
    
    ceiling x		=  if r > 0 then n + 1 else n
    			   where (n,r) = properFraction x
    
    floor x		=  if r < 0 then n - 1 else n
    			   where (n,r) = properFraction x

class  (RealFrac a, Floating a) => RealFloat a  where
    floatRadix		:: a -> Integer
    floatDigits		:: a -> Int
    floatRange		:: a -> (Int,Int)
    decodeFloat		:: a -> (Integer,Int)
    encodeFloat		:: Integer -> Int -> a
    exponent		:: a -> Int
    significand		:: a -> a
    scaleFloat		:: Int -> a -> a
    isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE
                        :: a -> Bool
    atan2	        :: a -> a -> a


    exponent x		=  if m == 0 then 0 else n + floatDigits x
			   where (m,n) = decodeFloat x

    significand x	=  encodeFloat m (negate (floatDigits x))
			   where (m,_) = decodeFloat x

    scaleFloat k x	=  encodeFloat m (n+k)
			   where (m,n) = decodeFloat x
			   
    atan2 y x
      | x > 0            =  atan (y/x)
      | x == 0 && y > 0  =  pi/2
      | x <  0 && y > 0  =  pi + atan (y/x) 
      |(x <= 0 && y < 0)            ||
       (x <  0 && isNegativeZero y) ||
       (isNegativeZero x && isNegativeZero y)
                         = -atan2 (-y) x
      | y == 0 && (x < 0 || isNegativeZero x)
                          =  pi    -- must be after the previous test on zero y
      | x==0 && y==0      =  y     -- must be after the other double zero tests
      | otherwise         =  x + y -- x or y is a NaN, return a NaN (via +)

\end{code}

%*********************************************************
%*							*
\subsection{Instances for @Int@}
%*							*
%*********************************************************

\begin{code}
instance  Num Int  where
    (+)	   x y =  plusInt x y
    (-)	   x y =  minusInt x y
    negate x   =  negateInt x
    (*)	   x y =  timesInt x y
    abs    n   = if n `geInt` 0 then n else (negateInt n)

    signum n | n `ltInt` 0 = negateInt 1
	     | n `eqInt` 0 = 0
	     | otherwise   = 1

    fromInteger (S# i#) = I# i#
    fromInteger (J# s# d#)
      = case (integer2Int# s# d#) of { i# -> I# i# }

    fromInt n		= n

instance  Real Int  where
    toRational x	=  toInteger x % 1

instance  Integral Int	where
    a@(I# _) `quotRem` b@(I# _)	= (a `quotInt` b, a `remInt` b)
    -- OK, so I made it a little stricter.  Shoot me.  (WDP 94/10)

    -- Following chks for zero divisor are non-standard (WDP)
    a `quot` b	=  if b /= 0
		   then a `quotInt` b
		   else error "Prelude.Integral.quot{Int}: divide by 0"
    a `rem` b	=  if b /= 0
		   then a `remInt` b
		   else error "Prelude.Integral.rem{Int}: divide by 0"

    x `div` y = if x > 0 && y < 0	then quotInt (x-y-1) y
		else if x < 0 && y > 0	then quotInt (x-y+1) y
		else quotInt x y
    x `mod` y = if x > 0 && y < 0 || x < 0 && y > 0 then
		    if r/=0 then r+y else 0
	    	else
		    r
	      where r = remInt x y

    divMod x@(I# _) y@(I# _) = (x `div` y, x `mod` y)
    -- Stricter.  Sorry if you don't like it.  (WDP 94/10)

--OLD:   even x = eqInt (x `mod` 2) 0
--OLD:   odd x  = neInt (x `mod` 2) 0

    toInteger (I# i)  = int2Integer i  -- give back a full-blown Integer
    toInt x	      = x

\end{code}

%*********************************************************
%*							*
\subsection{Instances for @Integer@}
%*							*
%*********************************************************

\begin{code}
instance  Ord Integer  where
    (S# i)     <=  (S# j)     = i <=# j
    (J# s d)   <=  (S# i)     = cmpIntegerInt# s d i <=# 0#
    (S# i)     <=  (J# s d)   = cmpIntegerInt# s d i >=# 0#
    (J# s1 d1) <=  (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) <=# 0#

    (S# i)     >   (S# j)     = i ># j
    (J# s d)   >   (S# i)     = cmpIntegerInt# s d i ># 0#
    (S# i)     >   (J# s d)   = cmpIntegerInt# s d i <# 0#
    (J# s1 d1) >   (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) ># 0#

    (S# i)     <   (S# j)     = i <# j
    (J# s d)   <   (S# i)     = cmpIntegerInt# s d i <# 0#
    (S# i)     <   (J# s d)   = cmpIntegerInt# s d i ># 0#
    (J# s1 d1) <   (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) <# 0#

    (S# i)     >=  (S# j)     = i >=# j
    (J# s d)   >=  (S# i)     = cmpIntegerInt# s d i >=# 0#
    (S# i)     >=  (J# s d)   = cmpIntegerInt# s d i <=# 0#
    (J# s1 d1) >=  (J# s2 d2) = (cmpInteger# s1 d1 s2 d2) >=# 0#

    compare (S# i)  (S# j)
       | i ==# j = EQ
       | i <=# j = LT
       | otherwise = GT
    compare (J# s d) (S# i)
       = case cmpIntegerInt# s d i of { res# ->
	 if res# <# 0# then LT else 
	 if res# ># 0# then GT else EQ
	 }
    compare (S# i) (J# s d)
       = case cmpIntegerInt# s d i of { res# ->
	 if res# ># 0# then LT else 
	 if res# <# 0# then GT else EQ
	 }
    compare (J# s1 d1) (J# s2 d2)
       = case cmpInteger# s1 d1 s2 d2 of { res# ->
	 if res# <# 0# then LT else 
	 if res# ># 0# then GT else EQ
	 }

toBig (S# i) = case int2Integer# i of { (# s, d #) -> J# s d }
toBig i@(J# s d) = i

instance  Num Integer  where
    (+) i1@(S# i) i2@(S# j)
	= case addIntC# i j of { (# r, c #) ->
	  if c ==# 0# then S# r
	  else toBig i1 + toBig i2 }
    (+) i1@(J# s d) i2@(S# i)	= i1 + toBig i2
    (+) i1@(S# i) i2@(J# s d)	= toBig i1 + i2
    (+) (J# s1 d1) (J# s2 d2)
      = case plusInteger# s1 d1 s2 d2 of (# s, d #) -> J# s d

    (-) i1@(S# i) i2@(S# j)
	= case subIntC# i j of { (# r, c #) ->
	  if c ==# 0# then S# r
	  else toBig i1 - toBig i2 }
    (-) i1@(J# s d) i2@(S# i)	= i1 - toBig i2
    (-) i1@(S# i) i2@(J# s d)	= toBig i1 - i2
    (-) (J# s1 d1) (J# s2 d2)
      = case minusInteger# s1 d1 s2 d2 of (# s, d #) -> J# s d

    (*) i1@(S# i) i2@(S# j)
	= case mulIntC# i j of { (# r, c #) ->
	  if c ==# 0# then S# r
	  else toBig i1 * toBig i2 }
    (*) i1@(J# s d) i2@(S# i)	= i1 * toBig i2
    (*) i1@(S# i) i2@(J# s d)	= toBig i1 * i2
    (*) (J# s1 d1) (J# s2 d2)
      = case timesInteger# s1 d1 s2 d2 of (# s, d #) -> J# s d

    negate i@(S# (-2147483648#)) = 2147483648
    negate (S# i) = S# (negateInt# i)
    negate (J# s d) = J# (negateInt# s) d

    -- ORIG: abs n = if n >= 0 then n else -n

    abs (S# i) = case abs (I# i) of I# j -> S# j
    abs n@(J# s d)
      = if (cmpIntegerInt# s d 0#) >=# 0#
	then n
	else J# (negateInt# s) d

    signum (S# i) = case signum (I# i) of I# j -> S# j
    signum (J# s d)
      = let
	    cmp = cmpIntegerInt# s d 0#
	in
	if      cmp >#  0# then S# 1#
	else if cmp ==# 0# then S# 0#
	else			S# (negateInt# 1#)

    fromInteger	x	=  x

    fromInt (I# i)	=  S# i

instance  Real Integer  where
    toRational x	=  x % 1

instance  Integral Integer where
	-- ToDo:  a `rem`  b returns a small integer if b is small,
	--	  a `quot` b returns a small integer if a is small.
    quotRem (S# i) (S# j)         
      = case quotRem (I# i) (I# j) of ( I# i, I# j ) -> ( S# i, S# j) 
    quotRem i1@(J# s d) i2@(S# i) = quotRem i1 (toBig i2)
    quotRem i1@(S# i) i2@(J# s d) = quotRem (toBig i1) i2
    quotRem (J# s1 d1) (J# s2 d2)
      = case (quotRemInteger# s1 d1 s2 d2) of
	  (# s3, d3, s4, d4 #)
	    -> (J# s3 d3, J# s4 d4)

{- USING THE UNDERLYING "GMP" CODE IS DUBIOUS FOR NOW:

    divMod (J# a1 s1 d1) (J# a2 s2 d2)
      = case (divModInteger# a1 s1 d1 a2 s2 d2) of
	  Return2GMPs a3 s3 d3 a4 s4 d4
	    -> (J# a3 s3 d3, J# a4 s4 d4)
-}
    toInteger n	     = n
    toInt (S# i)     = I# i
    toInt (J# s d)   = case (integer2Int# s d) of { n# -> I# n# }

    -- the rest are identical to the report default methods;
    -- you get slightly better code if you let the compiler
    -- see them right here:
    (S# n) `quot` (S# d) = S# (n `quotInt#` d)
    n `quot` d	=  if d /= 0 then q else 
		     error "Prelude.Integral.quot{Integer}: divide by 0"  
		   where (q,_) = quotRem n d

    (S# n) `rem` (S# d) = S# (n `remInt#` d)
    n `rem` d	=  if d /= 0 then r else 
		     error "Prelude.Integral.rem{Integer}: divide by 0"  
		   where (_,r) = quotRem n d

    n `div` d	=  q  where (q,_) = divMod n d
    n `mod` d	=  r  where (_,r) = divMod n d

    divMod n d 	=  case (quotRem n d) of { qr@(q,r) ->
		   if signum r == negate (signum d) then (q - 1, r+d) else qr }
		   -- Case-ified by WDP 94/10

------------------------------------------------------------------------
instance  Enum Integer  where
    succ x		 = x + 1
    pred x		 = x - 1
    toEnum n		 =  toInteger n
    fromEnum n		 =  toInt n

    {-# INLINE enumFrom #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromTo #-}
    {-# INLINE enumFromThenTo #-}
    enumFrom x             = build (\c n -> enumDeltaIntegerFB 	 c   x 1)
    enumFromThen x y       = build (\c n -> enumDeltaIntegerFB 	 c   x (y-x))
    enumFromTo x lim	   = build (\c n -> enumDeltaToIntegerFB c n x 1     lim)
    enumFromThenTo x y lim = build (\c n -> enumDeltaToIntegerFB c n x (y-x) lim)

enumDeltaIntegerFB :: (Integer -> b -> b) -> Integer -> Integer -> b
enumDeltaIntegerFB c x d = x `c` enumDeltaIntegerFB c (x+d) d

enumDeltaIntegerList :: Integer -> Integer -> [Integer]
enumDeltaIntegerList x d = x : enumDeltaIntegerList (x+d) d

enumDeltaToIntegerFB c n x delta lim
  | delta >= 0 = up_fb c n x delta lim
  | otherwise  = dn_fb c n x delta lim

enumDeltaToIntegerList x delta lim
  | delta >= 0 = up_list x delta lim
  | otherwise  = dn_list x delta lim

up_fb c n x delta lim = go (x::Integer)
		      where
			go x | x > lim   = n
			     | otherwise = x `c` go (x+delta)
dn_fb c n x delta lim = go (x::Integer)
		      where
			go x | x < lim   = n
			     | otherwise = x `c` go (x+delta)

up_list x delta lim = go (x::Integer)
		    where
			go x | x > lim   = []
			     | otherwise = x : go (x+delta)
dn_list x delta lim = go (x::Integer)
		    where
			go x | x < lim   = []
			     | otherwise = x : go (x+delta)

{-# RULES
"enumDeltaInteger" 	enumDeltaIntegerFB   (:)    = enumDeltaIntegerList
"enumDeltaToInteger" 	enumDeltaToIntegerFB (:) [] = enumDeltaToIntegerList
 #-}

------------------------------------------------------------------------

instance  Show Integer  where
    showsPrec   x = showSignedInteger x
    showList = showList__ (showsPrec 0) 


showSignedInteger :: Int -> Integer -> ShowS
showSignedInteger p n r
  | n < 0 && p > 6 = '(':jtos n (')':r)
  | otherwise      = jtos n r

jtos :: Integer -> String -> String
jtos i rs
 | i < 0     = '-' : jtos' (-i) rs
 | otherwise = jtos' i rs
 where
  jtos' :: Integer -> String -> String
  jtos' n cs
   | n < 10    = chr (fromInteger n + (ord_0::Int)) : cs
   | otherwise = jtos' q (chr (toInt r + (ord_0::Int)) : cs)
    where
     (q,r) = n `quotRem` 10

ord_0 :: Num a => a
ord_0 = fromInt (ord '0')
\end{code}

%*********************************************************
%*							*
\subsection{The @Ratio@ and @Rational@ types}
%*							*
%*********************************************************

\begin{code}
data  (Integral a)	=> Ratio a = !a :% !a  deriving (Eq)
type  Rational		=  Ratio Integer

{-# SPECIALISE (%) :: Integer -> Integer -> Rational #-}
(%)			:: (Integral a) => a -> a -> Ratio a
numerator, denominator	:: (Integral a) => Ratio a -> a
\end{code}

\tr{reduce} is a subsidiary function used only in this module .
It normalises a ratio by dividing both numerator and denominator by
their greatest common divisor.

\begin{code}
reduce ::  (Integral a) => a -> a -> Ratio a
reduce _ 0		=  error "Ratio.%: zero denominator"
reduce x y		=  (x `quot` d) :% (y `quot` d)
			   where d = gcd x y
\end{code}

\begin{code}
x % y			=  reduce (x * signum y) (abs y)

numerator   (x :% _)	=  x
denominator (_ :% y)	=  y

\end{code}

%*********************************************************
%*							*
\subsection{Overloaded numeric functions}
%*							*
%*********************************************************

\begin{code}

{-# SPECIALISE subtract :: Int -> Int -> Int #-}
subtract	:: (Num a) => a -> a -> a
subtract x y	=  y - x

even, odd	:: (Integral a) => a -> Bool
even n		=  n `rem` 2 == 0
odd		=  not . even

{-# SPECIALISE gcd ::
	Int -> Int -> Int,
	Integer -> Integer -> Integer #-}
gcd		:: (Integral a) => a -> a -> a
gcd 0 0		=  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' a 0  =  a
			 gcd' a b  =  gcd' b (a `rem` b)

{-# SPECIALISE lcm ::
	Int -> Int -> Int,
	Integer -> Integer -> Integer #-}
lcm		:: (Integral a) => a -> a -> a
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `quot` (gcd x y)) * y)

{-# SPECIALISE (^) ::
	Integer -> Integer -> Integer,
	Integer -> Int -> Integer,
	Int -> Int -> Int #-}
(^)		:: (Num a, Integral b) => a -> b -> a
_ ^ 0		=  1
x ^ n | n > 0	=  f x (n-1) x
		   where f _ 0 y = y
		         f a d y = g a d  where
			           g b i | even i  = g (b*b) (i `quot` 2)
				         | otherwise = f b (i-1) (b*y)
_ ^ _		= error "Prelude.^: negative exponent"

{- SPECIALISE (^^) ::
	Double -> Int -> Double,
	Rational -> Int -> Rational #-}
(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip (x^(negate n))
\end{code}

