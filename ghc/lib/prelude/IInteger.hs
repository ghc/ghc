module PreludeCore (
	Integer(..),
	int2Integer,
	_integer_0, _integer_1, _integer_m1
    ) where

import Cls
import Core
import IInt
import IRatio		( (%) )
import ITup2		-- instances
import List		( (++), foldr )
import Prel		( not, otherwise, (&&) )
import PS		( _PackedString, _unpackPS )
import Text

------------------------------------------------------
-- a magical Integer-ish function that
-- the compiler inserts references to

int2Integer :: Int -> Integer
int2Integer (I# i#) = int2Integer# i#

------------------------------------------------------
-- some *very* heavily-used constants

_integer_0, _integer_1, _integer_m1 :: Integer
_integer_0  = 0
_integer_1  = 1
_integer_m1 = (-1)

------------------------------------------------------

instance  Eq Integer  where
    (J# a1 s1 d1) == (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) ==# 0#

    (J# a1 s1 d1) /= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) /=# 0#

instance  Ord Integer  where
    (J# a1 s1 d1) <= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) <=# 0#

    (J# a1 s1 d1) <  (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) <# 0#

    (J# a1 s1 d1) >= (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) >=# 0#

    (J# a1 s1 d1) >  (J# a2 s2 d2)
      = (cmpInteger# a1 s1 d1 a2 s2 d2) ># 0#

    x@(J# a1 s1 d1) `max` y@(J# a2 s2 d2)
      = if ((cmpInteger# a1 s1 d1 a2 s2 d2) ># 0#) then x else y

    x@(J# a1 s1 d1) `min` y@(J# a2 s2 d2)
      = if ((cmpInteger# a1 s1 d1 a2 s2 d2) <# 0#) then x else y

    _tagCmp (J# a1 s1 d1) (J# a2 s2 d2)
       = case cmpInteger# a1 s1 d1 a2 s2 d2 of { res# ->
	 if res# <# 0# then _LT else 
	 if res# ># 0# then _GT else _EQ
	 }

instance  Num Integer  where
    (+) (J# a1 s1 d1) (J# a2 s2 d2)
      = plusInteger# a1 s1 d1 a2 s2 d2

    (-) (J# a1 s1 d1) (J# a2 s2 d2)
      = minusInteger# a1 s1 d1 a2 s2 d2

    negate (J# a s d) = negateInteger# a s d

    (*) (J# a1 s1 d1) (J# a2 s2 d2)
      = timesInteger# a1 s1 d1 a2 s2 d2

    -- ORIG: abs n = if n >= 0 then n else -n

    abs n@(J# a1 s1 d1)
      = case _integer_0 of { J# a2 s2 d2 ->
	if (cmpInteger# a1 s1 d1 a2 s2 d2) >=# 0#
	then n
	else negateInteger# a1 s1 d1
	}

    {- ORIG:
    signum n | n <  0 	= -1
	     | n == 0 	= 0
	     | otherwise= 1
    -}

    signum n@(J# a1 s1 d1)
      = case _integer_0	of { J# a2 s2 d2 ->
	let
	    cmp = cmpInteger# a1 s1 d1 a2 s2 d2
	in
	if      cmp >#  0# then _integer_1
	else if cmp ==# 0# then _integer_0
	else			_integer_m1
	}

    fromInteger	x	=  x

    fromInt (I# n#)	=  int2Integer# n# -- gives back a full-blown Integer

instance  Real Integer  where
    toRational x	=  x % 1

instance  Integral Integer where
    quotRem (J# a1 s1 d1) (J# a2 s2 d2)
      = case (quotRemInteger# a1 s1 d1 a2 s2 d2) of
	  _Return2GMPs a3 s3 d3 a4 s4 d4
	    -> (J# a3 s3 d3, J# a4 s4 d4)

{- USING THE UNDERLYING "GMP" CODE IS DUBIOUS FOR NOW:

    divMod (J# a1 s1 d1) (J# a2 s2 d2)
      = case (divModInteger# a1 s1 d1 a2 s2 d2) of
	  _Return2GMPs a3 s3 d3 a4 s4 d4
	    -> (J# a3 s3 d3, J# a4 s4 d4)
-}
    toInteger n	     = n
    toInt (J# a s d) = case (integer2Int# a s d) of { n# -> I# n# }

    -- the rest are identical to the report default methods;
    -- you get slightly better code if you let the compiler
    -- see them right here:
    n `quot` d	=  q  where (q,r) = quotRem n d
    n `rem` d	=  r  where (q,r) = quotRem n d
    n `div` d	=  q  where (q,r) = divMod n d
    n `mod` d	=  r  where (q,r) = divMod n d

    divMod n d 	=  case (quotRem n d) of { qr@(q,r) ->
		   if signum r == - signum d then (q-1, r+d) else qr }
		   -- Case-ified by WDP 94/10

    even x = (==) (rem x 2) 0
    odd x  = (/=) (rem x 2) 0

instance  Ix Integer  where
    range (m,n)		=  [m..n]
    index b@(m,n) i
	| inRange b i	=  fromInteger (i - m)
	| otherwise	=  error ("Ix.Integer.index{PreludeCore}: Index "
				  ++ show i ++ " outside the range "
				  ++ show b ++ ".\n")
    inRange (m,n) i	=  m <= i && i <= n

instance  Enum Integer  where
{- RAW PRELUDE ************************
    enumFrom		=  numericEnumFrom
    enumFromThen	=  numericEnumFromThen
-}
    enumFrom n = n : enumFrom (n + 1)
    enumFromThen m n = en' m (n - m)
	    where en' m n = m : en' (m + n) n


instance  Text Integer  where
    readsPrec p x = readSigned readDec x
    showsPrec   x = showSigned showInt x
