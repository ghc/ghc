module PreludeCore (
	Integer(..),
	__integer0,	-- These names must match those in PrelVals.hs
	__integer1,
	__integer2,
	__integerm1
    ) where

import Cls
import Core
import IInt
import IRatio		( (%) )
import ITup2		-- instances
import List		( (++), foldr, takeWhile )
import Prel		( not, otherwise, (&&) )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
import TyComplex

------------------------------------------------------
-- useful constants

__integer0, __integer1, __integer2, __integerm1 :: Integer

__integer0  = fromInt 0
__integer1  = fromInt 1
__integer2  = fromInt 2
__integerm1 = fromInt (-1)

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
      = case __integer0 of { J# a2 s2 d2 ->
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
      = case __integer0 of { J# a2 s2 d2 ->
	let
	    cmp = cmpInteger# a1 s1 d1 a2 s2 d2
	in
	if      cmp >#  0# then __integer1
	else if cmp ==# 0# then __integer0
	else			__integerm1
	}

    fromInteger	x	=  x

    fromInt (I# n#)	=  int2Integer# n# -- gives back a full-blown Integer

instance  Real Integer  where
    toRational x	=  x :% __integer1

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
		   if signum r == - signum d then (q - __integer1, r+d) else qr }
		   -- Case-ified by WDP 94/10

    even x = (==) (rem x __integer2) __integer0
    odd x  = (/=) (rem x __integer2) __integer0

instance  Ix Integer  where
    range (m,n)		=  [m..n]
    index b@(m,n) i
	| inRange b i	=  fromInteger (i - m)
	| otherwise	=  error ("Ix.Integer.index{PreludeCore}: Index "
				  ++ show i ++ " outside the range "
				  ++ show b ++ ".\n")
    inRange (m,n) i	=  m <= i && i <= n

instance  Enum Integer  where
    enumFrom n           =  n : enumFrom (n + __integer1)
    enumFromThen m n     =  en' m (n - m)
	                    where en' m n = m : en' (m + n) n
    enumFromTo n m       =  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n m p =  takeWhile (if m >= n then (<= p) else (>= p))
				      (enumFromThen n m)

instance  Text Integer  where
    readsPrec p x = readSigned readDec x
    showsPrec   x = showSigned showInt x
    readList = _readList (readsPrec 0)
    showList = _showList (showsPrec 0) 
