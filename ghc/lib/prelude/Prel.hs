-- Standard value bindings

module Prelude (
	-- NB: not the "real" Prelude.hi export list
	($),
	(&&),
	(.),
	(^),
	(^^),
	appendBin,
	asTypeOf,
	atan2,
	chr,
	const,
	flip,
	fromIntegral,
	fromRealFrac,
	fst,
	gcd,
	id,
	isAlpha,
	isAlphanum,
	isAscii,
	isControl,
	isDigit,
	isLower,
	isNullBin,
	isPrint,
	isSpace,
	isUpper,
	lcm,
	maxChar,
	maxInt,
	minChar,
	minInt,
	not,
	nullBin,
	ord,
	otherwise,
	snd,
	subtract,
	toLower,
	toUpper,
	until,
	(||)

#if defined(__UNBOXED_INSTANCES__)
	, minInt#, maxInt#
	, minChar#, maxChar#
	, toChar#, fromChar#
	, isAscii#, isControl#, isPrint#, isSpace#
	, isUpper#, isLower#, isAlpha#, isDigit#, isAlphanum#
	, toUpper#, toLower#
#endif
    ) where

import UTypes		( Bin ) -- so we don't get any data constructors!

import Cls
import Core
import TyComplex
import IChar
import IComplex
import IDouble
import IFloat
import IInt
import IInteger
import IList
import IRatio
import List		( (++) )
import PS		( _PackedString, _unpackPS )
import Text

--infixr 9  .
--infixr 8  ^, ^^
--infixr 3  &&
--infixr 2  ||
--infixr 0  $

---------------------------------------------------------------
-- Binary functions
---------------------------------------------------------------

nullBin	    	    	:: Bin
isNullBin    	    	:: Bin -> Bool
appendBin		:: Bin -> Bin -> Bin

-- *
nullBin			= error "nullBin{Prelude}\n"
isNullBin		= error "isNullBin{Prelude}\n"
appendBin		= error "appendBin{Prelude}\n"

---------------------------------------------------------------
-- Boolean functions
---------------------------------------------------------------

{-# INLINE (&&) #-}
{-# INLINE (||) #-}
(&&), (||)		:: Bool -> Bool -> Bool
True  && x		=  x
False && _		=  False
True  || _		=  True
False || x		=  x

{-# INLINE not #-} 
not			:: Bool -> Bool
not True		=  False
not False		=  True

{-# INLINE otherwise #-}
otherwise		:: Bool
otherwise 		=  True

---------------------------------------------------------------
-- Int functions
---------------------------------------------------------------

minInt, maxInt	:: Int
minInt		=  -2147483647	-- **********************
maxInt		=  2147483647	-- **********************

---------------------------------------------------------------
-- Char functions
---------------------------------------------------------------

minChar, maxChar	:: Char
minChar			= '\0'
maxChar			= '\255'

ord			:: Char -> Int
ord c 			=  case c of { C# ch -> I# (ord# ch) }

chr 			:: Int -> Char
chr i			=  case i of { I# ih -> C# (chr# ih) }

isAscii, isControl, isPrint, isSpace		:: Char -> Bool
isUpper, isLower, isAlpha, isDigit, isAlphanum	:: Char -> Bool

isAscii c	 	=  ord c < 128
isControl c		=  c < ' ' || c == '\DEL'
isPrint c		=  c >= ' ' && c <= '~'
isSpace c		=  c == ' ' || c == '\t' || c == '\n' || 
			   c == '\r' || c == '\f' || c == '\v'
isUpper c		=  c >= 'A' && c <= 'Z'
isLower c		=  c >= 'a' && c <= 'z'
isAlpha c		=  isUpper c || isLower c
isDigit c		=  c >= '0' && c <= '9'
isAlphanum c		=  isAlpha c || isDigit c


toUpper, toLower	:: Char -> Char
toUpper c | isLower c	= chr ((ord c - ord 'a') + ord 'A')
	  | otherwise	= c

toLower c | isUpper c	= chr ((ord c - ord 'A') + ord 'a')
	  | otherwise	= c

#if defined(__UNBOXED_INSTANCES__)
---------------------------------------------------------------
-- Int# functions
---------------------------------------------------------------

-- ToDo: Preferable to overload minInt and maxInt
--       minInt, maxInt	:: Num a => a
--       Solution: place in class Num (as pi is in Floating)

minInt#, maxInt#	:: Int#
minInt#			=  -2147483647#
maxInt#			=  2147483647#

---------------------------------------------------------------
-- Char# functions -- ToDo: class Chr ???
---------------------------------------------------------------

toChar# 	:: Char  -> Char#
toChar# (C# c#) = c#

fromChar# 	:: Char# -> Char
fromChar# c#	= C# c#

-- ord# and chr# are builtin

minChar#, maxChar#	:: Char#
minChar#		= '\0'#
maxChar#		= '\255'#

isAscii#, isControl#, isPrint#, isSpace#		:: Char# -> Bool
isUpper#, isLower#, isAlpha#, isDigit#, isAlphanum#	:: Char# -> Bool

isAscii# c	 	=  ord# c < 128#
isControl# c		=  c < ' '# || c == '\DEL'#
isPrint# c		=  c >= ' '# && c <= '~'#
isSpace# c		=  c == ' '# || c == '\t'# || c == '\n'# || 
			   c == '\r'# || c == '\f'# || c == '\v'#
isUpper# c		=  c >= 'A'# && c <= 'Z'#
isLower# c		=  c >= 'a'# && c <= 'z'#
isAlpha# c		=  isUpper# c || isLower# c
isDigit# c		=  c >= '0'# && c <= '9'#
isAlphanum# c		=  isAlpha# c || isDigit# c


toUpper#, toLower#	:: Char# -> Char#
toUpper# c | isLower# c	= chr# ((ord# c - ord# 'a'#) + ord# 'A'#)
	   | otherwise	= c

toLower# c | isUpper# c	= chr# ((ord# c - ord# 'A'#) + ord# 'a'#)
	   | otherwise	= c

#endif {-UNBOXED INSTANCES-}

---------------------------------------------------------------
-- Numeric functions
---------------------------------------------------------------

--{-# GENERATE_SPECS subtract a{Int#,Double#} #-}
{-# GENERATE_SPECS subtract a{~,Int,Double} #-}
subtract	:: (Num a) => a -> a -> a
#ifdef USE_REPORT_PRELUDE
subtract	=  flip (-)
#else
subtract x y	=  y - x
#endif /* ! USE_REPORT_PRELUDE */

--{-# GENERATE_SPECS gcd a{Int#,Int,Integer} #-}
{-# GENERATE_SPECS gcd a{~,Int,Integer} #-}
gcd		:: (Integral a) => a -> a -> a
gcd 0 0		=  error "gcd{Prelude}: gcd 0 0 is undefined\n"
gcd x y		=  gcd' (abs x) (abs y)
		   where gcd' x 0  =  x
			 gcd' x y  =  gcd' y (x `rem` y)

--{-# GENERATE_SPECS lcm a{Int#,Int,Integer} #-}
{-# GENERATE_SPECS lcm a{~,Int,Integer} #-}
lcm		:: (Integral a) => a -> a -> a
lcm _ 0		=  0
lcm 0 _		=  0
lcm x y		=  abs ((x `quot` (gcd x y)) * y)

--{-# GENERATE_SPECS (^) a{~,Int#,Double#,Int,Integer,Double,Complex(Double#),Complex(Double)} b{~,Int#,Int} #-}
{-# GENERATE_SPECS (^) a{~,Int,Integer,Double,Rational,Complex(Double)} b{~,Int} #-}
(^)		:: (Num a, Integral b) => a -> b -> a
x ^ 0		=  1
x ^ (n+1)	=  f x n x
		   where f _ 0 y = y
		         f x n y = g x n  where
			           g x n | odd n = f x (n-1) (x*y)
				         | otherwise  = g (x*x) (n `div` 2)
_ ^ _		= error "(^){Prelude}: negative exponent\n"

--{-# GENERATE_SPECS (^^) a{~,Double#,Double,Complex(Double#),Complex(Double)} b{~,Int#,Int} #-}
{-# GENERATE_SPECS (^^) a{~,Double,Rational} b{~,Int} #-}
(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip (x^(-n))

--{-# GENERATE_SPECS atan2 a{Double#,Double} #-}
{-# GENERATE_SPECS atan2 a{~,Double} #-}
atan2		:: (RealFloat a) => a -> a -> a
#if USE_REPORT_PRELUDE
atan2 y x	=  case (signum y, signum x) of
			( 0, 1) ->  0
			( 1, 0) ->  pi/2
			( 0,-1) ->  pi
			(-1, 0) -> -pi/2
			( _, 1) ->  atan (y/x)
			( _,-1) ->  atan (y/x) + pi
			( 0, 0) ->  error "atan2{Prelude}: atan2 of origin\n"
#else {- steal Lennart's version -}
atan2 y x =
	if y == 0 then
	         if x > 0 then 0
	    else if x < 0 then pi
	    else {- x == 0 -}  error "Prelude.atan2: atan2 of origin"
	else if x == 0 then
	    if y > 0 then pi/2
	    else {- y < 0 -} -pi/2
	else if x > 0 then
	    atan (y/x)		-- 1st and 4th quadrant
	else {- x < 0 -}
	    if y > 0 then
	        atan (y/x) + pi	-- 2nd quadrant
	    else {- y < 0 -}
	        atan (y/x) - pi	-- 3rd quadrant
#endif

---------------------------------------------------------------
-- Some standard functions:
---------------------------------------------------------------

-- component projections for pairs:
--{-# GENERATE_SPECS fst a b #-}
fst			:: (a,b) -> a
fst (x,y)		=  x

--{-# GENERATE_SPECS snd a b #-}
snd			:: (a,b) -> b
snd (x,y)		=  y

-- identity function
--{-# GENERATE_SPECS id a #-}
id			:: a -> a
id x			=  x

-- constant function
--{-# GENERATE_SPECS const a b #-}
const			:: a -> b -> a
const x _		=  x

-- function composition
{-# INLINE (.) #-}
--{-# GENERATE_SPECS (.) a b c #-}
(.)			:: (b -> c) -> (a -> b) -> a -> c
(f . g) x		=  f (g x)

-- flip f  takes its (first) two arguments in the reverse order of f.
--{-# GENERATE_SPECS flip a b c #-}
flip			:: (a -> b -> c) -> b -> a -> c
flip f x y		=  f y x

-- right-associating infix application operator (useful in continuation-
-- passing style)
--{-# GENERATE_SPECS ($) a b #-}
($)			:: (a -> b) -> a -> b
f $ x			=  f x

-- until p f  yields the result of applying f until p holds.
--{-# GENERATE_SPECS until a #-}
until			:: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x	=  x
	    | otherwise =  until p f (f x)

-- asTypeOf is a type-restricted version of const.  It is usually used
-- as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
--{-# GENERATE_SPECS asTypeOf a #-}
asTypeOf		:: a -> a -> a
asTypeOf		=  const

---------------------------------------------------------------
-- fromIntegral and fromRealFrac with explicit specialisations
---------------------------------------------------------------

{- LATER:
{-# SPECIALIZE fromIntegral ::
    Int# 	-> Int#		= id,
    Int#	-> Double#	= int2Double#,
    Int#	-> Int		= i2I#,
    Int#	-> Integer	= int2Integer#,
    Int#	-> Double	= i2D#,
    Int		-> Int#		= i2i,
    Int		-> Double#	= i2d,
    Int  	-> Int		= id,
    Int 	-> Integer	= i2Integer,
    Int		-> Double	= i2D,
    Integer	-> Int#		= integer2i,
    Integer	-> Double#	= integer2d,
    Integer  	-> Int		= integer2I,
    Integer 	-> Integer  	= id,
    Integer	-> Double	= integer2D 	#-}
-}

{-# SPECIALIZE fromIntegral ::
    Int  	-> Int		= id,
    Int 	-> Integer	= i2Integer,
    Int		-> Double	= i2D,
    Integer  	-> Int		= integer2I,
    Integer 	-> Integer  	= id,
    Integer	-> Double	= integer2D 	#-}

i2I# i# = I# i#
i2D# i# = D# (int2Double# i#)

i2i (I# i#) = i#
i2d (I# i#) = int2Double# i#
i2D (I# i#) = D# (int2Double# i#)
i2Integer (I# i#) = int2Integer# i#

integer2i (J# a# s# d#) = integer2Int# a# s# d#
integer2d (J# a# s# d#) = encodeDouble# a# s# d# 0#
integer2I (J# a# s# d#) = I# (integer2Int# a# s# d#)
integer2F (J# a# s# d#) = F# (encodeFloat# a# s# d# 0#)
integer2D (J# a# s# d#) = D# (encodeDouble# a# s# d# 0#)

fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger

{- LATER:
{-# SPECIALIZE fromRealFrac ::
    Double# 	-> Double#	= id,
    Double#	-> Double	= d2D#,
    Double 	-> Double#	= d2d,
    Double	-> Double	= id #-}
-}

{-# SPECIALIZE fromRealFrac ::
    Float	-> Float	= id,
    Float	-> Double	= f2D,
    Double	-> Float	= d2F,
    Double	-> Double	= id #-}

d2F# d# = F# (double2Float# d#)
d2D# d# = D# d#

f2d (F# f#) = float2Double# f#
f2D (F# f#) = D# (float2Double# f#)

d2d (D# d#) = d#
d2F (D# d#) = F# (double2Float# d#)

fromRealFrac	:: (RealFrac a, Fractional b) => a -> b
fromRealFrac	=  fromRational . toRational
