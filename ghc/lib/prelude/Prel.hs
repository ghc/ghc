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
	(||),

	minInt#, maxInt#,
	toInt#, fromInt#,
	minChar#, maxChar#,
	toChar#, fromChar#,
	isAscii#, isControl#, isPrint#, isSpace#,
	isUpper#, isLower#, isAlpha#, isDigit#, isAlphanum#,
	toUpper#, toLower#

    ) where

import UTypes		( Bin ) -- so we don't get any data constructors!

import Cls
import Core
import TyArray
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

---------------------------------------------------------------
-- Int# functions
---------------------------------------------------------------

toInt# 		:: Int  -> Int#
toInt# (I# i#) 	= i#

fromInt# 	:: Int# -> Int
fromInt# i#	= I# i#

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
minChar#	= '\0'#
maxChar#	= '\255'#

isAscii#, isControl#, isPrint#, isSpace#		:: Char# -> Bool
isUpper#, isLower#, isAlpha#, isDigit#, isAlphanum#	:: Char# -> Bool

isAscii# c	=  ord# c `ltInt#` 128#
isControl# c	=  c `ltChar#` ' '# || c `eqChar#` '\DEL'#
isPrint# c	=  c `geChar#` ' '# && c `leChar#` '~'#
isSpace# c	=  c `eqChar#` ' '# || c `eqChar#` '\t'# || c `eqChar#` '\n'# || 
		   c `eqChar#` '\r'# || c `eqChar#` '\f'# || c `eqChar#` '\v'#
isUpper# c	=  c `geChar#` 'A'# && c `leChar#` 'Z'#
isLower# c	=  c `geChar#` 'a'# && c `leChar#` 'z'#
isAlpha# c	=  isUpper# c || isLower# c
isDigit# c	=  c `geChar#` '0'# && c `leChar#` '9'#
isAlphanum# c	=  isAlpha# c || isDigit# c


toUpper#, toLower#	:: Char# -> Char#
toUpper# c | isLower# c	= chr# ((ord# c `minusInt#` ord# 'a'#) `plusInt#` ord# 'A'#)
	   | otherwise	= c
toLower# c | isUpper# c	= chr# ((ord# c `minusInt#` ord# 'A'#) `plusInt#` ord# 'a'#)
	   | otherwise	= c

---------------------------------------------------------------
-- Numeric functions
---------------------------------------------------------------

{-# GENERATE_SPECS subtract a{Int#,Double#,Int,Double,Complex(Double#),Complex(Double)} #-}
subtract	:: (Num a) => a -> a -> a
#ifdef USE_REPORT_PRELUDE
subtract	=  flip (-)
#else
subtract x y	=  y - x
#endif /* ! USE_REPORT_PRELUDE */

{-# GENERATE_SPECS gcd a{Int#,Int,Integer} #-}
gcd		:: (Integral a) => a -> a -> a
gcd x y | x == __i0 && y == __i0
	=  error "gcd{Prelude}: gcd 0 0 is undefined\n"
	| otherwise
	=  gcd' (abs x) (abs y)
		   where gcd' x y | y == __i0
				  =  x
			          | otherwise
				  =  gcd' y (x `rem` y)

{-# GENERATE_SPECS lcm a{Int#,Int,Integer} #-}
lcm		:: (Integral a) => a -> a -> a
lcm x y | y == __i0
	= __i0
	| x == __i0
	= __i0
	| otherwise
	= abs ((x `quot` (gcd x y)) * y)

{-# SPECIALIZE (^) :: Integer -> Integer -> Integer #-}
{-# GENERATE_SPECS (^) a{~,Int#,Double#,Int,Integer,Double,Rational,Complex(Double#),Complex(Double)} b{~,Int#,Int} #-}
(^)		:: (Num a, Integral b) => a -> b -> a
x ^ n | n == __i0
      = __i1
      | n > __i0
      = f x (n - __i1) x
      | otherwise
      = error "(^){Prelude}: negative exponent\n"
  where
    f x n y | n == __i0
	    = y
	    | otherwise
	    = g x n y
    g x n y | odd n
	    = f x (n - __i1) (x*y)
	    | otherwise
	    = g (x*x) (n `div` __i2) y

{-# GENERATE_SPECS (^^) a{~,Double#,Double,Rational,Complex(Double#),Complex(Double)} b{~,Int#,Int} #-}
(^^)		:: (Fractional a, Integral b) => a -> b -> a
x ^^ n		=  if n >= 0 then x^n else recip (x^(-n))

{-# GENERATE_SPECS atan2 a{Double#,Double} #-}
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
{-# GENERATE_SPECS fst a b #-}
fst			:: (a,b) -> a
fst (x,y)		=  x

{-# GENERATE_SPECS snd a b #-}
snd			:: (a,b) -> b
snd (x,y)		=  y

-- identity function
{-# GENERATE_SPECS id a #-}
id			:: a -> a
id x			=  x

-- constant function
{-# GENERATE_SPECS const a b #-}
const			:: a -> b -> a
const x _		=  x

-- function composition
{-# INLINE (.) #-}
{-# GENERATE_SPECS (.) a b c #-}
(.)			:: (b -> c) -> (a -> b) -> a -> c
(f . g) x		=  f (g x)

-- flip f  takes its (first) two arguments in the reverse order of f.
{-# GENERATE_SPECS flip a b c #-}
flip			:: (a -> b -> c) -> b -> a -> c
flip f x y		=  f y x

-- right-associating infix application operator (useful in continuation-
-- passing style)
{-# GENERATE_SPECS ($) a b #-}
($)			:: (a -> b) -> a -> b
f $ x			=  f x

-- until p f  yields the result of applying f until p holds.
{-# GENERATE_SPECS until a #-}
until			:: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x	=  x
	    | otherwise =  until p f (f x)

-- asTypeOf is a type-restricted version of const.  It is usually used
-- as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
{-# GENERATE_SPECS asTypeOf a #-}
asTypeOf		:: a -> a -> a
asTypeOf		=  const

---------------------------------------------------------------
-- fromIntegral and fromRealFrac with explicit specialisations
---------------------------------------------------------------

{-# SPECIALIZE fromIntegral ::
    Int		-> Rational,
    Integer	-> Rational,
    Int  	-> Int		= id,
    Int 	-> Integer	= i2Integer,
    Int		-> Float	= i2F,
    Int		-> Double	= i2D,
    Integer  	-> Int		= integer2I,
    Integer 	-> Integer  	= id,
    Integer	-> Float	= integer2F,
    Integer	-> Double	= integer2D #-}

#if defined(__UNBOXED_INSTANCES__)
{-# SPECIALIZE fromIntegral ::
    Int#	-> Rational,
    Int# 	-> Int#		= id,
    Int#	-> Double#	= i2d#,
    Int#	-> Int		= i2I#,
    Int#	-> Integer	= i2Integer#,
    Int#	-> Float	= i2F#,
    Int#	-> Double	= i2D#,
    Int		-> Int#		= i2i,
    Int		-> Double#	= i2d,
    Integer	-> Int#		= integer2i,
    Integer	-> Double#	= integer2d #-}
#endif

i2d# i# = int2Double# i#
i2I# i# = I# i#
i2Integer# i# = int2Integer# i#
i2F# i# = F# (int2Float# i#)
i2D# i# = D# (int2Double# i#)

i2i (I# i#) = i#
i2d (I# i#) = int2Double# i#
i2Integer (I# i#) = int2Integer# i#
i2F (I# i#) = F# (int2Float# i#)
i2D (I# i#) = D# (int2Double# i#)

integer2i (J# a# s# d#) = integer2Int# a# s# d#
integer2d (J# a# s# d#) = encodeDouble# a# s# d# 0#
integer2I (J# a# s# d#) = I# (integer2Int# a# s# d#)
integer2F (J# a# s# d#) = F# (encodeFloat# a# s# d# 0#)
integer2D (J# a# s# d#) = D# (encodeDouble# a# s# d# 0#)

fromIntegral	:: (Integral a, Num b) => a -> b
fromIntegral	=  fromInteger . toInteger

{-# SPECIALIZE fromRealFrac ::
    Double	-> Rational, 
    Rational	-> Double,
    Float	-> Rational,
    Rational	-> Float,
    Rational	-> Rational	= id,
    Double	-> Double	= id,
    Double	-> Float	= d2F,
    Float	-> Float	= id,
    Float	-> Double	= f2D #-}

#if defined(__UNBOXED_INSTANCES__)
{-# SPECIALIZE fromRealFrac ::
    Double#	-> Rational,
    Rational	-> Double#,
    Double# 	-> Double#	= id,
    Double#	-> Float	= d2F#,
    Double#	-> Double	= d2D#,
    Double 	-> Double#	= d2d,
    Float	-> Double#	= f2d #-}
#endif

d2F# d# = F# (double2Float# d#)
d2D# d# = D# d#

f2d (F# f#) = float2Double# f#
f2D (F# f#) = D# (float2Double# f#)

d2d (D# d#) = d#
d2F (D# d#) = F# (double2Float# d#)

fromRealFrac	:: (RealFrac a, Fractional b) => a -> b
fromRealFrac	=  fromRational . toRational
