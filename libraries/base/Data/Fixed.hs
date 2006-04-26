{-# OPTIONS -Wall -Werror -fno-warn-unused-binds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fixed
-- Copyright   :  (c) Ashley Yakeley 2005, 2006
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  Ashley Yakeley <ashley@semantic.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a "Fixed" type for fixed-precision arithmetic.
-- The parameter to Fixed is any type that's an instance of HasResolution.
-- HasResolution has a single method that gives the resolution of the Fixed type.
-- Parameter types E6 and E12 (for 10^6 and 10^12) are defined, as well as
-- type synonyms for Fixed E6 and Fixed E12.
--
-- This module also contains generalisations of div, mod, and divmod to work
-- with any Real instance.
--
-----------------------------------------------------------------------------

module Data.Fixed
(
	div',mod',divMod',

	Fixed,HasResolution(..),
	showFixed,
	E6,Micro,
	E12,Pico
) where

-- | generalisation of 'div' to any instance of Real
div' :: (Real a,Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))

-- | generalisation of 'divMod' to any instance of Real
divMod' :: (Real a,Integral b) => a -> a -> (b,a)
divMod' n d = (f,n - (fromIntegral f) * d) where
	f = div' n d

-- | generalisation of 'mod' to any instance of Real
mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where
	f = div' n d

newtype Fixed a = MkFixed Integer deriving (Eq,Ord)

class HasResolution a where
	resolution :: a -> Integer

fixedResolution :: (HasResolution a) => Fixed a -> Integer
fixedResolution fa = resolution (uf fa) where
	uf :: Fixed a -> a
	uf _ = undefined

withType :: (a -> f a) -> f a
withType foo = foo undefined

withResolution :: (HasResolution a) => (Integer -> f a) -> f a
withResolution foo = withType (foo . resolution)

instance Enum (Fixed a) where
	succ (MkFixed a) = MkFixed (succ a)
	pred (MkFixed a) = MkFixed (pred a)
	toEnum = MkFixed . toEnum
	fromEnum (MkFixed a) = fromEnum a
	enumFrom (MkFixed a) = fmap MkFixed (enumFrom a)
	enumFromThen (MkFixed a) (MkFixed b) = fmap MkFixed (enumFromThen a b)
	enumFromTo (MkFixed a) (MkFixed b) = fmap MkFixed (enumFromTo a b)
	enumFromThenTo (MkFixed a) (MkFixed b) (MkFixed c) = fmap MkFixed (enumFromThenTo a b c)

instance (HasResolution a) => Num (Fixed a) where
	(MkFixed a) + (MkFixed b) = MkFixed (a + b)
	(MkFixed a) - (MkFixed b) = MkFixed (a - b)
	fa@(MkFixed a) * (MkFixed b) = MkFixed (div (a * b) (fixedResolution fa))
	negate (MkFixed a) = MkFixed (negate a)
	abs (MkFixed a) = MkFixed (abs a)
	signum (MkFixed a) = fromInteger (signum a)
	fromInteger i = withResolution (\res -> MkFixed (i * res))

instance (HasResolution a) => Real (Fixed a) where
	toRational fa@(MkFixed a) = (toRational a) / (toRational (fixedResolution fa))

instance (HasResolution a) => Fractional (Fixed a) where
	fa@(MkFixed a) / (MkFixed b) = MkFixed (div (a * (fixedResolution fa)) b)
	recip fa@(MkFixed a) = MkFixed (div (res * res) a) where
		res = fixedResolution fa
	fromRational r = withResolution (\res -> MkFixed (floor (r * (toRational res))))

instance (HasResolution a) => RealFrac (Fixed a) where
	properFraction a = (i,a - (fromIntegral i)) where
		i = truncate a
	truncate f = truncate (toRational f)
	round f = round (toRational f)
	ceiling f = ceiling (toRational f)
	floor f = floor (toRational f)

chopZeros :: Integer -> String
chopZeros 0 = ""
chopZeros a | mod a 10 == 0 = chopZeros (div a 10)
chopZeros a = show a

-- only works for positive a
showIntegerZeros :: Bool -> Int -> Integer -> String
showIntegerZeros True _ 0 = ""
showIntegerZeros chopTrailingZeros digits a = replicate (digits - length s) '0' ++ s' where
	s = show a
	s' = if chopTrailingZeros then chopZeros a else s

withDot :: String -> String
withDot "" = ""
withDot s = '.':s

-- | First arg is whether to chop off trailing zeros
showFixed :: (HasResolution a) => Bool -> Fixed a -> String
showFixed chopTrailingZeros fa@(MkFixed a) | a < 0 = "-" ++ (showFixed chopTrailingZeros (asTypeOf (MkFixed (negate a)) fa))
showFixed chopTrailingZeros fa@(MkFixed a) = (show i) ++ (withDot (showIntegerZeros chopTrailingZeros digits fracNum)) where
	res = fixedResolution fa
	(i,d) = divMod a res
	-- enough digits to be unambiguous
	digits = ceiling (logBase 10 (fromInteger res) :: Double)
	maxnum = 10 ^ digits
	fracNum = div (d * maxnum) res

instance (HasResolution a) => Show (Fixed a) where
	show = showFixed False



data E6 = E6

instance HasResolution E6 where
	resolution _ = 1000000

type Micro = Fixed E6


data E12 = E12

instance HasResolution E12 where
	resolution _ = 1000000000000

type Pico = Fixed E12
