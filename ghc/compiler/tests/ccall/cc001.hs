--!!! cc001 -- ccall with standard boxed arguments and results

module Test where

import PreludeGlaIO

-- simple functions

a :: PrimIO Int
a = _ccall_ a

b :: Int -> PrimIO Int
b x = _ccall_ b x

c :: Int -> Char -> Float -> Double -> PrimIO Float
c x1 x2 x3 x4 = _ccall_ c x1 x2 x3 x4

-- simple monadic code

d =	a		`thenPrimIO` \ x ->
	b x		`thenPrimIO` \ y ->
	c y 'f' 1.0 2.0


	
