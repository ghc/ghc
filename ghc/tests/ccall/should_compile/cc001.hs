-- !!! cc001 -- ccall with standard boxed arguments and results

module ShouldCompile where

-- simple functions

a :: IO Int
a = _ccall_ a

b :: Int -> IO Int
b x = _ccall_ b x

c :: Int -> Char -> Float -> Double -> IO Float
c x1 x2 x3 x4 = _ccall_ c x1 x2 x3 x4

-- simple monadic code

d =	a		>>= \ x ->
	b x		>>= \ y ->
	c y 'f' 1.0 2.0


	
