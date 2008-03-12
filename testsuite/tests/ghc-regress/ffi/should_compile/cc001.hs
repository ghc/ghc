-- !!! cc001 -- ccall with standard boxed arguments and results

module ShouldCompile where

-- simple functions

foreign import ccall unsafe "a" a :: IO Int

foreign import ccall unsafe "b" b :: Int -> IO Int

foreign import ccall unsafe "c" 
  c :: Int -> Char -> Float -> Double -> IO Float

-- simple monadic code

d =	a		>>= \ x ->
	b x		>>= \ y ->
	c y 'f' 1.0 2.0


	
