-- !!! Test ccalls with large numbers of arguments

-- In 0.19, we lost the ability to do ccalls with more than 6 arguments
-- on the Sparc.  Just to make sure it never happens again...

import Foreign.C

main = 
  withCString "Testing %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n" $ \cstr -> 
  printf cstr
	 0  1  2  3  4  5  6  7  8  9
	10 11 12 13 14 15 16 17 18 19
	20 21 22 23 24 25 26 27 28 29
	30 31 32 33 34 35 36 37 38 39

foreign import ccall unsafe 
  printf :: CString
	 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
	 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
	 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
	 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
	 -> IO ()
