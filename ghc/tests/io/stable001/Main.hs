-- This is a rather exciting experiment in using the new call
-- makeStablePtr# and performIO. It doesn't do much but it took an
-- incredible effort to get it to do it.

import Stable
import GlaExts
import CString

-- module Main(main) where

main =	makeStablePtr test	>>= \ stablePtr ->
	((_casm_GC_ ``SaveAllStgRegs(); test1(%0); RestoreAllStgRegs();'' stablePtr)
						:: PrimIO ())
				>>= \ _ ->
	return ()

test :: IO Int
test =
	let f x = sum [1..x]
	    f :: Int -> Int
	in 
	_ccall_ printf
	      (packString "The stable pointer has just been used to print this number %d\n") (f 100)
				>>= \ _ ->
	return 5

