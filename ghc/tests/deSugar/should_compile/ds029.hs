-- !!! ds029: pattern binding with guards (dubious but valid)
--

module ShouldCompile where

f x = y
    where (y,z) | y < z     = (0,1)
    	    	| y > z     = (1,2)
		| True	    = (2,3)
