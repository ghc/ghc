module Main(main) where

-- In 0.19, we lost the ability to do ccalls with more than 6 arguments
-- on the Sparc.  Just to make sure it never happens again...

import Data.PackedString

main = 
	_ccall_ printf (packString "Testing %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n")
	(01::Int) (02::Int) (03::Int) (04::Int) (05::Int) (06::Int) (07::Int) (08::Int)
	(11::Int) (12::Int) (13::Int) (14::Int) (15::Int) (16::Int) (17::Int) (18::Int)
	(21::Int) (22::Int) (23::Int) (24::Int) (25::Int) (26::Int) (27::Int) (28::Int)
	(31::Int) (32::Int) (33::Int) (34::Int) (35::Int) (36::Int) (37::Int) (38::Int)
	>>
	return ()
