-- !!! Checking that you can hide a constructor
module ShouldCompile where

import Rn037Help hiding( C )
	-- C is the constructor, but we should
	-- still be able to hide it

f x = Rn037Help.C
