-- !!! Checking that you can hide a constructor
module ShouldCompile where

import Rn037Help hiding( C )
	-- C is the constructor, but we should
	-- still be able to hide it

-- we should still be able to refer to the type constructor, though
type Foo = T
