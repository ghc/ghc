{-# LANGUAGE ImplicitParams #-}

module ShouldCompile where

data UniqueSupply = US Integer

newUnique :: (?uniqueSupply :: UniqueSupply) => Integer
newUnique = r
    where US r = ?uniqueSupply
	-- The lazy pattern match in the where clause killed GHC 5.04
	-- because the type {?uniqueSupply::UniqueSupply} of the RHS
	-- of the 'where' didn't look like a UniqueSupply
