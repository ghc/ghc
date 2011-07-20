{-# LANGUAGE ImplicitParams #-}

-- !!! Desugaring of record updates
-- Showed up a bug in the newtype-squashing machinery


module ShouldCompile where

data R = R {field :: Int}

test:: (?param :: R) => a -> Int
test x = field (?param {field = 42})
	-- The type of the record to be updated is 
	-- {?param :: R} as well as plain R
	-- which confused the compiler

