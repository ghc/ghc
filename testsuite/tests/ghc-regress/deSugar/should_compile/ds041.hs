{-# LANGUAGE DatatypeContexts #-}
{- In 2.05 this one crashed with

	Fail: "basicTypes/Id.lhs", line 990: incomplete pattern(s) 
		to match in function "dataConFieldLabels"

   Reason: dsExpr (RecordCon ...) didn't extract 
	   the constructor properly.
-}

module ShouldCompile where

data Eq a => Foo a = Foo { x :: a }

foo :: Eq a => Foo a
foo = Foo{}

