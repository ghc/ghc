{-	This module tests that we can ge polymorphic recursion
	of overloaded functions.  GHC 2.02 produced the following
	bogus error:

	tmp.lhs:1: A group of type signatures have mismatched contexts
		       Abf.a ::  (PrelBase.Ord f{-aX6-}) => ...
		       Abf.b ::  (PrelBase.Ord f{-aX2-}) => ... 

	This was due to having more than one type signature for one
	group of recursive functions.
-}


module ShouldSucceed where

a :: (Ord f) => f 
a = b

b :: (Ord f) => f 
b = a 


