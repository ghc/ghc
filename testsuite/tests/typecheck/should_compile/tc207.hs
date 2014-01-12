
-- Tests enhanced polymorphism

module ShouldCompile where

foo xs = let 
	   f :: Eq a => [a] -> [a]
	   f [] = []
	   f xs | null (g [True]) = [] 
		| otherwise       = tail (g xs) 

	   g :: Eq b => [b] -> [b]
	   g [] = []
	   g xs | null (f "hello") = [] 
		| otherwise       = tail (f xs) 
 	in f xs
