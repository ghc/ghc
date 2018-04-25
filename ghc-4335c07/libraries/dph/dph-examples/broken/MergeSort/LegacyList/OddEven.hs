{-# LANGUAGE PatternGuards #-}

module LegacyList.OddEven (sort) where
import Util	

sort :: Ord a => [a] -> [a]
sort xx
	| isPowerOfTwo $ length xx
	= sortCore xx

	| otherwise
	= error "sort: length of list is not a power of two"


-- | Batcher odd/even merge sort.
--   This only works on on lists who's lengths are powers of two.
--   The length of the list must be a power of two, else loop.
sortCore :: Ord a => [a] -> [a]
sortCore xx
 	| []	<- xx	= []
 	| [x]	<- xx	= [x]
 	| otherwise
 	= let	(first, second)	= splitAt (length xx `div` 2) xx
   	  in	mergeCore (sortCore first ++ sortCore second)


-- | Batcher odd/even merge.
--   The two lists to be merged are appended.
--   The length of the lists must be a power of two, else loop.
mergeCore :: Ord a => [a] -> [a]
mergeCore zz@[x, y]
	| y < x		= [y, x]
	| otherwise	= zz
	
mergeCore zz
 = let	evens'		= mergeCore (evens zz)
	odds'		= mergeCore (odds  zz)
	(x : rest)	= interleave evens' odds'
   in	x : flipPairs (init rest) ++ [last rest]


-- | For each consecutive pair of elements, 
--	if they are out of order then flip them so they are.
flipPairs :: Ord a => [a] -> [a]
flipPairs xx
	= concat
	$ map (\(x, y) 
		-> if y < x 
			then [y, x]
			else [x, y])
	$ zip (evens xx) (odds xx)


-- | Interleave the elements of two lists.
interleave :: [a] -> [a] -> [a]
interleave xx yy
 	= concat
	$ map (\(x, y) -> [x, y])
	$ zip xx yy

	
-- | Select elements of a list in even or odd numbered positions.
evens, odds :: [a] -> [a]
evens xx
	= map snd
	$ filter (\(ix, x) -> ix `mod` 2 == 0)
	$ zip [0..] xx
		
odds xx
 = case xx of
	[]	-> []
	[x]	-> []
	x:y:xs	-> evens (y : xs)

