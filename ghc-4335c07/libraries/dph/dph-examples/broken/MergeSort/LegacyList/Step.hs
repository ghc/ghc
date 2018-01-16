
module LegacyList.Step (sort) where
	
-- | Merge sort using lists.
sort :: Ord a => [a] -> [a] 
sort xx
 = case xx of
	[]	-> []
	[x]	-> [x]
	xx	-> let (xs, ys)	= splitAt (length xx `div` 2) xx
		   in  merge (sort xs) (sort ys)


-- | This straightforward merge algorithm doesn't parallelise well
--   because we have to compare each pair of elements before
--   moving onto the rest of the list.
merge :: Ord a => [a] -> [a] -> [a]
merge xs []		= xs
merge [] ys		= ys
merge (x:xs) (y:ys)
	| x <= y	= x : merge xs (y:ys)
	| otherwise	= y : merge (x:xs) ys
