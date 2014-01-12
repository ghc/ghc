
import SumNatsVect (sumNats)

-- Solution for 1st Euler problem
-- Add all the natural numbers below 1000 that are multiples of 3 or 5.

solutionLists maxN
 = let	sumOnetoN n = n * (n+1) `div` 2
	sumStep s n = s * sumOnetoN (n `div` s)
   in	sumStep 3 (maxN - 1) + sumStep 5 (maxN - 1) - sumStep 15 (maxN - 1)

solutionLists2 maxN
  = sum	[ x 	| x <- [0.. maxN - 1]
		, (x `mod` 3 == 0) || (x `mod` 5 == 0) ]

main 
 = do	let n	= 1000
	print $ solutionLists  n
	print $ solutionLists2 n
	print $ sumNats n
	