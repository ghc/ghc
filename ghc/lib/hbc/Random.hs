{-
   This module implements a (good) random number generator.

   The June 1988 (v31 #6) issue of the Communications of the ACM has an
   article by Pierre L'Ecuyer called, "Efficient and Portable Combined
   Random Number Generators".  Here is the Portable Combined Generator of
   L'Ecuyer for 32-bit computers.  It has a period of roughly 2.30584e18.

   Transliterator: Lennart Augustsson
-}

module Random(randomInts, randomDoubles, normalRandomDoubles) where
-- Use seeds s1 in 1..2147483562 and s2 in 1..2147483398 to generate
-- an infinite list of random Ints.
randomInts :: Int -> Int -> [Int]
randomInts s1 s2 =
    if 1 <= s1 && s1 <= 2147483562 then
	if 1 <= s2 && s2 <= 2147483398 then
	    rands s1 s2
	else
	    error "randomInts: Bad second seed."
    else
	error "randomInts: Bad first seed."

rands :: Int -> Int -> [Int]
rands s1 s2 = z' : rands s1'' s2''
	where	z'   = if z < 1 then z + 2147483562 else z
		z    = s1'' - s2''

		k    = s1 `quot` 53668
		s1'  = 40014 * (s1 - k * 53668) - k * 12211
		s1'' = if s1' < 0 then s1' + 2147483563 else s1'
    
		k'   = s2 `quot` 52774
		s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
		s2'' = if s2' < 0 then s2' + 2147483399 else s2'
	
-- Same values for s1 and s2 as above, generates an infinite
-- list of Doubles uniformly distibuted in (0,1).
randomDoubles :: Int -> Int -> [Double]
randomDoubles s1 s2 = map (\x -> fromIntegral x * 4.6566130638969828e-10) (randomInts s1 s2)

-- The normal distribution stuff is stolen from Tim Lambert's
-- M*****a version

-- normalRandomDoubles is given two seeds and returns an infinite list of random
-- normal variates with mean 0 and variance 1.  (Box Muller method see
-- "Art of Computer Programming Vol 2")
normalRandomDoubles :: Int -> Int -> [Double]
normalRandomDoubles s1 s2 = boxMuller (map (\x->2*x-1) (randomDoubles s1 s2))

-- boxMuller takes a stream of uniform random numbers on [-1,1] and
-- returns a stream of normally distributed random numbers.
boxMuller :: [Double] -> [Double]
boxMuller (x1:x2:xs) | r <= 1    = x1*m : x2*m : rest
                     | otherwise = rest
				   where r = x1*x1 + x2*x2
				         m = sqrt(-2*log r/r)
				         rest = boxMuller xs
