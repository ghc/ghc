
This module implements a (good) random number generator.

The June 1988 (v31 #6) issue of the Communications of the ACM has an
article by Pierre L'Ecuyer called, "Efficient and Portable Combined
Random Number Generators".  Here is the Portable Combined Generator of
L'Ecuyer for 32-bit computers.  It has a period of roughly 2.30584e18.

Transliterator: Lennart Augustsson

\begin{code}
module Random
	(
	 random, 
	 randomIO
        ) where

import CPUTime (getCPUTime)
import Time (getClockTime, ClockTime(..))

randomIO :: (Integer, Integer) -> IO [Integer]
randomIO lh = do
    ct          <- getCPUTime
    (TOD sec _) <- getClockTime
    return (random lh (toInteger sec * 12345 + ct))

random :: (Integer, Integer) -> Integer -> [Integer]
random (l, h) s =
    if l > h then error "Random.random: Empty interval" else
    if s < 0 then random (l, h) (-s) else
	let (q, s1) = s `divMod` 2147483562
	    s2 = q `mod` 2147483398
            k = h-l + 1
            b = 2147483561
            n = iLogBase b k
	    f is = let (xs, is') = splitAt n is
                   in  foldr (\ i r -> fromInt i + r * b) 0 xs `mod` k + l : f is'
	in  f (randomInts (toInt (s1+1)) (toInt (s2+1)))

iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)

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
	
\end{code}
