-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

import System.Environment


main = do
	[arg] <- getArgs
	print $ nsoln $ read arg

nsoln nq = length (gen nq)
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: Int -> [[Int]]
    gen 0 = [[]]
    gen n = [ (q:b) | b <- gen (n-1), q <- [1..nq], safe q 1 b]
