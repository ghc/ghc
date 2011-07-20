-- The classic 8-queens problem made famous by Wirth.
-- This version Colin Runciman, March 2000.

main =
    if null solutions then putStrLn "no solution!"
    else putStr (board (head solutions))
    where
    solutions = queens 8

queens :: Int -> [[Int]]
queens n = valid n n 

valid :: Int -> Int -> [[Int]]
valid 0 n = [[]]
valid m n = filter safe (extend n (valid (m-1) n)) 

extend n b = cp (fromTo 1 n) b 

cp :: [a] -> [[a]] -> [[a]]
cp [] y = []
cp (a:x) y = map (a:) y ++ cp x y 

safe (a:b) = no_threat a b 1

no_threat a [] m = True
no_threat a (b:y) m =
    a /= b && a+m /= b && a-m /= b && no_threat a y (m+1) 

board :: [Int] -> String 
board b =
    unlines (concat (zipWith rank (from 1) b))
  where
    rank r qcol =
        map line ["o o o", " \\|/ ", " === "]
      where
        line crown_slice =
	    concat (zipWith square (from 1) b)
          where
	    square scol _ =
		if scol == qcol then crown_slice
	 	else if (scol `rem` (2::Int)) == (r `rem` (2::Int)) then "....."
		else "     "

-- in place of ..

from :: Int -> [Int]
from n = n : from (n+1)

fromTo :: Int -> Int -> [Int]
fromTo m n = if m > n then [] else m : fromTo (m+1) n
