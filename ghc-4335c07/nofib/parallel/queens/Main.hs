import Control.Parallel
import Control.Parallel.Strategies
import System.Environment

-- version of N-queens originally from nofib/imaginary/queens, parallelised
-- by Simon Marlow 03/2010.

main = do
  [n] <- fmap (fmap read) getArgs
  print (nqueens n)

nqueens :: Int -> Int
nqueens nq = length (pargen 0 [])
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: [[Int]] -> [[Int]]
    gen bs = [ (q:b) | b <- bs, q <- [1..nq], safe q 1 b ]

    pargen :: Int -> [Int] -> [[Int]]
    pargen n b
       | n >= threshold = iterate gen [b] !! (nq - n)
       | otherwise      = concat bs 
       where bs = map (pargen (n+1)) (gen [b]) `using` parList rdeepseq

    threshold = 3
