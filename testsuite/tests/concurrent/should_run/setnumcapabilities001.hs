import GHC.Conc
import Control.Parallel
import Control.Parallel.Strategies
import System.Environment
import System.IO
import Control.Monad
import Text.Printf
import Data.Time.Clock

main = do
  [n,q,t] <- fmap (fmap read) getArgs
  forkIO $ do
    forM_ (cycle ([n,n-1..1] ++ [2..n-1])) $ \m -> do
      setNumCapabilities m
      threadDelay t
  printf "%d" (nqueens q)

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
