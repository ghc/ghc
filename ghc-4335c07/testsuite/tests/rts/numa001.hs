import System.Environment
import Control.Monad
import Control.Concurrent

main = do
  [n] <- map read <$> getArgs
  mvars <- replicateM n newEmptyMVar
  sequence_ [ forkIO $ putMVar m $! nsoln n
            | (m,n) <- zip mvars (repeat 9) ]
  mapM_ takeMVar mvars

nsoln nq = length (gen nq)
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: Int -> [[Int]]
    gen 0 = [[]]
    gen n = [ (q:b) | b <- gen (n-1), q <- [1..nq], safe q 1 b]
