import GHC.Conc.Sync
import System.Environment

test n = atomically $ f [1..n]
 where
  f [] = retry
  f (x:xs) = do
    ys <- f xs
    return (x:ys)

main = do
  [n] <- getArgs
  test (read n)
