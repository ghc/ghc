import Concurrent
import Exception

-- !!! Try to get two threads into a knot depending on each other.

-- This should result in the main thread being sent a NonTermination
-- exception (in GHC 5.02, the program is terminated with "no threads
-- to run" instead).

main = do
  let a = sum [1..10000] + b
      b = sum [2..10000] + a
  forkIO (print a)
  r <- Exception.try $ print b
  print r
