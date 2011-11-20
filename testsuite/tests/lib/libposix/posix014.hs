-- !! Basic pipe usage
module Main (main) where

import System.Posix

main = do
  (rd, wd) <- createPipe
  pid <- forkProcess $ do (str, _) <- fdRead rd 32
                          putStrLn str
  fdWrite wd "Hi, there - forked child calling"
  getProcessStatus True False pid
  return ()

