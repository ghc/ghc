--!! Basic pipe usage
module Main(main) where

import Posix

main = do
  str <- getEffectiveUserName
  putStrLn str
  (rd, wd) <- createPipe
  n <- forkProcess
  case n of
    Nothing  -> do
       (str,_) <- fdRead rd 32
       -- avoid them zombies
       putStrLn str
    Just pid -> do
       fdWrite wd "Hi, there - forked child calling" 
--       getProcessStatus False True pid
       return ()
