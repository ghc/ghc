import GHC.Conc.Sync
import Control.Monad
import System.Environment

test n = do
  dog <- newTVarIO False
  cat <- newTVarIO False
  let unset = do
        d <- readTVar dog
        c <- readTVar cat
        if (d || c) then retry else return ()
      setDog = unset >> writeTVar dog True
      setCat = unset >> writeTVar cat True
      reset = do
        d <- readTVar dog
        c <- readTVar cat
        guard (d || c)
        writeTVar dog False
        writeTVar cat False
  
  replicateM_ n (do
    forkIO (atomically setDog)
    forkIO (atomically setCat)
    atomically reset
    atomically reset)

main = do
  [n] <- getArgs
  test (read n :: Int)
