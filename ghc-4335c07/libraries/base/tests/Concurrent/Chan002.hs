import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO
import System.Environment

-- test for deadlocks
main = do
  hSetBuffering stdout NoBuffering
  [n] <- getArgs
  replicateM_ (read n) $ do
         chan <- newChan
         wid <- forkIO $ forever $ writeChan chan (5::Int)
         rid <- forkIO $ forever $ void $ readChan chan
         threadDelay 1000
         throwTo rid ThreadKilled
         putStr "."
         readChan chan
         throwTo wid ThreadKilled
