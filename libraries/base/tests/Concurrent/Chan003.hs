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
         threadDelay 3000
         throwTo wid ThreadKilled
         putStr "."
         writeChan chan (3::Int)
