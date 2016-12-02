import Control.Concurrent
import Control.Exception
import System.Posix

main = do
  pid <- forkProcess $ do
           handle (\UserInterrupt{} -> putStrLn "caught")
                  $ threadDelay 2000000
  signalProcess sigINT pid
  threadDelay 2000000
