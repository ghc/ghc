-- test for bug #4078
import System.IO
import Control.Concurrent
import System.Exit

main = do
  m <- newEmptyMVar
  forkIO $ threadDelay 500000 >> putMVar m Nothing
  forkIO $ hReady stdin >>= putMVar m . Just
  takeMVar m >>= print
