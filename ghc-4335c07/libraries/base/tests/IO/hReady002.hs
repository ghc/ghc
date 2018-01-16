-- test for bug #4078
import System.IO
import Control.Concurrent
import System.Exit

main = do
  m <- newEmptyMVar
  forkIO $ do threadDelay 500000; putMVar m Nothing
  forkIO $ do hReady stdin >>= putMVar m . Just
  takeMVar m >>= print
