import Control.Concurrent
import Control.Exception
import Data.List
import System.Mem

-- !!! test that a child thread waiting on its own MVar will get killed by
-- a signal.

main = do
  forkIO (Control.Exception.catch (do { m <- newEmptyMVar; takeMVar m })
		$ \e -> putStrLn ("caught: " ++ show (e::SomeException)))
  threadDelay 10000
  System.Mem.performGC
  threadDelay 10000
