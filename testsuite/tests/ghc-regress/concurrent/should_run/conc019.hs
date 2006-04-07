import IO
import Control.Concurrent
import Control.Exception
import Data.List

-- !!! test that a child thread waiting on its own MVar will get killed by
-- a signal.

main = do
  forkIO (Control.Exception.catch (do { m <- newEmptyMVar; takeMVar m })
		          (\e -> putStrLn ("caught: " ++ show e)))
  let x = foldl' (+) 0 [1..500000]
  x `seq` print x
