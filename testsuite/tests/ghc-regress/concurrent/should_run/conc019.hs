import IO
import Control.Concurrent
import Control.Exception

main = do
  forkIO (Control.Exception.catch (do { m <- newEmptyMVar; takeMVar m })
		          (\e -> putStrLn ("caught: " ++ show e)))
  let x = sum [1..10000]
  x `seq` print x
