import IO
import Concurrent
import Exception

main = do
  forkIO (Exception.catch (do { m <- newEmptyMVar; takeMVar m })
		          (\e -> putStrLn ("caught: " ++ show e)))
  let x = sum [1..10000]
  x `seq` print x
