import Concurrent
import Exception

-- Test blocking of async exceptions in an exception handler.
-- The exception raised in the main thread should not be delivered
-- until the first exception handler finishes.
main = do
  main_thread <- myThreadId
  m <- newEmptyMVar
  forkIO (do { takeMVar m;  throwTo main_thread (ErrorCall "foo") })
  (error "wibble")
	`Exception.catch` (\e -> do putMVar m ()
			            sum [1..10000] `seq` putStrLn "done.")
  (threadDelay 500000)
	`Exception.catch` (\e -> putStrLn ("caught: " ++ show e))

