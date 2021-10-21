import Control.Concurrent
import Control.Exception

-- Test blocking of async exceptions in an exception handler.
-- The exception raised in the main thread should not be delivered
-- until the first exception handler finishes.
main = do
  main_thread <- myThreadId
  m <- newEmptyMVar
  forkIO (do { takeMVar m;  throwTo main_thread (ErrorCall "foo") })
  (do { throwIO (ErrorCall "wibble")
          `Control.Exception.catch`
            (\e -> let _ = e::ErrorCall in
                   do putMVar m (); evaluate (sum [1..10000]); putStrLn "done.")
       ; myDelay 500000 })
    `Control.Exception.catch`
       \e -> putStrLn ("caught: " ++ show (e::SomeExceptionWithLocation))

-- compensate for the fact that threadDelay is non-interruptible
-- on Windows with the threaded RTS in 6.6.
myDelay usec = do
  m <- newEmptyMVar
  forkIO $ do threadDelay usec; putMVar m ()
  takeMVar m
