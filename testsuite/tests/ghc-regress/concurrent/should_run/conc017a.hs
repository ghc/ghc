import Control.Concurrent
import Control.Exception

-- check that async exceptions are restored to their previous
-- state after an exception is raised and handled.

main = do
  main_thread <- myThreadId
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  m3 <- newEmptyMVar
  forkIO (do 
	     takeMVar m1
	     throwTo main_thread (ErrorCall "foo")
	     takeMVar m2
	     throwTo main_thread (ErrorCall "bar")
	     putMVar m3 ()
	 )
  (do 
    mask $ \restore -> do
	(do putMVar m1 () 
	    restore (
		-- unblocked, "foo" delivered to "caught1"
	       myDelay 100000
	     )
	 ) `Control.Exception.catch` 
              \e -> putStrLn ("caught1: " ++ show (e::SomeException))
	putMVar m2 ()
	-- blocked here, "bar" can't be delivered
	(sum [1..10000] `seq` return ())
	  `Control.Exception.catch` 
              \e -> putStrLn ("caught2: " ++ show (e::SomeException))
    -- unblocked here, "bar" delivered to "caught3"
    takeMVar m3
   )
   `Control.Exception.catch` 
       \e -> putStrLn ("caught3: " ++ show (e::SomeException))

-- compensate for the fact that threadDelay is non-interruptible
-- on Windows with the threaded RTS in 6.6.
myDelay usec = do
  m <- newEmptyMVar
  forkIO $ do threadDelay usec; putMVar m ()
  takeMVar m
