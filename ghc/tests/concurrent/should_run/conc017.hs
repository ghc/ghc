import Concurrent
import Exception

-- check that async exceptions are restored to their previous
-- state after an exception is raised and handled.

main = do
  main_thread <- myThreadId
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  m3 <- newEmptyMVar
  forkIO (do 
	     takeMVar m1
	     raiseInThread main_thread (ErrorCall "foo")
	     takeMVar m2
	     raiseInThread main_thread (ErrorCall "bar")
	     putMVar m3 ()
	 )
  (do 
    blockAsyncExceptions (do
	(do putMVar m1 () 
	    unblockAsyncExceptions (
		-- unblocked, "foo" delivered to "caught1"
	       threadDelay 100000
	     )
	 ) `catchAllIO` (\e -> putStrLn ("caught1: " ++ show e))
	putMVar m2 ()
	-- blocked here, "bar" can't be delivered
	(threadDelay 100000)
	  `catchAllIO` (\e -> putStrLn ("caught2: " ++ show e))
     )
    -- unblocked here, "bar" delivered to "caught3"
    takeMVar m3
   ) 
   `catchAllIO` (\e -> putStrLn ("caught3: " ++ show e))
