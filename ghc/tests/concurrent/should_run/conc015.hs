import Concurrent
import Exception

-- test blocking & unblocking of async exceptions.

-- the first exception "foo" should be caught by the "caught1" handler,
-- since async exceptions are blocked outside this handler.

-- the second exception "bar" should be caught by the outer "caught2" handler,
-- (i.e. this tests that async exceptions are properly unblocked after
-- being blocked).

main = do
  main_thread <- myThreadId
  m <- newEmptyMVar
  m2 <- newEmptyMVar
  forkIO (do takeMVar m
	     raiseInThread main_thread (ErrorCall "foo")
	     raiseInThread main_thread (ErrorCall "bar") 
	     putMVar m2 ()
	 )
  ( do
    blockAsyncExceptions (do
	putMVar m ()
	sum [1..10000] `seq` -- give 'foo' a chance to be raised
  	  (unblockAsyncExceptions (threadDelay 500000))
		`catchAllIO` (\e -> putStrLn ("caught1: " ++ show e))
     )
    takeMVar m2
   )
    `catchAllIO`
    (\e -> putStrLn ("caught2: " ++ show e))
