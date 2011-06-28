import Control.Concurrent
import Control.Exception

-- check that we can still kill a thread that is blocked on
-- delivering an exception to us.

-- NB. this test is non-deterministic in the threaded2 way since 6.14,
-- because throwTo is now always interruptible, so the main thread's
-- killThread can be legitimately interrupted by the child thread's
-- killThread, rather than the other way around.  This happens because
-- the child thread is running on another processor, so the main
-- thread's throwTo is blocked waiting for a response, and while
-- waiting it is interruptible.

main = do
  main_thread <- myThreadId
  m <- newEmptyMVar
  sub_thread <- forkIO (do
	    	 	 takeMVar m
	    	 	 throwTo main_thread (ErrorCall "foo")
	 		)
  mask_ $ do
    putMVar m ()
    sum [1..10000] `seq` -- to be sure the other thread is now blocked
      killThread sub_thread

  putStrLn "ok"
