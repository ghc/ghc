import Concurrent
import Exception

-- check that we can still kill a thread that is blocked on
-- delivering an exception to us.
main = do
  main_thread <- myThreadId
  m <- newEmptyMVar
  sub_thread <- forkIO (do
	    	 	 takeMVar m
	    	 	 raiseInThread main_thread (ErrorCall "foo")
	 		)
  blockAsyncExceptions (do
    putMVar m ()
    threadDelay 500000 -- to be sure the other thread is now blocked
    killThread sub_thread
   )
  putStrLn "ok"
