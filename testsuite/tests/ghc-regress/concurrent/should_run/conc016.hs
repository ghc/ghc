import Control.Concurrent
import Control.Exception

-- check that we can still kill a thread that is blocked on
-- delivering an exception to us.
main = do
  main_thread <- myThreadId
  m <- newEmptyMVar
  sub_thread <- forkIO (do
	    	 	 takeMVar m
	    	 	 throwTo main_thread (ErrorCall "foo")
	 		)
  block (do
    putMVar m ()
    sum [1..10000] `seq` -- to be sure the other thread is now blocked
      killThread sub_thread
   )
  putStrLn "ok"
