import Concurrent
import Exception

-- test that putMVar blocks on a full MVar rather than raising an
-- exception.

main = do
  t <- forkIO (
	    Exception.catch (do
		m <- newMVar ()
		putMVar m ()
	     )
	     (\e -> print e)
	   )
  threadDelay 500000
  killThread t
