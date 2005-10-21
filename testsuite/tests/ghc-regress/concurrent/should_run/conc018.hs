import Control.Concurrent
import Control.Exception

-- test that putMVar blocks on a full MVar rather than raising an
-- exception.

main = do
  t <- forkIO (
	    Control.Exception.catch (do
		m <- newMVar ()
		putMVar m ()
	     )
	     (\e -> print e)
	   )
  threadDelay 200000
