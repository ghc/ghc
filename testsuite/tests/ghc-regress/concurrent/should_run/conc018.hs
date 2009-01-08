import Control.Concurrent
import Control.Exception
import GHC.Conc

-- test that putMVar blocks on a full MVar rather than raising an
-- exception.

main = do
  m <- newEmptyMVar
  t <- forkIO (
	    Control.Exception.catch (do
		m <- newMVar ()
		putMVar m ()
	     )
	     (\e -> putMVar m (e::SomeException))
	   )
  takeMVar m >>= print
  -- should print "thread blocked indefinitely"
