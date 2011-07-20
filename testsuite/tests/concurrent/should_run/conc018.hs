import Control.Concurrent
import Control.Exception
import GHC.Conc
import Foreign

-- test that putMVar blocks on a full MVar rather than raising an
-- exception.

main = do
     -- In this test we want a thread to get BlockedIndefinitely; that
     -- can't be the main thread because in GHCi the main thread
     -- doesn't get BlockedIndefinitely.  So we have to use a
     -- subthread, and "prevent* the main thread from getting
     -- BlockedIndefinitely when we're not in GHCi, which is what the
     -- following hack does:
  myThreadId >>= newStablePtr

  m <- newEmptyMVar
  t <- forkIO $ do
	    Control.Exception.catch (do
		m <- newMVar ()
		putMVar m ()
	     )
	     (\e -> putMVar m (e::SomeException))
  takeMVar m >>= print
  -- should print "thread blocked indefinitely"
