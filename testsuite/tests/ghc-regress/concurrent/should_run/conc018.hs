import Control.Concurrent
import Control.Exception
import GHC.Conc

-- test that putMVar blocks on a full MVar rather than raising an
-- exception.

main = do
	    Control.Exception.catch (do
		m2 <- newMVar ()
		putMVar m2 ()
	     )
	     (\e -> print (e::SomeException))
  -- should print "thread blocked indefinitely"
