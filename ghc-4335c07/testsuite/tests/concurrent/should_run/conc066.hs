-- Test for bug #1047

import Control.Concurrent
import Control.Exception

-- This loop spends most of its time printing stuff, and very occasionally
-- executes 'unblock (return ())'.  This test ensures that a thread waiting
-- to throwTo this thread is not blocked indefinitely.
loop restore = do restore (return ()); print "alive"; loop restore

main = do tid <- forkIO (mask $ \restore -> loop restore)
          yield
          killThread tid
