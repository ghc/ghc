-- Test for bug #1047

import Control.Concurrent
import Control.Exception

-- This loop spends most of its time printing stuff, and very occasionally
-- pops outside 'block'.  This test ensures that an thread trying to
-- throwTo this thread will eventually succeed.
loop = mask_ (print "alive") >> loop

main = do tid <- forkIO loop
          threadDelay 1
          killThread tid
