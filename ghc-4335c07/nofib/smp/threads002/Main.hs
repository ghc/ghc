module Main where

-- a variant of thread001, this one creates N threads as fast as
-- possible.  The threads all signal a single QSemN, which the
-- main thread waits for.

-- If we are unlucky, the program can take a *long* time.  This is
-- because if a thread yields while holding the semaphore, it will
-- prevent all other threads from finishing, and we get into a
-- situation where there are a lot of blocked threads, and the number
-- of threads being created outnumbers those being retired.  The run
-- queue has two threads at any one time: the main thread, busy
-- creating new threads, and a single thread that has been unblocked.
-- Each pass over the run queue creates a bunch of new threads which
-- will all immediately block, and unblocks a single thread.  Having
-- two processors helps, because it means we can unblock threads more
-- quickly.

import Control.Concurrent
import System.Environment

main :: IO ()
main = do
   [s] <- getArgs
   let n =  read s :: Int

   sem <- newQSemN 0

   let 
   	spawner :: (IO () -> IO ThreadId) -> Int -> IO ()
   	spawner c 0 = return ()
   	spawner c n = do { c (signalQSemN sem 1); spawner c (n-1); }

   spawner forkIO n

   waitQSemN sem n
