{-# OPTIONS_GHC -O2 #-}
import System.IO
import System.Environment
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

{-
This test creates a number of threads in blocked mode, and then calls
killThread on each one in turn.  Since the threads are in blocked
mode, none of the killThreads can complete until the target thread has
exited, so this tests thread creation/completion as well as throwTo
blocking/unblocking performance.

On a 1.86GHz Intel Xeon, with GHC 6.10.1 -threaded

./Main 300000 +RTS -s 
     338,144,560 bytes allocated in the heap
   1,232,944,856 bytes copied during GC
     307,446,192 bytes maximum residency (9 sample(s))
     109,796,160 bytes maximum slop
             786 MB total memory in use (12 MB lost due to fragmentation)

  Generation 0:   640 collections,     0 parallel,  2.42s,  2.44s elapsed
  Generation 1:     9 collections,     0 parallel,  0.63s,  1.22s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    7.64s  (  7.75s elapsed)
  GC    time    3.05s  (  3.65s elapsed)
  EXIT  time    0.04s  (  0.04s elapsed)
  Total time   10.73s  ( 11.44s elapsed)

HEAD 7/1/2009 + patch to use HpLim for context-switching:

./Main 300000 +RTS -s 
     354,865,480 bytes allocated in the heap
   1,229,475,576 bytes copied during GC
     306,480,832 bytes maximum residency (9 sample(s))
     109,806,448 bytes maximum slop
             780 MB total memory in use (13 MB lost due to fragmentation)

  Generation 0:   643 collections,     0 parallel,  2.53s,  2.56s elapsed
  Generation 1:     9 collections,     0 parallel,  0.66s,  1.27s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    1.06s  (  1.06s elapsed)
  GC    time    3.19s  (  3.83s elapsed)
  EXIT  time    0.04s  (  0.04s elapsed)
  Total time    4.28s  (  4.93s elapsed)

(probably this is mostly due to not context-switching after forkIO)

-}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [nthreads] <- fmap (map read) getArgs
    tids <- replicateM nthreads . mask $ \_ -> forkIO $ return ()
    m <- newEmptyMVar
    -- do it in a subthread to avoid bound-thread overhead
    forkIO $ do mapM_ killThread tids; putMVar m ()
    takeMVar m
    return ()
