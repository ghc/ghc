import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import System.Mem
import System.IO.Unsafe

main :: IO ()
main = do
  forM_ [1 .. 1000] $ \r -> do
    -- Create a thunk that will never finish evaluating
    let t = unsafePerformIO (threadDelay (100000000000 + r))
    -- Begin nonmoving GC
    performMajorGC
    performMajorGC
    x <- newEmptyMVar
    y <- newEmptyMVar
    -- we start evaluating it on two threads in parallel
    k <- forkIO (putMVar x () >> void (evaluate t))
    performMajorGC
    -- The second thread gets added to the waking queue
    _ <- forkIO (putMVar y () >> void (evaluate t))
    takeMVar x
    takeMVar y
    -- We kill the original thread.
    -- And now wake up the other threads.
    -- We need to ensure that the nonmoving GC can see that thread.
    -- Otherwise we will get a NonTermination exception
    killThread k
    performMajorGC
    performMajorGC
  putStrLn "done"
