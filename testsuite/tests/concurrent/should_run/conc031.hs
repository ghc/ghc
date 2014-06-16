import Control.Concurrent
import Control.Exception
import System.Mem ( performGC )
import System.Mem.Weak ( addFinalizer )

data P = P (MVar Bool)

-- Bug reported by Manuel Chakravarty, namely that we weren't checking
-- for runnable finalizers before declaring that the program is
-- deadlocked.

main = do
--  gcThread  -- with this thread enabled, no error
  mv <- newEmptyMVar
  let p = P mv
  addFinalizer p (set p)
  takeMVar mv >>= print
  putStrLn "End."
  where
    set (P mv) = putMVar mv True
    --
    -- this is just to demonstrate that it is only about the GC timing
    --
    gcThread = forkIO $ let gc = do
				   putStrLn "delay"
				   threadDelay 100000
				   putStrLn "gc"
				   performGC
				   gc 
			in gc
