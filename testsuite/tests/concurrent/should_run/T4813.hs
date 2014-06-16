import Control.Concurrent
import Control.Monad
import Control.Exception
import System.Mem

-- caused an assertion failure with -debug in 7.0.1 (#4813)

main = do
   m <- newEmptyMVar
   ts <- replicateM 100 $ mask_ $ forkIO $ threadDelay 100000; putMVar m ()
   mapM_ killThread (reverse (init ts))
   takeMVar m
