-- This is nofib/smp/threads006.  It fails in GHC 7.8.2 with a GC crash.

{-# OPTIONS_GHC -O2 #-}
import System.IO
import System.Environment
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

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
