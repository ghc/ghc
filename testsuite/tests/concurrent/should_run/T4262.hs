{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

-- Tests that superfluous worker threads are discarded rather than
-- being kept around by the RTS.

import Control.Concurrent
import Control.Monad
import Foreign.C.Types
import System.Mem
import System.Posix.Process
import System.Directory
import Control.Concurrent.QSem

foreign import ccall safe sleep :: CUInt -> IO ()

main = do
    let amount = 200
    qsem <- newQSem 0
    replicateM_ amount . forkIO $ (sleep 2 >> signalQSem qsem)
    replicateM_ amount $ waitQSem qsem
    -- POSIX only: check thread usage manually
    pid <- getProcessID
    let dir = "/proc/" ++ show pid ++ "/task"
    contents <- getDirectoryContents dir
    let status = length contents - 2 -- . and ..
    print status
