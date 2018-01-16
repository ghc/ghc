{-# LANGUAGE ForeignFunctionInterface #-}
-- This benchmark is also ffi014 in the test suite.

-- This program behaves unpredictably with the non-threaded RTS,
-- because depending on when the context switches happen it might end
-- up building a deep stack of callbacks.  When this happens, the run
-- queue gets full of threads that have finished but cannot exit
-- because they do not belong to the topmost call to schedule(), and
-- the scheduler repeatedly traverses the run queue full of these
-- zombie threads.

module Main where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign.Ptr
import System.Environment
import System.IO

main = do
  [s] <- getArgs
  let n    = read s :: Int
      fork = if rtsSupportsBoundThreads then forkOS else forkIO
  sem <- newQSemN 0
  replicateM n $ putStr "." >> hFlush stdout >> fork (thread sem) >> thread sem
  putChar '\n'
  waitQSemN sem (n*2)

thread sem = do
  var <- newIORef 0
  let f = modifyIORef var (1+)
  callC =<< mkFunc f
  signalQSemN sem 1

type FUNC = IO ()

foreign import ccall unsafe "wrapper"
   mkFunc :: FUNC -> IO (FunPtr FUNC)

foreign import ccall safe "cbits.h callC"
   callC:: FunPtr FUNC -> IO ()

