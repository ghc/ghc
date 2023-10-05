{-# LANGUAGE ForeignFunctionInterface #-}

import System.IO

import Control.Concurrent
import Control.Monad (forever, void, forM_)
import GHC.Conc


-- Test that the start/end/restartEventLog interface works as expected.
main :: IO ()
main = do

  --
  -- Start other threads to generate some event log events.
  --

  let loop f = void $ forkIO $ forever (f >> yield)

  forM_ [1..10] $ \_ -> do
    -- start lots of short lived threads
    loop (forkIO $ yield)

    -- sparks
    loop (let x = 1 + (1 :: Int) in return (par x (sum [0,1,2,3,x])))

  --
  -- Try restarting event logging a few times.
  --

  putStrLn "Restarting eventlog..."
  hFlush stdout
  c_restart_eventlog

foreign import ccall safe "c_restart_eventlog"
  c_restart_eventlog :: IO ()
