{-# LANGUAGE ForeignFunctionInterface #-}

import System.IO

-- Test that the start/end/restartEventLog interface works as expected.
main :: IO ()
main = do
  putStrLn "Restarting eventlog..."
  hFlush stdout
  c_restart_eventlog

foreign import ccall unsafe "c_restart_eventlog"
  c_restart_eventlog :: IO ()
