{-# LANGUAGE ForeignFunctionInterface #-}

import System.IO

-- Test that the startEventLog interface works as expected.
main :: IO ()
main = do
  putStrLn "Starting eventlog..."
  hFlush stdout
  c_init_eventlog

foreign import ccall unsafe "init_eventlog"
  c_init_eventlog :: IO ()
