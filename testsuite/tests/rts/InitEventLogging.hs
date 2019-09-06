{-# LANGUAGE ForeignFunctionInterface #-}

-- Test that the startEventLog interface works as expected.
main :: IO ()
main = do
  putStrLn "Starting eventlog..."
  c_init_eventlog
  putStrLn "done"

foreign import ccall unsafe "init_eventlog"
  c_init_eventlog :: IO ()
