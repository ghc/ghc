{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Foreign
import Foreign.C
import System.Exit
import System.Timeout

foreign import ccall safe "rts_pause_lock.h assertDoneAfterOneSecond"
    safe_assertDoneAfterOneSecond_c :: Ptr CInt -> IO ()

foreign import ccall safe "rts_pause_lock.h lockThenPause"
    safe_lockThenPause_c :: Ptr CInt -> IO ()

main :: IO ()
main = alloca $ \donePtr -> do
  -- We don't expect a deadlock, but we want to avoid one in the case of a
  -- failed test.
  poke donePtr 0
  forkIO $ safe_assertDoneAfterOneSecond_c donePtr

  -- The actual test.
  safe_lockThenPause_c donePtr
