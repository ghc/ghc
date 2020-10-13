{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Foreign
import Foreign.C
import System.Exit
import System.Timeout

foreign import ccall safe "rts_pause_lock.h assertDoneAfterOneSecond"
    safe_assertDoneAfterOneSecond_c :: Ptr CInt -> IO ()

foreign import ccall safe "rts_pause_lock.h doublePause"
    safe_doublePause_c :: Ptr CInt -> IO ()

main :: IO ()
main = alloca $ \donePtr -> do
  poke donePtr 0
  forkOS $ safe_assertDoneAfterOneSecond_c donePtr
  safe_doublePause_c donePtr
