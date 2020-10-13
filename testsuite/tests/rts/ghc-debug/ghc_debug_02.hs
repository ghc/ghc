{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

foreign import ccall safe "ghc_debug.h pauseAndResumeViaThread"
    safe_pauseAndResumeViaThread_c :: Ptr CUInt -> IO CULong

foreign import ccall safe "pthread.h pthread_join"
    safe_pthread_join_c :: CULong -> IO ()

-- Simple test of rts_pause() followed by rts_resume() via a new thread created
-- in c code.
main :: IO ()
main = do
  alloca $ \countPtr -> do
    poke countPtr 0

    -- forever increment count. Changes will be observed from the c code.
    sequence_ $ replicate 4 $ forkIO $ forever $ do
      count <- peek countPtr
      poke countPtr (count + 1)
      threadDelay 10000   -- 10 milliseconds

    -- Test rts_pause/rts_resume.
    safe_pthread_join_c =<< safe_pauseAndResumeViaThread_c countPtr

    -- Test rts_pause/rts_resume from a unbound (worker) thread.
    mvar <- newEmptyMVar
    forkIO $ do
      safe_pthread_join_c =<< safe_pauseAndResumeViaThread_c countPtr
      putMVar mvar ()
    takeMVar mvar
