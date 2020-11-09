{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

foreign import ccall safe "ghc_debug.h pauseAndResume"
    safe_pauseAndResume_c :: CBool -> Ptr CUInt -> IO ()

-- Simple test of rts_pause() followed by rts_resume()
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
    safe_pauseAndResume_c cTrue countPtr

    -- Test rts_pause/rts_resume from a unbound (worker) thread.
    mvar <- newEmptyMVar
    forkIO $ do
      safe_pauseAndResume_c cTrue countPtr
      putMVar mvar ()
    takeMVar mvar

cTrue :: CBool
cTrue = 1
