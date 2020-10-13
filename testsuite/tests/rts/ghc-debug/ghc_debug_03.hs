{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Exit
import System.Timeout

foreign import ccall safe "ghc_debug.h pauseAndResume"
    safe_pauseAndResume_c :: CBool -> Ptr CUInt -> IO ()

-- Test that concurrent calls to rts_pause()/rts_resume() doesn't cause deadlock.
main :: IO ()
main = do
  alloca $ \countPtr -> do
    poke countPtr 0

    -- forever increment count. Changes will be observed from the c code.
    sequence_ $ replicate 4 $ forkIO $ forever $ do
      count <- peek countPtr
      poke countPtr (count + 1)
      threadDelay 10000   -- 10 milliseconds

    -- Note that each call blocks for about a second, so this will take 5
    -- seconds to complete.
    let n = 5
    mvars <- sequence $ replicate n newEmptyMVar
    forM_ mvars $ \mvar -> forkIO $ do
      safe_pauseAndResume_c
        -- Don't check rts_isPaused() before rts_pause nore after rts_resume
        -- because we're doing this concurrently so that would introduce a race
        -- condition.
        cFalse
        countPtr
      putMVar mvar ()

    -- Wait (at least 2n seconds to be safe) for all threads to finish.
    result <- timeout (2 * n * 1000000) (mapM_ takeMVar mvars)
    case result of
      Nothing -> do
        putStrLn "Not all rts_pause/rts_resume threads have finished. Assuming deadlocked and failing test."
        exitFailure
      Just () -> do
        putStrLn "All threads finished"
        exitSuccess

cFalse :: CBool
cFalse = 0
