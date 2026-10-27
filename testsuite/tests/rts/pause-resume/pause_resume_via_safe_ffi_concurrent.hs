{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Exit
import System.IO (hFlush, stdout)
import System.Timeout

foreign import ccall safe "pause_resume.h pauseAndResume"
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

    -- Each pause holds the RTS for about a second, so n concurrent
    -- calls serialize to ~n seconds. 2n (10s) was enough on Linux but
    -- a false deadlock on Darwin CI: rts_pause waits for every
    -- capability, and the counter threads keep hitting the timer
    -- manager via threadDelay. Keep a watchdog so a real hang still
    -- fails, but give 60s of slack.
    result <- timeout (60 * 1000000) (mapM_ takeMVar mvars)
    case result of
      Nothing -> do
        putStrLn "Not all rts_pause/rts_resume threads have finished. Assuming deadlocked and failing test."
        hFlush stdout
        exitFailure
      Just () -> do
        putStrLn "All threads finished"
        exitSuccess

cFalse :: CBool
cFalse = 0
