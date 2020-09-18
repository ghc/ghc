{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word
import Data.IORef
import GHC.Clock
import Control.Concurrent
import Foreign.C.Types
import System.Mem
import Control.Monad

foreign import ccall safe "rts_pause_and_resume_c.h pauseAndResume"
    safe_pauseAndResume_c :: IO ()

foreign import ccall unsafe "rts_pause_and_resume_c.h pauseAndResume"
    unsafe_pauseAndResume_c :: IO ()

foreign import ccall unsafe "rts_pause_and_resume_c.h pauseAndResumeViaNewThread"
    unsafe_pauseAndResumeViaNewThread_c :: IO ()

-- Note that these should be unsafe FFI calls. rts_pause() does not pause or
-- wait for safe FFI calls, as they do not own a capability.
foreign import ccall unsafe "rts_pause_and_resume_c.h getUnixTime"
    getUnixTime_c :: IO CTime

foreign import ccall unsafe "rts_pause_and_resume_c.h getPauseBegin"
    getPauseBegin_c :: IO CTime

foreign import ccall unsafe "rts_pause_and_resume_c.h getPauseEnd"
    getPauseEnd_c :: IO CTime

clockEachSecond :: IORef [CTime] -> IO ()
clockEachSecond ref = forever $ do
  time <- getUnixTime_c
  modifyIORef ref $ (time:)

  sleepSeconds 1

{- To show that rts_pause() and rts_resume() work, clockEachSecond adds the
current unix time to a list (once per Second). pauseAndResume_c stops the RTS
for 5 Seconds. Thus there's an invariant that there should be no timestamp in
the list that is in this 5 Seconds wide timeframe, which is defined by
getPauseBegin_c and getPauseEnd_c. -}
main :: IO ()
main = do
  -- Start thread that forever writes the current time to an IORef
  ref <- newIORef []
  forkIO $ clockEachSecond ref

  -- Attempt pause and resume in various forms
  withPauseAndResume ref
    "Pause and resume via safe FFI call"
    safe_pauseAndResume_c

  withPauseAndResume ref
    "Pause and resume via unsafe FFI call"
    unsafe_pauseAndResume_c

  withPauseAndResume ref
    "Pause and resume via unsafe FFI call that creates a new OS thread"
    unsafe_pauseAndResumeViaNewThread_c

withPauseAndResume :: IORef [CTime] -> String -> IO () -> IO ()
withPauseAndResume ref startMsg pauseAndResume = do
    putStrLn startMsg

    writeIORef ref []
    sleepSeconds 3
    pauseAndResume

    -- This seems to sleep for 8 - 5 Seconds. That's strange, but should be
    -- good enough for this test.
    -- 5 Seconds is the time the whole RTS is paused. But I (Sven) don't
    -- understand how this relates.
    sleepSeconds 8

    times <- readIORef ref

    pauseBegin <- getPauseBegin_c
    pauseEnd <- getPauseEnd_c
    filter (\t -> pauseBegin < t && t < pauseEnd) times `shouldBe` []
    filter (\t -> t <= pauseBegin) times `shouldNotBe` []
    filter (\t -> t >= pauseEnd) times `shouldNotBe` []

    putStrLn "DONE"

sleepSeconds :: Int -> IO ()
sleepSeconds t = threadDelay $ oneSecondInMicroSeconds * t

oneSecondInMicroSeconds :: Int
oneSecondInMicroSeconds = 1000000

shouldBe :: (Eq a, Show a) => a -> a -> IO ()
shouldBe x y =
  unless (x == y) $ fail $ show x ++ " is not equal to " ++ show y

shouldNotBe :: (Eq a, Show a) => a -> a -> IO ()
shouldNotBe x y =
  unless (x /= y) $ fail $ show x ++ " is equal to " ++ show y
