{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word
import Data.IORef
import GHC.Clock
import Control.Concurrent
import Foreign.C.Types
import System.Mem
import Control.Monad

foreign import ccall safe "pause_and_unpause_thread.h pauseAndUnpause"
    pauseAndUnpause_c :: IO ()

foreign import ccall safe "pause_and_unpause_thread.h getUnixTime"
    getUnixTime_c :: IO CTime

foreign import ccall safe "pause_and_unpause_thread.h getPauseBegin"
    getPauseBegin_c :: IO CTime

foreign import ccall safe "pause_and_unpause_thread.h getPauseEnd"
    getPauseEnd_c :: IO CTime

clockEachSecond :: IORef [CTime] -> IO ()
clockEachSecond ref = forever $ do
  time <- getUnixTime_c
  timesList <- readIORef ref
  writeIORef ref $ time : timesList

  sleepSeconds 1

{- To show that rts_pause() and rts_unpause() work, clockEachSecond adds the
current unix time to a list (once per Second). pauseAndUnpause_c stops the RTS
for 5 Seconds. Thus there's an invariant that there should be no timestamp in
the list that is in this 5 Seconds wide timeframe, which is defined by
getPauseBegin_c and getPauseEnd_c. -}
main :: IO ()
main = do
    ref <- newIORef []
    forkIO $ clockEachSecond ref

    sleepSeconds 3

    pauseAndUnpause_c

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

    return ()

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
