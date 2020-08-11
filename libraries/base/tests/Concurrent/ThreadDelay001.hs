{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
-- Test that threadDelay actually sleeps for (at least) as long as we
-- ask it

-- On windows the resolution of getCurrentTime is far too low to avoid
-- false positives for this test. So we use the internal method from
-- GHC.Event.Windows.Clock instead.

module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Time
#if defined(mingw32_HOST_OS)
import GHC.Event.Windows.Clock
#endif
main :: IO ()
main = mapM_ delay (0 : take 7 (iterate (*5) 100))

delay :: Int -> IO ()
delay n = do
#if defined(mingw32_HOST_OS)
  !sec_start <- getClock >>= getTime
  threadDelay n
  !sec_end <- getClock >>= getTime

  let diff = sec_end - sec_start
  when (diff * 1000000 < fromIntegral n) $ do
    putStrLn "threadDelay returned early"
    print(sec_start, sec_end, n, diff*1000000)

#else
  tS <- getCurrentTime
  threadDelay n
  tE <- getCurrentTime
  let req = fromIntegral n * 10 ^ (6 :: Int)
      obs = floor (diffUTCTime tE tS * 10 ^ (12 :: Int))
      diff = obs - req
      diff' :: Double
      diff' = fromIntegral diff /  10 ^ (12 :: Int)

  when (obs < req) $ print (tS, tE, req, obs, diff, diff')
#endif
