
-- Test that threadDelay actually sleeps for (at least) as long as we
-- ask it

module Main (main) where

import Control.Concurrent
import Control.Monad
import System.Time

main :: IO ()
main = mapM_ delay (0 : take 7 (iterate (*5) 100))

delay :: Int -> IO ()
delay n = do
  tS <- getClockTime
  threadDelay n
  tE <- getClockTime

  let req = fromIntegral n * 10 ^ (6 :: Int)
      obs = case normalizeTimeDiff (diffClockTimes tE tS) of
                TimeDiff 0 0 0 0 0 s ps -> 10 ^ (12 :: Int) * fromIntegral s + ps
                td ->
                    error ("Bad TimeDiff: " ++ show td)
      diff = obs - req
      diff' :: Double
      diff' = fromIntegral diff /  10 ^ (12 :: Int)

  when (obs < req) $ print (tS, tE, req, obs, diff, diff')

