
-- Test that threadDelay actually sleeps for (at least) as long as we
-- ask it

module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Time

main :: IO ()
main = mapM_ delay (0 : take 7 (iterate (*5) 100))

delay :: Int -> IO ()
delay n = do
  tS <- getCurrentTime
  threadDelay n
  tE <- getCurrentTime

  let req = fromIntegral n * 10 ^ (6 :: Int)
      obs = floor (diffUTCTime tE tS * 10 ^ (12 :: Int))
      diff = obs - req
      diff' :: Double
      diff' = fromIntegral diff /  10 ^ (12 :: Int)

  when (obs < req) $ print (tS, tE, req, obs, diff, diff')

