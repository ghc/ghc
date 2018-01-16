{-# LANGUAGE BangPatterns #-}

module Timer (timer) where

import Control.Exception (evaluate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Float (FFFormat(..), formatRealFloat)

ickyRound :: Int -> Double -> String
ickyRound k = formatRealFloat FFFixed (Just k)

timer :: Int -> a -> (a -> b) -> IO String
timer count a0 f = do
  let loop !k !fastest
        | k <= 0 = return fastest
        | otherwise = do
        start <- getPOSIXTime
        let inner a i
              | i <= 0    = return ()
              | otherwise = evaluate (f a) >> inner a (i-1)
        inner a0 count
        end <- getPOSIXTime
        let elapsed = end - start
        loop (k-1) (min fastest (elapsed / fromIntegral count))
  t <- loop (3::Int) 1e300
  let log10 x = log x / log 10
      ft = realToFrac t
      prec = round (log10 (fromIntegral count) - log10 ft)
  return $! ickyRound prec ft
{-# NOINLINE timer #-}
