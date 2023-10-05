module Main where

import GHC.Clock
import Control.Monad

main :: IO ()
main = do
  a <- getMonotonicTimeNSec
  b <- getMonotonicTimeNSec
  when (a > b) $ putStrLn "Non-monotonic time"

  c <- getMonotonicTime
  d <- getMonotonicTime
  when (c > d) $ putStrLn "Non-monotonic time"
