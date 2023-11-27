module Main where

import Control.Concurrent
import System.Environment

main :: IO ()
main = do (str:time:_) <- getArgs
          putStrLn str
          threadDelay (read time)
          return ()
