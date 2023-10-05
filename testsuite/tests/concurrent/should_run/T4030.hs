module Main where

import Control.Concurrent
import Control.Exception

main :: IO ()
main = do tid <- mask $ \_ -> forkIO $ let x = x in x
          killThread tid
