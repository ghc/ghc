
module Main where

import Control.Concurrent
import Control.Exception

main = do 
  id <- myThreadId
  throwTo id (ErrorCall "hello")
