
module Main where

import Control.Concurrent
import Control.Exception

-- Send ourselves a KillThread signal, catch it and recover.

main = do
  id <- myThreadId
  Control.Exception.catch (killThread id) $
     \e -> putStr (show (e::SomeExceptionWithLocation))
