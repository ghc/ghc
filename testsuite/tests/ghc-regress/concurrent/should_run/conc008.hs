{-# OPTIONS -fglasgow-exts #-}

module Main where

import Concurrent
import Exception

-- Send ourselves a KillThread signal, catch it and recover.

main = do 
  id <- myThreadId
  Exception.catch (killThread id) (\e -> putStr (show e))
