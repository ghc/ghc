{-# OPTIONS -fglasgow-exts #-}

module Main where

import Concurrent
import Exception

main = do 
  id <- myThreadId
  throwTo id (ErrorCall "hello")
