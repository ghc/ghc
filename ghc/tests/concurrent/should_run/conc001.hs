module Main where

import Concurrent

-- two processes, one MVar communication.

main = do
  s <- newEmptyMVar
  let 
    reader = do
	str <- takeMVar s
  	putStr str

  forkIO reader
  putMVar s "hello world\n"
