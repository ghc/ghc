module Main where

import Concurrent

-- two processes, one MVar communication.

main = do
  s <- newEmptyMVar
  let 
    write = do
	putMVar s "hello world\n"

  forkIO write
  str <- takeMVar s
  putStr str
