module Main where

import Concurrent

main = do
  c <- newChan
  let  writer = writeList2Chan c "Hello World\n"
  forkIO writer
  let  reader = do  char <- readChan c
		    if (char == '\n') 
			then return () 
			else do	putChar char; reader	
  reader

