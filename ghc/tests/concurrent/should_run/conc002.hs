module Main where

import Concurrent

main = do
  c <- newChan
  let 
    reader = do
	char <- readChan c
  	if (char == '\n') 
		then return () 
		else do	putChar char; reader	
  forkIO reader
  writeList2Chan c "Hello World\n"

