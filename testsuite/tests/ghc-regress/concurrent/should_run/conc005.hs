module Main where

import Concurrent
import MVar
import CVar

-- same as conc004, but using the ChannelVar abstraction

main = do
  v <- newCVar
  done <- newEmptyMVar
  let
	reader = do
 	    c <- readCVar v
	    if (c == '\n') 
		then putMVar done ()
		else do putChar c; reader

	writer []     = do writeCVar v '\n'; return ()
	writer (c:cs) = do writeCVar v c;    writer cs

  forkIO reader
  writer "Hello World"
  takeMVar done
