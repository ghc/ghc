module Main where

import Concurrent

-- same as conc004, but using the ChannelVar abstraction

main = do
  v <- newCVar
  let
	reader = do
 	    c <- readCVar v
	    if (c == '\n') 
		then return () 
		else do putChar c; reader

	writer []     = do writeCVar v '\n'; return ()
	writer (c:cs) = do writeCVar v c;    writer cs

  forkIO reader
  writer "Hello World"
  
