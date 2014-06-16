module Main where

import Control.Concurrent

-- simple handshaking using two MVars, 
-- must context switch twice for each character.

main = do
  ready <- newEmptyMVar
  datum <- newEmptyMVar
  let 
    reader = do
	putMVar ready ()
	char <- takeMVar datum
  	if (char == '\n') 
		then return () 
		else do	putChar char; reader

    writer "" = do
	takeMVar ready
	putMVar datum '\n'
    writer (c:cs) = do
	takeMVar ready
	putMVar datum c
	writer cs

  forkIO reader
  writer "Hello World"
