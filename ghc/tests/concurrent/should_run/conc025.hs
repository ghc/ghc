-- !!! Simple test of dupChan
-- Embarassingly, the published version fails!

module Main where

import Exception
import Chan

main = do
	  chan <- newChan
	  ch <- dupChan chan
	  writeChan chan "done"
	  x <- readChan chan
	  y <- readChan ch
	  print ("Got "++x ++" "++y) 
 
