-- !!! Simple test of dupChan
-- Embarassingly, the published version fails!

module Main where

import Control.Exception
import Control.Concurrent.Chan

main = do
	  chan <- newChan
	  ch <- dupChan chan
	  writeChan chan "done"
	  x <- readChan chan
	  y <- readChan ch
	  print ("Got "++x ++" "++y) 
 
