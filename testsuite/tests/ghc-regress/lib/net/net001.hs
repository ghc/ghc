module Main where

import Socket
import Concurrent
import IO

main = do
   forkIO server
   yield
   h <- connectTo "localhost" (PortNumber 22222)
   l <- hGetLine h
   hClose h
   print l
 where
   server = do
	  s <- listenOn (PortNumber 22222)
	  (h, host, port) <- accept s
	  hPutStrLn h "hello"
	  hClose h

