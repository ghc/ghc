module Main where

import Network
import Control.Concurrent
import System.IO

-- NOTE: this test depends on non-blocking I/O support,
-- which win32 doesn't support. Rather than having the
-- test program block, we fail to initialise WinSock
-- (via withSocketsDo) here so that the test will fall over
-- (and repeatedly remind us to implement Win32 support
-- for non-blocking I/O !)

main = {- withSocketsDo $ -} do
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

