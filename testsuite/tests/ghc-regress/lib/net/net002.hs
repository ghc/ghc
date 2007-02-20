-- $Id: net002.hs,v 1.5 2005/01/17 09:57:24 simonmar Exp $
-- http://www.bagley.org/~doug/shootout/
-- Haskell echo/client server
-- written by Brian Gregor
-- compile with:
-- ghc -O -o echo -package net -package concurrent -package lang echo.hs
    
-- !!! exposed a bug in 5.02.2's network library, accept wasn't setting the
-- socket it returned to non-blocking mode.

-- NOTE: this test depends on non-blocking I/O support,
-- which win32 doesn't support. Rather than having the
-- test program block, we fail to initialise WinSock
-- (via withSocketsDo) here so that the test will fall over
-- (and repeatedly remind us to implement Win32 support
-- for non-blocking I/O !)

module Main where

import Network.Socket

import Prelude hiding (putStr)
import System.IO hiding (putStr)
import qualified System.IO
import System.IO.Error
import Control.Concurrent
import System.Environment 	( getArgs )
import System.Exit 		( exitFailure )
import Control.Exception 	( finally )

server_sock :: IO (Socket)
server_sock = do
    s <- socket AF_INET Stream 6
    setSocketOption s ReuseAddr 1
    -- bindSocket s (SockAddrInet (mkPortNumber portnum) iNADDR_ANY)
    bindSocket s (SockAddrInet (PortNum portnum) iNADDR_ANY)
    listen s 2
    return s

eofAsEmptyHandler :: IOError -> IO String
eofAsEmptyHandler e
 | isEOFError e = return ""
 | otherwise    = ioError e

-- For debugging, enable the putStr below.  Turn it off to get deterministic
-- results: on a multiprocessor we can't predict the order of the messages.
putStr = const (return ())
-- putStr = System.IO.putStr 

echo_server s = do
    (s', clientAddr) <- accept s
    proc <- read_data s' 0
    putStrLn ("server processed "++(show proc)++" bytes")
    sClose s'
    where
        read_data sock totalbytes = do
            -- (str,i) <- readSocket sock 19
            str <- recv sock 19 `catch` eofAsEmptyHandler
            -- if (i >= 19) 
            putStr ("Server recv: " ++ str)
            if ((length str) >= 19) 
                then do
                    putStr ("Server read: " ++ str)
                    -- writ <- writeSocket sock str
                    writ <- send sock str
                    putStr ("Server wrote: " ++ str)
                    --
                    read_data sock $! (totalbytes+(length $! str))
                    -- read_data sock (totalbytes+(length str))
                else do
                    putStr ("server read: " ++ str)
                    return totalbytes

local       = "127.0.0.1"        
message     = "Hello there sailor\n"
portnum     = 7001

client_sock = do
    s <- socket AF_INET Stream 6
    ia <- inet_addr local
    -- connect s (SockAddrInet (mkPortNumber portnum) ia)
    connect s (SockAddrInet (PortNum portnum) ia)
    return s

echo_client n = do
    s <- client_sock
    drop <- server_echo s n
    sClose s
    where
        server_echo sock n = if n > 0
            then do 
                -- writeSocket sock message
                send sock message
                putStr ("Client wrote: " ++ message)
                --
                -- (str,i) <- readSocket sock 19
                str <- recv sock 19 `catch` eofAsEmptyHandler
                if (str /= message)
                    then do
                        putStr ("Client read error: " ++ str ++ "\n")
                        exitFailure
                    else do
                        putStr ("Client read success")
                        server_echo sock (n-1)
            else do 
                putStr "Client read nil\n"
                return []

main = {- withSocketsDo $ -} do 
    ~[n] <- getArgs
    -- server & client semaphores
    -- get the server socket
    ssock <- server_sock 
    -- fork off the server
    s <- myForkIO (echo_server ssock)
    -- fork off the client
    c <- myForkIO (echo_client (read n::Int))
    -- let 'em run until they've signaled they're done
    join s
    System.IO.putStr "join s\n"
    join c
    System.IO.putStr "join c\n"

-- these are used to make the main thread wait until
-- the child threads have exited
myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
    mvar <- newEmptyMVar
    forkIO (io `finally` putMVar mvar ())
    return mvar

join :: MVar () -> IO ()
join mvar = readMVar mvar
