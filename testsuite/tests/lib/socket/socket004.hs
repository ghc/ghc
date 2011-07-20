{- client

As for net002 but reads port number and message as arguments.
It also prints out the IP number of the peer machine.



TESTS:
    getPeerName
-}


module Main(main) where

import SocketPrim
import System


starbuck    = "130.209.240.81"
marcus	    = "130.209.247.2"

nauru	    = "130.209.247.5"		-- Linux 2.0.30 (RH-4.2) x86
easter	    = "130.209.247.6"		-- Linux 2.0.30 (RH-4.2) x86


main =
    getArgs					>>= \ [port, message] ->
    socket AF_INET Stream 6			>>= \ s ->
    inet_addr easter				>>= \ i_addr ->
    connect s (SockAddrInet (mkPortNumber (read port)) i_addr)	>>

    getPeerName s			>>= \ (SockAddrInet p haddr) ->   
    inet_ntoa haddr		        >>= \ a ->
    putStr ("Connected to : " ++ a ++ "\n") >>
    writeSocket s message			>>
    shutdown s ShutdownBoth			>>
    sClose s

