{- client

As for net002 but reads port number and message as arguments.
It also prints out the IP number of the peer machine.



TESTS:
    getPeerName
-}


module Main where

import SocketPrim
import LibSystem


starbuck    = "130.209.240.81"
marcus	    = "130.209.247.2"


main =
    getArgs					>>= \ [port, message] ->
    socket AF_INET Stream 6			>>= \ s ->
    connect s (SockAddrInet (read port) (inet_addr starbuck))	>>

    getPeerName s			>>= \ (SockAddrInet p haddr) ->   
    putStr ("Connected to : " ++ (inet_ntoa haddr) ++ "\n") >>
    writeSocket s message			>>
    shutdown s 2				>>
    sClose s

