{- client

Client side of net005

TESTS:
    socket
    connect
    writeSocket
    shutdown
    sClose
-}


module Main where

import SocketPrim

message	    = "Hello World"


main =
    socket AF_UNIX Datagram 0				>>= \ s ->
    connect s (SockAddrUnix "sock")			>>
    
    writeSocket s message				>>
    shutdown s 2					>>
    sClose s
