{- client

Client side to net001/Main.hs.

Note that the machine IP numbers have been hard coded into this
program so it is unlikely that you will be able to run this test if
you are not at dcs.gla.ac.uk :-(

The reason for this is to aviod using the BSD module at this stage of
testing.


TESTS:
    socket
    connect
    writeSocket
    shutdown
    inet_addr
-}


module Main where

import SocketPrim


starbuck    = "130.209.240.81"		-- SunOS 4.1.3 1 sun4c
marcus	    = "130.209.247.2"		-- SunOS 4.1.3 6 sun4m
avon	    = "130.209.247.4"		-- OSF1 V2.0 240 alpha
karkar	    = "130.209.247.3"		-- OSF1 V2.0 240 alpha
nauru	    = "130.209.247.5"		-- Linux 2.0.30 (RH-4.2) x86
easter	    = "130.209.247.6"		-- Linux 2.0.30 (RH-4.2) x86

message	    = "Hello World"


main =
    socket AF_INET Stream 6				>>= \ s ->
    inet_addr easter					>>= \ ia ->
    connect s (SockAddrInet (mkPortNumber 5000) ia)     >>
    
    writeSocket s message				>>
    shutdown s ShutdownBoth				>>
    sClose s

