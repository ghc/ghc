{- server

The purpose of this test driver is to test TCP Stream sockets.
All values have been hard coded since the BSD library is not used to
query the databases for the values.  In therory this code is thus not
portable but net007/Main.hs provides a portable version using the BSD
module.

This creates a stream socket bound to port 5000 and waits for incoming
messages it then reads all available data before closing the
connection to that peer.

No form of error checking is provided other than that already provided
by module SocketPrim.


TESTS:
    socket
    bindSocket
    listen
    accept
    readSocket
    sClose

-}


module Main where

import SocketPrim


main =
    socket AF_INET Stream 6			>>= \ s ->
    bindSocket s (SockAddrInet 5000 iNADDR_ANY)	>>
    listen s 5					>>

    let 
      loop = 
	accept s				>>= \ (s',peerAddr) ->
	putStr "*** Start of Transfer ***\n"	>>
	let 
	  read_all = 
	    readSocket s' 4			>>= \ (str, nbytes) ->
	    if nbytes /= 0 then
		putStr str			>>
		read_all
	    else
		putStr "\n*** End of Transfer ***\n" >>
		sClose s'
	in
	    read_all    
    in
	loop

