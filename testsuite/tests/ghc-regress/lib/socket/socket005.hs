{- server

Server as net001 but for Unix Domain Datagram sockets.

TESTS:
    socket
    bindSocket
    readSocket

-}


module Main where

import SocketPrim


main =
    socket AF_UNIX Datagram 0			>>= \ s ->
    bindSocket s (SockAddrUnix "sock")		>>

    let 
      loop = 
	putStr "*** Start of Transfer ***\n"	>>
	let 
	  read_all = 
	    readSocket s 1024			>>= \ (str, nbytes) ->
	    if nbytes /= 0 then
		putStr str			>>
		read_all
	    else
		putStr "\n*** End of Transfer ***\n"
	in
	    read_all    
    in
	loop

