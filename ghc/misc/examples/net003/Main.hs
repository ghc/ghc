{- server

As for net001 but gets the system to allocate the next free port
number.  It also prints out the IP number of the peer.

TESTS:
    getSocketName
    inet_ntoa

-}

module Main where

import SocketPrim


main =
    socket AF_INET Stream 6			>>= \ s ->
    bindSocket s (SockAddrInet aNY_PORT iNADDR_ANY)	>>
    getSocketName s				>>= \ (SockAddrInet port _) ->
    putStr ("Allocated Port Number: " ++ show port ++ "\n") >>
    listen s 5					>>


    let 
      loop = 
	accept s		>>= \ (s',(SockAddrInet _ haddr)) ->
	putStr ("*** Start of Transfer from: " ++ 
		(inet_ntoa haddr) ++ "***\n")	>>
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

