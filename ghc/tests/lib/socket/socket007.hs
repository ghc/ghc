{- server

As net003 but uses the BSD module for portability.  Also prints the
common name of the host rather than its IP number.

TESTS:
    getProtocolNumber
    getSocketName
    getHostByAddr

-}

module Main where

import BSD
import SocketPrim

main =
    getProtocolNumber "tcp"			>>= \ proto ->
    socket AF_INET Stream proto			>>= \ s ->
    bindSocket s (SockAddrInet aNY_PORT iNADDR_ANY)	>>
    getSocketName s				>>= \ (SockAddrInet port _) ->
    putStr ("Allocated Port Number: " ++ show port ++ "\n") >>
    listen s 5					>>


    let 
      loop = 
	accept s		    >>= \ (s',(SockAddrInet _ haddr)) ->
	getHostByAddr AF_INET haddr		>>= \ (HostEntry hname _ _ _) ->
	putStr ("*** Start of Transfer from: " ++ hname ++ "***\n")	>>
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
