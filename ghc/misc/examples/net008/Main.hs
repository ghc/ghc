module Main where

import SocketPrim
import BSD
import LibSystem


main =
    getArgs					>>= \ [host, port, message] ->
    getProtocolNumber "tcp"			>>= \ proto ->
    socket AF_INET Stream proto			>>= \ s ->
    getHostByName host				>>= \ (HostEntry _ _ _ haddrs) ->
    connect s (SockAddrInet (read port) 
		(head haddrs))			>>

    getPeerName s				>>= \ (SockAddrInet _ haddr) ->  
    getHostByAddr AF_INET haddr			>>= \ (HostEntry hname _ _ _) ->
    putStr ("Connected to : " ++ hname ++ "\n") >>
    writeSocket s message			>>
    shutdown s 2				>>
    sClose s

