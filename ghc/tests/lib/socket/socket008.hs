module Main where

import SocketPrim
import BSD
import System

main =
    getArgs					>>= \ [host, port, message] ->
    getProtocolNumber "tcp"			>>= \ proto ->
    socket AF_INET Stream proto			>>= \ s ->
    getHostByName host				>>= \ (HostEntry _ _ _ haddrs) ->
    connect s (SockAddrInet (mkPortNumber (read port))
		(head haddrs))			>>

    getPeerName s				>>= \ (SockAddrInet _ haddr) ->  
    getHostByAddr AF_INET haddr			>>= \ (HostEntry hname _ _ _) ->
    putStr ("Connected to : " ++ hname ++ "\n") >>
    writeSocket s message			>>
    shutdown s ShutdownBoth			>>
    sClose s

