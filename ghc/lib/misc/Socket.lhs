%
% (c) The GRASP/AQUA Project, Glasgow University, 1995, 1996
%
% Last Modified: Fri Jul 21 15:53:32 1995
% Darren J Moffat <moffatd@dcs.gla.ac.uk>
%
% Further hacked on by Sigbjorn Finne <sof@dcs.gla.ac.uk>
%
\section[Socket]{Haskell 1.3 Socket bindings}


\begin{code}       
{-# OPTIONS -#include "cbits/ghcSockets.h" #-}

#include "config.h"

module Socket (
        PortID(..),
	Hostname,

	connectTo,	-- :: Hostname -> PortID -> IO Handle
	listenOn,	-- :: PortID -> IO Socket
	
	accept,		-- :: Socket -> IO (Handle, HostName)

	sendTo,		-- :: Hostname -> PortID -> String -> IO ()
	recvFrom,	-- :: Hostname -> PortID -> IO String

	socketPort	-- :: Socket -> IO PortID

       ) where

import BSD
import SocketPrim hiding ( accept, socketPort )
import qualified SocketPrim ( accept, socketPort )
import IO
\end{code} 

%***************************************************************************
%*                                                                         *
\subsection[Socket-Setup]{High Level ``Setup'' functions}
%*                                                                         *
%***************************************************************************

Calling $connectTo$ creates a client side socket which is
connected to the given host and port.  The Protocol and socket type is
derived from the given port identifier.  If a port number is given
then the result is always an internet family $Stream$ socket. 

If the $PortID$ specifies a unix family socket and the $Hostname$
differs from that returned by $getHostname$ then an error is
raised. Alternatively an empty string may be given to $connectTo$
signalling that the current hostname applies.

\begin{code}
data PortID = 
	  Service String		-- Service Name eg "ftp"
	| PortNumber Int		-- User defined Port Number
#ifndef cygwin32_TARGET_OS
	| UnixSocket String		-- Unix family socket in file system
#endif

type Hostname = String
-- Maybe consider this alternative.
-- data Hostname = Name String | IP Int Int Int Int
\end{code}
   
If more control over the socket type is required then $socketPrim$
should be used instead.

\begin{code}
connectTo :: Hostname		-- Hostname
	  -> PortID 		-- Port Identifier
	  -> IO Handle		-- Connected Socket

connectTo hostname (Service serv) =
    getProtocolNumber "tcp"			    >>= \ proto ->
    socket AF_INET Stream proto			    >>= \ sock ->
    getServicePortNumber serv			    >>= \ port ->
    getHostByName hostname			    >>= \ (HostEntry _ _ _ haddrs) ->
    connect sock (SockAddrInet port (head haddrs))  >>
    socketToHandle sock	ReadWriteMode		    >>= \ h ->
    return h
connectTo hostname (PortNumber port) =
    getProtocolNumber "tcp"			    >>= \ proto ->
    socket AF_INET Stream proto			    >>= \ sock ->
    getHostByName hostname			    >>= \ (HostEntry _ _ _ haddrs) ->
    connect sock (SockAddrInet port (head haddrs))  >>
    socketToHandle sock ReadWriteMode

#ifndef cygwin32_TARGET_OS
connectTo _ (UnixSocket path) =
    socket AF_UNIX Datagram 0			    >>= \ sock ->
    connect sock (SockAddrUnix path)		    >>
    socketToHandle sock ReadWriteMode
#endif

\end{code}

The dual to the $connectTo$ call. This creates the server side
socket which has been bound to the specified port.

\begin{code}
listenOn :: PortID 	-- Port Identifier
	 -> IO Socket	-- Connected Socket

listenOn (Service serv) =
    getProtocolNumber "tcp"			    >>= \ proto ->
    socket AF_INET Stream proto			    >>= \ sock ->
    getServicePortNumber serv			    >>= \ port ->
    bindSocket sock (SockAddrInet port iNADDR_ANY)  >>
    listen sock maxListenQueue			    >>
    return sock
listenOn (PortNumber port) =
    getProtocolNumber "tcp"			    >>= \ proto ->
    socket AF_INET Stream proto			    >>= \ sock ->
    bindSocket sock (SockAddrInet port iNADDR_ANY)  >>
    listen sock maxListenQueue			    >>
    return sock
#ifndef cygwin32_TARGET_OS
listenOn (UnixSocket path) =
    socket AF_UNIX Datagram 0			    >>= \ sock ->
    bindSocket sock (SockAddrUnix path)		    >>
    return sock
#endif
\end{code}

\begin{code}
accept :: Socket 		-- Listening Socket
       -> IO (Handle, 		-- StdIO Handle for read/write
	      HostName)		-- HostName of Peer socket

accept sock =
 SocketPrim.accept sock	             >>= \ (sock', (SockAddrInet _ haddr)) ->
 getHostByAddr AF_INET haddr         >>= \ (HostEntry peer _ _ _) ->
 socketToHandle sock ReadWriteMode   >>= \ handle ->
 return (handle, peer)
\end{code}

Send and recived data from/to the given host and port number.  These
should normally only be used where the socket will not be required for
further calls.

Thse are wrappers around socket, bind, and listen.

\begin{code}
sendTo :: Hostname 	-- Hostname
       -> PortID	-- Port Number
       -> String	-- Message to send
       -> IO ()
sendTo h p msg = 
 connectTo h p	>>= \ s ->
 hPutStr s msg	>>
 hClose s

recvFrom :: Hostname 	-- Hostname
	 -> PortID	-- Port Number
	 -> IO String	-- Received Data
recvFrom host port =
 listenOn port		>>= \ s ->
 let 
  waiting =
   SocketPrim.accept s		>>= \ (s', (SockAddrInet _ haddr)) ->
   getHostByAddr AF_INET haddr  	>>= \ (HostEntry peer _ _ _) ->
   if peer /= host then
      sClose s'			>>
      waiting
   else
      readSocketAll s'		>>= \ msg ->
      sClose s'			>>
      return msg
 in
 waiting			>>= \ message ->
 sClose s			>>
 return message
\end{code}

Access function returning the port type/id of socket.

\begin{code}
socketPort :: Socket -> IO PortID
socketPort s =
    getSocketName s			>>= \ sockaddr ->
    return (case sockaddr of
		SockAddrInet port _	->
		    (PortNumber port)
#ifndef cygwin32_TARGET_OS
		SockAddrUnix path	->
		    (UnixSocket path)
#endif
	    )
\end{code}
