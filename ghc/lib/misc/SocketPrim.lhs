%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1998
%
\section[SocketPrim]{Low-level socket bindings}

The @SocketPrim@ module is for when you want full control over the
sockets, exposing the C socket API.

\begin{code}	   
{-# OPTIONS -#include "stgio.h" -#include "cbits/ghcSockets.h" #-}

#include "config.h"

module SocketPrim (

    Socket,		
    Family(..),		
    SocketType(..),
    SockAddr(..),
    HostAddress,
    ShutdownCmd(..),
    ProtocolNumber,

    socket,		-- :: Family -> SocketType -> ProtocolNumber -> IO Socket 
    connect,		-- :: Socket -> SockAddr -> IO ()
    bindSocket,		-- :: Socket -> SockAddr -> IO ()
    listen,		-- :: Socket -> Int -> IO ()
    accept,		-- :: Socket -> IO (Socket, SockAddr)
    getPeerName,	-- :: Socket -> IO SockAddr
    getSocketName,	-- :: Socket -> IO SockAddr

    socketPort,		-- :: Socket -> IO PortNumber

    writeSocket,	-- :: Socket -> String -> IO Int
    readSocket,		-- :: Socket -> Int -> IO (String, Int)
    readSocketAll,	-- :: Socket -> IO String

    socketToHandle,	-- :: Socket -> IO Handle

-- Alternative read/write interface not yet implemented.
--    sendto		-- :: Socket -> String -> SockAddr -> IO Int
--    recvfrm		-- :: Socket -> Int -> SockAddr -> IO (String, Int)
--    sendmsg		-- :: Socket -> Message -> MsgFlags -> IO Int
--    recvmsg		-- :: Socket -> MsgFlags -> IO Message

    shutdown,		-- :: Socket -> ShutdownCmd -> IO ()
    sClose,		-- :: Socket -> IO ()

    inet_addr,		-- :: String -> IO HostAddress
    inet_ntoa,		-- :: HostAddress -> IO String

    sIsConnected,	-- :: Socket -> IO Bool
    sIsBound,		-- :: Socket -> IO Bool
    sIsListening,	-- :: Socket -> IO Bool 
    sIsReadable,	-- :: Socket -> IO Bool
    sIsWritable,	-- :: Socket -> IO Bool


    PortNumber(..),
    mkPortNumber,	    -- :: Int -> PortNumber

-- Special Constants

    aNY_PORT,
    iNADDR_ANY,
--    sOL_SOCKET,
    sOMAXCONN,
    maxListenQueue,


-- The following are exported ONLY for use in the BSD module and
-- should not be used else where.

    packFamily, unpackFamily,
    packSocketType,
    packSockAddr, unpackSockAddr

) where
 
import GlaExts
import ST
import Ix
import PrelIOBase  -- IOError, Handle representation
import PrelHandle
import Foreign

import Posix
import PosixUtil
import IO
import IOExts	    ( IORef, newIORef, readIORef, writeIORef )
import PackedString ( unpackNBytesPS, byteArrayToPS, 
		      unpackCString, unpackCStringIO,
		      unpackCStringLenIO
		    )
\end{code}


%************************************************************************
%*									*
\subsection[Socket-SocketTypes]{Socket Types}
%*									*
%************************************************************************


There are a few possible ways to do this.  The first is convert the
structs used in the C library into an equivalent Haskell type.	An
other possible implementation is to keep all the internals in the C
code and use an Int\# and a status flag. The second method is used here
since a lot of the C structures are not required to be manipulated.

Originally the status was non-mutable so we had to return a new socket
each time we changed the status.  This version now uses mutable
variables to avoid the need to do this.	 The result is a cleaner
interface and better security since the application programmer now
can't circumvent the status information to perform invalid operations
on sockets.

\begin{code}  
data SocketStatus
  -- Returned Status	Function called
  = NotConnected	-- socket
  | Bound		-- bindSocket
  | Listening		-- listen
  | Connected		-- connect/accept
  | Error String	-- Any
    deriving (Eq, Show)

data Socket
  = MkSocket
	    Int		         -- File Descriptor
	    Family				  
	    SocketType				  
	    Int			 -- Protocol Number
	    (IORef SocketStatus) -- Status Flag
\end{code}

The scheme used for addressing sockets is somewhat quirky. The
calls in the BSD socket API that need to know the socket address all
operate in terms of \tr{struct sockaddr}, a `virtual' type of socket address. 

The Internet family of sockets are addressed as \tr{struct sockaddr\_in},
so when calling functions that operate on \tr{struct sockaddr}, we have
to type cast the Internet socket address into a \tr{struct sockaddr}. By luck(!),
the two structures are of the same size. Same casting is required of other
families of sockets such as Xerox NS. Similarly for Unix domain sockets.

To represent these socket addresses in Haskell-land, we do what BSD didn't do,
and use a union/algebraic type for the different families. Currently only
Unix domain sockets and the Internet family is supported.

\begin{code}

-- NOTE: HostAddresses are represented in network byte order.
--       Functions that expect the address in machine byte order
--       will have to perform the necessary translation.
type HostAddress = Word

--
-- newtyped to prevent accidental use of sane-looking
-- port numbers that haven't actually been converted to
-- network-byte-order first.
--
newtype PortNumber = PNum Int  -- 16-bit value stored in network byte order.
		     deriving ( Eq )

mkPortNumber :: Int -> PortNumber
mkPortNumber v = unsafePerformIO $ do
   po <- _casm_ ``%r=(int)htons((int)%0); '' v
   return (PNum po)

data SockAddr		-- C Names				
#ifndef cygwin32_TARGET_OS
  = SockAddrUnix        -- struct sockaddr_un
        String          -- sun_path
  | SockAddrInet	-- struct sockaddr_in
	PortNumber	-- sin_port  (network byte order)
	HostAddress	-- sin_addr  (ditto)
#else
  = SockAddrInet	-- struct sockaddr_in
	PortNumber	-- sin_port  (network byte order)
	HostAddress	-- sin_addr  (ditto)

#endif
    deriving Eq

type ProtocolNumber = Int

\end{code}


%************************************************************************
%*									*
\subsection[Socket-Connections]{Connection Functions}
%*									*
%************************************************************************

In the following connection and binding primitives.  The names of the
equivalent C functions have been preserved where possible. It should
be noted that some of these names used in the C library, \tr{bind} in
particular, have a different meaning to many Haskell programmers and
have thus been renamed by appending the prefix Socket.

Create an unconnected socket of the given family, type and protocol.
The most common invocation of $socket$ is the following:

\begin{verbatim}
   ...
   my_socket <- socket AF_INET Stream 6
   ...
\end{verbatim}

\begin{code}	   
socket :: Family 	 -- Family Name (usually AF_INET)
       -> SocketType 	 -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket	 -- Unconnected Socket

socket family stype protocol = do
    status <- _ccall_ createSocket (packFamily family) 
				   (packSocketType stype) 
				   protocol
    case status of
      -1 -> constructErrorAndFail "socket"
      n  -> do
	socket_status <- newIORef NotConnected
	return (MkSocket n family stype protocol socket_status)
\end{code}
      
Given a port number this {\em binds} the socket to that port. This
means that the programmer is only interested in data being sent to
that port number. The $Family$ passed to $bindSocket$ must
be the same as that passed to $socket$.	 If the special port
number $aNY\_PORT$ is passed then the system assigns the next
available use port.

Port numbers for standard unix services can be found by calling
$getServiceEntry$.  These are traditionally port numbers below
1000; although there are afew, namely NFS and IRC, which used higher
numbered ports.

The port number allocated to a socket bound by using $aNY\_PORT$ can be
found by calling $port$

\begin{code}
bindSocket :: Socket	-- Unconnected Socket
	   -> SockAddr	-- Address to Bind to
	   -> IO ()

bindSocket (MkSocket s family stype protocol socketStatus) addr = do
#ifndef cygwin32_TARGET_OS
 let isDomainSocket = if family == AF_UNIX then 1 else (0::Int)
#else
 let isDomainSocket = 0
#endif
 currentStatus <- readIORef socketStatus
 if currentStatus /= NotConnected 
  then
   fail (userError ("bindSocket: can't peform bind on socket in status " ++
	 show currentStatus))
  else do
   addr' <- packSockAddr addr
   let (_,sz) = boundsOfByteArray addr'
   status <- _ccall_ bindSocket s addr' sz isDomainSocket
   case status of
     -1 -> constructErrorAndFail "bindSocket"
     0  -> writeIORef socketStatus (Bound)
\end{code}
	

Make a connection to an already opened socket on a given machine and port.
assumes that we have already called createSocket, othewise it will fail.
			
This is the dual to $bindSocket$.  The {\em server} process will
usually bind to a port number, the {\em client} will then connect to 
the same port number.  Port numbers of user applications are normally
agreed in advance, otherwise we must rely on some meta protocol for telling
the other side what port number we have been allocated.	       

\begin{code}
connect :: Socket	-- Unconnected Socket
	-> SockAddr 	-- Socket address stuff
	-> IO ()

connect (MkSocket s family stype protocol socketStatus) addr = do
#ifndef cygwin32_TARGET_OS
 let isDomainSocket = if family == AF_UNIX then 1 else (0::Int)
#else
 let isDomainSocket = 0
#endif
 currentStatus <- readIORef socketStatus
 if currentStatus /= NotConnected 
  then
   fail (userError ("connect: can't peform connect on socket in status " ++
         show currentStatus))
  else do
   addr' <- packSockAddr addr
   let (_,sz) = boundsOfByteArray addr'
   status <- _ccall_ connectSocket s addr' sz isDomainSocket
   case status of
     -1 -> constructErrorAndFail "connect"
     0 -> writeIORef socketStatus Connected
\end{code}
       
The programmer must call $listen$ to tell the system software
that they are now interested in receiving data on this port.  This
must be called on the bound socket before any calls to read or write
data are made. 

The programmer also gives a number which indicates the length of the
incoming queue of unread messages for this socket. On most systems the
maximum queue length is around 5.  To remove a message from the queue
for processing a call to $accept$ should be made.	

\begin{code}
listen :: Socket  -- Connected & Bound Socket
       -> Int 	  -- Queue Length
       -> IO ()

listen (MkSocket s family stype protocol socketStatus) backlog = do
 currentStatus <- readIORef socketStatus
 if currentStatus /= Bound 
   then
    fail (userError ("listen: can't peform listen on socket in status " ++
          show currentStatus))
   else do
    status <- _ccall_ listenSocket s backlog
    case status of
      -1 -> constructErrorAndFail "listen"
      0  -> writeIORef socketStatus Listening
\end{code}

A call to $accept$ only returns when data is available on the given
socket, unless the socket has been set to non-blocking.	 It will
return a new socket which should be used to read the incoming data and
should then be closed. Using the socket returned by $accept$ allows
incoming requests to be queued on the original socket.

\begin{code}
accept :: Socket			-- Queue Socket
       -> IO (Socket,			-- Readable Socket
	      SockAddr)			-- Peer details

accept sock@(MkSocket s family stype protocol status) = do
 currentStatus <- readIORef status
 okay <- sIsAcceptable sock
 if not okay
   then
     fail (userError ("accept: can't peform accept on socket in status " ++
	 show currentStatus))
   else do
     (ptr, sz) <- allocSockAddr family
     int_star <- stToIO (newIntArray (0,1))
     stToIO (writeIntArray int_star 0 sz)
     sock <- _ccall_ acceptSocket s ptr int_star
     case sock of
	  -1 -> constructErrorAndFail "accept"
	  _  -> do
		sz <- stToIO (readIntArray int_star 0)
		addr <- unpackSockAddr ptr sz
		status <- newIORef Connected
		return ((MkSocket sock family stype protocol status), addr)
\end{code}

%************************************************************************
%*									*
\subsection[Socket-DataPass]{Data Passing Primitives}
%*									*
%************************************************************************

To allow Haskell to talk to C programs we need to be able to
communicate in terms of byte streams. @writeSocket@ and
@readSocket@ should only be used for this purpose and not for
communication between Haskell programs.	 Haskell programs should use
the 1.3 IO hPutStr and associated machinery for communicating with
each other.


\begin{code}
writeSocket :: Socket	-- Connected Socket
	    -> String	-- Data to send
	    -> IO Int	-- Number of Bytes sent

writeSocket (MkSocket s family stype protocol status) xs = do
 currentStatus <- readIORef status
 if not ((currentStatus /= Connected) || (currentStatus /= Listening)) 
   then
    fail (userError ("writeSocket: can't peform write on socket in status " ++
          show currentStatus))
   else do
    nbytes <- _ccall_ writeDescriptor s xs (length xs)
    case nbytes of
      -1 -> constructErrorAndFail "writeSocket"
      _  -> return nbytes

readSocket :: Socket		-- Connected Socket
	   -> Int		-- Number of Bytes to Read
	   -> IO (String, Int)	-- (Data Read, Number of Bytes)

readSocket (MkSocket s family stype protocol status) nbytes = do
 currentStatus <- readIORef status
 if not ((currentStatus /= Connected) || (currentStatus /= Listening))
   then
    fail (userError ("readSocket: can't perform read on socket in status " ++
	  show currentStatus))
   else do
    ptr <- stToIO (newCharArray (0, nbytes))
    nbytes <- _ccall_ readDescriptor s ptr nbytes
    case nbytes of
      -1 -> constructErrorAndFail "readSocket"
      n  -> do
	    barr <- stToIO (unsafeFreezeByteArray ptr)
            return (unpackNBytesPS (byteArrayToPS barr) n, n)

readSocketAll :: Socket -> IO String
readSocketAll s =
    let 
      loop xs =
       catch
        (readSocket s 4096			>>= \ (str, nbytes) ->
	 if nbytes /= 0 then
	    loop (str ++ xs)
	 else
	    return xs)
	(\ _ -> return xs)
    in
    	loop ""
\end{code}

The port number the given socket is currently connected to can be
determined by calling $port$, is generally only useful when bind
was given $aNY\_PORT$.

\begin{code}
socketPort :: Socket		-- Connected & Bound Socket
	   -> IO PortNumber	-- Port Number of Socket
socketPort sock@(MkSocket s AF_INET stype protocol status) =
    getSocketName sock >>= \(SockAddrInet port _) ->
    return port
socketPort (MkSocket s family stype protocol status) =
    fail (userError ("socketPort: not supported for Family " ++ show family))
\end{code}

Calling $getPeerName$ returns the address details of the machine,
other than the local one, which is connected to the socket. This is
used in programs such as FTP to determine where to send the returning
data.  The corresponding call to get the details of the local machine
is $getSocketName$.

\begin{code}
getPeerName   :: Socket -> IO SockAddr

getPeerName (MkSocket s family stype protocol status) = do
 (ptr, sz) <- allocSockAddr family
 int_star <- stToIO (newIntArray (0,1))
 stToIO (writeIntArray int_star 0 sz)
 status <- _ccall_ getPeerName s ptr int_star
 case status of
   -1 -> constructErrorAndFail "getPeerName"
   _  -> do
	  sz <- stToIO (readIntArray int_star 0)
	  unpackSockAddr ptr sz
    
getSocketName :: Socket -> IO SockAddr

getSocketName (MkSocket s family stype protocol status) = do
 (ptr, sz) <- allocSockAddr family
 int_star <- stToIO (newIntArray (0,1))
 stToIO (writeIntArray int_star 0 sz)
 status <- _ccall_ getSockName s ptr int_star
 case status of
   -1 -> constructErrorAndFail "getSocketName"
   _  -> do
         sz <- stToIO (readIntArray int_star 0)
	 unpackSockAddr ptr sz


\end{code}


%************************************************************************
%*									*
\subsection[Socket-Properties]{Socket Properties}
%*									*
%************************************************************************

\begin{code}
{-
data SocketOption =
      Debug
    | AcceptConnection
    | ReuseAddr
    | KeepAlive
    | DontRoute
    | Broadcast
    | UseLoopBack
    | Linger
    | OOBInline
    | SendBuffer
    | RecvBuffer
    | SendLowWater
    | RecvLowWater
    | SendTimeOut
    | RecvTimeOut
    | Error
    | Type

sOL_SOCKET = ``SOL_SOCKET''

setSocketOptions :: Socket ->
		    Int ->		-- Level
		    SocketOption ->	-- Option Name
		    String ->		-- Option Value
		    IO ()

getSocketOptions :: Socket ->
		   Int ->		-- Level
		   SocketOption ->	-- Option Name
		   IO String		-- Option Value
-}
\end{code}

A calling sequence table for the main functions is shown in the table below.

\begin{figure}[h]
\begin{center}
\begin{tabular}{|l|c|c|c|c|c|c|c|}
\hline
{\bf A Call to} & socket & connect & bindSocket & listen & accept & read & write \\
\hline
{\bf Precedes} & & & & & & & \\
\hline 
socket &	&	  &	       &	&	 &	& \\
\hline
connect & +	&	  &	       &	&	 &	& \\
\hline
bindSocket & +	&	  &	       &	&	 &	& \\
\hline
listen &	&	  & +	       &	&	 &	& \\
\hline
accept &	&	  &	       &  +	&	 &	& \\
\hline
read   &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
write  &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
\end{tabular}
\caption{Sequence Table for Major functions of Socket}
\label{tab:api-seq}
\end{center}
\end{figure}

%************************************************************************
%*									*
\subsection[Socket-OSDefs]{OS Dependent Definitions}
%*									*
%************************************************************************

    
The following Family and Socket Type declarations were manually derived
from @<sys/socket.h>@ on the appropriate machines.

Maybe a configure script that could parse the socket.h file to produce
the following declaration is required to make it ``portable'' rather than
using the dreaded \#ifdefs.

Presently only the following machine/os combinations are supported:

\begin{itemize}
\item Intelx86/Linux
\item SPARC/SunOS
\item SPARC/Solaris
\item Alpha/OSF
\item HPPA/HPUX9
\item MIPS/IRIX6.2
\end{itemize}

\begin{code}
unpackFamily	:: Int -> Family
packFamily	:: Family -> Int

packSocketType	:: SocketType -> Int


#if sunos4_TARGET_OS || solaris2_TARGET_OS
 
data Family = 
	  AF_UNSPEC	-- unspecified
	| AF_UNIX	-- local to host (pipes, portals
	| AF_INET	-- internetwork: UDP, TCP, etc
	| AF_IMPLINK	-- arpanet imp addresses
	| AF_PUP	-- pup protocols: e.g. BSP
	| AF_CHAOS	-- mit CHAOS protocols
	| AF_NS		-- XEROX NS protocols 
	| AF_NBS	-- nbs protocols
	| AF_ECMA	-- european computer manufacturers
	| AF_DATAKIT	-- datakit protocols
	| AF_CCITT	-- CCITT protocols, X.25 etc
	| AF_SNA	-- IBM SNA
	| AF_DECnet	-- DECnet
	| AF_DLI	-- Direct data link interface
	| AF_LAT	-- LAT
	| AF_HYLINK	-- NSC Hyperchannel
	| AF_APPLETALK	-- Apple Talk
	| AF_NIT	-- Network Interface Tap
	| AF_802	-- IEEE 802.2, also ISO 8802
	| AF_OSI	-- umbrella of all families used by OSI
	| AF_X25	-- CCITT X.25
	| AF_OSINET	-- AFI
	| AF_GOSSIP	-- US Government OSI
	| AF_IPX	-- Novell Internet Protocol
	deriving (Eq, Ord, Ix, Show)
			
packFamily = index (AF_UNSPEC, AF_IPX)
unpackFamily family = (range (AF_UNSPEC, AF_IPX))!!family

#endif

#if cygwin32_TARGET_OS
 
data Family = 
	  AF_UNSPEC	-- unspecified
	| AF_UNIX	-- local to host (pipes, portals)
	| AF_INET	-- internetwork: UDP, TCP, etc
	| AF_IMPLINK	-- arpanet imp addresses
	| AF_PUP	-- pup protocols: e.g. BSP
	| AF_CHAOS	-- mit CHAOS protocols
	| AF_NS		-- XEROX NS protocols 
	| AF_ISO	-- ISO protocols
	| AF_OSI	-- OSI protocols
	| AF_ECMA	-- european computer manufacturers
	| AF_DATAKIT	-- datakit protocols
	| AF_CCITT	-- CCITT protocols, X.25 etc
	| AF_SNA	-- IBM SNA
	| AF_DECnet	-- DECnet
	| AF_DLI	-- Direct data link interface
	| AF_LAT	-- LAT
	| AF_HYLINK	-- NSC Hyperchannel
	| AF_APPLETALK	-- Apple Talk
	| AF_NETBIOS	-- NetBios-style addresses
	deriving (Eq, Ord, Ix, Show)
			
packFamily = index (AF_UNSPEC, AF_NETBIOS)
unpackFamily family = (range (AF_UNSPEC, AF_NETBIOS))!!family


#endif

#if hpux_TARGET_OS
 
data Family = 
	  AF_UNSPEC	-- unspecified
	| AF_UNIX	-- local to host (pipes, portals
	| AF_INET	-- internetwork: UDP, TCP, etc
	| AF_IMPLINK	-- arpanet imp addresses
	| AF_PUP	-- pup protocols: e.g. BSP
	| AF_CHAOS	-- mit CHAOS protocols
	| AF_NS		-- XEROX NS protocols 
	| AF_NBS	-- nbs protocols
	| AF_ECMA	-- european computer manufacturers
	| AF_DATAKIT	-- datakit protocols
	| AF_CCITT	-- CCITT protocols, X.25 etc
	| AF_SNA	-- IBM SNA
	| AF_DECnet	-- DECnet
	| AF_DLI	-- Direct data link interface
	| AF_LAT	-- LAT
	| AF_HYLINK	-- NSC Hyperchannel
	| AF_APPLETALK	-- Apple Talk
	| AF_NIT	-- Network Interface Tap
	deriving (Eq, Ord, Ix, Show)
			
packFamily = index (AF_UNSPEC, AF_NIT)
unpackFamily family = (range (AF_UNSPEC, AF_NIT))!!family

#endif

#if osf1_TARGET_OS || osf3_TARGET_OS
       
data Family =
	  AF_UNSPEC	-- unspecified 
	| AF_UNIX	-- local to host (pipes, portals) 
	| AF_INET	-- internetwork: UDP, TCP, etc. 
	| AF_IMPLINK	-- arpanet imp addresses 
	| AF_PUP	-- pup protocols: e.g. BSP 
	| AF_CHAOS	-- mit CHAOS protocols 
	| AF_NS		-- XEROX NS protocols 
	| AF_ISO	-- ISO protocols 
	| AF_ECMA	-- european computer manufacturers 
	| AF_DATAKIT	-- datakit protocols 
	| AF_CCITT	-- CCITT protocols, X.25 etc 
	| AF_SNA	-- IBM SNA 
	| AF_DECnet	-- DECnet 
	| AF_DLI	-- DEC Direct data link interface 
	| AF_LAT	-- LAT 
	| AF_HYLINK	-- NSC Hyperchannel 
	| AF_APPLETALK	-- Apple Talk 
	| AF_ROUTE	-- Internal Routing Protocol 
	| AF_LINK	-- Link layer interface 
	| Pseudo_AF_XTP	-- eXpress Transfer Protocol (no AF) 
	| AF_NETMAN	-- DNA Network Management 
	| AF_X25	-- X25 protocol 
	| AF_CTF	-- Common Trace Facility 
	| AF_WAN	-- Wide Area Network protocols 
	deriving (Eq, Ord, Ix, Show)
  
packFamily = index (AF_UNSPEC, AF_WAN)
unpackFamily family = (range (AF_UNSPEC, AF_WAN))!!family
#endif 

#if linux_TARGET_OS

data Family = 
	  AF_UNSPEC
	| AF_UNIX
	| AF_INET
	| AF_AX25
	| AF_IPX
	deriving (Eq, Ord, Ix, Show)	

packFamily = index (AF_UNSPEC, AF_IPX)
unpackFamily family = (range (AF_UNSPEC, AF_IPX))!!family

#endif

#if irix_TARGET_OS

data Family = 
          AF_UNSPEC	      	-- unspecified
        | AF_UNIX	      	-- backward compatibility
        | AF_INET	      	-- internetwork: UDP, TCP, etc.
        | AF_IMPLINK	      	-- arpanet imp addresses
        | AF_PUP	      	-- pup protocols: e.g. BSP
        | AF_CHAOS	      	-- mit CHAOS protocols
        | AF_NS		      	-- XEROX NS protocols
        | AF_ISO	      	-- ISO protocols
        | AF_ECMA	      	-- european computer manufacturers
        | AF_DATAKIT	      	-- datakit protocols
        | AF_CCITT		-- CCITT protocols, X.25 etc
        | AF_SNA		-- IBM SNA
        | AF_DECnet		-- DECnet
        | AF_DLI		-- DEC Direct data link interface
        | AF_LAT		-- LAT
        | AF_HYLINK		-- NSC Hyperchannel
        | AF_APPLETALK		-- Apple Talk
        | AF_ROUTE		-- Internal Routing Protocol
        | AF_RAW		-- Link layer interface

-- these two overlap AF_ROUTE and AF_RAW
--	| AF_NIT	        -- Network Interface Tap
--	| AF_802	        -- IEEE 802.2, also ISO 8802

	| AF_OSI        	-- umbrella for all families used by OSI
	| AF_X25	        -- CCITT X.25
	| AF_OSINET	        -- AFI
	| AF_GOSIP	        -- US Government OSI

        | AF_SDL		-- SGI Data Link for DLPI
        | AF_INET6		-- Internet Protocol version 6
        | AF_LINK		-- Link layer interface
	deriving (Eq, Ord, Ix, Show)	

packFamily = index (AF_UNSPEC, AF_LINK)
unpackFamily family = (range (AF_UNSPEC, AF_LINK))!!family

#endif

#if aix_TARGET_OS

data Family = 
       	AF_UNSPEC	-- unspecified 
      |	AF_UNIX		-- local to host (pipes, portals) 
      |	AF_INET		-- internetwork: UDP, TCP, etc. 
      |	AF_IMPLINK	-- arpanet imp addresses 
      |	AF_PUP		-- pup protocols: e.g. BSP 
      |	AF_CHAOS	-- mit CHAOS protocols 
      |	AF_NS		-- XEROX NS protocols 
      |	AF_ISO		-- ISO protocols 
--    |	AF_OSI is the same as AF_ISO on AIX
      |	AF_ECMA		-- european computer manufacturers 
      |	AF_DATAKIT	-- datakit protocols 
      |	AF_CCITT	-- CCITT protocols, X.25 etc 
      |	AF_SNA		-- IBM SNA 
      | AF_DECnet	-- DECnet 
      | AF_DLI		-- DEC Direct data link interface 
      | AF_LAT		-- LAT 
      |	AF_HYLINK	-- NSC Hyperchannel 
      |	AF_APPLETALK	-- Apple Talk 
      |	AF_ROUTE	-- Internal Routing Protocol 
      |	AF_LINK		-- Link layer interface 
      |	Pseudo_AF_XTP	-- eXpress Transfer Protocol (no AF) 
      | AF_INTF		-- Debugging use only 
      | AF_RIF		-- raw interface 
      |	AF_NETWARE	
      |	AF_NDD		
      |	AF_MAX		
	deriving (Eq, Ord, Ix, Show)	

packFamily = index (AF_UNSPEC, AF_MAX)
unpackFamily family = (range (AF_UNSPEC, AF_MAX))!!family

#endif

#if freebsd_TARGET_OS

data Family = 
       	AF_UNSPEC	-- unspecified 
      |	AF_UNIX		-- local to host (pipes, portals) 
      |	AF_INET		-- internetwork: UDP, TCP, etc. 
      |	AF_IMPLINK	-- arpanet imp addresses 
      |	AF_PUP		-- pup protocols: e.g. BSP 
      |	AF_CHAOS	-- mit CHAOS protocols 
      |	AF_NS		-- XEROX NS protocols 
      |	AF_ISO		-- ISO protocols 
--    |	AF_OSI is the same as AF_ISO
      |	AF_ECMA		-- european computer manufacturers 
      |	AF_DATAKIT	-- datakit protocols 
      |	AF_CCITT	-- CCITT protocols, X.25 etc 
      |	AF_SNA		-- IBM SNA 
      | AF_DECnet	-- DECnet 
      | AF_DLI		-- DEC Direct data link interface 
      | AF_LAT		-- LAT 
      |	AF_HYLINK	-- NSC Hyperchannel 
      |	AF_APPLETALK	-- Apple Talk 
      |	AF_ROUTE	-- Internal Routing Protocol 
      |	AF_LINK		-- Link layer interface 
      |	Pseudo_AF_XTP	-- eXpress Transfer Protocol (no AF) 
      | AF_COIP         -- connection-oriented IP, aka ST II
      | AF_CNT		-- Computer Network Technology
      | Psuedo_AF_RTIP  -- Help Identify RTIP packets
      | AF_IPX		-- Novell Internet Protocol
      | AF_SIP          -- Simple Internet Protocol
      | Pseudo_AF_PIP   -- Help Identify PIP packets
      | AF_ISDN         -- Integrated Services Digital Network
--    | AF_E164	is the same as AF_ISDN
      | Pseudo_AF_KEY   -- Internal key-management function
      | AF_INET6	-- IPv6
      | AF_MAX
	deriving (Eq, Ord, Ix, Show)	

packFamily = index (AF_UNSPEC, AF_MAX)
unpackFamily family = (range (AF_UNSPEC, AF_MAX))!!family

#endif

-- Alpha running OSF or a SPARC with SunOS, rather than Solaris.

#if osf1_TARGET_OS || osf3_TARGET_OS || sunos4_TARGET_OS || hpux_TARGET_OS || \
	aix_TARGET_OS || freebsd_TARGET_OS
data SocketType = 
	  Stream 
	| Datagram
	| Raw 
	| RDM 
	| SeqPacket
	deriving (Eq, Ord, Ix, Show)
	
packSocketType stype = 1 + (index (Stream, SeqPacket) stype)	
#endif

-- This is for a box running cygwin32 toolchain.

#if defined(cygwin32_TARGET_OS)
data SocketType = 
	  Stream 
	| Datagram
	| Raw 
	| RDM       -- reliably delivered msg
	| SeqPacket
	| Packet
	deriving (Eq, Ord, Ix, Show)
	
packSocketType stype =
 case stype of 
   Stream    -> ``SOCK_STREAM''
   Datagram  -> ``SOCK_DGRAM''
   Raw       -> ``SOCK_RAW''
   RDM       -> ``SOCK_RDM'' 
   SeqPacket -> ``SOCK_SEQPACKET''
   Packet    -> ``SOCK_PACKET''

#endif

-- This is a Sun running Solaris rather than SunOS or SGI running IRIX

#if defined(solaris2_TARGET_OS) || defined(irix_TARGET_OS)
data SocketType =
	  Datagram
	| Stream
	| NC_TPI_COTS_ORD
	| Raw
	| RDM
	| SeqPacket
	deriving (Eq, Ord, Ix, Show)	

packSocketType stype = 1 + (index (Datagram, SeqPacket) stype)
#endif	
    

#if linux_TARGET_OS
data SocketType = 
	  Stream 
	| Datagram
	| Raw 
	| RDM 
	| SeqPacket
	| Packet
	deriving (Eq, Ord, Ix, Show)

packSocketType stype = 1 + (index (Stream, Packet) stype)	
#endif
\end{code}

%************************************************************************
%*									*
\subsection[Socket-Util]{Utility Functions}
%*									*
%************************************************************************

\begin{code}
aNY_PORT = 0::Int
iNADDR_ANY :: HostAddress
iNADDR_ANY = unsafePerformIO (_casm_ `` %r = htonl(INADDR_ANY); '')

sOMAXCONN = ``SOMAXCONN''::Int
maxListenQueue = sOMAXCONN

-------------------------------------------------------------------------------
data ShutdownCmd 
 = ShutdownReceive
 | ShutdownSend
 | ShutdownBoth

sdownCmdToInt :: ShutdownCmd -> Int
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = do
  let t = sdownCmdToInt stype
  status <- _ccall_ shutdownSocket s t
  case status of
    -1 -> constructErrorAndFail "shutdown"
    _  -> return ()

-------------------------------------------------------------------------------

sClose	 :: Socket -> IO ()
sClose (MkSocket s family stype protocol status) = _ccall_ close s

-------------------------------------------------------------------------------

sIsConnected :: Socket -> IO Bool
sIsConnected (MkSocket s family stype protocol status) = do
    value <- readIORef status
    return (value == Connected)	

-------------------------------------------------------------------------------

sIsBound :: Socket -> IO Bool
sIsBound (MkSocket s family stype protocol status) = do
    value <- readIORef status
    return (value == Bound)	

-------------------------------------------------------------------------------

sIsListening :: Socket -> IO Bool
sIsListening (MkSocket s family stype protocol status) = do
    value <- readIORef status
    return (value == Listening)	

-------------------------------------------------------------------------------

sIsReadable  :: Socket -> IO Bool
sIsReadable (MkSocket s family stype protocol status) = do
    value <- readIORef status
    return (value == Listening || value == Connected)

-------------------------------------------------------------------------------

sIsWritable  :: Socket -> IO Bool
sIsWritable = sIsReadable

-------------------------------------------------------------------------------

sIsAcceptable :: Socket -> IO Bool
#ifndef cygwin32_TARGET_OS
sIsAcceptable (MkSocket s AF_UNIX Stream protocol status) = do
    value <- readIORef status
    return (value == Connected || value == Bound || value == Listening)
sIsAcceptable (MkSocket s AF_UNIX _ protocol status) = 
    return False
#endif
sIsAcceptable (MkSocket s _ stype protocol status) = do
    value <- readIORef status
    return (value == Connected || value == Listening)
    
-------------------------------------------------------------------------------

{-
sSetBlocking :: Socket -> Bool -> IO ()
sIsBlocking  :: Socket -> IO Bool
-}

\end{code}

Internet address manipulation routines:

\begin{code}
inet_addr :: String -> IO HostAddress
inet_addr ipstr = do
   had <- _ccall_ inet_addr ipstr
   if had == (W# (int2Word# (negateInt# 1#))) -- hack to avoid depending on Int.intToWord here.
    then fail (userError ("inet_addr: Malformed address: " ++ ipstr))
    else return had  -- network byte order

inet_ntoa :: HostAddress -> IO String
inet_ntoa haddr = do
  pstr <- _casm_ ``struct in_addr addr;
		   addr.s_addr = %0;
		   %r = inet_ntoa (addr);'' haddr
  -- unpack straight away, since pstr points to static buffer.
  unpackCStringIO pstr

\end{code}

Marshaling and allocation helper functions:

\begin{code}
-------------------------------------------------------------------------------

allocSockAddr :: Family -> IO (MutableByteArray RealWorld Int, Int)

#ifndef cygwin32_TARGET_OS
allocSockAddr AF_UNIX = do
    ptr <- stToIO (newCharArray (0,``sizeof(struct sockaddr_un)''))
    let (_,sz) = boundsOfByteArray ptr
    return (ptr, sz)
#endif

allocSockAddr AF_INET = do
    ptr <- stToIO (newCharArray (0,``sizeof(struct sockaddr_in)''))
    let (_,sz) = boundsOfByteArray ptr
    return (ptr, sz)

-------------------------------------------------------------------------------

unpackSockAddr :: MutableByteArray RealWorld Int -> Int -> IO SockAddr
unpackSockAddr arr len = do
    fam <- _casm_ ``%r = ((struct sockaddr*)%0)->sa_family;'' arr
    case unpackFamily fam of
#ifndef cygwin32_TARGET_OS
	AF_UNIX -> unpackSockAddrUnix arr (len - ``sizeof(short)'')
#endif
	AF_INET -> unpackSockAddrInet arr

-------------------------------------------------------------------------------

#ifndef cygwin32_TARGET_OS

{-
  sun_path is *not* NULL terminated, hence we *do* need to know the
  length of it.
-}
unpackSockAddrUnix :: (MutableByteArray RealWorld Int) -> Int -> IO SockAddr
unpackSockAddrUnix ptr len = do
    char_star <- _casm_ ``%r = ((struct sockaddr_un*)%0)->sun_path;'' ptr
    path      <- unpackCStringLenIO char_star len
    return (SockAddrUnix path)

#endif

-------------------------------------------------------------------------------

unpackSockAddrInet :: (MutableByteArray RealWorld Int) -> IO SockAddr
unpackSockAddrInet ptr = do
  port <- _casm_ ``%r = ((struct sockaddr_in*)%0)->sin_port;''        ptr
  addr <- _casm_ ``%r = ((struct sockaddr_in*)%0)->sin_addr.s_addr;'' ptr
  return (SockAddrInet (PNum port) addr)

-------------------------------------------------------------------------------


packSockAddr :: SockAddr -> IO (MutableByteArray RealWorld Int)
#ifndef cygwin32_TARGET_OS
packSockAddr (SockAddrUnix path) = do
    (ptr,_) <- allocSockAddr AF_UNIX
    _casm_ ``(((struct sockaddr_un *)%0)->sun_family) = AF_UNIX;''    ptr
    _casm_ ``strcpy ((((struct sockaddr_un *)%0)->sun_path),%1);''    ptr path
    return ptr
#endif
packSockAddr (SockAddrInet (PNum port) address) = do
  (ptr,_) <- allocSockAddr AF_INET
  _casm_ ``(((struct sockaddr_in *)%0)->sin_family) = AF_INET;''  ptr
  _casm_ ``(((struct sockaddr_in *)%0)->sin_port) = (int)%1;''    ptr port
  _casm_ ``(((struct sockaddr_in *)%0)->sin_addr.s_addr) = %1;''  ptr address
  return ptr

-------------------------------------------------------------------------------
\end{code}

@socketHandle@ turns a @Socket@ into a Haskell IO @Handle@. By default, the new
handle will not be buffered, use @hSetBuffering@ if you want to change
it subsequently.

\begin{code}
#ifndef __PARALLEL_HASKELL__
socketToHandle :: Socket -> IOMode -> IO Handle

socketToHandle (MkSocket s family stype protocol status) m = do
    ptr <- _casm_ ``%r = fdopen (%0, (char *)%1);'' s m'
    fp <- makeForeignObj ptr (``&freeFile'' :: Addr)
    hndl <- newHandle (htype fp Nothing False)
    hSetBuffering hndl NoBuffering
    return hndl
 where
  m' = 
   case m of 
     ReadMode      -> "r"
     WriteMode     -> "w"
     AppendMode    -> "a"
     ReadWriteMode -> "r+"
  htype = 
   case m of 
     ReadMode      -> ReadHandle
     WriteMode     -> WriteHandle
     AppendMode    -> AppendHandle
     ReadWriteMode -> ReadWriteHandle
#else
socketToHandle (MkSocket s family stype protocol status) m =
  error "socketToHandle not implemented in a parallel setup"
#endif
\end{code}

