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

    sendTo,		-- :: Socket -> String -> SockAddr -> IO Int
    recvFrom,		-- :: Socket -> Int -> IO (String, Int, SockAddr)
--    sendmsg		-- :: Socket -> Message -> MsgFlags -> IO Int
--    recvmsg		-- :: Socket -> MsgFlags -> IO Message


    inet_addr,		-- :: String -> IO HostAddress
    inet_ntoa,		-- :: HostAddress -> IO String

    sIsConnected,	-- :: Socket -> IO Bool
    sIsBound,		-- :: Socket -> IO Bool
    sIsListening,	-- :: Socket -> IO Bool 
    sIsReadable,	-- :: Socket -> IO Bool
    sIsWritable,	-- :: Socket -> IO Bool
    shutdown,		-- :: Socket -> ShutdownCmd -> IO ()
    sClose,		-- :: Socket -> IO ()

    -- socket opts
    SocketOption(..),
    getSocketOption,     -- :: Socket -> SocketOption -> IO Int
    setSocketOption,     -- :: Socket -> SocketOption -> Int -> IO ()

    PortNumber(..),
    mkPortNumber,	    -- :: Int -> PortNumber

-- Special Constants

    aNY_PORT,
    iNADDR_ANY,
    sOMAXCONN,
    maxListenQueue,


-- The following are exported ONLY for use in the BSD module and
-- should not be used anywhere else.

    packFamily, unpackFamily,
    packSocketType,
    packSockAddr, unpackSockAddr

    , withSocketsDo  -- :: IO a -> IO a

) where
 
import GlaExts
import ST
import Ix
import Weak	    ( addForeignFinalizer )
import PrelIOBase  -- IOError, Handle representation
import PrelHandle
import Foreign
import Addr	    ( nullAddr )

import IO
import IOExts	    ( IORef, newIORef, readIORef, writeIORef )
import CString      ( unpackNBytesBAIO,
		      unpackCStringIO,
		      unpackCStringLenIO,
		      allocChars
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

instance Show PortNumber where
  showsPrec p pn = showsPrec p (ntohs pn)

mkPortNumber :: Int -> PortNumber
mkPortNumber v = unsafePerformIO $ do
   po <- _casm_ ``%r=(int)htons((int)%0); '' v
   return (PNum po)

ntohs :: PortNumber -> Int
ntohs (PNum po) = unsafePerformIO (_casm_ ``%r=(int)ntohs((int)%0); '' po)

instance Num PortNumber where
   fromInt     i = mkPortNumber i
   fromInteger i = fromInt (fromInteger i)
    -- for completeness.
   (+) x y   = mkPortNumber (ntohs x + ntohs y)
   (-) x y   = mkPortNumber (ntohs x - ntohs y)
   negate x  = mkPortNumber (-ntohs x)
   (*) x y   = mkPortNumber (ntohs x * ntohs y)
   abs n     = mkPortNumber (abs (ntohs n))
   signum n  = mkPortNumber (signum (ntohs n))

data SockAddr		-- C Names				
#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
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
    case (status::Int) of
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

bindSocket (MkSocket s _family _stype _protocol socketStatus) addr = do
#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
 let isDomainSocket = if _family == AF_UNIX then 1 else (0::Int)
#else
 let isDomainSocket = 0
#endif
 currentStatus <- readIORef socketStatus
 if currentStatus /= NotConnected 
  then
   ioError (userError ("bindSocket: can't peform bind on socket in status " ++
	 show currentStatus))
  else do
   addr' <- packSockAddr addr
   let (_,sz) = boundsOfMutableByteArray addr'
   status <- _ccall_ bindSocket s addr' sz (isDomainSocket::Int)
   case (status::Int) of
     -1 -> constructErrorAndFail "bindSocket"
     _  -> writeIORef socketStatus (Bound)
\end{code}
	

Make a connection to an already opened socket on a given machine and port.
assumes that we have already called createSocket, otherwise it will fail.
			
This is the dual to $bindSocket$.  The {\em server} process will
usually bind to a port number, the {\em client} will then connect to 
the same port number.  Port numbers of user applications are normally
agreed in advance, otherwise we must rely on some meta protocol for telling
the other side what port number we have been allocated.	       

\begin{code}
connect :: Socket	-- Unconnected Socket
	-> SockAddr 	-- Socket address stuff
	-> IO ()

connect (MkSocket s _family _stype _protocol socketStatus) addr = do
#ifndef _WIN32
 let isDomainSocket = if _family == AF_UNIX then 1 else (0::Int)
#else
 let isDomainSocket = 0
#endif
 currentStatus <- readIORef socketStatus
 if currentStatus /= NotConnected 
  then
   ioError (userError ("connect: can't peform connect on socket in status " ++
         show currentStatus))
  else do
   addr' <- packSockAddr addr
   let (_,sz) = boundsOfMutableByteArray addr'
   status <- _ccall_ connectSocket s addr' sz (isDomainSocket::Int)
   case (status::Int) of
     -1 -> constructErrorAndFail "connect"
     _  -> writeIORef socketStatus Connected
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

listen (MkSocket s _family _stype _protocol socketStatus) backlog = do
 currentStatus <- readIORef socketStatus
 if currentStatus /= Bound 
   then
    ioError (userError ("listen: can't peform listen on socket in status " ++
          show currentStatus))
   else do
    status <- _ccall_ listenSocket s backlog
    case (status::Int) of
      -1 -> constructErrorAndFail "listen"
      _  -> writeIORef socketStatus Listening
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
     ioError (userError ("accept: can't peform accept on socket in status " ++
	 show currentStatus))
   else do
     (ptr, sz) <- allocSockAddr family
     int_star <- stToIO (newIntArray ((0::Int),1))
     stToIO (writeIntArray int_star 0 sz)
     new_sock <- _ccall_ acceptSocket s ptr int_star
     case (new_sock::Int) of
	  -1 -> constructErrorAndFail "accept"
	  _  -> do
		a_sz <- stToIO (readIntArray int_star 0)
		addr <- unpackSockAddr ptr a_sz
		new_status <- newIORef Connected
		return ((MkSocket new_sock family stype protocol new_status), addr)
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

writeSocket (MkSocket s _family _stype _protocol status) xs = do
 currentStatus <- readIORef status
 if not ((currentStatus == Connected) || (currentStatus == Listening))
   then
    ioError (userError ("writeSocket: can't peform write on socket in status " ++
          show currentStatus))
   else do
    nbytes <- _ccall_ writeDescriptor s xs (length xs)
    case (nbytes::Int) of
      -1 -> constructErrorAndFail "writeSocket"
      _  -> return nbytes


sendTo :: Socket	-- Bound/Connected Socket
       -> String	-- Data to send
       -> SockAddr
       -> IO Int	-- Number of Bytes sent

sendTo (MkSocket s _family _stype _protocol status) xs addr = do
 currentStatus <- readIORef status
 if not ((currentStatus == Connected) || (currentStatus == Listening) || (currentStatus == Bound))
   then
    ioError (userError ("sendTo: can't peform write on socket in status " ++
          show currentStatus))
   else do
    addr' <- packSockAddr addr
    let (_,sz) = boundsOfMutableByteArray addr'
    nbytes <- _ccall_ sendTo__ s xs (length xs) addr' sz
    case (nbytes::Int) of
      -1 -> constructErrorAndFail "sendTo"
      _  -> return nbytes

readSocket :: Socket		-- Connected (or bound) Socket
	   -> Int		-- Number of Bytes to Read
	   -> IO (String, Int)	-- (Data Read, Number of Bytes)

readSocket (MkSocket s _family _stype _protocol status) nbytes = do
 currentStatus <- readIORef status
 if not ((currentStatus == Connected) || (currentStatus == Listening))
   then
    ioError (userError ("readSocket: can't perform read on socket in status " ++
	  show currentStatus))
   else do
    ptr  <- allocChars nbytes
    rlen <- _ccall_ readDescriptor s ptr nbytes
    case (rlen::Int) of
      -1 -> constructErrorAndFail "readSocket"
      n  -> do
	    barr <- stToIO (unsafeFreezeByteArray ptr)
	    str  <- unpackNBytesBAIO barr n
            return (str, n)

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

recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom (MkSocket s _family _stype _protocol status) nbytes = do
 currentStatus <- readIORef status
 if not ((currentStatus == Connected) || (currentStatus == Listening) || (currentStatus == Bound))
   then
    ioError (userError ("recvFrom: can't perform read on socket in status " ++
	  show currentStatus))
   else do
    ptr    <- allocChars nbytes 
    (ptr_addr,_) <- allocSockAddr AF_INET
    rlen   <- _ccall_ recvFrom__ s ptr nbytes ptr_addr
    case (rlen::Int) of
      -1 -> constructErrorAndFail "recvFrom"
      n  -> do
	    barr <- stToIO (unsafeFreezeByteArray ptr)
	    addr <- unpackSockAddrInet ptr_addr
	    str  <- unpackNBytesBAIO barr n
            return (str, n, addr)

\end{code}

The port number the given socket is currently connected to can be
determined by calling $port$, is generally only useful when bind
was given $aNY\_PORT$.

\begin{code}
socketPort :: Socket		-- Connected & Bound Socket
	   -> IO PortNumber	-- Port Number of Socket
socketPort sock@(MkSocket _ AF_INET _ _ _) =
    getSocketName sock >>= \(SockAddrInet port _) ->
    return port
socketPort (MkSocket _ family _ _ _) =
    ioError (userError ("socketPort: not supported for Family " ++ show family))
\end{code}

Calling $getPeerName$ returns the address details of the machine,
other than the local one, which is connected to the socket. This is
used in programs such as FTP to determine where to send the returning
data.  The corresponding call to get the details of the local machine
is $getSocketName$.

\begin{code}
getPeerName   :: Socket -> IO SockAddr

getPeerName (MkSocket s family _ _ _) = do
 (ptr, a_sz) <- allocSockAddr family
 int_star <- stToIO (newIntArray ((0::Int),1))
 stToIO (writeIntArray int_star 0 a_sz)
 status <- _ccall_ getPeerName s ptr int_star
 case (status::Int) of
   -1 -> constructErrorAndFail "getPeerName"
   _  -> do
	  sz <- stToIO (readIntArray int_star 0)
	  unpackSockAddr ptr sz
    
getSocketName :: Socket -> IO SockAddr

getSocketName (MkSocket s family _ _ _) = do
 (ptr, a_sz) <- allocSockAddr family
 int_star <- stToIO (newIntArray ((0::Int),1))
 stToIO (writeIntArray int_star 0 a_sz)
 rc <- _ccall_ getSockName s ptr int_star
 case (rc::Int) of
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
data SocketOption
    = Debug         {- SO_DEBUG     -}
    | ReuseAddr     {- SO_REUSEADDR -}
    | Type          {- SO_TYPE      -}
    | SoError       {- SO_ERROR     -}
    | DontRoute     {- SO_DONTROUTE -}
    | Broadcast     {- SO_BROADCAST -}
    | SendBuffer    {- SO_SNDBUF    -}
    | RecvBuffer    {- SO_RCVBUF    -}
    | KeepAlive     {- SO_KEEPALIVE -}
    | OOBInline     {- SO_OOBINLINE -}
#ifndef _WIN32
    | MaxSegment    {- TCP_MAXSEG   -}
#endif
    | NoDelay       {- TCP_NODELAY  -}
--    | Linger        {- SO_LINGER    -}
#if 0
    | ReusePort     {- SO_REUSEPORT -}	-- BSD only?
    | RecvLowWater  {- SO_RCVLOWAT  -}
    | SendLowWater  {- SO_SNDLOWAT  -}
    | RecvTimeOut   {- SO_RCVTIMEO  -}
    | SendTimeOut   {- SO_SNDTIMEO  -}
    | UseLoopBack   {- SO_USELOOPBACK -}  -- not used, I believe.
#endif

packSocketOption :: SocketOption -> Int
packSocketOption so =
  case so of
    Debug         -> ``SO_DEBUG''
    ReuseAddr     -> ``SO_REUSEADDR''
    Type          -> ``SO_TYPE''
    SoError       -> ``SO_ERROR''
    DontRoute     -> ``SO_DONTROUTE''
    Broadcast     -> ``SO_BROADCAST''
    SendBuffer    -> ``SO_SNDBUF''
    RecvBuffer    -> ``SO_RCVBUF''
    KeepAlive     -> ``SO_KEEPALIVE''
    OOBInline     -> ``SO_OOBINLINE''
#ifndef _WIN32
    MaxSegment    -> ``TCP_MAXSEG''
#endif
    NoDelay       -> ``TCP_NODELAY''
#if 0
    ReusePort     -> ``SO_REUSEPORT''	-- BSD only?
    RecvLowWater  -> ``SO_RCVLOWAT''
    SendLowWater  -> ``SO_SNDLOWAT''
    RecvTimeOut   -> ``SO_RCVTIMEO''
    SendTimeOut   -> ``SO_SNDTIMEO''
    UseLoopBack   -> ``SO_USELOOPBACK''
#endif

setSocketOption :: Socket 
		-> SocketOption -- Option Name
		-> Int		 -- Option Value
		-> IO ()
setSocketOption (MkSocket s _ _ _ _) so v = do
   rc <- _ccall_ setSocketOption__ s (packSocketOption so) v
   if rc /= (0::Int)
    then constructErrorAndFail "setSocketOption"
    else return ()

getSocketOption :: Socket
		-> SocketOption  -- Option Name
		-> IO Int	  -- Option Value
getSocketOption (MkSocket s _ _ _ _) so = do
   rc <- _ccall_ getSocketOption__ s (packSocketOption so)
   if rc == -1 -- let's just hope that value isn't taken..
    then constructErrorAndFail "getSocketOption"
    else return rc

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

#if defined(cygwin32_TARGET_OS) || defined(mingw32_TARGET_OS)
 
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

#if freebsd2_TARGET_OS || freebsd3_TARGET_OS

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
	aix_TARGET_OS || freebsd2_TARGET_OS || freebsd3_TARGET_OS
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

#if defined(mingw32_TARGET_OS) || defined(cygwin32_TARGET_OS)
data SocketType = 
	  Stream 
	| Datagram
	| Raw 
	| RDM       -- reliably delivered msg
	| SeqPacket
	deriving (Eq, Ord, Ix, Show)
	
packSocketType stype =
 case stype of 
   Stream    -> ``SOCK_STREAM''
   Datagram  -> ``SOCK_DGRAM''
   Raw       -> ``SOCK_RAW''
   RDM       -> ``SOCK_RDM'' 
   SeqPacket -> ``SOCK_SEQPACKET''

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
aNY_PORT :: PortNumber 
aNY_PORT = mkPortNumber 0

iNADDR_ANY :: HostAddress
iNADDR_ANY = unsafePerformIO (_casm_ `` %r = htonl(INADDR_ANY); '')

sOMAXCONN :: Int
sOMAXCONN = ``SOMAXCONN''

maxListenQueue :: Int
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
  case (status::Int) of
    -1 -> constructErrorAndFail "shutdown"
    _  -> return ()

-------------------------------------------------------------------------------

sClose	 :: Socket -> IO ()
sClose (MkSocket s _ _ _ _) = _ccall_ close s

-------------------------------------------------------------------------------

sIsConnected :: Socket -> IO Bool
sIsConnected (MkSocket _ _ _ _ status) = do
    value <- readIORef status
    return (value == Connected)	

-------------------------------------------------------------------------------

sIsBound :: Socket -> IO Bool
sIsBound (MkSocket _ _ _ _ status) = do
    value <- readIORef status
    return (value == Bound)	

-------------------------------------------------------------------------------

sIsListening :: Socket -> IO Bool
sIsListening (MkSocket _ _ _  _ status) = do
    value <- readIORef status
    return (value == Listening)	

-------------------------------------------------------------------------------

sIsReadable  :: Socket -> IO Bool
sIsReadable (MkSocket _ _ _ _ status) = do
    value <- readIORef status
    return (value == Listening || value == Connected)

-------------------------------------------------------------------------------

sIsWritable  :: Socket -> IO Bool
sIsWritable = sIsReadable -- sort of.

-------------------------------------------------------------------------------

sIsAcceptable :: Socket -> IO Bool
#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
sIsAcceptable (MkSocket _ AF_UNIX Stream _ status) = do
    value <- readIORef status
    return (value == Connected || value == Bound || value == Listening)
sIsAcceptable (MkSocket _ AF_UNIX _ _ _) = return False
#endif
sIsAcceptable (MkSocket _ _ _ _ status) = do
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
    then ioError (userError ("inet_addr: Malformed address: " ++ ipstr))
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

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
allocSockAddr AF_UNIX = do
    ptr <- allocChars ``sizeof(struct sockaddr_un)''
    let (_,sz) = boundsOfMutableByteArray ptr
    return (ptr, sz)
#endif

allocSockAddr AF_INET = do
    ptr <- allocChars ``sizeof(struct sockaddr_in)''
    let (_,sz) = boundsOfMutableByteArray ptr
    return (ptr, sz)

-------------------------------------------------------------------------------

unpackSockAddr :: MutableByteArray RealWorld Int -> Int -> IO SockAddr
unpackSockAddr arr len = do
    fam <- _casm_ ``%r = ((struct sockaddr*)%0)->sa_family;'' arr
    case unpackFamily fam of
#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
	AF_UNIX -> unpackSockAddrUnix arr (len - ``sizeof(short)'')
#endif
	AF_INET -> unpackSockAddrInet arr

-------------------------------------------------------------------------------

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)

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
#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
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

socketToHandle (MkSocket fd _ _ _ _) m = do
    fileobj <- _ccall_ openFd fd (file_mode::Int) (file_flags::Int)
    if fileobj == nullAddr then
       ioError (userError "socketHandle: Failed to open file desc")
     else do
       fo <- mkForeignObj fileobj
       addForeignFinalizer fo (freeFileObject fo)
       mkBuffer__ fo 0  -- not buffered
       hndl <- newHandle (Handle__ fo htype NoBuffering socket_str)
       return hndl
 where
  socket_str = "<socket: "++show fd
#ifdef _WIN32
  file_flags = flush_on_close + 1024{-I'm a socket fd, me!-}
#else
  file_flags = flush_on_close
#endif

  (flush_on_close, file_mode) =
   case m of 
           AppendMode    -> (1, 0)
           WriteMode     -> (1, 1)
           ReadMode      -> (0, 2)
           ReadWriteMode -> (1, 3)

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

If you're using WinSock, the programmer has to call a startup
routine before starting to use the goods. So, if you want to
stay portable across all ghc-supported platforms, you have to
use @withSocketsDo@...:

\begin{code}
withSocketsDo :: IO a -> IO a
#if !defined(HAVE_WINSOCK_H) || defined(cygwin32_TARGET_OS)
withSocketsDo x = x
#else
withSocketsDo act = do
   x <- initWinSock
   if ( x /= 0 ) then
     ioError (userError "Failed to initialise WinSock")
    else do
      v <- act
      shutdownWinSock
      return v

foreign import "initWinSock" initWinSock :: IO Int
foreign import "shutdownWinSock" shutdownWinSock :: IO ()

#endif

\end{code}
