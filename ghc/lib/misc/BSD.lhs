%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1997
%
\section[BSD]{Misc BSD bindings}

The @BSD@ module defines Haskell bindings to network programming
functionality that is only provided by BSD-style APIs.

\begin{code}       
{-# OPTIONS -#include "cbits/ghcSockets.h" #-}

module BSD (
       
    HostName,
    getHostName,	    -- :: IO HostName

    ServiceName,
    getServiceByName,	    -- :: ServiceName -> IO ServiceEntry
    getServicePortNumber,   -- :: ServiceName -> IO PortNumber

    ServiceEntry(..),
    getServiceEntry,	    -- :: IO ServiceEntry
    setServiceEntry,	    -- :: Bool -> IO ()
    endServiceEntry,	    -- :: IO ()
    getServiceEntries,	    -- :: Bool -> IO [ServiceEntry]

    ProtocolName,
    ProtocolEntry(..),
    getProtocolByName,	    -- :: ProtocolName   -> IO ProtocolEntry
    getProtocolByNumber,    -- :: ProtocolNumber -> IO ProtcolEntry

    setProtocolEntry,	    -- :: Bool -> IO ()
    getProtocolEntry,	    -- :: IO ProtocolEntry
    endProtocolEntry,	    -- :: IO ()
    getProtocolEntries,	    -- :: Bool -> IO [ProtocolEntry]

    PortNumber,
    getProtocolNumber,	    -- :: ProtocolName -> ProtocolNumber

    HostEntry(..),
    getHostByName,	    -- :: HostName -> IO HostEntry
    getHostByAddr,	    -- :: HostAddress -> Family -> IO HostEntry

    setHostEntry,	    -- :: Bool -> IO ()
    getHostEntry,	    -- :: IO HostEntry
    endHostEntry,	    -- :: IO ()
    getHostEntries,	    -- :: Bool -> IO [HostEntry]

    NetworkName,
    NetworkAddr,
    NetworkEntry(..),
    getNetworkByName,	    -- :: NetworkName -> IO NetworkEntry
    getNetworkByAddr,       -- :: NetworkAddr -> Family -> IO NetworkEntry
    setNetworkEntry,	    -- :: Bool -> IO ()
    getNetworkEntry,	    -- :: IO NetworkEntry
    endNetworkEntry,	    -- :: IO ()
    getNetworkEntries       -- :: Bool -> IO [NetworkEntry]
    
) where


import GlaExts

import PrelIOBase

import Foreign
import Addr
import PackedString ( byteArrayToPS, unpackPS )
  
import PosixUtil  ( strcpy, unvectorize )
import SocketPrim

\end{code}

  
%***************************************************************************
%*                                                                         *
\subsection[BSD-DBTypes]{Service, Protocol \& Host Database Types}
%*                                                                         *
%***************************************************************************

\begin{code}
type HostName = String
type ProtocolName = String
type ProtocolNumber = Int
type ServiceName = String
type PortNumber = Int

data ProtocolEntry = 
  ProtocolEntry
     ProtocolName	-- Official Name
     [ProtocolName]	-- aliases
     Int		-- Protocol Number

data ServiceEntry  = 
  ServiceEntry
     ServiceName	-- Official Name
     [ServiceName]	-- aliases
     PortNumber		-- Port Number
     ProtocolName	-- Protocol
 
data HostEntry = 
  HostEntry
     HostName		-- Official Name
     [HostName]		-- aliases
     Family		-- Host Type (currently AF_INET)
     [HostAddress]	-- Set of Network Addresses

\end{code}

%***************************************************************************
%*                                                                         *
\subsection[BSD-DBAccess]{Service, Protocol Host Database Access}
%*                                                                         *
%***************************************************************************

Calling @getServiceByName@ for a given service and protocol returns the
systems service entry.  This should be used to find the port numbers
for standard protocols such as SMTP and FTP.  The remaining three
functions should be used for browsing the service database
sequentially.

Calling @setServiceEntry@ with \tr{True} indicates that the service
database should be left open between calls to @getServiceEntry@.  To
close the database a call to @endServiceEntry@ is required.  This
database file is usually stored in the file /etc/services.

\begin{code}
getServiceByName :: ServiceName 	-- Service Name
		 -> ProtocolName 	-- Protocol Name
		 -> IO ServiceEntry	-- Service Entry
getServiceByName name proto = do
 ptr <- _ccall_ getservbyname name proto
 if ptr == ``NULL'' 
    then fail (IOError Nothing NoSuchThing "no such service entry")
    else unpackServiceEntry ptr

getServiceByPort :: PortNumber ->	
		    ProtocolName ->
		    IO ServiceEntry
getServiceByPort port proto = do
    ptr <- _ccall_ getservbyport port proto
    if ptr == ``NULL'' 
       then fail (IOError Nothing NoSuchThing "no such service entry")
       else unpackServiceEntry ptr
		   
getServicePortNumber :: ServiceName -> IO PortNumber
getServicePortNumber name = do
    (ServiceEntry _ _ port _) <- getServiceByName name "tcp"
    return port

getServiceEntry	:: IO ServiceEntry
getServiceEntry = do
    ptr <- _ccall_ getservent
    if ptr == ``NULL'' 
       then fail (IOError Nothing NoSuchThing "no such service entry")
       else unpackServiceEntry ptr

setServiceEntry	:: Bool -> IO ()
setServiceEntry flg = _ccall_ setservent stayOpen
 where stayOpen = if flg then 1 else 0

endServiceEntry	:: IO ()
endServiceEntry = _ccall_ endservent

getServiceEntries :: Bool -> IO [ServiceEntry]
getServiceEntries stayOpen = do
  setServiceEntry stayOpen
  getEntries (getServiceEntry) (endServiceEntry)

\end{code}

The following relate directly to the corresponding \tr{UNIX} {C} calls for
returning the protocol entries. The protocol entry is represented by
the Haskell type @ProtocolEntry@.

As for @setServiceEntry@ above, calling @setProtocolEntry@.
determines whether or not the protocol database file, usually
\tr{/etc/protocols}, is to be kept open between calls of
@getProtocolEntry@. Similarly, 

\begin{code}
getProtocolByName   :: ProtocolName -> IO ProtocolEntry
getProtocolByNumber :: PortNumber   -> IO ProtocolEntry
getProtocolNumber   :: ProtocolName -> IO ProtocolNumber

setProtocolEntry    :: Bool -> IO ()	-- Keep DB Open ?
getProtocolEntry    :: IO ProtocolEntry	-- Next Protocol Entry from DB
endProtocolEntry    :: IO ()
getProtocolEntries  :: Bool -> IO [ProtocolEntry]
\end{code}

\begin{code}
--getProtocolByName :: ProtocolName -> IO ProtocolEntry
getProtocolByName name = do
 ptr <- _ccall_ getprotobyname name
 if (ptr == ``NULL'' )
    then fail (IOError Nothing NoSuchThing "no such protocol entry")
    else unpackProtocolEntry ptr

--getProtocolByNumber :: PortNumber -> IO ProtocolEntry
getProtocolByNumber num = do
 ptr <- _ccall_ getprotobynumber num
 if ptr == ``NULL''
    then fail (IOError Nothing NoSuchThing "no such protocol entry")
    else unpackProtocolEntry ptr

--getProtocolNumber :: ProtocolName -> IO ProtocolNumber
getProtocolNumber proto = do
 (ProtocolEntry _ _ num) <- getProtocolByName proto
 return num

--getProtocolEntry :: IO ProtocolEntry	-- Next Protocol Entry from DB
getProtocolEntry = do
 ptr <- _ccall_ getprotoent
 if ptr == ``NULL'' 
    then fail (IOError Nothing NoSuchThing "no such protocol entry")
    else unpackProtocolEntry ptr

--setProtocolEntry :: Bool -> IO ()	-- Keep DB Open ?
setProtocolEntry flg = _ccall_ setprotoent v
 where v = if flg then 1 else 0

--endProtocolEntry :: IO ()
endProtocolEntry = _ccall_ endprotoent

--getProtocolEntries :: Bool -> IO [ProtocolEntry]
getProtocolEntries stayOpen = do
  setProtocolEntry stayOpen
  getEntries (getProtocolEntry) (endProtocolEntry)

\end{code}

\begin{code}
getHostByName :: HostName -> IO HostEntry
getHostByName name = do
    ptr <- _ccall_ gethostbyname name
    if ptr == ``NULL''
       then fail (IOError Nothing NoSuchThing "no such host entry")
       else unpackHostEntry ptr

getHostByAddr :: Family -> HostAddress -> IO HostEntry
getHostByAddr family addr = do
 ptr <- _casm_ ``struct in_addr addr;
	         addr.s_addr = htonl(%0);
	         %r = gethostbyaddr ((char*)&addr, sizeof(struct in_addr), %1);''
               addr
               (packFamily family)
 if ptr == ``NULL'' 
    then fail (IOError Nothing NoSuchThing "no such host entry")
    else unpackHostEntry ptr

getHostEntry :: IO HostEntry
getHostEntry = do
 ptr <- _ccall_ gethostent
 if ptr == ``NULL'' 
    then fail (IOError Nothing NoSuchThing "unable to retrieve host entry")
    else unpackHostEntry ptr

setHostEntry :: Bool -> IO ()
setHostEntry flg = _ccall_ sethostent v
 where v = if flg then 1 else 0

endHostEntry :: IO ()
endHostEntry = _ccall_ endhostent

getHostEntries :: Bool -> IO [HostEntry]
getHostEntries stayOpen = do
  setHostEntry stayOpen
  getEntries (getHostEntry) (endHostEntry)

\end{code}

%***************************************************************************
%*                                                                         *
\subsection[BSD-Network]{Accessing network information}
%*                                                                         *
%***************************************************************************

Same set of access functions as for accessing host,protocol and service
system info, this time for the types of networks supported.

\begin{code}
type NetworkAddr = Word
type NetworkName = String

data NetworkEntry =
  NetworkEntry
     NetworkName   -- official name
     [NetworkName] -- aliases
     Family	   -- type
     NetworkAddr

getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName name = do
 ptr <- _ccall_ getnetbyname name
 if ptr == ``NULL'' 
    then fail (IOError Nothing NoSuchThing "no such network entry")
    else unpackNetworkEntry ptr

getNetworkByAddr :: NetworkAddr -> Family -> IO NetworkEntry
getNetworkByAddr addr family = do
 ptr <-  _casm_ ``long naddr = htonl(%0);
	          %r = getnetbyaddr (naddr, (int)%1);''
                addr
                (packFamily family)
 if ptr == ``NULL''
    then fail (IOError Nothing NoSuchThing "no such network entry")
    else unpackNetworkEntry ptr

getNetworkEntry :: IO NetworkEntry
getNetworkEntry = do
 ptr <- _ccall_ getnetent
 if ptr == ``NULL'' 
   then fail (IOError Nothing NoSuchThing "no more network entries")
   else unpackNetworkEntry ptr

setNetworkEntry :: Bool -> IO ()
setNetworkEntry flg = _ccall_ setnetent v
 where v = if flg then 1 else 0

endNetworkEntry :: IO ()
endNetworkEntry = _ccall_ endnetent

getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries stayOpen = do
  setNetworkEntry stayOpen
  getEntries (getNetworkEntry) (endNetworkEntry)

\end{code}

%***************************************************************************
%*                                                                         *
\subsection[BSD-Misc]{Miscellaneous Functions}
%*                                                                         *
%***************************************************************************
    
Calling @getHostName@ returns the standard host name for the current
processor, as set at boot time.

\begin{code}
getHostName :: IO HostName
getHostName = do
  ptr <- stToIO (newCharArray (0,256))
  rc  <- _casm_ ``%r=gethostname(%0, 256);'' ptr
  ba  <- stToIO (unsafeFreezeByteArray ptr)
  if rc == -1 
     then fail (userError "getHostName: unable to determine host name")
     else return (unpackPS (byteArrayToPS ba))
\end{code}

Helper function used by the exported functions that provides a
Haskellised view of the enumerator functions:

\begin{code}
getEntries :: IO a  -- read
           -> IO () -- at end
	   -> IO [a]
getEntries getOne atEnd = loop
  where
   loop = 
     catch (do { v <- getOne; vs <- loop ; return (v:vs) })
           (\ _ -> do { atEnd; return [] } )
\end{code}


\begin{verbatim}
 struct    servent {
               char *s_name;  /* official name of service */
               char **s_aliases;   /* alias list */
               int  s_port;        /* port service resides at */
               char *s_proto; /* protocol to use */
          };

     The members of this structure are:
          s_name              The official name of the service.
          s_aliases           A zero terminated list of alternate
                              names for the service.
          s_port              The port number at which  the  ser-
                              vice  resides.   Port  numbers  are
                              returned  in  network  short   byte
                              order.
          s_proto             The name of  the  protocol  to  use
                              when contacting the service.
\end{verbatim}

\begin{code}
unpackServiceEntry :: Addr -> PrimIO ServiceEntry
unpackServiceEntry ptr = do
 str     <- _casm_ ``%r = ((struct servent*)%0)->s_name;'' ptr
 name    <- strcpy str
 alias   <- _casm_ ``%r = ((struct servent*)%0)->s_aliases;'' ptr
 aliases <- unvectorize alias 0
  -- Note: port numbers are represented as ints in (struct servent), but
  -- inet port numbers are 16-bit, hence the use of ntohs() rather than ntohl()
 port    <- _casm_ ``%r = (int)ntohs((int)(((struct servent*)%0)->s_port));'' ptr
 str     <- _casm_ ``%r = (char *)((struct servent*)%0)->s_proto;'' ptr
 proto   <- strcpy str
 return (ServiceEntry name aliases port proto)

-------------------------------------------------------------------------------

unpackProtocolEntry :: Addr -> IO ProtocolEntry
unpackProtocolEntry ptr = do
 str     <- _casm_ ``%r = ((struct protoent*)%0)->p_name;'' ptr
 name    <- strcpy str
 alias   <- _casm_ ``%r = ((struct protoent*)%0)->p_aliases;'' ptr
 aliases <- unvectorize alias 0
 proto   <- _casm_ ``%r = ((struct protoent*)%0)->p_proto;'' ptr
 return (ProtocolEntry name aliases proto)

-------------------------------------------------------------------------------

unpackHostEntry :: Addr -> IO HostEntry
unpackHostEntry ptr = do
  str      <- _casm_ ``%r = ((struct hostent*)%0)->h_name;'' ptr
  name     <- strcpy str
  alias    <- _casm_ ``%r = ((struct hostent*)%0)->h_aliases;'' ptr
  aliases  <- unvectorize alias 0
  addrList <- unvectorizeHostAddrs ptr 0
  return (HostEntry name aliases AF_INET addrList)

-------------------------------------------------------------------------------

unpackNetworkEntry :: Addr -> IO NetworkEntry
unpackNetworkEntry ptr = do
  str     <- _casm_ ``%r = ((struct netent*)%0)->n_name;'' ptr
  name    <- strcpy str
  alias   <- _casm_ ``%r = ((struct netent*)%0)->n_aliases;'' ptr
  aliases <- unvectorize alias 0
  fam     <- _casm_ ``%r = ((struct netent*)%0)->n_addrtype;'' ptr
  na      <- _casm_ ``%r = ((struct netent*)%0)->n_net;'' ptr
  return (NetworkEntry name aliases (unpackFamily fam) na)

-------------------------------------------------------------------------------

unvectorizeHostAddrs :: Addr -> Int -> IO [Word]
unvectorizeHostAddrs ptr n 
  | str == ``NULL'' = return []
  | otherwise = do
	x <- _casm_ ``{ unsigned long tmp;
		   if ((((struct hostent*)%0)->h_addr_list[(int)%1]) == NULL)
		      tmp=(W_)0;
		   else
		      tmp = (W_)ntohl(((struct in_addr *)(((struct hostent*)%0)->h_addr_list[(int)%1]))->s_addr); 
		   %r=(W_)tmp;} ''
		ptr n
	xs <- unvectorizeHostAddrs ptr (n+1)
	return (x : xs)
  where str = indexAddrOffAddr ptr n

\end{code}
