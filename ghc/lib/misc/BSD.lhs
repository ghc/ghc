%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1997
%
\section[BSD]{Misc BSD bindings}

The @BSD@ module defines Haskell bindings to functionality
provided by BSD Unix derivatives. Currently this covers
network programming functionality and symbolic links.
(OK, so the latter is pretty much supported by most *nixes
today, but it was BSD that introduced them.)

\begin{code}       
{-# OPTIONS -#include "cbits/ghcSockets.h" -#include "stgio.h" #-}

#include "config.h"

module BSD (
       
    HostName,
    getHostName,	    -- :: IO HostName

    ServiceEntry(..),
    ServiceName,
    getServiceByName,	    -- :: ServiceName -> ProtocolName -> IO ServiceEntry
    getServiceByPort,       -- :: PortNumber  -> ProtocolName -> IO ServiceEntry
    getServicePortNumber,   -- :: ServiceName -> IO PortNumber

#ifndef cygwin32_TARGET_OS
    getServiceEntry,	    -- :: IO ServiceEntry
    setServiceEntry,	    -- :: Bool -> IO ()
    endServiceEntry,	    -- :: IO ()
    getServiceEntries,	    -- :: Bool -> IO [ServiceEntry]
#endif

    ProtocolName,
    ProtocolNumber,
    ProtocolEntry(..),
    getProtocolByName,	    -- :: ProtocolName   -> IO ProtocolEntry
    getProtocolByNumber,    -- :: ProtocolNumber -> IO ProtcolEntry
    getProtocolNumber,	    -- :: ProtocolName   -> ProtocolNumber

#ifndef cygwin32_TARGET_OS
    setProtocolEntry,	    -- :: Bool -> IO ()
    getProtocolEntry,	    -- :: IO ProtocolEntry
    endProtocolEntry,	    -- :: IO ()
    getProtocolEntries,	    -- :: Bool -> IO [ProtocolEntry]
#endif

    PortNumber,
    mkPortNumber,	    -- :: Int -> PortNumber

    HostEntry(..),
    getHostByName,	    -- :: HostName -> IO HostEntry
    getHostByAddr,	    -- :: HostAddress -> Family -> IO HostEntry
    hostAddress,	    -- :: HostEntry -> HostAddress

#ifndef cygwin32_TARGET_OS
    setHostEntry,	    -- :: Bool -> IO ()
    getHostEntry,	    -- :: IO HostEntry
    endHostEntry,	    -- :: IO ()
    getHostEntries,	    -- :: Bool -> IO [HostEntry]
#endif

    NetworkName,
    NetworkAddr,
    NetworkEntry(..)
#ifndef cygwin32_TARGET_OS
    , getNetworkByName	    -- :: NetworkName -> IO NetworkEntry
    , getNetworkByAddr     -- :: NetworkAddr -> Family -> IO NetworkEntry
    , setNetworkEntry	    -- :: Bool -> IO ()
    , getNetworkEntry	    -- :: IO NetworkEntry
    , endNetworkEntry	    -- :: IO ()
    , getNetworkEntries     -- :: Bool -> IO [NetworkEntry]
#endif

#ifdef HAVE_SYMLINK
    , symlink		    -- :: String -> String -> IO ()
#endif
#ifdef HAVE_READLINK
    , readlink		    -- :: String -> IO String
#endif

    ) where


import GlaExts
import PrelIOBase ( IOError (..), IOErrorType(..) )

import Foreign
import Addr
import CString ( unpackCStringIO, unpackCStringBA, unvectorize, unpackNBytesBA )
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
type ServiceName = String

data ProtocolEntry = 
  ProtocolEntry  {
     protoName    :: ProtocolName,	-- Official Name
     protoAliases :: [ProtocolName],	-- aliases
     protoNumber  :: ProtocolNumber	-- Protocol Number
  }

data ServiceEntry  = 
  ServiceEntry  {
     serviceName     :: ServiceName,	-- Official Name
     serviceAliases  :: [ServiceName],	-- aliases
     servicePort     :: PortNumber,	-- Port Number  ( network byte order )
     serviceProtocol :: ProtocolName	-- Protocol
  }

data HostEntry = 
  HostEntry  {
     hostName      :: HostName,  	-- Official Name
     hostAliases   :: [HostName],	-- aliases
     hostFamily    :: Family,	        -- Host Type (currently AF_INET)
     hostAddresses :: [HostAddress]	-- Set of Network Addresses  (in network byte order)
  }

-- convenience function:
hostAddress :: HostEntry -> HostAddress
hostAddress (HostEntry nm _ _ ls) =
 case ls of
   []    -> error ("BSD.hostAddress: empty network address list for " ++ nm)
   (x:_) -> x

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
 if ptr == nullAddr
    then ioError (IOError Nothing NoSuchThing "getServiceByName" "no such service entry")
    else unpackServiceEntry ptr

getServiceByPort :: PortNumber
		 -> ProtocolName
		 -> IO ServiceEntry
getServiceByPort (PNum port) proto = do
    ptr <- _ccall_ getservbyport port proto
    if ptr == nullAddr
       then ioError (IOError Nothing NoSuchThing "getServiceByPort" "no such service entry")
       else unpackServiceEntry ptr
		   
getServicePortNumber :: ServiceName -> IO PortNumber
getServicePortNumber name = do
    (ServiceEntry _ _ port _) <- getServiceByName name "tcp"
    return port

#ifndef cygwin32_TARGET_OS
getServiceEntry	:: IO ServiceEntry
getServiceEntry = do
    ptr <- _ccall_ getservent
    if ptr == nullAddr
       then ioError (IOError Nothing NoSuchThing "getServiceEntry" "no such service entry")
       else unpackServiceEntry ptr

setServiceEntry	:: Bool -> IO ()
setServiceEntry flg = _ccall_ setservent stayOpen
 where stayOpen = (if flg then 1 else 0) :: Int

endServiceEntry	:: IO ()
endServiceEntry = _ccall_ endservent

getServiceEntries :: Bool -> IO [ServiceEntry]
getServiceEntries stayOpen = do
  setServiceEntry stayOpen
  getEntries (getServiceEntry) (endServiceEntry)
#endif
\end{code}

The following relate directly to the corresponding \tr{UNIX} {C} calls for
returning the protocol entries. The protocol entry is represented by
the Haskell type @ProtocolEntry@.

As for @setServiceEntry@ above, calling @setProtocolEntry@.
determines whether or not the protocol database file, usually
\tr{/etc/protocols}, is to be kept open between calls of
@getProtocolEntry@. Similarly, 

\begin{code}
getProtocolByName   :: ProtocolName   -> IO ProtocolEntry
getProtocolByNumber :: ProtocolNumber -> IO ProtocolEntry
getProtocolNumber   :: ProtocolName   -> IO ProtocolNumber

#ifndef cygwin32_TARGET_OS
setProtocolEntry    :: Bool -> IO ()	-- Keep DB Open ?
getProtocolEntry    :: IO ProtocolEntry	-- Next Protocol Entry from DB
endProtocolEntry    :: IO ()
getProtocolEntries  :: Bool -> IO [ProtocolEntry]
#endif
\end{code}

\begin{code}
--getProtocolByName :: ProtocolName -> IO ProtocolEntry
getProtocolByName name = do
 ptr <- _ccall_ getprotobyname name
 if ptr == nullAddr
    then ioError (IOError Nothing NoSuchThing "getProtocolByName" "no such protocol entry")
    else unpackProtocolEntry ptr

--getProtocolByNumber :: ProtocolNumber -> IO ProtocolEntry
getProtocolByNumber num = do
 ptr <- _ccall_ getprotobynumber num
 if ptr == nullAddr
    then ioError (IOError Nothing NoSuchThing "getProtocolByNumber" "no such protocol entry")
    else unpackProtocolEntry ptr

--getProtocolNumber :: ProtocolName -> IO ProtocolNumber
getProtocolNumber proto = do
 (ProtocolEntry _ _ num) <- getProtocolByName proto
 return num

#ifndef cygwin32_TARGET_OS
--getProtocolEntry :: IO ProtocolEntry	-- Next Protocol Entry from DB
getProtocolEntry = do
 ptr <- _ccall_ getprotoent
 if ptr == nullAddr
    then ioError (IOError Nothing NoSuchThing "getProtocolEntry" "no such protocol entry")
    else unpackProtocolEntry ptr

--setProtocolEntry :: Bool -> IO ()	-- Keep DB Open ?
setProtocolEntry flg = _ccall_ setprotoent v
 where v = (if flg then 1 else 0) :: Int

--endProtocolEntry :: IO ()
endProtocolEntry = _ccall_ endprotoent

--getProtocolEntries :: Bool -> IO [ProtocolEntry]
getProtocolEntries stayOpen = do
  setProtocolEntry stayOpen
  getEntries (getProtocolEntry) (endProtocolEntry)
#endif

\end{code}

\begin{code}
getHostByName :: HostName -> IO HostEntry
getHostByName name = do
    ptr <- _ccall_ gethostbyname name
    if ptr == nullAddr
       then ioError (IOError Nothing NoSuchThing "getHostByName" "no such host entry")
       else unpackHostEntry ptr

getHostByAddr :: Family -> HostAddress -> IO HostEntry
getHostByAddr family addr = do
 ptr <- _casm_ ``struct in_addr addr;
	         addr.s_addr = %0;
	         %r = gethostbyaddr ((char*)&addr, sizeof(struct in_addr), %1);''
               addr
               (packFamily family)
 if ptr == nullAddr
    then ioError (IOError Nothing NoSuchThing "getHostByAddr" "no such host entry")
    else unpackHostEntry ptr

#ifndef cygwin32_TARGET_OS
getHostEntry :: IO HostEntry
getHostEntry = do
 ptr <- _ccall_ gethostent
 if ptr == nullAddr
    then ioError (IOError Nothing NoSuchThing "getHostEntry" "unable to retrieve host entry")
    else unpackHostEntry ptr

setHostEntry :: Bool -> IO ()
setHostEntry flg = _ccall_ sethostent v
 where v = (if flg then 1 else 0) :: Int

endHostEntry :: IO ()
endHostEntry = _ccall_ endhostent

getHostEntries :: Bool -> IO [HostEntry]
getHostEntries stayOpen = do
  setHostEntry stayOpen
  getEntries (getHostEntry) (endHostEntry)
#endif
\end{code}

%***************************************************************************
%*                                                                         *
\subsection[BSD-Network]{Accessing network information}
%*                                                                         *
%***************************************************************************

Same set of access functions as for accessing host,protocol and service
system info, this time for the types of networks supported.

\begin{code}
-- network addresses are represented in host byte order.
type NetworkAddr = Word

type NetworkName = String

data NetworkEntry =
  NetworkEntry {
     networkName	:: NetworkName,   -- official name
     networkAliases	:: [NetworkName], -- aliases
     networkFamily	:: Family,	   -- type
     networkAddress	:: NetworkAddr
   }
#ifndef cygwin32_TARGET_OS
getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName name = do
 ptr <- _ccall_ getnetbyname name
 if ptr == nullAddr
    then ioError (IOError Nothing NoSuchThing "getNetworkByName" "no such network entry")
    else unpackNetworkEntry ptr

getNetworkByAddr :: NetworkAddr -> Family -> IO NetworkEntry
getNetworkByAddr addr family = do
 ptr <-  _ccall_ getnetbyaddr addr (packFamily family)
 if ptr == nullAddr
    then ioError (IOError Nothing NoSuchThing "getNetworkByAddr" "no such network entry")
    else unpackNetworkEntry ptr

getNetworkEntry :: IO NetworkEntry
getNetworkEntry = do
 ptr <- _ccall_ getnetent
 if ptr == nullAddr
   then ioError (IOError Nothing NoSuchThing "getNetworkEntry" "no more network entries")
   else unpackNetworkEntry ptr

setNetworkEntry :: Bool -> IO ()
setNetworkEntry flg = _ccall_ setnetent v
 where v = (if flg then 1 else 0) :: Int

endNetworkEntry :: IO ()
endNetworkEntry = _ccall_ endnetent

getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries stayOpen = do
  setNetworkEntry stayOpen
  getEntries (getNetworkEntry) (endNetworkEntry)
#endif

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
  if rc == ((-1)::Int)
     then ioError (userError "getHostName: unable to determine host name")
     else do
       ba  <- stToIO (unsafeFreezeByteArray ptr)
       return (unpackCStringBA ba)
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
 pname   <- _casm_ ``%r = ((struct servent*)%0)->s_name;'' ptr
 name    <- unpackCStringIO pname
 alias   <- _casm_ ``%r = ((struct servent*)%0)->s_aliases;'' ptr
 aliases <- unvectorize alias 0
 port    <- _casm_ ``%r = (int)(((struct servent*)%0)->s_port);'' ptr
 str     <- _casm_ ``%r = (char *)((struct servent*)%0)->s_proto;'' ptr
 proto   <- unpackCStringIO str
 return (ServiceEntry name aliases (PNum port) proto)

-------------------------------------------------------------------------------

unpackProtocolEntry :: Addr -> IO ProtocolEntry
unpackProtocolEntry ptr = do
 str     <- _casm_ ``%r = ((struct protoent*)%0)->p_name;'' ptr
 name    <- unpackCStringIO str
 alias   <- _casm_ ``%r = ((struct protoent*)%0)->p_aliases;'' ptr
 aliases <- unvectorize alias 0
 proto   <- _casm_ ``%r = ((struct protoent*)%0)->p_proto;'' ptr
 return (ProtocolEntry name aliases proto)

-------------------------------------------------------------------------------

unpackHostEntry :: Addr -> IO HostEntry
unpackHostEntry ptr = do
  str      <- _casm_ ``%r = ((struct hostent*)%0)->h_name;'' ptr
  name     <- unpackCStringIO str
  alias    <- _casm_ ``%r = ((struct hostent*)%0)->h_aliases;'' ptr
  aliases  <- unvectorize alias 0
  addrList <- unvectorizeHostAddrs ptr 0
  return (HostEntry name aliases AF_INET addrList)

-------------------------------------------------------------------------------

unpackNetworkEntry :: Addr -> IO NetworkEntry
unpackNetworkEntry ptr = do
  str     <- _casm_ ``%r = ((struct netent*)%0)->n_name;'' ptr
  name    <- unpackCStringIO str
  alias   <- _casm_ ``%r = ((struct netent*)%0)->n_aliases;'' ptr
  aliases <- unvectorize alias 0
  fam     <- _casm_ ``%r = ((struct netent*)%0)->n_addrtype;'' ptr
  na      <- _casm_ ``%r = ((struct netent*)%0)->n_net;'' ptr
  return (NetworkEntry name aliases (unpackFamily fam) na)

-------------------------------------------------------------------------------

unvectorizeHostAddrs :: Addr -> Int -> IO [HostAddress]
unvectorizeHostAddrs ptr n  = do
	x <- _casm_ ``{ unsigned long tmp;
		   if ((((struct hostent*)%0)->h_addr_list[(int)%1]) == NULL)
		      tmp=(W_)0;
		   else
		      tmp = (W_)((struct in_addr *)(((struct hostent*)%0)->h_addr_list[(int)%1]))->s_addr; 
		   %r=(W_)tmp;} ''
		ptr n
	if x == (W# (int2Word# 0#))
	 then return []
	 else do
	   xs <- unvectorizeHostAddrs ptr (n+1)
	   return (x : xs)


\end{code}

%***************************************************************************
%*                                                                         *
\subsection[BSD-symlink]{Symbolic links}
%*                                                                         *
%***************************************************************************


\begin{code}
#ifdef HAVE_SYMLINK
symlink :: String -> String -> IO ()
symlink actual_path sym_path = do
   rc <- _ccall_ symlink actual_path sym_path
   if rc == (0::Int) then
      return ()
    else do
      _ccall_ convertErrno
      cstr <- _ccall_ getErrStr__
      estr <- unpackCStringIO cstr
      ioError (userError ("BSD.symlink: " ++ estr))
#endif

#ifdef HAVE_READLINK
readlink :: String -> IO String
readlink sym = do
   mbuf <- stToIO (newCharArray (0, path_max))
   buf  <- stToIO (unsafeFreezeByteArray mbuf)
   rc  <- _ccall_ readlink sym buf (path_max + 1)
   if rc /= -1 then
      return (unpackNBytesBA buf rc)
    else do
      _ccall_ convertErrno
      cstr <- _ccall_ getErrStr__
      estr <- unpackCStringIO cstr
      ioError (userError ("BSD.readlink: " ++ estr))
 where
  path_max = (``PATH_MAX''::Int)
#endif

\end{code}
