`%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
% Last Modified: Fri Jul 21 12:08:19 1995
% Darren J Moffat <moffatd@dcs.gla.ac.uk>
\section[BSD]{Misc BSD bindings}


\begin{code}       
module BSD (
       
    HostName(..),
    ProtocolName(..),
    ServiceName(..),
    PortNumber(..),
    ProtocolEntry(..),
    ServiceEntry(..),
    HostEntry(..),
--    SelectData(..),

    getHostName,	    -- :: IO String
--    select,		    -- :: SelectData -> IO (Maybe SelectData)

    getServiceByName,	    -- :: ServiceName -> IO ServiceEntry
    getServicePortNumber,   -- :: ServiceName -> IO PortNumber
    getServiceEntry,	    -- :: IO ServiceEntry
    setServiceEntry,	    -- :: Bool -> IO ()
    endServiceEntry,	    -- :: IO ()

    getProtocolByName,	    -- :: ProtocolName -> IO ProtocolEntry
    getProtocolByNumber,    -- :: ProtocolNumber -> IO ProtcolEntry
    getProtocolNumber,	    -- :: ProtocolName -> ProtocolNumber
    getProtocolEntry,	    -- :: IO ProtocolEntry
    setProtocolEntry,	    -- :: Bool -> IO ()
    endProtocolEntry,	    -- :: IO ()

    getHostByName,	    -- :: HostName -> IO HostEntry
    getHostByAddr,	    -- :: Family -> HostAddress -> IO HostEntry
    getHostEntry,	    -- :: IO HostEntry
    setHostEntry,	    -- :: Bool -> IO ()
    endHostEntry,	    -- :: IO ()

    -- make interface self-sufficient:
    Family
) where
  
import LibPosixUtil
import SocketPrim
import PreludePrimIO
import PreludeGlaMisc
import PreludeGlaST
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
data ProtocolEntry = ProtocolEntry		
		     ProtocolName	-- Official Name
		     [ProtocolName]	-- Set of Aliases
		     Int		-- Protocol Number

data ServiceEntry  = ServiceEntry 
                     ServiceName	-- Official Name
		     [ServiceName]	-- Set of Aliases
		     PortNumber		-- Port Number
		     ProtocolName	-- Protocol
 
data HostEntry = HostEntry
     		 HostName		-- Official Name
		 [HostName]		-- Set of Aliases
		 Family			-- Host Type (currently AF_INET)
		 [HostAddress]		-- Set of Network Addresses
\end{code}

    

%***************************************************************************
%*                                                                         *
\subsection[LibSocket-DBAccess]{Service, Protocol Host Database Access}
%*                                                                         *
%***************************************************************************



Calling $getServiceByName$ for a given service and protocol returns the
systems service entry.  This should be used to find the port numbers
for standard protocols such as smtp and FTP.  The remaining three
functions should be used for browsing the service database
sequentially.

Calling $setServiceEntry$ with $True$ indicates that the service
database should be left open between calls to $getServiceEntry$.  To
close the database a call to $endServiceEntry$ is required.  This
database file is usually stored in the file /etc/services.


\begin{code}
getServiceByName :: ServiceName ->	-- Service Name
		    ProtocolName ->	-- Protocol Name
		    IO ServiceEntry	-- Service Entry
getServiceByName name proto = 
	_ccall_ getservbyname name proto	`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such service entry")
	else
	  unpackServiceEntry ptr		`thenPrimIO` \ servent ->
	  return servent

getServiceByPort :: PortNumber ->	
		    ProtocolName ->
		    IO ServiceEntry
getServiceByPort port proto =
	_ccall_ getservbyport port proto	`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such service entry")
	else
	  unpackServiceEntry ptr		`thenPrimIO` \ servent ->
	  return servent
		   
getServicePortNumber :: ServiceName -> IO PortNumber
getServicePortNumber name =
	getServiceByName name "tcp"	>>= \ (ServiceEntry _ _ port _) ->
	return port

getServiceEntry	:: IO ServiceEntry
getServiceEntry =
	_ccall_ getservent			`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such service entry")
	else
	  unpackServiceEntry ptr		`thenPrimIO` \ servent ->
	  return servent

setServiceEntry	:: Bool -> IO ()
setServiceEntry True  =	primIOToIO (_ccall_ setservent 1)
setServiceEntry False = primIOToIO (_ccall_ setservent 0)

endServiceEntry	:: IO ()
endServiceEntry = primIOToIO (_ccall_ endservent)

\end{code}

The following relate directly to the corresponding UNIX C calls for
returning the protocol entries. The protocol entry is represented by
the Haskell type type ProtocolEntry = (String, [String], Int).

As for $setServiceEntry$ above, calling $setProtocolEntry$.
determines whether or not the protocol database file, usually
/etc/protocols, is to be kept open between calls of
$getProtocolEntry$.

\begin{code}
getProtocolByName :: ProtocolName ->	-- Protocol Name
		     IO ProtocolEntry	-- Protocol Entry
getProtocolByName name = 
	_ccall_ getprotobyname name		`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such protocol entry")
	else
	  unpackProtocolEntry ptr		 `thenPrimIO` \ protoent ->
	  return protoent

getProtocolByNumber :: PortNumber ->	-- Protocol Number
		       IO ProtocolEntry	-- Protocol Entry
getProtocolByNumber num = 
	_ccall_ getprotobynumber num		`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such protocol entry")
	else
	  unpackProtocolEntry ptr		 `thenPrimIO` \ protoent ->
	  return protoent

getProtocolNumber :: ProtocolName -> IO ProtocolNumber
getProtocolNumber proto =
	getProtocolByName proto		>>= \ (ProtocolEntry _ _ num) ->
	return num

getProtocolEntry :: IO ProtocolEntry	-- Next Protocol Entry from DB
getProtocolEntry =
	_ccall_ getprotoent			`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such protocol entry")
	else
	  unpackProtocolEntry ptr		`thenPrimIO` \ protoent ->
	  return protoent

setProtocolEntry :: Bool -> IO ()	-- Keep DB Open ?
setProtocolEntry True  = primIOToIO (_ccall_ setprotoent 1)
setProtocolEntry False = primIOToIO (_ccall_ setprotoent 0)

endProtocolEntry :: IO ()
endProtocolEntry = primIOToIO (_ccall_ endprotoent)

\end{code}




\begin{code}
getHostByName :: HostName -> IO HostEntry
getHostByName name = 
	_ccall_ gethostbyname name 		`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such host entry")
	else
	  unpackHostEntry ptr			`thenPrimIO` \ hostent ->
	  return hostent

getHostByAddr :: Family -> HostAddress -> IO HostEntry
getHostByAddr family addr = 
	_casm_ ``%r = gethostbyaddr (%0, sizeof(%0), %1);''
		 addr (packFamily family)	`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such host entry")
	else
	  unpackHostEntry ptr			`thenPrimIO` \ hostent ->
	  return hostent

getHostEntry :: IO HostEntry
getHostEntry = 
	_ccall_ gethostent			`thenPrimIO` \ ptr ->
	if ptr == ``NULL'' then
	  failWith (NoSuchThing "no such host entry")
	else
	  unpackHostEntry ptr			`thenPrimIO` \ hostent ->
	  return hostent

setHostEntry :: Bool -> IO ()
setHostEntry True = primIOToIO (_ccall_ sethostent 1)
setHostEntry False = primIOToIO (_ccall_ sethostent 0)

endHostEntry :: IO ()
endHostEntry = primIOToIO (_ccall_ endprotoent)
\end{code}
    

%***************************************************************************
%*                                                                         *
\subsection[BSD-Misc]{Miscellaneous Functions}
%*                                                                         *
%***************************************************************************

    
The $select$ call is is used to make the process sleep until at least
one of the given handles, is ready for reading, writing or has had an
exception condition raised against it. The handles which are ready are
returned in $SelectData$.

Select will also return after the given timeout, which is given in
nanoseconds, has expired. In this case $Nothing$ is returned.

There is no provision of checking the amount of time remaining since
the $select$ system call does not make this information available on
all systems.  Some always return a zero timeout where others return
the time remaining.

Possible return values from select are then:
\begin{itemize}
\item ([Handle], [Handle], [Handle], Nothing)
\item Nothing
\end{itemize}

\begin{code}
{-
type SelectData = ([Handle],		-- Read Handles
		   [Handle],		-- Write Handles
		   [Handle],		-- Exception Handles
		   Maybe Integer)	-- Timeout
select :: SelectData -> IO (Maybe SelectData)
-}
\end{code}


Calling $getHostName$ returns the standard host name for the current
processor, as set at boot time.

\begin{code}

getHostName :: IO HostName
getHostName =
    newCharArray (0,256)			`thenPrimIO` \ ptr ->
    _casm_ ``%r = gethostname(%0, 256);'' ptr	`seqPrimIO`
    mutByteArr2Addr ptr				`thenPrimIO` \ ptr' ->
    if ptr' == ``NULL'' then
	fail "getHostName: unable to determine hostname"
    else
	return (_unpackPS (_packCString ptr'))
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
unpackServiceEntry :: _Addr -> PrimIO ServiceEntry
unpackServiceEntry ptr =	
	_casm_ ``%r = ((struct servent*)%0)->s_name;'' ptr
						`thenPrimIO` \ str ->
	strcpy str				`thenPrimIO` \ name ->
	_casm_ ``%r = ((struct servent*)%0)->s_aliases;'' ptr
						`thenPrimIO` \ alias ->
	unvectorize alias 0			`thenStrictlyST` \ aliases ->
	_casm_ ``%r = ((struct servent*)%0)->s_port;'' ptr
						`thenPrimIO` \ port ->
	_casm_ ``%r = ((struct servent*)%0)->s_proto;'' ptr
						`thenPrimIO` \ str ->
	strcpy str				`thenPrimIO` \ proto ->

	returnPrimIO (ServiceEntry name aliases port proto)

-------------------------------------------------------------------------------

unpackProtocolEntry :: _Addr -> PrimIO ProtocolEntry
unpackProtocolEntry ptr =
	_casm_ ``%r = ((struct protoent*)%0)->p_name;'' ptr
						`thenPrimIO` \ str ->
	strcpy str				`thenPrimIO` \ name ->
	_casm_ ``%r = ((struct protoent*)%0)->p_aliases;'' ptr
						`thenPrimIO` \ alias ->
	unvectorize alias 0			`thenStrictlyST` \ aliases ->
	_casm_ ``%r = ((struct protoent*)%0)->p_proto;'' ptr
						`thenPrimIO` \ proto ->

	returnPrimIO (ProtocolEntry name aliases proto)


-------------------------------------------------------------------------------

unpackHostEntry :: _Addr -> PrimIO HostEntry
unpackHostEntry ptr =
	_casm_ ``%r = ((struct hostent*)%0)->h_name;'' ptr
						`thenPrimIO` \ str ->
	strcpy str				`thenPrimIO` \ name ->
	_casm_ ``%r = ((struct hostent*)%0)->h_aliases;'' ptr
						`thenPrimIO` \ alias ->
	unvectorize alias 0			`thenStrictlyST` \ aliases ->
{-	_casm_ ``%r = ((struct hostent*)%0)->h_addr_list;'' ptr
						`thenPrimIO` \ addrs ->
	unvectorizeHostAddrs addrs 0		`thenStrictlyST` \ addrList ->
-}	unvectorizeHostAddrs ptr 0		`thenStrictlyST` \ addrList ->
	returnPrimIO (HostEntry name aliases AF_INET addrList)

-------------------------------------------------------------------------------

unvectorizeHostAddrs :: _Addr -> Int -> PrimIO [_Word]
unvectorizeHostAddrs ptr n 
  | str == ``NULL'' = returnPrimIO []
  | otherwise = 
	_casm_ ``%r = (W_)ntohl(((struct hostent*)%0)->h_addr_list[(int)%1]);''
		ptr n				    `thenPrimIO` \ x ->
	unvectorizeHostAddrs ptr (n+1)		    `thenPrimIO` \ xs ->
	returnPrimIO (x : xs)
  where str = indexAddrOffAddr ptr n

-------------------------------------------------------------------------------

mutByteArr2Addr :: _MutableByteArray _RealWorld Int -> PrimIO  _Addr
mutByteArr2Addr arr  = _casm_ `` %r=(void *)%0; '' arr


\end{code}
