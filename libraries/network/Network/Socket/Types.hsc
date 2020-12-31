{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Types (
    -- * Socket type
      Socket
    , withFdSocket
    , unsafeFdSocket
    , touchSocket
    , socketToFd
    , fdSocket
    , mkSocket
    , invalidateSocket
    , close
    , close'
    , c_close
    -- * Types of socket
    , SocketType(..)
    , isSupportedSocketType
    , packSocketType
    , packSocketType'
    , packSocketTypeOrThrow
    , unpackSocketType
    , unpackSocketType'

    -- * Family
    , Family(..)
    , isSupportedFamily
    , packFamily
    , unpackFamily

    -- * Socket address typeclass
    , SocketAddress(..)
    , withSocketAddress
    , withNewSocketAddress

    -- * Socket address type
    , SockAddr(..)
    , isSupportedSockAddr
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    , FlowInfo
    , ScopeID
    , peekSockAddr
    , pokeSockAddr
    , withSockAddr
    -- * Unsorted
    , ProtocolNumber
    , defaultProtocol
    , PortNumber
    , defaultPort

    -- * Low-level helpers
    , zeroMemory
    , htonl
    , ntohl
    ) where

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef', mkWeakIORef)
import Foreign.C.Error (throwErrno)
import Foreign.Marshal.Alloc
import GHC.Conc (closeFdWith)
import System.Posix.Types (Fd)
import Control.DeepSeq (NFData (..))
import GHC.Exts (touch##)
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import GHC.IO (IO (..))

#if defined(DOMAIN_SOCKET_SUPPORT)
import Foreign.Marshal.Array
#endif

import Network.Socket.Imports

-----------------------------------------------------------------------------

-- | Basic type for a socket.
data Socket = Socket !(IORef CInt) !CInt {- for Show -}

instance Show Socket where
    show (Socket _ ofd) = "<socket: " ++ show ofd ++ ">"

instance Eq Socket where
    Socket ref1 _ == Socket ref2 _ = ref1 == ref2

{-# DEPRECATED fdSocket "Use withFdSocket or unsafeFdSocket instead" #-}
-- | Currently, this is an alias of `unsafeFdSocket`.
fdSocket :: Socket -> IO CInt
fdSocket = unsafeFdSocket

-- | Getting a file descriptor from a socket.
--
--   If a 'Socket' is shared with multiple threads and
--   one uses 'unsafeFdSocket', unexpected issues may happen.
--   Consider the following scenario:
--
--   1) Thread A acquires a 'Fd' from 'Socket' by 'unsafeFdSocket'.
--
--   2) Thread B close the 'Socket'.
--
--   3) Thread C opens a new 'Socket'. Unfortunately it gets the same 'Fd'
--      number which thread A is holding.
--
--   In this case, it is safer for Thread A to clone 'Fd' by
--   'System.Posix.IO.dup'. But this would still suffer from
--   a race condition between 'unsafeFdSocket' and 'close'.
--
--   If you use this function, you need to guarantee that the 'Socket' does not
--   get garbage-collected until after you finish using the file descriptor.
--   'touchSocket' can be used for this purpose.
--
--   A safer option is to use 'withFdSocket' instead.
unsafeFdSocket :: Socket -> IO CInt
unsafeFdSocket (Socket ref _) = readIORef ref

-- | Ensure that the given 'Socket' stays alive (i.e. not garbage-collected)
--   at the given place in the sequence of IO actions. This function can be
--   used in conjunction with 'unsafeFdSocket' to guarantee that the file
--   descriptor is not prematurely freed.
--
-- > fd <- unsafeFdSocket sock
-- > -- using fd with blocking operations such as accept(2)
-- > touchSocket sock
touchSocket :: Socket -> IO ()
touchSocket (Socket ref _) = touch ref

touch :: IORef a -> IO ()
touch (IORef (STRef mutVar)) =
  -- Thanks to a GHC issue, this touch# may not be quite guaranteed
  -- to work. There's talk of replacing the touch# primop with one
  -- that works better with the optimizer. But this seems to be the
  -- "right" way to do it for now.
  IO $ \s -> (## touch## mutVar s, () ##)

-- | Get a file descriptor from a 'Socket'. The socket will never
-- be closed automatically before @withFdSocket@ completes, but
-- it may still be closed by an explicit call to 'close' or `close'`,
-- either before or during the call.
--
-- The file descriptor must not be used after @withFdSocket@ returns, because
-- the 'Socket' may have been garbage-collected, invalidating the file
-- descriptor.
--
-- Since: 3.1.0.0
withFdSocket :: Socket -> (CInt -> IO r) -> IO r
withFdSocket (Socket ref _) f = do
  fd <- readIORef ref
  -- Should we throw an exception if the socket is already invalid?
  -- That will catch some mistakes but certainly not all.

  r <- f fd

  touch ref
  return r

-- | Socket is closed and a duplicated file descriptor is returned.
--   The duplicated descriptor is no longer subject to the possibility
--   of unexpectedly being closed if the socket is finalized. It is
--   now the caller's responsibility to ultimately close the
--   duplicated file descriptor.
socketToFd :: Socket -> IO CInt
socketToFd s = do
#if defined(mingw32_HOST_OS)
    fd <- unsafeFdSocket s
    fd2 <- c_wsaDuplicate fd
    -- FIXME: throw error no if -1
    close s
    return fd2

foreign import ccall unsafe "wsaDuplicate"
   c_wsaDuplicate :: CInt -> IO CInt
#else
    fd <- unsafeFdSocket s
    -- FIXME: throw error no if -1
    fd2 <- c_dup fd
    close s
    return fd2

foreign import ccall unsafe "dup"
   c_dup :: CInt -> IO CInt
#endif

-- | Creating a socket from a file descriptor.
mkSocket :: CInt -> IO Socket
mkSocket fd = do
    ref <- newIORef fd
    let s = Socket ref fd
    void $ mkWeakIORef ref $ close s
    return s

invalidSocket :: CInt
#if defined(mingw32_HOST_OS)
invalidSocket = #const INVALID_SOCKET
#else
invalidSocket = -1
#endif

invalidateSocket ::
      Socket
   -> (CInt -> IO a)
   -> (CInt -> IO a)
   -> IO a
invalidateSocket (Socket ref _) errorAction normalAction = do
    oldfd <- atomicModifyIORef' ref $ \cur -> (invalidSocket, cur)
    if oldfd == invalidSocket then errorAction oldfd else normalAction oldfd

-----------------------------------------------------------------------------

-- | Close the socket. This function does not throw exceptions even if
--   the underlying system call returns errors.
--
--   If multiple threads use the same socket and one uses 'unsafeFdSocket' and
--   the other use 'close', unexpected behavior may happen.
--   For more information, please refer to the documentation of 'unsafeFdSocket'.
close :: Socket -> IO ()
close s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- closeFdWith avoids the deadlock of IO manager.
    closeFdWith closeFd (toFd oldfd)
  where
    toFd :: CInt -> Fd
    toFd = fromIntegral
    -- closeFd ignores the return value of c_close and
    -- does not throw exceptions
    closeFd :: Fd -> IO ()
    closeFd = void . c_close . fromIntegral

-- | Close the socket. This function throws exceptions if
--   the underlying system call returns errors.
close' :: Socket -> IO ()
close' s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- closeFdWith avoids the deadlock of IO manager.
    closeFdWith closeFd (toFd oldfd)
  where
    toFd :: CInt -> Fd
    toFd = fromIntegral
    closeFd :: Fd -> IO ()
    closeFd fd = do
        ret <- c_close $ fromIntegral fd
        when (ret == -1) $ throwErrno "Network.Socket.close'"

#if defined(mingw32_HOST_OS)
foreign import CALLCONV unsafe "closesocket"
  c_close :: CInt -> IO CInt
#else
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt
#endif

-----------------------------------------------------------------------------

-- | Protocol number.
type ProtocolNumber = CInt

-- | This is the default protocol for a given service.
--
-- >>> defaultProtocol
-- 0
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

-----------------------------------------------------------------------------
-- Socket types

-- There are a few possible ways to do this.  The first is convert the
-- structs used in the C library into an equivalent Haskell type. An
-- other possible implementation is to keep all the internals in the C
-- code and use an Int## and a status flag. The second method is used
-- here since a lot of the C structures are not required to be
-- manipulated.

-- Originally the status was non-mutable so we had to return a new
-- socket each time we changed the status.  This version now uses
-- mutable variables to avoid the need to do this.  The result is a
-- cleaner interface and better security since the application
-- programmer now can't circumvent the status information to perform
-- invalid operations on sockets.

-- | Socket Types.
--
-- The existence of a constructor does not necessarily imply that that
-- socket type is supported on your system: see 'isSupportedSocketType'.
data SocketType
        = NoSocketType -- ^ 0, used in getAddrInfo hints, for example
        | Stream -- ^ SOCK_STREAM
        | Datagram -- ^ SOCK_DGRAM
        | Raw -- ^ SOCK_RAW
        | RDM -- ^ SOCK_RDM
        | SeqPacket -- ^ SOCK_SEQPACKET
        deriving (Eq, Ord, Read, Show, Typeable)

-- | Does the SOCK_ constant corresponding to the given SocketType exist on
-- this system?
isSupportedSocketType :: SocketType -> Bool
isSupportedSocketType = isJust . packSocketType'

-- | Find the SOCK_ constant corresponding to the SocketType value.
packSocketType' :: SocketType -> Maybe CInt
packSocketType' stype = case Just stype of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just NoSocketType -> Just 0
#ifdef SOCK_STREAM
    Just Stream -> Just #const SOCK_STREAM
#endif
#ifdef SOCK_DGRAM
    Just Datagram -> Just #const SOCK_DGRAM
#endif
#ifdef SOCK_RAW
    Just Raw -> Just #const SOCK_RAW
#endif
#ifdef SOCK_RDM
    Just RDM -> Just #const SOCK_RDM
#endif
#ifdef SOCK_SEQPACKET
    Just SeqPacket -> Just #const SOCK_SEQPACKET
#endif

packSocketType :: SocketType -> CInt
packSocketType stype = fromMaybe (error errMsg) (packSocketType' stype)
  where
    errMsg = concat ["Network.Socket.packSocketType: ",
                     "socket type ", show stype, " unsupported on this system"]

-- | Try packSocketType' on the SocketType, if it fails throw an error with
-- message starting "Network.Socket." ++ the String parameter
packSocketTypeOrThrow :: String -> SocketType -> IO CInt
packSocketTypeOrThrow caller stype = maybe err return (packSocketType' stype)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller, ": ",
    "socket type ", show stype, " unsupported on this system"]


unpackSocketType:: CInt -> Maybe SocketType
unpackSocketType t = case t of
        0 -> Just NoSocketType
#ifdef SOCK_STREAM
        (#const SOCK_STREAM) -> Just Stream
#endif
#ifdef SOCK_DGRAM
        (#const SOCK_DGRAM) -> Just Datagram
#endif
#ifdef SOCK_RAW
        (#const SOCK_RAW) -> Just Raw
#endif
#ifdef SOCK_RDM
        (#const SOCK_RDM) -> Just RDM
#endif
#ifdef SOCK_SEQPACKET
        (#const SOCK_SEQPACKET) -> Just SeqPacket
#endif
        _ -> Nothing

-- | Try unpackSocketType on the CInt, if it fails throw an error with
-- message starting "Network.Socket." ++ the String parameter
unpackSocketType' :: String -> CInt -> IO SocketType
unpackSocketType' caller ty = maybe err return (unpackSocketType ty)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller, ": ",
    "socket type ", show ty, " unsupported on this system"]

------------------------------------------------------------------------
-- Protocol Families.

-- | Address families.
--
-- A constructor being present here does not mean it is supported by the
-- operating system: see 'isSupportedFamily'.
data Family
    = AF_UNSPEC           -- ^ unspecified
    | AF_UNIX             -- ^ UNIX-domain
    | AF_INET             -- ^ Internet Protocol version 4
    | AF_INET6            -- ^ Internet Protocol version 6
    | AF_IMPLINK          -- ^ Arpanet imp addresses
    | AF_PUP              -- ^ pup protocols: e.g. BSP
    | AF_CHAOS            -- ^ mit CHAOS protocols
    | AF_NS               -- ^ XEROX NS protocols
    | AF_NBS              -- ^ nbs protocols
    | AF_ECMA             -- ^ european computer manufacturers
    | AF_DATAKIT          -- ^ datakit protocols
    | AF_CCITT            -- ^ CCITT protocols, X.25 etc
    | AF_SNA              -- ^ IBM SNA
    | AF_DECnet           -- ^ DECnet
    | AF_DLI              -- ^ Direct data link interface
    | AF_LAT              -- ^ LAT
    | AF_HYLINK           -- ^ NSC Hyperchannel
    | AF_APPLETALK        -- ^ Apple Talk
    | AF_ROUTE            -- ^ Internal Routing Protocol (aka AF_NETLINK)
    | AF_NETBIOS          -- ^ NetBios-style addresses
    | AF_NIT              -- ^ Network Interface Tap
    | AF_802              -- ^ IEEE 802.2, also ISO 8802
    | AF_ISO              -- ^ ISO protocols
    | AF_OSI              -- ^ umbrella of all families used by OSI
    | AF_NETMAN           -- ^ DNA Network Management
    | AF_X25              -- ^ CCITT X.25
    | AF_AX25             -- ^ AX25
    | AF_OSINET           -- ^ AFI
    | AF_GOSSIP           -- ^ US Government OSI
    | AF_IPX              -- ^ Novell Internet Protocol
    | Pseudo_AF_XTP       -- ^ eXpress Transfer Protocol (no AF)
    | AF_CTF              -- ^ Common Trace Facility
    | AF_WAN              -- ^ Wide Area Network protocols
    | AF_SDL              -- ^ SGI Data Link for DLPI
    | AF_NETWARE          -- ^ Netware
    | AF_NDD              -- ^ NDD
    | AF_INTF             -- ^ Debugging use only
    | AF_COIP             -- ^ connection-oriented IP, aka ST II
    | AF_CNT              -- ^ Computer Network Technology
    | Pseudo_AF_RTIP      -- ^ Help Identify RTIP packets
    | Pseudo_AF_PIP       -- ^ Help Identify PIP packets
    | AF_SIP              -- ^ Simple Internet Protocol
    | AF_ISDN             -- ^ Integrated Services Digital Network
    | Pseudo_AF_KEY       -- ^ Internal key-management function
    | AF_NATM             -- ^ native ATM access
    | AF_ARP              -- ^ ARP (RFC 826)
    | Pseudo_AF_HDRCMPLT  -- ^ Used by BPF to not rewrite hdrs in iface output
    | AF_ENCAP            -- ^ ENCAP
    | AF_LINK             -- ^ Link layer interface
    | AF_RAW              -- ^ Link layer interface
    | AF_RIF              -- ^ raw interface
    | AF_NETROM           -- ^ Amateur radio NetROM
    | AF_BRIDGE           -- ^ multiprotocol bridge
    | AF_ATMPVC           -- ^ ATM PVCs
    | AF_ROSE             -- ^ Amateur Radio X.25 PLP
    | AF_NETBEUI          -- ^ Netbeui 802.2LLC
    | AF_SECURITY         -- ^ Security callback pseudo AF
    | AF_PACKET           -- ^ Packet family
    | AF_ASH              -- ^ Ash
    | AF_ECONET           -- ^ Acorn Econet
    | AF_ATMSVC           -- ^ ATM SVCs
    | AF_IRDA             -- ^ IRDA sockets
    | AF_PPPOX            -- ^ PPPoX sockets
    | AF_WANPIPE          -- ^ Wanpipe API sockets
    | AF_BLUETOOTH        -- ^ bluetooth sockets
    | AF_CAN              -- ^ Controller Area Network
      deriving (Eq, Ord, Read, Show)

-- | Converting 'Family' to 'CInt'.
packFamily :: Family -> CInt
packFamily f = case packFamily' f of
    Just fam -> fam
    Nothing -> error $
               "Network.Socket.packFamily: unsupported address family: " ++
               show f

-- | Does the AF_ constant corresponding to the given family exist on this
-- system?
isSupportedFamily :: Family -> Bool
isSupportedFamily = isJust . packFamily'

packFamily' :: Family -> Maybe CInt
packFamily' f = case Just f of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just AF_UNSPEC -> Just #const AF_UNSPEC
#ifdef AF_UNIX
    Just AF_UNIX -> Just #const AF_UNIX
#endif
#ifdef AF_INET
    Just AF_INET -> Just #const AF_INET
#endif
#ifdef AF_INET6
    Just AF_INET6 -> Just #const AF_INET6
#endif
#ifdef AF_IMPLINK
    Just AF_IMPLINK -> Just #const AF_IMPLINK
#endif
#ifdef AF_PUP
    Just AF_PUP -> Just #const AF_PUP
#endif
#ifdef AF_CHAOS
    Just AF_CHAOS -> Just #const AF_CHAOS
#endif
#ifdef AF_NS
    Just AF_NS -> Just #const AF_NS
#endif
#ifdef AF_NBS
    Just AF_NBS -> Just #const AF_NBS
#endif
#ifdef AF_ECMA
    Just AF_ECMA -> Just #const AF_ECMA
#endif
#ifdef AF_DATAKIT
    Just AF_DATAKIT -> Just #const AF_DATAKIT
#endif
#ifdef AF_CCITT
    Just AF_CCITT -> Just #const AF_CCITT
#endif
#ifdef AF_SNA
    Just AF_SNA -> Just #const AF_SNA
#endif
#ifdef AF_DECnet
    Just AF_DECnet -> Just #const AF_DECnet
#endif
#ifdef AF_DLI
    Just AF_DLI -> Just #const AF_DLI
#endif
#ifdef AF_LAT
    Just AF_LAT -> Just #const AF_LAT
#endif
#ifdef AF_HYLINK
    Just AF_HYLINK -> Just #const AF_HYLINK
#endif
#ifdef AF_APPLETALK
    Just AF_APPLETALK -> Just #const AF_APPLETALK
#endif
#ifdef AF_ROUTE
    Just AF_ROUTE -> Just #const AF_ROUTE
#endif
#ifdef AF_NETBIOS
    Just AF_NETBIOS -> Just #const AF_NETBIOS
#endif
#ifdef AF_NIT
    Just AF_NIT -> Just #const AF_NIT
#endif
#ifdef AF_802
    Just AF_802 -> Just #const AF_802
#endif
#ifdef AF_ISO
    Just AF_ISO -> Just #const AF_ISO
#endif
#ifdef AF_OSI
    Just AF_OSI -> Just #const AF_OSI
#endif
#ifdef AF_NETMAN
    Just AF_NETMAN -> Just #const AF_NETMAN
#endif
#ifdef AF_X25
    Just AF_X25 -> Just #const AF_X25
#endif
#ifdef AF_AX25
    Just AF_AX25 -> Just #const AF_AX25
#endif
#ifdef AF_OSINET
    Just AF_OSINET -> Just #const AF_OSINET
#endif
#ifdef AF_GOSSIP
    Just AF_GOSSIP -> Just #const AF_GOSSIP
#endif
#ifdef AF_IPX
    Just AF_IPX -> Just #const AF_IPX
#endif
#ifdef Pseudo_AF_XTP
    Just Pseudo_AF_XTP -> Just #const Pseudo_AF_XTP
#endif
#ifdef AF_CTF
    Just AF_CTF -> Just #const AF_CTF
#endif
#ifdef AF_WAN
    Just AF_WAN -> Just #const AF_WAN
#endif
#ifdef AF_SDL
    Just AF_SDL -> Just #const AF_SDL
#endif
#ifdef AF_NETWARE
    Just AF_NETWARE -> Just #const AF_NETWARE
#endif
#ifdef AF_NDD
    Just AF_NDD -> Just #const AF_NDD
#endif
#ifdef AF_INTF
    Just AF_INTF -> Just #const AF_INTF
#endif
#ifdef AF_COIP
    Just AF_COIP -> Just #const AF_COIP
#endif
#ifdef AF_CNT
    Just AF_CNT -> Just #const AF_CNT
#endif
#ifdef Pseudo_AF_RTIP
    Just Pseudo_AF_RTIP -> Just #const Pseudo_AF_RTIP
#endif
#ifdef Pseudo_AF_PIP
    Just Pseudo_AF_PIP -> Just #const Pseudo_AF_PIP
#endif
#ifdef AF_SIP
    Just AF_SIP -> Just #const AF_SIP
#endif
#ifdef AF_ISDN
    Just AF_ISDN -> Just #const AF_ISDN
#endif
#ifdef Pseudo_AF_KEY
    Just Pseudo_AF_KEY -> Just #const Pseudo_AF_KEY
#endif
#ifdef AF_NATM
    Just AF_NATM -> Just #const AF_NATM
#endif
#ifdef AF_ARP
    Just AF_ARP -> Just #const AF_ARP
#endif
#ifdef Pseudo_AF_HDRCMPLT
    Just Pseudo_AF_HDRCMPLT -> Just #const Pseudo_AF_HDRCMPLT
#endif
#ifdef AF_ENCAP
    Just AF_ENCAP -> Just #const AF_ENCAP
#endif
#ifdef AF_LINK
    Just AF_LINK -> Just #const AF_LINK
#endif
#ifdef AF_RAW
    Just AF_RAW -> Just #const AF_RAW
#endif
#ifdef AF_RIF
    Just AF_RIF -> Just #const AF_RIF
#endif
#ifdef AF_NETROM
    Just AF_NETROM -> Just #const AF_NETROM
#endif
#ifdef AF_BRIDGE
    Just AF_BRIDGE -> Just #const AF_BRIDGE
#endif
#ifdef AF_ATMPVC
    Just AF_ATMPVC -> Just #const AF_ATMPVC
#endif
#ifdef AF_ROSE
    Just AF_ROSE -> Just #const AF_ROSE
#endif
#ifdef AF_NETBEUI
    Just AF_NETBEUI -> Just #const AF_NETBEUI
#endif
#ifdef AF_SECURITY
    Just AF_SECURITY -> Just #const AF_SECURITY
#endif
#ifdef AF_PACKET
    Just AF_PACKET -> Just #const AF_PACKET
#endif
#ifdef AF_ASH
    Just AF_ASH -> Just #const AF_ASH
#endif
#ifdef AF_ECONET
    Just AF_ECONET -> Just #const AF_ECONET
#endif
#ifdef AF_ATMSVC
    Just AF_ATMSVC -> Just #const AF_ATMSVC
#endif
#ifdef AF_IRDA
    Just AF_IRDA -> Just #const AF_IRDA
#endif
#ifdef AF_PPPOX
    Just AF_PPPOX -> Just #const AF_PPPOX
#endif
#ifdef AF_WANPIPE
    Just AF_WANPIPE -> Just #const AF_WANPIPE
#endif
#ifdef AF_BLUETOOTH
    Just AF_BLUETOOTH -> Just #const AF_BLUETOOTH
#endif
#ifdef AF_CAN
    Just AF_CAN -> Just #const AF_CAN
#endif
    _ -> Nothing

--------- ----------

-- | Converting 'CInt' to 'Family'.
unpackFamily :: CInt -> Family
unpackFamily f = case f of
        (#const AF_UNSPEC) -> AF_UNSPEC
#ifdef AF_UNIX
        (#const AF_UNIX) -> AF_UNIX
#endif
#ifdef AF_INET
        (#const AF_INET) -> AF_INET
#endif
#ifdef AF_INET6
        (#const AF_INET6) -> AF_INET6
#endif
#ifdef AF_IMPLINK
        (#const AF_IMPLINK) -> AF_IMPLINK
#endif
#ifdef AF_PUP
        (#const AF_PUP) -> AF_PUP
#endif
#ifdef AF_CHAOS
        (#const AF_CHAOS) -> AF_CHAOS
#endif
#ifdef AF_NS
        (#const AF_NS) -> AF_NS
#endif
#ifdef AF_NBS
        (#const AF_NBS) -> AF_NBS
#endif
#ifdef AF_ECMA
        (#const AF_ECMA) -> AF_ECMA
#endif
#ifdef AF_DATAKIT
        (#const AF_DATAKIT) -> AF_DATAKIT
#endif
#ifdef AF_CCITT
        (#const AF_CCITT) -> AF_CCITT
#endif
#ifdef AF_SNA
        (#const AF_SNA) -> AF_SNA
#endif
#ifdef AF_DECnet
        (#const AF_DECnet) -> AF_DECnet
#endif
#ifdef AF_DLI
        (#const AF_DLI) -> AF_DLI
#endif
#ifdef AF_LAT
        (#const AF_LAT) -> AF_LAT
#endif
#ifdef AF_HYLINK
        (#const AF_HYLINK) -> AF_HYLINK
#endif
#ifdef AF_APPLETALK
        (#const AF_APPLETALK) -> AF_APPLETALK
#endif
#ifdef AF_ROUTE
        (#const AF_ROUTE) -> AF_ROUTE
#endif
#ifdef AF_NETBIOS
        (#const AF_NETBIOS) -> AF_NETBIOS
#endif
#ifdef AF_NIT
        (#const AF_NIT) -> AF_NIT
#endif
#ifdef AF_802
        (#const AF_802) -> AF_802
#endif
#ifdef AF_ISO
        (#const AF_ISO) -> AF_ISO
#endif
#ifdef AF_OSI
# if (!defined(AF_ISO)) || (defined(AF_ISO) && (AF_ISO != AF_OSI))
        (#const AF_OSI) -> AF_OSI
# endif
#endif
#ifdef AF_NETMAN
        (#const AF_NETMAN) -> AF_NETMAN
#endif
#ifdef AF_X25
        (#const AF_X25) -> AF_X25
#endif
#ifdef AF_AX25
        (#const AF_AX25) -> AF_AX25
#endif
#ifdef AF_OSINET
        (#const AF_OSINET) -> AF_OSINET
#endif
#ifdef AF_GOSSIP
        (#const AF_GOSSIP) -> AF_GOSSIP
#endif
#if defined(AF_IPX) && (!defined(AF_NS) || AF_NS != AF_IPX)
        (#const AF_IPX) -> AF_IPX
#endif
#ifdef Pseudo_AF_XTP
        (#const Pseudo_AF_XTP) -> Pseudo_AF_XTP
#endif
#ifdef AF_CTF
        (#const AF_CTF) -> AF_CTF
#endif
#ifdef AF_WAN
        (#const AF_WAN) -> AF_WAN
#endif
#ifdef AF_SDL
        (#const AF_SDL) -> AF_SDL
#endif
#ifdef AF_NETWARE
        (#const AF_NETWARE) -> AF_NETWARE
#endif
#ifdef AF_NDD
        (#const AF_NDD) -> AF_NDD
#endif
#ifdef AF_INTF
        (#const AF_INTF) -> AF_INTF
#endif
#ifdef AF_COIP
        (#const AF_COIP) -> AF_COIP
#endif
#ifdef AF_CNT
        (#const AF_CNT) -> AF_CNT
#endif
#ifdef Pseudo_AF_RTIP
        (#const Pseudo_AF_RTIP) -> Pseudo_AF_RTIP
#endif
#ifdef Pseudo_AF_PIP
        (#const Pseudo_AF_PIP) -> Pseudo_AF_PIP
#endif
#ifdef AF_SIP
        (#const AF_SIP) -> AF_SIP
#endif
#ifdef AF_ISDN
        (#const AF_ISDN) -> AF_ISDN
#endif
#ifdef Pseudo_AF_KEY
        (#const Pseudo_AF_KEY) -> Pseudo_AF_KEY
#endif
#ifdef AF_NATM
        (#const AF_NATM) -> AF_NATM
#endif
#ifdef AF_ARP
        (#const AF_ARP) -> AF_ARP
#endif
#ifdef Pseudo_AF_HDRCMPLT
        (#const Pseudo_AF_HDRCMPLT) -> Pseudo_AF_HDRCMPLT
#endif
#ifdef AF_ENCAP
        (#const AF_ENCAP) -> AF_ENCAP
#endif
#ifdef AF_LINK
        (#const AF_LINK) -> AF_LINK
#endif
#ifdef AF_RAW
        (#const AF_RAW) -> AF_RAW
#endif
#ifdef AF_RIF
        (#const AF_RIF) -> AF_RIF
#endif
#ifdef AF_NETROM
        (#const AF_NETROM) -> AF_NETROM
#endif
#ifdef AF_BRIDGE
        (#const AF_BRIDGE) -> AF_BRIDGE
#endif
#ifdef AF_ATMPVC
        (#const AF_ATMPVC) -> AF_ATMPVC
#endif
#ifdef AF_ROSE
        (#const AF_ROSE) -> AF_ROSE
#endif
#ifdef AF_NETBEUI
        (#const AF_NETBEUI) -> AF_NETBEUI
#endif
#ifdef AF_SECURITY
        (#const AF_SECURITY) -> AF_SECURITY
#endif
#ifdef AF_PACKET
        (#const AF_PACKET) -> AF_PACKET
#endif
#ifdef AF_ASH
        (#const AF_ASH) -> AF_ASH
#endif
#ifdef AF_ECONET
        (#const AF_ECONET) -> AF_ECONET
#endif
#ifdef AF_ATMSVC
        (#const AF_ATMSVC) -> AF_ATMSVC
#endif
#ifdef AF_IRDA
        (#const AF_IRDA) -> AF_IRDA
#endif
#ifdef AF_PPPOX
        (#const AF_PPPOX) -> AF_PPPOX
#endif
#ifdef AF_WANPIPE
        (#const AF_WANPIPE) -> AF_WANPIPE
#endif
#ifdef AF_BLUETOOTH
        (#const AF_BLUETOOTH) -> AF_BLUETOOTH
#endif
#ifdef AF_CAN
        (#const AF_CAN) -> AF_CAN
#endif
        unknown -> error $
          "Network.Socket.Types.unpackFamily: unknown address family: " ++
          show unknown

------------------------------------------------------------------------
-- Port Numbers

-- | Port number.
--   Use the @Num@ instance (i.e. use a literal) to create a
--   @PortNumber@ value.
--
-- >>> 1 :: PortNumber
-- 1
-- >>> read "1" :: PortNumber
-- 1
-- >>> show (12345 :: PortNumber)
-- "12345"
-- >>> 50000 < (51000 :: PortNumber)
-- True
-- >>> 50000 < (52000 :: PortNumber)
-- True
-- >>> 50000 + (10000 :: PortNumber)
-- 60000
newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Typeable, Num, Enum, Real, Integral)

-- Print "n" instead of "PortNum n".
instance Show PortNumber where
  showsPrec p (PortNum pn) = showsPrec p (fromIntegral pn :: Int)

-- Read "n" instead of "PortNum n".
instance Read PortNumber where
  readsPrec n = map (\(x,y) -> (fromIntegral (x :: Int), y)) . readsPrec n

foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16
-- | Converts the from host byte order to network byte order.
foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32
-- | Converts the from network byte order to host byte order.
foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
{-# DEPRECATED htonl "Use getAddrInfo instead" #-}
{-# DEPRECATED ntohl "Use getAddrInfo instead" #-}

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) (htons po)
   peek p = PortNum . ntohs <$> peek (castPtr p)

-- | Default port number.
--
-- >>> defaultPort
-- 0
defaultPort :: PortNumber
defaultPort = 0

------------------------------------------------------------------------

-- | The core typeclass to unify socket addresses.
class SocketAddress sa where
    sizeOfSocketAddress :: sa -> Int
    peekSocketAddress :: Ptr sa -> IO sa
    pokeSocketAddress  :: Ptr a -> sa -> IO ()

-- sizeof(struct sockaddr_storage) which has enough space to contain
-- sockaddr_in, sockaddr_in6 and sockaddr_un.
sockaddrStorageLen :: Int
sockaddrStorageLen = 128

withSocketAddress :: SocketAddress sa => sa -> (Ptr sa -> Int -> IO a) -> IO a
withSocketAddress addr f = do
    let sz = sizeOfSocketAddress addr
    allocaBytes sz $ \p -> pokeSocketAddress p addr >> f (castPtr p) sz

withNewSocketAddress :: SocketAddress sa => (Ptr sa -> Int -> IO a) -> IO a
withNewSocketAddress f = allocaBytes sockaddrStorageLen $ \ptr -> do
    zeroMemory ptr $ fromIntegral sockaddrStorageLen
    f ptr sockaddrStorageLen

------------------------------------------------------------------------
-- Socket addresses

-- The scheme used for addressing sockets is somewhat quirky. The
-- calls in the BSD socket API that need to know the socket address
-- all operate in terms of struct sockaddr, a `virtual' type of
-- socket address.

-- The Internet family of sockets are addressed as struct sockaddr_in,
-- so when calling functions that operate on struct sockaddr, we have
-- to type cast the Internet socket address into a struct sockaddr.
-- Instances of the structure for different families might *not* be
-- the same size. Same casting is required of other families of
-- sockets such as Xerox NS. Similarly for UNIX-domain sockets.

-- To represent these socket addresses in Haskell-land, we do what BSD
-- didn't do, and use a union/algebraic type for the different
-- families. Currently only UNIX-domain sockets and the Internet
-- families are supported.

-- | Flow information.
type FlowInfo = Word32
-- | Scope identifier.
type ScopeID = Word32

-- | Socket addresses.
--  The existence of a constructor does not necessarily imply that
--  that socket address type is supported on your system: see
-- 'isSupportedSockAddr'.
data SockAddr
  = SockAddrInet
        !PortNumber      -- sin_port
        !HostAddress     -- sin_addr  (ditto)
  | SockAddrInet6
        !PortNumber      -- sin6_port
        !FlowInfo        -- sin6_flowinfo (ditto)
        !HostAddress6    -- sin6_addr (ditto)
        !ScopeID         -- sin6_scope_id (ditto)
  -- | The path must have fewer than 104 characters. All of these characters must have code points less than 256.
  | SockAddrUnix
        String           -- sun_path
  deriving (Eq, Ord, Typeable)

instance NFData SockAddr where
  rnf (SockAddrInet _ _) = ()
  rnf (SockAddrInet6 _ _ _ _) = ()
  rnf (SockAddrUnix str) = rnf str

-- | Is the socket address type supported on this system?
isSupportedSockAddr :: SockAddr -> Bool
isSupportedSockAddr addr = case addr of
  SockAddrInet{}  -> True
  SockAddrInet6{} -> True
#if defined(DOMAIN_SOCKET_SUPPORT)
  SockAddrUnix{}  -> True
#else
  SockAddrUnix{}  -> False
#endif

instance SocketAddress SockAddr where
    sizeOfSocketAddress = sizeOfSockAddr
    peekSocketAddress   = peekSockAddr
    pokeSocketAddress   = pokeSockAddr

#if defined(mingw32_HOST_OS)
type CSaFamily = (#type unsigned short)
#elif defined(darwin_HOST_OS)
type CSaFamily = (#type u_char)
#else
type CSaFamily = (#type sa_family_t)
#endif

-- | Computes the storage requirements (in bytes) of the given
-- 'SockAddr'.  This function differs from 'Foreign.Storable.sizeOf'
-- in that the value of the argument /is/ used.
sizeOfSockAddr :: SockAddr -> Int
#if defined(DOMAIN_SOCKET_SUPPORT)
# ifdef linux_HOST_OS
-- http://man7.org/linux/man-pages/man7/unix.7.html says:
-- "an abstract socket address is distinguished (from a
-- pathname socket) by the fact that sun_path[0] is a null byte
-- ('\0').  The socket's address in this namespace is given by the
-- additional bytes in sun_path that are covered by the specified
-- length of the address structure.  (Null bytes in the name have no
-- special significance.)  The name has no connection with filesystem
-- pathnames.  When the address of an abstract socket is returned,
-- the returned addrlen is greater than sizeof(sa_family_t) (i.e.,
-- greater than 2), and the name of the socket is contained in the
-- first (addrlen - sizeof(sa_family_t)) bytes of sun_path."
sizeOfSockAddr (SockAddrUnix path) =
    case path of
        '\0':_ -> (#const sizeof(sa_family_t)) + length path
        _      -> #const sizeof(struct sockaddr_un)
# else
sizeOfSockAddr SockAddrUnix{}  = #const sizeof(struct sockaddr_un)
# endif
#else
sizeOfSockAddr SockAddrUnix{}  = error "sizeOfSockAddr: not supported"
#endif
sizeOfSockAddr SockAddrInet{}  = #const sizeof(struct sockaddr_in)
sizeOfSockAddr SockAddrInet6{} = #const sizeof(struct sockaddr_in6)

-- | Use a 'SockAddr' with a function requiring a pointer to a
-- 'SockAddr' and the length of that 'SockAddr'.
withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr addr f = do
    let sz = sizeOfSockAddr addr
    allocaBytes sz $ \p -> pokeSockAddr p addr >> f (castPtr p) sz

-- We cannot bind sun_paths longer than than the space in the sockaddr_un
-- structure, and attempting to do so could overflow the allocated storage
-- space.  This constant holds the maximum allowable path length.
--
#if defined(DOMAIN_SOCKET_SUPPORT)
unixPathMax :: Int
unixPathMax = #const sizeof(((struct sockaddr_un *)NULL)->sun_path)
#endif

-- We can't write an instance of 'Storable' for 'SockAddr' because
-- @sockaddr@ is a sum type of variable size but
-- 'Foreign.Storable.sizeOf' is required to be constant.

-- Note that on Darwin, the sockaddr structure must be zeroed before
-- use.

-- | Write the given 'SockAddr' to the given memory location.
pokeSockAddr :: Ptr a -> SockAddr -> IO ()
#if defined(DOMAIN_SOCKET_SUPPORT)
pokeSockAddr p sa@(SockAddrUnix path) = do
    when (length path > unixPathMax) $ error "pokeSockAddr: path is too long"
    zeroMemory p $ fromIntegral $ sizeOfSockAddr sa
# if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_un, sun_len) p ((#const sizeof(struct sockaddr_un)) :: Word8)
# endif
    (#poke struct sockaddr_un, sun_family) p ((#const AF_UNIX) :: CSaFamily)
    let pathC = map castCharToCChar path
    -- the buffer is already filled with nulls.
    pokeArray ((#ptr struct sockaddr_un, sun_path) p) pathC
#else
pokeSockAddr _ SockAddrUnix{} = error "pokeSockAddr: not supported"
#endif
pokeSockAddr p (SockAddrInet port addr) = do
    zeroMemory p (#const sizeof(struct sockaddr_in))
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_in, sin_len) p ((#const sizeof(struct sockaddr_in)) :: Word8)
#endif
    (#poke struct sockaddr_in, sin_family) p ((#const AF_INET) :: CSaFamily)
    (#poke struct sockaddr_in, sin_port) p port
    (#poke struct sockaddr_in, sin_addr) p addr
pokeSockAddr p (SockAddrInet6 port flow addr scope) = do
    zeroMemory p (#const sizeof(struct sockaddr_in6))
# if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_in6, sin6_len) p ((#const sizeof(struct sockaddr_in6)) :: Word8)
# endif
    (#poke struct sockaddr_in6, sin6_family) p ((#const AF_INET6) :: CSaFamily)
    (#poke struct sockaddr_in6, sin6_port) p port
    (#poke struct sockaddr_in6, sin6_flowinfo) p flow
    (#poke struct sockaddr_in6, sin6_addr) p (In6Addr addr)
    (#poke struct sockaddr_in6, sin6_scope_id) p scope

-- | Read a 'SockAddr' from the given memory location.
peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
  family <- (#peek struct sockaddr, sa_family) p
  case family :: CSaFamily of
#if defined(DOMAIN_SOCKET_SUPPORT)
    (#const AF_UNIX) -> do
        str <- peekCAString ((#ptr struct sockaddr_un, sun_path) p)
        return (SockAddrUnix str)
#endif
    (#const AF_INET) -> do
        addr <- (#peek struct sockaddr_in, sin_addr) p
        port <- (#peek struct sockaddr_in, sin_port) p
        return (SockAddrInet port addr)
    (#const AF_INET6) -> do
        port <- (#peek struct sockaddr_in6, sin6_port) p
        flow <- (#peek struct sockaddr_in6, sin6_flowinfo) p
        In6Addr addr <- (#peek struct sockaddr_in6, sin6_addr) p
        scope <- (#peek struct sockaddr_in6, sin6_scope_id) p
        return (SockAddrInet6 port flow addr scope)
    _ -> ioError $ userError $
      "Network.Socket.Types.peekSockAddr: address family '" ++
      show family ++ "' not supported."

------------------------------------------------------------------------

-- | The raw network byte order number is read using host byte order.
-- Therefore on little-endian architectures the byte order is swapped. For
-- example @127.0.0.1@ is represented as @0x0100007f@ on little-endian hosts
-- and as @0x7f000001@ on big-endian hosts.
--
-- For direct manipulation prefer 'hostAddressToTuple' and
-- 'tupleToHostAddress'.
type HostAddress = Word32

-- | Converts 'HostAddress' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(0x7f, 0, 0, 1)@
-- regardless of host endianness.
--
{- -- prop> tow == hostAddressToTuple (tupleToHostAddress tow) -}
hostAddressToTuple :: HostAddress -> (Word8, Word8, Word8, Word8)
hostAddressToTuple ha' =
    let ha = htonl ha'
        byte i = fromIntegral (ha `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToHostAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
tupleToHostAddress (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in ntohl $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'hostAddress6ToTuple' and
-- 'tupleToHostAddress6'.
type HostAddress6 = (Word32, Word32, Word32, Word32)

-- | Converts 'HostAddress6' to representation-independent IPv6 octuple.
--
{- -- prop> (w1,w2,w3,w4,w5,w6,w7,w8) == hostAddress6ToTuple (tupleToHostAddress6 (w1,w2,w3,w4,w5,w6,w7,w8)) -}
hostAddress6ToTuple :: HostAddress6 -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
hostAddress6ToTuple (w3, w2, w1, w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

-- | Converts IPv6 octuple to 'HostAddress6'.
tupleToHostAddress6 :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> HostAddress6
tupleToHostAddress6 (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in (w7 `add` w6, w5 `add` w4, w3 `add` w2, w1 `add` w0)

-- The peek32 and poke32 functions work around the fact that the RFCs
-- don't require 32-bit-wide address fields to be present.  We can
-- only portably rely on an 8-bit field, s6_addr.

s6_addr_offset :: Int
s6_addr_offset = (#offset struct in6_addr, s6_addr)

peek32 :: Ptr a -> Int -> IO Word32
peek32 p i0 = do
    let i' = i0 * 4
        peekByte n = peekByteOff p (s6_addr_offset + i' + n) :: IO Word8
        a `sl` i = fromIntegral a `shiftL` i
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i0 a = do
    let i' = i0 * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        x `sr` i = fromIntegral (x `shiftR` i) :: Word8
    pokeByte 0 (a `sr` 24)
    pokeByte 1 (a `sr` 16)
    pokeByte 2 (a `sr`  8)
    pokeByte 3 (a `sr`  0)

-- | Private newtype proxy for the Storable instance. To avoid orphan instances.
newtype In6Addr = In6Addr HostAddress6

-- #if __GLASGOW_HASKELL__ < 800
-- #let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
-- #endif


instance Storable In6Addr where
    sizeOf _    = #const sizeof(struct in6_addr)
    alignment _ = #alignment struct in6_addr

    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ In6Addr (a, b, c, d)

    poke p (In6Addr (a, b, c, d)) = do
        poke32 p 0 a
        poke32 p 1 b
        poke32 p 2 c
        poke32 p 3 d

------------------------------------------------------------------------
-- Helper functions

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Zero a structure.
zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 (fromIntegral nbytes)
