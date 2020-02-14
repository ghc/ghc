{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Info where

import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (maybeWith, with)
import GHC.IO (unsafePerformIO)
import GHC.IO.Exception (IOErrorType(NoSuchThing))
import System.IO.Error (ioeSetErrorString, mkIOError)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types

-----------------------------------------------------------------------------

-- | Either a host name e.g., @\"haskell.org\"@ or a numeric host
-- address string consisting of a dotted decimal IPv4 address or an
-- IPv6 address e.g., @\"192.168.0.1\"@.
type HostName       = String
-- | Either a service name e.g., @\"http\"@ or a numeric port number.
type ServiceName    = String

-----------------------------------------------------------------------------
-- Address and service lookups

-- | Flags that control the querying behaviour of 'getAddrInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-25>
data AddrInfoFlag =
    -- | The list of returned 'AddrInfo' values will
    --   only contain IPv4 addresses if the local system has at least
    --   one IPv4 interface configured, and likewise for IPv6.
    --   (Only some platforms support this.)
      AI_ADDRCONFIG
    -- | If 'AI_ALL' is specified, return all matching IPv6 and
    --   IPv4 addresses.  Otherwise, this flag has no effect.
    --   (Only some platforms support this.)
    | AI_ALL
    -- | The 'addrCanonName' field of the first returned
    --   'AddrInfo' will contain the "canonical name" of the host.
    | AI_CANONNAME
    -- | The 'HostName' argument /must/ be a numeric
    --   address in string form, and network name lookups will not be
    --   attempted.
    | AI_NUMERICHOST
    -- | The 'ServiceName' argument /must/ be a port
    --   number in string form, and service name lookups will not be
    --   attempted. (Only some platforms support this.)
    | AI_NUMERICSERV
    -- | If no 'HostName' value is provided, the network
    --   address in each 'SockAddr'
    --   will be left as a "wild card".
    --   This is useful for server applications that
    --   will accept connections from any client.
    | AI_PASSIVE
    -- | If an IPv6 lookup is performed, and no IPv6
    --   addresses are found, IPv6-mapped IPv4 addresses will be
    --   returned. (Only some platforms support this.)
    | AI_V4MAPPED
    deriving (Eq, Read, Show, Typeable)

aiFlagMapping :: [(AddrInfoFlag, CInt)]

aiFlagMapping =
    [
#if HAVE_DECL_AI_ADDRCONFIG
     (AI_ADDRCONFIG, #const AI_ADDRCONFIG),
#else
     (AI_ADDRCONFIG, 0),
#endif
#if HAVE_DECL_AI_ALL
     (AI_ALL, #const AI_ALL),
#else
     (AI_ALL, 0),
#endif
     (AI_CANONNAME, #const AI_CANONNAME),
     (AI_NUMERICHOST, #const AI_NUMERICHOST),
#if HAVE_DECL_AI_NUMERICSERV
     (AI_NUMERICSERV, #const AI_NUMERICSERV),
#else
     (AI_NUMERICSERV, 0),
#endif
     (AI_PASSIVE, #const AI_PASSIVE),
#if HAVE_DECL_AI_V4MAPPED
     (AI_V4MAPPED, #const AI_V4MAPPED)
#else
     (AI_V4MAPPED, 0)
#endif
    ]

-- | Indicate whether the given 'AddrInfoFlag' will have any effect on
-- this system.
addrInfoFlagImplemented :: AddrInfoFlag -> Bool
addrInfoFlagImplemented f = packBits aiFlagMapping [f] /= 0

data AddrInfo = AddrInfo {
    addrFlags :: [AddrInfoFlag]
  , addrFamily :: Family
  , addrSocketType :: SocketType
  , addrProtocol :: ProtocolNumber
  , addrAddress :: SockAddr
  , addrCanonName :: Maybe String
  } deriving (Eq, Show, Typeable)

instance Storable AddrInfo where
    sizeOf    _ = #const sizeof(struct addrinfo)
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        ai_flags <- (#peek struct addrinfo, ai_flags) p
        ai_family <- (#peek struct addrinfo, ai_family) p
        ai_socktype <- (#peek struct addrinfo, ai_socktype) p
        ai_protocol <- (#peek struct addrinfo, ai_protocol) p
        ai_addr <- (#peek struct addrinfo, ai_addr) p >>= peekSockAddr
        ai_canonname_ptr <- (#peek struct addrinfo, ai_canonname) p

        ai_canonname <- if ai_canonname_ptr == nullPtr
                        then return Nothing
                        else Just <$> peekCString ai_canonname_ptr

        socktype <- unpackSocketType' "AddrInfo.peek" ai_socktype
        return $ AddrInfo {
            addrFlags = unpackBits aiFlagMapping ai_flags
          , addrFamily = unpackFamily ai_family
          , addrSocketType = socktype
          , addrProtocol = ai_protocol
          , addrAddress = ai_addr
          , addrCanonName = ai_canonname
          }

    poke p (AddrInfo flags family sockType protocol _ _) = do
        c_stype <- packSocketTypeOrThrow "AddrInfo.poke" sockType

        (#poke struct addrinfo, ai_flags) p (packBits aiFlagMapping flags)
        (#poke struct addrinfo, ai_family) p (packFamily family)
        (#poke struct addrinfo, ai_socktype) p c_stype
        (#poke struct addrinfo, ai_protocol) p protocol

        -- stuff below is probably not needed, but let's zero it for safety

        (#poke struct addrinfo, ai_addrlen) p (0::CSize)
        (#poke struct addrinfo, ai_addr) p nullPtr
        (#poke struct addrinfo, ai_canonname) p nullPtr
        (#poke struct addrinfo, ai_next) p nullPtr

-- | Flags that control the querying behaviour of 'getNameInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-30>
data NameInfoFlag =
    -- | Resolve a datagram-based service name.  This is
    --   required only for the few protocols that have different port
    --   numbers for their datagram-based versions than for their
    --   stream-based versions.
      NI_DGRAM
    -- | If the hostname cannot be looked up, an IO error is thrown.
    | NI_NAMEREQD
    -- | If a host is local, return only the hostname part of the FQDN.
    | NI_NOFQDN
    -- | The name of the host is not looked up.
    --   Instead, a numeric representation of the host's
    --   address is returned.  For an IPv4 address, this will be a
    --   dotted-quad string.  For IPv6, it will be colon-separated
    --   hexadecimal.
    | NI_NUMERICHOST
    -- | The name of the service is not
    --   looked up.  Instead, a numeric representation of the
    --   service is returned.
    | NI_NUMERICSERV
    deriving (Eq, Read, Show, Typeable)

niFlagMapping :: [(NameInfoFlag, CInt)]

niFlagMapping = [(NI_DGRAM, #const NI_DGRAM),
                 (NI_NAMEREQD, #const NI_NAMEREQD),
                 (NI_NOFQDN, #const NI_NOFQDN),
                 (NI_NUMERICHOST, #const NI_NUMERICHOST),
                 (NI_NUMERICSERV, #const NI_NUMERICSERV)]

-- | Default hints for address lookup with 'getAddrInfo'.  The values
-- of the 'addrAddress' and 'addrCanonName' fields are 'undefined',
-- and are never inspected by 'getAddrInfo'.
--
-- >>> addrFlags defaultHints
-- []
-- >>> addrFamily defaultHints
-- AF_UNSPEC
-- >>> addrSocketType defaultHints
-- NoSocketType
-- >>> addrProtocol defaultHints
-- 0

defaultHints :: AddrInfo
defaultHints = AddrInfo {
    addrFlags      = []
  , addrFamily     = AF_UNSPEC
  , addrSocketType = NoSocketType
  , addrProtocol   = defaultProtocol
  , addrAddress    = undefined
  , addrCanonName  = undefined
  }

-- | Shows the fields of 'defaultHints', without inspecting the by-default undefined fields 'addrAddress' and 'addrCanonName'.
showDefaultHints :: AddrInfo -> String
showDefaultHints AddrInfo{..} = concat [
    "AddrInfo {"
  , "addrFlags = "
  , show addrFlags
  , ", addrFamily = "
  , show addrFamily
  , ", addrSocketType = "
  , show addrSocketType
  , ", addrProtocol = "
  , show addrProtocol
  , ", addrAddress = "
  , "<assumed to be undefined>"
  , ", addrCanonName = "
  , "<assumed to be undefined>"
  , "}"
  ]

-----------------------------------------------------------------------------
-- | Resolve a host or service name to one or more addresses.
-- The 'AddrInfo' values that this function returns contain 'SockAddr'
-- values that you can pass directly to 'connect' or
-- 'bind'.
--
-- This function is protocol independent.  It can return both IPv4 and
-- IPv6 address information.
--
-- The 'AddrInfo' argument specifies the preferred query behaviour,
-- socket options, or protocol.  You can override these conveniently
-- using Haskell's record update syntax on 'defaultHints', for example
-- as follows:
--
-- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
--
-- You must provide a 'Just' value for at least one of the 'HostName'
-- or 'ServiceName' arguments.  'HostName' can be either a numeric
-- network address (dotted quad for IPv4, colon-separated hex for
-- IPv6) or a hostname.  In the latter case, its addresses will be
-- looked up unless 'AI_NUMERICHOST' is specified as a hint.  If you
-- do not provide a 'HostName' value /and/ do not set 'AI_PASSIVE' as
-- a hint, network addresses in the result will contain the address of
-- the loopback interface.
--
-- If the query fails, this function throws an IO exception instead of
-- returning an empty list.  Otherwise, it returns a non-empty list
-- of 'AddrInfo' values.
--
-- There are several reasons why a query might result in several
-- values.  For example, the queried-for host could be multihomed, or
-- the service might be available via several protocols.
--
-- Note: the order of arguments is slightly different to that defined
-- for @getaddrinfo@ in RFC 2553.  The 'AddrInfo' parameter comes first
-- to make partial application easier.
--
-- >>> addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "http")
-- >>> addrAddress addr
-- 127.0.0.1:80

getAddrInfo
    :: Maybe AddrInfo -- ^ preferred socket type or protocol
    -> Maybe HostName -- ^ host name to look up
    -> Maybe ServiceName -- ^ service name to look up
    -> IO [AddrInfo] -- ^ resolved addresses, with "best" first
getAddrInfo hints node service = alloc getaddrinfo
  where
    alloc body = withSocketsDo $ maybeWith withCString node $ \c_node ->
        maybeWith withCString service                       $ \c_service ->
            maybeWith with filteredHints                    $ \c_hints ->
                  alloca                                    $ \ptr_ptr_addrs ->
                      body c_node c_service c_hints ptr_ptr_addrs
    getaddrinfo c_node c_service c_hints ptr_ptr_addrs = do
        ret <- c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs
        if ret == 0 then do
            ptr_addrs <- peek ptr_ptr_addrs
            ais       <- followAddrInfo ptr_addrs
            c_freeaddrinfo ptr_addrs
            -- POSIX requires that getaddrinfo(3) returns at least one addrinfo.
            -- See: http://pubs.opengroup.org/onlinepubs/9699919799/functions/getaddrinfo.html
            case ais of
              [] -> ioError $ mkIOError NoSuchThing message Nothing Nothing
              _ -> pure ais
          else do
            err <- gai_strerror ret
            ioError $ ioeSetErrorString
                        (mkIOError NoSuchThing message Nothing Nothing)
                        err
    message = concat [
        "Network.Socket.getAddrInfo (called with preferred socket type/protocol: "
      , maybe (show hints) showDefaultHints hints
      , ", host name: "
      , show node
      , ", service name: "
      , show service
      , ")"
      ]
#if defined(darwin_HOST_OS)
    -- Leaving out the service and using AI_NUMERICSERV causes a
    -- segfault on OS X 10.8.2. This code removes AI_NUMERICSERV
    -- (which has no effect) in that case.
    toHints h = h { addrFlags = delete AI_NUMERICSERV (addrFlags h) }
    filteredHints = case service of
        Nothing -> toHints <$> hints
        _       -> hints
#else
    filteredHints = hints
#endif

followAddrInfo :: Ptr AddrInfo -> IO [AddrInfo]
followAddrInfo ptr_ai
    | ptr_ai == nullPtr = return []
    | otherwise = do
        a  <- peek ptr_ai
        as <- (# peek struct addrinfo, ai_next) ptr_ai >>= followAddrInfo
        return (a : as)

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: CString -> CString -> Ptr AddrInfo -> Ptr (Ptr AddrInfo)
                  -> IO CInt

foreign import ccall safe "hsnet_freeaddrinfo"
    c_freeaddrinfo :: Ptr AddrInfo -> IO ()

gai_strerror :: CInt -> IO String

#ifdef HAVE_GAI_STRERROR
gai_strerror n = c_gai_strerror n >>= peekCString

foreign import ccall safe "gai_strerror"
    c_gai_strerror :: CInt -> IO CString
#else
gai_strerror n = ioError $ userError $ "Network.Socket.gai_strerror not supported: " ++ show n
#endif

-----------------------------------------------------------------------------

withCStringIf :: Bool -> Int -> (CSize -> CString -> IO a) -> IO a
withCStringIf False _ f = f 0 nullPtr
withCStringIf True  n f = allocaBytes n (f (fromIntegral n))

-- | Resolve an address to a host or service name.
-- This function is protocol independent.
-- The list of 'NameInfoFlag' values controls query behaviour.
--
-- If a host or service's name cannot be looked up, then the numeric
-- form of the address or service will be returned.
--
-- If the query fails, this function throws an IO exception.
--
-- >>> addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "http")
-- >>> getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True $ addrAddress addr
-- (Just "127.0.0.1",Just "80")
{-
-- >>> getNameInfo [] True True $ addrAddress addr
-- (Just "localhost",Just "http")
-}
getNameInfo
    :: [NameInfoFlag] -- ^ flags to control lookup behaviour
    -> Bool -- ^ whether to look up a hostname
    -> Bool -- ^ whether to look up a service name
    -> SockAddr -- ^ the address to look up
    -> IO (Maybe HostName, Maybe ServiceName)
getNameInfo flags doHost doService addr = alloc getnameinfo
  where
    alloc body = withSocketsDo $
        withCStringIf doHost (# const NI_MAXHOST)        $ \c_hostlen c_host ->
            withCStringIf doService (# const NI_MAXSERV) $ \c_servlen c_serv ->
                withSockAddr addr                        $ \ptr_addr sz ->
                  body c_hostlen c_host c_servlen c_serv ptr_addr sz
    getnameinfo c_hostlen c_host c_servlen c_serv ptr_addr sz = do
        ret <- c_getnameinfo ptr_addr
                             (fromIntegral sz)
                             c_host
                             c_hostlen
                             c_serv
                             c_servlen
                             (packBits niFlagMapping flags)
        if ret == 0 then do
            let peekIf doIf c_val =
                    if doIf then Just <$> peekCString c_val else return Nothing
            host <- peekIf doHost c_host
            serv <- peekIf doService c_serv
            return (host, serv)
          else do
            err <- gai_strerror ret
            ioError $ ioeSetErrorString
                        (mkIOError NoSuchThing message Nothing Nothing)
                        err
    message = concat [
        "Network.Socket.getNameInfo (called with flags: "
      , show flags
      , ", hostname lookup: "
      , show doHost
      , ", service name lookup: "
      , show doService
      , ", socket address: "
      , show addr
      , ")"
      ]

foreign import ccall safe "hsnet_getnameinfo"
    c_getnameinfo :: Ptr SockAddr -> CInt{-CSockLen???-} -> CString -> CSize -> CString
                  -> CSize -> CInt -> IO CInt

-- | Pack a list of values into a bitmask.  The possible mappings from
-- value to bit-to-set are given as the first argument.  We assume
-- that each value can cause exactly one bit to be set; unpackBits will
-- break if this property is not true.

-----------------------------------------------------------------------------

packBits :: (Eq a, Num b, Bits b) => [(a, b)] -> [a] -> b
packBits mapping xs = foldl' pack 0 mapping
  where
    pack acc (k, v) | k `elem` xs = acc .|. v
                    | otherwise   = acc

-- | Unpack a bitmask into a list of values.
unpackBits :: (Num b, Bits b) => [(a, b)] -> b -> [a]
-- Be permissive and ignore unknown bit values. At least on OS X,
-- getaddrinfo returns an ai_flags field with bits set that have no
-- entry in <netdb.h>.
unpackBits [] _    = []
unpackBits ((k,v):xs) r
    | r .&. v /= 0 = k : unpackBits xs (r .&. complement v)
    | otherwise    = unpackBits xs r

-----------------------------------------------------------------------------
-- SockAddr

instance Show SockAddr where
#if defined(DOMAIN_SOCKET_SUPPORT)
  showsPrec _ (SockAddrUnix str) = showString str
#else
  showsPrec _ SockAddrUnix{} = error "showsPrec: not supported"
#endif
  showsPrec _ addr@(SockAddrInet port _)
   = showString (unsafePerformIO $
                 fst <$> getNameInfo [NI_NUMERICHOST] True False addr >>=
                 maybe (fail "showsPrec: impossible internal error") return)
   . showString ":"
   . shows port
  showsPrec _ addr@(SockAddrInet6 port _ _ _)
   = showChar '['
   . showString (unsafePerformIO $
                 fst <$> getNameInfo [NI_NUMERICHOST] True False addr >>=
                 maybe (fail "showsPrec: impossible internal error") return)
   . showString "]:"
   . shows port
