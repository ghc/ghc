{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Options (
    SocketOption(..)
  , isSupportedSocketOption
  , getSocketType
  , getSocketOption
  , setSocketOption
  , c_getsockopt
  , c_setsockopt
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types

-----------------------------------------------------------------------------
-- Socket Properties

-- | Socket options for use with 'setSocketOption' and 'getSocketOption'.
--
-- The existence of a constructor does not imply that the relevant option
-- is supported on your system: see 'isSupportedSocketOption'
data SocketOption
    = Debug         -- ^ SO_DEBUG
    | ReuseAddr     -- ^ SO_REUSEADDR
    | Type          -- ^ SO_TYPE
    | SoError       -- ^ SO_ERROR
    | DontRoute     -- ^ SO_DONTROUTE
    | Broadcast     -- ^ SO_BROADCAST
    | SendBuffer    -- ^ SO_SNDBUF
    | RecvBuffer    -- ^ SO_RCVBUF
    | KeepAlive     -- ^ SO_KEEPALIVE
    | OOBInline     -- ^ SO_OOBINLINE
    | TimeToLive    -- ^ IP_TTL
    | MaxSegment    -- ^ TCP_MAXSEG
    | NoDelay       -- ^ TCP_NODELAY
    | Cork          -- ^ TCP_CORK
    | Linger        -- ^ SO_LINGER: timeout in seconds, 0 means disabling/disabled.
    | ReusePort     -- ^ SO_REUSEPORT
    | RecvLowWater  -- ^ SO_RCVLOWAT
    | SendLowWater  -- ^ SO_SNDLOWAT
    | RecvTimeOut   -- ^ SO_RCVTIMEO: this does not work at this moment.
    | SendTimeOut   -- ^ SO_SNDTIMEO: this does not work at this moment.
    | UseLoopBack   -- ^ SO_USELOOPBACK
    | UserTimeout   -- ^ TCP_USER_TIMEOUT
    | IPv6Only      -- ^ IPV6_V6ONLY: don't use this on OpenBSD.
    | CustomSockOpt (CInt, CInt)
    deriving (Show, Typeable)

-- | Does the 'SocketOption' exist on this system?
isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption = isJust . packSocketOption

-- | Get the 'SocketType' of an active socket.
--
--   Since: 3.0.1.0
getSocketType :: Socket -> IO SocketType
getSocketType s = (fromMaybe NoSocketType . unpackSocketType . fromIntegral)
                    <$> getSocketOption s Type

-- | For a socket option, return Just (level, value) where level is the
-- corresponding C option level constant (e.g. SOL_SOCKET) and value is
-- the option constant itself (e.g. SO_DEBUG)
-- If either constant does not exist, return Nothing.
packSocketOption :: SocketOption -> Maybe (CInt, CInt)
packSocketOption so =
  -- The Just here is a hack to disable GHC's overlapping pattern detection:
  -- the problem is if all constants are present, the fallback pattern is
  -- redundant, but if they aren't then it isn't. Hence we introduce an
  -- extra pattern (Nothing) that can't possibly happen, so that the
  -- fallback is always (in principle) necessary.
  -- I feel a little bad for including this, but such are the sacrifices we
  -- make while working with CPP - excluding the fallback pattern correctly
  -- would be a serious nuisance.
  -- (NB: comments elsewhere in this file refer to this one)
  case Just so of
#ifdef SOL_SOCKET
#ifdef SO_DEBUG
    Just Debug         -> Just ((#const SOL_SOCKET), (#const SO_DEBUG))
#endif
#ifdef SO_REUSEADDR
    Just ReuseAddr     -> Just ((#const SOL_SOCKET), (#const SO_REUSEADDR))
#endif
#ifdef SO_TYPE
    Just Type          -> Just ((#const SOL_SOCKET), (#const SO_TYPE))
#endif
#ifdef SO_ERROR
    Just SoError       -> Just ((#const SOL_SOCKET), (#const SO_ERROR))
#endif
#ifdef SO_DONTROUTE
    Just DontRoute     -> Just ((#const SOL_SOCKET), (#const SO_DONTROUTE))
#endif
#ifdef SO_BROADCAST
    Just Broadcast     -> Just ((#const SOL_SOCKET), (#const SO_BROADCAST))
#endif
#ifdef SO_SNDBUF
    Just SendBuffer    -> Just ((#const SOL_SOCKET), (#const SO_SNDBUF))
#endif
#ifdef SO_RCVBUF
    Just RecvBuffer    -> Just ((#const SOL_SOCKET), (#const SO_RCVBUF))
#endif
#ifdef SO_KEEPALIVE
    Just KeepAlive     -> Just ((#const SOL_SOCKET), (#const SO_KEEPALIVE))
#endif
#ifdef SO_OOBINLINE
    Just OOBInline     -> Just ((#const SOL_SOCKET), (#const SO_OOBINLINE))
#endif
#ifdef SO_LINGER
    Just Linger        -> Just ((#const SOL_SOCKET), (#const SO_LINGER))
#endif
#ifdef SO_REUSEPORT
    Just ReusePort     -> Just ((#const SOL_SOCKET), (#const SO_REUSEPORT))
#endif
#ifdef SO_RCVLOWAT
    Just RecvLowWater  -> Just ((#const SOL_SOCKET), (#const SO_RCVLOWAT))
#endif
#ifdef SO_SNDLOWAT
    Just SendLowWater  -> Just ((#const SOL_SOCKET), (#const SO_SNDLOWAT))
#endif
#ifdef SO_RCVTIMEO
    Just RecvTimeOut   -> Just ((#const SOL_SOCKET), (#const SO_RCVTIMEO))
#endif
#ifdef SO_SNDTIMEO
    Just SendTimeOut   -> Just ((#const SOL_SOCKET), (#const SO_SNDTIMEO))
#endif
#ifdef SO_USELOOPBACK
    Just UseLoopBack   -> Just ((#const SOL_SOCKET), (#const SO_USELOOPBACK))
#endif
#endif // SOL_SOCKET
#if HAVE_DECL_IPPROTO_IP
#ifdef IP_TTL
    Just TimeToLive    -> Just ((#const IPPROTO_IP), (#const IP_TTL))
#endif
#endif // HAVE_DECL_IPPROTO_IP
#if HAVE_DECL_IPPROTO_TCP
#ifdef TCP_MAXSEG
    Just MaxSegment    -> Just ((#const IPPROTO_TCP), (#const TCP_MAXSEG))
#endif
#ifdef TCP_NODELAY
    Just NoDelay       -> Just ((#const IPPROTO_TCP), (#const TCP_NODELAY))
#endif
#ifdef TCP_USER_TIMEOUT
    Just UserTimeout   -> Just ((#const IPPROTO_TCP), (#const TCP_USER_TIMEOUT))
#endif
#ifdef TCP_CORK
    Just Cork          -> Just ((#const IPPROTO_TCP), (#const TCP_CORK))
#endif
#endif // HAVE_DECL_IPPROTO_TCP
#if HAVE_DECL_IPPROTO_IPV6
#if HAVE_DECL_IPV6_V6ONLY
    Just IPv6Only      -> Just ((#const IPPROTO_IPV6), (#const IPV6_V6ONLY))
#endif
#endif // HAVE_DECL_IPPROTO_IPV6
    Just (CustomSockOpt opt) -> Just opt
    _             -> Nothing

-- | Return the option level and option value if they exist,
-- otherwise throw an error that begins "Network.Socket." ++ the String
-- parameter
packSocketOption' :: String -> SocketOption -> IO (CInt, CInt)
packSocketOption' caller so = maybe err return (packSocketOption so)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller,
    ": socket option ", show so, " unsupported on this system"]

#ifdef SO_LINGER
data StructLinger = StructLinger CInt CInt

instance Storable StructLinger where
    sizeOf _ = (#const sizeof(struct linger))
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        onoff  <- (#peek struct linger, l_onoff) p
        linger <- (#peek struct linger, l_linger) p
        return $ StructLinger onoff linger

    poke p (StructLinger onoff linger) = do
        (#poke struct linger, l_onoff)  p onoff
        (#poke struct linger, l_linger) p linger
#endif

-- | Set a socket option that expects an Int value.
-- There is currently no API to set e.g. the timeval socket options
setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> IO ()
#ifdef SO_LINGER
setSocketOption s Linger v = do
   (level, opt) <- packSocketOption' "setSocketOption" Linger
   let arg = if v == 0 then StructLinger 0 0 else StructLinger 1 (fromIntegral v)
   with arg $ \ptr_arg -> void $ do
     let ptr = ptr_arg :: Ptr StructLinger
         sz = fromIntegral $ sizeOf (undefined :: StructLinger)
     withFdSocket s $ \fd ->
       throwSocketErrorIfMinus1_ "Network.Socket.setSocketOption" $
         c_setsockopt fd level opt ptr sz
#endif
setSocketOption s so v = do
   (level, opt) <- packSocketOption' "setSocketOption" so
   with (fromIntegral v) $ \ptr_v -> void $ do
     let ptr = ptr_v :: Ptr CInt
         sz  = fromIntegral $ sizeOf (undefined :: CInt)
     withFdSocket s $ \fd ->
       throwSocketErrorIfMinus1_ "Network.Socket.setSocketOption" $
         c_setsockopt fd level opt ptr sz

-- | Get a socket option that gives an Int value.
-- There is currently no API to get e.g. the timeval socket options
getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> IO Int        -- Option Value
#ifdef SO_LINGER
getSocketOption s Linger = do
   (level, opt) <- packSocketOption' "getSocketOption" Linger
   alloca $ \ptr_v -> do
     let ptr = ptr_v :: Ptr StructLinger
         sz = fromIntegral $ sizeOf (undefined :: StructLinger)
     withFdSocket s $ \fd -> with sz $ \ptr_sz -> do
       throwSocketErrorIfMinus1Retry_ "Network.Socket.getSocketOption" $
         c_getsockopt fd level opt ptr ptr_sz
       StructLinger onoff linger <- peek ptr
       return $ fromIntegral $ if onoff == 0 then 0 else linger
#endif
getSocketOption s so = do
   (level, opt) <- packSocketOption' "getSocketOption" so
   alloca $ \ptr_v -> do
     let ptr = ptr_v :: Ptr CInt
         sz = fromIntegral $ sizeOf (undefined :: CInt)
     withFdSocket s $ \fd -> with sz $ \ptr_sz -> do
       throwSocketErrorIfMinus1Retry_ "Network.Socket.getSocketOption" $
         c_getsockopt fd level opt ptr ptr_sz
       fromIntegral <$> peek ptr

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt
