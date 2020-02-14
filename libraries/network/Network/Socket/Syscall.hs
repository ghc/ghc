{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.Syscall where

import Foreign.Marshal.Utils (with)
import qualified Control.Exception as E
# if defined(mingw32_HOST_OS)
import System.IO.Error (catchIOError)
#endif

#if defined(mingw32_HOST_OS)
import Foreign (FunPtr)
import GHC.Conc (asyncDoProc)
#else
import Foreign.C.Error (getErrno, eINTR, eINPROGRESS)
import GHC.Conc (threadWaitWrite)
#endif

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
import Network.Socket.Cbits
#else
import Network.Socket.Fcntl
#endif

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Options
import Network.Socket.Types

-- ----------------------------------------------------------------------------
-- On Windows, our sockets are not put in non-blocking mode (non-blocking
-- is not supported for regular file descriptors on Windows, and it would
-- be a pain to support it only for sockets).  So there are two cases:
--
--  - the threaded RTS uses safe calls for socket operations to get
--    non-blocking I/O, just like the rest of the I/O library
--
--  - with the non-threaded RTS, only some operations on sockets will be
--    non-blocking.  Reads and writes go through the normal async I/O
--    system.  accept() uses asyncDoProc so is non-blocking.  A handful
--    of others (recvFrom, sendFd, recvFd) will block all threads - if this
--    is a problem, -threaded is the workaround.
--

-----------------------------------------------------------------------------
-- Connection Functions

-- In the following connection and binding primitives.  The names of
-- the equivalent C functions have been preserved where possible. It
-- should be noted that some of these names used in the C library,
-- \tr{bind} in particular, have a different meaning to many Haskell
-- programmers and have thus been renamed by appending the prefix
-- Socket.

-- | Create a new socket using the given address family, socket type
-- and protocol number.  The address family is usually 'AF_INET',
-- 'AF_INET6', or 'AF_UNIX'.  The socket type is usually 'Stream' or
-- 'Datagram'.  The protocol number is usually 'defaultProtocol'.
-- If 'AF_INET6' is used and the socket type is 'Stream' or 'Datagram',
-- the 'IPv6Only' socket option is set to 0 so that both IPv4 and IPv6
-- can be handled with one socket.
--
-- >>> import Network.Socket
-- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
-- >>> addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
-- >>> sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >>> Network.Socket.bind sock (addrAddress addr)
-- >>> getSocketName sock
-- 127.0.0.1:5000
socket :: Family         -- Family Name (usually AF_INET)
       -> SocketType     -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket      -- Unconnected Socket
socket family stype protocol = E.bracketOnError create c_close $ \fd -> do
    -- Let's ensure that the socket (file descriptor) is closed even on
    -- asynchronous exceptions.
    setNonBlock fd
    s <- mkSocket fd
    -- This socket is not managed by the IO manager yet.
    -- So, we don't have to call "close" which uses "closeFdWith".
    unsetIPv6Only s
    return s
  where
    create = do
        c_stype <- modifyFlag <$> packSocketTypeOrThrow "socket" stype
        throwSocketErrorIfMinus1Retry "Network.Socket.socket" $
            c_socket (packFamily family) c_stype protocol

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
    modifyFlag c_stype = c_stype .|. sockNonBlock
#else
    modifyFlag c_stype = c_stype
#endif

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
    setNonBlock _ = return ()
#else
    setNonBlock fd = setNonBlockIfNeeded fd
#endif

#if HAVE_DECL_IPV6_V6ONLY
    unsetIPv6Only s = when (family == AF_INET6 && stype `elem` [Stream, Datagram]) $
# if defined(mingw32_HOST_OS)
      -- The IPv6Only option is only supported on Windows Vista and later,
      -- so trying to change it might throw an error.
      setSocketOption s IPv6Only 0 `catchIOError` \_ -> return ()
# elif defined(__OpenBSD__)
      -- don't change IPv6Only
      return ()
# else
      -- The default value of the IPv6Only option is platform specific,
      -- so we explicitly set it to 0 to provide a common default.
      setSocketOption s IPv6Only 0
# endif
#else
    unsetIPv6Only _ = return ()
#endif

-----------------------------------------------------------------------------
-- Binding a socket

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'defaultPort' is passed then the system assigns the next available
-- use port.
bind :: SocketAddress sa => Socket -> sa -> IO ()
bind s sa = withSocketAddress sa $ \p_sa siz -> void $ withFdSocket s $ \fd -> do
  let sz = fromIntegral siz
  throwSocketErrorIfMinus1Retry "Network.Socket.bind" $ c_bind fd p_sa sz

-----------------------------------------------------------------------------
-- Connecting a socket

-- | Connect to a remote socket at address.
connect :: SocketAddress sa => Socket -> sa -> IO ()
connect s sa = withSocketsDo $ withSocketAddress sa $ \p_sa sz ->
    connectLoop s p_sa (fromIntegral sz)

connectLoop :: SocketAddress sa => Socket -> Ptr sa -> CInt -> IO ()
connectLoop s p_sa sz = withFdSocket s $ \fd -> loop fd
  where
    errLoc = "Network.Socket.connect: " ++ show s
    loop fd = do
       r <- c_connect fd p_sa sz
       when (r == -1) $ do
#if defined(mingw32_HOST_OS)
           throwSocketError errLoc
#else
           err <- getErrno
           case () of
             _ | err == eINTR       -> loop fd
             _ | err == eINPROGRESS -> connectBlocked
--           _ | err == eAGAIN      -> connectBlocked
             _otherwise             -> throwSocketError errLoc

    connectBlocked = do
       withFdSocket s $ threadWaitWrite . fromIntegral
       err <- getSocketOption s SoError
       when (err /= 0) $ throwSocketErrorCode errLoc (fromIntegral err)
#endif

-----------------------------------------------------------------------------
-- Listen

-- | Listen for connections made to the socket.  The second argument
-- specifies the maximum number of queued connections and should be at
-- least 1; the maximum value is system-dependent (usually 5).
listen :: Socket -> Int -> IO ()
listen s backlog = withFdSocket s $ \fd -> do
    throwSocketErrorIfMinus1Retry_ "Network.Socket.listen" $
        c_listen fd $ fromIntegral backlog

-----------------------------------------------------------------------------
-- Accept
--
-- A call to `accept' only returns when data is available on the given
-- socket, unless the socket has been set to non-blocking.  It will
-- return a new socket which should be used to read the incoming data and
-- should then be closed. Using the socket returned by `accept' allows
-- incoming requests to be queued on the original socket.

-- | Accept a connection.  The socket must be bound to an address and
-- listening for connections.  The return value is a pair @(conn,
-- address)@ where @conn@ is a new socket object usable to send and
-- receive data on the connection, and @address@ is the address bound
-- to the socket on the other end of the connection.
-- On Unix, FD_CLOEXEC is set to the new 'Socket'.
accept :: SocketAddress sa => Socket -> IO (Socket, sa)
accept listing_sock = withNewSocketAddress $ \new_sa sz ->
    withFdSocket listing_sock $ \listing_fd -> do
 new_sock <- callAccept listing_fd new_sa sz >>= mkSocket
 new_addr <- peekSocketAddress new_sa
 return (new_sock, new_addr)
  where
#if defined(mingw32_HOST_OS)
     callAccept fd sa sz
       | threaded  = with (fromIntegral sz) $ \ ptr_len ->
                       throwSocketErrorIfMinus1Retry "Network.Socket.accept" $
                         c_accept_safe fd sa ptr_len
       | otherwise = do
             paramData <- c_newAcceptParams fd (fromIntegral sz) sa
             rc        <- asyncDoProc c_acceptDoProc paramData
             new_fd    <- c_acceptNewSock paramData
             c_free paramData
             when (rc /= 0) $
               throwSocketErrorCode "Network.Socket.accept" (fromIntegral rc)
             return new_fd
#else
     callAccept fd sa sz = with (fromIntegral sz) $ \ ptr_len -> do
# ifdef HAVE_ADVANCED_SOCKET_FLAGS
       throwSocketErrorWaitRead listing_sock "Network.Socket.accept"
                        (c_accept4 fd sa ptr_len (sockNonBlock .|. sockCloexec))
# else
       new_fd <- throwSocketErrorWaitRead listing_sock "Network.Socket.accept"
                        (c_accept fd sa ptr_len)
       setNonBlockIfNeeded new_fd
       setCloseOnExecIfNeeded new_fd
       return new_fd
# endif /* HAVE_ADVANCED_SOCKET_FLAGS */
#endif

foreign import CALLCONV unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "bind"
  c_bind :: CInt -> Ptr sa -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "connect"
  c_connect :: CInt -> Ptr sa -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "listen"
  c_listen :: CInt -> CInt -> IO CInt

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
foreign import CALLCONV unsafe "accept4"
  c_accept4 :: CInt -> Ptr sa -> Ptr CInt{-CSockLen???-} -> CInt -> IO CInt
#else
foreign import CALLCONV unsafe "accept"
  c_accept :: CInt -> Ptr sa -> Ptr CInt{-CSockLen???-} -> IO CInt
#endif

#if defined(mingw32_HOST_OS)
foreign import CALLCONV safe "accept"
  c_accept_safe :: CInt -> Ptr sa -> Ptr CInt{-CSockLen???-} -> IO CInt
foreign import ccall unsafe "rtsSupportsBoundThreads"
  threaded :: Bool
foreign import ccall unsafe "HsNet.h acceptNewSock"
  c_acceptNewSock :: Ptr () -> IO CInt
foreign import ccall unsafe "HsNet.h newAcceptParams"
  c_newAcceptParams :: CInt -> CInt -> Ptr a -> IO (Ptr ())
foreign import ccall unsafe "HsNet.h &acceptDoProc"
  c_acceptDoProc :: FunPtr (Ptr () -> IO Int)
foreign import ccall unsafe "free"
  c_free:: Ptr a -> IO ()
#endif
