module Network.Socket.SockAddr (
      getPeerName
    , getSocketName
    , connect
    , bind
    , accept
    , sendBufTo
    , recvBufFrom
    ) where

import qualified Network.Socket.Buffer as G
import qualified Network.Socket.Name as G
import qualified Network.Socket.Syscall as G
import Network.Socket.Imports
import Network.Socket.Types

-- | Getting peer's 'SockAddr'.
getPeerName :: Socket -> IO SockAddr
getPeerName = G.getPeerName

-- | Getting my 'SockAddr'.
getSocketName :: Socket -> IO SockAddr
getSocketName = G.getSocketName

-- | Connect to a remote socket at address.
connect :: Socket -> SockAddr -> IO ()
connect = G.connect

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'defaultPort' is passed then the system assigns the next available
-- use port.
bind :: Socket -> SockAddr -> IO ()
bind = G.bind

-- | Accept a connection.  The socket must be bound to an address and
-- listening for connections.  The return value is a pair @(conn,
-- address)@ where @conn@ is a new socket object usable to send and
-- receive data on the connection, and @address@ is the address bound
-- to the socket on the other end of the connection.
-- On Unix, FD_CLOEXEC is set to the new 'Socket'.
accept :: Socket -> IO (Socket, SockAddr)
accept = G.accept

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent.  Applications are responsible for
-- ensuring that all data has been sent.
sendBufTo :: Socket -> Ptr a -> Int -> SockAddr -> IO Int
sendBufTo = G.sendBufTo

-- | Receive data from the socket, writing it into buffer instead of
-- creating a new string.  The socket need not be in a connected
-- state. Returns @(nbytes, address)@ where @nbytes@ is the number of
-- bytes received and @address@ is a 'SockAddr' representing the
-- address of the sending socket.
--
-- If the first return value is zero, it means EOF.
--
-- For 'Stream' sockets, the second return value would be invalid.
--
-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom = G.recvBufFrom
