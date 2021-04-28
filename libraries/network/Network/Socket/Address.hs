-- | This module provides extensible APIs for socket addresses.
--
module Network.Socket.Address (
    -- * Socket Address
      SocketAddress(..)
    , getPeerName
    , getSocketName
    -- * Socket operations
    , connect
    , bind
    , accept
    -- * Sending and receiving ByteString
    , sendTo
    , sendAllTo
    , recvFrom
    -- * Sending and receiving data from a buffer
    , sendBufTo
    , recvBufFrom
    ) where

import Network.Socket.ByteString.IO
import Network.Socket.Buffer
import Network.Socket.Name
import Network.Socket.Syscall
import Network.Socket.Types
