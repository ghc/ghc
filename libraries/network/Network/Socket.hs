{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This is the main module of the network package supposed to be
-- used with either "Network.Socket.ByteString" or
-- "Network.Socket.ByteString.Lazy" for sending/receiving.
--
-- Here are two minimal example programs using the TCP/IP protocol:
--
-- * a server that echoes all data that it receives back
-- * a client using it
--
-- > -- Echo server program
-- > module Main (main) where
-- >
-- > import Control.Concurrent (forkFinally)
-- > import qualified Control.Exception as E
-- > import Control.Monad (unless, forever, void)
-- > import qualified Data.ByteString as S
-- > import Network.Socket
-- > import Network.Socket.ByteString (recv, sendAll)
-- >
-- > main :: IO ()
-- > main = runTCPServer Nothing "3000" talk
-- >   where
-- >     talk s = do
-- >         msg <- recv s 1024
-- >         unless (S.null msg) $ do
-- >           sendAll s msg
-- >           talk s
-- >
-- > -- from the "network-run" package.
-- > runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
-- > runTCPServer mhost port server = withSocketsDo $ do
-- >     addr <- resolve
-- >     E.bracket (open addr) close loop
-- >   where
-- >     resolve = do
-- >         let hints = defaultHints {
-- >                 addrFlags = [AI_PASSIVE]
-- >               , addrSocketType = Stream
-- >               }
-- >         head <$> getAddrInfo (Just hints) mhost (Just port)
-- >     open addr = do
-- >         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >         setSocketOption sock ReuseAddr 1
-- >         withFdSocket sock setCloseOnExecIfNeeded
-- >         bind sock $ addrAddress addr
-- >         listen sock 1024
-- >         return sock
-- >     loop sock = forever $ do
-- >         (conn, _peer) <- accept sock
-- >         void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > -- Echo client program
-- > module Main (main) where
-- >
-- > import qualified Control.Exception as E
-- > import qualified Data.ByteString.Char8 as C
-- > import Network.Socket
-- > import Network.Socket.ByteString (recv, sendAll)
-- >
-- > main :: IO ()
-- > main = runTCPClient "127.0.0.1" "3000" $ \s -> do
-- >     sendAll s "Hello, world!"
-- >     msg <- recv s 1024
-- >     putStr "Received: "
-- >     C.putStrLn msg
-- >
-- > -- from the "network-run" package.
-- > runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
-- > runTCPClient host port client = withSocketsDo $ do
-- >     addr <- resolve
-- >     E.bracket (open addr) close client
-- >   where
-- >     resolve = do
-- >         let hints = defaultHints { addrSocketType = Stream }
-- >         head <$> getAddrInfo (Just hints) (Just host) (Just port)
-- >     open addr = do
-- >         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >         connect sock $ addrAddress addr
-- >         return sock
--
-- The proper programming model is that one 'Socket' is handled by
-- a single thread. If multiple threads use one 'Socket' concurrently,
-- unexpected things would happen. There is one exception for multiple
-- threads vs a single 'Socket': one thread reads data from a 'Socket'
-- only and the other thread writes data to the 'Socket' only.
-----------------------------------------------------------------------------

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket
    (
    -- * Initialisation
      withSocketsDo

    -- * Address information
    , getAddrInfo
    -- ** Types
    , HostName
    , ServiceName
    , AddrInfo(..)
    , defaultHints
    -- ** Flags
    , AddrInfoFlag(..)
    , addrInfoFlagImplemented

    -- * Socket operations
    , connect
    , bind
    , listen
    , accept
    -- ** Closing
    , close
    , close'
    , gracefulClose
    , shutdown
    , ShutdownCmd(..)

    -- * Socket options
    , SocketOption(..)
    , isSupportedSocketOption
    , getSocketOption
    , setSocketOption

    -- * Socket
    , Socket
    , socket
    , withFdSocket
    , unsafeFdSocket
    , touchSocket
    , socketToFd
    , fdSocket
    , mkSocket
    , socketToHandle
    -- ** Types of Socket
    , SocketType(..)
    , isSupportedSocketType
    , getSocketType
    -- ** Family
    , Family(..)
    , isSupportedFamily
    , packFamily
    , unpackFamily
    -- ** Protocol number
    , ProtocolNumber
    , defaultProtocol
    -- * Basic socket address type
    , SockAddr(..)
    , isSupportedSockAddr
    , getPeerName
    , getSocketName
    , peekSocketAddress
    , sizeOfSocketAddress
    -- ** Host address
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress
    -- ** Host address6
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    -- ** Flow Info
    , FlowInfo
    -- ** Scope ID
    , ScopeID
    , ifNameToIndex
    , ifIndexToName
    -- ** Port number
    , PortNumber
    , defaultPort
    , socketPortSafe
    , socketPort

    -- * UNIX-domain socket
    , isUnixDomainSocketAvailable
    , socketPair
    , sendFd
    , recvFd
    , getPeerCredential

    -- * Name information
    , getNameInfo
    , NameInfoFlag(..)

    -- * Low level
    -- ** socket operations
    , setCloseOnExecIfNeeded
    , getCloseOnExec
    , setNonBlockIfNeeded
    , getNonBlock
    -- ** Sending and receiving data
    , sendBuf
    , recvBuf
    , sendBufTo
    , recvBufFrom

    -- * Special constants
    , maxListenQueue
    ) where

import Network.Socket.Buffer hiding (sendBufTo, recvBufFrom)
import Network.Socket.Cbits
import Network.Socket.Fcntl
import Network.Socket.Handle
import Network.Socket.If
import Network.Socket.Info
import Network.Socket.Internal
import Network.Socket.Name hiding (getPeerName, getSocketName)
import Network.Socket.Options
import Network.Socket.Shutdown
import Network.Socket.SockAddr
import Network.Socket.Syscall hiding (connect, bind, accept)
import Network.Socket.Types
import Network.Socket.Unix
