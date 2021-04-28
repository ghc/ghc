-- Test code for "threadWait: invalid argument (Bad file descriptor)"
-- See https://ghc.haskell.org/trac/ghc/ticket/14621
-- See https://github.com/haskell/network/issues/287
--
-- % runghc BadFileDescriptor.hs
-- BadFileDescriptor.hs: threadWait: invalid argument (Bad file descriptor)
module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = do
    let localhost = "localhost"
        listenPort = "9876"
        connectPort = "6789"
    proxy localhost listenPort connectPort

proxy :: HostName -> ServiceName -> ServiceName -> IO ()
proxy localhost listenPort connectPort = do
    fromClient <- serverSocket localhost listenPort
    toServer <- clientSocket localhost connectPort
    void $ forkIO $ relay toServer fromClient
    relay fromClient toServer

relay :: Socket -> Socket -> IO ()
relay s1 s2 = forever $ do
    payload <- recv s1 4096
    sendAll s2 payload

serverSocket :: HostName -> ServiceName -> IO Socket
serverSocket host port = do
    let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 1
    fst <$> accept sock

clientSocket :: HostName -> ServiceName -> IO Socket
clientSocket host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock
