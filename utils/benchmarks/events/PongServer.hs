{-# LANGUAGE CPP, OverloadedStrings #-}

-- Requires the network-bytestring library.
--
-- Start server and run
--   httperf --server=localhost --port=5002 --uri=/ --num-conns=10000
-- or
--   ab -n 10000 -c 100 http://localhost:5002/

import Args (ljust, parseArgs, positive, theLast)
import Control.Concurrent (forkIO, runInUnboundThread)
import Data.ByteString.Char8 ()
import Data.Monoid (Last(..))
import Network.Socket hiding (accept)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C ()
#if defined(USE_GHC_IO_MANAGER)
import Network.Socket (accept)
import Network.Socket.ByteString (recv, sendAll)
#else
import EventSocket (accept, recv, sendAll)
import GHC.Event (ensureIOManagerIsRunning)
#endif
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)
import System.Posix.Resource (ResourceLimit(..), ResourceLimits(..),
                              Resource(..), setResourceLimit)
import qualified Data.Semigroup as Sem

main = do
  (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
  let listenBacklog = theLast cfgListenBacklog cfg
      port = theLast cfgPort cfg
      lim  = ResourceLimit . fromIntegral . theLast cfgMaxFds $ cfg
      myHints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream }
#if !defined(USE_GHC_IO_MANAGER)
  ensureIOManagerIsRunning
#endif
  setResourceLimit ResourceOpenFiles
      ResourceLimits { softLimit = lim, hardLimit = lim }
  (ai:_) <- getAddrInfo (Just myHints) Nothing (Just port)
  sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress ai)
  listen sock listenBacklog
  runInUnboundThread $ acceptConnections sock

acceptConnections :: Socket -> IO ()
acceptConnections sock = loop
  where
    loop = do
        (c,_) <- accept sock
        _ <- forkIO $ client c
        loop

client :: Socket -> IO ()
client sock = do
  recvRequest ""
  sendAll sock msg
  close sock
 where
  msg = "HTTP/1.0 200 OK\r\nConnection: Close\r\nContent-Length: 5\r\n\r\nPong!"
  recvRequest r = do
    s <- recv sock 4096
    let t = S.append r s
    if S.null s || "\r\n\r\n" `S.isInfixOf` t
      then return ()
      else recvRequest t

------------------------------------------------------------------------
-- Configuration

data Config = Config {
      cfgListenBacklog :: Last Int
    , cfgMaxFds        :: Last Int
    , cfgPort          :: Last String
    }

defaultConfig :: Config
defaultConfig = Config {
      cfgListenBacklog = ljust 1024
    , cfgMaxFds        = ljust 256
    , cfgPort          = ljust "5002"
    }

instance Sem.Semigroup Config where
  Config {
    cfgListenBacklog = a
    , cfgMaxFds = b
    , cfgPort =  c
    } <> Config { cfgListenBacklog = d
                , cfgMaxFds = e
                , cfgPort = f
                } =
    Config {cfgListenBacklog = a <> d,
             cfgMaxFds = b <> e,
             cfgPort = c <> f}

instance Monoid Config where
    mempty = Config {
          cfgListenBacklog = mempty
        , cfgMaxFds        = mempty
        , cfgPort          = mempty
        }

    mappend = (<>)

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['p'] ["port"]
          (ReqArg (\s -> return mempty { cfgPort = ljust s }) "N")
          "server port"
    , Option ['m'] ["max-fds"]
          (ReqArg (positive "maximum number of file descriptors" $ \n ->
               mempty { cfgMaxFds = n }) "N")
          "maximum number of file descriptors"
    , Option [] ["listen-backlog"]
          (ReqArg (positive "maximum number of pending connections" $ \n ->
               mempty { cfgListenBacklog = n }) "N")
          "maximum number of pending connections"
    ]
