{-# LANGUAGE CPP, OverloadedStrings #-}

-- A simple tool that creates a number of "dead" connections to a
-- server.  A dead connection is a connection that doesn't transmit
-- any data but stays connected.  This tool is useful to simulate a
-- number of slow/idle connections to a server.

import Args (ljust, nonNegative, parseArgs, positive, theLast)
import EventSocket (connect, recv, sendAll)
import Control.Concurrent (forkIO)
import Control.Monad (forM_, forever)
import qualified Data.ByteString.Char8 as S
import Data.Monoid (Last(..))
import Network.Socket (AddrInfo(..), SocketType(..),
                        defaultHints, getAddrInfo,
                        socket, close, withSocketsDo)
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)
import GHC.Event (ensureIOManagerIsRunning, threadDelay)
import System.Posix.Resource (ResourceLimit(..),
                              ResourceLimits(..),
                              Resource(..), setResourceLimit)
import qualified Data.Semigroup as Sem

main = withSocketsDo $ do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
    let numConns = theLast cfgNumConns cfg
        host     = theLast cfgHost cfg
        port     = theLast cfgPort cfg
        delay    = theLast cfgDelay cfg * 1000
        lim      = ResourceLimit $ fromIntegral numConns + 50
        myHints  = defaultHints { addrSocketType = Stream }

    ensureIOManagerIsRunning
    setResourceLimit ResourceOpenFiles
        ResourceLimits { softLimit = lim, hardLimit = lim }

    addrinfos <- getAddrInfo (Just myHints) (Just host) (Just $ show port)
    let addr = head addrinfos

    putStrLn $ "Running " ++ show numConns ++ " threads to clobber " ++
        host ++ ":" ++ show port ++ "..."
    forM_ [0..numConns-1] $ \n -> forkIO . forever $ do
        let myDelay = delay + n * 1037
        sock <- socket (addrFamily addr) (addrSocketType addr)
                (addrProtocol addr)
        connect sock (addrAddress addr)
        let sendLoop s
                | S.null s = recvLoop
                | otherwise = do
                     threadDelay myDelay
                     let len = (n `mod` (S.length request - 1)) + 1
                     let (h,t) = S.splitAt len s
                     sendAll sock h
                     sendLoop t
            recvLoop = do
                     threadDelay myDelay
                     s <- recv sock 256
                     if S.null s
                       then close sock
                       else recvLoop
        sendLoop request
    putStrLn $ show numConns ++ " threads looping"

    -- Block process forever.
    --threadDelay maxBound

request = "GET / HTTP/1.1\r\nHost: www.test.com\r\n\r\n"

------------------------------------------------------------------------
-- Configuration

data Config = Config {
      cfgNumConns :: Last Int
    , cfgDelay    :: Last Int
    , cfgHost     :: Last String
    , cfgPort     :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config
    { cfgNumConns = ljust 50
    , cfgDelay    = ljust 100
    , cfgHost     = ljust "localhost"
    , cfgPort     = ljust 3000
    }

instance Sem.Semigroup Config where
  Config {
    cfgNumConns = a
    , cfgDelay  = b
    , cfgHost   = c
    , cfgPort   = d
    }
    <> Config { cfgNumConns = e
              , cfgDelay  = f
              , cfgHost   = g
              , cfgPort   = h
              } =
    Config { cfgNumConns = a <> e
           , cfgDelay  = b <> f
           , cfgHost   = c <> g
           , cfgPort   = d <> h
           }

instance Monoid Config where
    mempty = Config
        { cfgNumConns = mempty
        , cfgDelay    = mempty
        , cfgHost     = mempty
        , cfgPort     = mempty
        }

    mappend = (<>)

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['n'] ["connections"]
          (ReqArg (nonNegative "number of connections" $ \n ->
               mempty { cfgNumConns = n }) "N")
          "number of connections"
    , Option ['d'] ["delay"]
          (ReqArg (nonNegative "delay between chunks" $ \d ->
               mempty { cfgDelay = d }) "N")
          "delay between chunks (ms)"
    , Option ['h'] ["host"]
          (ReqArg (\s -> return $ mempty { cfgHost = ljust s }) "HOST")
          "server address"
    , Option ['p'] ["port"]
          (ReqArg (positive "server port" $ \n ->
               mempty { cfgPort = n }) "N")
          "server port"
    ]
