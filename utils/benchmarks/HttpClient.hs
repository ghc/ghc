{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings #-}

import Text.Printf
import System.Event.Clock
import qualified Data.Attoparsec as A (parseWith)
import qualified Data.Attoparsec.Char8 as A
import RFC2616
import Control.Exception
import Control.Concurrent.QSemN
import Control.Monad
import Network.Socket hiding (connect, recv)
import System.Console.GetOpt
import Data.Function
import Data.Monoid
import GHC.Conc (numCapabilities)
import Args (ljust, parseArgs, positive, theLast)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding (many, (<|>))
import Data.Char (isSpace)
import System.Event.Thread
import EventSocket

type URL = (String, String, String)

url :: Parser URL
url =
  (,,) <$> (string "http://" *> (many . satisfy $ \c -> c /= ':' && c /= '/'))
       <*> ((char ':' *> many digit) <|> pure "80")
       <*> ((many1 . satisfy $ not . isSpace) <|> pure "/")

urlConnector :: String -> IO (IO (Socket, B.ByteString))
urlConnector urlStr = do
  let (host, port, uri) = case parse url "<cmdline>" urlStr of
                            Left err -> error (show err)
                            Right req -> req
      myHints = defaultHints { addrSocketType = Stream }
  (ai:_) <- getAddrInfo (Just myHints) (Just host) (Just port)
  return $ do
    sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
    let req = B.concat ["GET ", B.pack uri, " HTTP/1.1\r\n"
                       ,"Host: ", B.pack host, ":", B.pack port, "\r\n"]
    connect sock (addrAddress ai)
    return (sock, req)

client ctors reqs = do
  forM_ ctors $ \connector -> do
    let loop slop !reqno sock reqStart = do
          let refill = recv sock 65536
              req = B.concat [reqStart, "\r\n"]
          sendAll sock req
          resp <- (if B.null slop then refill else return slop) >>=
                  A.parseWith refill RFC2616.response
          case resp of
            err@(A.Partial _) -> print err
            err@(A.Fail bs _ msg) -> print (msg, B.take 10 bs)
            A.Done bs (st, chdrs) -> do
              let hdrs  = map lowerHeader chdrs
                  close = Header "connection" ["close"]
                  contentLength = case A.parse A.decimal (B.concat (lookupHeader "content-length" hdrs)) `A.feed` "" of
                                    A.Done _ n -> n
                                    err -> error (show chdrs)
              let slurp !n s = do
                    let len = B.length s
                    if len == 0 || len >= n
                      then return $! B.drop n s
                      else slurp (n-len) =<< recv sock 65536
              if B.length bs >= contentLength
                then if reqno >= reqs || close `elem` hdrs
                     then return ()
                     else loop (B.drop contentLength bs) (reqno+1) sock reqStart
                else slurp contentLength bs >>= \s ->
                     if reqno >= reqs || close `elem` hdrs
                     then return ()
                     else loop s (reqno+1) sock reqStart
    bracket connector (sClose . fst) . uncurry $ loop "" 1


main = do
  (cfg, urls) <- parseArgs defaultConfig defaultOptions =<< getArgs
  when (null urls) $ error "no URLs"
  ensureIOManagerIsRunning
  ctors <- mapM urlConnector urls
  let clients = theLast cfgClients cfg
      conns = theLast cfgConnections cfg
      requests = theLast cfgRequests cfg
      total = clients * conns * requests
  putStrLn $ "issuing " ++ show total ++ " requests"
  sem <- newQSemN 0
  start <- getCurrentTime
  replicateM_ clients $ do
    forkIO $ (client (take conns (cycle ctors)) requests `finally` signalQSemN sem 1)
    return ()
  waitQSemN sem clients
  end <- getCurrentTime
  let elapsed = end - start
      rate = fromIntegral total / elapsed
  printf "%.6g reqs/sec in %.6g secs\n" rate elapsed

------------------------------------------------------------------------
-- Configuration

data Config = Config {
      cfgClients     :: Last Int
    , cfgConnections :: Last Int
    , cfgRequests    :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config {
      cfgClients     = ljust numCapabilities
    , cfgConnections = ljust numCapabilities
    , cfgRequests    = ljust 1
    }

instance Monoid Config where
    mempty = Config {
          cfgClients     = mempty
        , cfgConnections = mempty
        , cfgRequests    = mempty
        }

    mappend a b = Config {
          cfgClients     = app cfgClients a b
        , cfgConnections = app cfgConnections a b
        , cfgRequests    = app cfgRequests a b
        }
      where app :: (Monoid b) => (a -> b) -> a -> a -> b
            app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['c'] ["clients"]
          (ReqArg (positive "number of concurrent clients" $ \n ->
               mempty { cfgClients = n }) "N")
          "number of concurrent clients"
    , Option ['n'] ["connections"]
          (ReqArg (positive "number of connections" $ \n ->
               mempty { cfgConnections = n }) "N")
          "number of connections"
    , Option ['r'] ["requests"]
          (ReqArg (positive "number of requests per connection" $ \n ->
               mempty { cfgRequests = n }) "N")
          "number of requests"
    ]
