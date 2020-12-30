{-# LANGUAGE CPP, OverloadedStrings #-}

import Control.Concurrent (forkIO, runInUnboundThread)
import Control.Exception (bracket, finally)
import Control.Monad (unless, when)
import Control.Monad.Fix (fix)
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network.Socket hiding (accept, recv)
#ifdef USE_GHC_IO_MANAGER
import Network.Socket (accept)
import Network.Socket.ByteString (recv, sendAll)
#else
import EventSocket (accept, recv, sendAll)
import System.Event.Thread (ensureIOManagerIsRunning)
#endif
import qualified EventFile as F
import System.Posix.Files
import System.Posix.IO
import qualified Text.Show.ByteString as S
import NoPush
import RFC2616

strict :: L.ByteString -> B.ByteString
strict = B.concat . L.toChunks

main = do
  let port = "5002"
      myHints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream }
  (ai:_) <- getAddrInfo (Just myHints) Nothing (Just port)
#ifndef USE_GHC_IO_MANAGER
  ensureIOManagerIsRunning
#endif
  sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
  setSocketOption sock ReuseAddr 1
  bindSocket sock (addrAddress ai)
  listen sock 1024
  runInUnboundThread $ acceptConnections sock

acceptConnections :: Socket -> IO ()
acceptConnections sock = loop
  where
    loop = do
      (c,_) <- accept sock
      forkIO $ client c
      loop

parseM :: Monad m => (m B.ByteString) -> A.Parser a -> m (B.ByteString, Either String a)
parseM refill p = (step . A.parse p) =<< refill
  where step (A.Fail bs _stk msg) = return (bs, Left msg)
        step (A.Partial k) = (step . k) =<< refill
        step (A.Done bs r) = return (bs, Right r)

asInt :: Integral a => a -> Int
asInt = fromIntegral

withNoPush :: Socket -> IO a -> IO a
withNoPush sock act = setNoPush sock True >> act `finally` setNoPush sock False

client :: Socket -> IO ()
client sock = (`finally` sClose sock) loop
 where
  loop = do
    (bs, ereq) <- parseM (recv sock 4096) request
    case ereq of
      Right (req,hdrs) | requestMethod req == "GET" -> do
        let http10 = requestVersion req == "1.0"
            connection = lookupHeader "Connection" hdrs
            keepAlive = (http10 && connection == ["Keep-Alive"]) ||
                        (not http10 && connection /= ["Close"])
        bracket (openFd (B.unpack (requestUri req)) ReadOnly Nothing
                        defaultFileFlags{nonBlock=True}) closeFd $ \fd -> do
          st <- getFdStatus fd
          let fixedHeaders
                  | http10 && keepAlive =
                      B.intercalate "\r\n" [
                            "HTTP/1.0 200 OK"
                           , "Content-type: application/octet-stream"
                           , "Connection: Keep-Alive"
                           ]
                  | otherwise =
                      B.intercalate "\r\n" [
                            "HTTP/1.1 200 OK"
                           , "Content-type: application/octet-stream"
                           ]
          withNoPush sock $ do
            sendAll sock $! (`B.append` "\r\n\r\n") $ B.intercalate "\r\n" [
                fixedHeaders
              , B.append "Content-length: " . strict . S.show . asInt . fileSize $ st
              ]
            fix $ \sendLoop -> do
              s <- F.read fd 16384
              unless (B.null s) $ sendAll sock s >> sendLoop
        when keepAlive loop
      err | B.null bs -> return ()
          | otherwise -> print err >> sendAll sock "HTTP/1.1 400 Bad Request\r\nConnection: close\r\n\r\n"
