{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Error
import GHC.Conc hiding (ensureIOManagerIsRunning)
import System.Event as E
import System.Event.Thread
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import System.Posix.Types
import Network.Socket hiding (accept, recv)
import Network.Socket.Internal
import EventSocket (recv, sendAll, c_recv, c_send)
import EventUtil (setNonBlocking)
import Data.ByteString.Char8 as B hiding (zip)
import Data.ByteString.Internal as B

main = do
  ensureIOManagerIsRunning
  let port = "5002"
      myHints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream }
  (ai:_) <- getAddrInfo (Just myHints) Nothing (Just port)
  sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
  setSocketOption sock ReuseAddr 1
  bindSocket sock (addrAddress ai)
  listen sock 1024
  mgrs <- replicateM numCapabilities E.new
  done <- newEmptyMVar
  forM_ (zip [0..] mgrs) $ \(cpu,mgr) -> do
    forkOnIO cpu $ do
      accept mgr sock clinet
      loop mgr
      putMVar done ()
  takeMVar done

repeatOnIntr :: IO (Either Errno a) -> IO (Either Errno a)
repeatOnIntr act = do
  ret <- act
  case ret of
    l@(Left err) -> if err == eINTR
                    then repeatOnIntr act
                    else return l
    r            -> return r

blocking :: EventManager
         -> (Either (Fd,Event) FdKey)
         -> IO (Either Errno a)
         -> (Either Fd FdKey -> a -> IO ())
         -> IO ()
blocking mgr efdk act on_success = do
  ret <- repeatOnIntr act
  case ret of
    Left err
        | err /= eWOULDBLOCK && err /= eAGAIN ->
            ioError (errnoToIOError "accept" err Nothing Nothing)
        | otherwise ->
            case efdk of
              Left (fd,evts) -> registerFd_ mgr retry fd evts >> return ()
              Right _        -> return ()
    Right a -> case efdk of
                 Left (fd,_evts) -> on_success (Left fd) a
                 Right fdk       -> on_success (Right fdk) a
 where retry fdk evt = blocking mgr (Right fdk) act on_success


accept :: EventManager -> Socket
       -> (EventManager -> Socket -> SockAddr -> IO ())
       -> IO ()
accept mgr sock@(MkSocket fd family stype proto _status) serve = do
  let sz = sizeOfSockAddrByFamily family
      act :: IO (Either Errno (CInt, SockAddr))
      act = allocaBytes sz $ \sockaddr -> do
            n <- with (fromIntegral sz) $ c_accept (fromIntegral fd) sockaddr
            if n == -1
              then Left `fmap` getErrno
              else do
                sa <- peekSockAddr sockaddr
                return $! Right (n, sa)
  blocking mgr (Left (fromIntegral fd,evtRead)) act $ \_efdk (nfd,addr) -> do
    setNonBlocking (fromIntegral nfd)
    nsock <- MkSocket nfd family stype proto `fmap` newMVar Connected
    serve mgr nsock addr

clinet :: EventManager -> Socket -> SockAddr -> IO ()
clinet mgr sock addr = do
  let MkSocket fd _ _ _ _ = sock
      act = do
        let bufSize = 4096
        fp <- B.mallocByteString bufSize
        withForeignPtr fp $ \ptr -> do
          ret <- c_recv fd ptr (fromIntegral bufSize) 0
          if ret == -1
            then Left `fmap` getErrno
            else if ret == 0
            then return $! Right empty
            else do
              let !bs = PS (castForeignPtr fp) 0 (fromIntegral ret)
              return $! Right bs
  blocking mgr (Left (fromIntegral fd,evtRead)) act $ \efdk bs -> do
    fd <- case efdk of
            Left fd -> return fd
            Right fdk -> unregisterFd_ mgr fdk >> return (keyFd fdk)
    let (PS fp off len) = "HTTP/1.0 200 OK\r\nConnection: Close\r\nContent-Length: 5\r\n\r\nPong!"
    withForeignPtr fp $ \s ->
      c_send (fromIntegral fd) (s `plusPtr` off) (fromIntegral len) 0
    sClose sock

client :: EventManager -> Socket -> SockAddr -> IO ()
client _mgr sock _addr = loop `finally` sClose sock
 where
  loop = do
    req <- recvRequest ""
    sendAll sock msg
    if "Connection: Keep-Alive" `isInfixOf` req
      then loop
      else return ()
  msg = "HTTP/1.0 200 OK\r\nConnection: Close\r\nContent-Length: 5\r\n\r\nPong!"
  recvRequest r = do
    s <- recv sock 4096
    let t = B.append r s
    if B.null s || "\r\n\r\n" `B.isInfixOf` t
      then return t
      else recvRequest t

foreign import ccall unsafe "sys/socket.h accept"
    c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
