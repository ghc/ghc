{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- | Socket functions using GHC.Event instead of GHC's I/O manager.
module EventSocket
    (
      accept
    , connect
    , recv
    , send
    , sendAll
    , c_recv
    , c_send
    ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types (CChar(..), CInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Error (Errno(..), eINPROGRESS, eINTR,
                        errnoToIOError, getErrno, throwErrno)
#if __GLASGOW_HASKELL__ < 612
import GHC.IOBase (IOErrorType(..))
#else
import GHC.IO.Exception (IOErrorType(..))
#endif
import Network.Socket hiding (accept, connect, bind)
import qualified Network.Socket as NS (connect)
import Network.Socket.Address (pokeSocketAddress, sizeOfSocketAddress, peekSocketAddress)
import Network.Socket.Internal
import Prelude hiding (repeat)
import qualified GHC.Event as T
import System.IO.Error (ioeSetErrorString, mkIOError)
import EventUtil

connect :: Socket    -- Unconnected Socket
        -> SockAddr  -- Socket address stuff
        -> IO ()
connect sock addr = do
  let sz = sizeOfSocketAddress addr in
        allocaBytes sz $ \p_sock_addr  -> do
          pokeSocketAddress p_sock_addr addr
          withFdSocket sock $ \s -> do
             let connectLoop = do
                   r <- c_connect s (castPtr p_sock_addr) (CInt (fromIntegral sz))
                   if r == -1
                       then do
                         err <- getErrno
                         case () of
                           _ | err == eINTR       -> connectLoop
                           _ | err == eINPROGRESS -> connectBlocked s sock
                           _                      -> throwSocketError "connect"
                       else return r

             _ <- connectLoop
             return ()

  where
    connectBlocked s sk = do
        T.threadWaitWrite (fromIntegral s)
        err <- getSocketOption sk SoError
        if err == 0
          then return 0
          else ioError (errnoToIOError "connect"
                        (Errno (fromIntegral err))
                        Nothing Nothing)

foreign import ccall unsafe "connect"
  c_connect :: CInt -> Ptr SockAddr -> CInt{-CSockLen?? -} -> IO CInt

------------------------------------------------------------------------
-- Receiving

recv :: Socket -> Int -> IO ByteString
recv sock nbytes
    | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.ByteString.recv")
    | otherwise   = withFdSocket sock $ \s -> do
  fp <- B.mallocByteString nbytes
  n <- withForeignPtr fp $ recvInner s nbytes
  if n <= 0
    then return B.empty
    else return $! B.PS fp 0 n

recvInner :: CInt -> Int -> Ptr Word8 -> IO Int
recvInner s nbytes ptr = do
    len <- throwErrnoIfMinus1Retry_repeatOnBlock "recv"
           (T.threadWaitRead (fromIntegral s)) $
           c_recv s (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    case fromIntegral len of
         (-1) -> do errno <- getErrno
                    if errno == eINTR
                       then recvInner s nbytes ptr
                       else throwErrno "Network.Socket.ByteString.recv"
         n -> return n

------------------------------------------------------------------------
-- Sending

-- | Send data to the socket.  The socket must be connected to a
-- remote socket.  Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Number of bytes sent
send sock xs =
    unsafeUseAsCStringLen xs $ \(str, len) -> do
      withFdSocket sock $ \s -> do
        fmap fromIntegral $
            throwSocketErrorIfMinus1RetryMayBlock "send"
            (T.threadWaitWrite (fromIntegral s)) $
            c_send s str (fromIntegral len) 0

-- | Send data to the socket.  The socket must be connected to a
-- remote socket.  Unlike 'send', this function continues to send data
-- until either all data has been sent or an error occurs.  On error,
-- an exception is raised, and there is no way to determine how much
-- data, if any, was successfully sent.
sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll sock bs = do
    sent <- send sock bs
    when (sent < B.length bs) $ sendAll sock (B.drop sent bs)

------------------------------------------------------------------------
-- Accepting

accept :: Socket -> IO (Socket, SockAddr)
accept sock =
  withFdSocket sock $ \s -> do
    sockaddr <- getSocketName sock
    let sz = sizeOfSocketAddress sockaddr
    allocaBytes sz $ \ sock_addr_ptr -> do
      with sz $ \ ptr_len -> do
        new_sock_fd <- throwSocketErrorIfMinus1RetryMayBlock "accept"
                    (T.threadWaitRead (fromIntegral s)) $
                    c_accept s sock_addr_ptr (castPtr ptr_len)
        setNonBlocking (fromIntegral new_sock_fd)
        addr <- peekSocketAddress sock_addr_ptr
        new_sock <- mkSocket (fromIntegral new_sock_fd)
        NS.connect new_sock addr
        return (new_sock, addr)

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError InvalidArgument
                                               loc Nothing Nothing)
                            "non-positive length"

foreign import ccall unsafe "sys/socket.h accept"
    c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen?? -} -> IO CInt

foreign import ccall unsafe "sys/socket.h send"
    c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "sys/socket.h recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
