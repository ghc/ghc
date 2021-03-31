{-# LANGUAGE CPP #-}

##include "HsNetDef.h"
#if defined(mingw32_HOST_OS)
#  include "windows.h"
#endif

module Network.Socket.Buffer (
    sendBufTo
  , sendBuf
  , recvBufFrom
  , recvBuf
  , recvBufNoWait
  ) where

#if !defined(mingw32_HOST_OS)
import Foreign.C.Error (getErrno, eAGAIN, eWOULDBLOCK)
#endif
import Foreign.Marshal.Alloc (alloca)
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import System.IO.Error (mkIOError, ioeSetErrorString, catchIOError)

#if defined(mingw32_HOST_OS)
import GHC.IO.FD (FD(..), readRawBufferPtr, writeRawBufferPtr)
#endif

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Name
import Network.Socket.Types

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent.  Applications are responsible for
-- ensuring that all data has been sent.
sendBufTo :: SocketAddress sa =>
             Socket -- (possibly) bound/connected Socket
          -> Ptr a
          -> Int         -- Data to send
          -> sa
          -> IO Int      -- Number of Bytes sent
sendBufTo s ptr nbytes sa =
  withSocketAddress sa $ \p_sa siz -> fromIntegral <$> do
    withFdSocket s $ \fd -> do
        let sz = fromIntegral siz
            n = fromIntegral nbytes
            flags = 0
        throwSocketErrorWaitWrite s "Network.Socket.sendBufTo" $
          c_sendto fd ptr n flags p_sa sz

#if defined(mingw32_HOST_OS)
socket2FD :: Socket -> IO FD
socket2FD s = do
  fd <- unsafeFdSocket s
  -- HACK, 1 means True
  return $ FD{ fdFD = fd, fdIsSocket_ = 1 }
#endif

-- | Send data to the socket. The socket must be connected to a remote
-- socket. Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
sendBuf :: Socket    -- Bound/Connected Socket
        -> Ptr Word8  -- Pointer to the data to send
        -> Int        -- Length of the buffer
        -> IO Int     -- Number of Bytes sent
sendBuf s str len = fromIntegral <$> do
#if defined(mingw32_HOST_OS)
-- writeRawBufferPtr is supposed to handle checking for errors, but it's broken
-- on x86_64 because of GHC bug #12010 so we duplicate the check here. The call
-- to throwSocketErrorIfMinus1Retry can be removed when no GHC version with the
-- bug is supported.
    fd <- socket2FD s
    let clen = fromIntegral len
    throwSocketErrorIfMinus1Retry "Network.Socket.sendBuf" $
      writeRawBufferPtr "Network.Socket.sendBuf" fd (castPtr str) 0 clen
#else
    withFdSocket s $ \fd -> do
        let flags = 0
            clen = fromIntegral len
        throwSocketErrorWaitWrite s "Network.Socket.sendBuf" $
          c_send fd str clen flags
#endif

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
recvBufFrom :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
recvBufFrom s ptr nbytes
    | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvBufFrom")
    | otherwise = withNewSocketAddress $ \ptr_sa sz -> alloca $ \ptr_len ->
        withFdSocket s $ \fd -> do
            poke ptr_len (fromIntegral sz)
            let cnbytes = fromIntegral nbytes
                flags = 0
            len <- throwSocketErrorWaitRead s "Network.Socket.recvBufFrom" $
                     c_recvfrom fd ptr cnbytes flags ptr_sa ptr_len
            sockaddr <- peekSocketAddress ptr_sa
                `catchIOError` \_ -> getPeerName s
            return (fromIntegral len, sockaddr)

-- | Receive data from the socket.  The socket must be in a connected
-- state. This function may return fewer bytes than specified.  If the
-- message is longer than the specified length, it may be discarded
-- depending on the type of socket.  This function may block until a
-- message arrives.
--
-- Considering hardware and network realities, the maximum number of
-- bytes to receive should be a small power of 2, e.g., 4096.
--
-- The return value is the length of received data. Zero means
-- EOF. Historical note: Version 2.8.x.y or earlier,
-- an EOF error was thrown. This was changed in version 3.0.
recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf s ptr nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvBuf")
 | otherwise   = do
#if defined(mingw32_HOST_OS)
-- see comment in sendBuf above.
    fd <- socket2FD s
    let cnbytes = fromIntegral nbytes
    len <- throwSocketErrorIfMinus1Retry "Network.Socket.recvBuf" $
             readRawBufferPtr "Network.Socket.recvBuf" fd ptr 0 cnbytes
#else
    len <- withFdSocket s $ \fd ->
        throwSocketErrorWaitRead s "Network.Socket.recvBuf" $
             c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
#endif
    return $ fromIntegral len

-- | Receive data from the socket. This function returns immediately
--   even if data is not available. In other words, IO manager is NOT
--   involved. The length of data is returned if received.
--   -1 is returned in the case of EAGAIN or EWOULDBLOCK.
--   -2 is returned in other error cases.
recvBufNoWait :: Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWait s ptr nbytes = withFdSocket s $ \fd -> do
#if defined(mingw32_HOST_OS)
    alloca $ \ptr_bytes -> do
      res <- c_ioctlsocket fd #{const FIONREAD} ptr_bytes
      avail <- peek ptr_bytes
      r <- if res == #{const NO_ERROR} && avail > 0 then
               c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
           else if avail == 0 then
               -- Socket would block, could also mean socket is closed but
               -- can't distinguish
               return (-1)
           else do err <- c_WSAGetLastError
                   if err == #{const WSAEWOULDBLOCK}
                       || err == #{const WSAEINPROGRESS} then
                       return (-1)
                     else
                        return (-2)
      return $ fromIntegral r

#else
    r <- c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    if r >= 0 then
        return $ fromIntegral r
      else do
        err <- getErrno
        if err == eAGAIN || err == eWOULDBLOCK then
            return (-1)
          else
            return (-2)
#endif

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
#else
foreign import CALLCONV SAFE_ON_WIN "ioctlsocket"
  c_ioctlsocket :: CInt -> CLong -> Ptr CULong -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "WSAGetLastError"
  c_WSAGetLastError :: IO CInt
#endif
foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "sendto"
  c_sendto :: CInt -> Ptr a -> CSize -> CInt -> Ptr sa -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "recvfrom"
  c_recvfrom :: CInt -> Ptr a -> CSize -> CInt -> Ptr sa -> Ptr CInt -> IO CInt
