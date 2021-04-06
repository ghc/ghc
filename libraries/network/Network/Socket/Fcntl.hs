{-# LANGUAGE CPP #-}

module Network.Socket.Fcntl where

import qualified System.Posix.Internals

#if !defined(mingw32_HOST_OS)
import Network.Socket.Cbits
#endif
import Network.Socket.Imports

-- | Set the nonblocking flag on Unix.
--   On Windows, nothing is done.
setNonBlockIfNeeded :: CInt -> IO ()
setNonBlockIfNeeded fd =
    System.Posix.Internals.setNonBlockingFD fd True

-- | Set the close_on_exec flag on Unix.
--   On Windows, nothing is done.
--
--   Since 2.7.0.0.
setCloseOnExecIfNeeded :: CInt -> IO ()
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
setCloseOnExecIfNeeded _ = return ()
#else
setCloseOnExecIfNeeded fd = System.Posix.Internals.setCloseOnExec fd
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "fcntl"
  c_fcntl_read  :: CInt -> CInt -> CInt -> IO CInt
#endif

-- | Get the close_on_exec flag.
--   On Windows, this function always returns 'False'.
--
--   Since 2.7.0.0.
getCloseOnExec :: CInt -> IO Bool
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
getCloseOnExec _ = return False
#else
getCloseOnExec fd = do
    flags <- c_fcntl_read fd fGetFd 0
    let ret = flags .&. fdCloexec
    return (ret /= 0)
#endif

-- | Get the nonblocking flag.
--   On Windows, this function always returns 'False'.
--
--   Since 2.7.0.0.
getNonBlock :: CInt -> IO Bool
#if defined(mingw32_HOST_OS)
getNonBlock _ = return False
#else
getNonBlock fd = do
    flags <- c_fcntl_read fd fGetFl 0
    let ret = flags .&. oNonBlock
    return (ret /= 0)
#endif
