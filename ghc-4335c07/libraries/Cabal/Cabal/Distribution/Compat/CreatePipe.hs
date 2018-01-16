{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Compat.CreatePipe (createPipe) where

import System.IO (Handle, hSetEncoding, localeEncoding)

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Compat.Stack

-- The mingw32_HOST_OS CPP macro is GHC-specific
#ifdef mingw32_HOST_OS
import qualified Prelude
import Control.Exception (onException)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff)
import GHC.IO.FD (mkFD)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.Handle.FD (mkHandleFromFD)
import System.IO (IOMode(ReadMode, WriteMode))
#elif defined ghcjs_HOST_OS
#else
import System.Posix.IO (fdToHandle)
import qualified System.Posix.IO as Posix
#endif

createPipe :: IO (Handle, Handle)
-- The mingw32_HOST_OS CPP macro is GHC-specific
#ifdef mingw32_HOST_OS
createPipe = do
    (readfd, writefd) <- allocaArray 2 $ \ pfds -> do
        throwErrnoIfMinus1_ "_pipe" $ c__pipe pfds 2 ({- _O_BINARY -} 32768)
        readfd <- peek pfds
        writefd <- peekElemOff pfds 1
        return (readfd, writefd)
    (do readh <- fdToHandle readfd ReadMode
        writeh <- fdToHandle writefd WriteMode
        hSetEncoding readh localeEncoding
        hSetEncoding writeh localeEncoding
        return (readh, writeh)) `onException` (close readfd >> close writefd)
  where
    fdToHandle :: CInt -> IOMode -> NoCallStackIO Handle
    fdToHandle fd mode = do
        (fd', deviceType) <- mkFD fd mode (Just (Stream, 0, 0)) False False
        mkHandleFromFD fd' deviceType "" mode False Nothing

    close :: CInt -> IO ()
    close = throwErrnoIfMinus1_ "_close" . c__close
      where _ = callStack -- TODO: attach call stack to exception

    _ = callStack -- TODO: attach call stack to exceptions

foreign import ccall "io.h _pipe" c__pipe ::
    Ptr CInt -> CUInt -> CInt -> Prelude.IO CInt

foreign import ccall "io.h _close" c__close ::
    CInt -> Prelude.IO CInt
#elif defined ghcjs_HOST_OS
createPipe = error "createPipe"
  where
    _ = callStack
#else
createPipe = do
    (readfd, writefd) <- Posix.createPipe
    readh <- fdToHandle readfd
    writeh <- fdToHandle writefd
    hSetEncoding readh localeEncoding
    hSetEncoding writeh localeEncoding
    return (readh, writeh)
  where
    _ = callStack
#endif
