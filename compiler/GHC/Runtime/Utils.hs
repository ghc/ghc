{-# LANGUAGE CPP #-}

module GHC.Runtime.Utils
  ( runWithPipes
  )
where

import GHC.Prelude
import GHC.Utils.Exception (onException)

import System.Process
import System.Process.CommunicationHandle
import System.IO

#if defined(mingw32_HOST_OS)
import Control.Concurrent (rtsSupportsBoundThreads)
import GHC.IO.Handle.Windows (handleToHANDLE)
import GHC.IO.SubSystem (isWindowsNativeIO)
import qualified System.Win32 as Win32
#endif

-- | Spawn a process connected to us by two pipes. Important to close our copy
-- of the endpoints passed to the child so each side only keeps open copies it
-- needs. If we don't do that, then closing one end wouldn't interrupt reads on
-- the other.
runWithPipes :: (CreateProcess -> IO ProcessHandle)
             -> FilePath -> [String] -> [String] -> IO (ProcessHandle, Handle, Handle)
runWithPipes createProc prog pre_opts opts = do
    (rh, chTheyWrite) <- fixedCreateWeReadTheyWritePipe
    (chTheyRead, wh)  <- fixedCreateTheyReadWeWritePipe
    let args = pre_opts ++ (show chTheyWrite : show chTheyRead : opts)
        closeTheirs = do
          closeCommunicationHandle chTheyWrite
          closeCommunicationHandle chTheyRead
    -- These handles have no finalizers, so close them ourselves if the spawn fails.
    ph <- createProc (proc prog args)
            `onException` (closeTheirs >> hClose rh >> hClose wh)
    closeTheirs
    return (ph, rh, wh)

-- | 'createWeReadTheyWritePipe', except in a non-threaded runtime on Windows
-- without WinIO. There process gives us a synchronous native 'Handle', and
-- reading from it is a safe call to @ReadFile@, which blocks every Haskell
-- thread until data arrives. As 'GHCi.Message.Async' always has a thread
-- waiting to read the pipe, no other thread would get to run. So in that case
-- we swap our end for a file descriptor 'Handle', whose reads only block the
-- reading thread.
fixedCreateWeReadTheyWritePipe :: IO (Handle, CommunicationHandle)
fixedCreateWeReadTheyWritePipe = do
    (h, ch) <- createWeReadTheyWritePipe
    h' <- toFdHandleIfNeeded h
    return (h', ch)

-- | 'createTheyReadWeWritePipe', with the same exception as
-- 'fixedCreateWeReadTheyWritePipe'.
fixedCreateTheyReadWeWritePipe :: IO (CommunicationHandle, Handle)
fixedCreateTheyReadWeWritePipe = do
    (ch, h) <- createTheyReadWeWritePipe
    h' <- toFdHandleIfNeeded h
    return (ch, h')

toFdHandleIfNeeded :: Handle -> IO Handle
#if defined(mingw32_HOST_OS)
-- The native 'Handle' has a finalizer that closes its @HANDLE@, so we wrap a
-- duplicate and close the original.
toFdHandleIfNeeded h
  | not rtsSupportsBoundThreads && not isWindowsNativeIO = do
      me <- Win32.getCurrentProcess
      hh <- handleToHANDLE h
      dup <- Win32.duplicateHandle me hh me 0 False Win32.dUPLICATE_SAME_ACCESS
      hClose h
      Win32.hANDLEToHandle dup
  | otherwise = return h
#else
toFdHandleIfNeeded = return
#endif
