{-# LANGUAGE CPP #-}

module GHC.Runtime.Utils
  ( runWithPipes
  )
where

import GHC.Prelude

#if defined(mingw32_HOST_OS)
import Foreign.C
import GHC.IO.Handle.FD (fdToHandle)
import GHC.Utils.Exception as Ex
# if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows (handleToHANDLE)
import GHC.Event.Windows (associateHandle')
# endif
#else
import System.Posix as Posix
#endif
import System.Process
import System.IO

runWithPipes :: (CreateProcess -> IO ProcessHandle)
             -> FilePath -> [String] -> [String] -> IO (ProcessHandle, Handle, Handle)
#if defined(mingw32_HOST_OS)
foreign import ccall "io.h _close"
   c__close :: CInt -> IO CInt

foreign import ccall unsafe "io.h _get_osfhandle"
   _get_osfhandle :: CInt -> IO CIntPtr

runWithPipesPOSIX :: (CreateProcess -> IO ProcessHandle)
                  -> FilePath -> [String] -> [String] -> IO (ProcessHandle, Handle, Handle)
runWithPipesPOSIX createProc prog pre_opts opts = do
    (rfd1, wfd1) <- createPipeFd -- we read on rfd1
    (rfd2, wfd2) <- createPipeFd -- we write on wfd2
    wh_client    <- _get_osfhandle wfd1
    rh_client    <- _get_osfhandle rfd2
    let args = pre_opts ++ (show wh_client : show rh_client : opts)
    ph <- createProc (proc prog args)
    rh <- mkHandle rfd1
    wh <- mkHandle wfd2
    return (ph, rh, wh)
      where mkHandle :: CInt -> IO Handle
            mkHandle fd = (fdToHandle fd) `Ex.onException` (c__close fd)

# if defined (__IO_MANAGER_WINIO__)
runWithPipesNative :: (CreateProcess -> IO ProcessHandle)
                   -> FilePath -> [String] -> [String] -> IO (ProcessHandle, Handle, Handle)
runWithPipesNative createProc prog pre_opts opts = do
    (rh, wfd1) <- createPipe -- we read on rfd1
    (rfd2, wh) <- createPipe -- we write on wfd2
    wh_client    <- handleToHANDLE wfd1
    rh_client    <- handleToHANDLE rfd2
    -- Associate the handle with the current manager
    -- but don't touch the ones we're passing to the child
    -- since it needs to register the handle with its own manager.
    associateHandle' =<< handleToHANDLE rh
    associateHandle' =<< handleToHANDLE wh
    let args = pre_opts ++ (show wh_client : show rh_client : opts)
    ph <- createProc (proc prog args)
    return (ph, rh, wh)

runWithPipes = runWithPipesPOSIX <!> runWithPipesNative
# else
runWithPipes = runWithPipesPOSIX
# endif
#else
runWithPipes createProc prog pre_opts opts = do
    (rfd1, wfd1) <- Posix.createPipe -- we read on rfd1
    (rfd2, wfd2) <- Posix.createPipe -- we write on wfd2
    setFdOption rfd1 CloseOnExec True
    setFdOption wfd2 CloseOnExec True
    let args = pre_opts ++ (show wfd1 : show rfd2 : opts)
    ph <- createProc (proc prog args)
    closeFd wfd1
    closeFd rfd2
    rh <- fdToHandle rfd1
    wh <- fdToHandle wfd2
    return (ph, rh, wh)
#endif

