{-# LANGUAGE CPP #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- | File locking for Windows.
module GHC.Internal.IO.Handle.Lock.Windows where

#include "HsBaseConfig.h"

#if !defined(mingw32_HOST_OS)
-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Types ()
#else

#include <windows.h>

import GHC.Internal.Data.Bits
import GHC.Internal.Data.Function
import GHC.Internal.IO.Handle.Windows (handleToHANDLE)
import GHC.Internal.Foreign.C.Error
import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.Marshal.Alloc
import GHC.Internal.Foreign.Marshal.Utils
import GHC.Internal.Base
import qualified GHC.Internal.Event.Windows as Mgr
import GHC.Internal.Event.Windows (LPOVERLAPPED, withOverlapped)
import GHC.Internal.IO.FD
import GHC.Internal.IO.Handle.FD
import GHC.Internal.IO.Handle.Types (Handle)
import GHC.Internal.IO.Handle.Lock.Common (LockMode(..))
import GHC.Internal.IO.SubSystem
import GHC.Internal.Windows

lockImpl :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImpl = lockImplPOSIX <!> lockImplWinIO

lockImplWinIO :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImplWinIO h ctx mode block = do
  wh      <- handleToHANDLE h
  fix $ \retry ->
          do retcode <- Mgr.withException ctx $
                          withOverlapped ctx wh 0 (startCB wh) completionCB
             case () of
              _ | retcode == #{const ERROR_OPERATION_ABORTED} -> retry
                | retcode == #{const ERROR_SUCCESS}           -> return True
                | retcode == #{const ERROR_LOCK_VIOLATION} && not block
                    -> return False
                | otherwise -> failWith ctx retcode
    where
      cmode = case mode of
                SharedLock    -> 0
                ExclusiveLock -> #{const LOCKFILE_EXCLUSIVE_LOCK}
      flags = if block
                 then cmode
                 else cmode .|. #{const LOCKFILE_FAIL_IMMEDIATELY}

      startCB wh lpOverlapped = do
        ret <- c_LockFileEx wh flags 0 #{const INFINITE} #{const INFINITE}
                            lpOverlapped
        return $ Mgr.CbNone ret

      completionCB err _dwBytes
        | err == #{const ERROR_SUCCESS} = Mgr.ioSuccess 0
        | otherwise                     = Mgr.ioFailed err

lockImplPOSIX :: Handle -> String -> LockMode -> Bool -> IO Bool
lockImplPOSIX h ctx mode block = do
  FD{fdFD = fd} <- handleToFd h
  wh <- throwErrnoIf (== iNVALID_HANDLE_VALUE) ctx $ c_get_osfhandle fd
  allocaBytes sizeof_OVERLAPPED $ \ovrlpd -> do
    fillBytes ovrlpd 0 sizeof_OVERLAPPED
    let flags = cmode .|. (if block then 0 else #{const LOCKFILE_FAIL_IMMEDIATELY})
    -- We want to lock the whole file without looking up its size to be
    -- consistent with what flock does. According to documentation of LockFileEx
    -- "locking a region that goes beyond the current end-of-file position is
    -- not an error", hence we pass maximum value as the number of bytes to
    -- lock.
    fix $ \retry -> c_LockFileEx wh flags 0 #{const INFINITE} #{const INFINITE}
                                 ovrlpd >>= \case
      True  -> return True
      False -> getLastError >>= \err -> if
        | not block && err == #{const ERROR_LOCK_VIOLATION} -> return False
        | err == #{const ERROR_OPERATION_ABORTED}           -> retry
        | otherwise                                         -> failWith ctx err
  where
    sizeof_OVERLAPPED = #{size OVERLAPPED}

    cmode = case mode of
      SharedLock    -> 0
      ExclusiveLock -> #{const LOCKFILE_EXCLUSIVE_LOCK}

unlockImpl :: Handle -> IO ()
unlockImpl = unlockImplPOSIX <!> unlockImplWinIO

unlockImplWinIO :: Handle -> IO ()
unlockImplWinIO h = do
  wh <- handleToHANDLE h
  _ <- Mgr.withException "unlockImpl" $
          withOverlapped "unlockImpl" wh 0 (startCB wh) completionCB
  return ()
    where
      startCB wh lpOverlapped = do
        ret <- c_UnlockFileEx wh 0 #{const INFINITE} #{const INFINITE}
                              lpOverlapped
        return $ Mgr.CbNone ret

      completionCB err _dwBytes
        | err == #{const ERROR_SUCCESS} = Mgr.ioSuccess 0
        | otherwise                     = Mgr.ioFailed err

unlockImplPOSIX :: Handle -> IO ()
unlockImplPOSIX h = do
  FD{fdFD = fd} <- handleToFd h
  wh <- throwErrnoIf (== iNVALID_HANDLE_VALUE) "hUnlock" $ c_get_osfhandle fd
  allocaBytes sizeof_OVERLAPPED $ \ovrlpd -> do
    fillBytes ovrlpd 0 sizeof_OVERLAPPED
    c_UnlockFileEx wh 0 #{const INFINITE} #{const INFINITE} ovrlpd >>= \case
      True  -> return ()
      False -> getLastError >>= failWith "hUnlock"
  where
    sizeof_OVERLAPPED = #{size OVERLAPPED}

-- https://msdn.microsoft.com/en-us/library/aa297958.aspx
foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365203.aspx
foreign import ccall interruptible "LockFileEx"
  c_LockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED
               -> IO BOOL

-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa365716.aspx
foreign import ccall interruptible "UnlockFileEx"
  c_UnlockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED -> IO BOOL

#endif
