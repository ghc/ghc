{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Win32SelfUpgrade
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Support for self-upgrading executables on Windows platforms.
-----------------------------------------------------------------------------
module Distribution.Client.Win32SelfUpgrade (
-- * Explanation
--
-- | Windows inherited a design choice from DOS that while initially innocuous
-- has rather unfortunate consequences. It maintains the invariant that every
-- open file has a corresponding name on disk. One positive consequence of this
-- is that an executable can always find it's own executable file. The downside
-- is that a program cannot be deleted or upgraded while it is running without
-- hideous workarounds. This module implements one such hideous workaround.
--
-- The basic idea is:
--
-- * Move our own exe file to a new name
-- * Copy a new exe file to the previous name
-- * Run the new exe file, passing our own PID and new path
-- * Wait for the new process to start
-- * Close the new exe file
-- * Exit old process
--
-- Then in the new process:
--
-- * Inform the old process that we've started
-- * Wait for the old process to die
-- * Delete the old exe file
-- * Exit new process
--

    possibleSelfUpgrade,
    deleteOldExeFile,
  ) where

#ifdef mingw32_HOST_OS

import qualified System.Win32 as Win32
import System.Win32 (DWORD, BOOL, HANDLE, LPCTSTR)
import Foreign.Ptr (Ptr, nullPtr)
import System.Process (runProcess)
import System.Directory (canonicalizePath)
import System.FilePath (takeBaseName, replaceBaseName, equalFilePath)

import Distribution.Verbosity as Verbosity (Verbosity, showForCabal)
import Distribution.Simple.Utils (debug, info)

import Prelude hiding (log)

-- | If one of the given files is our own exe file then we arrange things such
-- that the nested action can replace our own exe file.
--
-- We require that the new process accepts a command line invocation that
-- calls 'deleteOldExeFile', passing in the PID and exe file.
--
possibleSelfUpgrade :: Verbosity
                    -> [FilePath]
                    -> IO a -> IO a
possibleSelfUpgrade verbosity newPaths action = do
  dstPath <- canonicalizePath =<< Win32.getModuleFileName Win32.nullHANDLE

  newPaths' <- mapM canonicalizePath newPaths
  let doingSelfUpgrade = any (equalFilePath dstPath) newPaths'

  if not doingSelfUpgrade
    then action
    else do
      info verbosity $ "cabal-install does the replace-own-exe-file dance..."
      tmpPath <- moveOurExeOutOfTheWay verbosity
      result <- action
      scheduleOurDemise verbosity dstPath tmpPath
        (\pid path -> ["win32selfupgrade", pid, path
                      ,"--verbose=" ++ Verbosity.showForCabal verbosity])
      return result

-- | The name of a Win32 Event object that we use to synchronise between the
-- old and new processes. We need to synchronise to make sure that the old
-- process has not yet terminated by the time the new one starts up and looks
-- for the old process. Otherwise the old one might have already terminated
-- and we could not wait on it terminating reliably (eg the PID might get
-- re-used).
--
syncEventName :: String
syncEventName = "Local\\cabal-install-upgrade"

-- | The first part of allowing our exe file to be replaced is to move the
-- existing exe file out of the way. Although we cannot delete our exe file
-- while we're still running, fortunately we can rename it, at least within
-- the same directory.
--
moveOurExeOutOfTheWay :: Verbosity -> IO FilePath
moveOurExeOutOfTheWay verbosity = do
  ourPID  <-       getCurrentProcessId
  dstPath <- Win32.getModuleFileName Win32.nullHANDLE

  let tmpPath = replaceBaseName dstPath (takeBaseName dstPath ++ show ourPID)

  debug verbosity $ "moving " ++ dstPath ++ " to " ++ tmpPath
  Win32.moveFile dstPath tmpPath
  return tmpPath

-- | Assuming we've now installed the new exe file in the right place, we
-- launch it and ask it to delete our exe file when we eventually terminate.
--
scheduleOurDemise :: Verbosity -> FilePath -> FilePath
                  -> (String -> FilePath -> [String]) -> IO ()
scheduleOurDemise verbosity dstPath tmpPath mkArgs = do
  ourPID <- getCurrentProcessId
  event  <- createEvent syncEventName

  let args = mkArgs (show ourPID) tmpPath
  log $ "launching child " ++ unwords (dstPath : map show args)
  _ <- runProcess dstPath args Nothing Nothing Nothing Nothing Nothing

  log $ "waiting for the child to start up"
  waitForSingleObject event (10*1000) -- wait at most 10 sec
  log $ "child started ok"

  where
    log msg = debug verbosity ("Win32Reinstall.parent: " ++ msg)

-- | Assuming we're now in the new child process, we've been asked by the old
-- process to wait for it to terminate and then we can remove the old exe file
-- that it renamed itself to.
--
deleteOldExeFile :: Verbosity -> Int -> FilePath -> IO ()
deleteOldExeFile verbosity oldPID tmpPath = do
  log $ "process started. Will delete exe file of process "
     ++ show oldPID ++ " at path " ++ tmpPath

  log $ "getting handle of parent process " ++ show oldPID
  oldPHANDLE <- Win32.openProcess Win32.sYNCHORNIZE False (fromIntegral oldPID)

  log $ "synchronising with parent"
  event <- openEvent syncEventName
  setEvent event

  log $ "waiting for parent process to terminate"
  waitForSingleObject oldPHANDLE Win32.iNFINITE
  log $ "parent process terminated"

  log $ "deleting parent's old .exe file"
  Win32.deleteFile tmpPath

  where
    log msg = debug verbosity ("Win32Reinstall.child: " ++ msg)

------------------------
-- Win32 foreign imports
--

-- A bunch of functions sadly not provided by the Win32 package.

#ifdef x86_64_HOST_ARCH
#define CALLCONV ccall
#else
#define CALLCONV stdcall
#endif

foreign import CALLCONV unsafe "windows.h GetCurrentProcessId"
  getCurrentProcessId :: IO DWORD

foreign import CALLCONV unsafe "windows.h WaitForSingleObject"
  waitForSingleObject_ :: HANDLE -> DWORD -> IO DWORD

waitForSingleObject :: HANDLE -> DWORD -> IO ()
waitForSingleObject handle timeout =
  Win32.failIf_ bad "WaitForSingleObject" $
    waitForSingleObject_ handle timeout
  where
    bad result   = not (result == 0 || result == wAIT_TIMEOUT)
    wAIT_TIMEOUT = 0x00000102

foreign import CALLCONV unsafe "windows.h CreateEventW"
  createEvent_ :: Ptr () -> BOOL -> BOOL -> LPCTSTR -> IO HANDLE

createEvent :: String -> IO HANDLE
createEvent name = do
  Win32.failIfNull "CreateEvent" $
    Win32.withTString name $
      createEvent_ nullPtr False False

foreign import CALLCONV unsafe "windows.h OpenEventW"
  openEvent_ :: DWORD -> BOOL -> LPCTSTR -> IO HANDLE

openEvent :: String -> IO HANDLE
openEvent name = do
  Win32.failIfNull "OpenEvent" $
    Win32.withTString name $
      openEvent_ eVENT_MODIFY_STATE False
  where
    eVENT_MODIFY_STATE :: DWORD
    eVENT_MODIFY_STATE = 0x0002

foreign import CALLCONV unsafe "windows.h SetEvent"
  setEvent_ :: HANDLE -> IO BOOL

setEvent :: HANDLE -> IO ()
setEvent handle =
  Win32.failIfFalse_ "SetEvent" $
    setEvent_ handle

#else

import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (die')

possibleSelfUpgrade :: Verbosity
                    -> [FilePath]
                    -> IO a -> IO a
possibleSelfUpgrade _ _ action = action

deleteOldExeFile :: Verbosity -> Int -> FilePath -> IO ()
deleteOldExeFile verbosity _ _ = die' verbosity "win32selfupgrade not needed except on win32"

#endif
