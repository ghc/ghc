#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Process
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Process where
import Control.Exception    ( bracket )
import Control.Monad        ( liftM5 )
import Foreign              ( Ptr, peekByteOff, allocaBytes, pokeByteOff
                            , plusPtr )
import Foreign.C.Types      ( CUInt(..) )
import System.Win32.File    ( closeHandle )
import System.Win32.Types

##include "windows_cconv.h"

#include <windows.h>
#include <tlhelp32.h>

-- constant to wait for a very long time.
iNFINITE :: DWORD
iNFINITE = #{const INFINITE}

foreign import WINDOWS_CCONV unsafe "windows.h Sleep"
  sleep :: DWORD -> IO ()


type ProcessId = DWORD
type ProcessHandle = HANDLE
type ProcessAccessRights = DWORD
#{enum ProcessAccessRights,
    , pROCESS_ALL_ACCESS            = PROCESS_ALL_ACCESS
    , pROCESS_CREATE_PROCESS        = PROCESS_CREATE_PROCESS
    , pROCESS_CREATE_THREAD         = PROCESS_CREATE_THREAD
    , pROCESS_DUP_HANDLE            = PROCESS_DUP_HANDLE
    , pROCESS_QUERY_INFORMATION     = PROCESS_QUERY_INFORMATION
    , pROCESS_SET_QUOTA             = PROCESS_SET_QUOTA
    , pROCESS_SET_INFORMATION       = PROCESS_SET_INFORMATION
    , pROCESS_TERMINATE             = PROCESS_TERMINATE
    , pROCESS_VM_OPERATION          = PROCESS_VM_OPERATION
    , pROCESS_VM_READ               = PROCESS_VM_READ
    , pROCESS_VM_WRITE              = PROCESS_VM_WRITE
    , sYNCHORNIZE                   = SYNCHRONIZE 
    }

foreign import WINDOWS_CCONV unsafe "windows.h OpenProcess"
    c_OpenProcess :: ProcessAccessRights -> BOOL -> ProcessId -> IO ProcessHandle


openProcess :: ProcessAccessRights -> BOOL -> ProcessId -> IO ProcessHandle
openProcess r inh i = failIfNull "OpenProcess" $ c_OpenProcess r inh i

foreign import WINDOWS_CCONV unsafe "windows.h GetProcessId"
    c_GetProcessId :: ProcessHandle -> IO ProcessId

getProcessId :: ProcessHandle -> IO ProcessId
getProcessId h = failIfZero "GetProcessId" $ c_GetProcessId h

foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentProcess"
    c_GetCurrentProcess :: IO ProcessHandle 

foreign import WINDOWS_CCONV unsafe "windows.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO ProcessId

getCurrentProcessId :: IO ProcessId
getCurrentProcessId = c_GetCurrentProcessId

getCurrentProcess :: IO ProcessHandle
getCurrentProcess = c_GetCurrentProcess

foreign import WINDOWS_CCONV unsafe "windows.h TerminateProcess"
    c_TerminateProcess :: ProcessHandle -> CUInt -> IO Bool

terminateProcessById :: ProcessId -> IO ()
terminateProcessById p = bracket
    (openProcess pROCESS_TERMINATE False p)
    closeHandle
    (\h -> failIfFalse_ "TerminateProcess" $ c_TerminateProcess h 1)

type Th32SnapHandle = HANDLE
type Th32SnapFlags = DWORD
-- | ProcessId, number of threads, parent ProcessId, process base priority, path of executable file
type ProcessEntry32 = (ProcessId, Int, ProcessId, LONG, String)

#{enum Th32SnapFlags,
    , tH32CS_SNAPALL        = TH32CS_SNAPALL
    , tH32CS_SNAPHEAPLIST   = TH32CS_SNAPHEAPLIST
    , tH32CS_SNAPMODULE     = TH32CS_SNAPMODULE
    , tH32CS_SNAPPROCESS    = TH32CS_SNAPPROCESS
    , tH32CS_SNAPTHREAD     = TH32CS_SNAPTHREAD
    }
{-
    , tH32CS_SNAPGETALLMODS = TH32CS_GETALLMODS
    , tH32CS_SNAPNOHEAPS    = TH32CS_SNAPNOHEAPS 
-}

foreign import WINDOWS_CCONV unsafe "tlhelp32.h CreateToolhelp32Snapshot"
    c_CreateToolhelp32Snapshot :: Th32SnapFlags -> ProcessId -> IO Th32SnapHandle

foreign import WINDOWS_CCONV unsafe "tlhelp32.h Process32FirstW"
    c_Process32First :: Th32SnapHandle -> Ptr ProcessEntry32 -> IO BOOL

foreign import WINDOWS_CCONV unsafe "tlhelp32.h Process32NextW"
    c_Process32Next :: Th32SnapHandle -> Ptr ProcessEntry32 -> IO BOOL

-- | Create a snapshot of specified resources.  Call closeHandle to close snapshot.
createToolhelp32Snapshot :: Th32SnapFlags -> Maybe ProcessId -> IO Th32SnapHandle
createToolhelp32Snapshot f p
    = failIfNull "CreateToolhelp32Snapshot" $ c_CreateToolhelp32Snapshot
        f (maybe 0 id p)

withTh32Snap :: Th32SnapFlags -> Maybe ProcessId -> (Th32SnapHandle -> IO a) -> IO a
withTh32Snap f p = bracket (createToolhelp32Snapshot f p) (closeHandle)


peekProcessEntry32 :: Ptr ProcessEntry32 -> IO ProcessEntry32
peekProcessEntry32 buf = liftM5 (,,,,)
    ((#peek PROCESSENTRY32W, th32ProcessID) buf)
    ((#peek PROCESSENTRY32W, cntThreads) buf)
    ((#peek PROCESSENTRY32W, th32ParentProcessID) buf)
    ((#peek PROCESSENTRY32W, pcPriClassBase) buf)
    (peekTString $ (#ptr PROCESSENTRY32W, szExeFile) buf)

-- | Enumerate processes using Process32First and Process32Next
th32SnapEnumProcesses :: Th32SnapHandle -> IO [ProcessEntry32]
th32SnapEnumProcesses h = allocaBytes (#size PROCESSENTRY32W) $ \pe -> do
    (#poke PROCESSENTRY32W, dwSize) pe ((#size PROCESSENTRY32W)::DWORD)
    ok <- c_Process32First h pe
    readAndNext ok pe []
    where
        readAndNext ok pe res
            | not ok    = do
                err <- getLastError
                if err == (#const ERROR_NO_MORE_FILES)
                    then return $ reverse res
                    else failWith "th32SnapEnumProcesses: Process32First/Process32Next" err
            | otherwise = do
                entry <- peekProcessEntry32 pe
                ok' <- c_Process32Next h pe
                readAndNext ok' pe (entry:res)
