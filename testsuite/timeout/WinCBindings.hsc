{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module WinCBindings where

#if defined(mingw32_HOST_OS)

import Foreign
import System.Win32.File
import System.Win32.Types

#include <windows.h>

type LPPROCESS_INFORMATION = Ptr PROCESS_INFORMATION
data PROCESS_INFORMATION = PROCESS_INFORMATION
    { piProcess :: HANDLE
    , piThread :: HANDLE
    , piProcessId :: DWORD
    , piThreadId :: DWORD
    } deriving Show

instance Storable PROCESS_INFORMATION where
    sizeOf = const #size PROCESS_INFORMATION
    alignment = sizeOf
    poke buf pi = do
        (#poke PROCESS_INFORMATION, hProcess)    buf (piProcess   pi)
        (#poke PROCESS_INFORMATION, hThread)     buf (piThread    pi)
        (#poke PROCESS_INFORMATION, dwProcessId) buf (piProcessId pi)
        (#poke PROCESS_INFORMATION, dwThreadId)  buf (piThreadId  pi)

    peek buf = do
        vhProcess    <- (#peek PROCESS_INFORMATION, hProcess)    buf
        vhThread     <- (#peek PROCESS_INFORMATION, hThread)     buf
        vdwProcessId <- (#peek PROCESS_INFORMATION, dwProcessId) buf
        vdwThreadId  <- (#peek PROCESS_INFORMATION, dwThreadId)  buf
        return $ PROCESS_INFORMATION {
            piProcess   = vhProcess,
            piThread    = vhThread,
            piProcessId = vdwProcessId,
            piThreadId  = vdwThreadId}

type LPSTARTUPINFO = Ptr STARTUPINFO
data STARTUPINFO = STARTUPINFO
    { siCb :: DWORD
    , siDesktop :: LPTSTR
    , siTitle :: LPTSTR
    , siX :: DWORD
    , siY :: DWORD
    , siXSize :: DWORD
    , siYSize :: DWORD
    , siXCountChars :: DWORD
    , siYCountChars :: DWORD
    , siFillAttribute :: DWORD
    , siFlags :: DWORD
    , siShowWindow :: WORD
    , siStdInput :: HANDLE
    , siStdOutput :: HANDLE
    , siStdError :: HANDLE
    } deriving Show

instance Storable STARTUPINFO where
    sizeOf = const #size STARTUPINFO
    alignment = sizeOf
    poke buf si = do
        (#poke STARTUPINFO, cb)              buf (siCb si)
        (#poke STARTUPINFO, lpDesktop)       buf (siDesktop si)
        (#poke STARTUPINFO, lpTitle)         buf (siTitle si)
        (#poke STARTUPINFO, dwX)             buf (siX si)
        (#poke STARTUPINFO, dwY)             buf (siY si)
        (#poke STARTUPINFO, dwXSize)         buf (siXSize si)
        (#poke STARTUPINFO, dwYSize)         buf (siYSize si)
        (#poke STARTUPINFO, dwXCountChars)   buf (siXCountChars si)
        (#poke STARTUPINFO, dwYCountChars)   buf (siYCountChars si)
        (#poke STARTUPINFO, dwFillAttribute) buf (siFillAttribute si)
        (#poke STARTUPINFO, dwFlags)         buf (siFlags si)
        (#poke STARTUPINFO, wShowWindow)     buf (siShowWindow si)
        (#poke STARTUPINFO, hStdInput)       buf (siStdInput si)
        (#poke STARTUPINFO, hStdOutput)      buf (siStdOutput si)
        (#poke STARTUPINFO, hStdError)       buf (siStdError si)

    peek buf = do
        vcb              <- (#peek STARTUPINFO, cb)              buf
        vlpDesktop       <- (#peek STARTUPINFO, lpDesktop)       buf
        vlpTitle         <- (#peek STARTUPINFO, lpTitle)         buf
        vdwX             <- (#peek STARTUPINFO, dwX)             buf
        vdwY             <- (#peek STARTUPINFO, dwY)             buf
        vdwXSize         <- (#peek STARTUPINFO, dwXSize)         buf
        vdwYSize         <- (#peek STARTUPINFO, dwYSize)         buf
        vdwXCountChars   <- (#peek STARTUPINFO, dwXCountChars)   buf
        vdwYCountChars   <- (#peek STARTUPINFO, dwYCountChars)   buf
        vdwFillAttribute <- (#peek STARTUPINFO, dwFillAttribute) buf
        vdwFlags         <- (#peek STARTUPINFO, dwFlags)         buf
        vwShowWindow     <- (#peek STARTUPINFO, wShowWindow)     buf
        vhStdInput       <- (#peek STARTUPINFO, hStdInput)       buf
        vhStdOutput      <- (#peek STARTUPINFO, hStdOutput)      buf
        vhStdError       <- (#peek STARTUPINFO, hStdError)       buf
        return $ STARTUPINFO {
            siCb            =  vcb,
            siDesktop       =  vlpDesktop,
            siTitle         =  vlpTitle,
            siX             =  vdwX,
            siY             =  vdwY,
            siXSize         =  vdwXSize,
            siYSize         =  vdwYSize,
            siXCountChars   =  vdwXCountChars,
            siYCountChars   =  vdwYCountChars,
            siFillAttribute =  vdwFillAttribute,
            siFlags         =  vdwFlags,
            siShowWindow    =  vwShowWindow,
            siStdInput      =  vhStdInput,
            siStdOutput     =  vhStdOutput,
            siStdError      =  vhStdError}

foreign import stdcall unsafe "windows.h WaitForSingleObject"
    waitForSingleObject :: HANDLE -> DWORD -> IO DWORD

cWAIT_ABANDONED :: DWORD
cWAIT_ABANDONED = #const WAIT_ABANDONED

cWAIT_OBJECT_0 :: DWORD
cWAIT_OBJECT_0 = #const WAIT_OBJECT_0

cWAIT_TIMEOUT :: DWORD
cWAIT_TIMEOUT = #const WAIT_TIMEOUT

foreign import stdcall unsafe "windows.h GetExitCodeProcess"
    getExitCodeProcess :: HANDLE -> LPDWORD -> IO BOOL

foreign import stdcall unsafe "windows.h TerminateJobObject"
    terminateJobObject :: HANDLE -> UINT -> IO BOOL

foreign import stdcall unsafe "windows.h AssignProcessToJobObject"
    assignProcessToJobObject :: HANDLE -> HANDLE -> IO BOOL

foreign import stdcall unsafe "windows.h CreateJobObjectW"
    createJobObjectW :: LPSECURITY_ATTRIBUTES -> LPCTSTR -> IO HANDLE

foreign import stdcall unsafe "windows.h CreateProcessW"
    createProcessW :: LPCTSTR -> LPTSTR
                   -> LPSECURITY_ATTRIBUTES -> LPSECURITY_ATTRIBUTES
                   -> BOOL -> DWORD -> LPVOID -> LPCTSTR -> LPSTARTUPINFO
                   -> LPPROCESS_INFORMATION -> IO BOOL

#endif

