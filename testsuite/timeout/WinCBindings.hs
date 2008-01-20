{-# INCLUDE <windows.h> #-}
{-# LINE 1 "WinCBindings.hsc" #-}
{-# OPTIONS -cpp -fffi #-}
{-# LINE 2 "WinCBindings.hsc" #-}

module WinCBindings where


{-# LINE 6 "WinCBindings.hsc" #-}

import Foreign
import System.Win32.File
import System.Win32.Types


{-# LINE 12 "WinCBindings.hsc" #-}

type LPPROCESS_INFORMATION = Ptr PROCESS_INFORMATION
data PROCESS_INFORMATION = PROCESS_INFORMATION
    { piProcess :: HANDLE
    , piThread :: HANDLE
    , piProcessId :: DWORD
    , piThreadId :: DWORD
    } deriving Show

instance Storable PROCESS_INFORMATION where
    sizeOf = const (16)
{-# LINE 23 "WinCBindings.hsc" #-}
    alignment = sizeOf
    poke buf pi = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))    buf (piProcess   pi)
{-# LINE 26 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4))     buf (piThread    pi)
{-# LINE 27 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) buf (piProcessId pi)
{-# LINE 28 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 12))  buf (piThreadId  pi)
{-# LINE 29 "WinCBindings.hsc" #-}

    peek buf = do
        vhProcess    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))    buf
{-# LINE 32 "WinCBindings.hsc" #-}
        vhThread     <- ((\hsc_ptr -> peekByteOff hsc_ptr 4))     buf
{-# LINE 33 "WinCBindings.hsc" #-}
        vdwProcessId <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) buf
{-# LINE 34 "WinCBindings.hsc" #-}
        vdwThreadId  <- ((\hsc_ptr -> peekByteOff hsc_ptr 12))  buf
{-# LINE 35 "WinCBindings.hsc" #-}
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
    sizeOf = const (68)
{-# LINE 62 "WinCBindings.hsc" #-}
    alignment = sizeOf
    poke buf si = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))              buf (siCb si)
{-# LINE 65 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8))       buf (siDesktop si)
{-# LINE 66 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 12))         buf (siTitle si)
{-# LINE 67 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16))             buf (siX si)
{-# LINE 68 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 20))             buf (siY si)
{-# LINE 69 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 24))         buf (siXSize si)
{-# LINE 70 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 28))         buf (siYSize si)
{-# LINE 71 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 32))   buf (siXCountChars si)
{-# LINE 72 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 36))   buf (siYCountChars si)
{-# LINE 73 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 40)) buf (siFillAttribute si)
{-# LINE 74 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 44))         buf (siFlags si)
{-# LINE 75 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 48))     buf (siShowWindow si)
{-# LINE 76 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 56))       buf (siStdInput si)
{-# LINE 77 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 60))      buf (siStdOutput si)
{-# LINE 78 "WinCBindings.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 64))       buf (siStdError si)
{-# LINE 79 "WinCBindings.hsc" #-}

    peek buf = do
        vcb              <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))              buf
{-# LINE 82 "WinCBindings.hsc" #-}
        vlpDesktop       <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))       buf
{-# LINE 83 "WinCBindings.hsc" #-}
        vlpTitle         <- ((\hsc_ptr -> peekByteOff hsc_ptr 12))         buf
{-# LINE 84 "WinCBindings.hsc" #-}
        vdwX             <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))             buf
{-# LINE 85 "WinCBindings.hsc" #-}
        vdwY             <- ((\hsc_ptr -> peekByteOff hsc_ptr 20))             buf
{-# LINE 86 "WinCBindings.hsc" #-}
        vdwXSize         <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))         buf
{-# LINE 87 "WinCBindings.hsc" #-}
        vdwYSize         <- ((\hsc_ptr -> peekByteOff hsc_ptr 28))         buf
{-# LINE 88 "WinCBindings.hsc" #-}
        vdwXCountChars   <- ((\hsc_ptr -> peekByteOff hsc_ptr 32))   buf
{-# LINE 89 "WinCBindings.hsc" #-}
        vdwYCountChars   <- ((\hsc_ptr -> peekByteOff hsc_ptr 36))   buf
{-# LINE 90 "WinCBindings.hsc" #-}
        vdwFillAttribute <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) buf
{-# LINE 91 "WinCBindings.hsc" #-}
        vdwFlags         <- ((\hsc_ptr -> peekByteOff hsc_ptr 44))         buf
{-# LINE 92 "WinCBindings.hsc" #-}
        vwShowWindow     <- ((\hsc_ptr -> peekByteOff hsc_ptr 48))     buf
{-# LINE 93 "WinCBindings.hsc" #-}
        vhStdInput       <- ((\hsc_ptr -> peekByteOff hsc_ptr 56))       buf
{-# LINE 94 "WinCBindings.hsc" #-}
        vhStdOutput      <- ((\hsc_ptr -> peekByteOff hsc_ptr 60))      buf
{-# LINE 95 "WinCBindings.hsc" #-}
        vhStdError       <- ((\hsc_ptr -> peekByteOff hsc_ptr 64))       buf
{-# LINE 96 "WinCBindings.hsc" #-}
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
cWAIT_ABANDONED = 128
{-# LINE 118 "WinCBindings.hsc" #-}

cWAIT_OBJECT_0 :: DWORD
cWAIT_OBJECT_0 = 0
{-# LINE 121 "WinCBindings.hsc" #-}

cWAIT_TIMEOUT :: DWORD
cWAIT_TIMEOUT = 258
{-# LINE 124 "WinCBindings.hsc" #-}

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


{-# LINE 144 "WinCBindings.hsc" #-}

