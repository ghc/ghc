{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module WinCBindings where

#if defined(mingw32_HOST_OS)

import Foreign
import Foreign.C.Types
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
    alignment = const #alignment PROCESS_INFORMATION
    poke buf pinfo = do
        (#poke PROCESS_INFORMATION, hProcess)    buf (piProcess   pinfo)
        (#poke PROCESS_INFORMATION, hThread)     buf (piThread    pinfo)
        (#poke PROCESS_INFORMATION, dwProcessId) buf (piProcessId pinfo)
        (#poke PROCESS_INFORMATION, dwThreadId)  buf (piThreadId  pinfo)

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
    alignment = const #alignment STARTUPINFO
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

data JOBOBJECT_EXTENDED_LIMIT_INFORMATION = JOBOBJECT_EXTENDED_LIMIT_INFORMATION
    { jeliBasicLimitInformation :: JOBOBJECT_BASIC_LIMIT_INFORMATION
    , jeliIoInfo                :: IO_COUNTERS
    , jeliProcessMemoryLimit    :: SIZE_T
    , jeliJobMemoryLimit        :: SIZE_T
    , jeliPeakProcessMemoryUsed :: SIZE_T
    , jeliPeakJobMemoryUsed     :: SIZE_T
    } deriving Show

instance Storable JOBOBJECT_EXTENDED_LIMIT_INFORMATION where
    sizeOf = const #size JOBOBJECT_EXTENDED_LIMIT_INFORMATION
    alignment = const #alignment JOBOBJECT_EXTENDED_LIMIT_INFORMATION
    poke buf jeli = do
        (#poke JOBOBJECT_EXTENDED_LIMIT_INFORMATION, BasicLimitInformation) buf (jeliBasicLimitInformation jeli)
        (#poke JOBOBJECT_EXTENDED_LIMIT_INFORMATION, IoInfo)                buf (jeliIoInfo jeli)
        (#poke JOBOBJECT_EXTENDED_LIMIT_INFORMATION, ProcessMemoryLimit)    buf (jeliProcessMemoryLimit jeli)
        (#poke JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobMemoryLimit)        buf (jeliJobMemoryLimit jeli)
        (#poke JOBOBJECT_EXTENDED_LIMIT_INFORMATION, PeakProcessMemoryUsed) buf (jeliPeakProcessMemoryUsed jeli)
        (#poke JOBOBJECT_EXTENDED_LIMIT_INFORMATION, PeakJobMemoryUsed)     buf (jeliPeakJobMemoryUsed jeli)
    peek buf = do
        vBasicLimitInformation <- (#peek JOBOBJECT_EXTENDED_LIMIT_INFORMATION, BasicLimitInformation) buf
        vIoInfo                <- (#peek JOBOBJECT_EXTENDED_LIMIT_INFORMATION, IoInfo)                buf
        vProcessMemoryLimit    <- (#peek JOBOBJECT_EXTENDED_LIMIT_INFORMATION, ProcessMemoryLimit)    buf
        vJobMemoryLimit        <- (#peek JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobMemoryLimit)        buf
        vPeakProcessMemoryUsed <- (#peek JOBOBJECT_EXTENDED_LIMIT_INFORMATION, PeakProcessMemoryUsed) buf
        vPeakJobMemoryUsed     <- (#peek JOBOBJECT_EXTENDED_LIMIT_INFORMATION, PeakJobMemoryUsed)     buf
        return $ JOBOBJECT_EXTENDED_LIMIT_INFORMATION {
            jeliBasicLimitInformation = vBasicLimitInformation,
            jeliIoInfo                = vIoInfo,
            jeliProcessMemoryLimit    = vProcessMemoryLimit,
            jeliJobMemoryLimit        = vJobMemoryLimit,
            jeliPeakProcessMemoryUsed = vPeakProcessMemoryUsed,
            jeliPeakJobMemoryUsed     = vPeakJobMemoryUsed}

type ULONGLONG = #type ULONGLONG

data IO_COUNTERS = IO_COUNTERS
    { icReadOperationCount  :: ULONGLONG
    , icWriteOperationCount :: ULONGLONG
    , icOtherOperationCount :: ULONGLONG
    , icReadTransferCount   :: ULONGLONG
    , icWriteTransferCount  :: ULONGLONG
    , icOtherTransferCount  :: ULONGLONG
    } deriving Show

instance Storable IO_COUNTERS where
    sizeOf = const #size IO_COUNTERS
    alignment = const #alignment IO_COUNTERS
    poke buf ic = do
        (#poke IO_COUNTERS, ReadOperationCount)  buf (icReadOperationCount ic)
        (#poke IO_COUNTERS, WriteOperationCount) buf (icWriteOperationCount ic)
        (#poke IO_COUNTERS, OtherOperationCount) buf (icOtherOperationCount ic)
        (#poke IO_COUNTERS, ReadTransferCount)   buf (icReadTransferCount ic)
        (#poke IO_COUNTERS, WriteTransferCount)  buf (icWriteTransferCount ic)
        (#poke IO_COUNTERS, OtherTransferCount)  buf (icOtherTransferCount ic)
    peek buf = do
        vReadOperationCount  <- (#peek IO_COUNTERS, ReadOperationCount)  buf
        vWriteOperationCount <- (#peek IO_COUNTERS, WriteOperationCount) buf
        vOtherOperationCount <- (#peek IO_COUNTERS, OtherOperationCount) buf
        vReadTransferCount   <- (#peek IO_COUNTERS, ReadTransferCount)   buf
        vWriteTransferCount  <- (#peek IO_COUNTERS, WriteTransferCount)  buf
        vOtherTransferCount  <- (#peek IO_COUNTERS, OtherTransferCount)  buf
        return $ IO_COUNTERS {
            icReadOperationCount  = vReadOperationCount,
            icWriteOperationCount = vWriteOperationCount,
            icOtherOperationCount = vOtherOperationCount,
            icReadTransferCount   = vReadTransferCount,
            icWriteTransferCount  = vWriteTransferCount,
            icOtherTransferCount  = vOtherTransferCount}

data JOBOBJECT_BASIC_LIMIT_INFORMATION = JOBOBJECT_BASIC_LIMIT_INFORMATION
    { jbliPerProcessUserTimeLimit :: LARGE_INTEGER
    , jbliPerJobUserTimeLimit     :: LARGE_INTEGER
    , jbliLimitFlags              :: DWORD
    , jbliMinimumWorkingSetSize   :: SIZE_T
    , jbliMaximumWorkingSetSize   :: SIZE_T
    , jbliActiveProcessLimit      :: DWORD
    , jbliAffinity                :: ULONG_PTR
    , jbliPriorityClass           :: DWORD
    , jbliSchedulingClass         :: DWORD
    } deriving Show

instance Storable JOBOBJECT_BASIC_LIMIT_INFORMATION where
    sizeOf = const #size JOBOBJECT_BASIC_LIMIT_INFORMATION
    alignment = const #alignment JOBOBJECT_BASIC_LIMIT_INFORMATION
    poke buf jbli = do
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, PerProcessUserTimeLimit) buf (jbliPerProcessUserTimeLimit jbli)
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, PerJobUserTimeLimit)     buf (jbliPerJobUserTimeLimit jbli)
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, LimitFlags)              buf (jbliLimitFlags jbli)
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, MinimumWorkingSetSize)   buf (jbliMinimumWorkingSetSize jbli)
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, MaximumWorkingSetSize)   buf (jbliMaximumWorkingSetSize jbli)
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, ActiveProcessLimit)      buf (jbliActiveProcessLimit jbli)
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, Affinity)                buf (jbliAffinity jbli)
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, PriorityClass)           buf (jbliPriorityClass jbli)
        (#poke JOBOBJECT_BASIC_LIMIT_INFORMATION, SchedulingClass)         buf (jbliSchedulingClass jbli)
    peek buf = do
        vPerProcessUserTimeLimit <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, PerProcessUserTimeLimit) buf
        vPerJobUserTimeLimit     <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, PerJobUserTimeLimit)     buf
        vLimitFlags              <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, LimitFlags)              buf
        vMinimumWorkingSetSize   <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, MinimumWorkingSetSize)   buf
        vMaximumWorkingSetSize   <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, MaximumWorkingSetSize)   buf
        vActiveProcessLimit      <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, ActiveProcessLimit)      buf
        vAffinity                <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, Affinity)                buf
        vPriorityClass           <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, PriorityClass)           buf
        vSchedulingClass         <- (#peek JOBOBJECT_BASIC_LIMIT_INFORMATION, SchedulingClass)         buf
        return $ JOBOBJECT_BASIC_LIMIT_INFORMATION {
            jbliPerProcessUserTimeLimit = vPerProcessUserTimeLimit,
            jbliPerJobUserTimeLimit     = vPerJobUserTimeLimit,
            jbliLimitFlags              = vLimitFlags,
            jbliMinimumWorkingSetSize   = vMinimumWorkingSetSize,
            jbliMaximumWorkingSetSize   = vMaximumWorkingSetSize,
            jbliActiveProcessLimit      = vActiveProcessLimit,
            jbliAffinity                = vAffinity,
            jbliPriorityClass           = vPriorityClass,
            jbliSchedulingClass         = vSchedulingClass}

data JOBOBJECT_ASSOCIATE_COMPLETION_PORT = JOBOBJECT_ASSOCIATE_COMPLETION_PORT
    { jacpCompletionKey  :: PVOID
    , jacpCompletionPort :: HANDLE
    } deriving Show

instance Storable JOBOBJECT_ASSOCIATE_COMPLETION_PORT where
    sizeOf = const #size JOBOBJECT_ASSOCIATE_COMPLETION_PORT
    alignment = const #alignment JOBOBJECT_ASSOCIATE_COMPLETION_PORT
    poke buf jacp = do
        (#poke JOBOBJECT_ASSOCIATE_COMPLETION_PORT, CompletionKey)  buf (jacpCompletionKey jacp)
        (#poke JOBOBJECT_ASSOCIATE_COMPLETION_PORT, CompletionPort) buf (jacpCompletionPort jacp)
    peek buf = do
        vCompletionKey  <- (#peek JOBOBJECT_ASSOCIATE_COMPLETION_PORT, CompletionKey)  buf
        vCompletionPort <- (#peek JOBOBJECT_ASSOCIATE_COMPLETION_PORT, CompletionPort) buf
        return $ JOBOBJECT_ASSOCIATE_COMPLETION_PORT {
            jacpCompletionKey  = vCompletionKey,
            jacpCompletionPort = vCompletionPort}


foreign import ccall unsafe "windows.h WaitForSingleObject"
    waitForSingleObject :: HANDLE -> DWORD -> IO DWORD

type JOBOBJECTINFOCLASS = CInt

type PVOID = Ptr ()
type PULONG_PTR = Ptr ULONG_PTR

jobObjectExtendedLimitInformation :: JOBOBJECTINFOCLASS
jobObjectExtendedLimitInformation = #const JobObjectExtendedLimitInformation

jobObjectAssociateCompletionPortInformation :: JOBOBJECTINFOCLASS
jobObjectAssociateCompletionPortInformation = #const JobObjectAssociateCompletionPortInformation

cJOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE :: DWORD
cJOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE = #const JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE

cJOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO :: DWORD
cJOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO = #const JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO

cJOB_OBJECT_MSG_EXIT_PROCESS :: DWORD
cJOB_OBJECT_MSG_EXIT_PROCESS = #const JOB_OBJECT_MSG_EXIT_PROCESS

cJOB_OBJECT_MSG_NEW_PROCESS :: DWORD
cJOB_OBJECT_MSG_NEW_PROCESS = #const JOB_OBJECT_MSG_NEW_PROCESS

cWAIT_ABANDONED :: DWORD
cWAIT_ABANDONED = #const WAIT_ABANDONED

cWAIT_OBJECT_0 :: DWORD
cWAIT_OBJECT_0 = #const WAIT_OBJECT_0

cWAIT_TIMEOUT :: DWORD
cWAIT_TIMEOUT = #const WAIT_TIMEOUT

cCREATE_SUSPENDED :: DWORD
cCREATE_SUSPENDED = #const CREATE_SUSPENDED

cHANDLE_FLAG_INHERIT :: DWORD
cHANDLE_FLAG_INHERIT = #const HANDLE_FLAG_INHERIT

foreign import ccall unsafe "windows.h GetExitCodeProcess"
    getExitCodeProcess :: HANDLE -> LPDWORD -> IO BOOL

foreign import ccall unsafe "windows.h CloseHandle"
    closeHandle :: HANDLE -> IO BOOL

foreign import ccall unsafe "windows.h TerminateJobObject"
    terminateJobObject :: HANDLE -> UINT -> IO BOOL

foreign import ccall unsafe "windows.h AssignProcessToJobObject"
    assignProcessToJobObject :: HANDLE -> HANDLE -> IO BOOL

foreign import ccall unsafe "windows.h CreateJobObjectW"
    createJobObjectW :: LPSECURITY_ATTRIBUTES -> LPCTSTR -> IO HANDLE

foreign import ccall unsafe "windows.h CreateProcessW"
    createProcessW :: LPCTSTR -> LPTSTR
                   -> LPSECURITY_ATTRIBUTES -> LPSECURITY_ATTRIBUTES
                   -> BOOL -> DWORD -> LPVOID -> LPCTSTR -> LPSTARTUPINFO
                   -> LPPROCESS_INFORMATION -> IO BOOL

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)

foreign import ccall unsafe "windows.h SetInformationJobObject"
    setInformationJobObject :: HANDLE -> JOBOBJECTINFOCLASS -> LPVOID -> DWORD -> IO BOOL

foreign import ccall unsafe "windows.h CreateIoCompletionPort"
    createIoCompletionPort :: HANDLE -> HANDLE -> ULONG_PTR -> DWORD -> IO HANDLE

foreign import ccall unsafe "windows.h GetQueuedCompletionStatus"
    getQueuedCompletionStatus :: HANDLE -> LPDWORD -> PULONG_PTR -> Ptr LPOVERLAPPED -> DWORD -> IO BOOL

foreign import ccall unsafe "windows.h SetHandleInformation"
    setHandleInformation :: HANDLE -> DWORD -> DWORD -> IO BOOL

setJobParameters :: HANDLE -> IO BOOL
setJobParameters hJob = alloca $ \p_jeli -> do
    let jeliSize = sizeOf (undefined :: JOBOBJECT_EXTENDED_LIMIT_INFORMATION)

    _ <- memset p_jeli 0 $ fromIntegral jeliSize
    -- Configure all child processes associated with the job to terminate when the
    -- last handle to the job is closed. This prevent half dead processes and that
    -- hanging ghc-iserv.exe process that happens when you interrupt the testsuite.
    (#poke JOBOBJECT_EXTENDED_LIMIT_INFORMATION, BasicLimitInformation.LimitFlags)
      p_jeli cJOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
    setInformationJobObject hJob jobObjectExtendedLimitInformation
                            p_jeli (fromIntegral jeliSize)

createCompletionPort :: HANDLE -> IO HANDLE
createCompletionPort hJob = do
    ioPort <- createIoCompletionPort iNVALID_HANDLE_VALUE nullPtr 0 1
    if ioPort == nullPtr
       then do err_code <- getLastError
               putStrLn $ "CreateIoCompletionPort error: " ++ show err_code
               return nullPtr
       else with (JOBOBJECT_ASSOCIATE_COMPLETION_PORT {
                    jacpCompletionKey  = hJob,
                    jacpCompletionPort = ioPort}) $ \p_Port -> do
              res <- setInformationJobObject hJob jobObjectAssociateCompletionPortInformation
                         (castPtr p_Port) (fromIntegral (sizeOf (undefined :: JOBOBJECT_ASSOCIATE_COMPLETION_PORT)))
              if res
                 then return ioPort
                 else do err_code <- getLastError
                         putStrLn $ "SetInformation, error: " ++ show err_code
                         return nullPtr

waitForJobCompletion :: HANDLE -> HANDLE -> DWORD -> IO BOOL
waitForJobCompletion _hJob ioPort timeout
  = alloca $ \p_CompletionCode ->
    alloca $ \p_CompletionKey ->
    alloca $ \p_Overlapped -> do

    -- getQueuedCompletionStatus is a blocking call,
    -- it will wake up for each completion event. So if it's
    -- not the one we want, sleep again.
    let loop :: IO ()
        loop = do
          res <- getQueuedCompletionStatus ioPort p_CompletionCode p_CompletionKey
                                           p_Overlapped timeout
          case res of
            False -> return ()
            True  -> do
                completionCode <- peek p_CompletionCode
                if completionCode == cJOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO
                           then return ()
                   else if completionCode == cJOB_OBJECT_MSG_EXIT_PROCESS
                           then loop -- Debug point, do nothing for now
                   else if completionCode == cJOB_OBJECT_MSG_NEW_PROCESS
                           then loop -- Debug point, do nothing for now
                           else loop

    loop -- Kick it all off

    overlapped <- peek p_Overlapped
    code       <- peek $ p_CompletionCode

    return $ if overlapped == nullPtr && code /= cJOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO
                then False -- Timeout occurred. *dark voice* YOU HAVE FAILED THIS TEST!.
                else True
#endif
