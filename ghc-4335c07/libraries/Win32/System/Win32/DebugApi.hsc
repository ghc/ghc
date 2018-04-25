#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.DebugApi
-- Copyright   :  (c) Esa Ilari Vuokko, 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for using Windows DebugApi.
--
-----------------------------------------------------------------------------
module System.Win32.DebugApi where

import Control.Exception( bracket_ )
import Data.Word        ( Word8, Word32 )
import Foreign          ( Ptr, nullPtr, ForeignPtr, mallocForeignPtrBytes
                        , peekByteOff, plusPtr, allocaBytes, castPtr, poke
                        , withForeignPtr, Storable, sizeOf, peek, pokeByteOff )
import System.IO        ( fixIO )
import System.Win32.Types   ( HANDLE, BOOL, WORD, DWORD, failIf_, failWith
                            , getLastError, failIf, LPTSTR, withTString )

##include "windows_cconv.h"
#include "windows.h"

type PID = DWORD
type TID = DWORD
type DebugEventId = (PID, TID)
type ForeignAddress = Word32

type PHANDLE = Ptr ()
type THANDLE = Ptr ()

type ThreadInfo = (THANDLE, ForeignAddress, ForeignAddress)   -- handle to thread, thread local, thread start
type ImageInfo = (HANDLE, ForeignAddress, DWORD, DWORD, ForeignAddress)
type ExceptionInfo = (Bool, Bool, ForeignAddress) -- First chance, continuable, address


data Exception
    = UnknownException
    | AccessViolation Bool ForeignAddress
    | ArrayBoundsExceeded
    | Breakpoint
    | DataTypeMisalignment
    | FltDenormalOperand
    | FltDivideByZero
    | FltInexactResult
    | FltInvalidOperation
    | FltOverflow
    | FltStackCheck
    | FltUnderflow
    | IllegalInstruction
    | InPageError
    | IntDivideByZero
    | IntOverflow
    | InvalidDisposition
    | NonContinuable
    | PrivilegedInstruction
    | SingleStep
    | StackOverflow
    deriving (Show)
    
data DebugEventInfo
    = UnknownDebugEvent
    | Exception         ExceptionInfo Exception
    | CreateThread      ThreadInfo
    | CreateProcess     PHANDLE ImageInfo ThreadInfo
    | ExitThread        TID
    | ExitProcess       PID
    | LoadDll           ImageInfo
    | UnloadDll         TID
    | DebugString       ForeignAddress Bool WORD
    deriving (Show)

type DebugEvent = (DebugEventId, DebugEventInfo)

--------------------------------------------------------------------------
-- Handling debugevents

peekDebugEvent :: Ptr a -> IO DebugEvent
peekDebugEvent p = do
    code <- (#peek DEBUG_EVENT, dwDebugEventCode) p
    pid  <- (#peek DEBUG_EVENT, dwProcessId) p
    tid  <- (#peek DEBUG_EVENT, dwThreadId) p
    r <- rest (code::DWORD) (plusPtr p (#offset DEBUG_EVENT, u))
    return ((pid,tid), r)
    where
        dwZero = 0 :: DWORD
        wZero = 0 :: WORD
        
        rest (#const EXCEPTION_DEBUG_EVENT) p' = do
            chance  <- (#peek EXCEPTION_DEBUG_INFO, dwFirstChance) p'
            flags   <- (#peek EXCEPTION_RECORD, ExceptionFlags) p'
            addr    <- (#peek EXCEPTION_RECORD, ExceptionAddress) p'
            code    <- (#peek EXCEPTION_RECORD, ExceptionCode) p'
            e <- case code::DWORD of
                (#const EXCEPTION_ACCESS_VIOLATION)         -> return $ AccessViolation False 0
                (#const EXCEPTION_ARRAY_BOUNDS_EXCEEDED)    -> return ArrayBoundsExceeded
                (#const EXCEPTION_BREAKPOINT)               -> return Breakpoint
                (#const EXCEPTION_DATATYPE_MISALIGNMENT)    -> return DataTypeMisalignment
                (#const EXCEPTION_FLT_DENORMAL_OPERAND)     -> return FltDenormalOperand
                (#const EXCEPTION_FLT_DIVIDE_BY_ZERO)       -> return FltDivideByZero
                (#const EXCEPTION_FLT_INEXACT_RESULT)       -> return FltInexactResult
                (#const EXCEPTION_FLT_INVALID_OPERATION)    -> return FltInvalidOperation
                (#const EXCEPTION_FLT_OVERFLOW)             -> return FltOverflow
                (#const EXCEPTION_FLT_STACK_CHECK)          -> return FltStackCheck
                (#const EXCEPTION_FLT_UNDERFLOW)            -> return FltUnderflow
                (#const EXCEPTION_ILLEGAL_INSTRUCTION)      -> return IllegalInstruction
                (#const EXCEPTION_IN_PAGE_ERROR)            -> return InPageError
                (#const EXCEPTION_INT_DIVIDE_BY_ZERO)       -> return IntDivideByZero
                (#const EXCEPTION_INT_OVERFLOW)             -> return IntOverflow
                (#const EXCEPTION_INVALID_DISPOSITION)      -> return InvalidDisposition
                (#const EXCEPTION_NONCONTINUABLE_EXCEPTION) -> return NonContinuable
                (#const EXCEPTION_PRIV_INSTRUCTION)         -> return PrivilegedInstruction
                (#const EXCEPTION_SINGLE_STEP)              -> return SingleStep
                (#const EXCEPTION_STACK_OVERFLOW)           -> return StackOverflow
                _                                           -> return UnknownException 
            return $ Exception (chance/=dwZero, flags==dwZero, addr) e

        rest (#const CREATE_THREAD_DEBUG_EVENT) p' = do
            handle <- (#peek CREATE_THREAD_DEBUG_INFO, hThread)          p'
            local <- (#peek CREATE_THREAD_DEBUG_INFO, lpThreadLocalBase) p'
            start <- (#peek CREATE_THREAD_DEBUG_INFO, lpStartAddress)    p'
            return $ CreateThread (handle, local, start)

        rest (#const CREATE_PROCESS_DEBUG_EVENT) p' = do
            file    <- (#peek CREATE_PROCESS_DEBUG_INFO, hFile) p'
            proc    <- (#peek CREATE_PROCESS_DEBUG_INFO, hProcess) p'
            thread  <- (#peek CREATE_PROCESS_DEBUG_INFO, hThread) p'
            imgbase <- (#peek CREATE_PROCESS_DEBUG_INFO, lpBaseOfImage) p'
            dbgoff  <- (#peek CREATE_PROCESS_DEBUG_INFO, dwDebugInfoFileOffset) p'
            dbgsize <- (#peek CREATE_PROCESS_DEBUG_INFO, nDebugInfoSize) p'
            local   <- (#peek CREATE_PROCESS_DEBUG_INFO, lpThreadLocalBase) p'
            start   <- (#peek CREATE_PROCESS_DEBUG_INFO, lpStartAddress) p'
            imgname <- (#peek CREATE_PROCESS_DEBUG_INFO, lpImageName) p'
            --unicode <- (#peek CREATE_PROCESS_DEBUG_INFO, fUnicode) p'
            return $ CreateProcess proc 
                        (file, imgbase, dbgoff, dbgsize, imgname) --, unicode/=wZero)
                        (thread, local, start)
        
        rest (#const EXIT_THREAD_DEBUG_EVENT) p' =
            (#peek EXIT_THREAD_DEBUG_INFO, dwExitCode) p' >>= return.ExitThread
        
        rest (#const EXIT_PROCESS_DEBUG_EVENT) p' =
            (#peek EXIT_PROCESS_DEBUG_INFO, dwExitCode) p' >>= return.ExitProcess
        
        rest (#const LOAD_DLL_DEBUG_EVENT) p' = do
            file    <- (#peek LOAD_DLL_DEBUG_INFO, hFile) p'
            imgbase <- (#peek LOAD_DLL_DEBUG_INFO, lpBaseOfDll) p'
            dbgoff  <- (#peek LOAD_DLL_DEBUG_INFO, dwDebugInfoFileOffset) p'
            dbgsize <- (#peek LOAD_DLL_DEBUG_INFO, nDebugInfoSize) p'
            imgname <- (#peek LOAD_DLL_DEBUG_INFO, lpImageName) p'
            --unicode <- (#peek LOAD_DLL_DEBUG_INFO, fUnicode) p'
            return $ 
                LoadDll (file, imgbase, dbgoff, dbgsize, imgname)--, unicode/=wZero)

        rest (#const OUTPUT_DEBUG_STRING_EVENT) p' = do
            dat     <- (#peek OUTPUT_DEBUG_STRING_INFO, lpDebugStringData) p'
            unicode <- (#peek OUTPUT_DEBUG_STRING_INFO, fUnicode) p'
            len     <- (#peek OUTPUT_DEBUG_STRING_INFO, nDebugStringLength) p'
            return $ DebugString dat (unicode/=wZero) len
        
        rest (#const UNLOAD_DLL_DEBUG_EVENT) p' =
            (#peek UNLOAD_DLL_DEBUG_INFO, lpBaseOfDll) p' >>= return.UnloadDll

        rest _ _ = return UnknownDebugEvent



waitForDebugEvent :: Maybe Int -> IO (Maybe DebugEvent)
waitForDebugEvent timeout = allocaBytes (#size DEBUG_EVENT) $ \buf -> do
    res <- c_WaitForDebugEvent buf $ maybe (#const INFINITE) fromIntegral timeout
    if res
        then peekDebugEvent buf >>= return.Just
        else getLastError >>= \e -> case e of
            (#const ERROR_INVALID_HANDLE)   -> return Nothing
            (#const ERROR_SEM_TIMEOUT)      -> return Nothing
            _                               -> die e
    where
        die res = failWith "WaitForDebugEvent" res

getDebugEvents :: Int -> IO [DebugEvent]
getDebugEvents timeout = waitForDebugEvent (Just timeout) >>= getMore
    where
        getMore e = case e of
            Nothing -> return []
            Just e'  -> do
                rest <- waitForDebugEvent (Just 0) >>= getMore
                return $ e':rest

continueDebugEvent :: DebugEventId -> Bool -> IO ()
continueDebugEvent (pid,tid) cont =
    failIf_ not "ContinueDebugEvent" $ c_ContinueDebugEvent pid tid cont'
    where
        cont' = if cont
            then (#const DBG_CONTINUE)
            else (#const DBG_EXCEPTION_NOT_HANDLED)

--------------------------------------------------------------------------
-- Process control

debugActiveProcess :: PID -> IO ()
debugActiveProcess pid =
    failIf_ not "debugActiveProcess: DebugActiveProcess" $
        c_DebugActiveProcess pid

-- Windows XP
-- debugActiveProcessStop :: PID -> IO ()
-- debugActiveProcessStop pid =
--     failIf_ not "debugActiveProcessStop: DebugActiveProcessStop" $
--         c_DebugActiveProcessStop pid

--------------------------------------------------------------------------
-- Process memory

peekProcessMemory :: PHANDLE -> ForeignAddress -> Int -> Ptr a -> IO ()
peekProcessMemory proc addr size buf =
    failIf_ not "peekProcessMemory: ReadProcessMemory" $
        c_ReadProcessMemory proc (plusPtr nullPtr $ fromIntegral addr) (castPtr buf) (fromIntegral size) nullPtr

readProcessMemory :: PHANDLE -> ForeignAddress -> Int -> IO (ForeignPtr a)
readProcessMemory proc addr size = do
    res <- mallocForeignPtrBytes size
    withForeignPtr res $ peekProcessMemory proc addr size
    return res

pokeProcessMemory :: PHANDLE -> ForeignAddress -> Int -> Ptr a -> IO ()
pokeProcessMemory proc addr size buf =
    failIf_ not "pokeProcessMemory: WriteProcessMemory" $
        c_WriteProcessMemory proc (plusPtr nullPtr $ fromIntegral addr) (castPtr buf) (fromIntegral size) nullPtr

withProcessMemory :: PHANDLE -> ForeignAddress -> Int -> (Ptr a -> IO b) -> IO b
withProcessMemory proc addr size act = allocaBytes size $ \buf -> do
    peekProcessMemory proc addr size buf
    res <- act buf
    pokeProcessMemory proc addr size buf
    return res

peekP :: (Storable a) => PHANDLE -> ForeignAddress -> IO a
peekP proc addr = fixIO $ \res -> withProcessMemory proc addr (sizeOf res) peek

pokeP :: (Storable a) => PHANDLE -> ForeignAddress -> a -> IO ()
pokeP proc addr v = withProcessMemory proc addr (sizeOf v) $ \buf -> poke buf v

--------------------------------------------------------------------------
-- Thread Control

suspendThread :: THANDLE -> IO DWORD
suspendThread t =
    failIf (==0-1) "SuspendThread" $ c_SuspendThread t

resumeThread :: THANDLE -> IO DWORD
resumeThread t =
    failIf (==0-1) "ResumeThread" $ c_ResumeThread t

withSuspendedThread :: THANDLE -> IO a -> IO a
withSuspendedThread t = bracket_ (suspendThread t) (resumeThread t)

--getThreadId :: THANDLE -> IO TID
--getThreadId = failIf (==0) "GetThreadId" . c_GetThreadId

--------------------------------------------------------------------------
-- Thread register control
getThreadContext :: THANDLE -> Ptr a -> IO ()
getThreadContext t buf =
    failIf_ not "GetThreadContext" $ c_GetThreadContext t (castPtr buf)

setThreadContext :: THANDLE -> Ptr a -> IO ()
setThreadContext t buf =
    failIf_ not "SetThreadContext" $ c_SetThreadContext t (castPtr buf)

useAllRegs :: Ptr a -> IO ()
useAllRegs buf = (#poke CONTEXT, ContextFlags) buf v
    where
        v = (#const CONTEXT_FULL|CONTEXT_DEBUG_REGISTERS|CONTEXT_FLOATING_POINT) :: DWORD

withThreadContext :: THANDLE -> (Ptr a -> IO b) -> IO b
withThreadContext t act =
    allocaBytes (#size CONTEXT)
        $ \buf -> bracket_
            (useAllRegs buf >> getThreadContext t buf)
            (useAllRegs buf >> setThreadContext t buf)
            (act buf)


#if __i386__
eax, ebx, ecx, edx :: Int
esi, edi :: Int
ebp, eip, esp :: Int
eax = (#offset CONTEXT, Eax)
ebx = (#offset CONTEXT, Ebx)
ecx = (#offset CONTEXT, Ecx)
edx = (#offset CONTEXT, Edx)
esi = (#offset CONTEXT, Esi)
edi = (#offset CONTEXT, Edi)
ebp = (#offset CONTEXT, Ebp)
eip = (#offset CONTEXT, Eip)
esp = (#offset CONTEXT, Esp)
#elif __x86_64__
rax, rbx, rcx, rdx :: Int
rsi, rdi :: Int
rbp, rip, rsp :: Int
rax = (#offset CONTEXT, Rax)
rbx = (#offset CONTEXT, Rbx)
rcx = (#offset CONTEXT, Rcx)
rdx = (#offset CONTEXT, Rdx)
rsi = (#offset CONTEXT, Rsi)
rdi = (#offset CONTEXT, Rdi)
rbp = (#offset CONTEXT, Rbp)
rip = (#offset CONTEXT, Rip)
rsp = (#offset CONTEXT, Rsp)
#else
#error Unsupported architecture
#endif

segCs, segDs, segEs, segFs, segGs :: Int
segCs = (#offset CONTEXT, SegCs)
segDs = (#offset CONTEXT, SegDs)
segEs = (#offset CONTEXT, SegEs)
segFs = (#offset CONTEXT, SegFs)
segGs = (#offset CONTEXT, SegGs)

eFlags :: Int
eFlags  = (#offset CONTEXT, EFlags)

dr :: Int -> Int
dr n = case n of
    0 -> (#offset CONTEXT, Dr0)
    1 -> (#offset CONTEXT, Dr1)
    2 -> (#offset CONTEXT, Dr2)
    3 -> (#offset CONTEXT, Dr3)
    6 -> (#offset CONTEXT, Dr6)
    7 -> (#offset CONTEXT, Dr7)
    _ -> undefined
    
setReg :: Ptr a -> Int -> DWORD -> IO ()
setReg = pokeByteOff

getReg :: Ptr a -> Int -> IO DWORD
getReg = peekByteOff

modReg :: Ptr a -> Int -> (DWORD->DWORD) -> IO DWORD
modReg buf r f = do
    old <- getReg buf r
    setReg buf r (f old)
    return old

makeModThreadContext :: [(Int, DWORD->DWORD)] -> Ptr a -> IO [DWORD]
makeModThreadContext act buf = mapM (uncurry $ modReg buf) act

modifyThreadContext :: THANDLE -> [(Int, DWORD->DWORD)] -> IO [DWORD]
modifyThreadContext t a = withThreadContext t $ makeModThreadContext a

--------------------------------------------------------------------------
-- On process being debugged

outputDebugString :: String -> IO ()
outputDebugString s = withTString s $ \c_s -> c_OutputDebugString c_s

--------------------------------------------------------------------------
-- Raw imports

foreign import WINDOWS_CCONV "windows.h SuspendThread"
    c_SuspendThread :: THANDLE -> IO DWORD

foreign import WINDOWS_CCONV "windows.h ResumeThread"
    c_ResumeThread :: THANDLE -> IO DWORD

foreign import WINDOWS_CCONV "windows.h WaitForDebugEvent"
    c_WaitForDebugEvent :: Ptr () -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h ContinueDebugEvent"
    c_ContinueDebugEvent :: DWORD -> DWORD -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h DebugActiveProcess"
    c_DebugActiveProcess :: DWORD -> IO Bool
    
-- Windows XP
-- foreign import WINDOWS_CCONV "windows.h DebugActiveProcessStop"
--     c_DebugActiveProcessStop :: DWORD -> IO Bool

foreign import WINDOWS_CCONV "windows.h ReadProcessMemory" c_ReadProcessMemory :: 
    PHANDLE -> Ptr () -> Ptr Word8 -> DWORD -> Ptr DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h WriteProcessMemory" c_WriteProcessMemory ::
    PHANDLE -> Ptr () -> Ptr Word8 -> DWORD -> Ptr DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h GetThreadContext"
    c_GetThreadContext :: THANDLE -> Ptr () -> IO BOOL

foreign import WINDOWS_CCONV "windows.h SetThreadContext"
    c_SetThreadContext :: THANDLE -> Ptr () -> IO BOOL

--foreign import WINDOWS_CCONV "windows.h GetThreadId"
--    c_GetThreadId :: THANDLE -> IO TID

foreign import WINDOWS_CCONV "windows.h OutputDebugStringW"
    c_OutputDebugString :: LPTSTR -> IO ()

foreign import WINDOWS_CCONV "windows.h IsDebuggerPresent"
    isDebuggerPresent :: IO BOOL

foreign import WINDOWS_CCONV "windows.h  DebugBreak"
    debugBreak :: IO ()
