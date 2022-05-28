{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
-- Whether there are identities depends on the platform
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Windows.Handle
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Raw read/write operations on Windows Handles
--
-----------------------------------------------------------------------------

module GHC.IO.Windows.Handle
 ( -- * Basic Types
   NativeHandle(),
   ConsoleHandle(),
   IoHandle(),
   HANDLE,
   Io(),

   -- * Utility functions
   convertHandle,
   toHANDLE,
   fromHANDLE,
   handleToMode,
   isAsynchronous,
   optimizeFileAccess,

   -- * Standard Handles
   stdin,
   stdout,
   stderr,

   -- * File utilities
   openFile,
   openFileAsTemp,
   release
 ) where

#include <windows.h>
#include <ntstatus.h>
#include <winnt.h>
##include "windows_cconv.h"

-- Can't avoid these semantics leaks, they are base constructs
import Data.Bits ((.|.), (.&.), shiftL)
import Data.Functor ((<$>))
import Data.Typeable

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.List
import GHC.Word (Word8, Word16, Word64)

import GHC.IO hiding (mask)
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import qualified GHC.IO.Device
import GHC.IO.Device (SeekMode(..), IODeviceType(..), IODevice(), devType, setSize)
import GHC.IO.Exception
import GHC.IO.IOMode
import GHC.IO.Windows.Encoding (withGhcInternalToUTF16, withUTF16ToGhcInternal)
import GHC.IO.Windows.Paths (getDevicePath)
import GHC.IO.Handle.Internals (debugIO)
import GHC.IORef
import GHC.Event.Windows (LPOVERLAPPED, withOverlappedEx, IOResult(..))
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with, fromBool)
import Foreign.Storable (Storable (..))
import qualified GHC.Event.Windows as Mgr

import GHC.Windows (LPVOID, LPDWORD, DWORD, HANDLE, BOOL, LPCTSTR, ULONG, WORD,
                    UCHAR, failIf, iNVALID_HANDLE_VALUE, failWith,
                    failIfFalse_, getLastError)
import Text.Show

-- -----------------------------------------------------------------------------
-- The Windows IO device handles

data NativeHandle
data ConsoleHandle

-- | Bit of a Hack, but we don't want every handle to have a cooked entry
--   but all copies of the handles for which we do want one need to share
--   the same value.
--   We can't store it separately because we don't know when the handle will
--   be destroyed or invalidated.
data IoHandle a where
  NativeHandle  :: { getNativeHandle  :: HANDLE
                   -- In certain cases we have inherited a handle and the
                   -- handle and it may not have been created for async
                   -- access.  In those case we can't issue a completion
                   -- request as it would never finish and we'd deadlock.
                   , isAsynchronous :: Bool } -> IoHandle NativeHandle
  ConsoleHandle :: { getConsoleHandle :: HANDLE
                   , cookedHandle :: IORef Bool
                   } -> IoHandle ConsoleHandle

type Io a = IoHandle a

-- | Convert a ConsoleHandle into a general FileHandle
--   This will change which DeviceIO is used.
convertHandle :: Io ConsoleHandle -> Bool -> Io NativeHandle
convertHandle io async
  = let !hwnd = getConsoleHandle io
    in NativeHandle hwnd async

-- | @since 4.11.0.0
instance Show (Io NativeHandle) where
  show = show . toHANDLE

-- | @since 4.11.0.0
instance Show (Io ConsoleHandle) where
  show = show . getConsoleHandle

-- | @since 4.11.0.0
instance GHC.IO.Device.RawIO (Io NativeHandle) where
  read             = hwndRead
  readNonBlocking  = hwndReadNonBlocking
  write            = hwndWrite
  writeNonBlocking = hwndWriteNonBlocking

-- | @since 4.11.0.0
instance GHC.IO.Device.RawIO (Io ConsoleHandle) where
  read             = consoleRead True
  readNonBlocking  = consoleReadNonBlocking
  write            = consoleWrite
  writeNonBlocking = consoleWriteNonBlocking

-- | Generalize a way to get and create handles.
class (GHC.IO.Device.RawIO a, IODevice a, BufferedIO a, Typeable a)
      => RawHandle a where
  toHANDLE   :: a -> HANDLE
  fromHANDLE :: HANDLE -> a
  isLockable :: a -> Bool
  setCooked  :: a -> Bool -> IO a
  isCooked   :: a -> IO Bool

instance RawHandle (Io NativeHandle) where
  toHANDLE     = getNativeHandle
  -- In order to convert to a native handle we have to check to see
  -- is the handle can be used async or not.
  fromHANDLE   = flip NativeHandle True
  isLockable _ = True
  setCooked    = const . return
  isCooked   _ = return False

instance RawHandle (Io ConsoleHandle) where
  toHANDLE         = getConsoleHandle
  fromHANDLE h     = unsafePerformIO $ ConsoleHandle h <$> newIORef False
  isLockable _     = False
  setCooked  h val =
    do writeIORef (cookedHandle h) val
       return h
  isCooked   h     = readIORef (cookedHandle h)

-- -----------------------------------------------------------------------------
-- The Windows IO device implementation

-- | @since 4.11.0.0
instance GHC.IO.Device.IODevice (Io NativeHandle) where
  ready      = handle_ready
  close      = handle_close
  isTerminal = handle_is_console
  isSeekable = handle_is_seekable
  seek       = handle_seek
  tell       = handle_tell
  getSize    = handle_get_size
  setSize    = handle_set_size
  setEcho    = handle_set_echo
  getEcho    = handle_get_echo
  setRaw     = handle_set_buffering
  devType    = handle_dev_type
  dup        = handle_duplicate

-- | @since 4.11.0.0
instance GHC.IO.Device.IODevice (Io ConsoleHandle) where
  ready      = handle_ready
  close      = handle_close . flip convertHandle False
  isTerminal = handle_is_console
  isSeekable = handle_is_seekable
  seek       = handle_console_seek
  tell       = handle_console_tell
  getSize    = handle_get_console_size
  setSize    = handle_set_console_size
  setEcho    = handle_set_echo
  getEcho    = handle_get_echo
  setRaw     = console_set_buffering
  devType    = handle_dev_type
  dup        = handle_duplicate

-- Default sequential read buffer size.
-- for Windows 8k seems to be the optimal
-- buffer size.
dEFAULT_BUFFER_SIZE :: Int
dEFAULT_BUFFER_SIZE = 8192

-- | @since 4.11.0.0
-- See libraries/base/GHC/IO/BufferedIO.hs
instance BufferedIO (Io NativeHandle) where
  newBuffer _dev state = newByteBuffer dEFAULT_BUFFER_SIZE state
  fillReadBuffer       = readBuf'
  fillReadBuffer0      = readBufNonBlocking
  flushWriteBuffer     = writeBuf'
  flushWriteBuffer0    = writeBufNonBlocking

-- | @since 4.11.0.0
-- See libraries/base/GHC/IO/BufferedIO.hs
instance BufferedIO (Io ConsoleHandle) where
  newBuffer _dev state = newByteBuffer dEFAULT_BUFFER_SIZE state
  fillReadBuffer       = readBuf'
  fillReadBuffer0      = readBufNonBlocking
  flushWriteBuffer     = writeBuf'
  flushWriteBuffer0    = writeBufNonBlocking


readBuf' :: RawHandle a => a -> Buffer Word8 -> IO (Int, Buffer Word8)
readBuf' hnd buf = do
  debugIO ("readBuf handle=" ++ show (toHANDLE hnd) ++ " " ++
           summaryBuffer buf ++ "\n")
  (r,buf') <- readBuf hnd buf
  debugIO ("after: " ++ summaryBuffer buf' ++ "\n")
  return (r,buf')

writeBuf' :: RawHandle a => a -> Buffer Word8 -> IO (Buffer Word8)
writeBuf' hnd buf = do
  debugIO ("writeBuf handle=" ++ show (toHANDLE hnd) ++ " " ++
           summaryBuffer buf ++ "\n")
  writeBuf hnd buf

-- -----------------------------------------------------------------------------
-- Standard I/O handles

type StdHandleId  = DWORD

#{enum StdHandleId,
 , sTD_INPUT_HANDLE  = STD_INPUT_HANDLE
 , sTD_OUTPUT_HANDLE = STD_OUTPUT_HANDLE
 , sTD_ERROR_HANDLE  = STD_ERROR_HANDLE
}

getStdHandle :: StdHandleId -> IO HANDLE
getStdHandle hid =
  failIf (== iNVALID_HANDLE_VALUE) "GetStdHandle" $ c_GetStdHandle hid

stdin, stdout, stderr :: Io ConsoleHandle
stdin  = unsafePerformIO $ mkConsoleHandle =<< getStdHandle sTD_INPUT_HANDLE
stdout = unsafePerformIO $ mkConsoleHandle =<< getStdHandle sTD_OUTPUT_HANDLE
stderr = unsafePerformIO $ mkConsoleHandle =<< getStdHandle sTD_ERROR_HANDLE

mkConsoleHandle :: HANDLE -> IO (Io ConsoleHandle)
mkConsoleHandle hwnd
  = do ref <- newIORef False
       return $ ConsoleHandle hwnd ref

-- -----------------------------------------------------------------------------
-- Some console internal types to detect EOF.

-- ASCII Ctrl+D (EOT) character.  Typically used by Unix consoles.
-- use for cross platform compatibility and to adhere to the ASCII standard.
acCtrlD :: Int
acCtrlD = 0x04
-- ASCII Ctrl+Z (SUB) character. Typically used by Windows consoles to denote
-- EOT.  Use for compatibility with user expectations.
acCtrlZ :: Int
acCtrlZ = 0x1A

-- Mask to use to trigger ReadConsole input processing end.
acEotMask :: ULONG
acEotMask = (1 `shiftL` acCtrlD) .|. (1 `shiftL` acCtrlZ)

-- Structure to hold the control character masks
type PCONSOLE_READCONSOLE_CONTROL = Ptr CONSOLE_READCONSOLE_CONTROL
data CONSOLE_READCONSOLE_CONTROL = CONSOLE_READCONSOLE_CONTROL
  { crcNLength           :: ULONG
  , crcNInitialChars     :: ULONG
  , crcDwCtrlWakeupMask  :: ULONG
  , crcDwControlKeyState :: ULONG
  } deriving Show

instance Storable CONSOLE_READCONSOLE_CONTROL where
  sizeOf = const #size CONSOLE_READCONSOLE_CONTROL
  alignment = const #alignment CONSOLE_READCONSOLE_CONTROL
  poke buf crc = do
    (#poke CONSOLE_READCONSOLE_CONTROL, nLength)           buf
        (crcNLength           crc)
    (#poke CONSOLE_READCONSOLE_CONTROL, nInitialChars)     buf
        (crcNInitialChars     crc)
    (#poke CONSOLE_READCONSOLE_CONTROL, dwCtrlWakeupMask)  buf
        (crcDwCtrlWakeupMask  crc)
    (#poke CONSOLE_READCONSOLE_CONTROL, dwControlKeyState) buf
        (crcDwControlKeyState crc)

  peek buf = do
    vNLength           <-
      (#peek CONSOLE_READCONSOLE_CONTROL, nLength)           buf
    vNInitialChars     <-
      (#peek CONSOLE_READCONSOLE_CONTROL, nInitialChars)     buf
    vDwCtrlWakeupMask  <-
      (#peek CONSOLE_READCONSOLE_CONTROL, dwCtrlWakeupMask)  buf
    vDwControlKeyState <-
      (#peek CONSOLE_READCONSOLE_CONTROL, dwControlKeyState) buf
    return $ CONSOLE_READCONSOLE_CONTROL {
        crcNLength           = vNLength,
        crcNInitialChars     = vNInitialChars,
        crcDwCtrlWakeupMask  = vDwCtrlWakeupMask,
        crcDwControlKeyState = vDwControlKeyState
      }

-- Create CONSOLE_READCONSOLE_CONTROL for breaking on control characters
-- specified by acEotMask
eotControl :: CONSOLE_READCONSOLE_CONTROL
eotControl =
  CONSOLE_READCONSOLE_CONTROL
    { crcNLength           = fromIntegral $
                               sizeOf (undefined :: CONSOLE_READCONSOLE_CONTROL)
    , crcNInitialChars     = 0
    , crcDwCtrlWakeupMask  = acEotMask
    , crcDwControlKeyState = 0
    }

type PINPUT_RECORD = Ptr ()
-- -----------------------------------------------------------------------------
-- Foreign imports


foreign import WINDOWS_CCONV safe "windows.h CreateFileW"
    c_CreateFile :: LPCTSTR -> DWORD -> DWORD -> LPSECURITY_ATTRIBUTES
                 -> DWORD -> DWORD -> HANDLE
                 -> IO HANDLE

foreign import WINDOWS_CCONV safe "windows.h SetFileCompletionNotificationModes"
    c_SetFileCompletionNotificationModes :: HANDLE -> UCHAR -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h ReadFile"
    c_ReadFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
               -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h WriteFile"
    c_WriteFile :: HANDLE -> LPVOID -> DWORD -> LPDWORD -> LPOVERLAPPED
                -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h GetStdHandle"
    c_GetStdHandle :: StdHandleId -> IO HANDLE

foreign import ccall safe "__handle_ready"
    c_handle_ready :: HANDLE -> BOOL -> CInt -> IO CInt

foreign import ccall safe "__is_console"
    c_is_console :: HANDLE -> IO BOOL

foreign import ccall safe "__set_console_buffering"
    c_set_console_buffering :: HANDLE -> BOOL -> IO BOOL

foreign import ccall safe "__set_console_echo"
    c_set_console_echo :: HANDLE -> BOOL -> IO BOOL

foreign import ccall safe "__get_console_echo"
    c_get_console_echo :: HANDLE -> IO BOOL

foreign import ccall safe "__close_handle"
    c_close_handle :: HANDLE -> IO Bool

foreign import ccall safe "__handle_type"
    c_handle_type :: HANDLE -> IO Int

foreign import ccall safe "__set_file_pointer"
  c_set_file_pointer :: HANDLE -> CLong -> DWORD -> Ptr CLong -> IO BOOL

foreign import ccall safe "__get_file_pointer"
  c_get_file_pointer :: HANDLE -> IO CLong

foreign import ccall safe "__get_file_size"
  c_get_file_size :: HANDLE -> IO CLong

foreign import ccall safe "__set_file_size"
  c_set_file_size :: HANDLE -> CLong -> IO BOOL

foreign import ccall safe "__duplicate_handle"
  c_duplicate_handle :: HANDLE -> Ptr HANDLE -> IO BOOL

foreign import ccall safe "__set_console_pointer"
  c_set_console_pointer :: HANDLE -> CLong -> DWORD -> Ptr CLong -> IO BOOL

foreign import ccall safe "__get_console_pointer"
  c_get_console_pointer :: HANDLE -> IO CLong

foreign import ccall safe "__get_console_buffer_size"
  c_get_console_buffer_size :: HANDLE -> IO CLong

foreign import ccall safe "__set_console_buffer_size"
  c_set_console_buffer_size :: HANDLE -> CLong -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h ReadConsoleW"
  c_read_console :: HANDLE -> Ptr Word16 -> DWORD -> Ptr DWORD
                 -> PCONSOLE_READCONSOLE_CONTROL -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h WriteConsoleW"
  c_write_console :: HANDLE -> Ptr Word16 -> DWORD -> Ptr DWORD -> Ptr ()
                  -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h ReadConsoleInputW"
  c_read_console_input :: HANDLE -> PINPUT_RECORD -> DWORD -> LPDWORD -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h GetNumberOfConsoleInputEvents"
  c_get_num_console_inputs :: HANDLE -> LPDWORD -> IO BOOL

type LPSECURITY_ATTRIBUTES = LPVOID

-- -----------------------------------------------------------------------------
-- Reading and Writing

-- For this to actually block, the file handle must have
-- been created with FILE_FLAG_OVERLAPPED not set. As an implementation note I
-- am choosing never to let this block. But this can be easily accomplished by
-- a getOverlappedResult call with True
hwndRead :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
hwndRead hwnd ptr offset bytes = do
  mngr <- Mgr.getSystemManager
  fmap fromIntegral $ Mgr.withException "hwndRead" $
     withOverlappedEx mngr "hwndRead" (toHANDLE hwnd) (isAsynchronous hwnd)
                      offset (startCB ptr) completionCB
  where
    startCB outBuf lpOverlapped = do
      debugIO ":: hwndRead"
      -- See Note [ReadFile/WriteFile].
      ret <- c_ReadFile (toHANDLE hwnd) (castPtr outBuf)
                        (fromIntegral bytes) nullPtr lpOverlapped
      return $ Mgr.CbNone ret

    completionCB err dwBytes
      | err == #{const ERROR_SUCCESS}       = Mgr.ioSuccess $ fromIntegral dwBytes
      | err == #{const ERROR_HANDLE_EOF}    = Mgr.ioSuccess 0
      | err == #{const STATUS_END_OF_FILE}  = Mgr.ioSuccess 0
      | err == #{const ERROR_BROKEN_PIPE}   = Mgr.ioSuccess 0
      | err == #{const STATUS_PIPE_BROKEN}  = Mgr.ioSuccess 0
      | err == #{const ERROR_NO_MORE_ITEMS} = Mgr.ioSuccess $ fromIntegral dwBytes
      | err == #{const ERROR_MORE_DATA}     = Mgr.ioSuccess $ fromIntegral dwBytes
      | otherwise                           = Mgr.ioFailed err

-- In WinIO we'll never block in the FFI call, so this call is equivalent to
-- hwndRead,  Though we may revisit this when implementing sockets and pipes.
-- It still won't block, but may set up extra book keeping so threadWait and
-- threadWrite may work.
hwndReadNonBlocking :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int
                    -> IO (Maybe Int)
hwndReadNonBlocking hwnd ptr offset bytes
  = do mngr <- Mgr.getSystemManager
       val <- withOverlappedEx mngr "hwndReadNonBlocking" (toHANDLE hwnd)
                               (isAsynchronous hwnd) offset (startCB ptr)
                               completionCB
       return $ ioValue val
  where
    startCB inputBuf lpOverlapped = do
      debugIO ":: hwndReadNonBlocking"
      -- See Note [ReadFile/WriteFile].
      ret <- c_ReadFile (toHANDLE hwnd) (castPtr inputBuf)
                        (fromIntegral bytes) nullPtr lpOverlapped
      return $ Mgr.CbNone ret

    completionCB err dwBytes
      | err == #{const ERROR_SUCCESS}       = Mgr.ioSuccess $ Just $! fromIntegral dwBytes
      | err == #{const ERROR_HANDLE_EOF}    = Mgr.ioSuccess Nothing
      | err == #{const STATUS_END_OF_FILE}  = Mgr.ioSuccess Nothing
      | err == #{const ERROR_BROKEN_PIPE}   = Mgr.ioSuccess Nothing
      | err == #{const STATUS_PIPE_BROKEN}  = Mgr.ioSuccess Nothing
      | err == #{const ERROR_NO_MORE_ITEMS} = Mgr.ioSuccess Nothing
      | err == #{const ERROR_MORE_DATA}     = Mgr.ioSuccess $ Just $! fromIntegral dwBytes
      | otherwise                           = Mgr.ioFailedAny err

hwndWrite :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int -> IO ()
hwndWrite hwnd ptr offset bytes
  = do mngr <- Mgr.getSystemManager
       _ <- Mgr.withException "hwndWrite" $
          withOverlappedEx mngr "hwndWrite" (toHANDLE hwnd)
                           (isAsynchronous hwnd) offset (startCB ptr)
                           completionCB
       return ()
  where
    startCB outBuf lpOverlapped = do
      debugIO ":: hwndWrite"
      -- See Note [ReadFile/WriteFile].
      ret <- c_WriteFile (toHANDLE hwnd) (castPtr outBuf)
                         (fromIntegral bytes) nullPtr lpOverlapped
      return $ Mgr.CbNone ret

    completionCB err dwBytes
        | err == #{const ERROR_SUCCESS}  =   Mgr.ioSuccess $ fromIntegral dwBytes
        | err == #{const ERROR_HANDLE_EOF} = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise                        = Mgr.ioFailed err

hwndWriteNonBlocking :: Io NativeHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
hwndWriteNonBlocking hwnd ptr offset bytes
  = do mngr <- Mgr.getSystemManager
       val <- withOverlappedEx mngr "hwndReadNonBlocking" (toHANDLE hwnd)
                               (isAsynchronous hwnd) offset (startCB ptr)
                               completionCB
       return $ fromIntegral $ ioValue val
  where
    startCB :: Ptr a -> LPOVERLAPPED -> IO (Mgr.CbResult a1)
    startCB outBuf lpOverlapped = do
      debugIO ":: hwndWriteNonBlocking"
      -- See Note [ReadFile/WriteFile].
      ret <- c_WriteFile (toHANDLE hwnd) (castPtr outBuf)
                         (fromIntegral bytes) nullPtr lpOverlapped
      return $ Mgr.CbNone ret

    completionCB err dwBytes
        | err == #{const ERROR_SUCCESS}    = Mgr.ioSuccess $ fromIntegral dwBytes
        | err == #{const ERROR_HANDLE_EOF} = Mgr.ioSuccess $ fromIntegral dwBytes
        | otherwise                        = Mgr.ioFailed err

-- Note [ReadFile/WriteFile]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The results of these functions are somewhat different when working in an
-- asynchronous manner. The returning bool has two meaning.
--
-- True: The operation is done and was completed synchronously.  This is
--       possible because of the optimization flags we enable.  In this case
--       there won't be a completion event for this call and so we shouldn't
--       queue one up. If we do this request will never terminate.  It's also
--       safe to free the OVERLAPPED structure immediately.
--
-- False: Only indicates that the operation was not completed synchronously, a
--        call to GetLastError () is needed to find out the actual status. If
--        the result is ERROR_IO_PENDING then the operation has been queued on
--        the completion port and we should proceed asynchronously.  Any other
--        state is usually an indication that the call failed.
--
-- NB. reading an EOF will result in ERROR_HANDLE_EOF or STATUS_END_OF_FILE
-- during the checking of the completion results.  We need to check for these
-- so we don't incorrectly fail.


consoleWrite :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int -> IO ()
consoleWrite hwnd ptr _offset bytes
  = alloca $ \res ->
      do failIfFalse_ "GHC.IO.Handle.consoleWrite" $ do
           debugIO ":: consoleWrite"
           withGhcInternalToUTF16 ptr bytes $ \(w_ptr, w_len) -> do
              success <- c_write_console (toHANDLE hwnd) w_ptr
                                         (fromIntegral w_len) res nullPtr
              if not success
                 then return False
                 else do val <- fromIntegral <$> peek res
                         return $ val == w_len

consoleWriteNonBlocking :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
consoleWriteNonBlocking hwnd ptr _offset bytes
  = alloca $ \res ->
      do failIfFalse_ "GHC.IO.Handle.consoleWriteNonBlocking" $ do
            debugIO ":: consoleWriteNonBlocking"
            withGhcInternalToUTF16 ptr bytes $ \(w_ptr, w_len) -> do
              c_write_console (toHANDLE hwnd) w_ptr (fromIntegral w_len)
                              res nullPtr
         val <- fromIntegral <$> peek res
         return val

consoleRead :: Bool -> Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int -> IO Int
consoleRead blocking hwnd ptr _offset bytes
  = withUTF16ToGhcInternal ptr bytes $ \reqBytes w_ptr ->
      alloca $ \res -> do
       cooked <- isCooked hwnd
       -- Cooked input must be handled differently when the STD handles are
       -- attached to a real console handle.  For File based handles we can't do
       -- proper cooked inputs, but since the actions are async you would get
       -- results as soon as available.
       --
       -- For console handles We have to use a lower level API then ReadConsole,
       -- namely we must use ReadConsoleInput which requires us to process
       -- all console message manually.
       --
       -- Do note that MSYS2 shells such as bash don't attach to a real handle,
       -- and instead have by default a pipe/file based std handles.  Which
       -- means the cooked behaviour is best when used in a native Windows
       -- terminal such as cmd, powershell or ConEmu.
       case cooked || not blocking of
        False -> do
          debugIO "consoleRead :: un-cooked I/O read."
          -- eotControl allows us to handle control characters like EOL
          -- without needing a newline, which would sort of defeat the point
          -- of an EOL.
          res_code <- with eotControl $ \p_eotControl ->
                c_read_console (toHANDLE hwnd) w_ptr (fromIntegral reqBytes) res
                               p_eotControl

          -- Restore a quirk of the POSIX read call, which only returns a fail
          -- when the handle is invalid, e.g. closed or not a handle.  It how-
          -- ever returns 0 when the handle is valid but unreadable, such as
          -- passing a handle with no GENERIC_READ permission, like /dev/null
          err <- getLastError
          when (not res_code) $
            case () of
             _ | err == #{const ERROR_INVALID_FUNCTION} -> return ()
               | otherwise -> failWith "GHC.IO.Handle.consoleRead" err
          b_read <- fromIntegral <$> peek res
          if b_read /= 1
              then return b_read
              else do w_first <- peekElemOff w_ptr 0
                      case () of
                        -- Handle Ctrl+Z which is the actual EOL sequence on
                        -- windows, but also handle Ctrl+D which is what the
                        -- ASCII standard defines as EOL.
                        _ | w_first == fromIntegral acCtrlD -> return 0
                          | w_first == fromIntegral acCtrlZ -> return 0
                          | otherwise                       -> return b_read
        True -> do
          debugIO "consoleRead :: cooked I/O read."
          -- Input is cooked, don't wait till a line return and consume all
          -- characters as they are.  Technically this function can handle any
          -- console event.  Including mouse, window and virtual key events
          -- but for now I'm only interested in key presses.
          let entries = fromIntegral $ reqBytes `div` (#size INPUT_RECORD)
          allocaBytes entries $ \p_inputs ->
            maybeReadEvent p_inputs entries res w_ptr

          -- Check to see if we have been explicitly asked to do a non-blocking
          -- I/O, and if we were, make sure that if we didn't have any console
          -- events that we don't block.
    where maybeReadEvent p_inputs entries res w_ptr =
            case (not blocking) of
              True -> do
                avail <- with (0 :: DWORD) $ \num_events_ptr -> do
                  failIfFalse_ "GHC.IO.Handle.consoleRead [non-blocking]" $
                    c_get_num_console_inputs (toHANDLE hwnd) num_events_ptr
                  peek num_events_ptr
                debugIO $ "consoleRead [avail] :: " ++ show avail
                if avail > 0
                  then readEvent p_inputs entries res w_ptr
                  else return 0
              False -> readEvent p_inputs entries res w_ptr

          -- Unconditionally issue the first read, but conditionally
          -- do the recursion.
          readEvent p_inputs entries res w_ptr = do
            failIfFalse_ "GHC.IO.Handle.consoleRead" $
              c_read_console_input (toHANDLE hwnd) p_inputs
                                   (fromIntegral entries) res

            b_read <- fromIntegral <$> peek res
            read <- cobble b_read w_ptr p_inputs
            if read > 0
               then return $ fromIntegral read
               else maybeReadEvent p_inputs entries res w_ptr

          -- Dereference and read console input records.  We only read the bare
          -- minimum required to know which key/sequences were pressed.  To do
          -- this and prevent having to fully port the PINPUT_RECORD structure
          -- in Haskell we use some GCC builtins to find the correct offsets.
          cobble :: Int -> Ptr Word16 -> PINPUT_RECORD -> IO Int
          cobble 0 _ _ = do debugIO "cobble: done."
                            return 0
          cobble n w_ptr p_inputs =
            do eventType <- peekByteOff p_inputs 0 :: IO WORD
               debugIO $ "cobble: Length=" ++ show n
               debugIO $ "cobble: Type=" ++ show eventType
               let ni_offset      = #size INPUT_RECORD
               let event          = #{const __builtin_offsetof (INPUT_RECORD, Event)}
               let char_offset    = event + #{const __builtin_offsetof (KEY_EVENT_RECORD, uChar)}
               let btnDown_offset = event + #{const __builtin_offsetof (KEY_EVENT_RECORD, bKeyDown)}
               let repeat_offset  = event + #{const __builtin_offsetof (KEY_EVENT_RECORD, wRepeatCount)}
               let n'             = n - 1
               let p_inputs'      = p_inputs `plusPtr` ni_offset
               btnDown  <- peekByteOff p_inputs btnDown_offset
               repeated <- fromIntegral <$> (peekByteOff p_inputs repeat_offset :: IO WORD)
               debugIO $ "cobble: BtnDown=" ++ show btnDown
               -- Handle the key only on button down and not on button up.
               if eventType == #{const KEY_EVENT} && btnDown
                  then do debugIO $ "cobble: read-char."
                          char <- peekByteOff p_inputs char_offset
                          let w_ptr' = w_ptr `plusPtr` 1
                          debugIO $ "cobble: offset - " ++ show char_offset
                          debugIO $ "cobble: show > " ++ show char
                          debugIO $ "cobble: repeat: " ++ show repeated
                          pokeArray w_ptr $ replicate repeated char
                          (+1) <$> cobble n' w_ptr' p_inputs'
                  else do debugIO $ "cobble: skip event."
                          cobble n' w_ptr p_inputs'


consoleReadNonBlocking :: Io ConsoleHandle -> Ptr Word8 -> Word64 -> Int
                       -> IO (Maybe Int)
consoleReadNonBlocking hwnd ptr offset bytes
  = Just <$> consoleRead False hwnd ptr offset bytes

-- -----------------------------------------------------------------------------
-- Operations on file handles

handle_ready :: RawHandle a => a -> Bool -> Int -> IO Bool
handle_ready hwnd write msecs = do
  r <- throwErrnoIfMinus1Retry "GHC.IO.Windows.Handle.handle_ready" $
          c_handle_ready (toHANDLE hwnd) write (fromIntegral msecs)
  return (toEnum (fromIntegral r))

handle_is_console :: RawHandle a => a -> IO Bool
handle_is_console = c_is_console . toHANDLE

handle_close :: RawHandle a => a -> IO ()
handle_close h = do release h
                    failIfFalse_ "handle_close" $ c_close_handle (toHANDLE h)

handle_dev_type :: RawHandle a => a -> IO IODeviceType
handle_dev_type hwnd = do _type <- c_handle_type $ toHANDLE hwnd
                          return $ case _type of
                                     _ | _type == 3 -> Stream
                                       | _type == 5 -> RawDevice
                                       | otherwise  -> RegularFile

handle_is_seekable :: RawHandle a => a -> IO Bool
handle_is_seekable hwnd = do
  t <- handle_dev_type hwnd
  return (t == RegularFile || t == RawDevice)

handle_seek :: RawHandle a => a -> SeekMode -> Integer -> IO Integer
handle_seek hwnd mode off =
  with 0 $ \off_rel -> do
    failIfFalse_ "GHC.IO.Handle.handle_seek" $
        c_set_file_pointer (toHANDLE hwnd) (fromIntegral off) seektype off_rel
    fromIntegral <$> peek off_rel
 where
    seektype :: DWORD
    seektype = case mode of
                   AbsoluteSeek -> #{const FILE_BEGIN}
                   RelativeSeek -> #{const FILE_CURRENT}
                   SeekFromEnd  -> #{const FILE_END}

handle_tell :: RawHandle a => a -> IO Integer
handle_tell hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_tell" $
          c_get_file_pointer (toHANDLE hwnd))

handle_set_size :: RawHandle a => a -> Integer -> IO ()
handle_set_size hwnd size =
  failIfFalse_ "GHC.IO.Handle.handle_set_size" $
      c_set_file_size (toHANDLE hwnd) (fromIntegral size)

handle_get_size :: RawHandle a => a -> IO Integer
handle_get_size hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_set_size" $
          c_get_file_size (toHANDLE hwnd))

handle_set_echo :: RawHandle a => a -> Bool -> IO ()
handle_set_echo hwnd value =
  failIfFalse_ "GHC.IO.Handle.handle_set_echo" $
      c_set_console_echo (toHANDLE hwnd) value

handle_get_echo :: RawHandle a => a -> IO Bool
handle_get_echo = c_get_console_echo . toHANDLE

handle_duplicate :: RawHandle a => a -> IO a
handle_duplicate hwnd = alloca $ \ptr -> do
  failIfFalse_ "GHC.IO.Handle.handle_duplicate" $
      c_duplicate_handle (toHANDLE hwnd) ptr
  fromHANDLE <$> peek ptr

console_set_buffering :: Io ConsoleHandle -> Bool -> IO ()
console_set_buffering hwnd value = setCooked hwnd value >> return ()

handle_set_buffering :: RawHandle a => a -> Bool -> IO ()
handle_set_buffering hwnd value =
  failIfFalse_ "GHC.IO.Handle.handle_set_buffering" $
      c_set_console_buffering (toHANDLE hwnd) value

handle_console_seek :: RawHandle a => a -> SeekMode -> Integer -> IO Integer
handle_console_seek hwnd mode off =
  with 0 $ \loc_ptr -> do
    failIfFalse_ "GHC.IO.Handle.handle_console_seek" $
      c_set_console_pointer (toHANDLE hwnd) (fromIntegral off) seektype loc_ptr
    fromIntegral <$> peek loc_ptr
 where
    seektype :: DWORD
    seektype = case mode of
                 AbsoluteSeek -> #{const FILE_BEGIN}
                 RelativeSeek -> #{const FILE_CURRENT}
                 SeekFromEnd  -> #{const FILE_END}

handle_console_tell :: RawHandle a => a -> IO Integer
handle_console_tell hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_console_tell" $
          c_get_console_pointer (toHANDLE hwnd))

handle_set_console_size :: RawHandle a => a -> Integer -> IO ()
handle_set_console_size hwnd size =
  failIfFalse_ "GHC.IO.Handle.handle_set_console_size" $
      c_set_console_buffer_size (toHANDLE hwnd) (fromIntegral size)

handle_get_console_size :: RawHandle a => a -> IO Integer
handle_get_console_size hwnd =
   fromIntegral `fmap`
      (throwErrnoIfMinus1Retry "GHC.IO.Handle.handle_get_console_size" $
          c_get_console_buffer_size (toHANDLE hwnd))

-- -----------------------------------------------------------------------------
-- opening files

-- | Describes if and which temp file flags to use.
data TempFileOptions = NoTemp | TempNonExcl | TempExcl deriving Eq

-- | Open a file and make an 'NativeHandle' for it.  Truncates the file to zero
-- size when the `IOMode` is `WriteMode`.
openFile
  :: FilePath -- ^ file to open
  -> IOMode   -- ^ mode in which to open the file
  -> Bool     -- ^ open the file in non-blocking mode?
  -> IO (Io NativeHandle, IODeviceType)
openFile filepath iomode non_blocking = openFile' filepath iomode non_blocking NoTemp

-- | Open a file as a temporary file and make an 'NativeHandle' for it.
-- Truncates the file to zero size when the `IOMode` is `WriteMode`.
openFileAsTemp
  :: FilePath -- ^ file to open
  -> Bool     -- ^ open the file in non-blocking mode?
  -> Bool     -- ^ Exclusive mode
  -> IO (Io NativeHandle, IODeviceType)
openFileAsTemp filepath non_blocking excl
  = openFile' filepath ReadWriteMode non_blocking (if excl then TempExcl else TempNonExcl)

-- | Open a file and make an 'NativeHandle' for it.  Truncates the file to zero
-- size when the `IOMode` is `WriteMode`.
openFile'
  :: FilePath -- ^ file to open
  -> IOMode   -- ^ mode in which to open the file
  -> Bool     -- ^ open the file in non-blocking mode?
  -> TempFileOptions
  -> IO (Io NativeHandle, IODeviceType)
openFile' filepath iomode non_blocking tmp_opts =
   do devicepath <- getDevicePath filepath
      h <- createFile devicepath
      -- Attach the handle to the I/O manager's CompletionPort.  This allows the
      -- I/O manager to service requests for this Handle.
      Mgr.associateHandle' h
      let hwnd = fromHANDLE h
      _type <- devType hwnd

      -- Use the rts to enforce any file locking we may need.
      let write_lock = iomode /= ReadMode

      case _type of
        -- Regular files need to be locked.
        -- See also Note [RTS File locking]
        RegularFile -> do
          optimizeFileAccess h -- Set a few optimization flags on file handles.
          (unique_dev, unique_ino) <- getUniqueFileInfo hwnd
          r <- lockFile (fromIntegral $ ptrToWordPtr h) unique_dev unique_ino
                        (fromBool write_lock)
          when (r == -1)  $
               ioException (IOError Nothing ResourceBusy "openFile"
                                  "file is locked" Nothing Nothing)

        -- I don't see a reason for blocking directories.  So unlike the FD
        -- implementation I'll allow it.
        _ -> return ()

      -- We want to truncate() if this is an open in WriteMode, but only
      -- if the target is a RegularFile.  but TRUNCATE_EXISTING would fail if
      -- the file didn't exit.  So just set the size afterwards.
      when (iomode == WriteMode && _type == RegularFile) $
        setSize hwnd 0

      return (hwnd, _type)
        where
          flagIf p f2
            | p         = f2
            | otherwise = 0
          -- We have to use in-process locking (e.g. use the locking mechanism
          -- in the rts) so we're consistent with the linux behavior and the
          -- rts knows about the lock.  See #4363 for more.
          file_share_mode =  #{const FILE_SHARE_READ}
                         .|. #{const FILE_SHARE_DELETE}
                         -- Don't support shared writing for temp files.
                         .|. (flagIf (tmp_opts == NoTemp)
                                     #{const FILE_SHARE_WRITE})

          file_access_mode =
            case iomode of
              ReadMode      -> #{const GENERIC_READ}
              WriteMode     -> #{const GENERIC_WRITE}
              ReadWriteMode -> #{const GENERIC_READ}
                            .|. #{const GENERIC_WRITE}
              AppendMode    -> #{const GENERIC_WRITE}
                            .|. #{const FILE_APPEND_DATA}

          file_open_mode =
            case iomode of
              ReadMode      -> #{const OPEN_EXISTING} -- O_RDONLY
              WriteMode     -> #{const OPEN_ALWAYS}   -- O_CREAT | O_WRONLY | O_TRUNC
              ReadWriteMode ->
                case tmp_opts of
                  NoTemp    -> #{const OPEN_ALWAYS}   -- O_CREAT | O_RDWR
                  TempNonExcl ->  #{const CREATE_ALWAYS} -- O_CREAT | O_RDWR
                  TempExcl  -> #{const CREATE_NEW}    -- O_CREAT | O_RDWR | O_EXCL
              AppendMode    -> #{const OPEN_ALWAYS}   -- O_APPEND

          file_create_flags =
            if non_blocking
               -- On Windows, the choice of whether an operation completes
               -- asynchronously or not depends on how the Handle was created
               -- and not on the operation called.  As in, the behaviour of
               -- ReadFile and WriteFile depends on the flags used to open the
               -- handle.   For WinIO we always use FILE_FLAG_OVERLAPPED, which
               -- means we always issue asynchronous file operation using an
               -- OVERLAPPED structure.  All blocking, if required must be done
               -- on the Haskell side by using existing mechanisms such as MVar
               -- or IOPorts.
               then #{const FILE_FLAG_OVERLAPPED}
                    -- I believe most haskell programs do sequential scans, so
                    -- optimize for the common case.  Though ideally, this would
                    -- be parameterized by openFile.  This will absolutely trash
                    -- the cache on reverse scans.
                    --
                    -- TODO: make a parameter to openFile and specify only for
                    -- operations we know are sequential.  This parameter should
                    -- be usable by madvise too.
                    .|. #{const FILE_FLAG_SEQUENTIAL_SCAN}
                    .|. (flagIf (tmp_opts /= NoTemp)
                                -- Hold data in cache for as long as possible
                                #{const FILE_ATTRIBUTE_TEMPORARY} )
               else #{const FILE_ATTRIBUTE_NORMAL}
                    .|. (flagIf (tmp_opts /= NoTemp)
                                -- Hold data in cache for as long as possible
                                #{const FILE_ATTRIBUTE_TEMPORARY} )

          createFile devicepath =
            withCWString devicepath $ \fp ->
                failIf (== iNVALID_HANDLE_VALUE) "CreateFile" $
                      c_CreateFile fp file_access_mode
                                      file_share_mode
                                      nullPtr
                                      file_open_mode
                                      file_create_flags
                                      nullPtr

-- Tell the OS that we support skipping the request Queue if the
-- IRQ can be handled immediately, e.g. if the data is in the cache.
optimizeFileAccess :: HANDLE -> IO ()
optimizeFileAccess handle =
    failIfFalse_ "SetFileCompletionNotificationModes"  $
      c_SetFileCompletionNotificationModes handle
          (    #{const FILE_SKIP_COMPLETION_PORT_ON_SUCCESS}
            .|. #{const FILE_SKIP_SET_EVENT_ON_HANDLE})

-- Reconstruct an I/O mode from an open HANDLE
handleToMode :: HANDLE -> IO IOMode
handleToMode hwnd = do
  mask <- c_get_handle_access_mask hwnd
  let hasFlag flag = (flag .&. mask) == flag
  case () of
    () | hasFlag (#{const FILE_APPEND_DATA})                        -> return AppendMode
       | hasFlag (#{const GENERIC_WRITE} .|. #{const GENERIC_READ}) -> return ReadWriteMode
       | hasFlag (#{const GENERIC_READ})                            -> return ReadMode
       | hasFlag (#{const GENERIC_WRITE})                           -> return WriteMode
       | otherwise -> error "unknown access mask in handleToMode."

foreign import ccall unsafe "__get_handle_access_mask"
  c_get_handle_access_mask :: HANDLE -> IO DWORD

release :: RawHandle a => a -> IO ()
release h = if isLockable h
               then do let handle = fromIntegral $ ptrToWordPtr $ toHANDLE h
                       _ <- unlockFile handle
                       return ()
               else return ()

-- -----------------------------------------------------------------------------
-- Locking/unlocking

foreign import ccall unsafe "lockFile"
  lockFile :: CUIntPtr -> Word64 -> Word64 -> CInt -> IO CInt

foreign import ccall unsafe "unlockFile"
  unlockFile :: CUIntPtr -> IO CInt

-- | Returns -1 on error. Otherwise writes two values representing
-- the file into the given ptrs.
foreign import ccall unsafe "get_unique_file_info_hwnd"
  c_getUniqueFileInfo :: HANDLE -> Ptr Word64 -> Ptr Word64 -> IO ()

-- | getUniqueFileInfo assumes the C call to getUniqueFileInfo
-- succeeds.
getUniqueFileInfo :: RawHandle a => a -> IO (Word64, Word64)
getUniqueFileInfo handle = do
  with 0 $ \devptr -> do
    with 0 $ \inoptr -> do
      c_getUniqueFileInfo (toHANDLE handle) devptr inoptr
      liftM2 (,) (peek devptr) (peek inoptr)
