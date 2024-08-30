{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE CPP, NoImplicitPrelude, CApiFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.System.Posix.Internals
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires POSIX)
--
-- POSIX support layer for the standard libraries.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-- This library is built on *every* platform, including Win32.
--
-- Non-POSIX compliant in order to support the following features:
--  * S_ISSOCK (no sockets in POSIX)
--
-----------------------------------------------------------------------------

module GHC.Internal.System.Posix.Internals where

#include <ghcplatform.h>
#include "HsBaseConfig.h"

import GHC.Internal.System.Posix.Types

import GHC.Internal.Foreign.C.Error
import GHC.Internal.Foreign.Marshal.Alloc
import GHC.Internal.Foreign.C.String
import GHC.Internal.Foreign.C.Types

import GHC.Internal.Data.Maybe

#if !defined(HTYPE_TCFLAG_T)
import GHC.Internal.System.IO.Error
#endif

import GHC.Internal.Base
import GHC.Internal.Bits
import GHC.Internal.Num
import GHC.Internal.Real
import GHC.Internal.Word
import GHC.Internal.IO
import GHC.Internal.IO.IOMode
import GHC.Internal.IO.Exception
import GHC.Internal.IO.Device
import GHC.Internal.Ptr

#if defined(javascript_HOST_ARCH)
import GHC.Internal.Int (Int64)
#endif

#if !defined(mingw32_HOST_OS)
import {-# SOURCE #-} GHC.Internal.IO.Encoding (getFileSystemEncoding)
import GHC.Internal.Foreign.Marshal.Utils
import GHC.Internal.Foreign.Storable
import qualified GHC.Internal.Foreign.C.String.Encoding as GHC
#else
import GHC.Internal.Int
import GHC.Internal.Data.OldList (elem)
#endif

-- ---------------------------------------------------------------------------
-- Debugging the base package

puts :: String -> IO ()
puts s = withCAStringLen (s ++ "\n") $ \(p, len) -> do
            -- In reality should be withCString, but assume ASCII to avoid loop
            -- if this is called by GHC.Internal.Foreign
           _ <- c_write 1 (castPtr p) (fromIntegral len)
           return ()


-- ---------------------------------------------------------------------------
-- Types

data {-# CTYPE "struct flock" #-} CFLock
data {-# CTYPE "struct group" #-} CGroup
data {-# CTYPE "struct lconv" #-} CLconv
data {-# CTYPE "struct passwd" #-} CPasswd
data {-# CTYPE "struct sigaction" #-} CSigaction
data {-# CTYPE "sigset_t" #-} CSigset
data {-# CTYPE "struct stat" #-}  CStat
data {-# CTYPE "struct termios" #-} CTermios
data {-# CTYPE "struct tm" #-} CTm
data {-# CTYPE "struct tms" #-} CTms
data {-# CTYPE "struct utimbuf" #-} CUtimbuf
data {-# CTYPE "struct utsname" #-} CUtsname

type FD = CInt

-- ---------------------------------------------------------------------------
-- stat()-related stuff

fdFileSize :: FD -> IO Integer
fdFileSize fd =
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry_ "fileSize" $
        c_fstat fd p_stat
    c_mode <- st_mode p_stat :: IO CMode
    if not (s_isreg c_mode)
        then return (-1)
        else do
      c_size <- st_size p_stat
      return (fromIntegral c_size)

fileType :: FilePath -> IO IODeviceType
fileType file =
  allocaBytes sizeof_stat $ \ p_stat ->
  withFilePath file $ \p_file -> do
    throwErrnoIfMinus1Retry_ "fileType" $
      c_stat p_file p_stat
    statGetType p_stat

-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
fdStat :: FD -> IO (IODeviceType, CDev, CIno)
fdStat fd =
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry_ "fdType" $
        c_fstat fd p_stat
    ty <- statGetType p_stat
    dev <- st_dev p_stat
    ino <- st_ino p_stat
    return (ty,dev,ino)

fdType :: FD -> IO IODeviceType
fdType fd = do (ty,_,_) <- fdStat fd; return ty

-- | Return a known device type or throw an exception if the device
-- type is unknown.
statGetType :: Ptr CStat -> IO IODeviceType
statGetType p_stat = do
   dev_ty_m <- statGetType_maybe p_stat
   case dev_ty_m of
      Nothing -> ioError ioe_unknownfiletype
      Just dev_ty -> pure dev_ty

-- | Unlike @statGetType@, @statGetType_maybe@ will not throw an exception
-- if the CStat refers to a unknown device type.
--
-- @since base-4.20.1.0
statGetType_maybe :: Ptr CStat -> IO (Maybe IODeviceType)
statGetType_maybe p_stat = do
  c_mode <- st_mode p_stat :: IO CMode
  case () of
      _ | s_isdir c_mode        -> return $ Just Directory
        | s_isfifo c_mode || s_issock c_mode || s_ischr  c_mode
                                -> return $ Just Stream
        | s_isreg c_mode        -> return $ Just RegularFile
         -- Q: map char devices to RawDevice too?
        | s_isblk c_mode        -> return $ Just RawDevice
        | otherwise             -> return Nothing

ioe_unknownfiletype :: IOException
ioe_unknownfiletype = IOError Nothing UnsupportedOperation "fdType"
                        "unknown file type"
                        Nothing
                        Nothing

fdGetMode :: FD -> IO IOMode
#if defined(mingw32_HOST_OS) || defined(javascript_HOST_ARCH)
fdGetMode _ = do
    -- We don't have a way of finding out which flags are set on FDs
    -- on Windows/JS, so make a handle that thinks that anything goes.
    let flags = o_RDWR
#else
fdGetMode fd = do
    flags <- throwErrnoIfMinus1Retry "fdGetMode"
                (c_fcntl_read fd const_f_getfl)
#endif
    let
       wH  = (flags .&. o_WRONLY) /= 0
       aH  = (flags .&. o_APPEND) /= 0
       rwH = (flags .&. o_RDWR) /= 0

       mode
         | wH && aH  = AppendMode
         | wH        = WriteMode
         | rwH       = ReadWriteMode
         | otherwise = ReadMode

    return mode

#if defined(mingw32_HOST_OS)
withFilePath :: FilePath -> (CWString -> IO a) -> IO a
withFilePath fp f = do
    checkForInteriorNuls fp
    withCWString fp f

newFilePath :: FilePath -> IO CWString
newFilePath fp = do
    checkForInteriorNuls fp
    newCWString fp

peekFilePath :: CWString -> IO FilePath
peekFilePath = peekCWString

-- | Check a 'FilePath' for internal NUL codepoints as these are
-- disallowed in Windows filepaths. See #13660.
checkForInteriorNuls :: FilePath -> IO ()
checkForInteriorNuls fp = when ('\0' `elem` fp) (throwInternalNulError fp)

#else

withFilePath :: FilePath -> (CString -> IO a) -> IO a
newFilePath :: FilePath -> IO CString
peekFilePath :: CString -> IO FilePath
peekFilePathLen :: CStringLen -> IO FilePath

withFilePath fp f = do
    enc <- getFileSystemEncoding
    GHC.withCStringLen0 enc fp $ \(str, len) -> do
        checkForInteriorNuls fp (str, len)
        f str
newFilePath fp = do
    enc <- getFileSystemEncoding
    (str, len) <- GHC.newCStringLen0 enc fp
    checkForInteriorNuls fp (str, len)
    return str
peekFilePath fp = getFileSystemEncoding >>= \enc -> GHC.peekCString enc fp
peekFilePathLen fp = getFileSystemEncoding >>= \enc -> GHC.peekCStringLen enc fp

-- | Check an encoded 'FilePath' for internal NUL octets as these are
-- disallowed in POSIX filepaths. See #13660.
checkForInteriorNuls :: FilePath -> CStringLen -> IO ()
checkForInteriorNuls fp (str, len) =
    when (len' /= len) (throwInternalNulError fp)
    -- N.B. If the string contains internal NUL codeunits then the strlen will
    -- indicate a size smaller than that returned by withCStringLen.
  where
    len' = case str of Ptr ptr -> I# (cstringLength# ptr)
#endif

throwInternalNulError :: FilePath -> IO a
throwInternalNulError fp = ioError err
  where
    err =
      IOError
        { ioe_handle = Nothing
        , ioe_type = InvalidArgument
        , ioe_location = "checkForInteriorNuls"
        , ioe_description = "FilePaths must not contain internal NUL code units."
        , ioe_errno = Nothing
        , ioe_filename = Just fp
        }

-- ---------------------------------------------------------------------------
-- Terminal-related stuff

#if defined(HTYPE_TCFLAG_T)

setEcho :: FD -> Bool -> IO ()
setEcho fd on =
  tcSetAttr fd $ \ p_tios -> do
    lflag <- c_lflag p_tios :: IO CTcflag
    let new_lflag
         | on        = lflag .|. fromIntegral const_echo
         | otherwise = lflag .&. complement (fromIntegral const_echo)
    poke_c_lflag p_tios (new_lflag :: CTcflag)

getEcho :: FD -> IO Bool
getEcho fd =
  tcSetAttr fd $ \ p_tios -> do
    lflag <- c_lflag p_tios :: IO CTcflag
    return ((lflag .&. fromIntegral const_echo) /= 0)

setCooked :: FD -> Bool -> IO ()
setCooked fd cooked =
  tcSetAttr fd $ \ p_tios -> do

    -- turn on/off ICANON
    lflag <- c_lflag p_tios :: IO CTcflag
    let new_lflag | cooked    = lflag .|. (fromIntegral const_icanon)
                  | otherwise = lflag .&. complement (fromIntegral const_icanon)
    poke_c_lflag p_tios (new_lflag :: CTcflag)

    -- set VMIN & VTIME to 1/0 respectively
    when (not cooked) $ do
            c_cc <- ptr_c_cc p_tios
            let vmin  = (c_cc `plusPtr` (fromIntegral const_vmin))  :: Ptr Word8
                vtime = (c_cc `plusPtr` (fromIntegral const_vtime)) :: Ptr Word8
            poke vmin  1
            poke vtime 0

tcSetAttr :: FD -> (Ptr CTermios -> IO a) -> IO a
tcSetAttr fd fun =
     allocaBytes sizeof_termios  $ \p_tios -> do
        throwErrnoIfMinus1Retry_ "tcSetAttr"
           (c_tcgetattr fd p_tios)

        -- Save a copy of termios, if this is a standard file descriptor.
        -- These terminal settings are restored in hs_exit().
        when (fd <= 2) $ do
          p <- get_saved_termios fd
          when (p == nullPtr) $ do
             saved_tios <- mallocBytes sizeof_termios
             copyBytes saved_tios p_tios sizeof_termios
             set_saved_termios fd saved_tios

        -- tcsetattr() when invoked by a background process causes the process
        -- to be sent SIGTTOU regardless of whether the process has TOSTOP set
        -- in its terminal flags (try it...).  This function provides a
        -- wrapper which temporarily blocks SIGTTOU around the call, making it
        -- transparent.
        allocaBytes sizeof_sigset_t $ \ p_sigset ->
          allocaBytes sizeof_sigset_t $ \ p_old_sigset -> do
             throwErrnoIfMinus1_ "sigemptyset" $
                 c_sigemptyset p_sigset
             throwErrnoIfMinus1_ "sigaddset" $
                 c_sigaddset   p_sigset const_sigttou
             throwErrnoIfMinus1_ "sigprocmask" $
                 c_sigprocmask const_sig_block p_sigset p_old_sigset
             r <- fun p_tios  -- do the business
             throwErrnoIfMinus1Retry_ "tcSetAttr" $
                 c_tcsetattr fd const_tcsanow p_tios
             throwErrnoIfMinus1_ "sigprocmask" $
                 c_sigprocmask const_sig_setmask p_old_sigset nullPtr
             return r

foreign import ccall unsafe "HsBase.h __hscore_get_saved_termios"
   get_saved_termios :: CInt -> IO (Ptr CTermios)

foreign import ccall unsafe "HsBase.h __hscore_set_saved_termios"
   set_saved_termios :: CInt -> (Ptr CTermios) -> IO ()

#elif defined(mingw32_HOST_OS)

-- 'raw' mode for Win32 means turn off 'line input' (=> buffering and
-- character translation for the console.) The Win32 API for doing
-- this is GetConsoleMode(), which also requires echoing to be disabled
-- when turning off 'line input' processing. Notice that turning off
-- 'line input' implies enter/return is reported as '\r' (and it won't
-- report that character until another character is input..odd.) This
-- latter feature doesn't sit too well with IO actions like IO.hGetLine..
-- consider yourself warned.
setCooked :: FD -> Bool -> IO ()
setCooked fd cooked = do
  x <- set_console_buffering fd (if cooked then 1 else 0)
  if (x /= 0)
   then ioError (ioe_unk_error "setCooked" "failed to set buffering")
   else return ()

ioe_unk_error :: String -> String -> IOException
ioe_unk_error loc msg
 = ioeSetErrorString (mkIOError OtherError loc Nothing Nothing) msg

-- Note: echoing goes hand in hand with enabling 'line input' / raw-ness
-- for Win32 consoles, hence setEcho ends up being the inverse of setCooked.
setEcho :: FD -> Bool -> IO ()
setEcho fd on = do
  x <- set_console_echo fd (if on then 1 else 0)
  if (x /= 0)
   then ioError (ioe_unk_error "setEcho" "failed to set echoing")
   else return ()

getEcho :: FD -> IO Bool
getEcho fd = do
  r <- get_console_echo fd
  if (r == (-1))
   then ioError (ioe_unk_error "getEcho" "failed to get echoing")
   else return (r == 1)

foreign import ccall unsafe "consUtils.h set_console_buffering__"
   set_console_buffering :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "consUtils.h set_console_echo__"
   set_console_echo :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "consUtils.h get_console_echo__"
   get_console_echo :: CInt -> IO CInt

foreign import ccall unsafe "consUtils.h is_console__"
   is_console :: CInt -> IO CInt

#else

setCooked :: FD -> Bool -> IO ()
setCooked _ _ = errorWithoutStackTrace "setCooked"

setEcho :: FD -> Bool -> IO ()
setEcho _ _ = errorWithoutStackTrace "setEcho"

getEcho :: FD -> IO Bool
getEcho _ = errorWithoutStackTrace "getEcho"

#endif

-- ---------------------------------------------------------------------------
-- Turning on non-blocking for a file descriptor

setNonBlockingFD :: FD -> Bool -> IO ()
#if !defined(mingw32_HOST_OS)
setNonBlockingFD fd set = do
  flags <- throwErrnoIfMinus1Retry "setNonBlockingFD"
                 (c_fcntl_read fd const_f_getfl)
  let flags' | set       = flags .|. o_NONBLOCK
             | otherwise = flags .&. complement o_NONBLOCK
  when (flags /= flags') $ do
    -- An error when setting O_NONBLOCK isn't fatal: on some systems
    -- there are certain file handles on which this will fail (eg. /dev/null
    -- on FreeBSD) so we throw away the return code from fcntl_write.
    _ <- c_fcntl_write fd const_f_setfl (fromIntegral flags')
    return ()
#else

-- bogus defns for win32
setNonBlockingFD _ _ = return ()

#endif

-- -----------------------------------------------------------------------------
-- Set close-on-exec for a file descriptor

#if !defined(mingw32_HOST_OS)
setCloseOnExec :: FD -> IO ()
setCloseOnExec fd =
  throwErrnoIfMinus1_ "setCloseOnExec" $
    c_fcntl_write fd const_f_setfd const_fd_cloexec
#endif

-- -----------------------------------------------------------------------------
-- foreign imports

#if !defined(mingw32_HOST_OS)
type CFilePath = CString
#else
type CFilePath = CWString
#endif

-- | The same as 'c_safe_open', but an /interruptible operation/
-- as described in "Control.Exception"—it respects `uninterruptibleMask`
-- but not `mask`.
--
-- We want to be able to interrupt an openFile call if
-- it's expensive (NFS, FUSE, etc.), and we especially
-- need to be able to interrupt a blocking open call.
-- See #17912.
--
-- @since base-4.16.0.0
c_interruptible_open :: CFilePath -> CInt -> CMode -> IO CInt
c_interruptible_open filepath oflags mode =
  getMaskingState >>= \case
    -- If we're in an uninterruptible mask, there's basically
    -- no point in using an interruptible FFI call. The open system call
    -- will be interrupted, but the exception won't be delivered
    -- unless the caller manually futzes with the masking state. So
    -- then the caller (assuming they're following the usual conventions)
    -- will retry the call (in response to EINTR), and we've just
    -- wasted everyone's time.
    MaskedUninterruptible -> c_safe_open_ filepath oflags mode
    _ -> do
      open_res <- c_interruptible_open_ filepath oflags mode
      -- c_interruptible_open_ is an interruptible foreign call.
      -- If the call is interrupted by an exception handler
      -- before the system call has returned (so the file is
      -- not yet open), we want to deliver the exception.
      -- In point of fact, we deliver any pending exception
      -- here regardless of the *reason* the system call
      -- fails.
      when (open_res == -1) $
        if hostIsThreaded
          then
            -- GHC.Internal.Control.Exception.allowInterrupt, inlined to avoid
            -- messing with any Haddock links.
            interruptible (pure ())
          else
            -- Try to make this work somewhat better on the non-threaded
            -- RTS. See #8684. This inlines the definition of `yield`; module
            -- dependencies look pretty hairy here and I don't want to make
            -- things worse for one little wrapper.
            interruptible (IO $ \s -> (# yield# s, () #))
      pure open_res

c_safe_open :: CFilePath -> CInt -> CMode -> IO CInt
c_safe_open filepath oflags mode =
  getMaskingState >>= \case
    -- When exceptions are unmasked, we use an interruptible
    -- open call. If the system call is successfully
    -- interrupted, the situation will be the same as if
    -- the exception had arrived before this function was
    -- called.
    Unmasked -> c_interruptible_open_ filepath oflags mode
    _ -> c_safe_open_ filepath oflags mode

-- | Consult the RTS to find whether it is threaded.
--
-- @since base-4.16.0.0
hostIsThreaded :: Bool
hostIsThreaded = rtsIsThreaded_ /= 0

#if !defined(javascript_HOST_ARCH)
foreign import ccall unsafe "HsBase.h __hscore_open"
   c_open :: CFilePath -> CInt -> CMode -> IO CInt

-- |
--
-- @since base-4.16.0.0
foreign import ccall interruptible "HsBase.h __hscore_open"
   c_interruptible_open_ :: CFilePath -> CInt -> CMode -> IO CInt

-- |
--
-- @since base-4.16.0.0
foreign import ccall unsafe "rts_isThreaded" rtsIsThreaded_ :: Int

foreign import ccall safe "HsBase.h __hscore_open"
   c_safe_open_ :: CFilePath -> CInt -> CMode -> IO CInt

foreign import ccall unsafe "HsBase.h __hscore_fstat"
   c_fstat :: CInt -> Ptr CStat -> IO CInt

foreign import ccall unsafe "HsBase.h __hscore_lstat"
   lstat :: CFilePath -> Ptr CStat -> IO CInt
#endif

#if defined(javascript_HOST_ARCH)

foreign import javascript unsafe "h$rts_isThreaded" rtsIsThreaded_ :: Int
foreign import javascript interruptible "h$base_access"
    c_access :: CString -> CInt -> IO CInt
foreign import javascript interruptible "h$base_chmod"
    c_chmod :: CString -> CMode -> IO CInt
foreign import javascript interruptible "h$base_close"
    c_close :: CInt -> IO CInt
foreign import javascript interruptible "h$base_creat"
    c_creat :: CString -> CMode -> IO CInt
foreign import javascript interruptible "h$base_dup"
    c_dup :: CInt -> IO CInt
foreign import javascript interruptible "h$base_dup2"
    c_dup2 :: CInt -> CInt -> IO CInt
foreign import javascript interruptible "h$base_fstat" -- fixme wrong type
    c_fstat :: CInt -> Ptr CStat -> IO CInt
foreign import javascript unsafe "h$base_isatty"
    c_isatty :: CInt -> IO CInt
foreign import javascript interruptible "h$base_lseek"
   c_lseek :: CInt -> COff -> CInt -> IO COff
foreign import javascript interruptible "h$base_lstat"
   lstat :: CFilePath -> Ptr CStat -> IO CInt
foreign import javascript interruptible "h$open"
   c_open :: CFilePath -> CInt -> CMode -> IO CInt
foreign import javascript interruptible "h$open"
   c_interruptible_open_ :: CFilePath -> CInt -> CMode -> IO CInt
foreign import javascript interruptible "h$open"
   c_safe_open_ :: CFilePath -> CInt -> CMode -> IO CInt
foreign import javascript interruptible "h$base_read"
   c_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
foreign import javascript interruptible "h$base_read"
   c_safe_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
foreign import javascript interruptible "h$base_stat" -- fixme wrong type
   c_stat :: CFilePath -> Ptr CStat -> IO CInt
foreign import javascript unsafe "h$base_umask"
   c_umask :: CMode -> IO CMode
foreign import javascript interruptible "h$base_write"
   c_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
foreign import javascript interruptible "h$base_write"
   c_safe_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
foreign import javascript interruptible "h$base_ftruncate" -- fixme COff
   c_ftruncate :: CInt -> FileOffset -> IO CInt
foreign import javascript interruptible "h$base_unlink"
   c_unlink :: CString -> IO CInt
foreign import javascript unsafe "h$base_getpid"
   c_getpid :: IO CPid
-- foreign import ccall unsafe "HsBase.h fork"
--   c_fork :: IO CPid
foreign import javascript interruptible "h$base_link"
   c_link :: CString -> CString -> IO CInt
foreign import javascript interruptible "h$base_mkfifo"
   c_mkfifo :: CString -> CMode -> IO CInt
foreign import javascript interruptible "h$base_pipe"
  c_pipe :: Ptr CInt -> IO CInt
foreign import javascript unsafe "h$base_sigemptyset"
   c_sigemptyset :: Ptr CSigset -> IO CInt
foreign import javascript unsafe "h$base_sigaddset"
   c_sigaddset :: Ptr CSigset -> CInt -> IO CInt
foreign import javascript unsafe "h$base_sigprocmask"
   c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt
foreign import javascript unsafe "h$base_tcgetattr"
   c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt
foreign import javascript unsafe "h$base_tcsetattr"
   c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt
foreign import javascript interruptible "h$base_utime"
   c_utime :: CString -> Ptr CUtimbuf -> IO CInt
foreign import javascript interruptible "h$base_waitpid"
   c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid

foreign import javascript unsafe "(() => { return h$base_o_rdonly; })"   o_RDONLY   :: CInt
foreign import javascript unsafe "(() => { return h$base_o_wronly; })"   o_WRONLY   :: CInt
foreign import javascript unsafe "(() => { return h$base_o_rdwr; })"     o_RDWR     :: CInt
foreign import javascript unsafe "(() => { return h$base_o_append; })"   o_APPEND   :: CInt
foreign import javascript unsafe "(() => { return h$base_o_creat; })"    o_CREAT    :: CInt
foreign import javascript unsafe "(() => { return h$base_o_excl; })"     o_EXCL     :: CInt
foreign import javascript unsafe "(() => { return h$base_o_trunc; })"    o_TRUNC    :: CInt
foreign import javascript unsafe "(() => { return h$base_o_noctty; })"   o_NOCTTY   :: CInt
foreign import javascript unsafe "(() => { return h$base_o_nonblock; })" o_NONBLOCK :: CInt
foreign import javascript unsafe "(() => { return h$base_o_binary; })"   o_BINARY   :: CInt

foreign import javascript unsafe "h$base_c_s_isreg"  c_s_isreg  :: CMode -> CInt
foreign import javascript unsafe "h$base_c_s_ischr"  c_s_ischr  :: CMode -> CInt
foreign import javascript unsafe "h$base_c_s_isblk"  c_s_isblk  :: CMode -> CInt
foreign import javascript unsafe "h$base_c_s_isdir"  c_s_isdir  :: CMode -> CInt
foreign import javascript unsafe "h$base_c_s_isfifo" c_s_isfifo :: CMode -> CInt

s_isreg  :: CMode -> Bool
s_isreg cm = c_s_isreg cm /= 0
s_ischr  :: CMode -> Bool
s_ischr cm = c_s_ischr cm /= 0
s_isblk  :: CMode -> Bool
s_isblk cm = c_s_isblk cm /= 0
s_isdir  :: CMode -> Bool
s_isdir cm = c_s_isdir cm /= 0
s_isfifo :: CMode -> Bool
s_isfifo cm = c_s_isfifo cm /= 0

foreign import javascript unsafe "h$base_sizeof_stat" sizeof_stat :: Int
foreign import javascript unsafe "h$base_st_mtime"    st_mtime :: Ptr CStat -> IO CTime
foreign import javascript unsafe "h$base_st_size"     st_size :: Ptr CStat -> IO Int64
foreign import javascript unsafe "h$base_st_mode"     st_mode :: Ptr CStat -> IO CMode
foreign import javascript unsafe "h$base_st_dev"      st_dev :: Ptr CStat -> IO CDev
foreign import javascript unsafe "h$base_st_ino"      st_ino :: Ptr CStat -> IO CIno

foreign import javascript unsafe "(() => { return h$base_echo; })"            const_echo :: CInt
foreign import javascript unsafe "(() => { return h$base_tcsanow; })"         const_tcsanow :: CInt
foreign import javascript unsafe "(() => { return h$base_icanon; })"          const_icanon :: CInt
foreign import javascript unsafe "(() => { return h$base_vmin; })"            const_vmin   :: CInt
foreign import javascript unsafe "(() => { return h$base_vtime; })"           const_vtime  :: CInt
foreign import javascript unsafe "(() => { return h$base_sigttou; })"         const_sigttou :: CInt
foreign import javascript unsafe "(() => { return h$base_sig_block; })"       const_sig_block :: CInt
foreign import javascript unsafe "(() => { return h$base_sig_setmask; })"     const_sig_setmask :: CInt
foreign import javascript unsafe "(() => { return h$base_f_getfl; })"         const_f_getfl :: CInt
foreign import javascript unsafe "(() => { return h$base_f_setfl; })"         const_f_setfl :: CInt
foreign import javascript unsafe "(() => { return h$base_f_setfd; })"         const_f_setfd :: CInt
foreign import javascript unsafe "(() => { return h$base_fd_cloexec; })"      const_fd_cloexec :: CLong
foreign import javascript unsafe "(() => { return h$base_sizeof_termios; })"  sizeof_termios :: Int
foreign import javascript unsafe "(() => { return h$base_sizeof_sigset_t; })" sizeof_sigset_t :: Int
foreign import javascript unsafe "h$base_lflag"           c_lflag :: Ptr CTermios -> IO CTcflag
foreign import javascript unsafe "h$base_poke_lflag"      poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
foreign import javascript unsafe "h$base_ptr_c_cc"        ptr_c_cc  :: Ptr CTermios -> IO (Ptr Word8)
s_issock :: CMode -> Bool
s_issock cmode = c_s_issock cmode /= 0
foreign import javascript unsafe "h$base_c_s_issock"          c_s_issock :: CMode -> CInt
foreign import javascript unsafe "(() => { return h$base_default_buffer_size; })" dEFAULT_BUFFER_SIZE :: Int
foreign import javascript unsafe "(() => { return h$base_SEEK_CUR; })"            sEEK_CUR :: CInt
foreign import javascript unsafe "(() => { return h$base_SEEK_SET; })"            sEEK_SET :: CInt
foreign import javascript unsafe "(() => { return h$base_SEEK_END; })"            sEEK_END :: CInt

-- fixme, unclear if these can be supported, remove?
foreign import javascript unsafe "h$base_c_fcntl_read"  c_fcntl_read  :: CInt -> CInt -> IO CInt
foreign import javascript unsafe "h$base_c_fcntl_write" c_fcntl_write :: CInt -> CInt -> CLong -> IO CInt
foreign import javascript unsafe "h$base_c_fcntl_lock"  c_fcntl_lock  :: CInt -> CInt -> Ptr CFLock -> IO CInt

#else

#if defined(mingw32_HOST_OS)
-- See Note [Windows types]
foreign import capi unsafe "HsBase.h _read"
   c_read :: CInt -> Ptr Word8 -> CUInt -> IO CInt

-- See Note [Windows types]
foreign import capi safe "HsBase.h _read"
   c_safe_read :: CInt -> Ptr Word8 -> CUInt -> IO CInt

foreign import ccall unsafe "HsBase.h _umask"
   c_umask :: CMode -> IO CMode

-- See Note [Windows types]
foreign import capi unsafe "HsBase.h _write"
   c_write :: CInt -> Ptr Word8 -> CUInt -> IO CInt

-- See Note [Windows types]
foreign import capi safe "HsBase.h _write"
   c_safe_write :: CInt -> Ptr Word8 -> CUInt -> IO CInt

foreign import ccall unsafe "HsBase.h _pipe"
   c_pipe :: Ptr CInt -> IO CInt

foreign import capi unsafe "HsBase.h _lseeki64"
   c_lseek :: CInt -> Int64 -> CInt -> IO Int64

foreign import capi unsafe "HsBase.h _access"
   c_access :: CString -> CInt -> IO CInt

#if !defined(HAVE_CHMOD)
c_chmod :: CString -> CMode -> IO CInt
c_chmod _ _ = ioError (ioeSetLocation unsupportedOperation "_chmod")
#else
foreign import ccall unsafe "HsBase.h _chmod"
   c_chmod :: CString -> CMode -> IO CInt
#endif

foreign import capi unsafe "HsBase.h _close"
   c_close :: CInt -> IO CInt

foreign import capi unsafe "HsBase.h _creat"
   c_creat :: CString -> CMode -> IO CInt

#if !defined(HAVE_DUP)
c_dup :: CInt -> IO CInt
c_dup _ = ioError (ioeSetLocation unsupportedOperation "_dup")

c_dup2 :: CInt -> CInt -> IO CInt
c_dup2 _ _ = ioError (ioeSetLocation unsupportedOperation "_dup2")
#else
foreign import ccall unsafe "HsBase.h _dup"
   c_dup :: CInt -> IO CInt

foreign import ccall unsafe "HsBase.h _dup2"
   c_dup2 :: CInt -> CInt -> IO CInt
#endif

foreign import capi unsafe "HsBase.h _isatty"
   c_isatty :: CInt -> IO CInt

foreign import capi unsafe "HsBase.h _unlink"
   c_unlink :: CString -> IO CInt

foreign import capi unsafe "HsBase.h _utime"
   c_utime :: CString -> Ptr CUtimbuf -> IO CInt

foreign import capi unsafe "HsBase.h _getpid"
   c_getpid :: IO CPid
#else
-- We use CAPI as on some OSs (eg. Linux) this is wrapped by a macro
-- which redirects to the 64-bit-off_t versions when large file
-- support is enabled.

-- See Note [Windows types]
foreign import capi unsafe "HsBase.h read"
   c_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize

-- See Note [Windows types]
foreign import capi safe "HsBase.h read"
   c_safe_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize

foreign import ccall unsafe "HsBase.h umask"
   c_umask :: CMode -> IO CMode

-- See Note [Windows types]
foreign import capi unsafe "HsBase.h write"
   c_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize

-- See Note [Windows types]
foreign import capi safe "HsBase.h write"
   c_safe_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize

#if !defined(HAVE_PIPE)
c_pipe :: Ptr CInt -> IO CInt
c_pipe _ = ioError (ioeSetLocation unsupportedOperation "pipe")
#else
foreign import ccall unsafe "HsBase.h pipe"
   c_pipe :: Ptr CInt -> IO CInt
#endif

foreign import capi unsafe "unistd.h lseek"
   c_lseek :: CInt -> COff -> CInt -> IO COff

foreign import ccall unsafe "HsBase.h access"
   c_access :: CString -> CInt -> IO CInt

#if !defined(HAVE_CHMOD)
c_chmod :: CString -> CMode -> IO CInt
c_chmod _ _ = ioError (ioeSetLocation unsupportedOperation "chmod")
#else
foreign import ccall unsafe "HsBase.h chmod"
   c_chmod :: CString -> CMode -> IO CInt
#endif

foreign import ccall unsafe "HsBase.h close"
   c_close :: CInt -> IO CInt

foreign import ccall unsafe "HsBase.h creat"
   c_creat :: CString -> CMode -> IO CInt

#if !defined(HAVE_DUP)
c_dup :: CInt -> IO CInt
c_dup _ = ioError (ioeSetLocation unsupportedOperation "dup")

c_dup2 :: CInt -> CInt -> IO CInt
c_dup2 _ _ = ioError (ioeSetLocation unsupportedOperation "dup2")
#else
foreign import ccall unsafe "HsBase.h dup"
   c_dup :: CInt -> IO CInt

foreign import ccall unsafe "HsBase.h dup2"
   c_dup2 :: CInt -> CInt -> IO CInt
#endif

foreign import ccall unsafe "HsBase.h isatty"
   c_isatty :: CInt -> IO CInt

foreign import ccall unsafe "HsBase.h unlink"
   c_unlink :: CString -> IO CInt

foreign import capi unsafe "HsBase.h utime"
   c_utime :: CString -> Ptr CUtimbuf -> IO CInt

#if !defined(HAVE_GETPID)
c_getpid :: IO CPid
c_getpid = pure 1
#else
foreign import ccall unsafe "HsBase.h getpid"
   c_getpid :: IO CPid
#endif
#endif

#if !defined(javascript_HOST_ARCH)
foreign import ccall unsafe "HsBase.h __hscore_stat"
   c_stat :: CFilePath -> Ptr CStat -> IO CInt

foreign import ccall unsafe "HsBase.h __hscore_ftruncate"
   c_ftruncate :: CInt -> COff -> IO CInt
#endif

#if !defined(mingw32_HOST_OS) && !defined(javascript_HOST_ARCH)
foreign import capi unsafe "HsBase.h fcntl"
   c_fcntl_read  :: CInt -> CInt -> IO CInt

foreign import capi unsafe "HsBase.h fcntl"
   c_fcntl_write :: CInt -> CInt -> CLong -> IO CInt

foreign import capi unsafe "HsBase.h fcntl"
   c_fcntl_lock  :: CInt -> CInt -> Ptr CFLock -> IO CInt

#if !defined(HAVE_FORK)
c_fork :: IO CPid
c_fork = ioError (ioeSetLocation unsupportedOperation "fork")
#else
foreign import ccall unsafe "HsBase.h fork"
   c_fork :: IO CPid
#endif

foreign import ccall unsafe "HsBase.h link"
   c_link :: CString -> CString -> IO CInt

#if !defined(HAVE_MKFIFO)
c_mkfifo :: CString -> CMode -> IO CInt
c_mkfifo _ _ = ioError (ioeSetLocation unsupportedOperation "mkfifo")
#else
-- capi is required at least on Android
foreign import capi unsafe "HsBase.h mkfifo"
   c_mkfifo :: CString -> CMode -> IO CInt
#endif

#if HAVE_SIGNAL_H
foreign import capi unsafe "signal.h sigemptyset"
   c_sigemptyset :: Ptr CSigset -> IO CInt

foreign import capi unsafe "signal.h sigaddset"
   c_sigaddset :: Ptr CSigset -> CInt -> IO CInt

foreign import capi unsafe "signal.h sigprocmask"
   c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt
#endif

#if !defined(HAVE_TERMIOS_H)

c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt
c_tcgetattr _ _ = ioError (ioeSetLocation unsupportedOperation "tcgetattr")

c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt
c_tcsetattr _ _ _ = ioError (ioeSetLocation unsupportedOperation "tcsetattr")

#else

-- capi is required at least on Android
foreign import capi unsafe "HsBase.h tcgetattr"
   c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt

-- capi is required at least on Android
foreign import capi unsafe "HsBase.h tcsetattr"
   c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt

#endif

#if defined(HAVE_GETPID)
foreign import ccall unsafe "HsBase.h waitpid"
   c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
#else
c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
c_waitpid _ _ _ = ioError (ioeSetLocation unsupportedOperation "waitpid")
#endif

#endif

#if !defined(javascript_HOST_ARCH)
-- POSIX flags only:
foreign import ccall unsafe "HsBase.h __hscore_o_rdonly" o_RDONLY :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_wronly" o_WRONLY :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_rdwr"   o_RDWR   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_append" o_APPEND :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_creat"  o_CREAT  :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_excl"   o_EXCL   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_trunc"  o_TRUNC  :: CInt

-- non-POSIX flags.
foreign import ccall unsafe "HsBase.h __hscore_o_noctty"   o_NOCTTY   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_nonblock" o_NONBLOCK :: CInt
foreign import ccall unsafe "HsBase.h __hscore_o_binary"   o_BINARY   :: CInt

foreign import capi unsafe "sys/stat.h S_ISREG"  c_s_isreg  :: CMode -> CInt
foreign import capi unsafe "sys/stat.h S_ISCHR"  c_s_ischr  :: CMode -> CInt
foreign import capi unsafe "sys/stat.h S_ISBLK"  c_s_isblk  :: CMode -> CInt
foreign import capi unsafe "sys/stat.h S_ISDIR"  c_s_isdir  :: CMode -> CInt
foreign import capi unsafe "sys/stat.h S_ISFIFO" c_s_isfifo :: CMode -> CInt

s_isreg  :: CMode -> Bool
s_isreg cm = c_s_isreg cm /= 0
s_ischr  :: CMode -> Bool
s_ischr cm = c_s_ischr cm /= 0
s_isblk  :: CMode -> Bool
s_isblk cm = c_s_isblk cm /= 0
s_isdir  :: CMode -> Bool
s_isdir cm = c_s_isdir cm /= 0
s_isfifo :: CMode -> Bool
s_isfifo cm = c_s_isfifo cm /= 0

foreign import ccall unsafe "HsBase.h __hscore_sizeof_stat" sizeof_stat :: Int
foreign import ccall unsafe "HsBase.h __hscore_st_mtime" st_mtime :: Ptr CStat -> IO CTime
#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "HsBase.h __hscore_st_size" st_size :: Ptr CStat -> IO Int64
#else
foreign import ccall unsafe "HsBase.h __hscore_st_size" st_size :: Ptr CStat -> IO COff
#endif
foreign import ccall unsafe "HsBase.h __hscore_st_mode" st_mode :: Ptr CStat -> IO CMode
foreign import ccall unsafe "HsBase.h __hscore_st_dev" st_dev :: Ptr CStat -> IO CDev
foreign import ccall unsafe "HsBase.h __hscore_st_ino" st_ino :: Ptr CStat -> IO CIno

foreign import ccall unsafe "HsBase.h __hscore_echo"         const_echo :: CInt
foreign import ccall unsafe "HsBase.h __hscore_tcsanow"      const_tcsanow :: CInt
foreign import ccall unsafe "HsBase.h __hscore_icanon"       const_icanon :: CInt
foreign import ccall unsafe "HsBase.h __hscore_vmin"         const_vmin   :: CInt
foreign import ccall unsafe "HsBase.h __hscore_vtime"        const_vtime  :: CInt
foreign import ccall unsafe "HsBase.h __hscore_sigttou"      const_sigttou :: CInt
foreign import ccall unsafe "HsBase.h __hscore_sig_block"    const_sig_block :: CInt
foreign import ccall unsafe "HsBase.h __hscore_sig_setmask"  const_sig_setmask :: CInt
foreign import ccall unsafe "HsBase.h __hscore_f_getfl"      const_f_getfl :: CInt
foreign import ccall unsafe "HsBase.h __hscore_f_setfl"      const_f_setfl :: CInt
foreign import ccall unsafe "HsBase.h __hscore_f_setfd"      const_f_setfd :: CInt
foreign import ccall unsafe "HsBase.h __hscore_fd_cloexec"   const_fd_cloexec :: CLong

#if defined(HTYPE_TCFLAG_T)
foreign import ccall unsafe "HsBase.h __hscore_sizeof_termios"  sizeof_termios :: Int
foreign import ccall unsafe "HsBase.h __hscore_sizeof_sigset_t" sizeof_sigset_t :: Int

foreign import ccall unsafe "HsBase.h __hscore_lflag" c_lflag :: Ptr CTermios -> IO CTcflag
foreign import ccall unsafe "HsBase.h __hscore_poke_lflag" poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
foreign import ccall unsafe "HsBase.h __hscore_ptr_c_cc" ptr_c_cc  :: Ptr CTermios -> IO (Ptr Word8)
#endif

s_issock :: CMode -> Bool
#if !defined(mingw32_HOST_OS)
s_issock cmode = c_s_issock cmode /= 0
foreign import capi unsafe "sys/stat.h S_ISSOCK" c_s_issock :: CMode -> CInt
#else
s_issock _ = False
#endif

foreign import ccall unsafe "__hscore_bufsiz"  dEFAULT_BUFFER_SIZE :: Int
foreign import capi  unsafe "stdio.h value SEEK_CUR" sEEK_CUR :: CInt
foreign import capi  unsafe "stdio.h value SEEK_SET" sEEK_SET :: CInt
foreign import capi  unsafe "stdio.h value SEEK_END" sEEK_END :: CInt
#endif
#endif

{-
Note [Windows types]
~~~~~~~~~~~~~~~~~~~~

Windows' _read and _write have types that differ from POSIX. They take an
unsigned int for length and return a signed int where POSIX uses size_t and
ssize_t. Those are different on x86_64 and equivalent on x86. We import them
with the types in Microsoft's documentation which means that c_read,
c_safe_read, c_write and c_safe_write have different Haskell types depending on
the OS.
-}
