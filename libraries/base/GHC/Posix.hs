{-# OPTIONS -fno-implicit-prelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Posix
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- POSIX support layer for the standard libraries.
-- This library is built on *every* platform, including Win32.
--
-- Non-posix compliant in order to support the following features:
--	* S_ISSOCK (no sockets in POSIX)
--
-----------------------------------------------------------------------------

module GHC.Posix where

#include "config.h"

import Control.Monad
import System.Posix.Types

import Foreign
import Foreign.C

import Data.Bits
import Data.Maybe

import GHC.Base
import GHC.Num
import GHC.Real
import GHC.IOBase

-- ---------------------------------------------------------------------------
-- Types

type CDir       = ()
type CDirent    = ()
type CFLock     = ()
type CGroup     = ()
type CLconv     = ()
type CPasswd    = ()
type CSigaction = ()
type CSigset    = ()
type CStat      = ()
type CTermios   = ()
type CTm	= ()
type CTms	= ()
type CUtimbuf   = ()
type CUtsname   = ()

-- ---------------------------------------------------------------------------
-- stat()-related stuff

fdFileSize :: Int -> IO Integer
fdFileSize fd = 
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry "fileSize" $
	c_fstat (fromIntegral fd) p_stat
    c_mode <- st_mode p_stat :: IO CMode 
    if not (s_isreg c_mode)
	then return (-1)
	else do
    c_size <- st_size p_stat :: IO COff
    return (fromIntegral c_size)

data FDType  = Directory | Stream | RegularFile
	       deriving (Eq)

fileType :: FilePath -> IO FDType
fileType file =
  allocaBytes sizeof_stat $ \ p_stat -> do
  withCString file $ \p_file -> do
    throwErrnoIfMinus1Retry "fileType" $
      c_stat p_file p_stat
    statGetType p_stat

-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
fdType :: Int -> IO FDType
fdType fd = 
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry "fdType" $
	c_fstat (fromIntegral fd) p_stat
    statGetType p_stat

statGetType p_stat = do
  c_mode <- st_mode p_stat :: IO CMode
  case () of
      _ | s_isdir c_mode    	-> return Directory
        | s_isfifo c_mode || s_issock c_mode || s_ischr  c_mode
			  	-> return Stream
	| s_isreg c_mode	-> return RegularFile
	| otherwise		-> ioException ioe_unknownfiletype
    

ioe_unknownfiletype = IOError Nothing UnsupportedOperation "fdType"
			"unknown file type" Nothing

-- It isn't clear whether ftruncate is POSIX or not (I've read several
-- manpages and they seem to conflict), so we truncate using open/2.
fileTruncate :: FilePath -> IO ()
fileTruncate file = do
  let flags = o_WRONLY .|. o_TRUNC
  withCString file $ \file_cstr -> do
    fd <- fromIntegral `liftM`
	    throwErrnoIfMinus1Retry "fileTruncate"
 	        (c_open file_cstr (fromIntegral flags) 0o666)
    c_close fd
  return ()

#ifdef mingw32_TARGET_OS
closeFd :: Bool -> CInt -> IO CInt
closeFd isStream fd 
  | isStream  = c_closesocket fd
  | otherwise = c_close fd

foreign import stdcall unsafe "closesocket"
   c_closesocket :: CInt -> IO CInt
#endif

fdGetMode :: Int -> IO IOMode
fdGetMode fd = do
#ifdef mingw32_TARGET_OS
    flags1 <- throwErrnoIfMinus1Retry "fdGetMode" 
                (c__setmode (fromIntegral fd) (fromIntegral o_WRONLY))
    flags  <- throwErrnoIfMinus1Retry "fdGetMode" 
                (c__setmode (fromIntegral fd) (fromIntegral flags1))
#else
    flags <- throwErrnoIfMinus1Retry "fdGetMode" 
		(c_fcntl_read (fromIntegral fd) const_f_getfl)
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

-- ---------------------------------------------------------------------------
-- Terminal-related stuff

fdIsTTY :: Int -> IO Bool
fdIsTTY fd = c_isatty (fromIntegral fd) >>= return.toBool

#ifndef mingw32_TARGET_OS

setEcho :: Int -> Bool -> IO ()
setEcho fd on = do
  allocaBytes sizeof_termios  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setEcho"
	(c_tcgetattr (fromIntegral fd) p_tios)
    c_lflag <- c_lflag p_tios :: IO CTcflag
    let new_c_lflag
	 | on        = c_lflag .|. fromIntegral const_echo
	 | otherwise = c_lflag .&. complement (fromIntegral const_echo)
    poke_c_lflag p_tios (new_c_lflag :: CTcflag)
    tcSetAttr fd const_tcsanow p_tios

getEcho :: Int -> IO Bool
getEcho fd = do
  allocaBytes sizeof_termios  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setEcho"
	(c_tcgetattr (fromIntegral fd) p_tios)
    c_lflag <- c_lflag p_tios :: IO CTcflag
    return ((c_lflag .&. fromIntegral const_echo) /= 0)

setCooked :: Int -> Bool -> IO ()
setCooked fd cooked = 
  allocaBytes sizeof_termios  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setCooked"
	(c_tcgetattr (fromIntegral fd) p_tios)

    -- turn on/off ICANON
    c_lflag <- c_lflag p_tios :: IO CTcflag
    let new_c_lflag | cooked    = c_lflag .|. (fromIntegral const_icanon)
	            | otherwise = c_lflag .&. complement (fromIntegral const_icanon)
    poke_c_lflag p_tios (new_c_lflag :: CTcflag)

    -- set VMIN & VTIME to 1/0 respectively
    when cooked $ do
            c_cc <- ptr_c_cc p_tios
	    let vmin  = (c_cc `plusPtr` (fromIntegral const_vmin))  :: Ptr Word8
		vtime = (c_cc `plusPtr` (fromIntegral const_vtime)) :: Ptr Word8
	    poke vmin  1
	    poke vtime 0

    tcSetAttr fd const_tcsanow p_tios

-- tcsetattr() when invoked by a background process causes the process
-- to be sent SIGTTOU regardless of whether the process has TOSTOP set
-- in its terminal flags (try it...).  This function provides a
-- wrapper which temporarily blocks SIGTTOU around the call, making it
-- transparent.

tcSetAttr :: FD -> CInt -> Ptr CTermios -> IO ()
tcSetAttr fd options p_tios = do
  allocaBytes sizeof_sigset_t $ \ p_sigset -> do
  allocaBytes sizeof_sigset_t $ \ p_old_sigset -> do
     c_sigemptyset p_sigset
     c_sigaddset   p_sigset const_sigttou
     c_sigprocmask const_sig_block p_sigset p_old_sigset
     throwErrnoIfMinus1Retry_ "tcSetAttr" $
	 c_tcsetattr (fromIntegral fd) options p_tios
     c_sigprocmask const_sig_setmask p_old_sigset nullPtr
     return ()

#else

-- bogus defns for win32
setCooked :: Int -> Bool -> IO ()
setCooked fd cooked = do
  x <- set_console_buffering (fromIntegral fd) (if cooked then 1 else 0)
  if (x /= 0)
   then ioException (ioe_unk_error "setCooked" "failed to set buffering")
   else return ()

ioe_unk_error loc msg 
 = IOError Nothing OtherError loc msg Nothing

setEcho :: Int -> Bool -> IO ()
setEcho fd on = do
  x <- set_console_echo (fromIntegral fd) (if on then 1 else 0)
  if (x /= 0)
   then ioException (ioe_unk_error "setEcho" "failed to set echoing")
   else return ()

getEcho :: Int -> IO Bool
getEcho fd = do
  r <- get_console_echo (fromIntegral fd)
  if (r == (-1))
   then ioException (ioe_unk_error "getEcho" "failed to get echoing")
   else return (r == 1)

foreign import ccall unsafe "consUtils.h set_console_buffering__"
   set_console_buffering :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "consUtils.h set_console_echo__"
   set_console_echo :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "consUtils.h get_console_echo__"
   get_console_echo :: CInt -> IO CInt

#endif

-- ---------------------------------------------------------------------------
-- Turning on non-blocking for a file descriptor

#ifndef mingw32_TARGET_OS

setNonBlockingFD fd = do
  flags <- throwErrnoIfMinus1Retry "setNonBlockingFD"
		 (c_fcntl_read (fromIntegral fd) const_f_getfl)
  -- An error when setting O_NONBLOCK isn't fatal: on some systems 
  -- there are certain file handles on which this will fail (eg. /dev/null
  -- on FreeBSD) so we throw away the return code from fcntl_write.
  c_fcntl_write (fromIntegral fd) const_f_setfl (flags .|. o_NONBLOCK)
#else

-- bogus defns for win32
setNonBlockingFD fd = return ()

#endif

-- -----------------------------------------------------------------------------
-- foreign imports

foreign import ccall unsafe "access"
   c_access :: CString -> CMode -> IO CInt

foreign import ccall unsafe "chmod"
   c_chmod :: CString -> CMode -> IO CInt

foreign import ccall unsafe "chdir"
   c_chdir :: CString -> IO CInt

foreign import ccall unsafe "close"
   c_close :: CInt -> IO CInt

foreign import ccall unsafe "closedir" 
   c_closedir :: Ptr CDir -> IO CInt

foreign import ccall unsafe "creat"
   c_creat :: CString -> CMode -> IO CInt

foreign import ccall unsafe "dup"
   c_dup :: CInt -> IO CInt

foreign import ccall unsafe "dup2"
   c_dup2 :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "fstat"
   c_fstat :: CInt -> Ptr CStat -> IO CInt

foreign import ccall unsafe "getcwd"
   c_getcwd   :: Ptr CChar -> CInt -> IO (Ptr CChar)

foreign import ccall unsafe "isatty"
   c_isatty :: CInt -> IO CInt

foreign import ccall unsafe "lseek"
   c_lseek :: CInt -> COff -> CInt -> IO COff

foreign import ccall unsafe "__hscore_lstat"
   lstat :: CString -> Ptr CStat -> IO CInt

foreign import ccall unsafe "open"
   c_open :: CString -> CInt -> CMode -> IO CInt

foreign import ccall unsafe "opendir" 
   c_opendir :: CString  -> IO (Ptr CDir)

foreign import ccall unsafe "__hscore_mkdir"
   mkdir :: CString -> CInt -> IO CInt

foreign import ccall unsafe "read" 
   c_read :: CInt -> Ptr CChar -> CSize -> IO CSsize

foreign import ccall unsafe "readdir" 
   c_readdir :: Ptr CDir -> IO (Ptr CDirent)

foreign import ccall unsafe "rename"
   c_rename :: CString -> CString -> IO CInt
		     
foreign import ccall unsafe "rewinddir"
   c_rewinddir :: Ptr CDir -> IO ()

foreign import ccall unsafe "rmdir"
   c_rmdir :: CString -> IO CInt

foreign import ccall unsafe "stat"
   c_stat :: CString -> Ptr CStat -> IO CInt

foreign import ccall unsafe "umask"
   c_umask :: CMode -> IO CMode

foreign import ccall unsafe "write" 
   c_write :: CInt -> Ptr CChar -> CSize -> IO CSsize

foreign import ccall unsafe "unlink"
   c_unlink :: CString -> IO CInt

#ifndef mingw32_TARGET_OS
foreign import ccall unsafe "fcntl"
   c_fcntl_read  :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "fcntl"
   c_fcntl_write :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "fcntl"
   c_fcntl_lock  :: CInt -> CInt -> Ptr CFLock -> IO CInt

foreign import ccall unsafe "fork"
   c_fork :: IO CPid 

foreign import ccall unsafe "getpid"
   c_getpid :: IO CPid

foreign import ccall unsafe "fpathconf"
   c_fpathconf :: CInt -> CInt -> IO CLong

foreign import ccall unsafe "link"
   c_link :: CString -> CString -> IO CInt

foreign import ccall unsafe "mkfifo"
   c_mkfifo :: CString -> CMode -> IO CInt

foreign import ccall unsafe "pathconf"
   c_pathconf :: CString -> CInt -> IO CLong

foreign import ccall unsafe "pipe"
   c_pipe :: Ptr CInt -> IO CInt

foreign import ccall unsafe "__hscore_sigemptyset"
   c_sigemptyset :: Ptr CSigset -> IO CInt

foreign import ccall unsafe "__hscore_sigaddset"
   c_sigaddset :: Ptr CSigset -> CInt -> IO CInt

foreign import ccall unsafe "sigprocmask"
   c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt

foreign import ccall unsafe "tcgetattr"
   c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt

foreign import ccall unsafe "tcsetattr"
   c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt

foreign import ccall unsafe "uname"
   c_uname :: Ptr CUtsname -> IO CInt

foreign import ccall unsafe "utime"
   c_utime :: CString -> Ptr CUtimbuf -> IO CMode

foreign import ccall unsafe "waitpid"
   c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
#else
foreign import ccall unsafe "_setmode"
   c__setmode :: CInt -> CInt -> IO CInt

--   /* Set "stdin" to have binary mode: */
--   result = _setmode( _fileno( stdin ), _O_BINARY );
--   if( result == -1 )
--      perror( "Cannot set mode" );
--   else
--      printf( "'stdin' successfully changed to binary mode\n" );
#endif

-- POSIX flags only:
foreign import ccall unsafe "__hscore_o_rdonly" o_RDONLY :: CInt
foreign import ccall unsafe "__hscore_o_wronly" o_WRONLY :: CInt
foreign import ccall unsafe "__hscore_o_rdwr"   o_RDWR   :: CInt
foreign import ccall unsafe "__hscore_o_append" o_APPEND :: CInt
foreign import ccall unsafe "__hscore_o_creat"  o_CREAT  :: CInt
foreign import ccall unsafe "__hscore_o_excl"   o_EXCL   :: CInt
foreign import ccall unsafe "__hscore_o_trunc"  o_TRUNC  :: CInt

-- non-POSIX flags.
foreign import ccall unsafe "__hscore_o_noctty"   o_NOCTTY   :: CInt
foreign import ccall unsafe "__hscore_o_nonblock" o_NONBLOCK :: CInt
foreign import ccall unsafe "__hscore_o_binary"   o_BINARY   :: CInt

foreign import ccall unsafe "__hscore_s_isreg"  s_isreg  :: CMode -> Bool
foreign import ccall unsafe "__hscore_s_ischr"  s_ischr  :: CMode -> Bool
foreign import ccall unsafe "__hscore_s_isblk"  s_isblk  :: CMode -> Bool
foreign import ccall unsafe "__hscore_s_isdir"  s_isdir  :: CMode -> Bool
foreign import ccall unsafe "__hscore_s_isfifo" s_isfifo :: CMode -> Bool

foreign import ccall unsafe "__hscore_sizeof_stat" sizeof_stat :: Int
foreign import ccall unsafe "__hscore_st_mtime" st_mtime :: Ptr CStat -> IO CTime
foreign import ccall unsafe "__hscore_st_size" st_size :: Ptr CStat -> IO COff
foreign import ccall unsafe "__hscore_st_mode" st_mode :: Ptr CStat -> IO CMode

foreign import ccall unsafe "__hscore_echo"         const_echo :: CInt
foreign import ccall unsafe "__hscore_tcsanow"      const_tcsanow :: CInt
foreign import ccall unsafe "__hscore_icanon"       const_icanon :: CInt
foreign import ccall unsafe "__hscore_vmin"         const_vmin   :: CInt
foreign import ccall unsafe "__hscore_vtime"        const_vtime  :: CInt
foreign import ccall unsafe "__hscore_sigttou"      const_sigttou :: CInt
foreign import ccall unsafe "__hscore_sig_block"    const_sig_block :: CInt
foreign import ccall unsafe "__hscore_sig_setmask"  const_sig_setmask :: CInt
foreign import ccall unsafe "__hscore_f_getfl"      const_f_getfl :: CInt
foreign import ccall unsafe "__hscore_f_setfl"      const_f_setfl :: CInt

#ifndef mingw32_TARGET_OS
foreign import ccall unsafe "__hscore_sizeof_termios"  sizeof_termios :: Int
foreign import ccall unsafe "__hscore_sizeof_sigset_t" sizeof_sigset_t :: Int

foreign import ccall unsafe "__hscore_lflag" c_lflag :: Ptr CTermios -> IO CTcflag
foreign import ccall unsafe "__hscore_poke_lflag" poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
foreign import ccall unsafe "__hscore_ptr_c_cc" ptr_c_cc  :: Ptr CTermios -> IO (Ptr Word8)
#endif

#ifndef mingw32_TARGET_OS
foreign import ccall unsafe "__hscore_s_issock" s_issock :: CMode -> Bool
#else
s_issock :: CMode -> Bool
s_issock cmode = False
#endif
