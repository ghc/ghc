{-# OPTIONS -fno-implicit-prelude -#include "PrelIOUtils.h" #-}

-- ---------------------------------------------------------------------------
--
-- POSIX support layer for the standard libraries
--
-- Non-posix compliant in order to support the following features:
--	* S_ISSOCK (no sockets in POSIX)

module PrelPosix where

-- See above comment for non-Posixness reasons.
-- #include "PosixSource.h"

#include "HsStd.h"

import PrelBase
import PrelNum
import PrelReal
import PrelMaybe
import PrelCString
import PrelPtr
import PrelWord
import PrelInt
import PrelCTypesISO
import PrelCTypes
import PrelCError
import PrelStorable
import PrelMarshalAlloc
import PrelMarshalUtils
import PrelBits
import PrelIOBase
import Monad


-- ---------------------------------------------------------------------------
-- Types

data CDir    = CDir
type CSigset = ()

type CDev    = #type dev_t
type CIno    = #type ino_t
type CMode   = #type mode_t
type COff    = #type off_t
type CPid    = #type pid_t

#ifdef mingw32_TARGET_OS
type CSsize  = #type size_t
#else
type CGid    = #type gid_t
type CNlink  = #type nlink_t
type CSsize  = #type ssize_t
type CUid    = #type uid_t
type CCc     = #type cc_t
type CSpeed  = #type speed_t
type CTcflag = #type tcflag_t
#endif

-- ---------------------------------------------------------------------------
-- stat()-related stuff

type CStat = ()

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

-- NOTE: On Win32 platforms, this will only work with file descriptors
-- referring to file handles. i.e., it'll fail for socket FDs.
fdType :: Int -> IO FDType
fdType fd = 
  allocaBytes sizeof_stat $ \ p_stat -> do
    throwErrnoIfMinus1Retry "fdType" $
	c_fstat (fromIntegral fd) p_stat
    c_mode <- st_mode p_stat :: IO CMode
    case () of
      _ |  s_isdir  c_mode  -> return Directory
        |  s_isfifo c_mode  -> return Stream
	|  s_issock c_mode  -> return Stream
	|  s_ischr  c_mode  -> return Stream
	|  s_isreg  c_mode  -> return RegularFile
	|  s_isblk  c_mode  -> return RegularFile
	| otherwise	    -> ioException ioe_unknownfiletype
    -- we consider character devices to be streams (eg. ttys),
    -- whereas block devices are more like regular files because they
    -- are seekable.

ioe_unknownfiletype = IOError Nothing UnsupportedOperation "fdType"
			"unknown file type" Nothing

foreign import "s_isreg_PrelPosix_wrap" unsafe s_isreg :: CMode -> Bool
foreign import "s_isdir_PrelPosix_wrap" unsafe s_isdir :: CMode -> Bool
foreign import "s_isfifo_PrelPosix_wrap" unsafe s_isfifo :: CMode -> Bool
foreign import "s_ischr_PrelPosix_wrap" unsafe s_ischr :: CMode -> Bool
foreign import "s_isblk_PrelPosix_wrap" unsafe s_isblk :: CMode -> Bool

#ifndef mingw32_TARGET_OS
foreign import "s_issock_PrelPosix_wrap" unsafe s_issock :: CMode -> Bool

#else
s_issock :: CMode -> Bool
s_issock cmode = False
#endif

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

-- ---------------------------------------------------------------------------
-- Terminal-related stuff

fdIsTTY :: Int -> IO Bool
fdIsTTY fd = c_isatty (fromIntegral fd) >>= return.toBool

#ifndef mingw32_TARGET_OS

type Termios = ()

setEcho :: Int -> Bool -> IO ()
setEcho fd on = do
  allocaBytes sizeof_termios  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setEcho"
	(c_tcgetattr (fromIntegral fd) p_tios)
    c_lflag <- c_lflag p_tios :: IO CTcflag
    let new_c_lflag | on        = c_lflag .|. prel_echo
	            | otherwise = c_lflag .&. complement prel_echo
    poke_c_lflag p_tios (new_c_lflag :: CTcflag)
    tcSetAttr fd prel_tcsanow p_tios

getEcho :: Int -> IO Bool
getEcho fd = do
  allocaBytes sizeof_termios  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setEcho"
	(c_tcgetattr (fromIntegral fd) p_tios)
    c_lflag <- c_lflag p_tios :: IO CTcflag
    return ((c_lflag .&. prel_echo) /= 0)

setCooked :: Int -> Bool -> IO ()
setCooked fd cooked = 
  allocaBytes sizeof_termios  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setCooked"
	(c_tcgetattr (fromIntegral fd) p_tios)

    -- turn on/off ICANON
    c_lflag <- c_lflag p_tios :: IO CTcflag
    let new_c_lflag | cooked    = c_lflag .|. prel_icanon
	            | otherwise = c_lflag .&. complement prel_icanon
    poke_c_lflag p_tios (new_c_lflag :: CTcflag)

    -- set VMIN & VTIME to 1/0 respectively
    when cooked $ do
            c_cc <- prel_ptr_c_cc p_tios
	    let vmin  = c_cc `plusPtr` prel_vmin  :: Ptr Word8
		vtime = c_cc `plusPtr` prel_vtime :: Ptr Word8
	    poke vmin  1
	    poke vtime 0

    tcSetAttr fd prel_tcsanow p_tios

-- tcsetattr() when invoked by a background process causes the process
-- to be sent SIGTTOU regardless of whether the process has TOSTOP set
-- in its terminal flags (try it...).  This function provides a
-- wrapper which temporarily blocks SIGTTOU around the call, making it
-- transparent.

tcSetAttr :: FD -> CInt -> Ptr Termios -> IO ()
tcSetAttr fd options p_tios = do
  allocaBytes sizeof_sigset_t $ \ p_sigset -> do
  allocaBytes sizeof_sigset_t $ \ p_old_sigset -> do
     c_sigemptyset p_sigset
     c_sigaddset   p_sigset prel_sigttou
     c_sigprocmask prel_sig_block p_sigset p_old_sigset
     throwErrnoIfMinus1Retry_ "tcSetAttr" $
	 c_tcsetattr (fromIntegral fd) options p_tios
     c_sigprocmask prel_sig_setmask p_old_sigset nullPtr

foreign import ccall "prel_lflag" c_lflag :: Ptr Termios -> IO CTcflag
foreign import ccall "prel_poke_lflag" poke_c_lflag :: Ptr Termios -> CTcflag -> IO ()
foreign import ccall "prel_ptr_c_cc" ptr_c_cc  :: Ptr Termios -> IO Word8

foreign import ccall "prel_echo"      unsafe prel_echo :: CInt
foreign import ccall "prel_tcsanow"   unsafe prel_tcsanow :: CInt
foreign import ccall "prel_icanon"    unsafe prel_icanon :: CInt
foreign import ccall "prel_vmin"      unsafe prel_vmin   :: CInt
foreign import ccall "prel_vtime"     unsafe prel_vtime  :: CInt
foreign import ccall "prel_sigttou"   unsafe prel_sigttou :: CInt
foreign import ccall "prel_sig_block" unsafe prel_sig_block :: CInt
foreign import ccall "prel_sig_setmask" unsafe prel_sig_setmask :: CInt
foreign import ccall "prel_f_getfl"     unsafe prel_f_getfl :: CInt
foreign import ccall "prel_f_setfl"     unsafe prel_f_setfl :: CInt
#else

-- bogus defns for win32
setCooked :: Int -> Bool -> IO ()
setCooked fd cooked = return ()

setEcho :: Int -> Bool -> IO ()
setEcho fd on = return ()

getEcho :: Int -> IO Bool
getEcho fd = return False

#endif

-- ---------------------------------------------------------------------------
-- Turning on non-blocking for a file descriptor

#ifndef mingw32_TARGET_OS

setNonBlockingFD fd = do
  flags <- throwErrnoIfMinus1Retry "setNonBlockingFD"
		 (fcntl_read (fromIntegral fd) prel_f_getfl)
  -- An error when setting O_NONBLOCK isn't fatal: on some systems 
  -- there are certain file handles on which this will fail (eg. /dev/null
  -- on FreeBSD) so we throw away the return code from fcntl_write.
  fcntl_write (fromIntegral fd) prel_f_setfl (flags .|. o_NONBLOCK)
#else

-- bogus defns for win32
setNonBlockingFD fd = return ()

#endif

-- -----------------------------------------------------------------------------
-- foreign imports

foreign import "stat" unsafe
   c_stat :: CString -> Ptr CStat -> IO CInt

foreign import "fstat" unsafe
   c_fstat :: CInt -> Ptr CStat -> IO CInt

foreign import "open" unsafe
   c_open :: CString -> CInt -> CMode -> IO CInt

foreign import ccall "prel_sizeof_stat" unsafe sizeof_stat :: Int
foreign import ccall "prel_st_mtime" unsafe st_mtime :: Ptr CStat -> IO CTime
foreign import ccall "prel_st_size" unsafe st_size :: Ptr CStat -> IO COff
foreign import ccall "prel_st_mode" unsafe st_mode :: Ptr CStat -> IO CMode

#ifndef mingw32_TARGET_OS
foreign import ccall "prel_sizeof_termios" unsafe sizeof_termios :: Int
foreign import ccall "prel_sizeof_sigset_t" unsafe sizeof_sigset_t :: Int
#endif

-- POSIX flags only:
foreign import ccall "prel_o_rdonly" unsafe o_RDONLY :: CInt
foreign import ccall "prel_o_wronly" unsafe o_WRONLY :: CInt
foreign import ccall "prel_o_rdwr"   unsafe o_RDWR   :: CInt
foreign import ccall "prel_o_append" unsafe o_APPEND :: CInt
foreign import ccall "prel_o_creat"  unsafe o_CREAT  :: CInt
foreign import ccall "prel_o_excl"   unsafe o_EXCL   :: CInt
foreign import ccall "prel_o_trunc"  unsafe o_TRUNC  :: CInt


-- non-POSIX flags.
foreign import ccall "prel_o_noctty"   unsafe o_NOCTTY   :: CInt
foreign import ccall "prel_o_nonblock" unsafe o_NONBLOCK :: CInt
foreign import ccall "prel_o_binary" unsafe o_BINARY :: CInt


foreign import "isatty" unsafe
   c_isatty :: CInt -> IO CInt

foreign import "close" unsafe
   c_close :: CInt -> IO CInt

#ifdef mingw32_TARGET_OS
closeFd :: Bool -> CInt -> IO CInt
closeFd isStream fd 
  | isStream  = c_closesocket fd
  | otherwise = c_close fd

foreign import "closesocket" unsafe
   c_closesocket :: CInt -> IO CInt
#endif

foreign import "lseek" unsafe
   c_lseek :: CInt -> COff -> CInt -> IO COff

#ifndef mingw32_TARGET_OS
foreign import "fcntl" unsafe
   fcntl_read  :: CInt -> CInt -> IO CInt

foreign import "fcntl" unsafe
   fcntl_write :: CInt -> CInt -> CInt -> IO CInt

foreign import "fork" unsafe
   fork :: IO CPid 

foreign import "sigemptyset_PrelPosix_wrap" unsafe
   c_sigemptyset :: Ptr CSigset -> IO ()

foreign import "sigaddset" unsafe
   c_sigaddset :: Ptr CSigset -> CInt -> IO ()

foreign import "sigprocmask" unsafe
   c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO ()

foreign import "tcgetattr" unsafe
   c_tcgetattr :: CInt -> Ptr Termios -> IO CInt

foreign import "tcsetattr" unsafe
   c_tcsetattr :: CInt -> CInt -> Ptr Termios -> IO CInt

foreign import "unlink" unsafe 
   c_unlink :: CString -> IO CInt

foreign import "waitpid" unsafe
   c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
#endif
