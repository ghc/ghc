{-# OPTIONS -fno-implicit-prelude -optc-DNON_POSIX_SOURCE #-}

-- ---------------------------------------------------------------------------
-- $Id: Posix.hsc,v 1.2 2001/07/31 12:48:13 simonmar Exp $
--
-- POSIX support layer for the standard libraries
--
-- NON_POSIX_SOURCE needed for the following features:
--	* S_ISSOCK (no sockets in POSIX)

module GHC.Posix where

#include "HsCore.h"

import Control.Monad

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
  allocaBytes (#const sizeof(struct stat)) $ \ p_stat -> do
    throwErrnoIfMinus1Retry "fdFileSize" $
	c_fstat (fromIntegral fd) p_stat
    c_mode <- (#peek struct stat, st_mode) p_stat :: IO CMode 
    if not (s_isreg c_mode)
	then return (-1)
	else do
    c_size <- (#peek struct stat, st_size) p_stat :: IO COff
    return (fromIntegral c_size)

data FDType  = Directory | Stream | RegularFile
	       deriving (Eq)

fileType :: FilePath -> IO FDType
fileType file =
  allocaBytes (#const sizeof(struct stat)) $ \ p_stat -> do
  withCString file $ \p_file -> do
    throwErrnoIfMinus1Retry "fileType" $
      c_stat p_file p_stat
    statGetType p_stat

fdType :: Int -> IO FDType
fdType fd = 
  allocaBytes (#const sizeof(struct stat)) $ \ p_stat -> do
    throwErrnoIfMinus1Retry "fdType" $
	c_fstat (fromIntegral fd) p_stat
    statGetType p_stat

statGetType p_stat = do
  c_mode <- (#peek struct stat, st_mode) p_stat :: IO CMode
  case () of
      _ | s_isdir c_mode 	 	     -> return Directory
        | s_isfifo c_mode || s_issock c_mode -> return Stream
	| s_isreg c_mode 		     -> return RegularFile
	| otherwise			     -> ioException ioe_unknownfiletype
    

ioe_unknownfiletype = IOError Nothing UnsupportedOperation "fdType"
			"unknown file type" Nothing

foreign import "s_isreg_wrap" s_isreg :: CMode -> Bool
#def inline int s_isreg_wrap(m) { return S_ISREG(m); }

foreign import "s_isdir_wrap" s_isdir :: CMode -> Bool
#def inline int s_isdir_wrap(m) { return S_ISDIR(m); }

foreign import "s_isfifo_wrap" s_isfifo :: CMode -> Bool
#def inline int s_isfifo_wrap(m) { return S_ISFIFO(m); }

#ifndef mingw32_TARGET_OS
foreign import "s_issock_wrap" s_issock :: CMode -> Bool
#def inline int s_issock_wrap(m) { return S_ISSOCK(m); }
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
  allocaBytes (#const sizeof(struct termios))  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setEcho"
	(c_tcgetattr (fromIntegral fd) p_tios)
    c_lflag <- (#peek struct termios, c_lflag) p_tios :: IO CTcflag
    let new_c_lflag | on        = c_lflag .|. (#const ECHO)
	            | otherwise = c_lflag .&. complement (#const ECHO)
    (#poke struct termios, c_lflag) p_tios (new_c_lflag :: CTcflag)
    tcSetAttr fd (#const TCSANOW) p_tios

getEcho :: Int -> IO Bool
getEcho fd = do
  allocaBytes (#const sizeof(struct termios))  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setEcho"
	(c_tcgetattr (fromIntegral fd) p_tios)
    c_lflag <- (#peek struct termios, c_lflag) p_tios :: IO CTcflag
    return ((c_lflag .&. (#const ECHO)) /= 0)

setCooked :: Int -> Bool -> IO ()
setCooked fd cooked = 
  allocaBytes (#const sizeof(struct termios))  $ \p_tios -> do
    throwErrnoIfMinus1Retry "setCooked"
	(c_tcgetattr (fromIntegral fd) p_tios)

    -- turn on/off ICANON
    c_lflag <- (#peek struct termios, c_lflag) p_tios :: IO CTcflag
    let new_c_lflag | cooked    = c_lflag .|. (#const ICANON)
	            | otherwise = c_lflag .&. complement (#const ICANON)
    (#poke struct termios, c_lflag) p_tios (new_c_lflag :: CTcflag)

    -- set VMIN & VTIME to 1/0 respectively
    when cooked $ do
	    let c_cc  = (#ptr struct termios, c_cc) p_tios
		vmin  = c_cc `plusPtr` (#const VMIN)  :: Ptr Word8
		vtime = c_cc `plusPtr` (#const VTIME) :: Ptr Word8
	    poke vmin  1
	    poke vtime 0

    tcSetAttr fd (#const TCSANOW) p_tios

-- tcsetattr() when invoked by a background process causes the process
-- to be sent SIGTTOU regardless of whether the process has TOSTOP set
-- in its terminal flags (try it...).  This function provides a
-- wrapper which temporarily blocks SIGTTOU around the call, making it
-- transparent.

tcSetAttr :: FD -> CInt -> Ptr Termios -> IO ()
tcSetAttr fd options p_tios = do
  allocaBytes (#const sizeof(sigset_t)) $ \ p_sigset -> do
  allocaBytes (#const sizeof(sigset_t)) $ \ p_old_sigset -> do
     c_sigemptyset p_sigset
     c_sigaddset   p_sigset (#const SIGTTOU)
     c_sigprocmask (#const SIG_BLOCK) p_sigset p_old_sigset
     throwErrnoIfMinus1Retry_ "tcSetAttr" $
	 c_tcsetattr (fromIntegral fd) options p_tios
     c_sigprocmask (#const SIG_SETMASK) p_old_sigset nullPtr

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
		 (fcntl_read (fromIntegral fd) (#const F_GETFL))
  throwErrnoIfMinus1Retry "setNonBlockingFD"
	(fcntl_write (fromIntegral fd) 
	   (#const F_SETFL) (flags .|. #const O_NONBLOCK))
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

#ifdef HAVE_LSTAT
foreign import "lstat" unsafe
   c_lstat :: CString -> Ptr CStat -> IO CInt
#endif

foreign import "open" unsafe
   c_open :: CString -> CInt -> CMode -> IO CInt

-- POSIX flags only:
o_RDONLY    = (#const O_RDONLY)	   :: CInt
o_WRONLY    = (#const O_WRONLY)	   :: CInt
o_RDWR      = (#const O_RDWR)	   :: CInt
o_APPEND    = (#const O_APPEND)	   :: CInt
o_CREAT     = (#const O_CREAT)	   :: CInt
o_EXCL	    = (#const O_EXCL)	   :: CInt
o_TRUNC     = (#const O_TRUNC)	   :: CInt

#ifdef mingw32_TARGET_OS
o_NOCTTY    = 0 :: CInt
o_NONBLOCK  = 0 :: CInt
#else
o_NOCTTY    = (#const O_NOCTTY)	   :: CInt
o_NONBLOCK  = (#const O_NONBLOCK)  :: CInt
#endif

#ifdef HAVE_O_BINARY
o_BINARY    = (#const O_BINARY)	   :: CInt
#endif

foreign import "isatty" unsafe
   c_isatty :: CInt -> IO CInt

foreign import "close" unsafe
   c_close :: CInt -> IO CInt

foreign import "lseek" unsafe
   c_lseek :: CInt -> COff -> CInt -> IO COff

foreign import "write" unsafe 
   c_write :: CInt -> Ptr CChar -> CSize -> IO CSsize

foreign import "read" unsafe 
   c_read :: CInt -> Ptr CChar -> CSize -> IO CSsize

#ifndef mingw32_TARGET_OS
foreign import "fcntl" unsafe
   fcntl_read  :: CInt -> CInt -> IO CInt

foreign import "fcntl" unsafe
   fcntl_write :: CInt -> CInt -> CInt -> IO CInt

foreign import "fork" unsafe
   fork :: IO CPid 

foreign import "sigemptyset" unsafe
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
