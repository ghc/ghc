{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , DeriveDataTypeable
  #-}
{-# OPTIONS_GHC -fno-warn-identities #-}
-- Whether there are identities depends on the platform
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.FD
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Raw read/write operations on file descriptors
--
-----------------------------------------------------------------------------

module GHC.IO.FD (
        FD(..),
        openFile, mkFD, release,
        setNonBlockingMode,
        readRawBufferPtr, readRawBufferPtrNoBlock, writeRawBufferPtr,
        stdin, stdout, stderr
    ) where

import GHC.Base
import GHC.Num
import GHC.Real
import GHC.Show
import GHC.Enum
import Data.Maybe
import Control.Monad
import Data.Typeable

import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import qualified GHC.IO.Device
import GHC.IO.Device (SeekMode(..), IODeviceType(..))
import GHC.Conc.IO
import GHC.IO.Exception
#ifdef mingw32_HOST_OS
import GHC.Windows
#endif

import Foreign
import Foreign.C
import qualified System.Posix.Internals
import System.Posix.Internals hiding (FD, setEcho, getEcho)
import System.Posix.Types

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

c_DEBUG_DUMP :: Bool
c_DEBUG_DUMP = False

-- -----------------------------------------------------------------------------
-- The file-descriptor IO device

data FD = FD {
  fdFD :: {-# UNPACK #-} !CInt,
#ifdef mingw32_HOST_OS
  -- On Windows, a socket file descriptor needs to be read and written
  -- using different functions (send/recv).
  fdIsSocket_ :: {-# UNPACK #-} !Int
#else
  -- On Unix we need to know whether this FD has O_NONBLOCK set.
  -- If it has, then we can use more efficient routines to read/write to it.
  -- It is always safe for this to be off.
  fdIsNonBlocking :: {-# UNPACK #-} !Int
#endif
 }
 deriving Typeable

#ifdef mingw32_HOST_OS
fdIsSocket :: FD -> Bool
fdIsSocket fd = fdIsSocket_ fd /= 0
#endif

instance Show FD where
  show fd = show (fdFD fd)

instance GHC.IO.Device.RawIO FD where
  read             = fdRead
  readNonBlocking  = fdReadNonBlocking
  write            = fdWrite
  writeNonBlocking = fdWriteNonBlocking

instance GHC.IO.Device.IODevice FD where
  ready         = ready
  close         = close
  isTerminal    = isTerminal
  isSeekable    = isSeekable
  seek          = seek
  tell          = tell
  getSize       = getSize
  setSize       = setSize
  setEcho       = setEcho
  getEcho       = getEcho
  setRaw        = setRaw
  devType       = devType
  dup           = dup
  dup2          = dup2

-- We used to use System.Posix.Internals.dEFAULT_BUFFER_SIZE, which is
-- taken from the value of BUFSIZ on the current platform.  This value
-- varies too much though: it is 512 on Windows, 1024 on OS X and 8192
-- on Linux.  So let's just use a decent size on every platform:
dEFAULT_FD_BUFFER_SIZE :: Int
dEFAULT_FD_BUFFER_SIZE = 8096

instance BufferedIO FD where
  newBuffer _dev state = newByteBuffer dEFAULT_FD_BUFFER_SIZE state
  fillReadBuffer    fd buf = readBuf' fd buf
  fillReadBuffer0   fd buf = readBufNonBlocking fd buf
  flushWriteBuffer  fd buf = writeBuf' fd buf
  flushWriteBuffer0 fd buf = writeBufNonBlocking fd buf

readBuf' :: FD -> Buffer Word8 -> IO (Int, Buffer Word8)
readBuf' fd buf = do
  when c_DEBUG_DUMP $
      puts ("readBuf fd=" ++ show fd ++ " " ++ summaryBuffer buf ++ "\n")
  (r,buf') <- readBuf fd buf
  when c_DEBUG_DUMP $
      puts ("after: " ++ summaryBuffer buf' ++ "\n")
  return (r,buf')

writeBuf' :: FD -> Buffer Word8 -> IO (Buffer Word8)
writeBuf' fd buf = do
  when c_DEBUG_DUMP $
      puts ("writeBuf fd=" ++ show fd ++ " " ++ summaryBuffer buf ++ "\n")
  writeBuf fd buf

-- -----------------------------------------------------------------------------
-- opening files

-- | Open a file and make an 'FD' for it.  Truncates the file to zero
-- size when the `IOMode` is `WriteMode`.
openFile
  :: FilePath -- ^ file to open
  -> IOMode   -- ^ mode in which to open the file
  -> Bool     -- ^ open the file in non-blocking mode?
  -> IO (FD,IODeviceType)

openFile filepath iomode non_blocking =
  withFilePath filepath $ \ f ->

    let 
      oflags1 = case iomode of
                  ReadMode      -> read_flags
                  WriteMode     -> write_flags
                  ReadWriteMode -> rw_flags
                  AppendMode    -> append_flags

#ifdef mingw32_HOST_OS
      binary_flags = o_BINARY
#else
      binary_flags = 0
#endif

      oflags2 = oflags1 .|. binary_flags

      oflags | non_blocking = oflags2 .|. nonblock_flags
             | otherwise    = oflags2
    in do

    -- the old implementation had a complicated series of three opens,
    -- which is perhaps because we have to be careful not to open
    -- directories.  However, the man pages I've read say that open()
    -- always returns EISDIR if the file is a directory and was opened
    -- for writing, so I think we're ok with a single open() here...
    fd <- throwErrnoIfMinus1Retry "openFile"
                (if non_blocking then c_open      f oflags 0o666
                                 else c_safe_open f oflags 0o666)

    (fD,fd_type) <- mkFD fd iomode Nothing{-no stat-}
                            False{-not a socket-} 
                            non_blocking
            `catchAny` \e -> do _ <- c_close fd
                                throwIO e

    -- we want to truncate() if this is an open in WriteMode, but only
    -- if the target is a RegularFile.  ftruncate() fails on special files
    -- like /dev/null.
    when (iomode == WriteMode && fd_type == RegularFile) $
      setSize fD 0

    return (fD,fd_type)

std_flags, output_flags, read_flags, write_flags, rw_flags,
    append_flags, nonblock_flags :: CInt
std_flags    = o_NOCTTY
output_flags = std_flags    .|. o_CREAT
read_flags   = std_flags    .|. o_RDONLY 
write_flags  = output_flags .|. o_WRONLY
rw_flags     = output_flags .|. o_RDWR
append_flags = write_flags  .|. o_APPEND
nonblock_flags = o_NONBLOCK


-- | Make a 'FD' from an existing file descriptor.  Fails if the FD
-- refers to a directory.  If the FD refers to a file, `mkFD` locks
-- the file according to the Haskell 2010 single writer/multiple reader
-- locking semantics (this is why we need the `IOMode` argument too).
mkFD :: CInt
     -> IOMode
     -> Maybe (IODeviceType, CDev, CIno)
     -- the results of fdStat if we already know them, or we want
     -- to prevent fdToHandle_stat from doing its own stat.
     -- These are used for:
     --   - we fail if the FD refers to a directory
     --   - if the FD refers to a file, we lock it using (cdev,cino)
     -> Bool   -- ^ is a socket (on Windows)
     -> Bool   -- ^ is in non-blocking mode on Unix
     -> IO (FD,IODeviceType)

mkFD fd iomode mb_stat is_socket is_nonblock = do

    let _ = (is_socket, is_nonblock) -- warning suppression

    (fd_type,dev,ino) <- 
        case mb_stat of
          Nothing   -> fdStat fd
          Just stat -> return stat

    let write = case iomode of
                   ReadMode -> False
                   _ -> True

    case fd_type of
        Directory -> 
           ioException (IOError Nothing InappropriateType "openFile"
                           "is a directory" Nothing Nothing)

        -- regular files need to be locked
        RegularFile -> do
           -- On Windows we need an additional call to get a unique device id
           -- and inode, since fstat just returns 0 for both.
           (unique_dev, unique_ino) <- getUniqueFileInfo fd dev ino
           r <- lockFile fd unique_dev unique_ino (fromBool write)
           when (r == -1)  $
                ioException (IOError Nothing ResourceBusy "openFile"
                                   "file is locked" Nothing Nothing)

        _other_type -> return ()

#ifdef mingw32_HOST_OS
    _ <- setmode fd True -- unconditionally set binary mode
#endif

    return (FD{ fdFD = fd,
#ifndef mingw32_HOST_OS
                fdIsNonBlocking = fromEnum is_nonblock
#else
                fdIsSocket_ = fromEnum is_socket
#endif
              },
            fd_type)

getUniqueFileInfo :: CInt -> CDev -> CIno -> IO (Word64, Word64)
#ifndef mingw32_HOST_OS
getUniqueFileInfo _ dev ino = return (fromIntegral dev, fromIntegral ino)
#else
getUniqueFileInfo fd _ _ = do
  with 0 $ \devptr -> do
    with 0 $ \inoptr -> do
      c_getUniqueFileInfo fd devptr inoptr
      liftM2 (,) (peek devptr) (peek inoptr)
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "__hscore_setmode"
  setmode :: CInt -> Bool -> IO CInt
#endif

-- -----------------------------------------------------------------------------
-- Standard file descriptors

stdFD :: CInt -> FD
stdFD fd = FD { fdFD = fd,
#ifdef mingw32_HOST_OS
                fdIsSocket_ = 0
#else
                fdIsNonBlocking = 0
   -- We don't set non-blocking mode on standard handles, because it may
   -- confuse other applications attached to the same TTY/pipe
   -- see Note [nonblock]
#endif
                }

stdin, stdout, stderr :: FD
stdin  = stdFD 0
stdout = stdFD 1
stderr = stdFD 2

-- -----------------------------------------------------------------------------
-- Operations on file descriptors

close :: FD -> IO ()
close fd =
  do let closer realFd =
           throwErrnoIfMinus1Retry_ "GHC.IO.FD.close" $
#ifdef mingw32_HOST_OS
           if fdIsSocket fd then
             c_closesocket (fromIntegral realFd)
           else
#endif
             c_close (fromIntegral realFd)

     -- release the lock *first*, because otherwise if we're preempted
     -- after closing but before releasing, the FD may have been reused.
     -- (#7646)
     release fd

     closeFdWith closer (fromIntegral (fdFD fd))

release :: FD -> IO ()
release fd = do _ <- unlockFile (fdFD fd)
                return ()

#ifdef mingw32_HOST_OS
foreign import WINDOWS_CCONV unsafe "HsBase.h closesocket"
   c_closesocket :: CInt -> IO CInt
#endif

isSeekable :: FD -> IO Bool
isSeekable fd = do
  t <- devType fd
  return (t == RegularFile || t == RawDevice)

seek :: FD -> SeekMode -> Integer -> IO ()
seek fd mode off = do
  throwErrnoIfMinus1Retry_ "seek" $
     c_lseek (fdFD fd) (fromIntegral off) seektype
 where
    seektype :: CInt
    seektype = case mode of
                   AbsoluteSeek -> sEEK_SET
                   RelativeSeek -> sEEK_CUR
                   SeekFromEnd  -> sEEK_END

tell :: FD -> IO Integer
tell fd =
 fromIntegral `fmap`
   (throwErrnoIfMinus1Retry "hGetPosn" $
      c_lseek (fdFD fd) 0 sEEK_CUR)

getSize :: FD -> IO Integer
getSize fd = fdFileSize (fdFD fd)

setSize :: FD -> Integer -> IO () 
setSize fd size = do
  throwErrnoIf_ (/=0) "GHC.IO.FD.setSize"  $
     c_ftruncate (fdFD fd) (fromIntegral size)

devType :: FD -> IO IODeviceType
devType fd = do (ty,_,_) <- fdStat (fdFD fd); return ty

dup :: FD -> IO FD
dup fd = do
  newfd <- throwErrnoIfMinus1 "GHC.IO.FD.dup" $ c_dup (fdFD fd)
  return fd{ fdFD = newfd }

dup2 :: FD -> FD -> IO FD
dup2 fd fdto = do
  -- Windows' dup2 does not return the new descriptor, unlike Unix
  throwErrnoIfMinus1_ "GHC.IO.FD.dup2" $
    c_dup2 (fdFD fd) (fdFD fdto)
  return fd{ fdFD = fdFD fdto } -- original FD, with the new fdFD

setNonBlockingMode :: FD -> Bool -> IO FD
setNonBlockingMode fd set = do 
  setNonBlockingFD (fdFD fd) set
#if defined(mingw32_HOST_OS)
  return fd
#else
  return fd{ fdIsNonBlocking = fromEnum set }
#endif

ready :: FD -> Bool -> Int -> IO Bool
ready fd write msecs = do
  r <- throwErrnoIfMinus1Retry "GHC.IO.FD.ready" $
          fdReady (fdFD fd) (fromIntegral $ fromEnum $ write)
                            (fromIntegral msecs)
#if defined(mingw32_HOST_OS)
                          (fromIntegral $ fromEnum $ fdIsSocket fd)
#else
                          0
#endif
  return (toEnum (fromIntegral r))

foreign import ccall safe "fdReady"
  fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt

-- ---------------------------------------------------------------------------
-- Terminal-related stuff

isTerminal :: FD -> IO Bool
isTerminal fd =
#if defined(mingw32_HOST_OS)
    is_console (fdFD fd) >>= return.toBool
#else
    c_isatty (fdFD fd) >>= return.toBool
#endif

setEcho :: FD -> Bool -> IO () 
setEcho fd on = System.Posix.Internals.setEcho (fdFD fd) on

getEcho :: FD -> IO Bool
getEcho fd = System.Posix.Internals.getEcho (fdFD fd)

setRaw :: FD -> Bool -> IO ()
setRaw fd raw = System.Posix.Internals.setCooked (fdFD fd) (not raw)

-- -----------------------------------------------------------------------------
-- Reading and Writing

fdRead :: FD -> Ptr Word8 -> Int -> IO Int
fdRead fd ptr bytes
  = do { r <- readRawBufferPtr "GHC.IO.FD.fdRead" fd ptr 0 (fromIntegral bytes)
       ; return (fromIntegral r) }

fdReadNonBlocking :: FD -> Ptr Word8 -> Int -> IO (Maybe Int)
fdReadNonBlocking fd ptr bytes = do
  r <- readRawBufferPtrNoBlock "GHC.IO.FD.fdReadNonBlocking" fd ptr 
           0 (fromIntegral bytes)
  case fromIntegral r of
    (-1) -> return (Nothing)
    n    -> return (Just n)


fdWrite :: FD -> Ptr Word8 -> Int -> IO ()
fdWrite fd ptr bytes = do
  res <- writeRawBufferPtr "GHC.IO.FD.fdWrite" fd ptr 0 (fromIntegral bytes)
  let res' = fromIntegral res
  if res' < bytes 
     then fdWrite fd (ptr `plusPtr` res') (bytes - res')
     else return ()

-- XXX ToDo: this isn't non-blocking
fdWriteNonBlocking :: FD -> Ptr Word8 -> Int -> IO Int
fdWriteNonBlocking fd ptr bytes = do
  res <- writeRawBufferPtrNoBlock "GHC.IO.FD.fdWriteNonBlocking" fd ptr 0
            (fromIntegral bytes)
  return (fromIntegral res)

-- -----------------------------------------------------------------------------
-- FD operations

-- Low level routines for reading/writing to (raw)buffers:

#ifndef mingw32_HOST_OS

{-
NOTE [nonblock]:

Unix has broken semantics when it comes to non-blocking I/O: you can
set the O_NONBLOCK flag on an FD, but it applies to the all other FDs
attached to the same underlying file, pipe or TTY; there's no way to
have private non-blocking behaviour for an FD.  See bug #724.

We fix this by only setting O_NONBLOCK on FDs that we create; FDs that
come from external sources or are exposed externally are left in
blocking mode.  This solution has some problems though.  We can't
completely simulate a non-blocking read without O_NONBLOCK: several
cases are wrong here.  The cases that are wrong:

  * reading/writing to a blocking FD in non-threaded mode.
    In threaded mode, we just make a safe call to read().  
    In non-threaded mode we call select() before attempting to read,
    but that leaves a small race window where the data can be read
    from the file descriptor before we issue our blocking read().
  * readRawBufferNoBlock for a blocking FD

NOTE [2363]:

In the threaded RTS we could just make safe calls to read()/write()
for file descriptors in blocking mode without worrying about blocking
other threads, but the problem with this is that the thread will be
uninterruptible while it is blocked in the foreign call.  See #2363.
So now we always call fdReady() before reading, and if fdReady
indicates that there's no data, we call threadWaitRead.

-}

readRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO Int
readRawBufferPtr loc !fd buf off len
  | isNonBlocking fd = unsafe_read -- unsafe is ok, it can't block
  | otherwise    = do r <- throwErrnoIfMinus1 loc 
                                (unsafe_fdReady (fdFD fd) 0 0 0)
                      if r /= 0 
                        then read
                        else do threadWaitRead (fromIntegral (fdFD fd)); read
  where
    do_read call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock loc call
                            (threadWaitRead (fromIntegral (fdFD fd)))
    read        = if threaded then safe_read else unsafe_read
    unsafe_read = do_read (c_read (fdFD fd) (buf `plusPtr` off) len)
    safe_read   = do_read (c_safe_read (fdFD fd) (buf `plusPtr` off) len)

-- return: -1 indicates EOF, >=0 is bytes read
readRawBufferPtrNoBlock :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO Int
readRawBufferPtrNoBlock loc !fd buf off len
  | isNonBlocking fd  = unsafe_read -- unsafe is ok, it can't block
  | otherwise    = do r <- unsafe_fdReady (fdFD fd) 0 0 0
                      if r /= 0 then safe_read
                                else return 0
       -- XXX see note [nonblock]
 where
   do_read call = do r <- throwErrnoIfMinus1RetryOnBlock loc call (return (-1))
                     case r of
                       (-1) -> return 0
                       0    -> return (-1)
                       n    -> return (fromIntegral n)
   unsafe_read  = do_read (c_read (fdFD fd) (buf `plusPtr` off) len)
   safe_read    = do_read (c_safe_read (fdFD fd) (buf `plusPtr` off) len)

writeRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeRawBufferPtr loc !fd buf off len
  | isNonBlocking fd = unsafe_write -- unsafe is ok, it can't block
  | otherwise   = do r <- unsafe_fdReady (fdFD fd) 1 0 0
                     if r /= 0 
                        then write
                        else do threadWaitWrite (fromIntegral (fdFD fd)); write
  where
    do_write call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock loc call
                        (threadWaitWrite (fromIntegral (fdFD fd)))
    write         = if threaded then safe_write else unsafe_write
    unsafe_write  = do_write (c_write (fdFD fd) (buf `plusPtr` off) len)
    safe_write    = do_write (c_safe_write (fdFD fd) (buf `plusPtr` off) len)

writeRawBufferPtrNoBlock :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeRawBufferPtrNoBlock loc !fd buf off len
  | isNonBlocking fd = unsafe_write -- unsafe is ok, it can't block
  | otherwise   = do r <- unsafe_fdReady (fdFD fd) 1 0 0
                     if r /= 0 then write
                               else return 0
  where
    do_write call = do r <- throwErrnoIfMinus1RetryOnBlock loc call (return (-1))
                       case r of
                         (-1) -> return 0
                         n    -> return (fromIntegral n)
    write         = if threaded then safe_write else unsafe_write
    unsafe_write  = do_write (c_write (fdFD fd) (buf `plusPtr` off) len)
    safe_write    = do_write (c_safe_write (fdFD fd) (buf `plusPtr` off) len)

isNonBlocking :: FD -> Bool
isNonBlocking fd = fdIsNonBlocking fd /= 0

foreign import ccall unsafe "fdReady"
  unsafe_fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt

#else /* mingw32_HOST_OS.... */

readRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
readRawBufferPtr loc !fd buf off len
  | threaded  = blockingReadRawBufferPtr loc fd buf off len
  | otherwise = asyncReadRawBufferPtr    loc fd buf off len

writeRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeRawBufferPtr loc !fd buf off len
  | threaded  = blockingWriteRawBufferPtr loc fd buf off len
  | otherwise = asyncWriteRawBufferPtr    loc fd buf off len

readRawBufferPtrNoBlock :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
readRawBufferPtrNoBlock = readRawBufferPtr

writeRawBufferPtrNoBlock :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeRawBufferPtrNoBlock = writeRawBufferPtr

-- Async versions of the read/write primitives, for the non-threaded RTS

asyncReadRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
asyncReadRawBufferPtr loc !fd buf off len = do
    (l, rc) <- asyncRead (fromIntegral (fdFD fd)) (fdIsSocket_ fd) 
                        (fromIntegral len) (buf `plusPtr` off)
    if l == (-1)
      then 
        ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)
      else return (fromIntegral l)

asyncWriteRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
asyncWriteRawBufferPtr loc !fd buf off len = do
    (l, rc) <- asyncWrite (fromIntegral (fdFD fd)) (fdIsSocket_ fd)
                  (fromIntegral len) (buf `plusPtr` off)
    if l == (-1)
      then 
        ioError (errnoToIOError loc (Errno (fromIntegral rc)) Nothing Nothing)
      else return (fromIntegral l)

-- Blocking versions of the read/write primitives, for the threaded RTS

blockingReadRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
blockingReadRawBufferPtr loc fd buf off len
  = fmap fromIntegral $ throwErrnoIfMinus1Retry loc $
        if fdIsSocket fd
           then c_safe_recv (fdFD fd) (buf `plusPtr` off) len 0
           else c_safe_read (fdFD fd) (buf `plusPtr` off) len

blockingWriteRawBufferPtr :: String -> FD -> Ptr Word8-> Int -> CSize -> IO CInt
blockingWriteRawBufferPtr loc fd buf off len 
  = fmap fromIntegral $ throwErrnoIfMinus1Retry loc $
        if fdIsSocket fd
           then c_safe_send  (fdFD fd) (buf `plusPtr` off) len 0
           else do
             r <- c_safe_write (fdFD fd) (buf `plusPtr` off) len
             when (r == -1) c_maperrno
             return r
      -- we don't trust write() to give us the correct errno, and
      -- instead do the errno conversion from GetLastError()
      -- ourselves.  The main reason is that we treat ERROR_NO_DATA
      -- (pipe is closing) as EPIPE, whereas write() returns EINVAL
      -- for this case.  We need to detect EPIPE correctly, because it
      -- shouldn't be reported as an error when it happens on stdout.

-- NOTE: "safe" versions of the read/write calls for use by the threaded RTS.
-- These calls may block, but that's ok.

foreign import WINDOWS_CCONV safe "recv"
   c_safe_recv :: CInt -> Ptr Word8 -> CSize -> CInt{-flags-} -> IO CSsize

foreign import WINDOWS_CCONV safe "send"
   c_safe_send :: CInt -> Ptr Word8 -> CSize -> CInt{-flags-} -> IO CSsize

#endif

foreign import ccall "rtsSupportsBoundThreads" threaded :: Bool

-- -----------------------------------------------------------------------------
-- utils

#ifndef mingw32_HOST_OS
throwErrnoIfMinus1RetryOnBlock  :: String -> IO CSsize -> IO CSsize -> IO CSsize
throwErrnoIfMinus1RetryOnBlock loc f on_block  = 
  do
    res <- f
    if (res :: CSsize) == -1
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoIfMinus1RetryOnBlock loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
                 then do on_block
                 else throwErrno loc
      else return res
#endif

-- -----------------------------------------------------------------------------
-- Locking/unlocking

foreign import ccall unsafe "lockFile"
  lockFile :: CInt -> Word64 -> Word64 -> CInt -> IO CInt

foreign import ccall unsafe "unlockFile"
  unlockFile :: CInt -> IO CInt

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "get_unique_file_info"
  c_getUniqueFileInfo :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
#endif
