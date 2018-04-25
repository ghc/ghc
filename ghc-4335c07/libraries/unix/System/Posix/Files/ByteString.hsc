#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Files.ByteString
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Functions defined by the POSIX standards for manipulating and querying the
-- file system. Names of underlying POSIX functions are indicated whenever
-- possible. A more complete documentation of the POSIX functions together
-- with a more detailed description of different error conditions are usually
-- available in the system's manual pages or from
-- <http://www.unix.org/version3/online.html> (free registration required).
--
-- When a function that calls an underlying POSIX function fails, the errno
-- code is converted to an 'IOError' using 'Foreign.C.Error.errnoToIOError'.
-- For a list of which errno codes may be generated, consult the POSIX
-- documentation for the underlying function.
--
-----------------------------------------------------------------------------

#include "HsUnix.h"

module System.Posix.Files.ByteString (
    -- * File modes
    -- FileMode exported by System.Posix.Types
    unionFileModes, intersectFileModes,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,
    fileTypeModes,
    blockSpecialMode, characterSpecialMode, namedPipeMode, regularFileMode,
    directoryMode, symbolicLinkMode, socketMode,

    -- ** Setting file modes
    setFileMode, setFdMode, setFileCreationMask,

    -- ** Checking file existence and permissions
    fileAccess, fileExist,

    -- * File status
    FileStatus,
    -- ** Obtaining file status
    getFileStatus, getFdStatus, getSymbolicLinkStatus,
    -- ** Querying file status
    deviceID, fileID, fileMode, linkCount, fileOwner, fileGroup,
    specialDeviceID, fileSize, accessTime, modificationTime,
    statusChangeTime,
    accessTimeHiRes, modificationTimeHiRes, statusChangeTimeHiRes,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    -- * Creation
    createNamedPipe,
    createDevice,

    -- * Hard links
    createLink, removeLink,

    -- * Symbolic links
    createSymbolicLink, readSymbolicLink,

    -- * Renaming files
    rename,

    -- * Changing file ownership
    setOwnerAndGroup,  setFdOwnerAndGroup,
#if HAVE_LCHOWN
    setSymbolicLinkOwnerAndGroup,
#endif

    -- * Changing file timestamps
    setFileTimes, setFileTimesHiRes,
    setFdTimesHiRes, setSymbolicLinkTimesHiRes,
    touchFile, touchFd, touchSymbolicLink,

    -- * Setting file sizes
    setFileSize, setFdSize,

    -- * Find system-specific limits for a file
    PathVar(..), getPathVar, getFdPathVar,
  ) where

import System.Posix.Types
import System.Posix.Internals hiding (withFilePath, peekFilePathLen)
import Foreign
import Foreign.C hiding (
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_ )

import System.Posix.Files.Common
import System.Posix.ByteString.FilePath

import Data.Time.Clock.POSIX (POSIXTime)

-- -----------------------------------------------------------------------------
-- chmod()

-- | @setFileMode path mode@ changes permission of the file given by @path@
-- to @mode@. This operation may fail with 'throwErrnoPathIfMinus1_' if @path@
-- doesn't exist or if the effective user ID of the current process is not that
-- of the file's owner.
--
-- Note: calls @chmod@.
setFileMode :: RawFilePath -> FileMode -> IO ()
setFileMode name m =
  withFilePath name $ \s -> do
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)


-- -----------------------------------------------------------------------------
-- access()

-- | @fileAccess name read write exec@ checks if the file (or other file system
-- object) @name@ can be accessed for reading, writing and\/or executing. To
-- check a permission set the corresponding argument to 'True'.
--
-- Note: calls @access@.
fileAccess :: RawFilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess name readOK writeOK execOK = access name flags
  where
   flags   = read_f .|. write_f .|. exec_f
   read_f  = if readOK  then (#const R_OK) else 0
   write_f = if writeOK then (#const W_OK) else 0
   exec_f  = if execOK  then (#const X_OK) else 0

-- | Checks for the existence of the file.
--
-- Note: calls @access@.
fileExist :: RawFilePath -> IO Bool
fileExist name =
  withFilePath name $ \s -> do
    r <- c_access s (#const F_OK)
    if (r == 0)
        then return True
        else do err <- getErrno
                if (err == eNOENT)
                   then return False
                   else throwErrnoPath "fileExist" name

access :: RawFilePath -> CMode -> IO Bool
access name flags =
  withFilePath name $ \s -> do
    r <- c_access s (fromIntegral flags)
    if (r == 0)
        then return True
        else do err <- getErrno
                if (err == eACCES || err == eROFS || err == eTXTBSY ||
                    err == ePERM)
                   then return False
                   else throwErrnoPath "fileAccess" name


-- | @getFileStatus path@ calls gets the @FileStatus@ information (user ID,
-- size, access times, etc.) for the file @path@.
--
-- Note: calls @stat@.
getFileStatus :: RawFilePath -> IO FileStatus
getFileStatus path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat))
  withForeignPtr fp $ \p ->
    withFilePath path $ \s ->
      throwErrnoPathIfMinus1Retry_ "getFileStatus" path (c_stat s p)
  return (FileStatus fp)

-- | Acts as 'getFileStatus' except when the 'RawFilePath' refers to a symbolic
-- link. In that case the @FileStatus@ information of the symbolic link itself
-- is returned instead of that of the file it points to.
--
-- Note: calls @lstat@.
getSymbolicLinkStatus :: RawFilePath -> IO FileStatus
getSymbolicLinkStatus path = do
  fp <- mallocForeignPtrBytes (#const sizeof(struct stat))
  withForeignPtr fp $ \p ->
    withFilePath path $ \s ->
      throwErrnoPathIfMinus1_ "getSymbolicLinkStatus" path (c_lstat s p)
  return (FileStatus fp)

foreign import capi unsafe "HsUnix.h lstat"
  c_lstat :: CString -> Ptr CStat -> IO CInt

-- | @createNamedPipe fifo mode@
-- creates a new named pipe, @fifo@, with permissions based on
-- @mode@. May fail with 'throwErrnoPathIfMinus1_' if a file named @name@
-- already exists or if the effective user ID of the current process doesn't
-- have permission to create the pipe.
--
-- Note: calls @mkfifo@.
createNamedPipe :: RawFilePath -> FileMode -> IO ()
createNamedPipe name mode = do
  withFilePath name $ \s ->
    throwErrnoPathIfMinus1_ "createNamedPipe" name (c_mkfifo s mode)

-- | @createDevice path mode dev@ creates either a regular or a special file
-- depending on the value of @mode@ (and @dev@).  @mode@ will normally be either
-- 'blockSpecialMode' or 'characterSpecialMode'.  May fail with
-- 'throwErrnoPathIfMinus1_' if a file named @name@ already exists or if the
-- effective user ID of the current process doesn't have permission to create
-- the file.
--
-- Note: calls @mknod@.
createDevice :: RawFilePath -> FileMode -> DeviceID -> IO ()
createDevice path mode dev =
  withFilePath path $ \s ->
    throwErrnoPathIfMinus1_ "createDevice" path (c_mknod s mode dev)

foreign import capi unsafe "HsUnix.h mknod"
  c_mknod :: CString -> CMode -> CDev -> IO CInt

-- -----------------------------------------------------------------------------
-- Hard links

-- | @createLink old new@ creates a new path, @new@, linked to an existing file,
-- @old@.
--
-- Note: calls @link@.
createLink :: RawFilePath -> RawFilePath -> IO ()
createLink name1 name2 =
  withFilePath name1 $ \s1 ->
  withFilePath name2 $ \s2 ->
  throwErrnoPathIfMinus1_ "createLink" name1 (c_link s1 s2)

-- | @removeLink path@ removes the link named @path@.
--
-- Note: calls @unlink@.
removeLink :: RawFilePath -> IO ()
removeLink name =
  withFilePath name $ \s ->
  throwErrnoPathIfMinus1_ "removeLink" name (c_unlink s)

-- -----------------------------------------------------------------------------
-- Symbolic Links

-- | @createSymbolicLink file1 file2@ creates a symbolic link named @file2@
-- which points to the file @file1@.
--
-- Symbolic links are interpreted at run-time as if the contents of the link
-- had been substituted into the path being followed to find a file or directory.
--
-- Note: calls @symlink@.
createSymbolicLink :: RawFilePath -> RawFilePath -> IO ()
createSymbolicLink file1 file2 =
  withFilePath file1 $ \s1 ->
  withFilePath file2 $ \s2 ->
  throwErrnoPathIfMinus1_ "createSymbolicLink" file2 (c_symlink s1 s2)

foreign import ccall unsafe "symlink"
  c_symlink :: CString -> CString -> IO CInt

-- ToDo: should really use SYMLINK_MAX, but not everyone supports it yet,
-- and it seems that the intention is that SYMLINK_MAX is no larger than
-- PATH_MAX.
#if !defined(PATH_MAX)
-- PATH_MAX is not defined on systems with unlimited path length.
-- Ugly.  Fix this.
#define PATH_MAX 4096
#endif

-- | Reads the @RawFilePath@ pointed to by the symbolic link and returns it.
--
-- Note: calls @readlink@.
readSymbolicLink :: RawFilePath -> IO RawFilePath
readSymbolicLink file =
  allocaArray0 (#const PATH_MAX) $ \buf -> do
    withFilePath file $ \s -> do
      len <- throwErrnoPathIfMinus1 "readSymbolicLink" file $
        c_readlink s buf (#const PATH_MAX)
      peekFilePathLen (buf,fromIntegral len)

foreign import ccall unsafe "readlink"
  c_readlink :: CString -> CString -> CSize -> IO CInt

-- -----------------------------------------------------------------------------
-- Renaming files

-- | @rename old new@ renames a file or directory from @old@ to @new@.
--
-- Note: calls @rename@.
rename :: RawFilePath -> RawFilePath -> IO ()
rename name1 name2 =
  withFilePath name1 $ \s1 ->
  withFilePath name2 $ \s2 ->
  throwErrnoPathIfMinus1_ "rename" name1 (c_rename s1 s2)

foreign import ccall unsafe "rename"
   c_rename :: CString -> CString -> IO CInt

-- -----------------------------------------------------------------------------
-- chown()

-- | @setOwnerAndGroup path uid gid@ changes the owner and group of @path@ to
-- @uid@ and @gid@, respectively.
--
-- If @uid@ or @gid@ is specified as -1, then that ID is not changed.
--
-- Note: calls @chown@.
setOwnerAndGroup :: RawFilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup name uid gid = do
  withFilePath name $ \s ->
    throwErrnoPathIfMinus1_ "setOwnerAndGroup" name (c_chown s uid gid)

foreign import ccall unsafe "chown"
  c_chown :: CString -> CUid -> CGid -> IO CInt

#if HAVE_LCHOWN
-- | Acts as 'setOwnerAndGroup' but does not follow symlinks (and thus
-- changes permissions on the link itself).
--
-- Note: calls @lchown@.
setSymbolicLinkOwnerAndGroup :: RawFilePath -> UserID -> GroupID -> IO ()
setSymbolicLinkOwnerAndGroup name uid gid = do
  withFilePath name $ \s ->
    throwErrnoPathIfMinus1_ "setSymbolicLinkOwnerAndGroup" name
        (c_lchown s uid gid)

foreign import ccall unsafe "lchown"
  c_lchown :: CString -> CUid -> CGid -> IO CInt
#endif

-- -----------------------------------------------------------------------------
-- Setting file times

-- | @setFileTimes path atime mtime@ sets the access and modification times
-- associated with file @path@ to @atime@ and @mtime@, respectively.
--
-- Note: calls @utime@.
setFileTimes :: RawFilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes name atime mtime = do
  withFilePath name $ \s ->
   allocaBytes (#const sizeof(struct utimbuf)) $ \p -> do
     (#poke struct utimbuf, actime)  p atime
     (#poke struct utimbuf, modtime) p mtime
     throwErrnoPathIfMinus1_ "setFileTimes" name (c_utime s p)

-- | Like 'setFileTimes' but timestamps can have sub-second resolution.
--
-- Note: calls @utimensat@ or @utimes@.
setFileTimesHiRes :: RawFilePath -> POSIXTime -> POSIXTime -> IO ()
#ifdef HAVE_UTIMENSAT
setFileTimesHiRes name atime mtime =
  withFilePath name $ \s ->
    withArray [toCTimeSpec atime, toCTimeSpec mtime] $ \times ->
      throwErrnoPathIfMinus1_ "setFileTimesHiRes" name $
        c_utimensat (#const AT_FDCWD) s times 0
#else
setFileTimesHiRes name atime mtime =
  withFilePath name $ \s ->
    withArray [toCTimeVal atime, toCTimeVal mtime] $ \times ->
      throwErrnoPathIfMinus1_ "setFileTimesHiRes" name (c_utimes s times)
#endif

-- | Like 'setFileTimesHiRes' but does not follow symbolic links.
-- This operation is not supported on all platforms. On these platforms,
-- this function will raise an exception.
--
-- Note: calls @utimensat@ or @lutimes@.
setSymbolicLinkTimesHiRes :: RawFilePath -> POSIXTime -> POSIXTime -> IO ()
#if HAVE_UTIMENSAT
setSymbolicLinkTimesHiRes name atime mtime =
  withFilePath name $ \s ->
    withArray [toCTimeSpec atime, toCTimeSpec mtime] $ \times ->
      throwErrnoPathIfMinus1_ "setSymbolicLinkTimesHiRes" name $
        c_utimensat (#const AT_FDCWD) s times (#const AT_SYMLINK_NOFOLLOW)
#elif HAVE_LUTIMES
setSymbolicLinkTimesHiRes name atime mtime =
  withFilePath name $ \s ->
    withArray [toCTimeVal atime, toCTimeVal mtime] $ \times ->
      throwErrnoPathIfMinus1_ "setSymbolicLinkTimesHiRes" name $
        c_lutimes s times
#else
setSymbolicLinkTimesHiRes =
  error "setSymbolicLinkTimesHiRes: not available on this platform"
#endif

-- | @touchFile path@ sets the access and modification times associated with
-- file @path@ to the current time.
--
-- Note: calls @utime@.
touchFile :: RawFilePath -> IO ()
touchFile name = do
  withFilePath name $ \s ->
   throwErrnoPathIfMinus1_ "touchFile" name (c_utime s nullPtr)

-- | Like 'touchFile' but does not follow symbolic links.
-- This operation is not supported on all platforms. On these platforms,
-- this function will raise an exception.
--
-- Note: calls @lutimes@.
touchSymbolicLink :: RawFilePath -> IO ()
#if HAVE_LUTIMES
touchSymbolicLink name =
  withFilePath name $ \s ->
    throwErrnoPathIfMinus1_ "touchSymbolicLink" name (c_lutimes s nullPtr)
#else
touchSymbolicLink =
  error "touchSymbolicLink: not available on this platform"
#endif

-- -----------------------------------------------------------------------------
-- Setting file sizes

-- | Truncates the file down to the specified length. If the file was larger
-- than the given length before this operation was performed the extra is lost.
--
-- Note: calls @truncate@.
setFileSize :: RawFilePath -> FileOffset -> IO ()
setFileSize file off =
  withFilePath file $ \s ->
    throwErrnoPathIfMinus1_ "setFileSize" file (c_truncate s off)

foreign import capi unsafe "HsUnix.h truncate"
  c_truncate :: CString -> COff -> IO CInt

-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

-- | @getPathVar var path@ obtains the dynamic value of the requested
-- configurable file limit or option associated with file or directory @path@.
-- For defined file limits, @getPathVar@ returns the associated
-- value.  For defined file options, the result of @getPathVar@
-- is undefined, but not failure.
--
-- Note: calls @pathconf@.
getPathVar :: RawFilePath -> PathVar -> IO Limit
getPathVar name v = do
  withFilePath name $ \ nameP ->
    throwErrnoPathIfMinus1 "getPathVar" name $
      c_pathconf nameP (pathVarConst v)

foreign import ccall unsafe "pathconf"
  c_pathconf :: CString -> CInt -> IO CLong
