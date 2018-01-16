module System.Directory.Internal.Posix where
#include <HsDirectoryConfig.h>
#ifndef mingw32_HOST_OS
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory.Internal.Common
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import System.FilePath (normalise)
import qualified System.Posix as Posix

-- we use the 'free' from the standard library here since it's not entirely
-- clear whether Haskell's 'free' corresponds to the same one
foreign import ccall unsafe "free" c_free :: Ptr a -> IO ()

c_PATH_MAX :: Maybe Int
#ifdef PATH_MAX
c_PATH_MAX | c_PATH_MAX' > toInteger maxValue = Nothing
           | otherwise                        = Just (fromInteger c_PATH_MAX')
  where c_PATH_MAX' = (#const PATH_MAX)
        maxValue    = maxBound `asTypeOf` case c_PATH_MAX of ~(Just x) -> x
#else
c_PATH_MAX = Nothing
#endif

foreign import ccall "realpath" c_realpath
  :: CString -> CString -> IO CString

withRealpath :: CString -> (CString -> IO a) -> IO a
withRealpath path action = case c_PATH_MAX of
  Nothing ->
    -- newer versions of POSIX support cases where the 2nd arg is NULL;
    -- hopefully that is the case here, as there is no safer way
    bracket (realpath nullPtr) c_free action
  Just pathMax ->
    -- allocate one extra just to be safe
    allocaBytes (pathMax + 1) (realpath >=> action)
  where realpath = throwErrnoIfNull "" . c_realpath path

type Metadata = Posix.FileStatus

-- note: normalise is needed to handle empty paths

getSymbolicLinkMetadata :: FilePath -> IO Metadata
getSymbolicLinkMetadata = Posix.getSymbolicLinkStatus . normalise

getFileMetadata :: FilePath -> IO Metadata
getFileMetadata = Posix.getFileStatus . normalise

fileTypeFromMetadata :: Metadata -> FileType
fileTypeFromMetadata stat
  | isLink    = SymbolicLink
  | isDir     = Directory
  | otherwise = File
  where
    isLink = Posix.isSymbolicLink stat
    isDir  = Posix.isDirectory stat

fileSizeFromMetadata :: Metadata -> Integer
fileSizeFromMetadata = fromIntegral . Posix.fileSize

accessTimeFromMetadata :: Metadata -> UTCTime
accessTimeFromMetadata =
  posixSecondsToUTCTime . posix_accessTimeHiRes

modificationTimeFromMetadata :: Metadata -> UTCTime
modificationTimeFromMetadata =
  posixSecondsToUTCTime . posix_modificationTimeHiRes

posix_accessTimeHiRes, posix_modificationTimeHiRes
  :: Posix.FileStatus -> POSIXTime
#if MIN_VERSION_unix(2, 6, 0)
posix_accessTimeHiRes = Posix.accessTimeHiRes
posix_modificationTimeHiRes = Posix.modificationTimeHiRes
#else
posix_accessTimeHiRes = realToFrac . Posix.accessTime
posix_modificationTimeHiRes = realToFrac . Posix.modificationTime
#endif

type Mode = Posix.FileMode

modeFromMetadata :: Metadata -> Mode
modeFromMetadata = Posix.fileMode

allWriteMode :: Posix.FileMode
allWriteMode =
  Posix.ownerWriteMode .|.
  Posix.groupWriteMode .|.
  Posix.otherWriteMode

hasWriteMode :: Mode -> Bool
hasWriteMode m = m .&. allWriteMode /= 0

setWriteMode :: Bool -> Mode -> Mode
setWriteMode False m = m .&. complement allWriteMode
setWriteMode True  m = m .|. allWriteMode

setFileMode :: FilePath -> Mode -> IO ()
setFileMode = Posix.setFileMode

setFilePermissions :: FilePath -> Mode -> IO ()
setFilePermissions = setFileMode

getAccessPermissions :: FilePath -> IO Permissions
getAccessPermissions path = do
  m <- getFileMetadata path
  let isDir = fileTypeIsDirectory (fileTypeFromMetadata m)
  r <- Posix.fileAccess path True  False False
  w <- Posix.fileAccess path False True  False
  x <- Posix.fileAccess path False False True
  return Permissions
         { readable   = r
         , writable   = w
         , executable = x && not isDir
         , searchable = x && isDir
         }

setAccessPermissions :: FilePath -> Permissions -> IO ()
setAccessPermissions path (Permissions r w e s) = do
  m <- getFileMetadata path
  setFileMode path (modifyBit (e || s) Posix.ownerExecuteMode .
                    modifyBit w Posix.ownerWriteMode .
                    modifyBit r Posix.ownerReadMode .
                    modeFromMetadata $ m)
  where
    modifyBit :: Bool -> Posix.FileMode -> Posix.FileMode -> Posix.FileMode
    modifyBit False b m = m .&. complement b
    modifyBit True  b m = m .|. b

#endif
