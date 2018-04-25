{-# LANGUAGE CPP #-}
module System.Directory.Internal.Common where
import Prelude ()
import System.Directory.Internal.Prelude
import System.FilePath ((</>), isPathSeparator, isRelative,
                        pathSeparator, splitDrive, takeDrive)
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
#endif

-- | Similar to 'try' but only catches a specify kind of 'IOError' as
--   specified by the predicate.
tryIOErrorType :: (IOError -> Bool) -> IO a -> IO (Either IOError a)
tryIOErrorType check action = do
  result <- tryIOError action
  case result of
    Left  err -> if check err then return (Left err) else ioError err
    Right val -> return (Right val)

specializeErrorString :: String -> (IOError -> Bool) -> IO a -> IO a
specializeErrorString str errType action = do
  mx <- tryIOErrorType errType action
  case mx of
    Left  e -> ioError (ioeSetErrorString e str)
    Right x -> return x

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = do
  ioeSetLocation e newLoc
  where
    newLoc = loc <> if null oldLoc then "" else ":" <> oldLoc
    oldLoc = ioeGetLocation e

data FileType = File
              | SymbolicLink -- ^ POSIX: either file or directory link; Windows: file link
              | Directory
              | DirectoryLink -- ^ Windows only
              deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check whether the given 'FileType' is considered a directory by the
-- operating system.  This affects the choice of certain functions
-- e.g. `removeDirectory` vs `removeFile`.
fileTypeIsDirectory :: FileType -> Bool
fileTypeIsDirectory Directory     = True
fileTypeIsDirectory DirectoryLink = True
fileTypeIsDirectory _             = False

data Permissions
  = Permissions
  { readable :: Bool
  , writable :: Bool
  , executable :: Bool
  , searchable :: Bool
  } deriving (Eq, Ord, Read, Show)

-- | Obtain the current working directory as an absolute path.
--
-- In a multithreaded program, the current working directory is a global state
-- shared among all threads of the process.  Therefore, when performing
-- filesystem operations from multiple threads, it is highly recommended to
-- use absolute rather than relative paths (see: 'makeAbsolute').
--
-- The operation may fail with:
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'isDoesNotExistError' or 'NoSuchThing'
-- There is no path referring to the working directory.
-- @[EPERM, ENOENT, ESTALE...]@
--
-- * 'isPermissionError' or 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EACCES]@
--
-- * 'ResourceExhausted'
-- Insufficient resources are available to perform the operation.
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of current working directory.
--
getCurrentDirectory :: IO FilePath
getCurrentDirectory = (`ioeAddLocation` "getCurrentDirectory") `modifyIOError`
  specializeErrorString
    "Current working directory no longer exists"
    isDoesNotExistError
#ifdef mingw32_HOST_OS
    Win32.getCurrentDirectory
#else
    Posix.getWorkingDirectory
#endif

-- | Convert a path into an absolute path.  If the given path is relative, the
-- current directory is prepended.  If the path is already absolute, the path
-- is returned unchanged.  The function preserves the presence or absence of
-- the trailing path separator.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may fail with the same exceptions as 'getCurrentDirectory'.
--
-- (internal API)
prependCurrentDirectory :: FilePath -> IO FilePath
prependCurrentDirectory path =
  modifyIOError ((`ioeAddLocation` "prependCurrentDirectory") .
                 (`ioeSetFileName` path)) $
  if isRelative path -- avoid the call to `getCurrentDirectory` if we can
  then do
    cwd <- getCurrentDirectory
    let curDrive = takeWhile (not . isPathSeparator) (takeDrive cwd)
    let (drive, subpath) = splitDrive path
    -- handle drive-relative paths (Windows only)
    return . (</> subpath) $
      case drive of
        _ : _ | (toUpper <$> drive) /= (toUpper <$> curDrive) ->
                  drive <> [pathSeparator]
        _ -> cwd
  else return path
