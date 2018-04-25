{-# LANGUAGE CPP #-}
-- | Utility functions specific to 'directory' tests
module TestUtils
  ( copyPathRecursive
  , modifyPermissions
  , symlinkOrCopy
  , supportsSymlinks
  ) where
import Prelude ()
import System.Directory.Internal.Prelude
import System.Directory
import System.FilePath ((</>), normalise, takeDirectory)
#ifdef mingw32_HOST_OS
import System.Directory.Internal (win32_getFinalPathNameByHandle)
import qualified System.Win32 as Win32
#endif

-- | @'copyPathRecursive' path@ copies an existing file or directory at
--   /path/ together with its contents and subdirectories.
--
--   Warning: mostly untested and might not handle symlinks correctly.
copyPathRecursive :: FilePath -> FilePath -> IO ()
copyPathRecursive source dest =
  (`ioeSetLocation` "copyPathRecursive") `modifyIOError` do
    dirExists <- doesDirectoryExist source
    if dirExists
      then do
        contents <- listDirectory source
        createDirectory dest
        mapM_ (uncurry copyPathRecursive)
          [(source </> x, dest </> x) | x <- contents]
      else copyFile source dest

modifyPermissions :: FilePath -> (Permissions -> Permissions) -> IO ()
modifyPermissions path modify = do
  permissions <- getPermissions path
  setPermissions path (modify permissions)

-- | On Windows, the handler is called if symbolic links are unsupported or
-- the user lacks the necessary privileges to create them.  On other
-- platforms, the handler is never run.
handleSymlinkUnavail
  :: IO a                               -- ^ handler
  -> IO a                               -- ^ arbitrary action
  -> IO a
handleSymlinkUnavail _handler action = action
#ifdef mingw32_HOST_OS
  `catchIOError` \ e ->
    case ioeGetErrorType e of
      UnsupportedOperation -> _handler
      _ | isIllegalOperation e || isPermissionError e -> _handler
      _ -> ioError e
#endif

-- | Create a symbolic link.  On Windows, this falls back to copying if
-- forbidden by Group Policy or is not supported.  On other platforms, there
-- is no fallback.  Also, automatically detect if the source is a file or a
-- directory and create the appropriate type of link.
symlinkOrCopy :: FilePath -> FilePath -> IO ()
symlinkOrCopy target link = do
  let fullTarget = takeDirectory link </> target
  handleSymlinkUnavail (copyPathRecursive fullTarget link) $ do
    isDir <- doesDirectoryExist fullTarget
    (if isDir then createDirectoryLink else createFileLink)
      (normalise target)
      link

supportsSymlinks :: IO Bool
supportsSymlinks = do
  canCreate <- supportsLinkCreation
  canDeref <- supportsLinkDeref
  return (canCreate && canDeref)

-- | On Windows, test if symbolic link creation is supported and the user has
-- the necessary privileges to create them.  On other platforms, this always
-- returns 'True'.
supportsLinkCreation :: IO Bool
supportsLinkCreation = do
  let path = "_symlink_test.tmp"
  isSupported <- handleSymlinkUnavail (return False) $ do
    True <$ createFileLink path path
  when isSupported $ do
    removeFile path
  return isSupported

supportsLinkDeref :: IO Bool
supportsLinkDeref = do
#ifdef mingw32_HOST_OS
    True <$ win32_getFinalPathNameByHandle Win32.nullHANDLE 0
  `catchIOError` \ e ->
    case ioeGetErrorType e of
      UnsupportedOperation -> return False
      _ -> return True
#else
    return True
#endif
