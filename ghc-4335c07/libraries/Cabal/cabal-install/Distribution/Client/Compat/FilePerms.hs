{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Distribution.Client.Compat.FilePerms (
  setFileOrdinary,
  setFileExecutable,
  setFileHidden,
  ) where

#ifndef mingw32_HOST_OS
import System.Posix.Types
         ( FileMode )
import System.Posix.Internals
         ( c_chmod )
import Foreign.C
         ( withCString
         , throwErrnoPathIfMinus1_ )
#else
import System.Win32.File (setFileAttributes, fILE_ATTRIBUTE_HIDDEN)
#endif /* mingw32_HOST_OS */

setFileHidden, setFileOrdinary,  setFileExecutable  :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
setFileOrdinary   path = setFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = setFileMode path 0o755 -- file perms -rwxr-xr-x
setFileHidden     _    = return ()

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m =
  withCString name $ \s ->
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)
#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
setFileHidden  path = setFileAttributes path fILE_ATTRIBUTE_HIDDEN
#endif
