{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Distribution.Compat.CopyFile (
  copyFile,
  copyFileChanged,
  filesEqual,
  copyOrdinaryFile,
  copyExecutableFile,
  setFileOrdinary,
  setFileExecutable,
  setDirOrdinary,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Compat.Exception
import Distribution.Compat.Internal.TempFile

import Control.Exception
         ( bracketOnError, throwIO )
import qualified Data.ByteString.Lazy as BSL
import System.IO.Error
         ( ioeSetLocation )
import System.Directory
         ( doesFileExist, renameFile, removeFile )
import System.FilePath
         ( takeDirectory )
import System.IO
         ( IOMode(ReadMode), hClose, hGetBuf, hPutBuf, hFileSize
         , withBinaryFile )
import Foreign
         ( allocaBytes )

#ifndef mingw32_HOST_OS
import System.Posix.Types
         ( FileMode )
import System.Posix.Internals
         ( c_chmod, withFilePath )
import Foreign.C
         ( throwErrnoPathIfMinus1_ )
#endif /* mingw32_HOST_OS */

copyOrdinaryFile, copyExecutableFile :: FilePath -> FilePath -> NoCallStackIO ()
copyOrdinaryFile   src dest = copyFile src dest >> setFileOrdinary   dest
copyExecutableFile src dest = copyFile src dest >> setFileExecutable dest

setFileOrdinary,  setFileExecutable, setDirOrdinary  :: FilePath -> NoCallStackIO ()
#ifndef mingw32_HOST_OS
setFileOrdinary   path = setFileMode path 0o644 -- file perms -rw-r--r--
setFileExecutable path = setFileMode path 0o755 -- file perms -rwxr-xr-x

setFileMode :: FilePath -> FileMode -> NoCallStackIO ()
setFileMode name m =
  withFilePath name $ \s -> do
    throwErrnoPathIfMinus1_ "setFileMode" name (c_chmod s m)
#else
setFileOrdinary   _ = return ()
setFileExecutable _ = return ()
#endif
-- This happens to be true on Unix and currently on Windows too:
setDirOrdinary = setFileExecutable

-- | Copies a file to a new destination.
-- Often you should use `copyFileChanged` instead.
copyFile :: FilePath -> FilePath -> NoCallStackIO ()
copyFile fromFPath toFPath =
  copy
    `catchIO` (\ioe -> throwIO (ioeSetLocation ioe "copyFile"))
    where copy = withBinaryFile fromFPath ReadMode $ \hFrom ->
                 bracketOnError openTmp cleanTmp $ \(tmpFPath, hTmp) ->
                 do allocaBytes bufferSize $ copyContents hFrom hTmp
                    hClose hTmp
                    renameFile tmpFPath toFPath
          openTmp = openBinaryTempFile (takeDirectory toFPath) ".copyFile.tmp"
          cleanTmp (tmpFPath, hTmp) = do
            hClose hTmp          `catchIO` \_ -> return ()
            removeFile tmpFPath  `catchIO` \_ -> return ()
          bufferSize = 4096

          copyContents hFrom hTo buffer = do
                  count <- hGetBuf hFrom buffer bufferSize
                  when (count > 0) $ do
                          hPutBuf hTo buffer count
                          copyContents hFrom hTo buffer

-- | Like `copyFile`, but does not touch the target if source and destination
-- are already byte-identical. This is recommended as it is useful for
-- time-stamp based recompilation avoidance.
copyFileChanged :: FilePath -> FilePath -> NoCallStackIO ()
copyFileChanged src dest = do
  equal <- filesEqual src dest
  unless equal $ copyFile src dest

-- | Checks if two files are byte-identical.
-- Returns False if either of the files do not exist or if files
-- are of different size.
filesEqual :: FilePath -> FilePath -> NoCallStackIO Bool
filesEqual f1 f2 = do
  ex1 <- doesFileExist f1
  ex2 <- doesFileExist f2
  if not (ex1 && ex2) then return False else
    withBinaryFile f1 ReadMode $ \h1 ->
      withBinaryFile f2 ReadMode $ \h2 -> do
        s1 <- hFileSize h1
        s2 <- hFileSize h2
        if s1 /= s2
          then return False
          else do
            c1 <- BSL.hGetContents h1
            c2 <- BSL.hGetContents h2
            return $! c1 == c2
