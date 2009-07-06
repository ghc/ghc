{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Test where

import qualified System.FilePath as FilePath.Native
         ( joinPath )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, splitDirectories )
import qualified System.FilePath.Windows as FilePath.Windows
         ( joinPath )

data TarPath = TarPath FilePath -- path name, 100 characters max.
                       FilePath -- path prefix, 155 characters max.

fromTarPath, fromTarPathToPosixPath
           , fromTarPathToWindowsPath :: TarPath -> FilePath

fromTarPath (TarPath name prefix) =
  FilePath.Native.joinPath $ FilePath.Posix.splitDirectories prefix
                          ++ FilePath.Posix.splitDirectories name

fromTarPathToPosixPath (TarPath name prefix) =
  FilePath.Posix.joinPath $ FilePath.Posix.splitDirectories prefix
                         ++ FilePath.Posix.splitDirectories name

fromTarPathToWindowsPath (TarPath name prefix) =
  FilePath.Windows.joinPath $ FilePath.Posix.splitDirectories prefix
                           ++ FilePath.Posix.splitDirectories name
