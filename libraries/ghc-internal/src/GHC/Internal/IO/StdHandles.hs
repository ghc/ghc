{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.IO.StdHandles
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This model abstracts away the platform specific handles that can be toggled
-- through the RTS.
--
-----------------------------------------------------------------------------

module GHC.Internal.IO.StdHandles
  ( -- std handles
    stdin, stdout, stderr,
    openFile, openBinaryFile, openFileBlocking,
    withFile, withBinaryFile, withFileBlocking
  ) where

import GHC.Internal.IO
import GHC.Internal.IO.IOMode
import GHC.Internal.IO.Handle.Types

import qualified GHC.Internal.IO.Handle.FD as POSIX

-- windows only imports
#if defined(mingw32_HOST_OS)
import GHC.Internal.IO.SubSystem
import qualified GHC.Internal.IO.Handle.Windows as Win
import GHC.Internal.IO.Handle.Internals (hClose_impl)
#endif


-- | 'stdin' is a handle managing the programs standard input.
stdin  :: Handle

-- | 'stdout' is a handle managing the programs standard output.
stdout :: Handle

-- | 'stderr' is a handle managing the programs standard error.
stderr :: Handle

-- | The computation @'openFile' path mode@ returns a file handle that can be
-- used to interact with the file.
openFile
  :: FilePath -- ^ The path to the file that should be opened
  -> IOMode   -- ^ The mode in which the file should be opened
  -> IO Handle

-- | The computation @'openBinaryFile' path mode@ returns a file handle that can be
-- used to interact with the binary file.
--
-- This is different from 'openFile' as in that it does not use any file encoding.
openBinaryFile
  :: FilePath -- ^ The path to the binary file that should be opened
  -> IOMode   -- ^ The mode in which the binary file should be opened
  -> IO Handle

-- | The computation @'withFile' path mode action@ opens the file and runs @action@
-- with the obtained handle before closing the file.
withFile
  :: FilePath         -- ^ The path to the file that should be opened
  -> IOMode           -- ^ The mode in which the file should be opened
  -> (Handle -> IO r) -- ^ The action to run with the obtained handle
  -> IO r

-- | The computation @'withBinaryFile' path mode action@ opens the binary file
-- and runs @action@ with the obtained handle before closing the binary file.
--
-- This is different from 'withFile' as in that it does not use any file encoding.
withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

openFileBlocking :: FilePath -> IOMode -> IO Handle
withFileBlocking :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

#if defined(mingw32_HOST_OS)
stdin  = POSIX.stdin <!> Win.stdin
stdout = POSIX.stdout <!> Win.stdout
stderr = POSIX.stderr <!> Win.stderr

openFile = POSIX.openFile <!> Win.openFile
-- TODO: implement as for POSIX
withFile = POSIX.withFile <!> wf
  where
    wf path mode act = bracket (Win.openFile path mode) hClose_impl act

openBinaryFile = POSIX.openBinaryFile <!> Win.openBinaryFile
withBinaryFile = POSIX.withBinaryFile <!> wf
  where
    wf path mode act = bracket (Win.openBinaryFile path mode) hClose_impl act

openFileBlocking = POSIX.openFileBlocking <!> Win.openFileBlocking
withFileBlocking = POSIX.withFileBlocking <!> wf
  where
    wf path mode act = bracket (Win.openFileBlocking path mode) hClose_impl act

#else
stdin  = POSIX.stdin
stdout = POSIX.stdout
stderr = POSIX.stderr

openFile = POSIX.openFile
withFile = POSIX.withFile

openBinaryFile = POSIX.openBinaryFile
withBinaryFile = POSIX.withBinaryFile

openFileBlocking = POSIX.openFileBlocking
withFileBlocking = POSIX.withFileBlocking
#endif
