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
stdin :: Handle
#if defined(mingw32_HOST_OS)
stdin = POSIX.stdin <!> Win.stdin
#else
stdin = POSIX.stdin
#endif

-- | 'stdout' is a handle managing the programs standard output.
stdout :: Handle
#if defined(mingw32_HOST_OS)
stdout = POSIX.stdout <!> Win.stdout
#else
stdout = POSIX.stdout
#endif

-- | 'stderr' is a handle managing the programs standard error.
stderr :: Handle
#if defined(mingw32_HOST_OS)
stderr = POSIX.stderr <!> Win.stderr
#else
stderr = POSIX.stderr
#endif

-- | The computation @'openFile' path mode@ returns a file handle that can be
-- used to interact with the file.
--
-- The handle is open in text mode with 'System.IO.localeEncoding'.
-- You can change the encoding with 'System.IO.hSetEncoding'.
openFile
  :: FilePath -- ^ The path to the file that should be opened
  -> IOMode   -- ^ The mode in which the file should be opened
  -> IO Handle
#if defined(mingw32_HOST_OS)
openFile = POSIX.openFile <!> Win.openFile
#else
openFile = POSIX.openFile
#endif

-- | The computation @'openBinaryFile' path mode@ returns a file handle that can be
-- used to interact with the binary file.
--
-- This is different from 'openFile' as in that it does not use any file encoding.
openBinaryFile
  :: FilePath -- ^ The path to the binary file that should be opened
  -> IOMode   -- ^ The mode in which the binary file should be opened
  -> IO Handle
#if defined(mingw32_HOST_OS)
openBinaryFile = POSIX.openBinaryFile <!> Win.openBinaryFile
#else
openBinaryFile = POSIX.openBinaryFile
#endif

-- | The computation @'withFile' path mode action@ opens the file and runs @action@
-- with the obtained handle before closing the file.
--
-- Even when an exception is raised within the 'action', the file will still be closed.
-- This is why @'withFile' path mode act@ is preferable to
--
-- @'openFile' path mode >>= (\\hdl -> act hdl >>= 'System.IO.hClose' hdl)@
--
-- See also: 'System.IO.bracket'
withFile
  :: FilePath         -- ^ The path to the file that should be opened
  -> IOMode           -- ^ The mode in which the file should be opened
  -> (Handle -> IO r) -- ^ The action to run with the obtained handle
  -> IO r
#if defined(mingw32_HOST_OS)
-- TODO: implement as for POSIX
withFile = POSIX.withFile <!> wf
  where
    wf path mode act = bracket (Win.openFile path mode) hClose_impl act
#else
withFile = POSIX.withFile
#endif

-- | The computation @'withBinaryFile' path mode action@ opens the binary file
-- and runs @action@ with the obtained handle before closing the binary file.
--
-- This is different from 'withFile' as in that it does not use any file encoding.
--
-- Even when an exception is raised within the 'action', the file will still be closed.
-- This is why @'withBinaryFile' path mode act@ is preferable to
--
-- @'openBinaryFile' path mode >>= (\\hdl -> act hdl >>= 'System.IO.hClose' hdl)@
--
-- See also: 'System.IO.bracket'
withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
#if defined(mingw32_HOST_OS)
withBinaryFile = POSIX.withBinaryFile <!> wf
  where
    wf path mode act = bracket (Win.openBinaryFile path mode) hClose_impl act
#else
withBinaryFile = POSIX.withBinaryFile
#endif

openFileBlocking :: FilePath -> IOMode -> IO Handle
#if defined(mingw32_HOST_OS)
openFileBlocking = POSIX.openFileBlocking <!> Win.openFileBlocking
#else
openFileBlocking = POSIX.openFileBlocking
#endif

withFileBlocking :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
#if defined(mingw32_HOST_OS)
withFileBlocking = POSIX.withFileBlocking <!> wf
  where
    wf path mode act = bracket (Win.openFileBlocking path mode) hClose_impl act
#else
withFileBlocking = POSIX.withFileBlocking
#endif
