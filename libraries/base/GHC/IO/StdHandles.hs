{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.StdHandles
-- Copyright   :  (c) The University of Glasgow, 2017
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- This model abtracts away the platform specific handles that can be toggled
-- through the RTS.
--
-----------------------------------------------------------------------------

module GHC.IO.StdHandles
  ( -- std handles
    stdin, stdout, stderr,
    openFile, openBinaryFile, openFileBlocking,
    withFile, withBinaryFile, withFileBlocking
  ) where

import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Handle.Types

import qualified GHC.IO.Handle.FD as POSIX
#if defined(mingw32_HOST_OS)
import GHC.IO.SubSystem
import qualified GHC.IO.Handle.Windows as Win
import GHC.IO.Handle.Internals (hClose_impl)

stdin :: Handle
stdin = POSIX.stdin <!> Win.stdin

stdout :: Handle
stdout = POSIX.stdout <!> Win.stdout

stderr :: Handle
stderr = POSIX.stderr <!> Win.stderr

openFile :: FilePath -> IOMode -> IO Handle
openFile = POSIX.openFile <!> Win.openFile

-- TODO: implement as for POSIX
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile = POSIX.withFile <!> wf
  where
    wf path mode act = bracket (Win.openFile path mode) hClose_impl act

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = POSIX.openBinaryFile <!> Win.openBinaryFile

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = POSIX.withBinaryFile <!> wf
  where
    wf path mode act = bracket (Win.openBinaryFile path mode) hClose_impl act

openFileBlocking :: FilePath -> IOMode -> IO Handle
openFileBlocking = POSIX.openFileBlocking <!> Win.openFileBlocking

withFileBlocking :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileBlocking = POSIX.withFileBlocking <!> wf
  where
    wf path mode act = bracket (Win.openFileBlocking path mode) hClose_impl act

#else

stdin :: Handle
stdin = POSIX.stdin

stdout :: Handle
stdout = POSIX.stdout

stderr :: Handle
stderr = POSIX.stderr

openFile :: FilePath -> IOMode -> IO Handle
openFile = POSIX.openFile

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile = POSIX.withFile

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = POSIX.openBinaryFile

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = POSIX.withBinaryFile

openFileBlocking :: FilePath -> IOMode -> IO Handle
openFileBlocking = POSIX.openFileBlocking

withFileBlocking :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileBlocking = POSIX.withFileBlocking

#endif
